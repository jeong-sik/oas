(** Tool execution helpers — lookup, hooks, event_bus, parallel Eio fibers.

    These functions are parameterized by explicit fields rather than Agent.t
    to avoid circular module dependencies (Agent_tools is compiled before Agent). *)

open Types

let _log = Log.create ~module_name:"agent_tools" ()

type tool_failure_kind = Types.tool_failure_kind =
  | Validation_error
  | Recoverable_tool_error
  | Non_retryable_tool_error
  | Reported_tool_error

type tool_execution_result =
  { tool_use_id : string
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; content : string
  ; outcome : Types.tool_result_outcome
  }

type scheduled_tool_use =
  { index : int
  ; id : string
  ; name : string
  ; input : Yojson.Safe.t
  ; execution_mode : Tool.execution_mode
  }

type execution_batch =
  | Concurrent_batch of scheduled_tool_use list
  | Serial_batch of scheduled_tool_use

type tool_index = (string, Tool.t) Hashtbl.t

let add_first tbl key value = if not (Hashtbl.mem tbl key) then Hashtbl.add tbl key value

let build_index tools =
  let capacity = max 16 (List.length tools * 2) in
  let by_name = Hashtbl.create capacity in
  List.iter
    (fun (tool : Tool.t) ->
       let name = tool.schema.name in
       add_first by_name name tool)
    tools;
  by_name
;;

let find_in_index (index : tool_index) name : Tool.t option = Hashtbl.find_opt index name

let tool_names_of_index index =
  Hashtbl.fold (fun name _ acc -> name :: acc) index [] |> List.sort_uniq String.compare
;;

let preview_tool_names ?(limit = 12) names =
  match names with
  | [] -> "(none)"
  | _ ->
    let rec take n rest =
      if n = 0
      then []
      else (
        match rest with
        | [] -> []
        | x :: xs -> x :: take (n - 1) xs)
    in
    let visible = take limit names in
    let suffix =
      let extra = List.length names - List.length visible in
      if extra > 0 then [ Printf.sprintf "...(+%d more)" extra ] else []
    in
    String.concat "," (visible @ suffix)
;;

let unknown_tool_failure ~requested ~available =
  let available_preview = preview_tool_names available in
  let failure_kind =
    if available = [] then Non_retryable_tool_error else Validation_error
  in
  ( Printf.sprintf "Tool not found: %s. Available tools: %s" requested available_preview
  , failure_kind )
;;

let resolve_tool_call tool_index name input = name, input, find_in_index tool_index name

let tool_failure_result ~id ~name ~input ~content ~error_class =
  { tool_use_id = id
  ; tool_name = name
  ; input
  ; content
  ; outcome =
      Tool_failed
        { failure_kind = Non_retryable_tool_error; error_class = Some error_class }
  }
;;

let blocked_tool_result ~id ~name ~input ~content =
  tool_failure_result ~id ~name ~input ~content ~error_class:Types.Deterministic
;;

let tool_exception_result ~id ~name ~input exn =
  let content = Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn) in
  tool_failure_result ~id ~name ~input ~content ~error_class:Types.Unknown
;;

let approval_required_without_callback_result ~id ~name ~input =
  let reason = "approval required but no approval callback is registered" in
  blocked_tool_result ~id ~name ~input ~content:("Tool rejected: " ^ reason)
;;

let schedule_tool_use ~tool_index index (id, name, input) =
  let execution_mode =
    match find_in_index tool_index name with
    | Some tool -> Tool.execution_mode tool
    | None -> Tool.Serial
  in
  { index; id; name; input; execution_mode }
;;

let execution_batches tool_uses =
  let flush_concurrent acc = function
    | [] -> acc
    | concurrent_tools -> Concurrent_batch (List.rev concurrent_tools) :: acc
  in
  let rec build acc current_concurrent = function
    | [] -> List.rev (flush_concurrent acc current_concurrent)
    | tool_use :: rest ->
      (match tool_use.execution_mode with
       | Tool.Concurrent -> build acc (tool_use :: current_concurrent) rest
       | Tool.Serial ->
         let acc = flush_concurrent acc current_concurrent in
         build (Serial_batch tool_use :: acc) [] rest)
  in
  build [] [] tool_uses
;;

let hook_schedule_of_tool_use ~batch_index ~batch_size (tool_use : scheduled_tool_use)
  : Hooks.tool_schedule
  =
  { planned_index = tool_use.index
  ; batch_index
  ; batch_size
  ; execution_mode = tool_use.execution_mode
  }
;;

let invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count ~hook_name hook_opt event
  =
  Tracing.with_span
    tracer
    { kind = Hook_invoke
    ; name = hook_name
    ; agent_name
    ; turn = turn_count
    ; extra = []
    ; links = []
    }
    (fun _ ->
       let decision = Hooks.invoke_validated ~hook_name hook_opt event in
       (match on_hook_invoked with
        | Some callback ->
          callback
            ~hook_name
            ~decision
            ~detail:
              (match event with
               | Hooks.PreToolUse { tool_name; _ }
               | Hooks.PostToolUse { tool_name; _ }
               | Hooks.PostToolUseFailure { tool_name; _ } -> Some tool_name
               | Hooks.BeforeTurn _
               | Hooks.BeforeTurnParams _
               | Hooks.AfterTurn _
               | Hooks.OnStop _
               | Hooks.OnError _
               | Hooks.OnToolError _ -> None)
        | None -> ());
       decision)
;;

(** Find and execute a single tool, invoking PostToolUse hook.
    Returns a structured execution result. *)
let find_and_execute_tool_with_index
      ~context
      ~tool_index
      ~(hooks : Hooks.hooks)
      ~event_bus
      ~tracer
      ~agent_name
      ~turn_count
      ?correlation_id
      ?run_id
      ?on_hook_invoked
      ~schedule
      name
      input
      id
  =
  let requested_name = name in
  let name, input, tool_opt = resolve_tool_call tool_index name input in
  (* ToolCalled event — capture the published envelope's run_id so the
     matching ToolCompleted records it as caused_by, preserving the
     call -> completion causation chain per tool invocation (#877).
     Using [ev.meta.run_id] works whether the caller supplies
     [~run_id] explicitly or we fall back to the fresh id minted by
     [mk_event]. *)
  let tool_called_run_id =
    match event_bus with
    | Some bus ->
      let ev =
        Event_bus.mk_event
          ?correlation_id
          ?run_id
          (ToolCalled
             { agent_name; tool_name = name; tool_use_id = id; input; turn = turn_count })
      in
      (try Event_bus.publish bus ev with
       | exn ->
         Log.warn
           _log
           "Event_bus.publish failed (ToolCalled)"
           [ Log.S ("error", Printexc.to_string exn) ]);
      Some ev.meta.run_id
    | None -> None
  in
  let result =
    try
      match tool_opt with
      | Some tool ->
        let validation_error_result ~input message =
          { tool_use_id = id
          ; tool_name = name
          ; input
          ; content = message
          ; outcome =
              Tool_failed
                { failure_kind = Validation_error
                ; error_class = Some Types.Deterministic
                }
          }
        in
        let emit_post_tool_use_failure ~input message =
          ignore
            (invoke_hook
               ?on_hook_invoked
               ~tracer
               ~agent_name
               ~turn_count
               ~hook_name:"post_tool_use_failure"
               hooks.post_tool_use_failure
               (Hooks.PostToolUseFailure
                  { tool_use_id = id; tool_name = name; input; error = message; schedule })
             : Hooks.hook_decision)
        in
        (* Tool inputs cross this boundary unchanged. The schema either accepts
           the exact JSON value or produces a typed validation failure for the
           model to correct in a later native ToolUse block. *)
        let validated_input =
          match Tool_input_validation.validate tool.schema input with
          | Tool_input_validation.Valid exact_input -> Ok exact_input
          | Tool_input_validation.Invalid errors ->
            let message =
              Tool_input_validation.format_errors_inline
                ~tool_name:name
                ~args:input
                errors
            in
            emit_post_tool_use_failure ~input message;
            Error message
        in
        (match validated_input with
         | Error message -> validation_error_result ~input message
         | Ok exact_input ->
           let t0 = Unix.gettimeofday () in
           let result = Tool.execute ~context tool exact_input in
           let duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
           let result_bytes =
             match result with
             | Ok { content; _meta = _ } -> String.length content
             | Error { message; _ } -> String.length message
           in
           let _post =
             invoke_hook
               ?on_hook_invoked
               ~tracer
               ~agent_name
               ~turn_count
               ~hook_name:"post_tool_use"
               hooks.post_tool_use
               (Hooks.PostToolUse
                  { tool_use_id = id
                  ; tool_name = name
                  ; input = exact_input
                  ; output = result
                  ; result_bytes
                  ; duration_ms
                  ; schedule
                  })
           in
           (match result with
            | Error { message; _ } ->
              ignore
                (invoke_hook
                   ?on_hook_invoked
                   ~tracer
                   ~agent_name
                   ~turn_count
                   ~hook_name:"post_tool_use_failure"
                   hooks.post_tool_use_failure
                   (Hooks.PostToolUseFailure
                      { tool_use_id = id
                      ; tool_name = name
                      ; input = exact_input
                      ; error = message
                      ; schedule
                      })
                 : Hooks.hook_decision);
              (* OnToolError: minimal tool-name/error event for consumers that
            don't need the PostToolUseFailure context (tool_use_id,
            schedule). Previously the hook type existed but had no emit
            site — registering [on_tool_error] was a silent no-op (#1029). *)
              ignore
                (invoke_hook
                   ?on_hook_invoked
                   ~tracer
                   ~agent_name
                   ~turn_count
                   ~hook_name:"on_tool_error"
                   hooks.on_tool_error
                   (Hooks.OnToolError { tool_name = name; error = message })
                 : Hooks.hook_decision)
            | Ok _ -> ());
           let content, outcome =
             match result with
             | Ok { content; _meta = _ } -> content, Tool_succeeded
             | Error { message; recoverable; error_class } ->
               let failure_kind =
                 if recoverable then Recoverable_tool_error else Non_retryable_tool_error
               in
               message, Tool_failed { failure_kind; error_class }
           in
           { tool_use_id = id; tool_name = name; input = exact_input; content; outcome })
      | None ->
        (* Tool dispatch failure (the LLM asked for a tool that isn't
         registered). Distinct from OnToolError — that fires when a
         tool actually ran and returned Error. This is a configuration
         / routing mistake, so it belongs on the general OnError
         channel. Unknown names are treated as validation errors when
         the current turn has visible tools, so the retry path can use
         the actual schema instead of preserving a stale name. *)
        let available = tool_names_of_index tool_index in
        let message, failure_kind =
          unknown_tool_failure ~requested:requested_name ~available
        in
        Log.warn
          _log
          "tool not found"
          [ Log.S ("tool", requested_name)
          ; Log.S ("available_tools", preview_tool_names available)
          ];
        ignore
          (invoke_hook
             ?on_hook_invoked
             ~tracer
             ~agent_name
             ~turn_count
             ~hook_name:"on_error"
             hooks.on_error
             (Hooks.OnError
                { detail = message; context = "agent_tools.find_and_execute_tool" })
           : Hooks.hook_decision);
        { tool_use_id = id
        ; tool_name = requested_name
        ; input
        ; content = message
        ; outcome = Tool_failed { failure_kind; error_class = Some Types.Deterministic }
        }
    with
    | Out_of_memory -> raise Out_of_memory
    | Stack_overflow -> raise Stack_overflow
    | Sys.Break -> raise Sys.Break
    | Eio.Cancel.Cancelled _ as ex -> raise ex
    | exn -> tool_exception_result ~id ~name ~input exn
  in
  (* ToolCompleted event *)
  (match event_bus with
   | Some bus ->
     let output_content = result.content in
     let output = Types.tool_result_of_outcome ~content:output_content result.outcome in
     (try
        Event_bus.publish
          bus
          (Event_bus.mk_event
             ?correlation_id
             ?run_id
             ?caused_by:tool_called_run_id
             (ToolCompleted
                { agent_name
                ; tool_name = name
                ; tool_use_id = id
                ; output
                ; turn = turn_count
                }))
      with
      | exn ->
        Log.warn
          _log
          "Event_bus.publish failed (ToolCompleted)"
          [ Log.S ("error", Printexc.to_string exn) ])
   | None -> ());
  result
;;

let find_and_execute_tool
      ~context
      ~tools
      ~(hooks : Hooks.hooks)
      ~event_bus
      ~tracer
      ~agent_name
      ~turn_count
      ?correlation_id
      ?run_id
      ?on_hook_invoked
      ~schedule
      name
      input
      id
  =
  let tool_index = build_index tools in
  find_and_execute_tool_with_index
    ~context
    ~tool_index
    ~hooks
    ~event_bus
    ~tracer
    ~agent_name
    ~turn_count
    ?correlation_id
    ?run_id
    ?on_hook_invoked
    ~schedule
    name
    input
    id
;;

let execute_scheduled_tool
      ~context
      ~tools:_
      ~tool_index
      ~(hooks : Hooks.hooks)
      ~event_bus
      ?journal
      ~tracer
      ~agent_name
      ~turn_count
      ~(usage : Types.usage_stats)
      ~approval
      ?correlation_id
      ?run_id
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      ~schedule
      (tool_use : scheduled_tool_use)
  =
  let { index; id; name; input; _ } = tool_use in
  let idem_key = Durable_event.make_idempotency_key ~tool_name:name ~input in
  (match journal with
   | Some j ->
     Durable_event.append
       j
       (Tool_called
          { turn = turn_count
          ; tool_name = name
          ; idempotency_key = idem_key
          ; input_hash = Digest.string (Yojson.Safe.to_string input)
          ; timestamp = Unix.gettimeofday ()
          })
   | None -> ());
  (match on_tool_execution_started with
   | Some callback -> callback ~tool_use_id:id ~tool_name:name ~input ~schedule
   | None -> ());
  let t0_tool = Unix.gettimeofday () in
  let triple =
    Tracing.with_span
      tracer
      { kind = Tool_exec; name; agent_name; turn = turn_count; extra = []; links = [] }
      (fun _tracer ->
         try
           let decision =
             invoke_hook
               ?on_hook_invoked
               ~tracer
               ~agent_name
               ~turn_count
               ~hook_name:"pre_tool_use"
               hooks.pre_tool_use
               (Hooks.PreToolUse
                  { tool_use_id = id
                  ; tool_name = name
                  ; input
                  ; accumulated_cost_usd = usage.Types.estimated_cost_usd
                  ; turn = turn_count
                  ; schedule
                  })
           in
           match decision with
           | Hooks.ApprovalRequired ->
             (match approval with
              | None ->
                Log.warn
                  _log
                  "ApprovalRequired but no approval callback — returning explicit failure"
                  [ Log.S ("tool", name); Log.S ("agent", agent_name) ];
                approval_required_without_callback_result ~id ~name ~input
              | Some approve_fn ->
                (match approve_fn ~tool_name:name ~input with
                 | Hooks.Approve ->
                   find_and_execute_tool_with_index
                     ~context
                     ~tool_index
                     ~hooks
                     ~event_bus
                     ~tracer
                     ~agent_name
                     ~turn_count
                     ?correlation_id
                     ?run_id
                     ?on_hook_invoked
                     ~schedule
                     name
                     input
                     id
                 | Hooks.Reject reason ->
                   blocked_tool_result
                     ~id
                     ~name
                     ~input
                     ~content:("Tool rejected: " ^ reason)
                 | Hooks.Edit new_input ->
                   find_and_execute_tool_with_index
                     ~context
                     ~tool_index
                     ~hooks
                     ~event_bus
                     ~tracer
                     ~agent_name
                     ~turn_count
                     ?correlation_id
                     ?run_id
                     ?on_hook_invoked
                     ~schedule
                     name
                     new_input
                     id))
           | Hooks.Continue ->
             find_and_execute_tool_with_index
               ~context
               ~tool_index
               ~hooks
               ~event_bus
               ~tracer
               ~agent_name
               ~turn_count
               ?correlation_id
               ?run_id
               ?on_hook_invoked
               ~schedule
               name
               input
               id
           | Hooks.AdjustParams _ ->
             find_and_execute_tool_with_index
               ~context
               ~tool_index
               ~hooks
               ~event_bus
               ~tracer
               ~agent_name
               ~turn_count
               ?correlation_id
               ?run_id
               ?on_hook_invoked
               ~schedule
               name
               input
               id
           | Hooks.ElicitInput _ | Hooks.Nudge _ ->
             find_and_execute_tool_with_index
               ~context
               ~tool_index
               ~hooks
               ~event_bus
               ~tracer
               ~agent_name
               ~turn_count
               ?correlation_id
               ?run_id
               ?on_hook_invoked
               ~schedule
               name
               input
               id
           | Hooks.HookFailed { stage; detail } ->
             blocked_tool_result
               ~id
               ~name
               ~input
               ~content:
                 (Printf.sprintf
                    "Tool execution blocked: hook pre_tool_use failed at %s: %s"
                    stage
                    detail)
           | Hooks.Block reason ->
             (* Intentional policy rejection from a PreToolUse hook. The host
                executes no tool; the reason string becomes the tool result
                content verbatim. Distinct from [Hooks.HookFailed], which
                represents an unintentional hook failure. *)
             blocked_tool_result ~id ~name ~input ~content:reason
         with
         | Out_of_memory -> raise Out_of_memory
         | Stack_overflow -> raise Stack_overflow
         | Sys.Break -> raise Sys.Break
         | Eio.Cancel.Cancelled _ as ex -> raise ex
         | exn ->
           let msg =
             Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn)
           in
           { tool_use_id = id
           ; tool_name = name
           ; input
           ; content = msg
           ; outcome =
               Tool_failed
                 { failure_kind = Non_retryable_tool_error
                 ; error_class = Some Types.Unknown
                 }
           })
  in
  let duration_ms_tool = (Unix.gettimeofday () -. t0_tool) *. 1000.0 in
  (match journal with
   | Some j ->
     Durable_event.append
       j
       (Tool_completed
          { turn = turn_count
          ; tool_name = name
          ; idempotency_key = idem_key
          ; output_json = `String triple.content
          ; is_error = Types.tool_result_outcome_is_error triple.outcome
          ; duration_ms = duration_ms_tool
          ; timestamp = Unix.gettimeofday ()
          })
   | None -> ());
  (match on_tool_execution_finished with
   | Some callback ->
     callback
       ~tool_use_id:id
       ~tool_name:name
       ~content:triple.content
       ~is_error:(Types.tool_result_outcome_is_error triple.outcome)
   | None -> ());
  index, triple
;;

let execute_tools
      ~context
      ~tools
      ~(hooks : Hooks.hooks)
      ~event_bus
      ?journal
      ~tracer
      ~agent_name
      ~turn_count
      ~(usage : Types.usage_stats)
      ~approval
      ?correlation_id
      ?run_id
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      tool_uses
  =
  let tool_index = build_index tools in
  let tool_use_blocks =
    List.filter_map
      (fun (block : Types.content_block) ->
         match block with
         | ToolUse { id; name; input } -> Some (id, name, input)
         | Text _
         | Thinking _
         | ReasoningDetails _
         | RedactedThinking _
         | ToolResult _
         | Image _
         | Document _
         | Audio _ ->
           (* Only [ToolUse] blocks dispatch tools. Enumerated explicitly
              so a new [content_block] variant cannot inherit "no tool"
              behavior without review. *)
           None)
      tool_uses
  in
  let scheduled = List.mapi (schedule_tool_use ~tool_index) tool_use_blocks in
  let run_one =
    execute_scheduled_tool
      ~context
      ~tools
      ~tool_index
      ~hooks
      ~event_bus
      ?journal
      ~tracer
      ~agent_name
      ~turn_count
      ~usage
      ~approval
      ?correlation_id
      ?run_id
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
  in
  execution_batches scheduled
  |> List.mapi (fun batch_index batch ->
    match batch with
    | Serial_batch tool_use ->
      let schedule = hook_schedule_of_tool_use ~batch_index ~batch_size:1 tool_use in
      [ run_one ~schedule tool_use ]
    | Concurrent_batch tool_uses ->
      let batch_size = List.length tool_uses in
      tool_uses
      |> List.map (fun tool_use ->
        let schedule = hook_schedule_of_tool_use ~batch_index ~batch_size tool_use in
        tool_use, schedule)
      |> Eio.Fiber.List.map (fun (tool_use, schedule) -> run_one ~schedule tool_use))
  |> List.concat
  |> List.sort (fun (left_index, _) (right_index, _) ->
    Int.compare left_index right_index)
  |> List.map snd
;;
