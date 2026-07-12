(** Tool execution helpers — lookup, hooks, event_bus, parallel Eio fibers.

    These functions are parameterized by explicit fields rather than Agent.t
    to avoid circular module dependencies (Agent_tools is compiled before Agent). *)

open Types

let _log = Log.create ~module_name:"agent_tools" ()

type tool_failure_kind = Types.tool_failure_kind =
  | Validation_error
  | Recoverable_tool_error
  | Non_retryable_tool_error

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
  ; concurrency_class : Tool.concurrency_class
  }

type execution_batch =
  | Parallel_batch of scheduled_tool_use list
  | Sequential_batch of scheduled_tool_use
  | Exclusive_batch of scheduled_tool_use

type tool_index =
  { by_id : (Tool_id.t, Tool.t) Hashtbl.t
  ; by_name : (string, Tool.t) Hashtbl.t
  }

let add_first tbl key value = if not (Hashtbl.mem tbl key) then Hashtbl.add tbl key value

let build_index tools =
  let capacity = max 16 (List.length tools * 2) in
  let by_id = Hashtbl.create capacity in
  let by_name = Hashtbl.create capacity in
  List.iter
    (fun (tool : Tool.t) ->
       let name = tool.schema.name in
       add_first by_name name tool;
       add_first by_id (Tool_id.of_string name) tool)
    tools;
  { by_id; by_name }
;;

let find_in_index index name =
  match Hashtbl.find_opt index.by_name name with
  | Some _ as found -> found
  | None ->
    (* Fallback applies the case-insensitive [Tool_id.of_string] mapping so
       canonical built-ins and MCP IDs still resolve when the caller emits a
       case-variant. For user-defined tools, [Tool_id.of_string] lowercases the
       raw name into [User _], which would let a case-variant call dispatch a
       *different* user tool (e.g. caller "MyTool" → user "mytool" when only
       the latter is registered). To preserve approval/audit behavior, gate
       the fallback so user IDs require an exact [by_name] match. *)
    (match Tool_id.of_string name with
     | Tool_id.User _ -> None
     | id -> Hashtbl.find_opt index.by_id id)
;;

let concurrency_class_from_descriptor (descriptor : Tool.descriptor) =
  (* Scheduling-time classification must not raise: descriptors that did not
     pass through [Tool.create] (e.g., record-constructed or deserialized
     tools) should still schedule safely. Validation belongs at construction
     time, not during batch scheduling. *)
  match descriptor.Tool.concurrency_class with
  | Some cc -> cc
  | None ->
    (match
       Option.bind
         descriptor.Tool.mutation_class
         Tool.expected_concurrency_class_of_mutation_class
     with
     | Some inferred -> inferred
     | None -> Tool.Sequential_workspace)
;;

(* RFC-OAS-009 v2 PR-B: removed CDAL builtin_descriptor fallback.
   Tools without descriptor fall through to Sequential_workspace
   (fail-closed). Consumers should supply Tool.descriptor at
   construction time so the agent runtime never reaches into the
   CDAL builtin registry to classify concurrency. *)
let concurrency_class_of_tool tool =
  match Tool.descriptor tool with
  | Some descriptor -> concurrency_class_from_descriptor descriptor
  | None -> Tool.Sequential_workspace
;;

let json_object_keys_for_log = function
  | `Assoc fields ->
    fields |> List.map fst |> List.sort_uniq String.compare |> String.concat ","
  | _ -> "-"
;;

let tool_names_of_index index =
  Hashtbl.fold (fun name _ acc -> name :: acc) index.by_name []
  |> List.sort_uniq String.compare
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

let resolve_tool_call tool_index name input =
  match find_in_index tool_index name with
  | Some tool -> name, input, Some tool, None
  | None ->
    (match Agent_tool_name_alias.resolve ~requested:name ~input with
     | Some (alias, aliased_input) ->
       (match find_in_index tool_index alias with
        | Some tool -> alias, aliased_input, Some tool, Some alias
        | None -> name, input, None, None)
     | None -> name, input, None, None)
;;

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

let protect_tool_lifecycle_callback ~tool_name ~callback_name f =
  try f () with
  | Out_of_memory -> raise Out_of_memory
  | Stack_overflow -> raise Stack_overflow
  | Sys.Break -> raise Sys.Break
  | Eio.Cancel.Cancelled _ as ex -> raise ex
  | exn ->
    Log.warn
      _log
      "tool lifecycle callback raised"
      [ Log.S ("tool", tool_name)
      ; Log.S ("callback", callback_name)
      ; Log.S ("error", Printexc.to_string exn)
      ]
;;

let approval_required_without_callback_result ~id ~name ~input =
  let reason = "approval required but no approval callback is registered" in
  blocked_tool_result ~id ~name ~input ~content:("Tool rejected: " ^ reason)
;;

let schedule_tool_use ~tool_index index (id, name, input) =
  let concurrency_class =
    match find_in_index tool_index name with
    | Some tool -> concurrency_class_of_tool tool
    | None -> Tool.Sequential_workspace
  in
  { index; id; name; input; concurrency_class }
;;

let execution_batches tool_uses =
  let flush_parallel acc = function
    | [] -> acc
    | parallel_tools -> Parallel_batch (List.rev parallel_tools) :: acc
  in
  let rec build acc current_parallel = function
    | [] -> List.rev (flush_parallel acc current_parallel)
    | tool_use :: rest ->
      (match tool_use.concurrency_class with
       | Tool.Parallel_read -> build acc (tool_use :: current_parallel) rest
       | Tool.Sequential_workspace ->
         let acc = flush_parallel acc current_parallel in
         build (Sequential_batch tool_use :: acc) [] rest
       | Tool.Exclusive_external ->
         let acc = flush_parallel acc current_parallel in
         build (Exclusive_batch tool_use :: acc) [] rest)
  in
  build [] [] tool_uses
;;

let hook_schedule_of_tool_use
      ~batch_index
      ~batch_size
      ~batch_kind
      (tool_use : scheduled_tool_use)
  : Hooks.tool_schedule
  =
  { planned_index = tool_use.index
  ; batch_index
  ; batch_size
  ; concurrency_class = Tool.concurrency_class_name tool_use.concurrency_class
  ; batch_kind
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
               | Hooks.OnIdle _
               | Hooks.OnIdleEscalated _
               | Hooks.OnError _
               | Hooks.OnToolError _
               | Hooks.PreCompact _
               | Hooks.PostCompact _
               | Hooks.OnContextCompacted _ -> None)
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
  let name, input, tool_opt, alias_opt = resolve_tool_call tool_index name input in
  (match alias_opt with
   | Some alias ->
     Log.info
       _log
       "provider tool name alias resolved"
       [ Log.S ("requested_tool", requested_name); Log.S ("resolved_tool", alias) ]
   | None -> ());
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
        (* Multi-stage deterministic correction before execution.
       Correction_pipeline runs safe default stages (type coercion and format
       normalization) then validates. Optional zero-default injection is
       opt-in because adding absent optional fields can change a tool-call
       union branch. If det correction fixes the input, skip the LLM retry
       path entirely. If still invalid, fall back to
       Tool_middleware.validate_and_coerce for structured error feedback.
       Ref: Samchon function calling harness (6.75% → 100%). *)
        let validated_input =
          match Correction_pipeline.run ~schema:tool.schema input with
          | Correction_pipeline.Fixed { corrected; corrections } ->
            if corrections <> []
            then (
              let field_names =
                corrections
                |> List.map (fun (c : Correction_pipeline.correction) -> c.field)
                |> List.sort_uniq String.compare
                |> String.concat ","
              in
              let stage_names =
                corrections
                |> List.map (fun (c : Correction_pipeline.correction) -> c.stage)
                |> List.sort_uniq String.compare
                |> String.concat ","
              in
              let added_fields =
                corrections
                |> List.filter_map (fun (c : Correction_pipeline.correction) ->
                  match c.from_value with
                  | None -> Some c.field
                  | Some _ -> None)
                |> List.sort_uniq String.compare
                |> String.concat ","
              in
              let changed_fields =
                corrections
                |> List.filter_map (fun (c : Correction_pipeline.correction) ->
                  match c.from_value with
                  | Some _ -> Some c.field
                  | None -> None)
                |> List.sort_uniq String.compare
                |> String.concat ","
              in
              Log.info
                _log
                "correction_pipeline fixed tool input fields"
                [ Log.S ("tool", name)
                ; Log.I ("fixes", List.length corrections)
                ; Log.S ("fields", field_names)
                ; Log.S ("stages", stage_names)
                ; Log.S ("input_keys", json_object_keys_for_log input)
                ; Log.S ("corrected_keys", json_object_keys_for_log corrected)
                ; Log.S ("added_fields", added_fields)
                ; Log.S ("changed_fields", changed_fields)
                ]);
            Ok corrected
          | Correction_pipeline.Still_invalid { corrected; errors; attempted } ->
            Log.warn
              _log
              "correction_pipeline still invalid after deterministic fixes"
              [ Log.S ("tool", name)
              ; Log.I ("error_count", List.length errors)
              ; Log.I ("attempted_corrections", List.length attempted)
              ];
            (* Det correction insufficient — build structured feedback for the
           turn-level retry policy (pipeline Stage 5) to relay to the LLM. *)
            let message =
              Correction_pipeline.build_nondet_feedback
                ~tool_name:name
                ~args:corrected
                ~still_invalid:errors
                ~attempted
            in
            emit_post_tool_use_failure ~input:corrected message;
            Error (corrected, message)
        in
        (match validated_input with
         | Error (corrected_input, msg) ->
           validation_error_result ~input:corrected_input msg
         | Ok coerced_input ->
           let shell_constraint_result =
             match Tool.descriptor tool with
             | None -> Tool_middleware.Pass
             | Some descriptor ->
               Tool_middleware.validate_shell_constraints
                 ~tool_name:name
                 ~descriptor
                 coerced_input
           in
           (match shell_constraint_result with
            | Tool_middleware.Reject { message; _ } ->
              emit_post_tool_use_failure ~input:coerced_input message;
              validation_error_result ~input:coerced_input message
            | Tool_middleware.Pass | Tool_middleware.Proceed _ ->
              let t0 = Unix.gettimeofday () in
              let result = Tool.execute ~context tool coerced_input in
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
                     ; input = coerced_input
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
                         ; input = coerced_input
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
                    if recoverable
                    then Recoverable_tool_error
                    else Non_retryable_tool_error
                  in
                  message, Tool_failed { failure_kind; error_class }
              in
              { tool_use_id = id
              ; tool_name = name
              ; input = coerced_input
              ; content
              ; outcome
              }))
        (* Tool_middleware validation match *)
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
      ~missing_approval_callback_policy
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
   | Some callback ->
     protect_tool_lifecycle_callback ~tool_name:name ~callback_name:"started" (fun () ->
       callback ~tool_use_id:id ~tool_name:name ~input ~schedule)
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
           | Hooks.Skip ->
             { tool_use_id = id
             ; tool_name = name
             ; input
             ; content = "Tool execution skipped by hook"
             ; outcome = Tool_succeeded
             }
           | Hooks.Override value ->
             { tool_use_id = id
             ; tool_name = name
             ; input
             ; content = value
             ; outcome = Tool_succeeded
             }
           | Hooks.ApprovalRequired ->
             (match approval with
              | None ->
                (match missing_approval_callback_policy with
                 | Hooks.Execute_without_callback ->
                   Log.debug
                     _log
                     "ApprovalRequired but no approval callback — executing"
                     [ Log.S ("tool", name); Log.S ("agent", agent_name) ];
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
                 | Hooks.Reject_without_callback ->
                   Log.warn
                     _log
                     "ApprovalRequired but no approval callback — rejecting"
                     [ Log.S ("tool", name); Log.S ("agent", agent_name) ];
                   approval_required_without_callback_result ~id ~name ~input)
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
                content verbatim. Distinct from [Hooks.Override] (soft nudge,
                is_error=false) and [Hooks.HookFailed] (infra failure). *)
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
     protect_tool_lifecycle_callback ~tool_name:name ~callback_name:"finished" (fun () ->
       callback
         ~tool_use_id:id
         ~tool_name:name
         ~content:triple.content
         ~is_error:(Types.tool_result_outcome_is_error triple.outcome))
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
      ~missing_approval_callback_policy
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
      ~missing_approval_callback_policy
      ?correlation_id
      ?run_id
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
  in
  execution_batches scheduled
  |> List.mapi (fun batch_index batch ->
    match batch with
    | Sequential_batch tool_use ->
      let schedule =
        hook_schedule_of_tool_use
          ~batch_index
          ~batch_size:1
          ~batch_kind:"sequential"
          tool_use
      in
      [ run_one ~schedule tool_use ]
    | Exclusive_batch tool_use ->
      let schedule =
        hook_schedule_of_tool_use
          ~batch_index
          ~batch_size:1
          ~batch_kind:"exclusive"
          tool_use
      in
      [ run_one ~schedule tool_use ]
    | Parallel_batch tool_uses ->
      let batch_size = List.length tool_uses in
      tool_uses
      |> List.map (fun tool_use ->
        let schedule =
          hook_schedule_of_tool_use
            ~batch_index
            ~batch_size
            ~batch_kind:"parallel"
            tool_use
        in
        tool_use, schedule)
      |> Eio.Fiber.List.map (fun (tool_use, schedule) -> run_one ~schedule tool_use))
  |> List.concat
  |> List.sort (fun (left_index, _) (right_index, _) ->
    Int.compare left_index right_index)
  |> List.map snd
;;
