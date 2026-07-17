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
  | Unattributed_tool_error

type tool_execution_result =
  { invocation : Tool.Invocation.t
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; content : string
  ; outcome : Types.tool_result_outcome
  }

type execution_error =
  | Hook_execution_failed of
      { hook_name : string
      ; stage : Hooks.hook_stage
      ; tool_name : string
      ; invocation : Tool.Invocation.t
      ; detail : string
      }

type execution_failure_cause =
  | Hook_failure of execution_error
  | Observer_failure of
      { invocation : Tool.Invocation.t
      ; exception_ : exn
      ; backtrace : Printexc.raw_backtrace
      }

type execution_failure =
  { completed_results : tool_execution_result list
  ; cause : execution_failure_cause
  }

let observer_failure ~invocation exception_ backtrace =
  Llm_provider.Reserved_exn.reraise_if_reserved exception_;
  Observer_failure { invocation; exception_; backtrace }
;;

let failure_summary = function
  | Hook_failure (Hook_execution_failed { hook_name; detail; _ }) ->
    Printf.sprintf "hook %s failed: %s" hook_name detail
  | Observer_failure { exception_; _ } -> Printexc.to_string exception_
;;

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
  let by_name = Hashtbl.create (List.length tools) in
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

let render_tool_names names =
  match names with
  | [] -> "(none)"
  | _ -> String.concat "," names
;;

let unknown_tool_failure ~requested ~available =
  let available_preview = render_tool_names available in
  ( Printf.sprintf "Tool not found: %s. Available tools: %s" requested available_preview
  , Validation_error )
;;

let resolve_tool_call tool_index name input = name, input, find_in_index tool_index name

let tool_failure_result ~invocation ~name ~input ~content ~error_class =
  { invocation
  ; tool_name = name
  ; input
  ; content
  ; outcome =
      Tool_failed
        { failure_kind = Non_retryable_tool_error; error_class = Some error_class }
  }
;;

let blocked_tool_result ~invocation ~name ~input ~content =
  tool_failure_result ~invocation ~name ~input ~content ~error_class:Types.Deterministic
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
  : Tool.schedule
  =
  { planned_index = tool_use.index
  ; batch_index
  ; batch_size
  ; execution_mode = tool_use.execution_mode
  }
;;

exception Hook_observer_raised of exn * Printexc.raw_backtrace

let raise_hook_observer exn =
  let backtrace = Printexc.get_raw_backtrace () in
  raise (Hook_observer_raised (exn, backtrace))
;;

let reraise_hook_observer exn backtrace = Printexc.raise_with_backtrace exn backtrace

let invoke_hook ?on_hook_invoked ~tracer ~agent_name ~turn_count ~hook_name hook_opt event
  =
  try
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
            (try
               callback
                 ~invocation:(Hooks.invocation_of_event event)
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
             with
             | exn -> raise_hook_observer exn)
          | None -> ());
         decision)
  with
  | Hook_observer_raised _ as exn -> raise exn
  | exn ->
    Llm_provider.Reserved_exn.reraise_if_reserved exn;
    (* A user-hook exception is converted to [HookFailed] inside
       [Hooks.invoke_validated]. Anything escaping the tracing envelope is
       therefore an observer/tracer failure and must reach the caller. *)
    raise_hook_observer exn
;;

type deferred_failure = execution_failure_cause

type tool_dispatch =
  { result : tool_execution_result
  ; deferred_failure : deferred_failure option
  }

let hook_execution_error ~hook_name ~stage ~tool_name ~invocation ~detail =
  Hook_execution_failed { hook_name; stage; tool_name; invocation; detail }
;;

let invoke_post_hook
      ?on_hook_invoked
      ~tracer
      ~agent_name
      ~invocation
      ~hook_name
      ~tool_name
      hook_opt
      event
  =
  let turn_count = Tool.Invocation.turn invocation in
  try
    match
      invoke_hook
        ?on_hook_invoked
        ~tracer
        ~agent_name
        ~turn_count
        ~hook_name
        hook_opt
        event
    with
    | Hooks.Continue -> None
    | Hooks.HookFailed { stage; detail } ->
      Some
        (Hook_failure
           (hook_execution_error ~hook_name ~stage ~tool_name ~invocation ~detail))
    | decision ->
      let stage = Hooks.stage_of_event event in
      Some
        (Hook_failure
           (hook_execution_error
              ~hook_name
              ~stage
              ~tool_name
              ~invocation
              ~detail:
                (Printf.sprintf
                   "illegal decision %s escaped hook validation"
                   (Hooks.decision_kind_to_string (Hooks.classify_decision decision)))))
  with
  | Hook_observer_raised (exception_, backtrace) ->
    Llm_provider.Reserved_exn.reraise_if_reserved exception_;
    Some (Observer_failure { invocation; exception_; backtrace })
;;

let resolve_dispatch dispatch =
  match dispatch.deferred_failure with
  | None -> Ok dispatch.result
  | Some (Hook_failure error) -> Error error
  | Some (Observer_failure { exception_; backtrace; _ }) ->
    reraise_hook_observer exception_ backtrace
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
      ?correlation_id
      ?run_id
      ?on_hook_invoked
      ~invocation
      name
      input
  =
  let id = Tool.Invocation.tool_use_id invocation in
  let requested_name = name in
  let name, input, tool_opt = resolve_tool_call tool_index name input in
  let first_deferred_failure = ref None in
  let record_deferred_failure failure =
    match !first_deferred_failure with
    | None -> first_deferred_failure := Some failure
    | Some primary ->
      let retained, suppressed =
        match primary, failure with
        | Hook_failure _, Observer_failure _ ->
          first_deferred_failure := Some failure;
          failure, primary
        | _ -> primary, failure
      in
      Log.error
        _log
        "additional tool dispatch failure"
        [ Log.S ("tool", name)
        ; Log.S ("tool_use_id", id)
        ; Log.S ("retained", failure_summary retained)
        ; Log.S ("suppressed", failure_summary suppressed)
        ]
  in
  let record_deferred_exception exception_ backtrace =
    record_deferred_failure (observer_failure ~invocation exception_ backtrace)
  in
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
          (ToolCalled { invocation; agent_name; tool_name = name; input })
      in
      (try
         Event_bus.publish bus ev;
         Some ev.meta.run_id
       with
       | exception_ ->
         let backtrace = Printexc.get_raw_backtrace () in
         record_deferred_exception exception_ backtrace;
         None)
    | None -> None
  in
  let dispatch =
    match tool_opt with
    | Some tool ->
      let validation_error_result ~input message =
        { invocation
        ; tool_name = name
        ; input
        ; content = message
        ; outcome =
            Tool_failed
              { failure_kind = Validation_error; error_class = Some Types.Deterministic }
        }
      in
      let emit_post_tool_use_failure ~input message =
        invoke_post_hook
          ?on_hook_invoked
          ~tracer
          ~agent_name
          ~invocation
          ~hook_name:"post_tool_use_failure"
          ~tool_name:name
          hooks.post_tool_use_failure
          (Hooks.PostToolUseFailure
             { invocation; tool_name = name; input; error = message })
      in
      (* Tool inputs cross this boundary unchanged. The schema either accepts
           the exact JSON value or produces a typed validation failure for the
           model to correct in a later native ToolUse block. *)
      let validated_input =
        match Tool_input_validation.validate tool.schema input with
        | Tool_input_validation.Valid exact_input -> Ok exact_input
        | Tool_input_validation.Invalid errors ->
          let message =
            Tool_input_validation.format_errors_inline ~tool_name:name ~args:input errors
          in
          Error (message, emit_post_tool_use_failure ~input message)
      in
      (match validated_input with
       | Error (message, deferred_failure) ->
         Option.iter record_deferred_failure deferred_failure;
         { result = validation_error_result ~input message; deferred_failure = None }
       | Ok exact_input ->
         let t0 = Unix.gettimeofday () in
         let result =
           try Tool.execute ~context ~invocation tool exact_input with
           | exn ->
             Llm_provider.Reserved_exn.reraise_if_reserved exn;
             Error
               { message =
                   Printf.sprintf "Tool '%s' raised: %s" name (Printexc.to_string exn)
               ; recoverable = false
               ; error_class = Some Types.Unknown
               }
         in
         let duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
         let result_bytes =
           match result with
           | Ok { content; _meta = _ } -> String.length content
           | Error { message; _ } -> String.length message
         in
         invoke_post_hook
           ?on_hook_invoked
           ~tracer
           ~agent_name
           ~invocation
           ~hook_name:"post_tool_use"
           ~tool_name:name
           hooks.post_tool_use
           (Hooks.PostToolUse
              { invocation
              ; tool_name = name
              ; input = exact_input
              ; output = result
              ; result_bytes
              ; duration_ms
              })
         |> Option.iter record_deferred_failure;
         (match result with
          | Ok _ -> ()
          | Error { message; _ } ->
            invoke_post_hook
              ?on_hook_invoked
              ~tracer
              ~agent_name
              ~invocation
              ~hook_name:"post_tool_use_failure"
              ~tool_name:name
              hooks.post_tool_use_failure
              (Hooks.PostToolUseFailure
                 { invocation; tool_name = name; input = exact_input; error = message })
            |> Option.iter record_deferred_failure;
            (* [OnToolError] is the compact error projection. The exact
               invocation remains shared with the richer failure hook. *)
            invoke_post_hook
              ?on_hook_invoked
              ~tracer
              ~agent_name
              ~invocation
              ~hook_name:"on_tool_error"
              ~tool_name:name
              hooks.on_tool_error
              (Hooks.OnToolError { invocation; tool_name = name; error = message })
            |> Option.iter record_deferred_failure);
         let content, outcome =
           match result with
           | Ok { content; _meta = _ } -> content, Tool_succeeded
           | Error { message; recoverable; error_class } ->
             let failure_kind =
               if recoverable then Recoverable_tool_error else Non_retryable_tool_error
             in
             message, Tool_failed { failure_kind; error_class }
         in
         { result =
             { invocation; tool_name = name; input = exact_input; content; outcome }
         ; deferred_failure = None
         })
    | None ->
      (* Tool dispatch failure (the LLM asked for a tool that isn't
         registered). Distinct from OnToolError — that fires when a
         tool actually ran and returned Error. This is a configuration
         / routing mistake, so it belongs on the general OnError
         channel. Unknown names are always typed as validation errors; the
         available names remain diagnostic data only. *)
      let available = tool_names_of_index tool_index in
      let message, failure_kind =
        unknown_tool_failure ~requested:requested_name ~available
      in
      Log.warn
        _log
        "tool not found"
        [ Log.S ("tool", requested_name)
        ; Log.S ("available_tools", render_tool_names available)
        ];
      let deferred_failure =
        invoke_post_hook
          ?on_hook_invoked
          ~tracer
          ~agent_name
          ~invocation
          ~hook_name:"on_error"
          ~tool_name:requested_name
          hooks.on_error
          (Hooks.OnError
             { invocation = Some invocation
             ; detail = message
             ; context = "agent_tools.find_and_execute_tool"
             })
      in
      Option.iter record_deferred_failure deferred_failure;
      { result =
          { invocation
          ; tool_name = requested_name
          ; input
          ; content = message
          ; outcome = Tool_failed { failure_kind; error_class = Some Types.Deterministic }
          }
      ; deferred_failure = None
      }
  in
  (* ToolCompleted event *)
  (match event_bus with
   | Some bus ->
     let output_content = dispatch.result.content in
     let output =
       Types.tool_result_of_outcome ~content:output_content dispatch.result.outcome
     in
     (try
        Event_bus.publish
          bus
          (Event_bus.mk_event
             ?correlation_id
             ?run_id
             ?caused_by:tool_called_run_id
             (ToolCompleted { invocation; agent_name; tool_name = name; output }))
      with
      | exception_ ->
        let backtrace = Printexc.get_raw_backtrace () in
        record_deferred_exception exception_ backtrace)
   | None -> ());
  { dispatch with deferred_failure = !first_deferred_failure }
;;

let find_and_execute_tool
      ~context
      ~tools
      ~(hooks : Hooks.hooks)
      ~event_bus
      ~tracer
      ~agent_name
      ?correlation_id
      ?run_id
      ?on_hook_invoked
      ~invocation
      name
      input
  =
  let tool_index = build_index tools in
  try
    find_and_execute_tool_with_index
      ~context
      ~tool_index
      ~hooks
      ~event_bus
      ~tracer
      ~agent_name
      ?correlation_id
      ?run_id
      ?on_hook_invoked
      ~invocation
      name
      input
    |> resolve_dispatch
  with
  | Hook_observer_raised (exn, backtrace) -> reraise_hook_observer exn backtrace
;;

type scheduled_tool_outcome =
  { index : int
  ; completed_result : tool_execution_result option
  ; failure : execution_failure_cause option
  }

exception Abort_tool_dispatch

let append_journal journal event =
  match Durable_event.append journal event with
  | Ok () -> ()
  | Error { exception_; backtrace } -> Printexc.raise_with_backtrace exception_ backtrace
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
      ?correlation_id
      ?run_id
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      ~(schedule : Tool.schedule)
      (tool_use : scheduled_tool_use)
  =
  let { index; id; name; input; _ } = tool_use in
  let invocation = Tool.Invocation.create ~tool_use_id:id ~turn:turn_count ~schedule in
  let id = Tool.Invocation.tool_use_id invocation in
  let turn_count = Tool.Invocation.turn invocation in
  let completed_dispatch = ref None in
  let first_failure = ref None in
  let record_failure failure =
    match !first_failure with
    | None -> first_failure := Some failure
    | Some primary ->
      let retained, suppressed =
        match primary, failure with
        | Hook_failure _, Observer_failure _ ->
          first_failure := Some failure;
          failure, primary
        | _ -> primary, failure
      in
      Log.error
        _log
        "additional tool execution failure"
        [ Log.S ("tool", name)
        ; Log.S ("tool_use_id", id)
        ; Log.S ("retained", failure_summary retained)
        ; Log.S ("suppressed", failure_summary suppressed)
        ]
  in
  let record_caught_exception exception_ backtrace =
    record_failure (observer_failure ~invocation exception_ backtrace)
  in
  let observe_before_completion callback =
    try callback () with
    | Hook_observer_raised (exception_, backtrace) ->
      record_caught_exception exception_ backtrace;
      raise Abort_tool_dispatch
    | exception_ ->
      let backtrace = Printexc.get_raw_backtrace () in
      record_caught_exception exception_ backtrace;
      raise Abort_tool_dispatch
  in
  let observe_after_completion callback =
    try callback () with
    | Hook_observer_raised (exception_, backtrace) ->
      record_caught_exception exception_ backtrace
    | exception_ ->
      let backtrace = Printexc.get_raw_backtrace () in
      record_caught_exception exception_ backtrace
  in
  let outcome () =
    { index
    ; completed_result = Option.map (fun dispatch -> dispatch.result) !completed_dispatch
    ; failure = !first_failure
    }
  in
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
           { invocation
           ; tool_name = name
           ; input
           ; accumulated_cost_usd = usage.Types.estimated_cost_usd
           })
    in
    match decision with
    | Hooks.Block reason ->
      (* Intentional caller rejection is a model-visible tool result, but it is
         not a tool execution. No Tool_called/Tool_completed lifecycle evidence
         is emitted for a call that never crossed the caller's gate. *)
      { index
      ; completed_result =
          Some (blocked_tool_result ~invocation ~name ~input ~content:reason)
      ; failure = None
      }
    | Hooks.HookFailed { stage; detail } ->
      { index
      ; completed_result = None
      ; failure =
          Some
            (Hook_failure
               (hook_execution_error
                  ~hook_name:"pre_tool_use"
                  ~stage
                  ~tool_name:name
                  ~invocation
                  ~detail))
      }
    | (Hooks.AdjustParams _ | Hooks.ElicitInput _ | Hooks.Nudge _) as decision ->
      { index
      ; completed_result = None
      ; failure =
          Some
            (Hook_failure
               (hook_execution_error
                  ~hook_name:"pre_tool_use"
                  ~stage:Hooks.Pre_tool_use
                  ~tool_name:name
                  ~invocation
                  ~detail:
                    (Printf.sprintf
                       "illegal decision %s escaped hook validation"
                       (Hooks.decision_kind_to_string (Hooks.classify_decision decision)))))
      }
    | Hooks.Continue ->
      let idem_key = Durable_event.make_idempotency_key ~tool_name:name ~input in
      (try
         let dispatch =
           Tracing.with_span
             tracer
             { kind = Tool_exec
             ; name
             ; agent_name
             ; turn = turn_count
             ; extra = []
             ; links = []
             }
             (fun _tracer ->
                observe_before_completion (fun () ->
                  match journal with
                  | Some j ->
                    append_journal
                      j
                      (Tool_called
                         { turn = Tool.Invocation.turn invocation
                         ; tool_name = name
                         ; idempotency_key = idem_key
                         ; input_hash = Digest.string (Yojson.Safe.to_string input)
                         ; timestamp = Unix.gettimeofday ()
                         })
                  | None -> ());
                observe_before_completion (fun () ->
                  match on_tool_execution_started with
                  | Some callback -> callback ~invocation ~tool_name:name ~input
                  | None -> ());
                let t0_tool = Unix.gettimeofday () in
                let dispatch =
                  find_and_execute_tool_with_index
                    ~context
                    ~tool_index
                    ~hooks
                    ~event_bus
                    ~tracer
                    ~agent_name
                    ?correlation_id
                    ?run_id
                    ?on_hook_invoked
                    ~invocation
                    name
                    input
                in
                completed_dispatch := Some dispatch;
                Option.iter record_failure dispatch.deferred_failure;
                let duration_ms_tool = (Unix.gettimeofday () -. t0_tool) *. 1000.0 in
                observe_after_completion (fun () ->
                  match journal with
                  | Some j ->
                    append_journal
                      j
                      (Tool_completed
                         { turn = Tool.Invocation.turn invocation
                         ; tool_name = name
                         ; idempotency_key = idem_key
                         ; output_json = `String dispatch.result.content
                         ; is_error =
                             Types.tool_result_outcome_is_error dispatch.result.outcome
                         ; duration_ms = duration_ms_tool
                         ; timestamp = Unix.gettimeofday ()
                         })
                  | None -> ());
                observe_after_completion (fun () ->
                  match on_tool_execution_finished with
                  | Some callback ->
                    callback
                      ~invocation
                      ~tool_name:name
                      ~content:dispatch.result.content
                      ~is_error:
                        (Types.tool_result_outcome_is_error dispatch.result.outcome)
                  | None -> ());
                dispatch)
         in
         completed_dispatch := Some dispatch;
         outcome ()
       with
       | Abort_tool_dispatch -> outcome ()
       | Hook_observer_raised (exception_, backtrace) ->
         record_caught_exception exception_ backtrace;
         outcome ()
       | exception_ ->
         let backtrace = Printexc.get_raw_backtrace () in
         record_caught_exception exception_ backtrace;
         outcome ())
  with
  | Hook_observer_raised (exception_, backtrace) ->
    record_caught_exception exception_ backtrace;
    outcome ()
  | exception_ ->
    let backtrace = Printexc.get_raw_backtrace () in
    record_caught_exception exception_ backtrace;
    outcome ()
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
      ?correlation_id
      ?run_id
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
  in
  let collect_batch outcomes =
    let completed =
      List.filter_map
        (fun outcome ->
           Option.map (fun result -> outcome.index, result) outcome.completed_result)
        outcomes
    in
    let failure = List.find_map (fun outcome -> outcome.failure) outcomes in
    completed, failure
  in
  let ordered_results completed =
    completed
    |> List.sort (fun (left_index, _) (right_index, _) ->
      Int.compare left_index right_index)
    |> List.map snd
  in
  let rec run_batches batch_index completed = function
    | [] -> Ok (ordered_results completed)
    | batch :: rest ->
      let batch_results =
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
          |> Eio.Fiber.List.map (fun (tool_use, schedule) -> run_one ~schedule tool_use)
      in
      let batch_completed, failure = collect_batch batch_results in
      let completed = List.rev_append batch_completed completed in
      (match failure with
       | Some cause -> Error { completed_results = ordered_results completed; cause }
       | None -> run_batches (batch_index + 1) completed rest)
  in
  run_batches 0 [] (execution_batches scheduled)
;;
