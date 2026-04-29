open Types
open Agent_types
open Agent_trace

(** Stage 1/2 helpers extracted from [Pipeline].

    These functions stay internal to the library. [Pipeline] re-exports them
    via local aliases so existing tests and call sites remain unchanged while
    the file is split by stage responsibility. *)

let stage_input ?raw_trace_run agent =
  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~ready_at:ts Ready;
  let before_decision =
    invoke_hook_with_trace
      agent
      ?raw_trace_run
      ~hook_name:"before_turn"
      agent.options.hooks.before_turn
      (Hooks.BeforeTurn { turn = agent.state.turn_count; messages = agent.state.messages })
  in
  match before_decision with
  | Hooks.ElicitInput req ->
    (match agent.options.elicitation with
     | Some cb ->
       let response = cb req in
       (match agent.options.event_bus with
        | Some bus ->
          Event_bus.publish
            bus
            (Event_bus.mk_event
               (ElicitationCompleted
                  { agent_name = agent.state.config.name
                  ; question = req.question
                  ; response
                  }))
        | None -> ());
       (match response with
        | Hooks.Answer json ->
          let text =
            Printf.sprintf "[User input] %s: %s" req.question (Yojson.Safe.to_string json)
          in
          update_state agent (fun s ->
            { s with
              messages =
                Util.snoc
                  s.messages
                  { role = User
                  ; content = [ Text text ]
                  ; name = None
                  ; tool_call_id = None
                  ; metadata = []
                  }
            })
        | Hooks.Declined | Hooks.Timeout -> ())
     | None -> ())
  | Hooks.Nudge nudge_msg ->
    (* Keep BeforeTurn nudge behavior identical to the inlined pipeline path:
         append it as a User message so it is seen in this same turn. *)
    update_state agent (fun s ->
      { s with
        messages =
          Util.snoc
            s.messages
            { role = User
            ; content = [ Text nudge_msg ]
            ; name = None
            ; tool_call_id = None
            ; metadata = []
            }
      })
  | _ -> ()
;;

let last_tool_results_from messages =
  let extract_results msg =
    if msg.role <> User
    then []
    else
      List.filter_map
        (function
          | ToolResult { content; is_error; _ } ->
            if is_error
            then
              Some
                (Error { Types.message = content; recoverable = true; error_class = None }
                 : Types.tool_result)
            else Some (Ok { Types.content } : Types.tool_result)
          | _ -> None)
        msg.content
  in
  List.fold_left
    (fun acc msg ->
       match extract_results msg with
       | [] -> acc
       | results -> results)
    []
    messages
;;

let prepare_turn_for_agent agent ~turn_params =
  Agent_turn.prepare_turn
    ~guardrails:agent.options.guardrails
    ~operator_policy:agent.options.operator_policy
    ~policy_channel:agent.options.policy_channel
    ~tools:agent.tools
    ~messages:agent.state.messages
    ~context_reducer:agent.options.context_reducer
    ~tiered_memory:agent.options.tiered_memory
    ~turn_params
    ?tool_selector:agent.options.tool_selector
    ()
;;

let dedupe_preserve_order xs =
  let seen = Hashtbl.create (List.length xs) in
  List.filter
    (fun x ->
       if x = "" || Hashtbl.mem seen x
       then false
       else (
         Hashtbl.replace seen x ();
         true))
    xs
;;

let turn_ready_tool_names_from_policy ?runtime_mcp_policy visible_tool_names =
  let runtime_tool_names =
    match runtime_mcp_policy with
    | None -> []
    | Some policy -> policy.Llm_provider.Llm_transport.allowed_tool_names
  in
  dedupe_preserve_order (visible_tool_names @ runtime_tool_names)
;;

let turn_ready_tool_names agent (prep : Agent_turn.turn_preparation) =
  turn_ready_tool_names_from_policy
    ?runtime_mcp_policy:agent.options.runtime_mcp_policy
    prep.visible_tool_names
;;

let%test "turn_ready_tool_names includes runtime MCP policy names" =
  let runtime_mcp_policy =
    { Llm_provider.Llm_transport.empty_runtime_mcp_policy with
      allowed_tool_names = [ "masc_status"; "keeper_bash"; "inline_tool" ]
    }
  in
  turn_ready_tool_names_from_policy ~runtime_mcp_policy [ "inline_tool" ]
  = [ "inline_tool"; "masc_status"; "keeper_bash" ]
;;

let stage_parse ?raw_trace_run agent =
  let turn_params =
    match agent.options.hooks.before_turn_params with
    | None -> Hooks.default_turn_params
    | Some _ ->
      let last_results = last_tool_results_from agent.state.messages in
      let reasoning = Hooks.extract_reasoning agent.state.messages in
      let decision =
        invoke_hook_with_trace
          agent
          ?raw_trace_run
          ~hook_name:"before_turn_params"
          agent.options.hooks.before_turn_params
          (Hooks.BeforeTurnParams
             { turn = agent.state.turn_count
             ; max_turns = agent.state.config.max_turns
             ; messages = agent.state.messages
             ; last_tool_results = last_results
             ; current_params = Hooks.default_turn_params
             ; reasoning
             })
      in
      (match decision with
       | Hooks.AdjustParams params -> params
       | _ -> Hooks.default_turn_params)
  in
  let original_config = agent.state.config in
  let new_config =
    { original_config with
      temperature =
        (match turn_params.temperature with
         | Some _ as t -> t
         | None -> original_config.temperature)
    ; thinking_budget =
        (match turn_params.thinking_budget with
         | Some _ as t -> t
         | None -> original_config.thinking_budget)
    ; enable_thinking =
        (match turn_params.enable_thinking with
         | Some _ as t -> t
         | None -> original_config.enable_thinking)
    ; tool_choice =
        (match turn_params.tool_choice with
         | Some _ as t -> t
         | None -> original_config.tool_choice)
    ; system_prompt =
        (match turn_params.system_prompt_override with
         | Some _ as s -> s
         | None -> original_config.system_prompt)
        |> Option.map Llm_provider.Utf8_sanitize.sanitize
    }
  in
  update_state agent (fun s -> { s with config = new_config });
  let original_config = original_config in
  (match agent.options.event_bus with
   | Some bus ->
     Event_bus.publish
       bus
       { meta =
           Event_bus.mk_envelope
             ~correlation_id:
               (match Option.bind agent.options.raw_trace Raw_trace.session_id with
                | Some session_id -> session_id
                | None -> Event_bus.fresh_id ())
             ~run_id:
               (match
                  Option.bind (lifecycle_snapshot agent) (fun s -> s.current_run_id)
                with
                | Some run_id -> run_id
                | None -> Event_bus.fresh_id ())
             ()
       ; payload =
           TurnStarted
             { agent_name = agent.state.config.name; turn = agent.state.turn_count }
       }
   | None -> ());
  (match agent.options.journal with
   | Some j ->
     Durable_event.append
       j
       (Turn_started { turn = agent.state.turn_count; timestamp = Unix.gettimeofday () })
   | None -> ());
  let prep = prepare_turn_for_agent agent ~turn_params in
  let ready_tool_names = turn_ready_tool_names agent prep in
  (* TurnReady event — emitted after guardrails + operator policy +
     tool_filter_override + tool_selector have produced the final tool
     list the LLM will see this turn. CLI runtime-MCP tools are included
     from the request-scoped runtime policy because those tools bypass
     inline Tool.t schemas. Downstream substrate observability
     subscribers use this to verify deterministically
     which tools the autonomous agent actually has access to, before
     making claims about LLM behaviour from a missing tool call.
     Sibling of TurnStarted (announce) and TurnCompleted (post-LLM). *)
  (match agent.options.event_bus with
   | Some bus ->
     Event_bus.publish
       bus
       { meta =
           Event_bus.mk_envelope
             ~correlation_id:
               (match Option.bind agent.options.raw_trace Raw_trace.session_id with
                | Some session_id -> session_id
                | None -> Event_bus.fresh_id ())
             ~run_id:
               (match
                  Option.bind (lifecycle_snapshot agent) (fun s -> s.current_run_id)
                with
                | Some run_id -> run_id
                | None -> Event_bus.fresh_id ())
             ()
       ; payload =
           TurnReady
             { agent_name = agent.state.config.name
             ; turn = agent.state.turn_count
             ; tool_names = ready_tool_names
             }
       }
   | None -> ());
  prep, original_config, turn_params
;;
