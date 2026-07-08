open Types
open Agent_types
open Agent_trace
open Result_syntax

(** Stage 1/2 helpers extracted from [Pipeline].

    These functions stay internal to the library. [Pipeline] re-exports them
    via local aliases so existing tests and call sites remain unchanged while
    the file is split by stage responsibility. *)

let _stage_log = Log.create ~module_name:"pipeline_stage_prepare" ()

(* Shared with Pipeline via Pipeline_common (re-raises Eio cancellation);
   the thin wrapper keeps this module's log label. *)
let safe_publish bus event = Pipeline_common.safe_publish ~log:_stage_log bus event

let hook_failed_sdk_error ~hook_name ~stage ~detail =
  Error.Internal (Printf.sprintf "hook %s failed at %s: %s" hook_name stage detail)
;;

let illegal_hook_decision ~stage ~decision =
  Error.Internal
    (Printf.sprintf
       "illegal hook decision %s in %s"
       (Agent_lifecycle.hook_decision_to_string decision)
       stage)
;;

let stage_input ?raw_trace_run ?clock agent =
  let ts = Pipeline_common.timestamp_now ?clock () in
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
          safe_publish
            bus
            (Event_bus.mk_event
               (ElicitationCompleted
                  { agent_name = agent.state.config.name
                  ; question = req.question
                  ; response
                  }))
        | None -> ());
       (match Agent_elicitation.message_of_response ~question:req.question response with
        | Some message ->
          update_state agent (fun s -> { s with messages = Util.snoc s.messages message })
        | None -> ());
       Ok ()
     | None ->
       let input_required =
         Agent_elicitation.input_required_of_request
           ~agent_name:agent.state.config.name
           ~turn:agent.state.turn_count
           ~created_at:ts
           req
       in
       Error (Error.Agent (InputRequired input_required)))
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
      });
    Ok ()
  | Hooks.Continue -> Ok ()
  | Hooks.HookFailed { stage; detail } ->
    Error (hook_failed_sdk_error ~hook_name:"before_turn" ~stage ~detail)
  | Hooks.Skip | Hooks.Override _ | Hooks.ApprovalRequired | Hooks.AdjustParams _ | Hooks.Block _ ->
    (* Reject illegal hook decisions with a typed error instead of crashing.
       [Hooks.invoke_validated] normally filters these out; this branch guards
       against a validation bypass or future hook matrix drift. [Block] is
       legal only at pre_tool_use, so it is illegal here. *)
    Error (illegal_hook_decision ~stage:"before_turn" ~decision:before_decision)
;;

(* Lower a canonical tool-result projection to the [Types.tool_result] the
   [before_turn_params] hook and disclosure resolver consume. [is_error]
   selects the Error/Ok branch; [content] is the canonical string payload.
   [structured_content]/[content_blocks] from the projection are not needed by
   these local consumers but are surfaced by the projection for a downstream
   external consumer (RFC-OAS-024). *)
let tool_result_of_projection (proj : Llm_provider.Canonical_tool.provider_tool_result)
  : Types.tool_result
  =
  if proj.is_error
  then Error { Types.message = proj.content; recoverable = true; error_class = None }
  else Ok { Types.content = proj.content; _meta = None }
;;

let role_can_carry_tool_results = function
  | User | Tool -> true
  | System | Assistant -> false
;;

let last_tool_results_from messages =
  let extract_results msg =
    if not (role_can_carry_tool_results msg.role)
    then []
    else
      List.filter_map
        (fun (block : content_block) ->
           Llm_provider.Canonical_tool.tool_result_of_block block
           |> Option.map tool_result_of_projection)
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

(* Wiring coverage (RFC-OAS-024 WP8 Inc1): the consumed [last_tool_results_from]
   path routes [ToolResult] blocks through
   [Canonical_tool.tool_result_of_block] and lowers the projection back to
   [Types.tool_result]. A result carrying a [json] (WP4 structured) payload
   must still lower to [Ok { content; _meta = None }] — the projection surfaces
   [structured_content] without disturbing the existing string contract. *)
let%test "last_tool_results_from routes through canonical projection (with json)" =
  let msgs =
    [ { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "ok payload"
              ; is_error = false
              ; json = Some (`Assoc [ "rows", `Int 2 ])
              ; content_blocks = None
              }
          ; ToolResult
              { tool_use_id = "t2"
              ; content = "boom"
              ; is_error = true
              ; json = None
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match last_tool_results_from msgs with
  | [ Ok { content = "ok payload"; _meta = _ }
    ; Error { message = "boom"; recoverable = true; error_class = None }
    ] -> true
  | _ -> false
;;

let resolve_disclosure_level agent =
  match agent.options.disclosure_resolver with
  | None -> agent.options.disclosure_level
  | Some resolver ->
    Disclosure_resolver.resolve
      ~resolver:(Some resolver)
      ~static:agent.options.disclosure_level
      ~last_results:(last_tool_results_from agent.state.messages)
;;

let prepare_turn_for_agent agent ~turn_params =
  Agent_turn.prepare_turn
    ~config:agent.state.config
    ~guardrails:agent.options.guardrails
    ~operator_policy:agent.options.operator_policy
    ~policy_channel:agent.options.policy_channel
    ~tools:agent.tools
    ~messages:agent.state.messages
    ~context_reducer:agent.options.context_reducer
    ~turn_params
    ?tool_selector:agent.options.tool_selector
    ?disclosure_level:(resolve_disclosure_level agent)
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

let turn_ready_tool_names (prep : Agent_turn.turn_preparation) =
  turn_ready_tool_names_from_policy
    ?runtime_mcp_policy:prep.runtime_mcp_policy
    prep.visible_tool_names
;;

let%test "turn_ready_tool_names includes runtime MCP policy names" =
  let runtime_mcp_policy =
    { Llm_provider.Llm_transport.empty_runtime_mcp_policy with
      allowed_tool_names = [ "status_tool"; "shell_tool"; "inline_tool" ]
    }
  in
  turn_ready_tool_names_from_policy ~runtime_mcp_policy [ "inline_tool" ]
  = [ "inline_tool"; "status_tool"; "shell_tool" ]
;;

let filter_runtime_tool_names tool_filter names =
  match tool_filter with
  | Guardrails.AllowAll -> names
  | AllowList allowed -> List.filter (fun name -> List.mem name allowed) names
  | DenyList denied -> List.filter (fun name -> not (List.mem name denied)) names
  | Custom _ -> []
;;

let narrow_runtime_mcp_policy_for_turn
      (guardrails : Guardrails.t)
      (policy : Llm_provider.Llm_transport.runtime_mcp_policy)
  =
  { policy with
    allowed_tool_names =
      filter_runtime_tool_names guardrails.tool_filter policy.allowed_tool_names
  }
;;

let runtime_mcp_policy_for_prepared_turn
      runtime_mcp_policy
      (prep : Agent_turn.turn_preparation)
  =
  runtime_mcp_policy
  |> Option.map (narrow_runtime_mcp_policy_for_turn prep.effective_guardrails)
;;

let%test "runtime MCP policy is narrowed by AllowList guardrails" =
  let policy =
    { Llm_provider.Llm_transport.empty_runtime_mcp_policy with
      allowed_tool_names = [ "status_tool"; "shell_tool"; "ledger_tool" ]
    }
  in
  let narrowed =
    narrow_runtime_mcp_policy_for_turn
      { Guardrails.permissive with
        tool_filter = Guardrails.AllowList [ "status_tool"; "ledger_tool" ]
      }
      policy
  in
  narrowed.allowed_tool_names = [ "status_tool"; "ledger_tool" ]
;;

let stage_parse ?raw_trace_run ?clock agent =
  let* turn_params =
    match agent.options.hooks.before_turn_params with
    | None -> Ok Hooks.default_turn_params
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
       | Hooks.AdjustParams params -> Ok params
       | Hooks.Continue -> Ok Hooks.default_turn_params
       | Hooks.HookFailed { stage; detail } ->
         Error (hook_failed_sdk_error ~hook_name:"before_turn_params" ~stage ~detail)
       | Hooks.Skip
       | Hooks.Override _
       | Hooks.ApprovalRequired
       | Hooks.ElicitInput _
       | Hooks.Nudge _
       | Hooks.Block _ ->
         (* Reject illegal hook decisions with a typed error instead of crashing.
            [Block] is legal only at pre_tool_use, so it is illegal here. *)
         Error (illegal_hook_decision ~stage:"before_turn_params" ~decision))
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
    ; preserve_thinking =
        (match turn_params.preserve_thinking with
         | Some _ as t -> t
         | None -> original_config.preserve_thinking)
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
     safe_publish
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
       (Turn_started
          { turn = agent.state.turn_count
          ; timestamp = Pipeline_common.timestamp_now ?clock ()
          })
   | None -> ());
  let prep = prepare_turn_for_agent agent ~turn_params in
  let runtime_mcp_policy =
    runtime_mcp_policy_for_prepared_turn agent.options.runtime_mcp_policy prep
  in
  let prep = { prep with runtime_mcp_policy } in
  let ready_tool_names = turn_ready_tool_names prep in
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
     safe_publish
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
  Ok (prep, original_config, turn_params)
;;
