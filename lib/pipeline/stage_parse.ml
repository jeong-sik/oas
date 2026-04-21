open Types
open Agent_types
open Agent_trace

open Pipeline_common



let last_tool_results_from messages =
  (* Fold from left to track the last User message with ToolResults,
     avoiding List.rev allocation on the full message list. *)
  let extract_results msg =
    if msg.role <> User then []
    else
      List.filter_map (function
        | ToolResult { content; is_error; _ } ->
          if is_error then
            Some
              (Error
                 {
                   Types.message = content;
                   recoverable = true;
                   error_class = None;
                 }
                : Types.tool_result)
          else Some (Ok { Types.content } : Types.tool_result)
        | _ -> None
      ) msg.content
  in
  List.fold_left (fun acc msg ->
    match extract_results msg with
    | [] -> acc
    | results -> results
  ) [] messages

(** Prepare the turn using current [agent.state.messages] and the given
    [turn_params].  Centralises the [Agent_turn.prepare_turn] parameter
    list to avoid duplication between [stage_parse] and post-compaction
    re-preparation (Stage 2.3). *)
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

(** Invoke BeforeTurnParams hook, apply turn params, prepare tools.
    Returns (turn_preparation, original_config, turn_params). *)
let stage_parse ?raw_trace_run agent =
  let turn_params = match agent.options.hooks.before_turn_params with
    | None -> Hooks.default_turn_params
    | Some _ ->
      let last_results = last_tool_results_from agent.state.messages in
      let reasoning = Hooks.extract_reasoning agent.state.messages in
      let decision =
        invoke_hook_with_trace agent ?raw_trace_run
          ~hook_name:"before_turn_params"
          agent.options.hooks.before_turn_params
          (Hooks.BeforeTurnParams {
            turn = agent.state.turn_count;
            max_turns = agent.state.config.max_turns;
            messages = agent.state.messages;
            last_tool_results = last_results;
            current_params = Hooks.default_turn_params;
            reasoning;
          })
      in
      match decision with
      | Hooks.AdjustParams params -> params
      | _ -> Hooks.default_turn_params
  in

  (* Apply ephemeral turn params, save original for restoration *)
  let original_config = agent.state.config in
  let new_config = {
    original_config with
    temperature =
      (match turn_params.temperature with Some _ as t -> t | None -> original_config.temperature);
    thinking_budget =
      (match turn_params.thinking_budget with Some _ as t -> t | None -> original_config.thinking_budget);
    enable_thinking =
      (match turn_params.enable_thinking with Some _ as t -> t | None -> original_config.enable_thinking);
    tool_choice =
      (match turn_params.tool_choice with Some _ as t -> t | None -> original_config.tool_choice);
    system_prompt =
      (match turn_params.system_prompt_override with Some _ as s -> s | None -> original_config.system_prompt)
      |> Option.map Llm_provider.Utf8_sanitize.sanitize;
  } in
  update_state agent (fun s -> { s with config = new_config });
  let original_config = original_config in

  (* TurnStarted event *)
  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       { meta = event_envelope agent;
         payload = TurnStarted
           { agent_name = agent.state.config.name;
             turn = agent.state.turn_count } }
   | None -> ());
  (match agent.options.journal with
   | Some j ->
       Durable_event.append j
         (Turn_started { turn = agent.state.turn_count;
                         timestamp = Unix.gettimeofday () })
   | None -> ());

  let prep = prepare_turn_for_agent agent ~turn_params in
  (prep, original_config, turn_params)

