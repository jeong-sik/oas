(** Shared turn logic for sync and streaming paths.

    Contains helper functions that both [Agent.run_turn_with_trace] and
    [Agent.run_turn_stream_with_trace] call, eliminating ~60% code duplication.

    These functions take explicit parameters (not Agent.t) to avoid
    circular module dependency: Agent -> Agent_turn is fine,
    Agent_turn -> Agent is not. *)

open Types

(* ── Fingerprint-based idle detection ─────────────────────────── *)

type tool_call_fingerprint = {
  fp_name: string;
  fp_input: string;
}

let compute_fingerprints tool_uses =
  List.filter_map (function
    | ToolUse { name; input; _ } ->
      Some { fp_name = name; fp_input = Yojson.Safe.to_string input }
    | _ -> None
  ) tool_uses

let is_idle (prev : tool_call_fingerprint list option) current =
  match prev with
  | None -> false
  | Some prev_fps ->
    List.length current = List.length prev_fps &&
    List.for_all2 (fun a b -> a.fp_name = b.fp_name && a.fp_input = b.fp_input)
      current prev_fps

(* ── Turn preparation ─────────────────────────────────────────── *)

type turn_preparation = {
  tools_json: Yojson.Safe.t list option;
  effective_messages: message list;
  effective_guardrails: Guardrails.t;
}

(* ── Extract last user text from messages (for Tool_selector context) ── *)

let extract_last_user_text (messages : message list) : string =
  let rec find_last = function
    | [] -> ""
    | msg :: rest ->
      if msg.role = User then
        let texts = List.filter_map (function
          | Text s -> Some s
          | _ -> None
        ) msg.content in
        match texts with
        | [] -> find_last rest
        | _ -> String.concat " " texts
      else find_last rest
  in
  find_last (List.rev messages)

let prepare_tools ~guardrails ~operator_policy ~policy_channel ~(tools : Tool_set.t) ~turn_params
    ?tool_selector ?messages () =
  (* Precedence: policy_channel > operator > hook > agent.
     Policy channel accumulates Tool_op.t pushed by a parent agent.
     Operator policy is the hard ceiling after channel resolution.
     Hook tool_filter_override is intersected with the merged result,
     so it can only narrow — never re-grant a denied tool. *)
  let effective_operator = match policy_channel with
    | None -> operator_policy
    | Some ch ->
      match Policy_channel.poll ch with
      | None -> operator_policy
      | Some op ->
        let current_names = Tool_set.names tools in
        let channel_filter = Guardrails.AllowList (Tool_op.apply op current_names) in
        (* Intersect channel result with operator policy so the channel
           can only narrow — never widen — the operator ceiling. *)
        let constrained = match operator_policy with
          | None -> channel_filter
          | Some op_filter ->
            Guardrails.intersect_filters op_filter channel_filter
        in
        Some constrained
  in
  let merged, source =
    Guardrails.merge_operator_policy ~operator:effective_operator ~agent:guardrails
  in
  let effective_guardrails = match turn_params.Hooks.tool_filter_override with
    | Some filter ->
      let intersected = Guardrails.intersect_filters merged.tool_filter filter in
      { merged with Guardrails.tool_filter = intersected }
    | None -> merged
  in
  (* Audit log when operator policy is active *)
  (match source with
   | Guardrails.Operator ->
     let _log = Log.create ~module_name:"agent_turn" () in
     Log.info _log "operator policy applied to tool filter"
       [S ("source", Guardrails.show_policy_source source)]
   | Guardrails.Agent -> ());
  let visible = Tool_set.filter effective_guardrails tools in
  (* Apply tool selector to narrow visible tools *)
  let selected = match tool_selector with
    | None -> Tool_set.to_list visible
    | Some strategy ->
      let context = match messages with
        | Some msgs -> extract_last_user_text msgs
        | None -> ""
      in
      Tool_selector.select ~strategy ~context ~tools:(Tool_set.to_list visible)
  in
  let tool_schemas = List.map Tool.schema_to_json selected in
  let tools_json = if tool_schemas = [] then None else Some tool_schemas in
  (tools_json, effective_guardrails)

let prepare_messages ~messages ~context_reducer ~turn_params =
  let effective = match context_reducer with
    | None -> messages
    | Some reducer -> Context_reducer.reduce reducer messages
  in
  match turn_params.Hooks.extra_system_context with
  | None -> effective
  | Some ctx ->
    let system_msg = { role = User; content = [Text ("[system context] " ^ ctx)]; name = None; tool_call_id = None } in
    system_msg :: effective

let prepare_turn ~guardrails ~operator_policy ~policy_channel ~tools ~messages ~context_reducer ~turn_params
    ?tool_selector () =
  let tools_json, effective_guardrails =
    prepare_tools ~guardrails ~operator_policy ~policy_channel ~tools ~turn_params
      ?tool_selector ~messages ()
  in
  let effective_messages =
    prepare_messages ~messages ~context_reducer ~turn_params
  in
  { tools_json; effective_messages; effective_guardrails }

(* ── Usage accumulation ───────────────────────────────────────── *)

let accumulate_usage ~current_usage ~provider ~response_usage =
  match response_usage with
  | Some u ->
    let base = add_usage current_usage u in
    let turn_cost =
      match u.cost_usd with
      | Some cost -> cost
      | None ->
          let model_id = match provider with
            | Some (cfg : Provider.config) -> cfg.model_id
            | None -> ""
          in
          let pricing = Provider.pricing_for_model model_id in
          Provider.estimate_cost ~pricing
            ~input_tokens:u.input_tokens ~output_tokens:u.output_tokens
            ~cache_creation_input_tokens:u.cache_creation_input_tokens
            ~cache_read_input_tokens:u.cache_read_input_tokens ()
    in
    { base with estimated_cost_usd = base.estimated_cost_usd +. turn_cost }
  | None -> { current_usage with api_calls = current_usage.api_calls + 1 }

(* ── Turn params resolution ───────────────────────────────────── *)

let resolve_turn_params ~hooks ~messages ~max_turns ~turn ~invoke_hook =
  match hooks.Hooks.before_turn_params with
  | None -> Hooks.default_turn_params
  | Some _ ->
    let last_results =
      let rec find_last = function
        | [] -> []
        | msg :: rest ->
          if msg.role = User then
            let results = List.filter_map (function
              | ToolResult { content; is_error; _ } ->
                if is_error then Some (Error { message = content; recoverable = true } : tool_result)
                else Some (Ok { content } : tool_result)
              | _ -> None
            ) msg.content in
            if results <> [] then results
            else find_last rest
          else find_last rest
      in
      find_last (List.rev messages)
    in
    let reasoning = Hooks.extract_reasoning messages in
    let decision =
      invoke_hook ~hook_name:"before_turn_params"
        hooks.Hooks.before_turn_params
        (Hooks.BeforeTurnParams {
          turn;
          max_turns;
          messages;
          last_tool_results = last_results;
          current_params = Hooks.default_turn_params;
          reasoning;
        })
    in
    match decision with
    | Hooks.AdjustParams params -> params
    | _ -> Hooks.default_turn_params

(* ── Context injection after tool execution ───────────────────── *)

let filter_valid_messages ~messages extra_messages =
  match messages with
  | [] -> extra_messages
  | _ ->
    let last_role = (List.nth messages
      (List.length messages - 1)).role in
    let rec filter_valid prev_role = function
      | [] -> []
      | (msg : message) :: rest ->
        if msg.role = prev_role then
          filter_valid prev_role rest
        else
          msg :: filter_valid msg.role rest
    in
    filter_valid last_role extra_messages

let recoverable_of_failure_kind = function
  | Some Agent_tools.Validation_error | Some Agent_tools.Recoverable_tool_error ->
      true
  | Some Agent_tools.Non_retryable_tool_error | None -> false

let apply_context_injection ~context ~messages ~injector ~tool_uses ~results =
  let current_messages = ref messages in
  List.iter2 (fun block (result : Agent_tools.tool_execution_result) ->
    match block with
    | ToolUse { name; input; _ } ->
      let output : tool_result =
        if result.is_error then
          Error
            {
              message = result.content;
              recoverable = recoverable_of_failure_kind result.failure_kind;
            }
        else Ok { content = result.content }
      in
      (try
        match injector ~tool_name:name ~input ~output with
        | None -> ()
        | Some inj ->
          List.iter (fun (key, value) ->
            Context.set context key value
          ) inj.Hooks.context_updates;
          let valid_messages =
            filter_valid_messages ~messages:!current_messages inj.extra_messages
          in
          if valid_messages <> [] then
            current_messages := Util.snoc_list !current_messages valid_messages
      with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | exn ->
        let _log = Log.create ~module_name:"agent_turn" () in
        Log.warn _log
          "context_injector raised"
          [S ("tool", name); S ("error", Printexc.to_string exn)])
    | _ -> ()
  ) tool_uses results;
  !current_messages

(* ── Token budget check ───────────────────────────────────────── *)

let check_token_budget config usage =
  let exceeded_input =
    match config.max_input_tokens with
    | Some limit when usage.total_input_tokens > limit ->
        Some (Error.Agent (TokenBudgetExceeded { kind = "Input"; used = usage.total_input_tokens; limit }))
    | _ -> None
  in
  let exceeded_total =
    match config.max_total_tokens with
    | Some limit ->
        let total = usage.total_input_tokens + usage.total_output_tokens in
        if total > limit then
          Some (Error.Agent (TokenBudgetExceeded { kind = "Total"; used = total; limit }))
        else None
    | _ -> None
  in
  match exceeded_input with
  | Some _ as err -> err
  | None -> exceeded_total

(* ── Stop reason handling (tool execution branch) ─────────────── *)

type idle_state = {
  last_tool_calls: tool_call_fingerprint list option;
  consecutive_idle_turns: int;
}

type idle_result = {
  new_state: idle_state;
  is_idle: bool;
}

let update_idle_detection ~idle_state ~tool_uses =
  let current_fps = compute_fingerprints tool_uses in
  let idle = is_idle idle_state.last_tool_calls current_fps in
  let new_consecutive =
    if idle then idle_state.consecutive_idle_turns + 1
    else 0
  in
  {
    new_state = {
      last_tool_calls = Some current_fps;
      consecutive_idle_turns = new_consecutive;
    };
    is_idle = idle;
  }

(** Process tool results into ToolResult content blocks.
    All entries are valid ToolUse results — non-ToolUse blocks are filtered
    upstream in {!Agent_tools.execute_tools}. *)
let make_tool_results results =
  List.map (fun (result : Agent_tools.tool_execution_result) ->
    ToolResult {
      tool_use_id = result.tool_use_id;
      content = result.content;
      is_error = result.is_error;
      json = None;
    }
  ) results
