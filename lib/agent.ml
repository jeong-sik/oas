(** Agent implementation using Eio structured concurrency.
    Supports hooks, context, guardrails, and handoffs as optional features. *)

open Types

(** Configuration options for agent behavior.
    Core runtime resources (net, tools, context) are kept on Agent.t directly;
    everything else lives here so new options don't bloat the Agent.create
    signature. *)
type options = {
  base_url: string;
  provider: Provider.config option;
  hooks: Hooks.hooks;
  guardrails: Guardrails.t;
  tracer: Tracing.t;
  approval: Hooks.approval_callback option;
  context_reducer: Context_reducer.t option;
  mcp_clients: Mcp.managed list;
  event_bus: Event_bus.t option;
}

let default_options = {
  base_url = Api.default_base_url;
  provider = None;
  hooks = Hooks.empty;
  guardrails = Guardrails.default;
  tracer = Tracing.null;
  approval = None;
  context_reducer = None;
  mcp_clients = [];
  event_bus = None;
}

type t = {
  mutable state: agent_state;
  tools: Tool.t list;
  net: [ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t;
  context: Context.t;
  options: options;
}

let create ~net ?(config=default_config) ?(tools=[]) ?context
    ?(options=default_options) () =
  let mcp_tools =
    List.concat_map (fun (m : Mcp.managed) -> m.tools) options.mcp_clients
  in
  let all_tools = tools @ mcp_tools in
  let state = {
    config;
    messages = [];
    turn_count = 0;
    usage = empty_usage;
  } in
  let ctx = match context with
    | Some c -> c
    | None -> Context.create ()
  in
  { state; tools = all_tools; net; context = ctx; options }

(** Clone an agent with independent state.
    By default the clone gets a fresh (empty) context.
    Pass [~copy_context:true] to shallow-copy all context entries.
    Tools, net, options, and mcp_clients are shared (immutable/stateless). *)
let clone ?(copy_context=false) agent =
  let ctx = if copy_context then Context.copy agent.context else Context.create () in
  let state = {
    config = agent.state.config;
    messages = agent.state.messages;
    turn_count = agent.state.turn_count;
    usage = agent.state.usage;
  } in
  { state; tools = agent.tools; net = agent.net; context = ctx; options = agent.options }

(** Close all MCP server connections held by this agent. *)
let close agent =
  Mcp.close_all agent.options.mcp_clients

(** Execute tools in parallel — delegates to Agent_tools with explicit parameters. *)
let execute_tools agent tool_uses =
  Agent_tools.execute_tools
    ~context:agent.context ~tools:agent.tools
    ~hooks:agent.options.hooks ~event_bus:agent.options.event_bus
    ~tracer:agent.options.tracer ~agent_name:agent.state.config.name
    ~turn_count:agent.state.turn_count ~approval:agent.options.approval
    tool_uses

(** Run a single turn with hook and guardrail support *)
let run_turn ~sw ?clock agent =
  (* BeforeTurn hook *)
  let _before = Hooks.invoke agent.options.hooks.before_turn
    (Hooks.BeforeTurn { turn = agent.state.turn_count; messages = agent.state.messages }) in

  (* TurnStarted event *)
  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       (TurnStarted { agent_name = agent.state.config.name; turn = agent.state.turn_count })
   | None -> ());

  (* Apply guardrails: filter tools visible to LLM *)
  let visible_tools = Guardrails.filter_tools agent.options.guardrails agent.tools in
  let tool_schemas = List.map Tool.schema_to_json visible_tools in
  let tools_json = if tool_schemas = [] then None else Some tool_schemas in

  (* Apply context reducer: only the API call sees the windowed messages.
     The full history is preserved in agent.state.messages. *)
  let effective_messages = match agent.options.context_reducer with
    | None -> agent.state.messages
    | Some reducer -> Context_reducer.reduce reducer agent.state.messages
  in

  let api_result = Tracing.with_span agent.options.tracer
    { kind = Api_call; name = "create_message";
      agent_name = agent.state.config.name;
      turn = agent.state.turn_count; extra = [] }
    (fun _tracer ->
      Api.create_message ~sw ~net:agent.net ~base_url:agent.options.base_url
        ?provider:agent.options.provider ?clock ~config:agent.state
        ~messages:effective_messages ?tools:tools_json ())
  in
  match api_result with
  | Error e -> Error e
  | Ok response ->
      (* Accumulate usage stats *)
      let usage = match response.usage with
        | Some u -> add_usage agent.state.usage u
        | None -> { agent.state.usage with api_calls = agent.state.usage.api_calls + 1 }
      in

      (* AfterTurn hook *)
      let _after = Hooks.invoke agent.options.hooks.after_turn
        (Hooks.AfterTurn { turn = agent.state.turn_count; response }) in

      (* TurnCompleted event *)
      (match agent.options.event_bus with
       | Some bus -> Event_bus.publish bus
           (TurnCompleted { agent_name = agent.state.config.name; turn = agent.state.turn_count })
       | None -> ());

      agent.state <- { agent.state with
        messages = agent.state.messages @ [{ role = Assistant; content = response.content }];
        turn_count = agent.state.turn_count + 1;
        usage;
      };

      match response.stop_reason with
      | StopToolUse ->
        let tool_uses = List.filter (function ToolUse _ -> true | _ -> false) response.content in

        (* Guardrail: check tool call count limit *)
        let count = List.length tool_uses in
        (match Guardrails.exceeds_limit agent.options.guardrails count with
        | true ->
          let msg = Printf.sprintf "Tool call limit exceeded: %d calls in one turn" count in
          agent.state <- { agent.state with
            messages = agent.state.messages @ [{ role = User; content = [Text msg] }] };
          Ok (`ToolsExecuted)
        | false ->
          let results = execute_tools agent tool_uses in
          let tool_results = List.map (fun (id, content, is_error) ->
            ToolResult (id, content, is_error)
          ) results in
          agent.state <- { agent.state with messages = agent.state.messages @ [{ role = User; content = tool_results }] };
          Ok (`ToolsExecuted))
      | EndTurn | MaxTokens | StopSequence ->
        (* OnStop hook *)
        let _stop = Hooks.invoke agent.options.hooks.on_stop
          (Hooks.OnStop { reason = response.stop_reason; response }) in
        Ok (`Complete response)
      | Unknown reason ->
        Error (Error.Agent (UnrecognizedStopReason { reason }))

(** Check if token budget has been exceeded. Returns a structured error or None. *)
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

(** Run loop *)
let run ~sw ?clock agent user_prompt =
  agent.state <- { agent.state with messages = agent.state.messages @ [{ role = User; content = [Text user_prompt] }] };

  let rec loop () =
    if agent.state.turn_count >= agent.state.config.max_turns then
      Error (Error.Agent (MaxTurnsExceeded { turns = agent.state.turn_count; limit = agent.state.config.max_turns }))
    else
      match check_token_budget agent.state.config agent.state.usage with
      | Some err -> Error err
      | None ->
        match run_turn ~sw ?clock agent with
        | Error e -> Error e
        | Ok `Complete response -> Ok response
        | Ok `ToolsExecuted -> loop ()
  in
  loop ()

(** Run a single turn with streaming.
    Uses Streaming.create_message_stream for Anthropic providers,
    falls back to Api.create_message + synthetic events for others. *)
let run_turn_stream ~sw ?clock ~on_event agent =
  (* BeforeTurn hook *)
  let _before = Hooks.invoke agent.options.hooks.before_turn
    (Hooks.BeforeTurn { turn = agent.state.turn_count; messages = agent.state.messages }) in

  (* TurnStarted event *)
  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       (TurnStarted { agent_name = agent.state.config.name; turn = agent.state.turn_count })
   | None -> ());

  (* Apply guardrails: filter tools visible to LLM *)
  let visible_tools = Guardrails.filter_tools agent.options.guardrails agent.tools in
  let tool_schemas = List.map Tool.schema_to_json visible_tools in
  let tools_json = if tool_schemas = [] then None else Some tool_schemas in

  (* Apply context reducer *)
  let effective_messages = match agent.options.context_reducer with
    | None -> agent.state.messages
    | Some reducer -> Context_reducer.reduce reducer agent.state.messages
  in

  let api_result = Tracing.with_span agent.options.tracer
    { kind = Api_call; name = "create_message_stream";
      agent_name = agent.state.config.name;
      turn = agent.state.turn_count; extra = [] }
    (fun _tracer ->
      (* Try streaming first for Anthropic, fallback for others *)
      let stream_result =
        Streaming.create_message_stream ~sw ~net:agent.net
          ~base_url:agent.options.base_url ?provider:agent.options.provider
          ~config:agent.state ~messages:effective_messages ?tools:tools_json
          ~on_event ()
      in
      match stream_result with
      | Ok _ -> stream_result
      | Error (Error.Config (UnsupportedProvider _)) ->
          (* Non-Anthropic provider: fallback to sync + synthetic events *)
          let sync_result = Api.create_message ~sw ~net:agent.net
            ~base_url:agent.options.base_url ?provider:agent.options.provider
            ?clock ~config:agent.state ~messages:effective_messages
            ?tools:tools_json () in
          (match sync_result with
           | Ok response ->
             Streaming.emit_synthetic_events response on_event;
             Ok response
           | Error _ -> sync_result)
      | Error _ -> stream_result)
  in
  match api_result with
  | Error e -> Error e
  | Ok response ->
    (* Accumulate usage stats *)
    let usage = match response.usage with
      | Some u -> add_usage agent.state.usage u
      | None -> { agent.state.usage with api_calls = agent.state.usage.api_calls + 1 }
    in

    (* AfterTurn hook *)
    let _after = Hooks.invoke agent.options.hooks.after_turn
      (Hooks.AfterTurn { turn = agent.state.turn_count; response }) in

    (* TurnCompleted event *)
    (match agent.options.event_bus with
     | Some bus -> Event_bus.publish bus
         (TurnCompleted { agent_name = agent.state.config.name; turn = agent.state.turn_count })
     | None -> ());

    agent.state <- { agent.state with
      messages = agent.state.messages @ [{ role = Assistant; content = response.content }];
      turn_count = agent.state.turn_count + 1;
      usage;
    };

    match response.stop_reason with
    | StopToolUse ->
      let tool_uses = List.filter (function ToolUse _ -> true | _ -> false) response.content in
      let count = List.length tool_uses in
      (match Guardrails.exceeds_limit agent.options.guardrails count with
      | true ->
        let msg = Printf.sprintf "Tool call limit exceeded: %d calls in one turn" count in
        agent.state <- { agent.state with
          messages = agent.state.messages @ [{ role = User; content = [Text msg] }] };
        Ok (`ToolsExecuted)
      | false ->
        let results = execute_tools agent tool_uses in
        let tool_results = List.map (fun (id, content, is_error) ->
          ToolResult (id, content, is_error)
        ) results in
        agent.state <- { agent.state with
          messages = agent.state.messages @ [{ role = User; content = tool_results }] };
        Ok (`ToolsExecuted))
    | EndTurn | MaxTokens | StopSequence ->
      let _stop = Hooks.invoke agent.options.hooks.on_stop
        (Hooks.OnStop { reason = response.stop_reason; response }) in
      Ok (`Complete response)
    | Unknown reason ->
      Error (Error.Agent (UnrecognizedStopReason { reason }))

(** Run agent loop with streaming.
    Same as [run] but uses [run_turn_stream] for each turn. *)
let run_stream ~sw ?clock ~on_event agent user_prompt =
  agent.state <- { agent.state with
    messages = agent.state.messages @ [{ role = User; content = [Text user_prompt] }] };
  let rec loop () =
    if agent.state.turn_count >= agent.state.config.max_turns then
      Error (Error.Agent (MaxTurnsExceeded { turns = agent.state.turn_count; limit = agent.state.config.max_turns }))
    else
      match check_token_budget agent.state.config agent.state.usage with
      | Some err -> Error err
      | None ->
        match run_turn_stream ~sw ?clock ~on_event agent with
        | Error e -> Error e
        | Ok `Complete response -> Ok response
        | Ok `ToolsExecuted -> loop ()
  in
  loop ()

(* Re-export Agent_handoff helpers for backward compatibility *)
let find_handoff_in_messages = Agent_handoff.find_handoff_in_messages
let replace_tool_result = Agent_handoff.replace_tool_result

(** Run with handoff support.
    Handoff tools are added to the agent's tool list.
    When the LLM calls a transfer_to_* tool, a sub-agent is spawned and run.
    The sub-agent's final text is returned as the tool result. *)
let run_with_handoffs ~sw ?clock agent ~targets user_prompt =
  let handoff_tools = List.map Handoff.make_handoff_tool targets in
  let all_tools = agent.tools @ handoff_tools in
  let agent_with_handoffs = { agent with tools = all_tools } in

  let rec loop () =
    if agent_with_handoffs.state.turn_count >= agent_with_handoffs.state.config.max_turns then
      Error (Error.Agent (MaxTurnsExceeded { turns = agent.state.turn_count; limit = agent.state.config.max_turns }))
    else
      match run_turn ~sw ?clock agent_with_handoffs with
      | Error e -> Error e
      | Ok `Complete response -> Ok response
      | Ok `ToolsExecuted ->
        (* Check if the last assistant message contained a handoff tool call *)
        (match find_handoff_in_messages agent_with_handoffs.state.messages with
         | Some (tool_id, target_name, prompt) ->
           (* Find matching target definition *)
           let target_opt = List.find_opt
             (fun (t : Handoff.handoff_target) -> t.name = target_name) targets in
           (match target_opt with
            | None ->
              (* Unknown target: replace the sentinel result with a useful error. *)
              let err_msg = Printf.sprintf "Unknown handoff target: %s" target_name in
              agent_with_handoffs.state <- { agent_with_handoffs.state with
                messages =
                  replace_tool_result agent_with_handoffs.state.messages
                    ~tool_id ~content:err_msg ~is_error:true };
              loop ()
            | Some target ->
              (* Create sub-agent with target's config and tools *)
              let sub = create ~net:agent.net ~config:target.config
                ~tools:target.tools ~options:{ default_options with
                  base_url = agent.options.base_url;
                  provider = agent.options.provider } () in
               (match run ~sw ?clock sub prompt with
               | Error e ->
                 let err_msg = Printf.sprintf "Handoff to %s failed: %s" target_name (Error.to_string e) in
                 agent_with_handoffs.state <- { agent_with_handoffs.state with
                   messages =
                     replace_tool_result agent_with_handoffs.state.messages
                       ~tool_id ~content:err_msg ~is_error:true };
                 loop ()
               | Ok sub_response ->
                 (* Extract text from sub-agent response *)
                 let text = List.fold_left (fun acc block ->
                   match block with
                   | Text s -> if acc = "" then s else acc ^ "\n" ^ s
                   | _ -> acc
                 ) "" sub_response.content in
                 agent_with_handoffs.state <- { agent_with_handoffs.state with
                   messages =
                     replace_tool_result agent_with_handoffs.state.messages
                       ~tool_id ~content:text ~is_error:false };
                 loop ()))
         | None ->
           (* No handoff detected, normal tool execution loop *)
           loop ())
  in
  agent_with_handoffs.state <- { agent_with_handoffs.state with
    messages = agent_with_handoffs.state.messages @ [{ role = User; content = [Text user_prompt] }] };
  loop ()

(** Restore an agent from a checkpoint.
    Tools must be re-provided since handlers are closures (not serializable).
    Config fields not captured in checkpoint (max_tokens, max_turns, etc.)
    use defaults or can be overridden via [?config]. *)
let resume ~net ~(checkpoint : Checkpoint.t) ?(tools=[]) ?context
    ?(options=default_options) ?config () =
  let base_config = match config with
    | Some c -> c
    | None -> default_config
  in
  let restored_config = {
    base_config with
    name = checkpoint.agent_name;
    model = checkpoint.model;
    system_prompt = checkpoint.system_prompt;
    temperature = checkpoint.temperature;
    top_p = checkpoint.top_p;
    top_k = checkpoint.top_k;
    min_p = checkpoint.min_p;
    enable_thinking = checkpoint.enable_thinking;
    response_format_json = checkpoint.response_format_json;
    thinking_budget = checkpoint.thinking_budget;
    tool_choice = checkpoint.tool_choice;
    cache_system_prompt = checkpoint.cache_system_prompt;
    max_input_tokens = checkpoint.max_input_tokens;
    max_total_tokens = checkpoint.max_total_tokens;
  } in
  let state = {
    config = restored_config;
    messages = checkpoint.messages;
    turn_count = checkpoint.turn_count;
    usage = checkpoint.usage;
  } in
  let ctx = match context with
    | Some c -> c
    | None -> Context.create ()
  in
  { state; tools; net; context = ctx; options }

(** Create a checkpoint from the current agent state. *)
let checkpoint ?(session_id="") agent =
  {
    Checkpoint.version = Checkpoint.checkpoint_version;
    session_id;
    agent_name = agent.state.config.name;
    model = agent.state.config.model;
    system_prompt = agent.state.config.system_prompt;
    messages = agent.state.messages;
    usage = agent.state.usage;
    turn_count = agent.state.turn_count;
    created_at = Unix.gettimeofday ();
    tools = List.map (fun (t : Tool.t) -> t.schema) agent.tools;
    tool_choice = agent.state.config.tool_choice;
    temperature = agent.state.config.temperature;
    top_p = agent.state.config.top_p;
    top_k = agent.state.config.top_k;
    min_p = agent.state.config.min_p;
    enable_thinking = agent.state.config.enable_thinking;
    response_format_json = agent.state.config.response_format_json;
    thinking_budget = agent.state.config.thinking_budget;
    cache_system_prompt = agent.state.config.cache_system_prompt;
    max_input_tokens = agent.state.config.max_input_tokens;
    max_total_tokens = agent.state.config.max_total_tokens;
    mcp_sessions = Mcp_session.capture_all agent.options.mcp_clients;
  }
