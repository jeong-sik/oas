(** Turn pipeline: 6-stage decomposition of agent turn execution.

    Replaces the monolithic run_turn_core with named stages:
    1. Input   — lifecycle, BeforeTurn hook, elicitation
    2. Parse   — BeforeTurnParams hook, context reduction, tool preparation
    3. Route   — provider selection, API call dispatch (sync/stream)
    4. Collect — usage accumulation, AfterTurn hook, events, message append
    5. Execute — tool execution on StopToolUse (idle detection, guardrails)
    6. Output  — stop reason → turn_outcome *)

open Types
open Agent_types
open Agent_trace

let ( let* ) = Result.bind

type api_strategy =
  | Sync
  | Stream of { on_event: Types.sse_event -> unit }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted

(* ── Stage 1: Input ──────────────────────────────────────── *)

(** Set lifecycle to Ready, invoke BeforeTurn hook, handle elicitation. *)
let stage_input ?raw_trace_run agent =
  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~ready_at:ts Ready;

  let before_decision =
    invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"before_turn"
      agent.options.hooks.before_turn
      (Hooks.BeforeTurn { turn = agent.state.turn_count;
                          messages = agent.state.messages })
  in
  (match before_decision with
   | Hooks.ElicitInput req ->
     (match agent.options.elicitation with
      | Some cb ->
        let response = cb req in
        (match agent.options.event_bus with
         | Some bus -> Event_bus.publish bus
             (ElicitationCompleted {
                agent_name = agent.state.config.name;
                question = req.question;
                response })
         | None -> ());
        (match response with
         | Hooks.Answer json ->
           let text = Printf.sprintf "[User input] %s: %s"
             req.question (Yojson.Safe.to_string json) in
           agent.state <- { agent.state with
             messages = Util.snoc agent.state.messages
               { role = User; content = [Text text]; name = None; tool_call_id = None } }
         | Hooks.Declined | Hooks.Timeout -> ())
      | None -> ())
   | _ -> ())

(* ── Stage 2: Parse ──────────────────────────────────────── *)

let last_tool_results_from messages =
  let rec find_last = function
    | [] -> []
    | msg :: rest ->
      if msg.role = User then
        let results = List.filter_map (function
          | ToolResult { content; is_error; _ } ->
            if is_error then Some (Error { Types.message = content; recoverable = true } : Types.tool_result)
            else Some (Ok { Types.content } : Types.tool_result)
          | _ -> None
        ) msg.content in
        if results <> [] then results
        else find_last rest
      else find_last rest
  in
  find_last (List.rev messages)

(** Invoke BeforeTurnParams hook, apply turn params, prepare tools.
    Returns (turn_preparation, original_config). *)
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
    tool_choice =
      (match turn_params.tool_choice with Some _ as t -> t | None -> original_config.tool_choice);
  } in
  agent.state <- { agent.state with config = new_config };
  let original_config = original_config in

  (* TurnStarted event *)
  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       (TurnStarted { agent_name = agent.state.config.name;
                       turn = agent.state.turn_count })
   | None -> ());

  let prep = Agent_turn.prepare_turn
    ~guardrails:agent.options.guardrails ~tools:agent.tools
    ~messages:agent.state.messages
    ~context_reducer:agent.options.context_reducer ~turn_params
  in
  (prep, original_config)

(* ── Stage 3: Route ──────────────────────────────────────── *)

(** Dispatch the API call via the chosen strategy (sync or stream). *)
let stage_route ~sw ?clock ~api_strategy agent prep =
  match api_strategy with
  | Sync ->
    Tracing.with_span agent.options.tracer
      { kind = Api_call; name = "create_message";
        agent_name = agent.state.config.name;
        turn = agent.state.turn_count; extra = [] }
      (fun _tracer ->
        match agent.named_cascade with
        | Some named ->
          Api.create_message_named ~sw ~net:agent.net ?clock
            ~named_cascade:named ~config:agent.state
            ~messages:prep.Agent_turn.effective_messages
            ?tools:prep.tools_json ()
        | None ->
          (match agent.options.cascade with
           | Some casc ->
             Api.create_message_cascade ~sw ~net:agent.net ?clock
               ~cascade:casc ~config:agent.state
               ~messages:prep.Agent_turn.effective_messages
               ?tools:prep.tools_json ()
           | None ->
             Api.create_message ~sw ~net:agent.net
               ~base_url:agent.options.base_url
               ?provider:agent.options.provider ?clock ~config:agent.state
               ~messages:prep.effective_messages ?tools:prep.tools_json ()))
  | Stream { on_event } ->
    Tracing.with_span agent.options.tracer
      { kind = Api_call; name = "create_message_stream";
        agent_name = agent.state.config.name;
        turn = agent.state.turn_count; extra = [] }
      (fun _tracer ->
        match agent.named_cascade with
        | Some named ->
          Api.create_message_named_stream ~sw ~net:agent.net ?clock
            ~named_cascade:named ~config:agent.state
            ~messages:prep.effective_messages ?tools:prep.tools_json
            ~on_event ()
        | None ->
          let can_stream = match agent.options.provider with
            | Some p -> Provider_intf.supports_streaming p
            | None -> true  (* Default Anthropic supports streaming *)
          in
          if can_stream then
            Streaming.create_message_stream ~sw ~net:agent.net
              ~base_url:agent.options.base_url
              ?provider:agent.options.provider
              ~config:agent.state ~messages:prep.effective_messages
              ?tools:prep.tools_json ~on_event ()
          else
            (* Provider does not support native streaming —
               fall back to sync call + synthetic SSE events. *)
            let sync_result =
              match agent.options.cascade with
              | Some casc ->
                Api.create_message_cascade ~sw ~net:agent.net ?clock
                  ~cascade:casc ~config:agent.state
                  ~messages:prep.effective_messages
                  ?tools:prep.tools_json ()
              | None ->
                Api.create_message ~sw ~net:agent.net
                  ~base_url:agent.options.base_url
                  ?provider:agent.options.provider ?clock
                  ~config:agent.state
                  ~messages:prep.effective_messages
                  ?tools:prep.tools_json ()
            in
            (match sync_result with
             | Ok response ->
               Streaming.emit_synthetic_events response on_event;
               Ok response
             | Error _ -> sync_result))

(* ── Stage 4: Collect ────────────────────────────────────── *)

(** Accumulate usage, invoke AfterTurn hook, emit events, append
    assistant message, increment turn_count.  Restores original_config. *)
let stage_collect ?raw_trace_run agent ~original_config response =
  agent.state <- { agent.state with config = original_config };

  let ts = Unix.gettimeofday () in
  set_lifecycle agent ~first_progress_at:ts ~last_progress_at:ts Running;
  let* () = trace_assistant_blocks raw_trace_run response.content in
  let usage = Agent_turn.accumulate_usage
    ~current_usage:agent.state.usage
    ~provider:agent.options.provider
    ~response_usage:response.usage
  in

  let _after =
    invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"after_turn"
      agent.options.hooks.after_turn
      (Hooks.AfterTurn { turn = agent.state.turn_count; response })
  in

  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       (TurnCompleted { agent_name = agent.state.config.name;
                        turn = agent.state.turn_count })
   | None -> ());

  agent.state <- { agent.state with
    messages = Util.snoc agent.state.messages
      { role = Assistant; content = response.content; name = None; tool_call_id = None };
    turn_count = agent.state.turn_count + 1;
    usage;
  };
  Ok ()

(* ── Stage 5: Execute ────────────────────────────────────── *)

(** Handle tool execution: idle detection, guardrails, context injection. *)
let stage_execute ?raw_trace_run agent ~effective_guardrails tool_uses =
  let idle_result = Agent_turn.update_idle_detection
    ~idle_state:{
      last_tool_calls = agent.last_tool_calls;
      consecutive_idle_turns = agent.consecutive_idle_turns;
    }
    ~tool_uses
  in
  agent.last_tool_calls <- idle_result.new_state.last_tool_calls;
  agent.consecutive_idle_turns <-
    idle_result.new_state.consecutive_idle_turns;
  if idle_result.is_idle then begin
    let _idle =
      invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"on_idle"
        agent.options.hooks.on_idle
        (Hooks.OnIdle {
          consecutive_idle_turns = agent.consecutive_idle_turns;
          tool_names = List.filter_map (function
            | ToolUse { name; _ } -> Some name | _ -> None
          ) tool_uses })
    in
    ()
  end;

  let count = List.length tool_uses in
  match Guardrails.exceeds_limit effective_guardrails count with
  | true ->
    let msg = Printf.sprintf
      "Tool call limit exceeded: %d calls in one turn" count in
    agent.state <- { agent.state with
      messages = Util.snoc agent.state.messages
        { role = User; content = [Text msg]; name = None; tool_call_id = None } };
    Ok ToolsExecuted
  | false ->
    let results =
      try Ok (execute_tools_with_trace agent raw_trace_run tool_uses)
      with Raw_trace.Trace_error err -> Error err
    in
    let* results = results in
    let tool_results = Agent_turn.make_tool_results results in
    agent.state <- { agent.state with
      messages = Util.snoc agent.state.messages
        { role = User; content = tool_results; name = None; tool_call_id = None } };
    (match agent.options.context_injector with
     | None -> ()
     | Some injector ->
       let new_messages = Agent_turn.apply_context_injection
         ~context:agent.context ~messages:agent.state.messages
         ~injector ~tool_uses ~results
       in
       agent.state <- { agent.state with messages = new_messages });
    Ok ToolsExecuted

(* ── Stage 6: Output ─────────────────────────────────────── *)

(** Map stop_reason to turn_outcome. *)
let stage_output ?raw_trace_run agent ~effective_guardrails response =
  match response.stop_reason with
  | StopToolUse ->
    let tool_uses = List.filter
      (function ToolUse _ -> true | _ -> false) response.content in
    stage_execute ?raw_trace_run agent ~effective_guardrails tool_uses
  | EndTurn | MaxTokens | StopSequence ->
    let _stop =
      invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"on_stop"
        agent.options.hooks.on_stop
        (Hooks.OnStop { reason = response.stop_reason; response })
    in
    Ok (Complete response)
  | Unknown reason ->
    Error (Error.Agent (UnrecognizedStopReason { reason }))

(* ── Pipeline coordinator ────────────────────────────────── *)

let tag_error stage result =
  match result with
  | Ok _ as ok -> ok
  | Error e ->
    let poly = Error_domain.of_sdk_error e in
    let _ctx = Error_domain.with_stage stage poly in
    (* Stage context is created for diagnostics;
       we still propagate sdk_error for backward compat *)
    Error e

let run_turn ~sw ?clock ~api_strategy ?raw_trace_run agent =
  (* Stage 1: Input *)
  stage_input ?raw_trace_run agent;

  (* Stage 2: Parse *)
  let (prep, original_config) = stage_parse ?raw_trace_run agent in

  (* Stage 2.5: Async input validation *)
  let async_guard = agent.options.guardrails_async in
  (match Guardrails_async.run_input async_guard.input_validators
           prep.Agent_turn.effective_messages with
   | Guardrails_async.Fail { validator_name; reason } ->
     agent.state <- { agent.state with config = original_config };
     Error (Error.Agent (GuardrailViolation {
       validator = validator_name; reason }))
   | Guardrails_async.Pass ->

  (* Stage 3: Route *)
  let api_result = stage_route ~sw ?clock ~api_strategy agent prep
    |> tag_error "route" in

  (* Stage 4+5+6: Collect, Execute/Output *)
  match api_result with
  | Error e ->
    agent.state <- { agent.state with config = original_config };
    Error e
  | Ok response ->
    (* Stage 3.5: Async output validation *)
    (match Guardrails_async.run_output async_guard.output_validators response with
     | Guardrails_async.Fail { validator_name; reason } ->
       agent.state <- { agent.state with config = original_config };
       Error (Error.Agent (GuardrailViolation {
         validator = validator_name; reason }))
     | Guardrails_async.Pass ->
    let* () = stage_collect ?raw_trace_run agent ~original_config response
      |> tag_error "collect" in
    stage_output ?raw_trace_run agent
      ~effective_guardrails:prep.effective_guardrails response
    |> tag_error "output"))
