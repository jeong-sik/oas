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
  | IdleSkipped

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
           update_state agent (fun s ->
             { s with messages = Util.snoc s.messages
                 { role = User; content = [Text text]; name = None; tool_call_id = None } })
         | Hooks.Declined | Hooks.Timeout -> ())
      | None -> ())
   | _ -> ())

(* ── Stage 2: Parse ──────────────────────────────────────── *)

let last_tool_results_from messages =
  (* Fold from left to track the last User message with ToolResults,
     avoiding List.rev allocation on the full message list. *)
  let extract_results msg =
    if msg.role <> User then []
    else
      List.filter_map (function
        | ToolResult { content; is_error; _ } ->
          if is_error then Some (Error { Types.message = content; recoverable = true } : Types.tool_result)
          else Some (Ok { Types.content } : Types.tool_result)
        | _ -> None
      ) msg.content
  in
  List.fold_left (fun acc msg ->
    match extract_results msg with
    | [] -> acc
    | results -> results
  ) [] messages

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
    system_prompt =
      (match turn_params.system_prompt_override with Some _ as s -> s | None -> original_config.system_prompt);
  } in
  update_state agent (fun s -> { s with config = new_config });
  let original_config = original_config in

  (* TurnStarted event *)
  (match agent.options.event_bus with
   | Some bus -> Event_bus.publish bus
       (TurnStarted { agent_name = agent.state.config.name;
                       turn = agent.state.turn_count })
   | None -> ());

  let prep = Agent_turn.prepare_turn
    ~guardrails:agent.options.guardrails
    ~operator_policy:agent.options.operator_policy
    ~policy_channel:agent.options.policy_channel
    ~tools:agent.tools
    ~messages:agent.state.messages
    ~context_reducer:agent.options.context_reducer ~turn_params
    ?tool_selector:agent.options.tool_selector
    ()
  in
  (prep, original_config)

(* ── Stage 3: Route ──────────────────────────────────────── *)

(** Dispatch the API call via the chosen strategy (sync or stream). *)
let stage_route ~sw ?clock ~api_strategy agent prep =
  let priority = agent.state.config.priority in
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
            ?tools:prep.tools_json ~metrics:named.metrics ?priority ()
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
            ~metrics:named.metrics ~on_event ?priority ()
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
  update_state agent (fun s -> { s with config = original_config });

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

  update_state agent (fun s ->
    { s with
      messages = Util.snoc s.messages
        { role = Assistant; content = response.content; name = None; tool_call_id = None };
      turn_count = s.turn_count + 1;
      usage });
  Ok ()

let retry_failures_of_results
    (results : Agent_tools.tool_execution_result list) :
    Tool_retry_policy.failure list =
  results
  |> List.filter_map (fun (result : Agent_tools.tool_execution_result) ->
         match result.failure_kind with
         | Some Agent_tools.Validation_error ->
             Some
               {
                 Tool_retry_policy.tool_name = result.tool_name;
                 detail = result.content;
                 kind = Tool_retry_policy.Validation_error;
               }
         | Some Agent_tools.Recoverable_tool_error ->
             Some
               {
                 Tool_retry_policy.tool_name = result.tool_name;
                 detail = result.content;
                 kind = Tool_retry_policy.Recoverable_tool_error;
               }
         | Some Agent_tools.Non_retryable_tool_error | None -> None)

let retry_feedback_blocks ~(policy : Tool_retry_policy.t) ~(retry_count : int)
    ~(summary : string) ~(tool_results : Types.content_block list) =
  match policy.feedback_style with
  | Tool_retry_policy.Structured_tool_result -> tool_results
  | Tool_retry_policy.Plain_error_text ->
      [
        Tool_retry_policy.plain_feedback_block ~retry_count
          ~max_retries:policy.max_retries ~summary;
      ]

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
  Eio.Mutex.use_rw ~protect:true agent.mu (fun () ->
    agent.last_tool_calls <- idle_result.new_state.last_tool_calls;
    agent.consecutive_idle_turns <-
      idle_result.new_state.consecutive_idle_turns);
  let idle_skip = ref false in
  let idle_handled = ref false in  (* true when Nudge or Skip handled idle *)
  if idle_result.is_idle then begin
    let idle_decision =
      invoke_hook_with_trace agent ?raw_trace_run ~hook_name:"on_idle"
        agent.options.hooks.on_idle
        (Hooks.OnIdle {
          consecutive_idle_turns = agent.consecutive_idle_turns;
          tool_names = List.filter_map (function
            | ToolUse { name; _ } -> Some name | _ -> None
          ) tool_uses })
    in
    match idle_decision with
    | Hooks.Skip ->
      idle_skip := true;
      idle_handled := true
    | Hooks.Nudge nudge_msg ->
      (* Inject a nudge message but keep idle counter accumulating.
         Resetting to 0 caused an infinite nudge loop: model repeats
         the same tool, counter resets, never reaches Skip threshold.
         With accumulation: nudge at 1, nudge at 2, Skip at 3. *)
      update_state agent (fun s ->
        { s with messages = Util.snoc s.messages
            { role = User; content = [Text nudge_msg];
              name = None; tool_call_id = None } });
      idle_handled := true
    | _ -> ()
  end;
  (* Early exit: skip tool execution when on_idle hook says Skip.
     Prevents executing redundant tools and avoids further counter drift. *)
  if !idle_skip then Ok IdleSkipped
  else
  let count = List.length tool_uses in
  match Guardrails.exceeds_limit effective_guardrails count with
  | true ->
    Tool_retry_policy.clear_context_retry_count agent.context;
    let msg = Printf.sprintf
      "Tool call limit exceeded: %d calls in one turn" count in
    update_state agent (fun s ->
      { s with messages = Util.snoc s.messages
          { role = User; content = [Text msg]; name = None; tool_call_id = None } });
    Ok ToolsExecuted
  | false ->
    let results =
      try Ok (execute_tools_with_trace agent raw_trace_run tool_uses)
      with Raw_trace.Trace_error err ->
        Tool_retry_policy.clear_context_retry_count agent.context;
        Error err
    in
    let* results = results in
    let tool_results = Agent_turn.make_tool_results results in
    let* tool_feedback =
      match agent.options.tool_retry_policy with
      | None ->
          Tool_retry_policy.clear_context_retry_count agent.context;
          Ok tool_results
      | Some policy -> (
          match
            Tool_retry_policy.decide ~policy
              ~prior_retries:
                (Tool_retry_policy.context_retry_count agent.context)
              (retry_failures_of_results results)
          with
          | Tool_retry_policy.No_retry ->
              Tool_retry_policy.clear_context_retry_count agent.context;
              Ok tool_results
          | Tool_retry_policy.Retry { retry_count; summary } ->
              Tool_retry_policy.set_context_retry_count agent.context retry_count;
              Ok
                (retry_feedback_blocks ~policy ~retry_count ~summary
                   ~tool_results)
          | Tool_retry_policy.Exhausted { attempts; limit; summary } ->
              Tool_retry_policy.clear_context_retry_count agent.context;
              Error
                (Error.Agent
                   (ToolRetryExhausted { attempts; limit; detail = summary })))
    in
    (* Anti-repetition hint: append warning to tool feedback when idle detected
       but not already handled by Nudge or Skip. Nudge injects its own message
       and resets the counter; Skip causes early return above. *)
    let effective_feedback =
      if idle_result.is_idle && not !idle_handled then
        tool_feedback @ [Text (Printf.sprintf
          "[Idle warning: You called the same tool(s) with identical arguments %d time(s) in a row. Try a different tool or change your arguments to make progress.]"
          agent.consecutive_idle_turns)]
      else tool_feedback
    in
    update_state agent (fun s ->
      {
        s with
        messages =
          Util.snoc s.messages
            {
              role = User;
              content = effective_feedback;
              name = None;
              tool_call_id = None;
            };
      });
    (match agent.options.context_injector with
     | None -> ()
     | Some injector ->
       let new_messages = Agent_turn.apply_context_injection
         ~context:agent.context ~messages:agent.state.messages
         ~injector ~tool_uses ~results
       in
       update_state agent (fun s -> { s with messages = new_messages }));
    (* Anti-repetition hint is now in effective_feedback above.
       Removed duplicate User message injection (Copilot review #3). *)
    ignore idle_handled;  (* suppress unused warning after dedup *)
    (* In-memory message hygiene after each tool execution round.
       Without this, agent.state.messages grows unbounded across turns —
       context_reducer only trims before API calls, not in the stored state.

       Two-step pruning (Claude Code Tier 1 pattern):
       1. Stub old tool results: keep 2 most recent in full, replace older
          with short stubs. Tool results are the largest allocation source.
       2. Hard message cap: keep last 100 messages. Prevents unbounded growth
          in long-running agents (600+ turns). *)
    let pruner = Context_reducer.compose [
      Context_reducer.stub_tool_results ~keep_recent:2;
      Context_reducer.keep_last 100;
    ] in
    update_state agent (fun s ->
      { s with messages =
          Context_reducer.reduce pruner s.messages });
    Ok ToolsExecuted

(* ── Stage 6: Output ─────────────────────────────────────── *)

(** Map stop_reason to turn_outcome. *)
let stage_output ?raw_trace_run agent ~effective_guardrails response =
  match response.stop_reason with
  | StopToolUse ->
    let tool_uses = List.filter
      (function ToolUse _ -> true | _ -> false) response.content in
    let result = stage_execute ?raw_trace_run agent ~effective_guardrails tool_uses in
    (match result with
     | Ok IdleSkipped ->
       (* on_idle hook returned Skip: stop gracefully with the current response *)
       Ok (Complete response)
     | other -> other)
  | EndTurn | MaxTokens | StopSequence ->
    Tool_retry_policy.clear_context_retry_count agent.context;
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
     update_state agent (fun s -> { s with config = original_config });
     Error (Error.Agent (GuardrailViolation {
       validator = validator_name; reason }))
   | Guardrails_async.Pass ->

  (* Stage 3: Route *)
  let api_result = stage_route ~sw ?clock ~api_strategy agent prep
    |> tag_error "route" in

  (* Stage 4+5+6: Collect, Execute/Output *)
  match api_result with
  | Error e ->
    update_state agent (fun s -> { s with config = original_config });
    Error e
  | Ok response ->
    (* Stage 3.5: Async output validation *)
    (match Guardrails_async.run_output async_guard.output_validators response with
     | Guardrails_async.Fail { validator_name; reason } ->
       update_state agent (fun s -> { s with config = original_config });
       Error (Error.Agent (GuardrailViolation {
         validator = validator_name; reason }))
     | Guardrails_async.Pass ->
    let* () = stage_collect ?raw_trace_run agent ~original_config response
      |> tag_error "collect" in
    stage_output ?raw_trace_run agent
      ~effective_guardrails:prep.effective_guardrails response
    |> tag_error "output"))

[@@@coverage off]
(* === Inline tests === *)

let%test "last_tool_results_from empty messages" =
  last_tool_results_from [] = []

let%test "last_tool_results_from no tool results" =
  let msgs = [
    { role = User; content = [Text "hello"]; name = None; tool_call_id = None };
  ] in
  last_tool_results_from msgs = []

let%test "last_tool_results_from finds tool results in last user message" =
  let msgs = [
    { role = Assistant; content = [Text "thinking..."]; name = None; tool_call_id = None };
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "result1"; is_error = false };
        ToolResult { tool_use_id = "t2"; content = "error msg"; is_error = true };
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Ok { content = "result1" }; Error { message = "error msg"; recoverable = true }] -> true
  | _ -> false

let%test "last_tool_results_from skips non-tool user messages" =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "first"; is_error = false };
      ]; name = None; tool_call_id = None };
    { role = Assistant; content = [Text "response"]; name = None; tool_call_id = None };
    { role = User; content = [Text "follow up"]; name = None; tool_call_id = None };
  ] in
  (* Should find the tool result from the first user message since the last
     user message has no tool results *)
  match last_tool_results_from msgs with
  | [Ok { content = "first" }] -> true
  | _ -> false

let%test "tag_error passes through Ok" =
  let result = tag_error "test_stage" (Ok 42) in
  result = Ok 42

let%test "tag_error passes through Error" =
  let err = Error.Internal "test error" in
  match tag_error "test_stage" (Error err) with
  | Error e -> e = err
  | Ok _ -> false

(* --- Additional coverage tests --- *)

let%test "last_tool_results_from assistant-only messages" =
  let msgs = [
    { role = Assistant; content = [Text "hello"]; name = None; tool_call_id = None };
    { role = Assistant; content = [Text "world"]; name = None; tool_call_id = None };
  ] in
  last_tool_results_from msgs = []

let%test "last_tool_results_from picks last user with tool results" =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "first"; is_error = false };
      ]; name = None; tool_call_id = None };
    { role = Assistant; content = [Text "mid"]; name = None; tool_call_id = None };
    { role = User; content = [
        ToolResult { tool_use_id = "t2"; content = "second"; is_error = false };
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Ok { content = "second" }] -> true
  | _ -> false

let%test "last_tool_results_from mixed content in user message" =
  let msgs = [
    { role = User; content = [
        Text "some text";
        ToolResult { tool_use_id = "t1"; content = "ok"; is_error = false };
        Text "more text";
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Ok { content = "ok" }] -> true
  | _ -> false

let%test "last_tool_results_from error tool result" =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "fail msg"; is_error = true };
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Error { message = "fail msg"; recoverable = true }] -> true
  | _ -> false

let%test "tag_error with Config error" =
  let err = Error.Config (MissingEnvVar { var_name = "X" }) in
  match tag_error "parse" (Error err) with
  | Error e -> e = err
  | Ok _ -> false

let%test "tag_error with Agent error" =
  let err = Error.Agent (UnrecognizedStopReason { reason = "weird" }) in
  match tag_error "output" (Error err) with
  | Error e -> e = err
  | Ok _ -> false

let%test "tag_error string result Ok" =
  tag_error "collect" (Ok "success") = Ok "success"

(* --- Additional pipeline tests --- *)

let%test "last_tool_results_from only non-user roles" =
  let msgs = [
    { role = System; content = [Text "system"]; name = None; tool_call_id = None };
    { role = Assistant; content = [Text "reply"]; name = None; tool_call_id = None };
  ] in
  last_tool_results_from msgs = []

let%test "last_tool_results_from multiple tool results in one message" =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "r1"; is_error = false };
        ToolResult { tool_use_id = "t2"; content = "r2"; is_error = false };
        ToolResult { tool_use_id = "t3"; content = "r3"; is_error = true };
      ]; name = None; tool_call_id = None };
  ] in
  List.length (last_tool_results_from msgs) = 3

let%test "last_tool_results_from user msg with only non-tool content" =
  let msgs = [
    { role = User; content = [
        Text "just text";
        Types.ToolUse { id = "tu1"; name = "fn"; input = `Null };
      ]; name = None; tool_call_id = None };
  ] in
  last_tool_results_from msgs = []

let%test "tag_error with Serialization error" =
  let err = Error.Serialization (JsonParseError { detail = "bad json" }) in
  match tag_error "route" (Error err) with
  | Error e -> e = err
  | Ok _ -> false

let%test "tag_error with Io error" =
  let err = Error.Io (FileOpFailed { op = "read"; path = "/tmp/x"; detail = "not found" }) in
  match tag_error "input" (Error err) with
  | Error e -> e = err
  | Ok _ -> false

let%test "tag_error with Mcp error" =
  let err = Error.Mcp (InitializeFailed { detail = "timeout" }) in
  match tag_error "parse" (Error err) with
  | Error e -> e = err
  | Ok _ -> false

let%test "tag_error Ok unit" =
  tag_error "collect" (Ok ()) = Ok ()

let%test "tag_error Ok list" =
  tag_error "output" (Ok [1; 2; 3]) = Ok [1; 2; 3]
