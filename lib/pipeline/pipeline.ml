open Types
open Agent_types
open Agent_trace
include Pipeline_common
open Stage_input
open Stage_parse
open Stage_route
open Stage_collect
open Stage_execute
open Stage_output


let run_turn ~sw ?clock ~api_strategy ?raw_trace_run agent =
  (* Stage 1: Input *)
  stage_input ?raw_trace_run agent;

  (* Stage 2: Parse *)
  let (prep, original_config, turn_params) = stage_parse ?raw_trace_run agent in

  (* Stage 2.3: Proactive watermark compaction — compact before overflow.
     Runs before async input validation so that validators operate on the
     already-compacted message set.  Re-prepares the turn via
     Agent_turn.prepare_turn directly (not stage_parse) to avoid emitting
     TurnStarted a second time or re-invoking before_turn_params.

     Hard budget gate (OAS-2): when context_compact_ratio is not configured,
     a ratio >= 0.9 still triggers compaction. This prevents the silent
     pass-through that caused a downstream consumer's CTX 101% overrun
     (observed in upstream issue #7083). *)
  let prep =
    let watermark = match agent.state.config.context_compact_ratio with
      | Some w when w > 0.0 && w < 1.0 -> w
      | _ -> 0.9  (* hard floor: compact before 90% regardless of config *)
    in
    let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
    let context_window = proactive_context_window_tokens agent in
    let ratio = float_of_int est_tokens /. float_of_int context_window in
    if ratio >= watermark then begin
      (* Emit ContextOverflowImminent before compaction *)
      (match agent.options.event_bus with
       | Some bus -> Event_bus.publish bus
           { meta = event_envelope agent;
             payload = ContextOverflowImminent {
               agent_name = agent.state.config.name;
               estimated_tokens = est_tokens;
               limit_tokens = context_window;
               ratio } }
       | None -> ());
      (* Emit ContextCompactStarted *)
      (match agent.options.event_bus with
       | Some bus -> Event_bus.publish bus
           { meta = event_envelope agent;
             payload = ContextCompactStarted {
               agent_name = agent.state.config.name;
               trigger = "proactive" } }
       | None -> ());
      let compacted = proactive_compact ?raw_trace_run agent ~watermark () in
      if compacted then
        prepare_turn_for_agent agent ~turn_params
      else prep
    end
    else prep
  in

  (* Stage 2.5: Async input validation *)
  let async_guard = agent.options.guardrails_async in
  (match Guardrails_async.run_input async_guard.input_validators
           prep.Agent_turn.effective_messages with
   | Guardrails_async.Fail { validator_name; reason } ->
     update_state agent (fun s -> { s with config = original_config });
     Error (Error.Agent (GuardrailViolation {
       validator = validator_name; reason }))
   | Guardrails_async.Pass ->

  (* Stage 2.7: Proactive watermark compaction — post-validation pass.
     Same hard budget gate as 2.3 — if context still exceeds watermark
     after validation (validators can inject messages), compact again. *)
  let prep =
    let watermark = match agent.state.config.context_compact_ratio with
      | Some w when w > 0.0 && w < 1.0 -> w
      | _ -> 0.9
    in
    let est_tokens = total_prompt_tokens_for_agent agent agent.state.messages in
    let context_window = proactive_context_window_tokens agent in
    let ratio = float_of_int est_tokens /. float_of_int context_window in
    if ratio >= watermark then begin
      let compacted = proactive_compact ?raw_trace_run agent ~watermark () in
      if compacted then begin
        update_state agent (fun s -> { s with config = original_config });
        let (prep', _, _) = stage_parse ?raw_trace_run agent in
        prep'
      end else prep
    end
    else prep
  in

  (* Stage 3: Route — with compact-and-retry on context overflow *)
  let rec attempt_route ~prep ~compact_attempts =
    let est_input =
      List.fold_left (fun acc msg ->
        acc + Context_reducer.estimate_message_tokens msg)
        0 prep.Agent_turn.effective_messages
    in
    (match agent.options.journal with
     | Some j ->
         Durable_event.append j
           (Llm_request
              { turn = agent.state.turn_count;
                model = agent.state.config.model;
                input_tokens = est_input;
                timestamp = Unix.gettimeofday () })
     | None -> ());
    let t0 = Unix.gettimeofday () in
    let api_result = stage_route ~sw ?clock ~api_strategy agent prep
      |> tag_error "route" in
    let duration_ms = (Unix.gettimeofday () -. t0) *. 1000.0 in
    (match agent.options.journal, api_result with
     | Some j, Ok response ->
         let out_tokens = match response.usage with
           | Some u -> u.output_tokens | None -> 0
         in
         Durable_event.append j
           (Llm_response
              { turn = agent.state.turn_count;
                output_tokens = out_tokens;
                stop_reason = Types.show_stop_reason response.stop_reason;
                duration_ms;
                timestamp = Unix.gettimeofday () })
     | Some j, Error err ->
         Durable_event.append j
           (Error_occurred
              { turn = agent.state.turn_count;
                error_domain = "Api";
                detail = Error.to_string err;
                timestamp = Unix.gettimeofday () })
     | None, _ -> ());
    match api_result with
    | Error (Error.Api (Retry.ContextOverflow { limit; _ }))
      when compact_attempts < 2 ->
      (match agent.options.event_bus with
       | Some bus -> Event_bus.publish bus
           { meta = event_envelope agent;
             payload = ContextCompactStarted {
               agent_name = agent.state.config.name;
               trigger = "emergency" } }
       | None -> ());
      let compacted = emergency_compact ?raw_trace_run agent ?limit () in
      if not compacted then api_result
      else begin
        update_state agent (fun s -> { s with config = original_config });
        let (prep', _, _) = stage_parse ?raw_trace_run agent in
        attempt_route ~prep:prep' ~compact_attempts:(compact_attempts + 1)
      end
    | other -> other
  in
  let api_result = attempt_route ~prep ~compact_attempts:0 in

  (* Stage 4+5+6: Collect, Execute/Output *)
  match api_result with
  | Error e ->
    update_state agent (fun s -> { s with config = original_config });
    Error e
  | Ok raw_response ->
    (* Stage 3.4: Lenient tool-use recovery.
       Some providers (GLM, smaller Ollama models) return tool-call
       intent as text content instead of a ToolUse content block.
       Promote recoverable Text blocks to ToolUse before contract
       validation so the pipeline proceeds normally.
       Ref: Samchon harness Layer 1 (dev.to/samchon, Qwen 2025). *)
    let valid_tool_names = Tool_set.names agent.tools in
    let response =
      Tool_use_recovery.recover_response ~valid_tool_names raw_response
    in
    let* () =
      validate_completion_contract agent response
      |> tag_error "route_contract"
    in
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
        ToolResult { tool_use_id = "t1"; content = "result1"; is_error = false; json = None };
        ToolResult { tool_use_id = "t2"; content = "error msg"; is_error = true; json = None };
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [ Ok { content = "result1" };
      Error { message = "error msg"; recoverable = true; error_class = None } ] ->
      true
  | _ -> false

let%test "last_tool_results_from skips non-tool user messages" =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "first"; is_error = false; json = None };
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
        ToolResult { tool_use_id = "t1"; content = "first"; is_error = false; json = None };
      ]; name = None; tool_call_id = None };
    { role = Assistant; content = [Text "mid"]; name = None; tool_call_id = None };
    { role = User; content = [
        ToolResult { tool_use_id = "t2"; content = "second"; is_error = false; json = None };
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Ok { content = "second" }] -> true
  | _ -> false

let%test "last_tool_results_from mixed content in user message" =
  let msgs = [
    { role = User; content = [
        Text "some text";
        ToolResult { tool_use_id = "t1"; content = "ok"; is_error = false; json = None };
        Text "more text";
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Ok { content = "ok" }] -> true
  | _ -> false

let%test "last_tool_results_from error tool result" =
  let msgs = [
    { role = User; content = [
        ToolResult { tool_use_id = "t1"; content = "fail msg"; is_error = true; json = None };
      ]; name = None; tool_call_id = None };
  ] in
  match last_tool_results_from msgs with
  | [Error { message = "fail msg"; recoverable = true; error_class = None }] ->
      true
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
        ToolResult { tool_use_id = "t1"; content = "r1"; is_error = false; json = None };
        ToolResult { tool_use_id = "t2"; content = "r2"; is_error = false; json = None };
        ToolResult { tool_use_id = "t3"; content = "r3"; is_error = true; json = None };
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

(* --- Proactive compaction: phase selection is tested via
   Budget_strategy inline tests; integration tested via consumer agent
   turns that set context_compact_ratio in agent config. --- *)
