(** Pipeline tests — verify the pipeline's type contracts and
    the agent's behavior through mock provider (unit-level).
    Pipeline is internal, so we test correctness through
    Provider_mock.next_response and agent state inspection. *)

open Agent_sdk

(* ── Provider mock: verify pipeline stages via mock responses ── *)

let test_mock_text_response () =
  let mock = Provider_mock.create
    ~responses:[Provider_mock.text_response "hello from pipeline"]
    () in
  match Provider_mock.next_response mock [] with
  | Ok resp ->
    Alcotest.(check string) "stop_reason" "end_turn"
      (match resp.stop_reason with EndTurn -> "end_turn" | _ -> "other");
    let text = List.filter_map (function
      | Types.Text s -> Some s | _ -> None
    ) resp.content |> String.concat "" in
    Alcotest.(check string) "text content" "hello from pipeline" text
  | Error e -> Alcotest.failf "unexpected: %s" (Error.to_string e)

let test_mock_tool_use_response () =
  let mock = Provider_mock.create
    ~responses:[
      Provider_mock.tool_use_response
        ~tool_name:"search" ~tool_input:(`Assoc [("q", `String "test")]) ()
    ] () in
  match Provider_mock.next_response mock [] with
  | Ok resp ->
    Alcotest.(check string) "stop_reason" "tool_use"
      (match resp.stop_reason with StopToolUse -> "tool_use" | _ -> "other");
    let tool_names = List.filter_map (function
      | Types.ToolUse { name; _ } -> Some name | _ -> None
    ) resp.content in
    Alcotest.(check (list string)) "tool names" ["search"] tool_names
  | Error e -> Alcotest.failf "unexpected: %s" (Error.to_string e)

let test_mock_tool_then_text_sequence () =
  let responses = Provider_mock.tool_then_text
    ~tool_name:"calc" ~tool_input:(`Assoc []) ~final_text:"42" () in
  let mock = Provider_mock.create ~responses () in
  (* First call: tool use *)
  (match Provider_mock.next_response mock [] with
   | Ok resp ->
     Alcotest.(check string) "first is tool_use" "tool_use"
       (match resp.stop_reason with StopToolUse -> "tool_use" | _ -> "other")
   | Error _ -> Alcotest.fail "expected first response");
  (* Second call: text *)
  (match Provider_mock.next_response mock [] with
   | Ok resp ->
     let text = List.filter_map (function
       | Types.Text s -> Some s | _ -> None
     ) resp.content |> String.concat "" in
     Alcotest.(check string) "final text" "42" text
   | Error _ -> Alcotest.fail "expected second response");
  Alcotest.(check int) "call count" 2 (Provider_mock.call_count mock)

(* ── Agent state: verify pipeline modifies state correctly ─── *)

let test_agent_initial_state () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent = Agent.create ~net
    ~config:{ Types.default_config with name = "state-test"; max_turns = 10 }
    () in
  let state = Agent.state agent in
  Alcotest.(check int) "initial turn_count" 0 state.turn_count;
  Alcotest.(check int) "initial api_calls" 0 state.usage.api_calls;
  Alcotest.(check (list string)) "initial messages" []
    (List.map (fun (m : Types.message) ->
       match m.content with
       | [Types.Text s] -> s
       | _ -> "<complex>"
     ) state.messages)

let test_agent_tools_registered () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let tool = Tool.create ~name:"my_tool" ~description:"test"
    ~parameters:[] (fun _ -> Ok { Types.content = "ok" }) in
  let agent = Agent.create ~net ~tools:[tool] () in
  let tools = Agent.tools agent in
  Alcotest.(check int) "tool count" 1 (Tool_set.size tools);
  Alcotest.(check string) "tool name" "my_tool"
    (List.hd (Tool_set.to_list tools)).schema.name

(* ── Pipeline type contracts ─────────────────────────────── *)

let test_agent_turn_preparation () =
  (* Verify Agent_turn.prepare_turn produces valid preparation *)
  let tools = Tool_set.of_list [
    Tool.create ~name:"a" ~description:"tool a" ~parameters:[]
      (fun _ -> Ok { Types.content = "a" });
    Tool.create ~name:"b" ~description:"tool b" ~parameters:[]
      (fun _ -> Ok { Types.content = "b" });
  ] in
  let messages = [{ Types.role = User; content = [Text "hello"]; name = None; tool_call_id = None }] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~operator_policy:None
    ~tools ~messages
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params in
  (* tools_json should be Some with 2 tools *)
  (match prep.tools_json with
   | Some tools_json ->
     Alcotest.(check int) "2 tools in json" 2 (List.length tools_json)
   | None -> Alcotest.fail "expected tools_json");
  (* effective_messages should contain our user message *)
  Alcotest.(check int) "1 message" 1 (List.length prep.effective_messages)

let test_agent_turn_idle_detection () =
  let tool_uses = [
    Types.ToolUse { id = "1"; name = "search"; input = `Assoc [] }
  ] in
  let idle_result = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses in
  Alcotest.(check bool) "first call not idle" false idle_result.is_idle;
  (* Same tool again *)
  let idle_result2 = Agent_turn.update_idle_detection
    ~idle_state:idle_result.new_state
    ~tool_uses in
  Alcotest.(check bool) "repeated call is idle" true idle_result2.is_idle;
  Alcotest.(check int) "consecutive 1" 1
    idle_result2.new_state.consecutive_idle_turns

(* ── Provider_mock: additional coverage ─────────────────── *)

let test_mock_reset () =
  let mock = Provider_mock.create
    ~responses:[Provider_mock.text_response "a"; Provider_mock.text_response "b"]
    () in
  let _r1 = Provider_mock.next_response mock [] in
  Alcotest.(check int) "count after 1" 1 (Provider_mock.call_count mock);
  Provider_mock.reset mock;
  Alcotest.(check int) "count after reset" 0 (Provider_mock.call_count mock);
  (* First response again *)
  (match Provider_mock.next_response mock [] with
   | Ok resp ->
     let text = List.filter_map (function
       | Types.Text s -> Some s | _ -> None
     ) resp.content |> String.concat "" in
     Alcotest.(check string) "reset replays from start" "a" text
   | Error _ -> Alcotest.fail "unexpected error after reset")

let test_mock_cycle_wraps () =
  let mock = Provider_mock.create
    ~responses:[Provider_mock.text_response "only"]
    () in
  let _r1 = Provider_mock.next_response mock [] in
  let _r2 = Provider_mock.next_response mock [] in
  let _r3 = Provider_mock.next_response mock [] in
  Alcotest.(check int) "count after 3 calls" 3 (Provider_mock.call_count mock);
  (* All 3 calls succeed because of wrap-around *)
  (match _r3 with
   | Ok resp ->
     let text = List.filter_map (function
       | Types.Text s -> Some s | _ -> None
     ) resp.content |> String.concat "" in
     Alcotest.(check string) "wrapped" "only" text
   | Error _ -> Alcotest.fail "wrap-around failed")

let test_mock_empty_responses () =
  let mock = Provider_mock.create ~responses:[] () in
  match Provider_mock.next_response mock [] with
  | Error (Error.Internal msg) ->
    Alcotest.(check bool) "mentions empty" true
      (String.length msg > 0)
  | Error _ -> Alcotest.fail "wrong error type"
  | Ok _ -> Alcotest.fail "expected error for empty responses"

let test_mock_thinking_response () =
  let mock = Provider_mock.create
    ~responses:[Provider_mock.thinking_response
      ~thinking:"Let me think..." ~text:"The answer is 42" ()]
    () in
  match Provider_mock.next_response mock [] with
  | Ok resp ->
    let has_thinking = List.exists (function
      | Types.Thinking _ -> true | _ -> false
    ) resp.content in
    let has_text = List.exists (function
      | Types.Text _ -> true | _ -> false
    ) resp.content in
    Alcotest.(check bool) "has thinking" true has_thinking;
    Alcotest.(check bool) "has text" true has_text
  | Error e -> Alcotest.failf "unexpected: %s" (Error.to_string e)

let test_mock_to_provider_config () =
  let cfg = Provider_mock.to_provider_config () in
  Alcotest.(check string) "model_id" "mock-model" cfg.model_id;
  Alcotest.(check string) "api_key_env" "MOCK_API_KEY" cfg.api_key_env

(* ── Guardrails: tool filtering (exercises pipeline's Stage 2/5) ── *)

let test_guardrails_allow_all () =
  let g = Guardrails.default in
  let schema : Types.tool_schema = {
    name = "test"; description = "test"; parameters = [];
  } in
  Alcotest.(check bool) "allow all" true (Guardrails.is_allowed g schema)

let test_guardrails_allow_list () =
  let g = { Guardrails.tool_filter = AllowList ["search"; "calc"];
             max_tool_calls_per_turn = None } in
  let s1 : Types.tool_schema = { name = "search"; description = ""; parameters = [] } in
  let s2 : Types.tool_schema = { name = "unknown"; description = ""; parameters = [] } in
  Alcotest.(check bool) "search allowed" true (Guardrails.is_allowed g s1);
  Alcotest.(check bool) "unknown denied" false (Guardrails.is_allowed g s2)

let test_guardrails_deny_list () =
  let g = { Guardrails.tool_filter = DenyList ["dangerous"];
             max_tool_calls_per_turn = None } in
  let s1 : Types.tool_schema = { name = "safe"; description = ""; parameters = [] } in
  let s2 : Types.tool_schema = { name = "dangerous"; description = ""; parameters = [] } in
  Alcotest.(check bool) "safe allowed" true (Guardrails.is_allowed g s1);
  Alcotest.(check bool) "dangerous denied" false (Guardrails.is_allowed g s2)

let test_guardrails_exceeds_limit () =
  let g = { Guardrails.tool_filter = AllowAll;
             max_tool_calls_per_turn = Some 3 } in
  Alcotest.(check bool) "2 within" false (Guardrails.exceeds_limit g 2);
  Alcotest.(check bool) "3 within" false (Guardrails.exceeds_limit g 3);
  Alcotest.(check bool) "4 exceeds" true (Guardrails.exceeds_limit g 4)

let test_guardrails_no_limit () =
  let g = Guardrails.default in
  Alcotest.(check bool) "100 within (no limit)" false (Guardrails.exceeds_limit g 100)

(* ── Agent state: more detail ──────────────────────────── *)

let test_agent_initial_usage () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent = Agent.create ~net
    ~config:{ Types.default_config with name = "usage-test" }
    () in
  let state = Agent.state agent in
  Alcotest.(check int) "total_input_tokens 0" 0 state.usage.total_input_tokens;
  Alcotest.(check int) "total_output_tokens 0" 0 state.usage.total_output_tokens;
  Alcotest.(check int) "api_calls 0" 0 state.usage.api_calls

let test_agent_empty_tools () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent = Agent.create ~net ~tools:[] () in
  let tools = Agent.tools agent in
  Alcotest.(check int) "0 tools" 0 (Tool_set.size tools)

let test_agent_multiple_tools () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let t1 = Tool.create ~name:"tool_a" ~description:"A"
    ~parameters:[] (fun _ -> Ok { Types.content = "a" }) in
  let t2 = Tool.create ~name:"tool_b" ~description:"B"
    ~parameters:[] (fun _ -> Ok { Types.content = "b" }) in
  let t3 = Tool.create ~name:"tool_c" ~description:"C"
    ~parameters:[] (fun _ -> Ok { Types.content = "c" }) in
  let agent = Agent.create ~net ~tools:[t1; t2; t3] () in
  Alcotest.(check int) "3 tools" 3 (Tool_set.size (Agent.tools agent))

(* ── Agent_turn: prepare_turn edge cases ──────────────── *)

let test_prepare_turn_no_tools () =
  let messages = [{ Types.role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~operator_policy:None
    ~tools:Tool_set.empty ~messages
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params in
  (match prep.tools_json with
   | None -> ()
   | Some _ -> Alcotest.fail "expected no tools_json for empty tool set");
  Alcotest.(check int) "1 message" 1 (List.length prep.effective_messages)

let test_prepare_turn_preserves_messages () =
  let messages = [
    { Types.role = User; content = [Text "hello"]; name = None; tool_call_id = None };
    { Types.role = Assistant; content = [Text "hi there"]; name = None; tool_call_id = None };
    { Types.role = User; content = [Text "how are you"]; name = None; tool_call_id = None };
  ] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~operator_policy:None
    ~tools:Tool_set.empty ~messages
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params in
  Alcotest.(check int) "3 messages" 3 (List.length prep.effective_messages)

(* ── Agent_turn: idle detection edge cases ────────────── *)

let test_idle_detection_different_tools () =
  let tool1 = [
    Types.ToolUse { id = "1"; name = "search"; input = `Assoc [("q", `String "a")] }
  ] in
  let tool2 = [
    Types.ToolUse { id = "2"; name = "calc"; input = `Assoc [("x", `Int 1)] }
  ] in
  let r1 = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses:tool1 in
  Alcotest.(check bool) "first not idle" false r1.is_idle;
  let r2 = Agent_turn.update_idle_detection
    ~idle_state:r1.new_state
    ~tool_uses:tool2 in
  Alcotest.(check bool) "different tool not idle" false r2.is_idle;
  Alcotest.(check int) "consecutive 0" 0 r2.new_state.consecutive_idle_turns

let test_idle_detection_same_input () =
  let tool_uses = [
    Types.ToolUse { id = "1"; name = "search"; input = `Assoc [("q", `String "test")] }
  ] in
  let r1 = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses in
  let r2 = Agent_turn.update_idle_detection
    ~idle_state:r1.new_state
    ~tool_uses in
  Alcotest.(check bool) "same input is idle" true r2.is_idle;
  let r3 = Agent_turn.update_idle_detection
    ~idle_state:r2.new_state
    ~tool_uses in
  Alcotest.(check bool) "still idle" true r3.is_idle;
  Alcotest.(check int) "consecutive 2" 2 r3.new_state.consecutive_idle_turns

let test_idle_detection_empty_tools () =
  let r1 = Agent_turn.update_idle_detection
    ~idle_state:{ last_tool_calls = None; consecutive_idle_turns = 0 }
    ~tool_uses:[] in
  Alcotest.(check bool) "empty not idle" false r1.is_idle;
  let r2 = Agent_turn.update_idle_detection
    ~idle_state:r1.new_state
    ~tool_uses:[] in
  Alcotest.(check bool) "empty again is idle" true r2.is_idle

(* ── Agent_turn.make_tool_results ───────────────────── *)

let test_make_tool_results_ok () =
  let results = [
    ("tu1", "result1", false);
    ("tu2", "result2", false);
  ] in
  let tool_results = Agent_turn.make_tool_results results in
  Alcotest.(check int) "2 tool results" 2 (List.length tool_results);
  List.iter (fun block ->
    match block with
    | Types.ToolResult { is_error; _ } ->
      Alcotest.(check bool) "not error" false is_error
    | _ -> Alcotest.fail "expected ToolResult"
  ) tool_results

let test_make_tool_results_error () =
  let results = [
    ("tu1", "failed", true);
  ] in
  let tool_results = Agent_turn.make_tool_results results in
  Alcotest.(check int) "1 tool result" 1 (List.length tool_results);
  (match List.hd tool_results with
   | Types.ToolResult { is_error = true; content; _ } ->
     Alcotest.(check bool) "error content" true (String.length content > 0)
   | _ -> Alcotest.fail "expected error ToolResult")

let test_make_tool_results_mixed () =
  let results = [
    ("tu1", "good", false);
    ("tu2", "bad", true);
  ] in
  let tool_results = Agent_turn.make_tool_results results in
  Alcotest.(check int) "2 results" 2 (List.length tool_results)

(* ── accumulate_usage ────────────────────────────────────── *)

let test_accumulate_usage_with_response () =
  let current = Types.empty_usage in
  let resp_usage = Some {
    Types.input_tokens = 100; output_tokens = 50;
    cache_creation_input_tokens = 20; cache_read_input_tokens = 10;
    cost_usd = None
  } in
  let result = Agent_turn.accumulate_usage
    ~current_usage:current ~provider:None ~response_usage:resp_usage in
  Alcotest.(check int) "input tokens" 100 result.total_input_tokens;
  Alcotest.(check int) "output tokens" 50 result.total_output_tokens;
  Alcotest.(check int) "api_calls" 1 result.api_calls

let test_accumulate_usage_no_response () =
  let current = { Types.empty_usage with
    api_calls = 2; total_input_tokens = 500; total_output_tokens = 200 } in
  let result = Agent_turn.accumulate_usage
    ~current_usage:current ~provider:None ~response_usage:None in
  Alcotest.(check int) "api_calls incremented" 3 result.api_calls;
  Alcotest.(check int) "input tokens preserved" 500 result.total_input_tokens;
  Alcotest.(check int) "output tokens preserved" 200 result.total_output_tokens

let test_accumulate_usage_cumulative () =
  let u1 = Some {
    Types.input_tokens = 50; output_tokens = 20;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0;
    cost_usd = None
  } in
  let u2 = Some {
    Types.input_tokens = 30; output_tokens = 10;
    cache_creation_input_tokens = 0; cache_read_input_tokens = 0;
    cost_usd = None
  } in
  let after1 = Agent_turn.accumulate_usage
    ~current_usage:Types.empty_usage ~provider:None ~response_usage:u1 in
  let after2 = Agent_turn.accumulate_usage
    ~current_usage:after1 ~provider:None ~response_usage:u2 in
  Alcotest.(check int) "cumulative input" 80 after2.total_input_tokens;
  Alcotest.(check int) "cumulative output" 30 after2.total_output_tokens;
  Alcotest.(check int) "cumulative calls" 2 after2.api_calls

(* ── check_token_budget ──────────────────────────────────── *)

let test_token_budget_no_limit () =
  let config = Types.default_config in
  let usage = { Types.empty_usage with total_input_tokens = 999999 } in
  Alcotest.(check bool) "no limit = None" true
    (Option.is_none (Agent_turn.check_token_budget config usage))

let test_token_budget_input_under () =
  let config = { Types.default_config with max_input_tokens = Some 1000 } in
  let usage = { Types.empty_usage with total_input_tokens = 500 } in
  Alcotest.(check bool) "under limit" true
    (Option.is_none (Agent_turn.check_token_budget config usage))

let test_token_budget_input_exceeded () =
  let config = { Types.default_config with max_input_tokens = Some 1000 } in
  let usage = { Types.empty_usage with total_input_tokens = 1500 } in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; used; limit })) ->
    Alcotest.(check string) "kind" "Input" kind;
    Alcotest.(check int) "used" 1500 used;
    Alcotest.(check int) "limit" 1000 limit
  | _ -> Alcotest.fail "expected TokenBudgetExceeded"

let test_token_budget_total_exceeded () =
  let config = { Types.default_config with max_total_tokens = Some 200 } in
  let usage = {
    Types.empty_usage with
    total_input_tokens = 120; total_output_tokens = 100;
  } in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; used; limit })) ->
    Alcotest.(check string) "kind" "Total" kind;
    Alcotest.(check int) "used" 220 used;
    Alcotest.(check int) "limit" 200 limit
  | _ -> Alcotest.fail "expected TokenBudgetExceeded"

let test_token_budget_total_under () =
  let config = { Types.default_config with max_total_tokens = Some 500 } in
  let usage = {
    Types.empty_usage with
    total_input_tokens = 200; total_output_tokens = 100;
  } in
  Alcotest.(check bool) "under total limit" true
    (Option.is_none (Agent_turn.check_token_budget config usage))

let test_token_budget_input_takes_precedence () =
  (* If both input and total are exceeded, input error is returned *)
  let config = {
    Types.default_config with
    max_input_tokens = Some 100; max_total_tokens = Some 200;
  } in
  let usage = {
    Types.empty_usage with
    total_input_tokens = 150; total_output_tokens = 100;
  } in
  match Agent_turn.check_token_budget config usage with
  | Some (Error.Agent (TokenBudgetExceeded { kind; _ })) ->
    Alcotest.(check string) "input takes precedence" "Input" kind
  | _ -> Alcotest.fail "expected TokenBudgetExceeded"

(* ── prepare_turn: extra_system_context ──────────────────── *)

let test_prepare_turn_extra_context () =
  let messages = [{ Types.role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
  let turn_params = { Hooks.default_turn_params with
    extra_system_context = Some "You are in debug mode." } in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~operator_policy:None
    ~tools:Tool_set.empty ~messages
    ~context_reducer:None
    ~turn_params in
  (* Extra context adds a system message at the start *)
  Alcotest.(check int) "2 messages (context + original)" 2
    (List.length prep.effective_messages);
  let first = List.hd prep.effective_messages in
  (match first.content with
   | [Types.Text s] ->
     Alcotest.(check string) "injected context"
       "[system context] You are in debug mode." s
   | _ -> Alcotest.fail "expected Text block")

(* ── prepare_turn: tool_filter_override ──────────────────── *)

let test_prepare_turn_tool_filter_override () =
  let tools = Tool_set.of_list [
    Tool.create ~name:"allowed" ~description:"ok" ~parameters:[]
      (fun _ -> Ok { Types.content = "ok" });
    Tool.create ~name:"blocked" ~description:"no" ~parameters:[]
      (fun _ -> Ok { Types.content = "no" });
  ] in
  let messages = [{ Types.role = User; content = [Text "hi"]; name = None; tool_call_id = None }] in
  let turn_params = { Hooks.default_turn_params with
    tool_filter_override = Some (AllowList ["allowed"]) } in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~operator_policy:None
    ~tools ~messages
    ~context_reducer:None
    ~turn_params in
  match prep.tools_json with
  | Some tools_json ->
    Alcotest.(check int) "only 1 tool after filter" 1 (List.length tools_json)
  | None -> Alcotest.fail "expected some tools"

(* ── Error_domain: tag_error ─────────────────────────────── *)

let test_error_domain_of_sdk_error () =
  let err = Error.Agent (UnrecognizedStopReason { reason = "weird" }) in
  let poly = Error_domain.of_sdk_error err in
  match poly with
  | `Unrecognized_stop_reason s ->
    Alcotest.(check string) "reason" "weird" s
  | _ -> Alcotest.fail "expected Unrecognized_stop_reason"

let test_error_domain_roundtrip () =
  let err = Error.Internal "test error" in
  let poly = Error_domain.of_sdk_error err in
  let back = Error_domain.to_sdk_error poly in
  Alcotest.(check string) "roundtrip" (Error.to_string err) (Error.to_string back)

let test_error_domain_with_stage () =
  let poly = Error_domain.of_sdk_error (Error.Internal "fail") in
  let ctx = Error_domain.with_stage "route" poly in
  let s = Error_domain.ctx_to_string ctx in
  Alcotest.(check bool) "contains stage" true
    (String.length s > 0);
  Alcotest.(check bool) "has route prefix" true
    (let prefix = "[route]" in
     String.length s >= String.length prefix &&
     String.sub s 0 (String.length prefix) = prefix)

let test_error_domain_is_retryable () =
  Alcotest.(check bool) "rate limited is retryable" true
    (Error_domain.is_retryable (`Rate_limited (Some 1.0)));
  Alcotest.(check bool) "internal not retryable" false
    (Error_domain.is_retryable (`Internal "nope"));
  Alcotest.(check bool) "network error retryable" true
    (Error_domain.is_retryable (`Network_error "timeout"))

let test_error_domain_provider_errors () =
  let errs : Error_domain.sdk_error_poly list = [
    `Auth_error "bad key";
    `Server_error (500, "internal");
    `Overloaded;
    `Provider_timeout "slow";
    `Invalid_request "bad";
  ] in
  List.iter (fun e ->
    let s = Error_domain.to_string e in
    Alcotest.(check bool) "has string" true (String.length s > 0)
  ) errs

(* ── Provider_mock: multi-tool response ───────────────────── *)

let test_mock_multi_tool_response () =
  let mock = Provider_mock.create
    ~responses:[Provider_mock.tool_use_response
      ~tool_name:"t1" ~tool_input:(`Assoc []) ()]
    () in
  match Provider_mock.next_response mock [] with
  | Ok resp ->
    let tool_count = List.length (List.filter (function
      | Types.ToolUse _ -> true | _ -> false
    ) resp.content) in
    Alcotest.(check bool) "has tool use" true (tool_count > 0)
  | Error _ -> Alcotest.fail "expected ok"

(* ── Agent_turn: filter_valid_messages ────────────────────── *)

let test_filter_valid_empty_messages () =
  let extra = [
    { Types.role = User; content = [Text "a"]; name = None; tool_call_id = None };
    { Types.role = Assistant; content = [Text "b"]; name = None; tool_call_id = None };
  ] in
  let result = Agent_turn.filter_valid_messages ~messages:[] extra in
  Alcotest.(check int) "passthrough on empty" 2 (List.length result)

let test_filter_valid_removes_consecutive_same_role () =
  let messages = [
    { Types.role = User; content = [Text "x"]; name = None; tool_call_id = None };
  ] in
  let extra = [
    { Types.role = User; content = [Text "a"]; name = None; tool_call_id = None };
    { Types.role = Assistant; content = [Text "b"]; name = None; tool_call_id = None };
  ] in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  (* First extra msg has same role (User) as last message, should be filtered *)
  Alcotest.(check int) "consecutive same role filtered" 1 (List.length result);
  let first = List.hd result in
  (match first.content with
   | [Types.Text s] -> Alcotest.(check string) "kept assistant" "b" s
   | _ -> Alcotest.fail "wrong content")

let test_filter_valid_alternating_roles () =
  let messages = [
    { Types.role = Assistant; content = [Text "x"]; name = None; tool_call_id = None };
  ] in
  let extra = [
    { Types.role = User; content = [Text "a"]; name = None; tool_call_id = None };
    { Types.role = Assistant; content = [Text "b"]; name = None; tool_call_id = None };
  ] in
  let result = Agent_turn.filter_valid_messages ~messages extra in
  Alcotest.(check int) "alternating kept" 2 (List.length result)

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Pipeline" [
    "mock_responses", [
      Alcotest.test_case "text response" `Quick test_mock_text_response;
      Alcotest.test_case "tool use response" `Quick test_mock_tool_use_response;
      Alcotest.test_case "tool then text" `Quick test_mock_tool_then_text_sequence;
      Alcotest.test_case "reset" `Quick test_mock_reset;
      Alcotest.test_case "cycle wraps" `Quick test_mock_cycle_wraps;
      Alcotest.test_case "empty responses" `Quick test_mock_empty_responses;
      Alcotest.test_case "thinking response" `Quick test_mock_thinking_response;
      Alcotest.test_case "to_provider_config" `Quick test_mock_to_provider_config;
    ];
    "agent_state", [
      Alcotest.test_case "initial state" `Quick test_agent_initial_state;
      Alcotest.test_case "tools registered" `Quick test_agent_tools_registered;
      Alcotest.test_case "initial usage" `Quick test_agent_initial_usage;
      Alcotest.test_case "empty tools" `Quick test_agent_empty_tools;
      Alcotest.test_case "multiple tools" `Quick test_agent_multiple_tools;
    ];
    "turn_mechanics", [
      Alcotest.test_case "turn preparation" `Quick test_agent_turn_preparation;
      Alcotest.test_case "idle detection" `Quick test_agent_turn_idle_detection;
      Alcotest.test_case "no tools prep" `Quick test_prepare_turn_no_tools;
      Alcotest.test_case "preserves messages" `Quick test_prepare_turn_preserves_messages;
      Alcotest.test_case "different tools" `Quick test_idle_detection_different_tools;
      Alcotest.test_case "same input idle" `Quick test_idle_detection_same_input;
      Alcotest.test_case "empty tools idle" `Quick test_idle_detection_empty_tools;
      Alcotest.test_case "extra system context" `Quick test_prepare_turn_extra_context;
      Alcotest.test_case "tool filter override" `Quick test_prepare_turn_tool_filter_override;
    ];
    "guardrails", [
      Alcotest.test_case "allow all" `Quick test_guardrails_allow_all;
      Alcotest.test_case "allow list" `Quick test_guardrails_allow_list;
      Alcotest.test_case "deny list" `Quick test_guardrails_deny_list;
      Alcotest.test_case "exceeds limit" `Quick test_guardrails_exceeds_limit;
      Alcotest.test_case "no limit" `Quick test_guardrails_no_limit;
    ];
    "tool_results", [
      Alcotest.test_case "make ok" `Quick test_make_tool_results_ok;
      Alcotest.test_case "make error" `Quick test_make_tool_results_error;
      Alcotest.test_case "make mixed" `Quick test_make_tool_results_mixed;
    ];
    "accumulate_usage", [
      Alcotest.test_case "with response" `Quick test_accumulate_usage_with_response;
      Alcotest.test_case "no response" `Quick test_accumulate_usage_no_response;
      Alcotest.test_case "cumulative" `Quick test_accumulate_usage_cumulative;
    ];
    "token_budget", [
      Alcotest.test_case "no limit" `Quick test_token_budget_no_limit;
      Alcotest.test_case "input under" `Quick test_token_budget_input_under;
      Alcotest.test_case "input exceeded" `Quick test_token_budget_input_exceeded;
      Alcotest.test_case "total exceeded" `Quick test_token_budget_total_exceeded;
      Alcotest.test_case "total under" `Quick test_token_budget_total_under;
      Alcotest.test_case "input precedence" `Quick test_token_budget_input_takes_precedence;
    ];
    "error_domain", [
      Alcotest.test_case "of_sdk_error" `Quick test_error_domain_of_sdk_error;
      Alcotest.test_case "roundtrip" `Quick test_error_domain_roundtrip;
      Alcotest.test_case "with_stage" `Quick test_error_domain_with_stage;
      Alcotest.test_case "is_retryable" `Quick test_error_domain_is_retryable;
      Alcotest.test_case "provider errors" `Quick test_error_domain_provider_errors;
    ];
    "provider_mock_extra", [
      Alcotest.test_case "multi tool response" `Quick test_mock_multi_tool_response;
    ];
    "filter_valid_messages", [
      Alcotest.test_case "empty messages" `Quick test_filter_valid_empty_messages;
      Alcotest.test_case "same role filtered" `Quick test_filter_valid_removes_consecutive_same_role;
      Alcotest.test_case "alternating roles" `Quick test_filter_valid_alternating_roles;
    ];
  ]
