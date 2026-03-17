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
  let messages = [{ Types.role = User; content = [Text "hello"] }] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
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
  let messages = [{ Types.role = User; content = [Text "hi"] }] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
    ~tools:Tool_set.empty ~messages
    ~context_reducer:None
    ~turn_params:Hooks.default_turn_params in
  (match prep.tools_json with
   | None -> ()
   | Some _ -> Alcotest.fail "expected no tools_json for empty tool set");
  Alcotest.(check int) "1 message" 1 (List.length prep.effective_messages)

let test_prepare_turn_preserves_messages () =
  let messages = [
    { Types.role = User; content = [Text "hello"] };
    { Types.role = Assistant; content = [Text "hi there"] };
    { Types.role = User; content = [Text "how are you"] };
  ] in
  let prep = Agent_turn.prepare_turn
    ~guardrails:Guardrails.default
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
  ]
