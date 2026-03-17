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
  Alcotest.(check int) "tool count" 1 (List.length tools);
  Alcotest.(check string) "tool name" "my_tool"
    (List.hd tools).schema.name

(* ── Pipeline type contracts ─────────────────────────────── *)

let test_agent_turn_preparation () =
  (* Verify Agent_turn.prepare_turn produces valid preparation *)
  let tools = [
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

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Pipeline" [
    "mock_responses", [
      Alcotest.test_case "text response" `Quick test_mock_text_response;
      Alcotest.test_case "tool use response" `Quick test_mock_tool_use_response;
      Alcotest.test_case "tool then text" `Quick test_mock_tool_then_text_sequence;
    ];
    "agent_state", [
      Alcotest.test_case "initial state" `Quick test_agent_initial_state;
      Alcotest.test_case "tools registered" `Quick test_agent_tools_registered;
    ];
    "turn_mechanics", [
      Alcotest.test_case "turn preparation" `Quick test_agent_turn_preparation;
      Alcotest.test_case "idle detection" `Quick test_agent_turn_idle_detection;
    ];
  ]
