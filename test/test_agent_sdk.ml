open Alcotest
open Agent_sdk
open Types

let test_model_string () =
  Alcotest.(check string) "claude_sonnet" "claude-sonnet-4-20250514" (model_to_string Claude_sonnet_4)

let test_role_string () =
  Alcotest.(check string) "user" "user" (role_to_string User);
  Alcotest.(check string) "assistant" "assistant" (role_to_string Assistant)

let test_stop_reason () =
  Alcotest.(check bool) "end_turn" true (stop_reason_of_string "end_turn" = EndTurn);
  Alcotest.(check bool) "tool_use" true (stop_reason_of_string "tool_use" = StopToolUse)

let test_simple_tool () =
  let tool = Tool.create 
    ~name:"echo" 
    ~description:"Echo input" 
    ~parameters:[{ name="msg"; description="Message"; param_type=String; required=true }] 
    (fun input ->
       let msg = Yojson.Safe.Util.(input |> member "msg" |> to_string) in
       Ok msg)
  in
  
  let input = `Assoc [("msg", `String "hello")] in
  match Tool.execute tool input with
  | Ok output -> Alcotest.(check string) "echo output" "hello" output
  | Error _ -> Alcotest.fail "Tool execution failed"

let test_extract_text () =
  let content = [Text "Hello"; ToolUse ("1", "t", `Null); Text " World"] in
  let text = List.filter_map (function Text s -> Some s | _ -> None) content |> String.concat "" in
  Alcotest.(check string) "extract text" "Hello World" text

let test_agent_create () =
  Eio_main.run @@ fun env ->
  let agent = Agent.create ~net:env#net ~base_url:"http://test" () in
  Alcotest.(check int) "initial turn count" 0 agent.state.turn_count;
  Alcotest.(check int) "initial messages" 0 (List.length agent.state.messages)

let test_add_message () =
  Eio_main.run @@ fun env ->
  let agent = Agent.create ~net:env#net ~base_url:"http://test" () in
  agent.state <- { agent.state with messages = agent.state.messages @ [{ role = User; content = [Text "Hi"] }] };
  Alcotest.(check int) "message count" 1 (List.length agent.state.messages)

let () =
  run "Agent SDK" [
    "types", [
      test_case "model_string" `Quick test_model_string;
      test_case "role_string" `Quick test_role_string;
      test_case "stop_reason" `Quick test_stop_reason;
    ];
    "tool", [
      test_case "simple_tool" `Quick test_simple_tool;
    ];
    "api", [
      test_case "extract_text" `Quick test_extract_text;
    ];
    "agent", [
      test_case "create" `Quick test_agent_create;
      test_case "add_message" `Quick test_add_message;
    ];
  ]
