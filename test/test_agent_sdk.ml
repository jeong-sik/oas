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
       Ok { Types.content = msg })
  in

  let input = `Assoc [("msg", `String "hello")] in
  match Tool.execute tool input with
  | Ok { content } -> Alcotest.(check string) "echo output" "hello" content
  | Error _ -> Alcotest.fail "Tool execution failed"

let test_extract_text () =
  let content = [Text "Hello"; ToolUse { id = "1"; name = "t"; input = `Null }; Text " World"] in
  let text = List.filter_map (function Text s -> Some s | _ -> None) content |> String.concat "" in
  Alcotest.(check string) "extract text" "Hello World" text

let test_agent_create () =
  Eio_main.run @@ fun env ->
  let options = { Agent.default_options with base_url = "http://test" } in
  let agent = Agent.create ~net:env#net ~options () in
  let st = Agent.state agent in
  Alcotest.(check int) "initial turn count" 0 st.turn_count;
  Alcotest.(check int) "initial messages" 0 (List.length st.messages)

let test_agent_accessors () =
  Eio_main.run @@ fun env ->
  let options = { Agent.default_options with base_url = "http://test" } in
  let agent = Agent.create ~net:env#net ~options () in
  Alcotest.(check int) "no tools" 0 (List.length (Agent.tools agent));
  Alcotest.(check bool) "no lifecycle" true (Option.is_none (Agent.lifecycle agent));
  let opts = Agent.options agent in
  Alcotest.(check string) "base_url" "http://test" opts.base_url

let test_version_info () =
  Alcotest.(check string) "version" "0.30.0" Agent_sdk.version;
  Alcotest.(check string) "sdk_name" "agent_sdk" Agent_sdk.sdk_name

let test_build_safe_valid () =
  Eio_main.run @@ fun env ->
  let result =
    Builder.create ~net:env#net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_system_prompt "test"
    |> Builder.with_max_turns 5
    |> Builder.with_max_tokens 1024
    |> Builder.build_safe
  in
  Alcotest.(check bool) "build_safe ok" true (Result.is_ok result)

let test_build_safe_invalid_turns () =
  Eio_main.run @@ fun env ->
  let result =
    Builder.create ~net:env#net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_max_turns 0
    |> Builder.build_safe
  in
  Alcotest.(check bool) "build_safe error" true (Result.is_error result)

let test_build_safe_thinking_without_enable () =
  Eio_main.run @@ fun env ->
  let result =
    Builder.create ~net:env#net ~model:Types.Claude_sonnet_4_6
    |> Builder.with_thinking_budget 1000
    |> Builder.build_safe
  in
  Alcotest.(check bool) "thinking_budget without enable" true (Result.is_error result)

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
      test_case "accessors" `Quick test_agent_accessors;
      test_case "version info" `Quick test_version_info;
    ];
    "builder", [
      test_case "build_safe valid" `Quick test_build_safe_valid;
      test_case "build_safe invalid turns" `Quick test_build_safe_invalid_turns;
      test_case "build_safe thinking" `Quick test_build_safe_thinking_without_enable;
    ];
  ]
