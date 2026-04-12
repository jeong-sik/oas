open Alcotest
open Agent_sdk
open Types

let test_model_string () =
  Alcotest.(check string) "claude_sonnet" "claude-sonnet-4-20250514" (model_to_string "claude-sonnet-4")

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
  Alcotest.(check int) "no tools" 0 (Tool_set.size (Agent.tools agent));
  Alcotest.(check bool) "no lifecycle" true (Option.is_none (Agent.lifecycle agent));
  let opts = Agent.options agent in
  Alcotest.(check string) "base_url" "http://test" opts.base_url

let test_version_info () =
  Alcotest.(check string) "version" Agent_sdk.Sdk_version.version Agent_sdk.version;
  Alcotest.(check string) "sdk_name" "agent_sdk" Agent_sdk.sdk_name

let test_build_safe_valid () =
  Eio_main.run @@ fun env ->
  let result =
    Builder.create ~net:env#net ~model:"claude-sonnet-4-6"
    |> Builder.with_system_prompt "test"
    |> Builder.with_max_turns 5
    |> Builder.with_max_tokens 1024
    |> Builder.build_safe
  in
  Alcotest.(check bool) "build_safe ok" true (Result.is_ok result)

let test_build_safe_invalid_turns () =
  Eio_main.run @@ fun env ->
  let result =
    Builder.create ~net:env#net ~model:"claude-sonnet-4-6"
    |> Builder.with_max_turns 0
    |> Builder.build_safe
  in
  Alcotest.(check bool) "build_safe error" true (Result.is_error result)

let test_build_safe_thinking_without_enable () =
  Eio_main.run @@ fun env ->
  let result =
    Builder.create ~net:env#net ~model:"claude-sonnet-4-6"
    |> Builder.with_thinking_budget 1000
    |> Builder.build_safe
  in
  Alcotest.(check bool) "thinking_budget without enable" true (Result.is_error result)

(* --- create_agent coverage --- *)

let test_create_agent_defaults () =
  Eio_main.run @@ fun env ->
  let agent = Agent_sdk.create_agent ~net:env#net () in
  let st = Agent.state agent in
  Alcotest.(check int) "initial turn count" 0 st.turn_count

let test_create_agent_with_name_model () =
  Eio_main.run @@ fun env ->
  let agent = Agent_sdk.create_agent ~net:env#net
    ~name:"test-agent"
    ~model:"claude-haiku-4-5"
    ~system_prompt:"You are helpful."
    ~max_tokens:2048
    ~max_turns:5
    () in
  let config = (Agent.state agent).config in
  Alcotest.(check string) "name" "test-agent" config.name;
  Alcotest.(check string) "model" "claude-haiku-4-5" config.model;
  Alcotest.(check (option string)) "prompt" (Some "You are helpful.") config.system_prompt;
  Alcotest.(check (option int)) "max_tokens" (Some 2048) config.max_tokens;
  Alcotest.(check int) "max_turns" 5 config.max_turns

let test_create_agent_with_provider () =
  Eio_main.run @@ fun env ->
  let provider_cfg = { Provider.provider = Provider.Local { base_url = "http://localhost:11434" }; model_id = "test"; api_key_env = "" } in
  let agent = Agent_sdk.create_agent ~net:env#net ~provider:provider_cfg () in
  let opts = Agent.options agent in
  Alcotest.(check bool) "provider set" true (Option.is_some opts.provider)

let test_create_agent_with_raw_trace () =
  Eio_main.run @@ fun env ->
  let path = Filename.concat (Filename.get_temp_dir_name ()) "oas-test-trace" in
  (match Raw_trace.create ~path () with
   | Ok trace ->
     let agent = Agent_sdk.create_agent ~net:env#net ~raw_trace:trace () in
     let opts = Agent.options agent in
     Alcotest.(check bool) "raw_trace set" true (Option.is_some opts.raw_trace)
   | Error _ ->
     (* Skip if directory creation fails *)
     ())

let test_create_agent_with_provider_and_trace () =
  Eio_main.run @@ fun env ->
  let provider_cfg = { Provider.provider = Provider.Local { base_url = "http://localhost:11434" }; model_id = "test"; api_key_env = "" } in
  let path = Filename.concat (Filename.get_temp_dir_name ()) "oas-test-trace2" in
  (match Raw_trace.create ~path () with
   | Ok trace ->
     let agent = Agent_sdk.create_agent ~net:env#net ~provider:provider_cfg ~raw_trace:trace () in
     let opts = Agent.options agent in
     Alcotest.(check bool) "provider set" true (Option.is_some opts.provider);
     Alcotest.(check bool) "trace set" true (Option.is_some opts.raw_trace)
   | Error _ ->
     ())

let test_create_agent_with_cache () =
  Eio_main.run @@ fun env ->
  let agent = Agent_sdk.create_agent ~net:env#net ~cache_system_prompt:true () in
  let config = (Agent.state agent).config in
  Alcotest.(check bool) "cache enabled" true config.cache_system_prompt

let test_tool_retry_policy_clamps_negative_max_retries () =
  let policy = {
    Tool_retry_policy.default_internal with
    max_retries = -3;
  } in
  let failures = [
    {
      Tool_retry_policy.tool_name = "tool";
      detail = "bad output";
      kind = Tool_retry_policy.Recoverable_tool_error;
    };
  ] in
  match Tool_retry_policy.decide ~policy ~prior_retries:0 failures with
  | Tool_retry_policy.Exhausted { attempts; limit; summary } ->
      Alcotest.(check int) "attempts" 0 attempts;
      Alcotest.(check int) "clamped limit" 0 limit;
      Alcotest.(check bool) "summary mentions tool" true
        (String.length summary > 0)
  | _ -> Alcotest.fail "expected immediate exhaustion with clamped limit"

let test_tool_retry_policy_feedback_uses_retry_counts () =
  let text =
    Tool_retry_policy.retry_feedback_text ~retry_count:1 ~max_retries:2
      ~summary:"- tool: bad output"
  in
  Alcotest.(check bool) "uses retry label" true
    (Util.string_contains ~needle:"retry 1/2" text)

let test_tool_retry_policy_feedback_preserves_positive_limit () =
  let text =
    Tool_retry_policy.retry_feedback_text ~retry_count:5 ~max_retries:3
      ~summary:"- tool: bad output"
  in
  Alcotest.(check bool) "shows actual positive limit" true
    (Util.string_contains ~needle:"retry 5/3" text)

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
    "create_agent", [
      test_case "defaults" `Quick test_create_agent_defaults;
      test_case "with name/model/prompt" `Quick test_create_agent_with_name_model;
      test_case "with provider" `Quick test_create_agent_with_provider;
      test_case "with raw_trace" `Quick test_create_agent_with_raw_trace;
      test_case "with provider+trace" `Quick test_create_agent_with_provider_and_trace;
      test_case "with cache" `Quick test_create_agent_with_cache;
    ];
    "tool_retry_policy", [
      test_case "clamps negative max_retries" `Quick
        test_tool_retry_policy_clamps_negative_max_retries;
      test_case "feedback uses retry counts" `Quick
        test_tool_retry_policy_feedback_uses_retry_counts;
      test_case "feedback preserves positive limit" `Quick
        test_tool_retry_policy_feedback_preserves_positive_limit;
    ];
  ]
