open Agent_sdk

let base_info : Agent_card.agent_info = {
  agent_name = "test-agent";
  agent_description = Some "A test agent";
  config = { Types.default_config with
    name = "test-agent";
    enable_thinking = Some true;
  };
  tool_schemas = [{
    Types.name = "get_weather";
    description = "Get weather";
    parameters = [{ name = "city"; description = "City";
                    param_type = String; required = true }];
  }];
  provider = None;
  cascade = None;
  mcp_clients_count = 0;
  has_elicitation = false;
  skill_registry = None;
}

let test_of_info_basic () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check string) "name" "test-agent" card.name;
  Alcotest.(check (option string)) "description"
    (Some "A test agent") card.description;
  Alcotest.(check string) "version" "0.30.0" card.version

let test_capabilities_tools () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "has Tools" true
    (Agent_card.has_capability card Agent_card.Tools)

let test_capabilities_streaming () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "has Streaming" true
    (Agent_card.has_capability card Agent_card.Streaming)

let test_capabilities_thinking () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "has Thinking" true
    (Agent_card.has_capability card Agent_card.Thinking)

let test_capabilities_no_thinking () =
  let info = { base_info with
    config = { base_info.config with enable_thinking = None } } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool) "no Thinking" false
    (Agent_card.has_capability card Agent_card.Thinking)

let test_capabilities_mcp () =
  let info = { base_info with mcp_clients_count = 2 } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool) "has MCP" true
    (Agent_card.has_capability card Agent_card.MCP)

let test_capabilities_elicitation () =
  let info = { base_info with has_elicitation = true } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool) "has Elicitation" true
    (Agent_card.has_capability card Agent_card.Elicitation)

let test_capabilities_no_elicitation () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "no Elicitation" false
    (Agent_card.has_capability card Agent_card.Elicitation)

let test_can_handle_tool () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "can handle get_weather" true
    (Agent_card.can_handle_tool card "get_weather");
  Alcotest.(check bool) "cannot handle unknown" false
    (Agent_card.can_handle_tool card "unknown_tool")

let test_no_tools () =
  let info = { base_info with tool_schemas = [] } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool) "no Tools cap" false
    (Agent_card.has_capability card Agent_card.Tools)

let test_providers_default () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check (list string)) "default provider"
    ["anthropic"] card.supported_providers

let test_providers_custom () =
  let provider = Provider.{
    provider = Ollama { base_url = "http://localhost:11434"; mode = Chat };
    model_id = "qwen3.5";
    api_key_env = "";
  } in
  let info = { base_info with provider = Some provider } in
  let card = Agent_card.of_info info in
  Alcotest.(check (list string)) "ollama provider"
    ["ollama"] card.supported_providers

let test_skills_from_registry () =
  let reg = Skill_registry.create () in
  let skill = Skill.of_markdown "---\nname: greet\n---\nHello" in
  Skill_registry.register reg skill;
  let info = { base_info with skill_registry = Some reg } in
  let card = Agent_card.of_info info in
  Alcotest.(check int) "1 skill" 1 (List.length card.skills);
  Alcotest.(check bool) "has greet" true
    (Agent_card.has_skill card "greet")

let test_json_roundtrip () =
  let card = Agent_card.of_info base_info in
  let json = Agent_card.to_json card in
  match Agent_card.of_json json with
  | Ok card2 ->
    Alcotest.(check string) "name preserved" card.name card2.name;
    Alcotest.(check string) "version preserved" card.version card2.version;
    Alcotest.(check int) "caps count"
      (List.length card.capabilities) (List.length card2.capabilities)
  | Error e -> Alcotest.fail (Error.to_string e)

let test_to_json_structure () =
  let card = Agent_card.of_info base_info in
  let json = Agent_card.to_json card in
  let open Yojson.Safe.Util in
  let name = json |> member "name" |> to_string in
  let caps = json |> member "capabilities" |> to_list in
  let tools = json |> member "tools" |> to_list in
  Alcotest.(check string) "json name" "test-agent" name;
  Alcotest.(check bool) "has caps" true (List.length caps > 0);
  Alcotest.(check int) "1 tool" 1 (List.length tools)

let test_of_json_invalid () =
  match Agent_card.of_json (`String "bad") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on invalid json"

let () =
  let open Alcotest in
  run "Agent_card" [
    "of_info", [
      test_case "basic" `Quick test_of_info_basic;
      test_case "tools cap" `Quick test_capabilities_tools;
      test_case "streaming cap" `Quick test_capabilities_streaming;
      test_case "thinking cap" `Quick test_capabilities_thinking;
      test_case "no thinking" `Quick test_capabilities_no_thinking;
      test_case "mcp cap" `Quick test_capabilities_mcp;
      test_case "elicitation cap" `Quick test_capabilities_elicitation;
      test_case "no elicitation" `Quick test_capabilities_no_elicitation;
      test_case "no tools" `Quick test_no_tools;
    ];
    "queries", [
      test_case "can_handle_tool" `Quick test_can_handle_tool;
      test_case "providers default" `Quick test_providers_default;
      test_case "providers custom" `Quick test_providers_custom;
      test_case "skills from registry" `Quick test_skills_from_registry;
    ];
    "json", [
      test_case "roundtrip" `Quick test_json_roundtrip;
      test_case "to_json structure" `Quick test_to_json_structure;
      test_case "of_json invalid" `Quick test_of_json_invalid;
    ];
  ]
