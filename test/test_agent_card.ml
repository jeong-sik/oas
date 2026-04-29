open Agent_sdk

let jsonrpc_interface url : Agent_card.supported_interface =
  { url; protocol_binding = "JSONRPC"; protocol_version = "1.0"; tenant = None }
;;

let base_info : Agent_card.agent_info =
  { agent_name = "test-agent"
  ; agent_description = Some "A test agent"
  ; version = Agent_sdk.Sdk_version.version
  ; config =
      { Types.default_config with name = "test-agent"; enable_thinking = Some true }
  ; tool_schemas =
      [ { Types.name = "get_weather"
        ; description = "Get weather"
        ; parameters =
            [ { name = "city"
              ; description = "City"
              ; param_type = String
              ; required = true
              }
            ]
        }
      ]
  ; provider = None
  ; cascade = None
  ; mcp_clients_count = 0
  ; has_elicitation = false
  ; skill_registry = None
  }
;;

let test_of_info_basic () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check string) "name" "test-agent" card.name;
  Alcotest.(check (option string)) "description" (Some "A test agent") card.description;
  Alcotest.(check string) "protocol version" "1.0" card.protocol_version;
  Alcotest.(check string) "version" Agent_sdk.Sdk_version.version card.version
;;

let test_capabilities_tools () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "has Tools" true (Agent_card.has_capability card Agent_card.Tools)
;;

let test_capabilities_streaming () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool)
    "has Streaming"
    true
    (Agent_card.has_capability card Agent_card.Streaming)
;;

let test_capabilities_thinking () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool)
    "has Thinking"
    true
    (Agent_card.has_capability card Agent_card.Thinking)
;;

let test_capabilities_no_thinking () =
  let info =
    { base_info with config = { base_info.config with enable_thinking = None } }
  in
  let card = Agent_card.of_info info in
  Alcotest.(check bool)
    "no Thinking"
    false
    (Agent_card.has_capability card Agent_card.Thinking)
;;

let test_capabilities_mcp () =
  let info = { base_info with mcp_clients_count = 2 } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool) "has MCP" true (Agent_card.has_capability card Agent_card.MCP)
;;

let test_capabilities_elicitation () =
  let info = { base_info with has_elicitation = true } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool)
    "has Elicitation"
    true
    (Agent_card.has_capability card Agent_card.Elicitation)
;;

let test_capabilities_no_elicitation () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool)
    "no Elicitation"
    false
    (Agent_card.has_capability card Agent_card.Elicitation)
;;

let test_can_handle_tool () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool)
    "can handle get_weather"
    true
    (Agent_card.can_handle_tool card "get_weather");
  Alcotest.(check bool)
    "cannot handle unknown"
    false
    (Agent_card.can_handle_tool card "unknown_tool")
;;

let test_no_tools () =
  let info = { base_info with tool_schemas = [] } in
  let card = Agent_card.of_info info in
  Alcotest.(check bool)
    "no Tools cap"
    false
    (Agent_card.has_capability card Agent_card.Tools)
;;

let test_providers_default () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check (list string))
    "default provider"
    [ "anthropic" ]
    card.supported_providers
;;

let test_providers_custom () =
  let provider =
    Provider.
      { provider =
          OpenAICompat
            { base_url = "http://localhost:8085"
            ; auth_header = None
            ; path = "/v1/chat/completions"
            ; static_token = None
            }
      ; model_id = "qwen3.5"
      ; api_key_env = ""
      }
  in
  let info = { base_info with provider = Some provider } in
  let card = Agent_card.of_info info in
  Alcotest.(check (list string))
    "openai-compat provider"
    [ "openai-compat" ]
    card.supported_providers
;;

let test_skills_from_registry () =
  let reg = Skill_registry.create () in
  let skill = Skill.of_markdown "---\nname: greet\n---\nHello" in
  Skill_registry.register reg skill;
  let info = { base_info with skill_registry = Some reg } in
  let card = Agent_card.of_info info in
  Alcotest.(check int) "1 skill" 1 (List.length card.skills);
  Alcotest.(check bool) "has greet" true (Agent_card.has_skill card "greet")
;;

let test_json_roundtrip () =
  let card = Agent_card.of_info base_info in
  let json = Agent_card.to_json card in
  match Agent_card.of_json json with
  | Ok card2 ->
    Alcotest.(check string) "name preserved" card.name card2.name;
    Alcotest.(check string)
      "protocol preserved"
      card.protocol_version
      card2.protocol_version;
    Alcotest.(check string) "version preserved" card.version card2.version;
    Alcotest.(check int)
      "caps count"
      (List.length card.capabilities)
      (List.length card2.capabilities)
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_to_json_structure () =
  let card = Agent_card.of_info base_info in
  let json = Agent_card.to_json card in
  let open Yojson.Safe.Util in
  let name = json |> member "name" |> to_string in
  let protocol_version = json |> member "protocolVersion" |> to_string in
  let caps = json |> member "capabilities" |> to_list in
  let tools = json |> member "tools" |> to_list in
  let interfaces = json |> member "supportedInterfaces" |> to_list in
  Alcotest.(check string) "json name" "test-agent" name;
  Alcotest.(check string) "json protocol version" "1.0" protocol_version;
  Alcotest.(check bool) "has caps" true (List.length caps > 0);
  Alcotest.(check int) "1 tool" 1 (List.length tools);
  Alcotest.(check int) "no interfaces by default" 0 (List.length interfaces)
;;

let test_to_json_synthesizes_supported_interface () =
  let card : Agent_card.agent_card =
    { name = "synth-agent"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = Some "http://agent.local/a2a"
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = []
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let json = Agent_card.to_json card in
  let open Yojson.Safe.Util in
  let interfaces = json |> member "supportedInterfaces" |> to_list in
  Alcotest.(check int) "synthesized interface count" 1 (List.length interfaces);
  let iface = List.hd interfaces in
  Alcotest.(check string)
    "url"
    "http://agent.local/a2a"
    (iface |> member "url" |> to_string);
  Alcotest.(check string)
    "binding"
    "JSONRPC"
    (iface |> member "protocolBinding" |> to_string);
  Alcotest.(check string)
    "protocol version"
    "1.0"
    (iface |> member "protocolVersion" |> to_string)
;;

let test_of_json_invalid () =
  match Agent_card.of_json (`String "bad") with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "should fail on invalid json"
;;

(* ── capability_to/of_string roundtrip ──────────────────── *)

let test_capability_roundtrip () =
  let caps =
    [ Agent_card.Tools
    ; Streaming
    ; Thinking
    ; StructuredOutput
    ; Handoff
    ; Checkpoint
    ; MCP
    ; Elicitation
    ; Custom_cap "my_cap"
    ]
  in
  List.iter
    (fun cap ->
       let s = Agent_card.capability_to_string cap in
       let decoded = Agent_card.capability_of_string s in
       Alcotest.(check bool) ("roundtrip " ^ s) true (cap = decoded))
    caps
;;

(* ── to_json/of_json with authentication ───────────────── *)

let test_json_with_authentication () =
  let card : Agent_card.agent_card =
    { name = "auth-agent"
    ; description = Some "with auth"
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = Some "http://agent.local:8080"
    ; authentication =
        Some { schemes = [ "bearer"; "api-key" ]; credentials = Some "secret" }
    ; supported_interfaces = [ jsonrpc_interface "http://agent.local:8080" ]
    ; capabilities = [ Tools; Streaming ]
    ; tools = []
    ; skills = []
    ; supported_providers = [ "anthropic" ]
    ; metadata = [ "env", `String "test" ]
    }
  in
  let json = Agent_card.to_json card in
  match Agent_card.of_json json with
  | Ok card2 ->
    Alcotest.(check string) "name" "auth-agent" card2.name;
    Alcotest.(check string) "protocol version" "1.0" card2.protocol_version;
    Alcotest.(check (option string)) "url" (Some "http://agent.local:8080") card2.url;
    (match card2.authentication with
     | Some auth ->
       Alcotest.(check (list string)) "schemes" [ "bearer"; "api-key" ] auth.schemes;
       Alcotest.(check (option string)) "creds" (Some "secret") auth.credentials
     | None -> Alcotest.fail "expected auth");
    Alcotest.(check int) "interface count" 1 (List.length card2.supported_interfaces);
    Alcotest.(check int) "metadata" 1 (List.length card2.metadata)
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_json_no_auth_no_metadata () =
  let card : Agent_card.agent_card =
    { name = "simple"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "0.1"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = []
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let json = Agent_card.to_json card in
  match Agent_card.of_json json with
  | Ok card2 ->
    Alcotest.(check (option string)) "no desc" None card2.description;
    Alcotest.(check (option string)) "no url" None card2.url;
    Alcotest.(check bool) "no auth" true (Option.is_none card2.authentication);
    Alcotest.(check (list string)) "no metadata" [] (List.map fst card2.metadata)
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_json_auth_no_credentials () =
  let card : Agent_card.agent_card =
    { name = "auth-noc"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = Some { schemes = [ "oauth" ]; credentials = None }
    ; supported_interfaces = []
    ; capabilities = []
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let json = Agent_card.to_json card in
  match Agent_card.of_json json with
  | Ok card2 ->
    (match card2.authentication with
     | Some auth -> Alcotest.(check (option string)) "no creds" None auth.credentials
     | None -> Alcotest.fail "expected auth")
  | Error e -> Alcotest.fail (Error.to_string e)
;;

(* ── provider_name ──────────────────────────────────────── *)

let test_provider_name_local () =
  let cfg : Provider.config =
    { provider = Local { base_url = "http://localhost:8085" }
    ; model_id = "local"
    ; api_key_env = "DUMMY"
    }
  in
  Alcotest.(check string) "local" "local" (Agent_card.provider_name cfg)
;;

let test_provider_name_custom () =
  let cfg : Provider.config =
    { provider = Custom_registered { name = "myengine" }
    ; model_id = "x"
    ; api_key_env = "DUMMY"
    }
  in
  Alcotest.(check string) "custom" "myengine" (Agent_card.provider_name cfg)
;;

(* ── has_capability / has_skill ─────────────────────────── *)

let test_has_capability_false () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "no checkpoint" false (Agent_card.has_capability card Checkpoint)
;;

let test_has_skill_false () =
  let card = Agent_card.of_info base_info in
  Alcotest.(check bool) "no skill" false (Agent_card.has_skill card "nonexistent")
;;

(* ── to_json with skills ─────────────────────────────────── *)

let test_to_json_with_skills () =
  let card : Agent_card.agent_card =
    { name = "skill-agent"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = []
    ; tools = []
    ; skills =
        [ Skill.of_markdown "---\nname: greet\ndescription: Say hi\n---\nHello"
        ; Skill.of_markdown "---\nname: deploy\n---\nDeploy"
        ]
    ; supported_providers = []
    ; metadata = []
    }
  in
  let json = Agent_card.to_json card in
  let open Yojson.Safe.Util in
  let skills = json |> member "skills" |> to_list in
  Alcotest.(check int) "2 skills" 2 (List.length skills);
  let first = List.hd skills in
  Alcotest.(check string) "skill name" "greet" (first |> member "name" |> to_string)
;;

let test_legacy_json_backfills_interfaces () =
  let legacy_json =
    `Assoc
      [ "name", `String "legacy-agent"
      ; "description", `String "legacy"
      ; "version", `String "0.9"
      ; "url", `String "http://legacy.local/a2a"
      ; "capabilities", `List []
      ; "tools", `List []
      ; "skills", `List []
      ; "supported_providers", `List []
      ]
  in
  match Agent_card.of_json legacy_json with
  | Ok card ->
    Alcotest.(check string) "default protocol version" "0.1" card.protocol_version;
    Alcotest.(check int) "synthesized interface" 1 (List.length card.supported_interfaces)
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let test_interface_protocol_version_inherits_card_version () =
  let json =
    `Assoc
      [ "name", `String "v1-agent"
      ; "version", `String "1.2.3"
      ; "protocolVersion", `String "1.0"
      ; ( "supportedInterfaces"
        , `List
            [ `Assoc
                [ "url", `String "http://agent.local/a2a"
                ; "protocolBinding", `String "JSONRPC"
                ]
            ] )
      ; "capabilities", `List []
      ; "tools", `List []
      ; "skills", `List []
      ; "supported_providers", `List []
      ]
  in
  match Agent_card.of_json json with
  | Ok card ->
    Alcotest.(check string) "card protocol version" "1.0" card.protocol_version;
    let iface = List.hd card.supported_interfaces in
    Alcotest.(check string) "interface protocol version" "1.0" iface.protocol_version
  | Error e -> Alcotest.fail (Error.to_string e)
;;

let () =
  let open Alcotest in
  run
    "Agent_card"
    [ ( "of_info"
      , [ test_case "basic" `Quick test_of_info_basic
        ; test_case "tools cap" `Quick test_capabilities_tools
        ; test_case "streaming cap" `Quick test_capabilities_streaming
        ; test_case "thinking cap" `Quick test_capabilities_thinking
        ; test_case "no thinking" `Quick test_capabilities_no_thinking
        ; test_case "mcp cap" `Quick test_capabilities_mcp
        ; test_case "elicitation cap" `Quick test_capabilities_elicitation
        ; test_case "no elicitation" `Quick test_capabilities_no_elicitation
        ; test_case "no tools" `Quick test_no_tools
        ] )
    ; ( "queries"
      , [ test_case "can_handle_tool" `Quick test_can_handle_tool
        ; test_case "providers default" `Quick test_providers_default
        ; test_case "providers custom" `Quick test_providers_custom
        ; test_case "skills from registry" `Quick test_skills_from_registry
        ; test_case "has_capability false" `Quick test_has_capability_false
        ; test_case "has_skill false" `Quick test_has_skill_false
        ] )
    ; "capabilities", [ test_case "roundtrip all" `Quick test_capability_roundtrip ]
    ; ( "json"
      , [ test_case "roundtrip" `Quick test_json_roundtrip
        ; test_case "to_json structure" `Quick test_to_json_structure
        ; test_case
            "to_json synthesizes supported interface"
            `Quick
            test_to_json_synthesizes_supported_interface
        ; test_case "of_json invalid" `Quick test_of_json_invalid
        ; test_case "with auth" `Quick test_json_with_authentication
        ; test_case "no auth no meta" `Quick test_json_no_auth_no_metadata
        ; test_case "auth no creds" `Quick test_json_auth_no_credentials
        ; test_case "with skills" `Quick test_to_json_with_skills
        ; test_case
            "legacy json backfills interfaces"
            `Quick
            test_legacy_json_backfills_interfaces
        ; test_case
            "interface protocol version inherits card version"
            `Quick
            test_interface_protocol_version_inherits_card_version
        ] )
    ; ( "provider_name"
      , [ test_case "local" `Quick test_provider_name_local
        ; test_case "custom" `Quick test_provider_name_custom
        ] )
    ]
;;
