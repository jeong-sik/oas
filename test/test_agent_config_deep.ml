(** Contract tests for the explicit Agent_config boundary. *)

open Agent_sdk

let tc name f = Alcotest.test_case name `Quick f

let expect_invalid_config_field expected json =
  match Agent_config.of_json json with
  | Error (Error.Config (InvalidConfig { field; _ })) ->
    Alcotest.(check string) "invalid field" expected field
  | Error error -> Alcotest.fail (Error.to_string error)
  | Ok _ -> Alcotest.failf "expected InvalidConfig for %s" expected
;;

let test_exact_model_required () =
  expect_invalid_config_field "model" (`Assoc []);
  expect_invalid_config_field "model" (`Assoc [ "model", `String "" ]);
  expect_invalid_config_field "model" (`Assoc [ "model", `Int 7 ])
;;

let test_explicit_config () =
  let json =
    `Assoc
      [ "name", `String "configured-agent"
      ; "model", `String "exact-model-id"
      ; "system_prompt", `String "Be precise."
      ; "max_tokens", `Int 2048
      ; "enable_thinking", `Bool true
      ; "preserve_thinking", `Bool true
      ; "thinking_budget", `Int 1024
      ; "reasoning_effort", `String "max"
      ; "provider", `String "claude"
      ]
  in
  match Agent_config.of_json json with
  | Error error -> Alcotest.fail (Error.to_string error)
  | Ok config ->
    Alcotest.(check string) "name" "configured-agent" config.name;
    Alcotest.(check string) "model" "exact-model-id" config.model;
    Alcotest.(check (option string)) "provider" (Some "claude") config.provider;
    Alcotest.(check (option int)) "max tokens" (Some 2048) config.max_tokens;
    Alcotest.(check (option int)) "thinking budget" (Some 1024) config.thinking_budget;
    Alcotest.(check (option string))
      "reasoning effort"
      (Some "max")
      (Option.map Llm_provider.Reasoning_effort.to_string config.reasoning_effort)
;;

let test_legacy_base_url_rejected () =
  expect_invalid_config_field
    "base_url"
    (`Assoc
        [ "model", `String "exact-model-id"
        ; "provider", `String "local"
        ; "base_url", `String "http://127.0.0.1:9000"
        ])
;;

let test_inline_tools_rejected () =
  expect_invalid_config_field
    "tools"
    (`Assoc
        [ "model", `String "exact-model-id"
        ; ( "tools"
          , `List
              [ `Assoc
                  [ "name", `String "echo"
                  ; "description", `String "Echo"
                  ; "parameters", `List []
                  ]
              ] )
        ])
;;

let test_stdio_mcp () =
  let json =
    `Assoc
      [ "command", `String "node"
      ; "args", `List [ `String "server.js" ]
      ; "name", `String "stdio-server"
      ; "env", `List [ `String "MODE=typed" ]
      ]
  in
  match Agent_config.parse_mcp json with
  | Ok (Agent_config.Stdio_mcp { command; args; name; env }) ->
    Alcotest.(check string) "command" "node" command;
    Alcotest.(check (list string)) "args" [ "server.js" ] args;
    Alcotest.(check string) "name" "stdio-server" name;
    Alcotest.(check (list string)) "env" [ "MODE=typed" ] env
  | Ok (Agent_config.Http_mcp _) -> Alcotest.fail "expected stdio MCP"
  | Error error -> Alcotest.fail (Error.to_string error)
;;

let test_http_mcp () =
  let json =
    `Assoc
      [ "url", `String "https://mcp.example.test/rpc"
      ; "name", `String "http-server"
      ; "headers", `Assoc [ "Authorization", `String "Bearer token" ]
      ]
  in
  match Agent_config.parse_mcp json with
  | Ok (Agent_config.Http_mcp { url; headers; name }) ->
    Alcotest.(check string) "url" "https://mcp.example.test/rpc" url;
    Alcotest.(check string) "name" "http-server" name;
    Alcotest.(check (list (pair string string)))
      "headers"
      [ "Authorization", "Bearer token" ]
      headers
  | Ok (Agent_config.Stdio_mcp _) -> Alcotest.fail "expected HTTP MCP"
  | Error error -> Alcotest.fail (Error.to_string error)
;;

let test_exact_registered_provider () =
  match Agent_config.resolve_provider ~model_id:"exact-model-id" "claude" with
  | Ok { Provider.provider = Custom_registered { name }; model_id; _ } ->
    Alcotest.(check string) "provider id" "claude" name;
    Alcotest.(check string) "model id" "exact-model-id" model_id
  | Ok _ -> Alcotest.fail "expected registered Anthropic provider"
  | Error error -> Alcotest.fail (Error.to_string error)
;;

let test_unknown_provider_rejected () =
  List.iter
    (fun provider_id ->
       match Agent_config.resolve_provider ~model_id:"exact-model-id" provider_id with
       | Error (Error.Config (InvalidConfig { field = "provider"; _ })) -> ()
       | Error error -> Alcotest.fail (Error.to_string error)
       | Ok _ -> Alcotest.failf "unknown provider %S was accepted" provider_id)
    [ "local"; "TOTALLY_BOGUS_KEY" ]
;;

let () =
  Alcotest.run
    "Agent_config explicit boundary"
    [ ( "json"
      , [ tc "exact model required" test_exact_model_required
        ; tc "explicit config" test_explicit_config
        ; tc "legacy base_url rejected" test_legacy_base_url_rejected
        ; tc "inline tools rejected" test_inline_tools_rejected
        ] )
    ; "mcp", [ tc "stdio" test_stdio_mcp; tc "http" test_http_mcp ]
    ; ( "provider"
      , [ tc "exact registered provider" test_exact_registered_provider
        ; tc "unknown provider rejected" test_unknown_provider_rejected
        ] )
    ]
;;
