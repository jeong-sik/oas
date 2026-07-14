(** Tests for Agent_config — JSON config file loader. *)

open Alcotest
open Agent_sdk

(* ── of_json ────────────────────────────────────────────── *)

let test_minimal_config () =
  let json = `Assoc [ "name", `String "test"; "model", `String "exact-model" ] in
  match Agent_config.of_json json with
  | Ok cfg ->
    check string "name" "test" cfg.name;
    check string "model" "exact-model" cfg.model;
    check (option string) "no prompt" None cfg.system_prompt;
    check (option int) "no max_tokens" None cfg.max_tokens;
    check int "no mcp" 0 (List.length cfg.mcp_servers)
  | Error e -> fail (Error.to_string e)
;;

let test_full_config () =
  let json =
    `Assoc
      [ "name", `String "full-agent"
      ; "model", `String "claude-opus-4-6"
      ; "system_prompt", `String "You are helpful."
      ; "max_tokens", `Int 8192
      ; ( "mcp_servers"
        , `List
            [ `Assoc
                [ "command", `String "npx"
                ; "args", `List [ `String "-y"; `String "server" ]
                ; "name", `String "my-server"
                ]
            ] )
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    check string "name" "full-agent" cfg.name;
    check string "model" "claude-opus-4-6" cfg.model;
    check (option string) "prompt" (Some "You are helpful.") cfg.system_prompt;
    check (option int) "max_tokens" (Some 8192) cfg.max_tokens;
    check int "mcp" 1 (List.length cfg.mcp_servers);
    (match List.hd cfg.mcp_servers with
     | Agent_config.Stdio_mcp { command; name; _ } ->
       check string "mcp command" "npx" command;
       check string "mcp name" "my-server" name
     | Agent_config.Http_mcp _ -> fail "expected Stdio_mcp")
  | Error e -> fail (Error.to_string e)
;;

let test_missing_model_rejected () =
  let json = `Assoc [] in
  match Agent_config.of_json json with
  | Error (Error.Config (InvalidConfig { field = "model"; _ })) -> ()
  | Error error -> fail (Error.to_string error)
  | Ok _ -> fail "missing model must be rejected"
;;

let test_reasoning_effort_max () =
  let json =
    `Assoc [ "model", `String "exact-model"; "reasoning_effort", `String "max" ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    check
      (option string)
      "typed reasoning effort"
      (Some "max")
      (Option.map Llm_provider.Reasoning_effort.to_string cfg.reasoning_effort)
  | Error error -> fail (Error.to_string error)
;;

let expect_invalid_config_field field json =
  match Agent_config.of_json json with
  | Error (Error.Config (InvalidConfig { field = actual; _ })) ->
    check string "field" field actual
  | Error e -> fail (Printf.sprintf "unexpected error: %s" (Error.to_string e))
  | Ok _ -> fail "expected invalid config"
;;

let test_rejects_non_object_config () = expect_invalid_config_field "<root>" (`List [])

let test_rejects_unknown_field () =
  expect_invalid_config_field
    "removed_lifecycle_knob"
    (`Assoc [ "removed_lifecycle_knob", `Int 1 ])
;;

let test_rejects_removed_tools_field () =
  expect_invalid_config_field
    "tools"
    (`Assoc
        [ "tools", `List [ `Assoc [ "name", `String "calc"; "parameters", `Assoc [] ] ] ])
;;

let test_rejects_non_list_mcp_servers () =
  expect_invalid_config_field "mcp_servers" (`Assoc [ "mcp_servers", `String "node" ])
;;

let test_rejects_non_string_mcp_args () =
  expect_invalid_config_field
    "args"
    (`Assoc
        [ ( "mcp_servers"
          , `List [ `Assoc [ "command", `String "node"; "args", `List [ `Int 1 ] ] ] )
        ])
;;

let test_rejects_non_string_http_headers () =
  expect_invalid_config_field
    "/headers/Authorization"
    (`Assoc
        [ ( "mcp_servers"
          , `List
              [ `Assoc
                  [ "url", `String "http://example.com/mcp"
                  ; "headers", `Assoc [ "Authorization", `Int 1 ]
                  ]
              ] )
        ])
;;

let expect_invalid_mcp_field expected json =
  match Agent_config.parse_mcp json with
  | Error (Error.Config (InvalidConfig { field; _ })) ->
    check string "invalid MCP field" expected field
  | Error error -> fail (Error.to_string error)
  | Ok _ -> fail "expected invalid MCP configuration"
;;

let test_rejects_wrong_typed_mcp_transport_and_name_fields () =
  let cases =
    [ "url", `Assoc [ "url", `Int 1 ]
    ; "name", `Assoc [ "url", `String "http://example.test/mcp"; "name", `Int 1 ]
    ; "name", `Assoc [ "command", `String "node"; "name", `Bool true ]
    ]
  in
  List.iter (fun (field, json) -> expect_invalid_mcp_field field json) cases
;;

let test_rejects_ambiguous_or_missing_mcp_transport () =
  expect_invalid_mcp_field
    "mcp_server"
    (`Assoc [ "url", `String "http://example.test/mcp"; "command", `String "node" ]);
  expect_invalid_mcp_field "mcp_server" (`Assoc [ "name", `String "missing-transport" ])
;;

let test_rejects_fields_from_the_other_mcp_variant () =
  expect_invalid_mcp_field
    "args"
    (`Assoc [ "url", `String "http://example.test/mcp"; "args", `List [] ]);
  expect_invalid_mcp_field
    "headers"
    (`Assoc [ "command", `String "node"; "headers", `Assoc [] ])
;;

let test_rejects_unknown_reasoning_effort () =
  expect_invalid_config_field
    "reasoning_effort"
    (`Assoc [ "reasoning_effort", `String "urgent" ])
;;

let test_optional_fields_accept_null_as_absent () =
  let json =
    `Assoc
      [ "model", `String "exact-model"
      ; "name", `Null
      ; "system_prompt", `Null
      ; "max_tokens", `Null
      ; "enable_thinking", `Null
      ; "preserve_thinking", `Null
      ; "thinking_budget", `Null
      ; "provider", `Null
      ]
  in
  match Agent_config.of_json json with
  | Error error -> fail (Error.to_string error)
  | Ok cfg ->
    check string "null name uses documented default" "agent" cfg.name;
    check (option string) "null system_prompt" None cfg.system_prompt;
    check (option int) "null max_tokens" None cfg.max_tokens;
    check (option bool) "null enable_thinking" None cfg.enable_thinking;
    check (option bool) "null preserve_thinking" None cfg.preserve_thinking;
    check (option int) "null thinking_budget" None cfg.thinking_budget;
    check (option string) "null provider" None cfg.provider
;;

let test_optional_fields_reject_present_wrong_types () =
  let cases =
    [ "name", `Bool true
    ; "system_prompt", `Int 1
    ; "max_tokens", `String "4096"
    ; "enable_thinking", `Int 1
    ; "preserve_thinking", `String "true"
    ; "thinking_budget", `Bool false
    ; "provider", `List []
    ]
  in
  List.iter
    (fun (field, wrong_value) ->
       expect_invalid_config_field
         field
         (`Assoc [ "model", `String "exact-model"; field, wrong_value ]))
    cases
;;

(* ── load ───────────────────────────────────────────────── *)

let test_load_nonexistent () =
  match Agent_config.load "/tmp/nonexistent_oas_config_12345.json" with
  | Error _ -> () (* expected *)
  | Ok _ -> fail "should fail for nonexistent file"
;;

let test_load_invalid_json () =
  let path = "/tmp/oas_test_invalid.json" in
  Out_channel.with_open_text path (fun oc -> output_string oc "not json {{{");
  match Agent_config.load path with
  | Error _ ->
    (try Sys.remove path with
     | _ -> ())
  | Ok _ ->
    (try Sys.remove path with
     | _ -> ());
    fail "should fail for invalid JSON"
;;

let test_load_valid () =
  let path = "/tmp/oas_test_valid.json" in
  Out_channel.with_open_text path (fun oc ->
    output_string oc {|{"name":"test-agent","model":"claude-sonnet-4-6"}|});
  match Agent_config.load path with
  | Ok cfg ->
    (try Sys.remove path with
     | _ -> ());
    check string "name" "test-agent" cfg.name
  | Error e ->
    (try Sys.remove path with
     | _ -> ());
    fail (Error.to_string e)
;;

(* ── to_builder ─────────────────────────────────────────── *)

let config_with_mcp_servers mcp_servers : Agent_config.agent_file_config =
  { name = "mcp-builder-test"
  ; model = "exact-model"
  ; system_prompt = None
  ; max_tokens = None
  ; mcp_servers
  ; enable_thinking = None
  ; preserve_thinking = None
  ; thinking_budget = None
  ; reasoning_effort = None
  ; provider = None
  }
;;

let configured_stdio_mcp =
  Agent_config.Stdio_mcp
    { command = "not-started-without-runtime-resources"
    ; args = []
    ; name = "configured-server"
    ; env = []
    }
;;

let unreachable_http_mcp =
  Agent_config.Http_mcp
    { url = "http://127.0.0.1:1/mcp"; headers = []; name = "unreachable-http-server" }
;;

let expect_mcp_runtime_config_error = function
  | Error (Error.Config (InvalidConfig { field; _ })) ->
    check string "configuration field" "mcp_servers" field
  | Error error -> fail (Error.to_string error)
  | Ok _ -> fail "configured MCP servers must not be dropped silently"
;;

let test_to_builder_rejects_mcp_without_switch_or_manager () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let cfg = config_with_mcp_servers [ configured_stdio_mcp ] in
  Agent_config.to_builder ~net cfg |> expect_mcp_runtime_config_error
;;

let test_to_builder_rejects_mcp_without_manager () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let cfg = config_with_mcp_servers [ configured_stdio_mcp ] in
  Agent_config.to_builder ~sw ~net cfg |> expect_mcp_runtime_config_error
;;

let test_to_builder_rejects_mcp_without_switch () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let mgr = Eio.Stdenv.process_mgr env in
  let cfg = config_with_mcp_servers [ configured_stdio_mcp ] in
  Agent_config.to_builder ~mgr ~net cfg |> expect_mcp_runtime_config_error
;;

let test_http_mcp_does_not_require_process_manager () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  match Agent_config.connect_mcp_server ~sw ~net unreachable_http_mcp with
  | Error (Error.Mcp _) -> ()
  | Error error -> fail (Error.to_string error)
  | Ok managed ->
    Mcp.close_managed managed;
    fail "unreachable HTTP MCP server unexpectedly connected"
;;

let test_to_builder_connects_mcp_with_runtime_resources () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let mgr = Eio.Stdenv.process_mgr env in
  Eio.Switch.run
  @@ fun sw ->
  let server =
    {|
import json, sys

for line in sys.stdin:
    request = json.loads(line)
    request_id = request.get("id")
    if request_id is None:
        continue
    if request.get("method") == "initialize":
        result = {
            "protocolVersion": "2025-11-25",
            "capabilities": {},
            "serverInfo": {"name": "agent-config-test", "version": "1.0"},
        }
    elif request.get("method") == "tools/list":
        result = {"tools": []}
    else:
        result = {}
    response = {"jsonrpc": "2.0", "id": request_id, "result": result}
    sys.stdout.write(json.dumps(response) + "\n")
    sys.stdout.flush()
|}
  in
  let cfg =
    config_with_mcp_servers
      [ Agent_config.Stdio_mcp
          { command = "python3"
          ; args = [ "-u"; "-c"; server ]
          ; name = "working-server"
          ; env = []
          }
      ]
  in
  match Agent_config.to_builder ~sw ~mgr ~net cfg with
  | Error error -> fail (Error.to_string error)
  | Ok builder ->
    (match Builder.build_safe builder with
     | Error error -> fail (Error.to_string error)
     | Ok agent ->
       check int "connected MCP clients" 1 (List.length (Agent.options agent).mcp_clients);
       Agent.close agent)
;;

let test_to_builder () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let cfg : Agent_config.agent_file_config =
    { name = "builder-test"
    ; model = "claude-sonnet-4-6"
    ; system_prompt = Some "test prompt"
    ; max_tokens = Some 2048
    ; mcp_servers = []
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; reasoning_effort = Some Llm_provider.Reasoning_effort.Max
    ; provider = None
    }
  in
  let builder = Result.get_ok (Agent_config.to_builder ~net cfg) in
  match Builder.build_safe builder with
  | Ok agent ->
    let card = Agent.card agent in
    check string "agent name" "builder-test" card.name;
    check
      (option string)
      "reasoning effort"
      (Some "max")
      (Option.map
         Llm_provider.Reasoning_effort.to_string
         (Agent.state agent).config.reasoning_effort)
  | Error e -> fail (Error.to_string e)
;;

let test_to_builder_minimal () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let cfg : Agent_config.agent_file_config =
    { name = "minimal"
    ; model = "claude-sonnet-4-6"
    ; system_prompt = None
    ; max_tokens = None
    ; mcp_servers = []
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; reasoning_effort = None
    ; provider = None
    }
  in
  let builder = Result.get_ok (Agent_config.to_builder ~net cfg) in
  match Builder.build_safe builder with
  | Ok _ -> ()
  | Error e -> fail (Error.to_string e)
;;

(* ── to_builder: model string mapping ─────────────────── *)

let test_to_builder_all_models () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let models =
    [ "claude-opus-4-6"
    ; "claude-sonnet-4-6"
    ; "claude-opus-4-5"
    ; "claude-sonnet-4"
    ; "claude-haiku-4-5"
    ; "claude-3-7-sonnet"
    ; "custom-model"
    ]
  in
  List.iter
    (fun model_str ->
       let cfg : Agent_config.agent_file_config =
         { name = "m-test"
         ; model = model_str
         ; system_prompt = None
         ; max_tokens = None
         ; enable_thinking = None
         ; preserve_thinking = None
         ; thinking_budget = None
         ; reasoning_effort = None
         ; provider = None
         ; mcp_servers = []
         }
       in
       let builder = Result.get_ok (Agent_config.to_builder ~net cfg) in
       match Builder.build_safe builder with
       | Ok _ -> ()
       | Error e -> fail (Printf.sprintf "model %s: %s" model_str (Error.to_string e)))
    models
;;

(* ── parse_mcp: edge cases ───────────────────────────── *)

let test_mcp_defaults () =
  let json =
    `Assoc
      [ "name", `String "test"
      ; "model", `String "exact-model"
      ; "mcp_servers", `List [ `Assoc [ "command", `String "node" ] ]
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    check int "1 mcp" 1 (List.length cfg.mcp_servers);
    (match List.hd cfg.mcp_servers with
     | Agent_config.Stdio_mcp { name; args; env; _ } ->
       check string "name defaults to command" "node" name;
       check (list string) "empty args" [] args;
       check (list string) "empty env" [] env
     | Agent_config.Http_mcp _ -> fail "expected Stdio_mcp")
  | Error e -> fail (Error.to_string e)
;;

let test_mcp_with_env () =
  let json =
    `Assoc
      [ "name", `String "test"
      ; "model", `String "exact-model"
      ; ( "mcp_servers"
        , `List
            [ `Assoc
                [ "command", `String "node"
                ; "args", `List [ `String "server.js" ]
                ; "name", `String "my-server"
                ; "env", `List [ `String "NODE_ENV=production" ]
                ]
            ] )
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    (match List.hd cfg.mcp_servers with
     | Agent_config.Stdio_mcp { env; args; _ } ->
       check (list string) "env" [ "NODE_ENV=production" ] env;
       check (list string) "args" [ "server.js" ] args
     | Agent_config.Http_mcp _ -> fail "expected Stdio_mcp")
  | Error e -> fail (Error.to_string e)
;;

(* ── HTTP MCP parsing ──────────────────────────────────── *)

let test_http_mcp_config () =
  let json =
    `Assoc
      [ "name", `String "test"
      ; "model", `String "exact-model"
      ; ( "mcp_servers"
        , `List
            [ `Assoc
                [ "url", `String "http://localhost:8935/mcp"
                ; "name", `String "example_mcp"
                ; "headers", `Assoc [ "Authorization", `String "Bearer tok" ]
                ]
            ] )
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    check int "1 mcp" 1 (List.length cfg.mcp_servers);
    (match List.hd cfg.mcp_servers with
     | Agent_config.Http_mcp { url; name; headers } ->
       check string "url" "http://localhost:8935/mcp" url;
       check string "name" "example_mcp" name;
       check int "1 header" 1 (List.length headers);
       let hk, hv = List.hd headers in
       check string "header key" "Authorization" hk;
       check string "header val" "Bearer tok" hv
     | Agent_config.Stdio_mcp _ -> fail "expected Http_mcp")
  | Error e -> fail (Error.to_string e)
;;

let test_http_mcp_defaults () =
  let json =
    `Assoc
      [ "name", `String "test"
      ; "model", `String "exact-model"
      ; "mcp_servers", `List [ `Assoc [ "url", `String "http://example.com/mcp" ] ]
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    (match List.hd cfg.mcp_servers with
     | Agent_config.Http_mcp { url; name; headers } ->
       check string "url" "http://example.com/mcp" url;
       check string "name defaults to url" "http://example.com/mcp" name;
       check (list (pair string string)) "no headers" [] headers
     | Agent_config.Stdio_mcp _ -> fail "expected Http_mcp")
  | Error e -> fail (Error.to_string e)
;;

let test_mixed_mcp_config () =
  let json =
    `Assoc
      [ "name", `String "test"
      ; "model", `String "exact-model"
      ; ( "mcp_servers"
        , `List
            [ `Assoc
                [ "command", `String "npx"
                ; "args", `List [ `String "-y"; `String "server" ]
                ; "name", `String "stdio-server"
                ]
            ; `Assoc
                [ "url", `String "http://localhost:8080/mcp"
                ; "name", `String "http-server"
                ]
            ] )
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    check int "2 mcp servers" 2 (List.length cfg.mcp_servers);
    (match List.nth cfg.mcp_servers 0 with
     | Agent_config.Stdio_mcp { name; _ } ->
       check string "first is stdio" "stdio-server" name
     | _ -> fail "expected Stdio_mcp first");
    (match List.nth cfg.mcp_servers 1 with
     | Agent_config.Http_mcp { name; _ } ->
       check string "second is http" "http-server" name
     | _ -> fail "expected Http_mcp second")
  | Error e -> fail (Error.to_string e)
;;

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run
    "Agent_config"
    [ ( "of_json"
      , [ test_case "minimal" `Quick test_minimal_config
        ; test_case "full" `Quick test_full_config
        ; test_case "missing model rejected" `Quick test_missing_model_rejected
        ; test_case "reasoning effort max" `Quick test_reasoning_effort_max
        ; test_case "mcp defaults" `Quick test_mcp_defaults
        ; test_case "mcp with env" `Quick test_mcp_with_env
        ; test_case "http mcp" `Quick test_http_mcp_config
        ; test_case "http mcp defaults" `Quick test_http_mcp_defaults
        ; test_case "mixed mcp" `Quick test_mixed_mcp_config
        ; test_case "reject non-object config" `Quick test_rejects_non_object_config
        ; test_case "reject unknown field" `Quick test_rejects_unknown_field
        ; test_case "reject removed tools field" `Quick test_rejects_removed_tools_field
        ; test_case "reject non-list mcp_servers" `Quick test_rejects_non_list_mcp_servers
        ; test_case "reject non-string mcp args" `Quick test_rejects_non_string_mcp_args
        ; test_case
            "reject non-string http headers"
            `Quick
            test_rejects_non_string_http_headers
        ; test_case
            "reject wrong-typed MCP transport and name fields"
            `Quick
            test_rejects_wrong_typed_mcp_transport_and_name_fields
        ; test_case
            "reject ambiguous or missing MCP transport"
            `Quick
            test_rejects_ambiguous_or_missing_mcp_transport
        ; test_case
            "reject fields from the other MCP variant"
            `Quick
            test_rejects_fields_from_the_other_mcp_variant
        ; test_case
            "reject unknown reasoning effort"
            `Quick
            test_rejects_unknown_reasoning_effort
        ; test_case
            "optional fields accept null as absent"
            `Quick
            test_optional_fields_accept_null_as_absent
        ; test_case
            "optional fields reject present wrong types"
            `Quick
            test_optional_fields_reject_present_wrong_types
        ] )
    ; ( "load"
      , [ test_case "nonexistent" `Quick test_load_nonexistent
        ; test_case "invalid json" `Quick test_load_invalid_json
        ; test_case "valid" `Quick test_load_valid
        ] )
    ; ( "to_builder"
      , [ test_case "base config" `Quick test_to_builder
        ; test_case "minimal config" `Quick test_to_builder_minimal
        ; test_case
            "configured MCP requires switch and manager"
            `Quick
            test_to_builder_rejects_mcp_without_switch_or_manager
        ; test_case
            "configured MCP requires manager"
            `Quick
            test_to_builder_rejects_mcp_without_manager
        ; test_case
            "configured MCP requires switch"
            `Quick
            test_to_builder_rejects_mcp_without_switch
        ; test_case
            "HTTP MCP does not require process manager"
            `Quick
            test_http_mcp_does_not_require_process_manager
        ; test_case
            "configured MCP connects with runtime resources"
            `Quick
            test_to_builder_connects_mcp_with_runtime_resources
        ; test_case "all models" `Quick test_to_builder_all_models
        ] )
    ]
;;
