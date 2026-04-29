(** Tests for Agent_config — JSON config file loader. *)

open Alcotest
open Agent_sdk

(* ── of_json ────────────────────────────────────────────── *)

let test_minimal_config () =
  let json = `Assoc [ "name", `String "test" ] in
  match Agent_config.of_json json with
  | Ok cfg ->
    check string "name" "test" cfg.name;
    check string "model" "claude-sonnet-4-6" cfg.model;
    check (option string) "no prompt" None cfg.system_prompt;
    check (option int) "no max_tokens" None cfg.max_tokens;
    check (option int) "no max_turns" None cfg.max_turns;
    check int "no tools" 0 (List.length cfg.tools);
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
      ; "max_turns", `Int 20
      ; ( "tools"
        , `List
            [ `Assoc
                [ "name", `String "get_weather"
                ; "description", `String "Get weather info"
                ; ( "parameters"
                  , `List
                      [ `Assoc
                          [ "name", `String "city"
                          ; "description", `String "City name"
                          ; "type", `String "string"
                          ; "required", `Bool true
                          ]
                      ] )
                ]
            ] )
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
    check (option int) "max_turns" (Some 20) cfg.max_turns;
    check int "tools" 1 (List.length cfg.tools);
    let tool = List.hd cfg.tools in
    check string "tool name" "get_weather" tool.name;
    check int "tool params" 1 (List.length tool.parameters);
    check int "mcp" 1 (List.length cfg.mcp_servers);
    (match List.hd cfg.mcp_servers with
     | Agent_config.Stdio_mcp { command; name; _ } ->
       check string "mcp command" "npx" command;
       check string "mcp name" "my-server" name
     | Agent_config.Http_mcp _ -> fail "expected Stdio_mcp")
  | Error e -> fail (Error.to_string e)
;;

let test_defaults () =
  let json = `Assoc [] in
  match Agent_config.of_json json with
  | Ok cfg ->
    check string "default name" "agent" cfg.name;
    check string "default model" "claude-sonnet-4-6" cfg.model
  | Error e -> fail (Error.to_string e)
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

let test_to_builder () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let cfg : Agent_config.agent_file_config =
    { name = "builder-test"
    ; model = "claude-sonnet-4-6"
    ; system_prompt = Some "test prompt"
    ; max_tokens = Some 2048
    ; max_turns = Some 5
    ; tools = [ { name = "echo"; description = "Echo"; parameters = [] } ]
    ; mcp_servers = []
    ; enable_thinking = None
    ; thinking_budget = None
    ; provider = None
    ; base_url = None
    }
  in
  let builder = Agent_config.to_builder ~net cfg in
  match Builder.build_safe builder with
  | Ok agent ->
    let card = Agent.card agent in
    check string "agent name" "builder-test" card.name
  | Error e -> fail (Error.to_string e)
;;

let test_to_builder_no_tools () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let cfg : Agent_config.agent_file_config =
    { name = "no-tools"
    ; model = "claude-sonnet-4-6"
    ; system_prompt = None
    ; max_tokens = None
    ; max_turns = None
    ; tools = []
    ; mcp_servers = []
    ; enable_thinking = None
    ; thinking_budget = None
    ; provider = None
    ; base_url = None
    }
  in
  let builder = Agent_config.to_builder ~net cfg in
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
         ; max_turns = None
         ; enable_thinking = None
         ; thinking_budget = None
         ; provider = None
         ; base_url = None
         ; tools = []
         ; mcp_servers = []
         }
       in
       let builder = Agent_config.to_builder ~net cfg in
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

(* ── parse_tool: param type mapping ──────────────────── *)

let test_tool_param_types () =
  let json =
    `Assoc
      [ "name", `String "multi"
      ; ( "tools"
        , `List
            [ `Assoc
                [ "name", `String "calc"
                ; "description", `String "Calculator"
                ; ( "parameters"
                  , `List
                      [ `Assoc [ "name", `String "x"; "type", `String "number" ]
                      ; `Assoc
                          [ "name", `String "op"
                          ; "type", `String "string"
                          ; "required", `Bool true
                          ]
                      ; `Assoc [ "name", `String "flag"; "type", `String "boolean" ]
                      ] )
                ]
            ] )
      ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    let tool = List.hd cfg.tools in
    check int "3 params" 3 (List.length tool.parameters)
  | Error e -> fail (Error.to_string e)
;;

let test_tool_no_params () =
  let json =
    `Assoc
      [ "name", `String "test"; "tools", `List [ `Assoc [ "name", `String "simple" ] ] ]
  in
  match Agent_config.of_json json with
  | Ok cfg ->
    let tool = List.hd cfg.tools in
    check string "default desc" "" tool.description;
    check int "no params" 0 (List.length tool.parameters)
  | Error e -> fail (Error.to_string e)
;;

(* ── HTTP MCP parsing ──────────────────────────────────── *)

let test_http_mcp_config () =
  let json =
    `Assoc
      [ "name", `String "test"
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
        ; test_case "defaults" `Quick test_defaults
        ; test_case "mcp defaults" `Quick test_mcp_defaults
        ; test_case "mcp with env" `Quick test_mcp_with_env
        ; test_case "tool param types" `Quick test_tool_param_types
        ; test_case "tool no params" `Quick test_tool_no_params
        ; test_case "http mcp" `Quick test_http_mcp_config
        ; test_case "http mcp defaults" `Quick test_http_mcp_defaults
        ; test_case "mixed mcp" `Quick test_mixed_mcp_config
        ] )
    ; ( "load"
      , [ test_case "nonexistent" `Quick test_load_nonexistent
        ; test_case "invalid json" `Quick test_load_invalid_json
        ; test_case "valid" `Quick test_load_valid
        ] )
    ; ( "to_builder"
      , [ test_case "with tools" `Quick test_to_builder
        ; test_case "no tools" `Quick test_to_builder_no_tools
        ; test_case "all models" `Quick test_to_builder_all_models
        ] )
    ]
;;
