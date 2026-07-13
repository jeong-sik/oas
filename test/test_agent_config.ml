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
    check int "tools" 0 (List.length cfg.tools);
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

let test_rejects_non_list_tools () =
  expect_invalid_config_field "tools" (`Assoc [ "tools", `Assoc [] ])
;;

let test_rejects_inline_config_tools () =
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

let test_rejects_unknown_reasoning_effort () =
  expect_invalid_config_field
    "reasoning_effort"
    (`Assoc [ "reasoning_effort", `String "urgent" ])
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
    ; tools = []
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

let test_to_builder_rejects_direct_inline_tools () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let cfg : Agent_config.agent_file_config =
    { name = "builder-test"
    ; model = "claude-sonnet-4-6"
    ; system_prompt = None
    ; max_tokens = None
    ; tools = [ { name = "echo"; description = "Echo"; parameters = [] } ]
    ; mcp_servers = []
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; reasoning_effort = None
    ; provider = None
    }
  in
  match Agent_config.to_builder ~net cfg with
  | Error (Error.Config (InvalidConfig { field = "tools"; _ })) -> ()
  | Error error -> fail (Error.to_string error)
  | Ok _ -> fail "inline config tools must be rejected"
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
    ; tools = []
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
         ; tools = []
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

(* ── Inline config tools are rejected ──────────────────── *)

let test_tool_param_types () =
  expect_invalid_config_field
    "tools"
    (`Assoc
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
        ])
;;

let test_tool_no_params () =
  expect_invalid_config_field
    "tools"
    (`Assoc
        [ "name", `String "test"; "tools", `List [ `Assoc [ "name", `String "simple" ] ] ])
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
        ; test_case "reject tool param types" `Quick test_tool_param_types
        ; test_case "reject tool no params" `Quick test_tool_no_params
        ; test_case "http mcp" `Quick test_http_mcp_config
        ; test_case "http mcp defaults" `Quick test_http_mcp_defaults
        ; test_case "mixed mcp" `Quick test_mixed_mcp_config
        ; test_case "reject non-object config" `Quick test_rejects_non_object_config
        ; test_case "reject unknown field" `Quick test_rejects_unknown_field
        ; test_case "reject non-list tools" `Quick test_rejects_non_list_tools
        ; test_case "reject inline config tools" `Quick test_rejects_inline_config_tools
        ; test_case "reject non-list mcp_servers" `Quick test_rejects_non_list_mcp_servers
        ; test_case "reject non-string mcp args" `Quick test_rejects_non_string_mcp_args
        ; test_case
            "reject non-string http headers"
            `Quick
            test_rejects_non_string_http_headers
        ; test_case
            "reject unknown reasoning effort"
            `Quick
            test_rejects_unknown_reasoning_effort
        ] )
    ; ( "load"
      , [ test_case "nonexistent" `Quick test_load_nonexistent
        ; test_case "invalid json" `Quick test_load_invalid_json
        ; test_case "valid" `Quick test_load_valid
        ] )
    ; ( "to_builder"
      , [ test_case "base config" `Quick test_to_builder
        ; test_case
            "reject direct inline tools"
            `Quick
            test_to_builder_rejects_direct_inline_tools
        ; test_case "no tools" `Quick test_to_builder_no_tools
        ; test_case "all models" `Quick test_to_builder_all_models
        ] )
    ]
;;
