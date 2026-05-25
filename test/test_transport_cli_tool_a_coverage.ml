open Alcotest
open Llm_provider

let write_script name body =
  let path = Filename.temp_file name ".sh" in
  Out_channel.with_open_text path (fun oc -> output_string oc body);
  Unix.chmod path 0o755;
  path
;;

let remove_quietly path =
  try Sys.remove path with
  | Sys_error _ -> ()
;;

let with_script name body f =
  let path = write_script name body in
  match f path with
  | result ->
    remove_quietly path;
    result
  | exception exn ->
    remove_quietly path;
    raise exn
;;

let req_config ?(model_id = "auto") ?min_p ?top_k () =
  Provider_config.make
    ~kind:Provider_config.Cli_tool_a
    ~model_id
    ~base_url:""
    ?min_p
    ?top_k
    ()
;;

let user_message text : Types.message =
  { role = Types.User
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let request
      ?(config = req_config ())
      ?runtime_mcp_policy
      ?(messages = [ user_message "hello" ])
      ()
  =
  { Llm_transport.config; messages; tools = []; runtime_mcp_policy }
;;

let agent_code_success_script =
  {|#!/bin/sh
cat <<'JSONL'
{"type":"thread.started","thread_id":"thread-a"}
{"type":"item.started","item":{"id":"call-1","type":"mcp_tool_call","server":"runtime","tool":"inspect","arguments":{"path":"README.md"}}}
{"type":"item.completed","item":{"id":"msg-1","type":"agent_message","text":"Hello from agent_code"}}
{"type":"item.completed","item":{"id":"call-1","type":"mcp_tool_call","server":"runtime","tool":"inspect","arguments":{"path":"README.md"},"result":{"content":[{"type":"text","text":"ok"}],"isError":true}}}
{"type":"item.completed","item":{"id":"cmd-1","type":"command_execution","command":"pwd","aggregated_output":"/tmp","exit_code":0}}
{"type":"turn.completed","usage":{"input_tokens":3,"cached_input_tokens":1,"output_tokens":4}}
JSONL
|}
;;

let agent_code_recovered_exit_script =
  {|#!/bin/sh
cat <<'JSONL'
{"type":"thread.started","thread_id":"thread-recovered"}
{"type":"item.completed","item":{"id":"msg-1","type":"agent_message","text":"Recovered"}}
{"type":"turn.completed","usage":{"input_tokens":1,"output_tokens":1}}
JSONL
echo 'late session persistence failure' >&2
exit 1
|}
;;

let agent_code_empty_script =
  {|#!/bin/sh
exit 0
|}
;;

let agent_code_capture_script =
  {|#!/bin/sh
printf '%s\n' "$*" > "$ARGS_CAPTURE"
printf '%s\n' "$OAS_CLI_TOOL_A_MCP_WORKER_BEARER" > "$ENV_CAPTURE"
cat <<'JSONL'
{"type":"thread.started","thread_id":"thread-mcp"}
{"type":"item.completed","item":{"id":"msg-1","type":"agent_message","text":"MCP configured"}}
{"type":"turn.completed","usage":{"input_tokens":1,"output_tokens":1}}
JSONL
|}
;;

let agent_code_stdin_capture_script =
  {|#!/bin/sh
cat > "$CAPTURE_PATH"
cat <<'JSONL'
{"type":"thread.started","thread_id":"thread-stdin"}
{"type":"item.completed","item":{"id":"msg-1","type":"agent_message","text":"Captured"}}
{"type":"turn.completed","usage":{"input_tokens":1,"output_tokens":1}}
JSONL
|}
;;

let with_transport_config ?(configure = fun config -> config) script f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let config =
    { Transport_cli_tool_a.default_config with
      agent_code_path = script
    ; model = Some "agent-code-default"
    ; mcp_config = Some "/tmp/ignored-mcp.json"
    ; allowed_tools = [ "Read" ]
    ; max_turns = Some 2
    ; permission_mode = Some "acceptEdits"
    }
    |> configure
  in
  let transport =
    Transport_cli_tool_a.create ~sw ~mgr:(Eio.Stdenv.process_mgr env) ~config
  in
  f transport
;;

let with_transport script f = with_transport_config script f

let text_of_first_block = function
  | Types.Text text :: _ -> text
  | _ -> ""
;;

let test_complete_sync_restores_jsonl_blocks_and_telemetry () =
  with_script "oas-agent-code-success" agent_code_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  let config = req_config ~model_id:"agent-code-requested" ~min_p:0.1 ~top_k:40 () in
  match (transport.Llm_transport.complete_sync (request ~config ())).response with
  | Ok response ->
    check string "id" "thread-a" response.id;
    check string "model" "agent-code-requested" response.model;
    (match response.content with
     | [ Types.Text "Hello from agent_code"
       ; Types.ToolUse { id = "call-1"; name = "mcp__runtime__inspect"; input }
       ; Types.ToolResult
           { tool_use_id = "call-1"; content = "ok"; is_error = true; json }
       ] ->
       check string "tool input" {|{"path":"README.md"}|} (Yojson.Safe.to_string input);
       check bool "tool result json preserved" true (Option.is_some json)
     | _ -> fail "unexpected content blocks");
    (match response.telemetry with
     | Some telemetry ->
       check
         (option int)
         "provider internal action count"
         (Some 1)
         telemetry.provider_internal_action_count
     | None -> fail "expected telemetry")
  | Error _ -> fail "expected agent_code JSONL success"
;;

let test_complete_stream_emits_tool_result_and_stop () =
  with_script "oas-agent-code-stream" agent_code_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  let events = ref [] in
  match
    transport.Llm_transport.complete_stream
      ~on_event:(fun event -> events := event :: !events)
      (request ())
  with
  | Ok response ->
    check string "id" "thread-a" response.id;
    let events = List.rev !events in
    check
      bool
      "message start"
      true
      (List.exists
         (function
           | Types.MessageStart _ -> true
           | _ -> false)
         events);
    check
      bool
      "tool result error block"
      true
      (List.exists
         (function
           | Types.ContentBlockStart { content_type = "tool_result_error"; _ } -> true
           | _ -> false)
         events);
    check
      bool
      "message stop"
      true
      (List.exists
         (function
           | Types.MessageStop -> true
           | _ -> false)
         events)
  | Error _ -> fail "expected streaming success"
;;

let test_nonzero_exit_recovers_when_turn_completed_seen () =
  with_script "oas-agent-code-recovered" agent_code_recovered_exit_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Ok response ->
    check string "id" "thread-recovered" response.id;
    check string "text" "Recovered" (text_of_first_block response.content)
  | Error _ -> fail "expected stdout recovery success"
;;

let test_runtime_mcp_bearer_header_uses_env_var_override () =
  let args_path = Filename.temp_file "oas-agent-code-args" ".txt" in
  let env_path = Filename.temp_file "oas-agent-code-env" ".txt" in
  match
    with_script "oas-agent-code-capture" agent_code_capture_script
    @@ fun script ->
    with_transport_config
      ~configure:(fun config ->
        { config with
          mcp_config = None
        ; allowed_tools = []
        ; max_turns = None
        ; permission_mode = None
        })
      script
    @@ fun transport ->
    Unix.putenv "ARGS_CAPTURE" args_path;
    Unix.putenv "ENV_CAPTURE" env_path;
    let policy =
      { Llm_transport.empty_runtime_mcp_policy with
        servers =
          [ Llm_transport.Http_server
              { name = "worker"
              ; url = "http://127.0.0.1:9182/mcp"
              ; headers = [ "Authorization", "Bearer secret-token"; "X-Trace", "enabled" ]
              }
          ]
      ; allowed_tool_names = [ "inspect" ]
      }
    in
    match
      (transport.Llm_transport.complete_sync (request ~runtime_mcp_policy:policy ()))
        .response
    with
    | Ok response ->
      check string "text" "MCP configured" (text_of_first_block response.content);
      let args = In_channel.with_open_text args_path In_channel.input_all in
      let env = In_channel.with_open_text env_path In_channel.input_all |> String.trim in
      check bool "bearer token passed through env" true (String.equal "secret-token" env);
      check
        bool
        "argv names bearer env var"
        true
        (Agent_sdk.Util.string_contains ~needle:"bearer_token_env_var" args);
      check
        bool
        "argv omits raw secret"
        false
        (Agent_sdk.Util.string_contains ~needle:"secret-token" args)
    | Error _ -> fail "expected runtime MCP success"
  with
  | result ->
    remove_quietly args_path;
    remove_quietly env_path;
    result
  | exception exn ->
    remove_quietly args_path;
    remove_quietly env_path;
    raise exn
;;

let test_large_prompt_is_sent_over_stdin () =
  let capture_path = Filename.temp_file "oas-agent-code-stdin" ".txt" in
  match
    with_script "oas-agent-code-stdin" agent_code_stdin_capture_script
    @@ fun script ->
    with_transport_config
      ~configure:(fun config ->
        { config with
          mcp_config = None
        ; allowed_tools = []
        ; max_turns = None
        ; permission_mode = None
        })
      script
    @@ fun transport ->
    Unix.putenv "CAPTURE_PATH" capture_path;
    let large = String.make (600 * 1024) 'a' in
    match
      (transport.Llm_transport.complete_sync
         (request ~messages:[ user_message large ] ()))
        .response
    with
    | Ok response ->
      check string "id" "thread-stdin" response.id;
      let captured = In_channel.with_open_text capture_path In_channel.input_all in
      check bool "captured via stdin" true (String.length captured >= 600 * 1024)
    | Error _ -> fail "expected stdin prompt success"
  with
  | result ->
    remove_quietly capture_path;
    result
  | exception exn ->
    remove_quietly capture_path;
    raise exn
;;

let test_empty_output_returns_parse_error () =
  with_script "oas-agent-code-empty" agent_code_empty_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Error (Http_client.NetworkError { message; _ }) ->
    check
      bool
      "mentions no events"
      true
      (Agent_sdk.Util.string_contains ~needle:"no events parsed" message)
  | Error _ -> fail "expected parse NetworkError"
  | Ok _ -> fail "expected parse error"
;;

let () =
  run
    "transport_cli_tool_a_coverage"
    [ ( "transport"
      , [ test_case
            "complete_sync restores JSONL blocks and telemetry"
            `Quick
            test_complete_sync_restores_jsonl_blocks_and_telemetry
        ; test_case
            "complete_stream emits tool result and stop"
            `Quick
            test_complete_stream_emits_tool_result_and_stop
        ; test_case
            "nonzero exit recovers after turn.completed"
            `Quick
            test_nonzero_exit_recovers_when_turn_completed_seen
        ; test_case
            "runtime MCP bearer header uses env var"
            `Quick
            test_runtime_mcp_bearer_header_uses_env_var_override
        ; test_case "large prompt uses stdin" `Quick test_large_prompt_is_sent_over_stdin
        ; test_case
            "empty output returns parse error"
            `Quick
            test_empty_output_returns_parse_error
        ] )
    ]
;;
