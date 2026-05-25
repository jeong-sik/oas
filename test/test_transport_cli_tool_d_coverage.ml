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

let req_config ?(model_id = "auto") () =
  Provider_config.make ~kind:Provider_config.Cli_tool_d ~model_id ~base_url:"" ()
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

let cli_tool_d_stream_success_script =
  {|#!/bin/sh
cat <<'JSONL'
{"type":"system","subtype":"init","model":"agent_llm_a-test","session_id":"session-d"}
{"type":"assistant","message":{"id":"msg-1","model":"agent_llm_a-test","content":[{"type":"text","text":"Hello from agent_llm_a"},{"type":"thinking","thinking":"careful thought","text":"careful thought"},{"type":"tool_use","id":"tool-1","name":"inspect","input":{"path":"README.md"}}],"stop_reason":"tool_use","usage":{"input_tokens":3,"output_tokens":2}}}
{"type":"result","subtype":"success","is_error":false,"result":"fallback text","model":"agent_llm_a-test","stop_reason":"end_turn","session_id":"session-d","usage":{"input_tokens":8,"output_tokens":5,"cache_creation_input_tokens":1,"cache_read_input_tokens":2}}
JSONL
|}
;;

let cli_tool_d_json_success_script =
  {|#!/bin/sh
echo '{"type":"result","subtype":"success","is_error":false,"result":"Plain JSON result","model":"agent_llm_a-json","stop_reason":"end_turn","session_id":"json-session","usage":{"input_tokens":7,"output_tokens":4}}'
|}
;;

let cli_tool_d_error_max_turns_script =
  {|#!/bin/sh
cat <<'JSONL'
{"type":"result","subtype":"error_max_turns","is_error":true,"result":"","model":"agent_llm_a-test","stop_reason":"tool_use","session_id":"session-d","num_turns":31}
JSONL
|}
;;

let cli_tool_d_capture_script =
  {|#!/bin/sh
printf '%s\n' "$*" > "$ARGS_CAPTURE"
cat <<'JSONL'
{"type":"system","subtype":"init","model":"agent_llm_a-test","session_id":"session-mcp"}
{"type":"assistant","message":{"id":"msg-1","model":"agent_llm_a-test","content":[{"type":"text","text":"MCP configured"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}}
{"type":"result","subtype":"success","is_error":false,"result":"MCP configured","model":"agent_llm_a-test","stop_reason":"end_turn","session_id":"session-mcp","usage":{"input_tokens":1,"output_tokens":1}}
JSONL
|}
;;

let cli_tool_d_stdin_capture_script =
  {|#!/bin/sh
cat > "$CAPTURE_PATH"
cat <<'JSONL'
{"type":"system","subtype":"init","model":"agent_llm_a-test","session_id":"session-stdin"}
{"type":"assistant","message":{"id":"msg-1","model":"agent_llm_a-test","content":[{"type":"text","text":"Captured"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}}
{"type":"result","subtype":"success","is_error":false,"result":"Captured","model":"agent_llm_a-test","stop_reason":"end_turn","session_id":"session-stdin","usage":{"input_tokens":1,"output_tokens":1}}
JSONL
|}
;;

let cli_tool_d_empty_script =
  {|#!/bin/sh
exit 0
|}
;;

let with_transport_config ?(configure = fun config -> config) script f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let config =
    { Transport_cli_tool_d.default_config with
      agent_llm_a_path = script
    ; model = Some "agent_llm_a-default"
    ; max_turns = Some 40
    ; allowed_tools = [ "Read" ]
    ; permission_mode = Some "acceptEdits"
    ; mcp_config = Some "/tmp/ignored-mcp.json"
    ; forward_tool_results = true
    }
    |> configure
  in
  let transport =
    Transport_cli_tool_d.create ~sw ~mgr:(Eio.Stdenv.process_mgr env) ~config
  in
  f transport
;;

let with_transport script f = with_transport_config script f

let text_of_first_block = function
  | Types.Text text :: _ -> text
  | _ -> ""
;;

let test_complete_sync_stream_json_preserves_structured_blocks () =
  with_script "oas-cli-tool-d-stream-success" cli_tool_d_stream_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  let config = req_config ~model_id:"agent_llm_a-requested" () in
  match (transport.Llm_transport.complete_sync (request ~config ())).response with
  | Ok response ->
    check string "id" "session-d" response.id;
    check string "model" "agent_llm_a-test" response.model;
    (match response.content with
     | [ Types.Text "Hello from agent_llm_a"
       ; Types.Thinking { thinking_type = "thinking"; content = "careful thought" }
       ; Types.ToolUse { id = "tool-1"; name = "inspect"; input }
       ] ->
       check string "tool input" {|{"path":"README.md"}|} (Yojson.Safe.to_string input)
     | _ -> fail "unexpected content blocks");
    (match response.usage with
     | Some usage ->
       check int "input" 8 usage.input_tokens;
       check int "output" 5 usage.output_tokens;
       check int "cache create" 1 usage.cache_creation_input_tokens;
       check int "cache read" 2 usage.cache_read_input_tokens
     | None -> fail "expected usage")
  | Error _ -> fail "expected stream-json sync success"
;;

let test_complete_stream_emits_content_events_and_stop () =
  with_script "oas-cli-tool-d-stream" cli_tool_d_stream_success_script
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
    check string "id" "session-d" response.id;
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
      "tool use delta"
      true
      (List.exists
         (function
           | Types.ContentBlockDelta { delta = Types.InputJsonDelta _; _ } -> true
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

let test_complete_sync_json_output_path () =
  with_script "oas-cli-tool-d-json" cli_tool_d_json_success_script
  @@ fun script ->
  with_transport_config
    ~configure:(fun config -> { config with tool_use_via_stream_json = false })
    script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Ok response ->
    check string "id" "json-session" response.id;
    check string "model" "agent_llm_a-json" response.model;
    check string "text" "Plain JSON result" (text_of_first_block response.content)
  | Error _ -> fail "expected JSON output success"
;;

let test_error_max_turns_maps_to_provider_terminal () =
  with_script "oas-cli-tool-d-max-turns" cli_tool_d_error_max_turns_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Error (Http_client.ProviderTerminal { kind = Http_client.Max_turns r; message }) ->
    check int "turns" 31 r.turns;
    check int "limit" 31 r.limit;
    check
      bool
      "message mentions max turns"
      true
      (Agent_sdk.Util.string_contains ~needle:"max_turns" message)
  | Error _ -> fail "expected ProviderTerminal"
  | Ok _ -> fail "expected max-turns error"
;;

let test_runtime_mcp_policy_builds_allowed_tools_and_config () =
  let args_path = Filename.temp_file "oas-cli-tool-d-args" ".txt" in
  match
    with_script "oas-cli-tool-d-capture" cli_tool_d_capture_script
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
    let policy =
      { Llm_transport.empty_runtime_mcp_policy with
        servers =
          [ Llm_transport.Stdio_server
              { name = "worker"
              ; command = "worker-mcp"
              ; args = [ "--stdio" ]
              ; env = [ "TOKEN", "abc" ]
              }
          ]
      ; allowed_tool_names = [ "inspect" ]
      ; permission_mode = Some "acceptEdits"
      ; strict = true
      }
    in
    match
      (transport.Llm_transport.complete_sync (request ~runtime_mcp_policy:policy ()))
        .response
    with
    | Ok response ->
      check string "text" "MCP configured" (text_of_first_block response.content);
      let args = In_channel.with_open_text args_path In_channel.input_all in
      check
        bool
        "allowed MCP tool emitted"
        true
        (Agent_sdk.Util.string_contains ~needle:"mcp__worker__inspect" args);
      check
        bool
        "inline MCP config emitted"
        true
        (Agent_sdk.Util.string_contains ~needle:"mcpServers" args);
      check
        bool
        "strict config emitted"
        true
        (Agent_sdk.Util.string_contains ~needle:"--strict-mcp-config" args)
    | Error _ -> fail "expected runtime MCP success"
  with
  | result ->
    remove_quietly args_path;
    result
  | exception exn ->
    remove_quietly args_path;
    raise exn
;;

let test_large_prompt_is_sent_over_stdin () =
  let capture_path = Filename.temp_file "oas-cli-tool-d-stdin" ".txt" in
  match
    with_script "oas-cli-tool-d-stdin" cli_tool_d_stdin_capture_script
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
    let large = String.make (600 * 1024) 'd' in
    match
      (transport.Llm_transport.complete_sync
         (request ~messages:[ user_message large ] ()))
        .response
    with
    | Ok response ->
      check string "id" "session-stdin" response.id;
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
  with_script "oas-cli-tool-d-empty" cli_tool_d_empty_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Error (Http_client.NetworkError { message; _ }) ->
    check
      bool
      "mentions missing stream result"
      true
      (Agent_sdk.Util.string_contains ~needle:"No result or assistant message" message)
  | Error _ -> fail "expected parse NetworkError"
  | Ok _ -> fail "expected parse error"
;;

let () =
  run
    "transport_cli_tool_d_coverage"
    [ ( "transport"
      , [ test_case
            "complete_sync stream-json preserves structured blocks"
            `Quick
            test_complete_sync_stream_json_preserves_structured_blocks
        ; test_case
            "complete_stream emits content events"
            `Quick
            test_complete_stream_emits_content_events_and_stop
        ; test_case "json output path" `Quick test_complete_sync_json_output_path
        ; test_case
            "error_max_turns maps to ProviderTerminal"
            `Quick
            test_error_max_turns_maps_to_provider_terminal
        ; test_case
            "runtime MCP policy builds args"
            `Quick
            test_runtime_mcp_policy_builds_allowed_tools_and_config
        ; test_case "large prompt uses stdin" `Quick test_large_prompt_is_sent_over_stdin
        ; test_case
            "empty output returns parse error"
            `Quick
            test_empty_output_returns_parse_error
        ] )
    ]
;;
