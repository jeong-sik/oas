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

let req_config =
  Provider_config.make ~kind:Provider_config.Cli_tool_c ~model_id:"auto" ~base_url:"" ()
;;

let user_message text : Types.message =
  { role = Types.User
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let request ?(messages = [ user_message "hello" ]) ?(tools = []) () =
  { Llm_transport.config = req_config; messages; tools; runtime_mcp_policy = None }
;;

let provider_c_success_script =
  {|#!/bin/sh
echo '{"id":"resp-cli","role":"assistant","model":"provider_c-test","content":"Hello"}'
echo '{"role":"assistant","tool_calls":[{"type":"function","id":"call-1","function":{"name":"inspect","arguments":"{\"path\":\"README.md\"}"}}]}'
echo '{"role":"tool","tool_call_id":"call-1","content":"{\"ok\":true}"}'
echo '{"usage":{"input_tokens":3,"output_tokens":4}}'
|}
;;

let provider_c_exit_1_script =
  {|#!/bin/sh
echo 'auth failed' >&2
exit 1
|}
;;

let provider_c_empty_script =
  {|#!/bin/sh
exit 0
|}
;;

let provider_c_stdin_capture_script =
  {|#!/bin/sh
cat > "$CAPTURE_PATH"
echo '{"id":"stdin-cli","role":"assistant","model":"provider_c-test","content":"Captured"}'
|}
;;

let provider_c_argv_capture_script =
  {|#!/bin/sh
printf '%s\n' "$*" >> "$ARGS_CAPTURE"
echo '{"id":"argv-cli","role":"assistant","model":"provider_c-test","content":"Argv"}'
|}
;;

let with_transport_config ?(configure = fun config -> config) script f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let config =
    { Transport_cli_tool_c.default_config with
      provider_c_path = script
    ; model = Some "provider_c-test"
    ; extra_env = [ "OAS_TEST_TRANSPORT_C", "1" ]
    }
    |> configure
  in
  let transport =
    Transport_cli_tool_c.create ~sw ~mgr:(Eio.Stdenv.process_mgr env) ~config
  in
  f transport
;;

let with_transport script f = with_transport_config script f

let test_complete_sync_restores_text_tool_and_usage () =
  with_script "oas-provider-c-success" provider_c_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Ok response ->
    check string "id" "resp-cli" response.id;
    check string "model" "provider_c-test" response.model;
    (match response.content with
     | [ Types.Text "Hello"
       ; Types.ToolUse { id = "call-1"; name = "inspect"; input }
       ; Types.ToolResult { tool_use_id; content; json; _ }
       ] ->
       check string "tool input" {|{"path":"README.md"}|} (Yojson.Safe.to_string input);
       check string "tool result id" "call-1" tool_use_id;
       check string "tool result content" {|{"ok":true}|} content;
       check bool "tool result json parsed" true (Option.is_some json)
     | _ -> fail "unexpected content blocks");
    (match response.usage with
     | Some usage ->
       check bool "estimated input positive" true (usage.input_tokens > 0);
       check bool "estimated output positive" true (usage.output_tokens > 0)
     | None -> fail "expected usage")
  | Error err ->
    let message =
      match err with
      | Http_client.HttpError { code; body } -> Printf.sprintf "http %d %s" code body
      | Http_client.NetworkError { message; _ }
      | Http_client.TimeoutError { message; _ }
      | Http_client.ProviderTerminal { message; _ }
      | Http_client.ProviderFailure { message; _ } -> message
      | Http_client.AcceptRejected { reason } -> reason
      | Http_client.CliTransportRequired { kind } -> kind
    in
    fail ("unexpected error: " ^ message)
;;

let test_complete_stream_emits_blocks_and_stop () =
  with_script "oas-provider-c-stream" provider_c_success_script
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
    check string "id" "resp-cli" response.id;
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
      "content start"
      true
      (List.exists
         (function
           | Types.ContentBlockStart _ -> true
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

let test_complete_sync_exit_1_surfaces_subprocess_error () =
  with_script "oas-provider-c-exit1" provider_c_exit_1_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Error (Http_client.NetworkError { message; _ }) ->
    check
      bool
      "mentions exit 1"
      true
      (Agent_sdk.Util.string_contains ~needle:"exit code 1" message
       || Agent_sdk.Util.string_contains ~needle:"exited with code 1" message)
  | Error _ -> fail "expected subprocess network error"
  | Ok _ -> fail "expected error"
;;

let test_warns_once_for_external_tools () =
  with_script "oas-provider-c-tools" provider_c_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  let tool = `Assoc [ "name", `String "external_tool" ] in
  match (transport.Llm_transport.complete_sync (request ~tools:[ tool ] ())).response with
  | Ok response ->
    check
      string
      "text restored"
      "Hello"
      (match response.content with
       | Types.Text text :: _ -> text
       | _ -> "")
  | Error _ -> fail "expected success with ignored external tool callback"
;;

let test_complete_sync_empty_output_returns_parse_error () =
  with_script "oas-provider-c-empty" provider_c_empty_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Error (Http_client.NetworkError { message; _ }) ->
    check
      bool
      "mentions no messages"
      true
      (Agent_sdk.Util.string_contains ~needle:"no messages parsed" message)
  | Error _ -> fail "expected parse NetworkError"
  | Ok _ -> fail "expected parse error"
;;

let test_large_prompt_is_sent_over_stdin () =
  let capture_path = Filename.temp_file "oas-provider-c-stdin" ".txt" in
  match
    with_script "oas-provider-c-stdin" provider_c_stdin_capture_script
    @@ fun script ->
    with_transport_config
      ~configure:(fun config ->
        { config with extra_env = ("CAPTURE_PATH", capture_path) :: config.extra_env })
      script
    @@ fun transport ->
    let large = String.make (70 * 1024) 'x' in
    match
      (transport.Llm_transport.complete_sync
         (request ~messages:[ user_message large ] ()))
        .response
    with
    | Ok response ->
      check string "id" "stdin-cli" response.id;
      let captured = In_channel.with_open_text capture_path In_channel.input_all in
      check bool "captured via stdin" true (String.length captured >= 70 * 1024)
    | Error _ -> fail "expected stdin prompt success"
  with
  | result ->
    remove_quietly capture_path;
    result
  | exception exn ->
    remove_quietly capture_path;
    raise exn
;;

let test_session_reuse_sends_only_message_delta () =
  let args_path = Filename.temp_file "oas-provider-c-argv" ".txt" in
  match
    with_script "oas-provider-c-argv" provider_c_argv_capture_script
    @@ fun script ->
    with_transport_config
      ~configure:(fun config ->
        { config with
          session_id = Some "session-xyz"
        ; extra_env = ("ARGS_CAPTURE", args_path) :: config.extra_env
        })
      script
    @@ fun transport ->
    let first = request ~messages:[ user_message "first turn" ] () in
    let second =
      request ~messages:[ user_message "first turn"; user_message "second turn" ] ()
    in
    ignore (transport.Llm_transport.complete_sync first);
    ignore (transport.Llm_transport.complete_sync second);
    let lines =
      In_channel.with_open_text args_path In_channel.input_all
      |> String.split_on_char '\n'
      |> List.filter (fun line -> String.trim line <> "")
    in
    match lines with
    | first_args :: second_args :: _ ->
      check
        bool
        "session flag emitted"
        true
        (Agent_sdk.Util.string_contains ~needle:"--session session-xyz" first_args);
      check
        bool
        "first prompt includes first turn"
        true
        (Agent_sdk.Util.string_contains ~needle:"first turn" first_args);
      check
        bool
        "second prompt includes second turn"
        true
        (Agent_sdk.Util.string_contains ~needle:"second turn" second_args);
      check
        bool
        "second prompt omits prior turn"
        false
        (Agent_sdk.Util.string_contains ~needle:"first turn" second_args)
    | _ -> fail "expected two argv capture lines"
  with
  | result ->
    remove_quietly args_path;
    result
  | exception exn ->
    remove_quietly args_path;
    raise exn
;;

let () =
  run
    "transport_cli_tool_c_coverage"
    [ ( "transport"
      , [ test_case
            "complete_sync restores text/tool/usage"
            `Quick
            test_complete_sync_restores_text_tool_and_usage
        ; test_case
            "complete_stream emits block events"
            `Quick
            test_complete_stream_emits_blocks_and_stop
        ; test_case
            "exit 1 surfaces subprocess error"
            `Quick
            test_complete_sync_exit_1_surfaces_subprocess_error
        ; test_case
            "external tools warning path still succeeds"
            `Quick
            test_warns_once_for_external_tools
        ; test_case
            "empty output returns parse error"
            `Quick
            test_complete_sync_empty_output_returns_parse_error
        ; test_case "large prompt uses stdin" `Quick test_large_prompt_is_sent_over_stdin
        ; test_case
            "session reuse sends only delta"
            `Quick
            test_session_reuse_sends_only_message_delta
        ] )
    ]
;;
