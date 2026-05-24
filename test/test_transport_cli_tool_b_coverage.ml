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
  Provider_config.make
    ~kind:Provider_config.Cli_tool_b
    ~model_id:"provider_f-2.5-pro"
    ~base_url:""
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

let request ?runtime_mcp_policy () =
  { Llm_transport.config = req_config
  ; messages = [ user_message "hello" ]
  ; tools = []
  ; runtime_mcp_policy
  }
;;

let provider_f_success_script =
  {|#!/bin/sh
echo '{"session_id":"gemini-cli","response":"Hello from provider_f","stats":{"models":{"provider_f-2.5-pro":{"tokens":{"input":3,"candidates":4,"cached":1}}}}}'
|}
;;

let provider_f_quota_script =
  {|#!/bin/sh
echo 'TerminalQuotaError retryDelayMs: 2000' >&2
exit 1
|}
;;

let with_transport script f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let config =
    { Transport_cli_tool_b.default_config with
      provider_f_path = script
    ; model = Some "provider_f-2.5-pro"
    ; mcp_config = Some "/tmp/ignored-mcp.json"
    ; allowed_tools = [ "Read" ]
    ; max_turns = Some 2
    ; permission_mode = Some "acceptEdits"
    }
  in
  let transport =
    Transport_cli_tool_b.create ~sw ~mgr:(Eio.Stdenv.process_mgr env) ~config
  in
  f transport
;;

let test_complete_sync_parses_provider_f_json () =
  with_script "oas-provider-f-success" provider_f_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Ok response ->
    check string "id" "gemini-cli" response.id;
    check string "model" "provider_f" response.model;
    check
      string
      "text"
      "Hello from provider_f"
      (match response.content with
       | Types.Text text :: _ -> text
       | _ -> "");
    (match response.usage with
     | Some usage ->
       check int "input tokens" 3 usage.input_tokens;
       check int "output tokens" 4 usage.output_tokens;
       check int "cached tokens" 1 usage.cache_read_input_tokens
     | None -> fail "expected usage")
  | Error _ -> fail "expected provider_f JSON success"
;;

let test_complete_stream_replays_synthetic_events () =
  with_script "oas-provider-f-stream" provider_f_success_script
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
    check string "id" "gemini-cli" response.id;
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
      "message stop"
      true
      (List.exists
         (function
           | Types.MessageStop -> true
           | _ -> false)
         events)
  | Error _ -> fail "expected synthetic stream success"
;;

let test_runtime_mcp_policy_is_rejected_before_subprocess () =
  with_script "oas-provider-f-unused" provider_f_success_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  let policy =
    { Llm_transport.empty_runtime_mcp_policy with
      servers =
        [ Llm_transport.Http_server
            { name = "mcp"; url = "http://127.0.0.1:1"; headers = [] }
        ]
    }
  in
  match
    (transport.Llm_transport.complete_sync (request ~runtime_mcp_policy:policy ()))
      .response
  with
  | Error
      (Http_client.ProviderFailure
         { kind = Capability_mismatch { capability = Some cap }; _ }) ->
    check string "capability" "request_scoped_runtime_mcp" cap
  | Error _ -> fail "expected capability mismatch"
  | Ok _ -> fail "expected runtime MCP rejection"
;;

let test_quota_stderr_maps_to_provider_failure () =
  with_script "oas-provider-f-quota" provider_f_quota_script
  @@ fun script ->
  with_transport script
  @@ fun transport ->
  match (transport.Llm_transport.complete_sync (request ())).response with
  | Error
      (Http_client.ProviderFailure
         { kind = Hard_quota { retry_after = Some retry_after }; _ }) ->
    check (float 0.001) "retry after seconds" 2.0 retry_after
  | Error _ -> fail "expected hard quota provider failure"
  | Ok _ -> fail "expected quota failure"
;;

let () =
  run
    "transport_cli_tool_b_coverage"
    [ ( "transport"
      , [ test_case
            "complete_sync parses provider_f JSON"
            `Quick
            test_complete_sync_parses_provider_f_json
        ; test_case
            "complete_stream replays synthetic events"
            `Quick
            test_complete_stream_replays_synthetic_events
        ; test_case
            "runtime MCP policy rejected"
            `Quick
            test_runtime_mcp_policy_is_rejected_before_subprocess
        ; test_case
            "quota stderr maps to provider failure"
            `Quick
            test_quota_stderr_maps_to_provider_failure
        ] )
    ]
;;
