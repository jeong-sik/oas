open Agent_sdk
open Types

let openai_response =
  {|{"id":"chatcmpl-legacy-api","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":2}}|}
;;

let fresh_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> Alcotest.fail "expected inet socket"
  in
  Unix.close socket;
  port
;;

let with_mock_server ?port handler f =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let port = Option.value port ~default:(fresh_port ()) in
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog:128
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net:env#net ~clock:env#clock ~base_url;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let empty_openai_response finish_reason =
  Printf.sprintf
    {|{"id":"chatcmpl-empty","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":null},"finish_reason":"%s"}],"usage":{"prompt_tokens":1,"completion_tokens":0}}|}
    finish_reason
;;

let state_and_messages (provider : Provider.config) =
  let config =
    { default_config with
      model = provider.model_id
    ; system_prompt = Some "reply briefly"
    ; max_turns = 1
    ; max_tokens = Some 16
    }
  in
  let state = { config; messages = []; turn_count = 0; usage = empty_usage } in
  let messages =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  state, messages
;;

let expect_provider_unavailable = function
  | Error (Error.Provider (Llm_provider.Error.ProviderUnavailable _)) -> ()
  | Error err ->
    Alcotest.failf "expected ProviderUnavailable, got %s" (Error.to_string err)
  | Ok _ -> Alcotest.fail "expected ProviderUnavailable, got Ok"
;;

let test_create_message_uses_hardened_http_client () =
  let seen_connection = ref None in
  let seen_content_length = ref None in
  let seen_path = ref None in
  let handler _conn req body =
    let headers = Cohttp.Request.headers req in
    seen_connection := Cohttp.Header.get headers "connection";
    seen_content_length := Cohttp.Header.get headers "content-length";
    seen_path := Some (Uri.path (Cohttp.Request.uri req));
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:openai_response ()
  in
  with_mock_server ~port:18341 handler (fun ~sw ~net ~clock ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let config =
      { default_config with
        model = provider.model_id
      ; system_prompt = Some "reply briefly"
      ; max_turns = 1
      ; max_tokens = Some 16
      }
    in
    let state = { config; messages = []; turn_count = 0; usage = empty_usage } in
    let messages =
      [ { role = User
        ; content = [ Text "hello" ]
        ; name = None
        ; tool_call_id = None
        ; metadata = []
        }
      ]
    in
    match Api.create_message ~sw ~net ~clock ~provider ~config:state ~messages () with
    | Error err -> Alcotest.failf "expected Ok, got %s" (Error.to_string err)
    | Ok response ->
      Alcotest.(check (option string))
        "request path"
        (Some "/v1/chat/completions")
        !seen_path;
      Alcotest.(check (option string)) "connection close" (Some "close") !seen_connection;
      Alcotest.(check bool)
        "content-length set"
        true
        (match !seen_content_length with
         | Some raw -> int_of_string_opt raw |> Option.value ~default:0 > 0
         | None -> false);
      Alcotest.(check string) "model" "mock" response.model;
      (match response.telemetry with
       | Some { output_token_receipt = Some receipt; _ } ->
         Alcotest.(check (option int))
           "legacy receipt requested"
           (Some 16)
           (output_token_receipt_requested receipt);
         Alcotest.(check (option int))
           "legacy receipt effective"
           (Some 16)
           (output_token_receipt_effective receipt);
         Alcotest.(check bool)
           "legacy receipt envelope"
           true
           (Yojson.Safe.Util.member "envelope" (output_token_receipt_to_yojson receipt)
            = output_token_envelope_to_yojson Openai_chat_max_tokens)
       | Some { output_token_receipt = None; _ } | None ->
         Alcotest.fail "expected legacy API output-token receipt");
      Alcotest.(check int)
        "text blocks"
        1
        (List.length
           (List.filter_map
              (function
                | Text text -> Some text
                | _ -> None)
              response.content)))
;;

let test_create_message_empty_completion_maps_to_provider_unavailable () =
  List.iter
    (fun finish_reason ->
       let handler _conn _req body =
         ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
         Cohttp_eio.Server.respond_string
           ~status:`OK
           ~body:(empty_openai_response finish_reason)
           ()
       in
       with_mock_server handler (fun ~sw ~net ~clock ~base_url ->
         let provider : Provider.config =
           { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
         in
         let config, messages = state_and_messages provider in
         Api.create_message ~sw ~net ~clock ~provider ~config ~messages ()
         |> expect_provider_unavailable))
    [ "stop"; "length" ]
;;

let test_custom_stream_fallback_empty_maps_to_provider_unavailable () =
  List.iter
    (fun stop_reason ->
       let handler _conn _req body =
         ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
         Cohttp_eio.Server.respond_string ~status:`OK ~body:"custom-empty" ()
       in
       with_mock_server handler (fun ~sw ~net ~clock:_ ~base_url ->
         let name =
           "api-custom-empty-" ^ Llm_provider.Types.stop_reason_to_string stop_reason
         in
         let impl : Provider.provider_impl =
           { name
           ; request_kind = Provider.Custom name
           ; request_path = "/v1/custom"
           ; capabilities =
               { Provider.default_capabilities with supports_native_streaming = false }
           ; build_body = (fun ~config:_ ~messages:_ ?tools:_ () -> "{}")
           ; parse_response =
               (fun _ ->
                 { id = "custom-empty"
                 ; model = "mock"
                 ; stop_reason
                 ; content = []
                 ; usage = None
                 ; telemetry = None
                 })
           ; resolve =
               (fun _ -> Ok (base_url, "", [ "Content-Type", "application/json" ]))
           }
         in
         Provider.register_provider impl;
         let provider = Provider.custom_provider ~name ~model_id:"mock" () in
         let config, messages = state_and_messages provider in
         let events = ref [] in
         let result =
           Streaming.create_message_stream
             ~sw
             ~net
             ~provider
             ~config
             ~messages
             ~on_event:(fun event -> events := event :: !events)
             ()
         in
         Alcotest.(check int) "no synthetic events" 0 (List.length !events);
         expect_provider_unavailable result))
    [ Llm_provider.Types.EndTurn; Llm_provider.Types.MaxTokens ]
;;

let () =
  Alcotest.run
    "Api_http_client"
    [ ( "legacy_create_message"
      , [ Alcotest.test_case
            "uses hardened post_sync headers"
            `Quick
            test_create_message_uses_hardened_http_client
        ; Alcotest.test_case
            "empty completion maps to provider unavailable"
            `Quick
            test_create_message_empty_completion_maps_to_provider_unavailable
        ; Alcotest.test_case
            "custom stream fallback maps empty to provider unavailable"
            `Quick
            test_custom_stream_fallback_empty_maps_to_provider_unavailable
        ] )
    ]
;;
