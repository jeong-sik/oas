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

let register_failing_custom_provider ~name ~base_url parse_response =
  let impl : Provider.provider_impl =
    { name
    ; request_kind = Provider.Custom name
    ; request_path = "/v1/custom"
    ; capabilities = Provider.default_capabilities
    ; build_body = (fun ~config:_ ~messages:_ ?tools:_ () -> "{}")
    ; parse_response
    ; resolve = (fun _ -> Ok (base_url, "", [ "Content-Type", "application/json" ]))
    }
  in
  Provider.register_provider impl;
  Provider.custom_provider ~name ~model_id:"mock" ()
;;

let require_detailed_error = function
  | Error detailed -> detailed
  | Ok _ -> Alcotest.fail "expected detailed error, got Ok"
;;

let require_attribution detailed =
  match detailed.Provider_failure_attribution.provider_failure with
  | Some attribution -> attribution
  | None -> Alcotest.fail "expected provider failure attribution"
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

let test_custom_parser_failure_preserves_legacy_error_and_parse_evidence () =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"malformed" ()
  in
  with_mock_server handler (fun ~sw ~net ~clock:_ ~base_url ->
    let provider =
      register_failing_custom_provider
        ~name:"api-custom-parser-failure"
        ~base_url
        (fun _ -> failwith "custom parser failed")
    in
    let config, messages = state_and_messages provider in
    let detailed =
      Api.create_message_detailed ~sw ~net ~provider ~config ~messages ()
      |> require_detailed_error
    in
    (match detailed.Provider_failure_attribution.error with
     | Error.Api
         (Llm_provider.Retry.NetworkError
            { message = "custom parser failed"; kind = Llm_provider.Http_client.Unknown })
       -> ()
     | error ->
       Alcotest.failf
         "expected legacy parser NetworkError, got %s"
         (Error.to_string error));
    let attribution = require_attribution detailed in
    match attribution.Provider_failure_attribution.evidence with
    | Provider_failure_attribution.Response_parse -> ()
    | _ -> Alcotest.fail "expected typed response-parse evidence")
;;

let test_wall_clock_timeout_preserves_legacy_error_and_typed_evidence () =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"slow" ()
  in
  with_mock_server handler (fun ~sw ~net ~clock:_ ~base_url ->
    let provider =
      register_failing_custom_provider
        ~name:"api-custom-wall-clock-timeout"
        ~base_url
        (fun _ -> raise Eio.Time.Timeout)
    in
    let config, messages = state_and_messages provider in
    let detailed =
      Api.create_message_detailed ~sw ~net ~provider ~config ~messages ()
      |> require_detailed_error
    in
    (match detailed.Provider_failure_attribution.error with
     | Error.Api (Llm_provider.Retry.Timeout { phase = None; _ }) -> ()
     | error ->
       Alcotest.failf
         "expected legacy timeout with no phase, got %s"
         (Error.to_string error));
    let attribution = require_attribution detailed in
    match attribution.Provider_failure_attribution.evidence with
    | Provider_failure_attribution.Timeout Llm_provider.Http_client.Wall_clock -> ()
    | _ -> Alcotest.fail "expected typed wall-clock timeout evidence")
;;

(* Forward-looking invariant lock for the legacy [.error] projection of non-2xx
   HTTP responses. [.error] is a public contract consumers read; on an HTTP error
   it must stay exactly [Retry.classify_error ~status ~body], keeping its
   per-status discrimination rather than collapsing, with the typed attribution
   carried alongside.

   Scope, negative-tested against pre-#2576 [lib/api.ml]: this path was NOT part
   of the #2572 regression — it passes on both baselines because [of_http_error]
   already carried the classified projection for HTTP-status errors. The #2572
   regression was confined to the parser-failure and wall-clock-timeout paths,
   which #2576 guards directly. This adds coverage for the highest-traffic
   transport path, which #2576 left untested, so future projection drift here is
   caught. *)
let http_error_statuses =
  [ `Bad_request, 400
  ; `Unauthorized, 401
  ; `Forbidden, 403
  ; `Not_found, 404
  ; `Too_many_requests, 429
  ; `Internal_server_error, 500
  ; `Service_unavailable, 503
  ]
;;

let test_http_error_preserves_legacy_classified_projection () =
  List.iter
    (fun (status_code, status) ->
       let body = Printf.sprintf {|{"error":{"message":"upstream %d"}}|} status in
       let handler _conn _req req_body =
         ignore
           (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) req_body |> take_all) : string);
         Cohttp_eio.Server.respond_string ~status:status_code ~body ()
       in
       with_mock_server handler (fun ~sw ~net ~clock:_ ~base_url ->
         let provider =
           register_failing_custom_provider
             ~name:(Printf.sprintf "api-custom-http-%d" status)
             ~base_url
             (fun _ -> Alcotest.failf "parser must not run on HTTP %d" status)
         in
         let config, messages = state_and_messages provider in
         let detailed =
           Api.create_message_detailed ~sw ~net ~provider ~config ~messages ()
           |> require_detailed_error
         in
         let expected = Error.Api (Llm_provider.Retry.classify_error ~status ~body) in
         let actual = detailed.Provider_failure_attribution.error in
         if actual <> expected
         then
           Alcotest.failf
             "HTTP %d: legacy .error drifted from classify_error; got %s, expected %s"
             status
             (Error.to_string actual)
             (Error.to_string expected);
         (* typed provider attribution is still carried alongside the legacy
           projection *)
         ignore (require_attribution detailed : Provider_failure_attribution.t)))
    http_error_statuses
;;

(* Forward-looking invariant lock for the legacy [.error] projection of a
   transport-level failure (connection refused): it must surface the legacy
   [Retry.NetworkError] projection with the typed attribution carried alongside.

   Scope, negative-tested against pre-#2576 [lib/api.ml]: like the HTTP-status
   path, this was NOT part of the #2572 regression (it passes on both baselines).
   It locks the transport-failure projection, previously untested, against future
   drift. *)
let test_network_failure_preserves_legacy_network_projection () =
  let dead_port = fresh_port () in
  let handler _conn _req req_body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) req_body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"" ()
  in
  (* [with_mock_server] only supplies the Eio context here; the provider targets
     [dead_port], where nothing listens, so [post_sync] fails to connect. *)
  with_mock_server handler (fun ~sw ~net ~clock:_ ~base_url:_ ->
    let dead_url = Printf.sprintf "http://127.0.0.1:%d" dead_port in
    let provider =
      register_failing_custom_provider
        ~name:"api-custom-network-failure"
        ~base_url:dead_url
        (fun _ -> Alcotest.fail "parser must not run on a transport failure")
    in
    let config, messages = state_and_messages provider in
    let detailed =
      Api.create_message_detailed ~sw ~net ~provider ~config ~messages ()
      |> require_detailed_error
    in
    (match detailed.Provider_failure_attribution.error with
     | Error.Api (Llm_provider.Retry.NetworkError _) -> ()
     | error ->
       Alcotest.failf
         "expected legacy NetworkError projection, got %s"
         (Error.to_string error));
    ignore (require_attribution detailed : Provider_failure_attribution.t))
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
        ; Alcotest.test_case
            "custom parser failure preserves legacy error and parse evidence"
            `Quick
            test_custom_parser_failure_preserves_legacy_error_and_parse_evidence
        ; Alcotest.test_case
            "wall-clock timeout preserves legacy error and typed evidence"
            `Quick
            test_wall_clock_timeout_preserves_legacy_error_and_typed_evidence
        ; Alcotest.test_case
            "http error preserves legacy classified projection across statuses"
            `Quick
            test_http_error_preserves_legacy_classified_projection
        ; Alcotest.test_case
            "transport failure preserves legacy network projection"
            `Quick
            test_network_failure_preserves_legacy_network_projection
        ] )
    ]
;;
