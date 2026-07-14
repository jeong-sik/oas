(** Provider_intf tests — module type satisfaction and dispatch. *)

open Agent_sdk
open Agent_sdk.Types
module Retry = Llm_provider.Retry

let require_provider config =
  match Provider_intf.of_config config with
  | Ok m -> m
  | Error err -> Alcotest.failf "Expected provider, got error: %s" (Error.to_string err)
;;

let require_streaming_provider config =
  match Provider_intf.of_config_streaming config with
  | Ok (Some m) -> m
  | Ok None -> Alcotest.fail "Expected a streaming provider, got None"
  | Error err ->
    Alcotest.failf "Expected streaming provider, got error: %s" (Error.to_string err)
;;

let require_detailed_provider config =
  match Provider_intf.of_config_detailed config with
  | Ok provider -> provider
  | Error detailed ->
    Alcotest.failf
      "Expected detailed provider, got error: %s"
      (Error.to_string detailed.error)
;;

let require_detailed_streaming_provider config =
  match Provider_intf.of_config_streaming_detailed config with
  | Ok (Some provider) -> provider
  | Ok None -> Alcotest.fail "Expected a detailed streaming provider, got None"
  | Error detailed ->
    Alcotest.failf
      "Expected detailed streaming provider, got error: %s"
      (Error.to_string detailed.error)
;;

(* ── Module type satisfaction ────────────────────────────── *)

let test_of_config_local () =
  let config =
    Provider.local_llm
      ~base_url:Llm_provider.Constants.Endpoints.default_url_localhost
      ~model_id:"test-model"
      ()
  in
  let (module P : Provider_intf.PROVIDER) = require_provider config in
  (* Module was constructed — type check passed at compile time.
     We can't call create_message without a real network, but the
     module satisfying PROVIDER is the key guarantee. *)
  ignore (module P : Provider_intf.PROVIDER)
;;

let test_of_config_openai () =
  let config = Provider.openrouter ~model_id:"gpt-4" () in
  let (module P : Provider_intf.PROVIDER) = require_provider config in
  ignore (module P : Provider_intf.PROVIDER)
;;

let test_of_config_propagates_resolve_error () =
  let config =
    { Provider.provider = Provider.Anthropic
    ; model_id = "claude-3-5-sonnet-20241022"
    ; api_key_env = "OAS_PROVIDER_INTF_NONEXISTENT_KEY"
    }
  in
  match Provider_intf.of_config config with
  | Error (Error.Config (MissingEnvVar _)) -> ()
  | Ok _ -> Alcotest.fail "Expected resolve error for missing env var"
  | Error err ->
    Alcotest.failf "Expected MissingEnvVar error, got: %s" (Error.to_string err)
;;

let test_streaming_resolves_before_capability_projection () =
  let config : Provider.config =
    { provider = Provider.Custom_registered { name = "unregistered-provider" }
    ; model_id = "model-a"
    ; api_key_env = ""
    }
  in
  match Provider_intf.of_config_streaming_detailed config with
  | Error
      { error = Error.Config (InvalidConfig { field = "provider"; _ })
      ; provider_failure =
          Some { ownership = Provider_failure_attribution.Unclassified; _ }
      } -> ()
  | Error detailed ->
    Alcotest.failf
      "Expected explicit unregistered-provider error, got: %s"
      (Error.to_string detailed.error)
  | Ok None -> Alcotest.fail "Unregistered non-streaming provider was silently accepted"
  | Ok (Some _) -> Alcotest.fail "Unregistered provider resolved to a streaming module"
;;

(* ── supports_streaming ──────────────────────────────────── *)

let test_anthropic_supports_streaming () =
  let config = Provider.anthropic ~model_id:"claude-sonnet-4-6" () in
  Alcotest.(check bool) "anthropic streams" true (Provider_intf.supports_streaming config)
;;

(* ── of_config_streaming ─────────────────────────────────── *)

let test_streaming_provider_some () =
  let config =
    Provider.local_llm
      ~base_url:Llm_provider.Constants.Endpoints.default_url_localhost
      ~model_id:"test-model"
      ()
  in
  let (module SP : Provider_intf.STREAMING_PROVIDER) =
    require_streaming_provider config
  in
  ignore (module SP : Provider_intf.STREAMING_PROVIDER)
;;

(* ── HTTP dispatch ───────────────────────────────────────── *)

let openai_response =
  {|{"id":"chatcmpl-provider-intf","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":2}}|}
;;

let empty_openai_response finish_reason =
  Printf.sprintf
    {|{"id":"chatcmpl-empty","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":null},"finish_reason":"%s"}],"usage":{"prompt_tokens":1,"completion_tokens":0}}|}
    finish_reason
;;

let user_messages =
  [ { role = User
    ; content = [ Text "hello" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  ]
;;

let state_for_provider (provider : Provider.config) =
  let config =
    { (default_config ~model:provider.model_id) with
      system_prompt = Some "reply briefly"
    ; max_tokens = Some 16
    }
  in
  { config; messages = []; turn_count = 0; usage = empty_usage }
;;

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> Alcotest.fail "expected inet socket"
  in
  Unix.close s;
  port
;;

let with_mock_server ?port handler f =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let port = Option.value ~default:(fresh_port ()) port in
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
    f ~sw ~net:env#net ~base_url;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let expect_provider_unavailable = function
  | Error (Error.Provider (Llm_provider.Error.ProviderUnavailable _)) -> ()
  | Error err ->
    Alcotest.failf "expected ProviderUnavailable, got %s" (Error.to_string err)
  | Ok _ -> Alcotest.fail "expected ProviderUnavailable, got Ok"
;;

let with_openai_response response_body check_result =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  in
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let (module P : Provider_intf.PROVIDER) = require_provider provider in
    P.create_message
      ~sw
      ~net
      ~config:(state_for_provider provider)
      ~messages:user_messages
      ()
    |> check_result)
;;

let expect_invalid_request ~reason ~message_prefix = function
  | Error (Error.Api (Retry.InvalidRequest { message; reason = actual_reason })) ->
    Alcotest.(check bool) "reason preserved" true (actual_reason = reason);
    Alcotest.(check bool)
      "message prefix preserved"
      true
      (String.starts_with ~prefix:message_prefix message)
  | Error err -> Alcotest.failf "expected InvalidRequest, got %s" (Error.to_string err)
  | Ok _ -> Alcotest.fail "expected InvalidRequest, got Ok"
;;

let test_provider_dispatch_uses_http_client () =
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
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let (module P : Provider_intf.PROVIDER) = require_provider provider in
    match
      P.create_message
        ~sw
        ~net
        ~config:(state_for_provider provider)
        ~messages:user_messages
        ()
    with
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
      Alcotest.(check string) "model" "mock" response.model)
;;

let test_provider_dispatch_maps_server_error () =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string
      ~status:`Service_unavailable
      ~body:"temporarily down"
      ()
  in
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let (module P : Provider_intf.PROVIDER) = require_provider provider in
    match
      P.create_message
        ~sw
        ~net
        ~config:(state_for_provider provider)
        ~messages:user_messages
        ()
    with
    | Error (Error.Api (Retry.ServerError { status; message })) ->
      Alcotest.(check int) "status" 503 status;
      Alcotest.(check string) "message" "temporarily down" message
    | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
    | Ok _ -> Alcotest.fail "expected server error")
;;

let test_provider_dispatch_rejects_malformed_openai_response () =
  with_openai_response
    {|{"choices":"not-a-list"}|}
    (expect_invalid_request
       ~reason:Retry.Json_parse_error
       ~message_prefix:"JSON type error:")
;;

let test_provider_dispatch_preserves_provider_error_projection () =
  with_openai_response
    {|{"error":{"message":"context window exceeded"}}|}
    (expect_invalid_request
       ~reason:Retry.Unknown_invalid_request
       ~message_prefix:"context window exceeded")
;;

let test_provider_dispatch_preserves_json_parse_error_projection () =
  with_openai_response
    "{"
    (expect_invalid_request
       ~reason:Retry.Json_parse_error
       ~message_prefix:"JSON parse error:")
;;

let test_provider_dispatch_preserves_json_undefined_projection () =
  with_openai_response
    {|{"choices":[]}|}
    (expect_invalid_request
       ~reason:Retry.Json_parse_error
       ~message_prefix:"JSON undefined field error:")
;;

let test_provider_dispatch_empty_openai_maps_to_unavailable () =
  List.iter
    (fun finish_reason ->
       let handler _conn _req body =
         ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
         Cohttp_eio.Server.respond_string
           ~status:`OK
           ~body:(empty_openai_response finish_reason)
           ()
       in
       with_mock_server handler (fun ~sw ~net ~base_url ->
         let provider : Provider.config =
           { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
         in
         let (module P : Provider_intf.PROVIDER) = require_provider provider in
         P.create_message
           ~sw
           ~net
           ~config:(state_for_provider provider)
           ~messages:user_messages
           ()
         |> expect_provider_unavailable))
    [ "stop"; "length" ]
;;

let test_custom_provider_empty_maps_to_unavailable () =
  List.iter
    (fun stop_reason ->
       let handler _conn _req body =
         ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
         Cohttp_eio.Server.respond_string ~status:`OK ~body:"custom-empty" ()
       in
       with_mock_server handler (fun ~sw ~net ~base_url ->
         let name =
           "provider-intf-empty-" ^ Llm_provider.Types.stop_reason_to_string stop_reason
         in
         let impl : Provider.provider_impl =
           { name
           ; provider_kind = Llm_provider.Provider_config.OpenAI_compat
           ; request_kind = Provider.Custom name
           ; request_path = "/v1/custom"
           ; capabilities =
               { Provider.default_capabilities with supports_native_streaming = false }
           ; build_body = (fun ~config:_ ~messages:_ ?tools:_ () -> "{}")
           ; parse_response =
               (fun _ ->
                 { id = "custom-empty"
                 ; model = "custom-model"
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
         let provider = Provider.custom_provider ~name ~model_id:"custom-model" () in
         let (module P : Provider_intf.PROVIDER) = require_provider provider in
         P.create_message
           ~sw
           ~net
           ~config:(state_for_provider provider)
           ~messages:user_messages
           ()
         |> expect_provider_unavailable))
    [ Llm_provider.Types.EndTurn; Llm_provider.Types.MaxTokens ]
;;

let test_custom_provider_dispatch_uses_registered_impl () =
  let custom_name = "provider-intf-custom-dispatch" in
  let seen_path = ref None in
  let seen_body = ref None in
  let handler _conn req body =
    seen_path := Some (Uri.path (Cohttp.Request.uri req));
    seen_body := Some Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"custom response body" ()
  in
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let impl : Provider.provider_impl =
      { name = custom_name
      ; provider_kind = Llm_provider.Provider_config.OpenAI_compat
      ; request_kind = Provider.Custom custom_name
      ; request_path = "/v1/custom"
      ; capabilities =
          { Provider.default_capabilities with supports_native_streaming = false }
      ; build_body = (fun ~config:_ ~messages:_ ?tools:_ () -> {|{"custom":true}|})
      ; parse_response =
          (fun body ->
            { id = "custom-id"
            ; model = "custom-model"
            ; stop_reason = EndTurn
            ; content = [ Text body ]
            ; usage = None
            ; telemetry = None
            })
      ; resolve = (fun _cfg -> Ok (base_url, "", [ "Content-Type", "application/json" ]))
      }
    in
    Provider.register_provider impl;
    let provider =
      Provider.custom_provider ~name:custom_name ~model_id:"custom-model" ()
    in
    (match Provider_intf.of_config_streaming provider with
     | Ok None -> ()
     | Ok (Some _) -> Alcotest.fail "custom provider should not expose streaming"
     | Error err ->
       Alcotest.failf "unexpected streaming resolve error: %s" (Error.to_string err));
    let (module P : Provider_intf.PROVIDER) = require_provider provider in
    match
      P.create_message
        ~sw
        ~net
        ~config:(state_for_provider provider)
        ~messages:user_messages
        ()
    with
    | Error err -> Alcotest.failf "expected Ok, got %s" (Error.to_string err)
    | Ok response ->
      Alcotest.(check (option string)) "custom path" (Some "/v1/custom") !seen_path;
      Alcotest.(check (option string)) "custom body" (Some {|{"custom":true}|}) !seen_body;
      Alcotest.(check string) "custom response id" "custom-id" response.id;
      Alcotest.(check string)
        "custom response text"
        "custom response body"
        (match response.content with
         | [ Text text ] -> text
         | _ -> Alcotest.fail "expected text response"))
;;

let require_attribution
      (result : (Types.api_response, Provider_failure_attribution.detailed_error) result)
  =
  match result with
  | Ok _ -> Alcotest.fail "expected detailed provider failure"
  | Error detailed ->
    (match detailed.provider_failure with
     | Some attribution -> detailed, attribution
     | None -> Alcotest.fail "expected provider attribution")
;;

let test_detailed_api_provider_intf_and_streaming_share_boundary () =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string
      ~status:`Service_unavailable
      ~body:"binding unavailable"
      ()
  in
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let state = state_for_provider provider in
    let api_detailed, api_attribution =
      Api.create_message_detailed
        ~sw
        ~net
        ~provider
        ~config:state
        ~messages:user_messages
        ()
      |> require_attribution
    in
    let (module P : Provider_intf.DETAILED_PROVIDER) =
      require_detailed_provider provider
    in
    let provider_detailed, provider_attribution =
      P.create_message_detailed ~sw ~net ~config:state ~messages:user_messages ()
      |> require_attribution
    in
    let (module SP : Provider_intf.DETAILED_STREAMING_PROVIDER) =
      require_detailed_streaming_provider provider
    in
    let stream_detailed, stream_attribution =
      SP.create_message_stream_detailed
        ~sw
        ~net
        ~config:state
        ~messages:user_messages
        ~on_event:(fun _ -> ())
        ()
      |> require_attribution
    in
    List.iter
      (fun (name, detailed, attribution) ->
         (match detailed.Provider_failure_attribution.error with
          | Error.Api (Retry.ServerError { status = 503; _ }) -> ()
          | error ->
            Alcotest.failf "%s: expected HTTP 503, got %s" name (Error.to_string error));
         Alcotest.(check bool)
           (name ^ " ownership")
           true
           (attribution.Provider_failure_attribution.ownership
            = Provider_failure_attribution.Unclassified))
      [ "api", api_detailed, api_attribution
      ; "provider_intf", provider_detailed, provider_attribution
      ; "streaming", stream_detailed, stream_attribution
      ];
    let binding name attribution =
      match attribution.Provider_failure_attribution.binding with
      | Some binding -> binding
      | None -> Alcotest.failf "%s: missing binding identity" name
    in
    let api_binding = binding "api" api_attribution in
    Alcotest.(check bool)
      "API and Provider_intf identity"
      true
      (Binding_identity.equal api_binding (binding "provider_intf" provider_attribution));
    Alcotest.(check bool)
      "API and streaming identity"
      true
      (Binding_identity.equal api_binding (binding "streaming" stream_attribution)))
;;

(* ── capabilities type identity ──────────────────────────── *)

(* Compile-time proof that [Provider.capabilities] is the SAME type as
   [Llm_provider.Capabilities.capabilities] — provider.mli exposes the type
   equation. If that equation is dropped, these identity coercions stop
   compiling, which is exactly what forced downstream consumers (catalog
   overlays) to hand-copy every field. *)
let capabilities_as_provider (c : Llm_provider.Capabilities.capabilities)
  : Provider.capabilities
  =
  c
;;

let capabilities_as_source (c : Provider.capabilities)
  : Llm_provider.Capabilities.capabilities
  =
  c
;;

let test_capabilities_type_equality () =
  let c = Provider.default_capabilities in
  (* Round-trips through both directions with no conversion: the values are
     physically identical because the two names denote one type. *)
  let c' = capabilities_as_provider (capabilities_as_source c) in
  Alcotest.(check bool) "capabilities round-trips by identity" true (c == c')
;;

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Provider_intf"
    [ ( "of_config"
      , [ Alcotest.test_case "local satisfies PROVIDER" `Quick test_of_config_local
        ; Alcotest.test_case "openai satisfies PROVIDER" `Quick test_of_config_openai
        ; Alcotest.test_case
            "propagates resolve errors"
            `Quick
            test_of_config_propagates_resolve_error
        ] )
    ; ( "streaming"
      , [ Alcotest.test_case
            "anthropic supports streaming"
            `Quick
            test_anthropic_supports_streaming
        ; Alcotest.test_case "of_config_streaming" `Quick test_streaming_provider_some
        ; Alcotest.test_case
            "resolve before capability projection"
            `Quick
            test_streaming_resolves_before_capability_projection
        ] )
    ; ( "http_dispatch"
      , [ Alcotest.test_case
            "uses hardened post_sync headers"
            `Quick
            test_provider_dispatch_uses_http_client
        ; Alcotest.test_case
            "maps server error"
            `Quick
            test_provider_dispatch_maps_server_error
        ; Alcotest.test_case
            "rejects malformed response"
            `Quick
            test_provider_dispatch_rejects_malformed_openai_response
        ; Alcotest.test_case
            "preserves provider error projection"
            `Quick
            test_provider_dispatch_preserves_provider_error_projection
        ; Alcotest.test_case
            "preserves JSON parse error projection"
            `Quick
            test_provider_dispatch_preserves_json_parse_error_projection
        ; Alcotest.test_case
            "preserves JSON undefined projection"
            `Quick
            test_provider_dispatch_preserves_json_undefined_projection
        ; Alcotest.test_case
            "empty OpenAI maps to provider unavailable"
            `Quick
            test_provider_dispatch_empty_openai_maps_to_unavailable
        ; Alcotest.test_case
            "custom empty maps to provider unavailable"
            `Quick
            test_custom_provider_empty_maps_to_unavailable
        ; Alcotest.test_case
            "custom provider dispatch"
            `Quick
            test_custom_provider_dispatch_uses_registered_impl
        ; Alcotest.test_case
            "detailed API/provider/stream boundary"
            `Quick
            test_detailed_api_provider_intf_and_streaming_share_boundary
        ] )
    ; ( "capabilities_identity"
      , [ Alcotest.test_case
            "Provider.capabilities = Llm_provider.Capabilities.capabilities"
            `Quick
            test_capabilities_type_equality
        ] )
    ]
;;
