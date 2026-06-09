(** Provider_intf tests — module type satisfaction and dispatch. *)

open Agent_sdk
open Agent_sdk.Types
module Retry = Llm_provider.Retry

(* ── Module type satisfaction ────────────────────────────── *)

let test_of_config_provider_a () =
  let config = Provider.anthropic_sonnet () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
  (* Module was constructed — type check passed at compile time.
     We can't call create_message without a real network, but the
     module satisfying PROVIDER is the key guarantee. *)
  ignore (module P : Provider_intf.PROVIDER)
;;

let test_of_config_provider_d () =
  let config = Provider.openrouter ~model_id:"model-d-4" () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
  ignore (module P : Provider_intf.PROVIDER)
;;

(* ── supports_streaming ──────────────────────────────────── *)

let test_provider_a_supports_streaming () =
  let config = Provider.anthropic_sonnet () in
  Alcotest.(check bool) "anthropic streams" true (Provider_intf.supports_streaming config)
;;

(* ── of_config_streaming ─────────────────────────────────── *)

let test_streaming_provider_some () =
  let config = Provider.anthropic_sonnet () in
  match Provider_intf.of_config_streaming config with
  | Some (module SP : Provider_intf.STREAMING_PROVIDER) ->
    ignore (module SP : Provider_intf.STREAMING_PROVIDER)
  | None -> Alcotest.fail "expected Some for anthropic"
;;

(* ── HTTP dispatch ───────────────────────────────────────── *)

let provider_d_response =
  {|{"id":"chatcmpl-provider-intf","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":2}}|}
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
    { default_config with
      model = provider.model_id
    ; system_prompt = Some "reply briefly"
    ; max_turns = 1
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
    Cohttp_eio.Server.respond_string ~status:`OK ~body:provider_d_response ()
  in
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config provider in
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
    let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config provider in
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

let test_provider_dispatch_rejects_malformed_provider_d_response () =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`OK ~body:{|{"choices":"not-a-list"}|} ()
  in
  with_mock_server handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config provider in
    match
      P.create_message
        ~sw
        ~net
        ~config:(state_for_provider provider)
        ~messages:user_messages
        ()
    with
    | Error (Error.Api (Retry.InvalidRequest { message })) ->
      Alcotest.(check bool) "parse message present" true (String.length message > 0)
    | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
    | Ok _ -> Alcotest.fail "expected malformed response rejection")
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
     | None -> ()
     | Some _ -> Alcotest.fail "custom provider should not expose streaming");
    let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config provider in
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

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Provider_intf"
    [ ( "of_config"
      , [ Alcotest.test_case
            "anthropic satisfies PROVIDER"
            `Quick
            test_of_config_provider_a
        ; Alcotest.test_case "openai satisfies PROVIDER" `Quick test_of_config_provider_d
        ] )
    ; ( "streaming"
      , [ Alcotest.test_case
            "anthropic supports streaming"
            `Quick
            test_provider_a_supports_streaming
        ; Alcotest.test_case "of_config_streaming" `Quick test_streaming_provider_some
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
            test_provider_dispatch_rejects_malformed_provider_d_response
        ; Alcotest.test_case
            "custom provider dispatch"
            `Quick
            test_custom_provider_dispatch_uses_registered_impl
        ] )
    ]
;;
