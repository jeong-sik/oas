(** Provider_intf tests — module type satisfaction and dispatch. *)

open Agent_sdk
open Agent_sdk.Types

(* ── Module type satisfaction ────────────────────────────── *)

let test_of_config_anthropic () =
  let config = Provider.anthropic_sonnet () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
  (* Module was constructed — type check passed at compile time.
     We can't call create_message without a real network, but the
     module satisfying PROVIDER is the key guarantee. *)
  ignore (module P : Provider_intf.PROVIDER)
;;

let test_of_config_openai () =
  let config = Provider.openrouter ~model_id:"gpt-4" () in
  let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config config in
  ignore (module P : Provider_intf.PROVIDER)
;;

(* ── supports_streaming ──────────────────────────────────── *)

let test_anthropic_supports_streaming () =
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

let openai_response =
  {|{"id":"chatcmpl-provider-intf","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":2}}|}
;;

let with_mock_server ~port handler f =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
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
    Cohttp_eio.Server.respond_string ~status:`OK ~body:openai_response ()
  in
  with_mock_server ~port:18342 handler (fun ~sw ~net ~base_url ->
    let provider : Provider.config =
      { provider = Local { base_url }; model_id = "mock"; api_key_env = "DUMMY_KEY" }
    in
    let (module P : Provider_intf.PROVIDER) = Provider_intf.of_config provider in
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
    match P.create_message ~sw ~net ~config:state ~messages () with
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

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Provider_intf"
    [ ( "of_config"
      , [ Alcotest.test_case
            "anthropic satisfies PROVIDER"
            `Quick
            test_of_config_anthropic
        ; Alcotest.test_case "openai satisfies PROVIDER" `Quick test_of_config_openai
        ] )
    ; ( "streaming"
      , [ Alcotest.test_case
            "anthropic supports streaming"
            `Quick
            test_anthropic_supports_streaming
        ; Alcotest.test_case "of_config_streaming" `Quick test_streaming_provider_some
        ] )
    ; ( "http_dispatch"
      , [ Alcotest.test_case
            "uses hardened post_sync headers"
            `Quick
            test_provider_dispatch_uses_http_client
        ] )
    ]
;;
