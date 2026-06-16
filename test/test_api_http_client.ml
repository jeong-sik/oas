open Agent_sdk
open Types

let openai_response =
  {|{"id":"chatcmpl-legacy-api","object":"chat.completion","model":"mock","choices":[{"index":0,"message":{"role":"assistant","content":"ok"},"finish_reason":"stop"}],"usage":{"prompt_tokens":1,"completion_tokens":2}}|}
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
    f ~sw ~net:env#net ~clock:env#clock ~base_url;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
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

let () =
  Alcotest.run
    "Api_http_client"
    [ ( "legacy_create_message"
      , [ Alcotest.test_case
            "uses hardened post_sync headers"
            `Quick
            test_create_message_uses_hardened_http_client
        ] )
    ]
;;
