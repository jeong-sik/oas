open Alcotest
open Llm_provider
open Types
module Count = Input_token_count

let msg role content : message =
  { role; content; name = None; tool_call_id = None; metadata = [] }
;;

let tool =
  `Assoc
    [ "name", `String "inspect"
    ; "description", `String "Inspect one artifact"
    ; ( "input_schema"
      , `Assoc
          [ "type", `String "object"
          ; "properties", `Assoc [ "path", `Assoc [ "type", `String "string" ] ]
          ] )
    ]
;;

let messages =
  [ msg
      User
      [ Text "Inspect this image"
      ; Image { media_type = "image/png"; data = "AAAA"; source_type = Base64 }
      ]
  ; msg
      Assistant
      [ ToolUse
          { id = "tool-1"
          ; name = "inspect"
          ; input = `Assoc [ "path", `String "artifact.png" ]
          }
      ]
  ; msg
      User
      [ ToolResult
          { tool_use_id = "tool-1"
          ; content = "inspection complete"
          ; outcome = Tool_succeeded
          ; json = None
          ; content_blocks =
              Some
                [ Text "inspection complete"
                ; Image { media_type = "image/png"; data = "BBBB"; source_type = Base64 }
                ]
          }
      ]
  ]
;;

let config
      ?(kind = Provider_config.Anthropic)
      ?(request_path = "/proxy/messages")
      base_url
  =
  Provider_config.make
    ~kind
    ~model_id:"input-count-fixture"
    ~base_url
    ~api_key:"test-key"
    ~headers:[ "Content-Type", "application/json"; "anthropic-version", "2023-06-01" ]
    ~request_path
    ~max_tokens:64
    ~temperature:0.2
    ~top_p:0.8
    ~top_k:40
    ~system_prompt:"Count the exact projected input."
    ~cache_system_prompt:true
    ~tool_choice:Any
    ~disable_parallel_tool_use:true
    ~supports_tool_choice_override:true
    ~output_schema:(`Assoc [ "type", `String "object" ])
    ()
;;

let assoc body =
  match Yojson.Safe.from_string body with
  | `Assoc fields -> fields
  | _ -> fail "request body must be an object"
;;

let field_json name fields =
  match List.assoc_opt name fields with
  | Some json -> Yojson.Safe.to_string json
  | None -> fail ("missing request field: " ^ name)
;;

let test_shared_projection () =
  let cfg = config "https://api.anthropic.com" in
  let completion =
    Backend_anthropic.build_request ~config:cfg ~messages ~tools:[ tool ] () |> assoc
  in
  let count =
    Backend_anthropic.build_count_tokens_request ~config:cfg ~messages ~tools:[ tool ] ()
    |> assoc
  in
  List.iter
    (fun name ->
       check
         string
         ("shared field " ^ name)
         (field_json name completion)
         (field_json name count))
    [ "model"; "messages"; "system"; "tools"; "tool_choice"; "output_config" ];
  List.iter
    (fun name -> check bool ("count omits " ^ name) false (List.mem_assoc name count))
    [ "max_tokens"; "stream"; "temperature"; "top_p"; "top_k" ]
;;

let fresh_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> fail "loopback socket did not expose a TCP port"
  in
  Unix.close socket;
  port
;;

let with_mock ~status ~response f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let port = fresh_port () in
  let captured, resolve_captured = Eio.Promise.create () in
  let handler _conn request body =
    let body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    Eio.Promise.resolve
      resolve_captured
      (Cohttp.Request.uri request |> Uri.path, Cohttp.Request.headers request, body);
    Cohttp_eio.Server.respond_string ~status ~body:response ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:4
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  let result = f ~sw ~net ~base_url in
  result, Eio.Promise.await captured
;;

let test_transport_success () =
  let result, (path, headers, body) =
    with_mock ~status:`OK ~response:{|{"input_tokens":321}|}
    @@ fun ~sw ~net ~base_url ->
    Count_tokens_sync.count_anthropic
      ~sw
      ~net
      ~config:(config base_url)
      ~messages
      ~tools:[ tool ]
      ()
  in
  (match result with
   | Ok count ->
     check int "input tokens" 321 count.input_tokens;
     check string "model id" "input-count-fixture" count.model_id;
     check
       bool
       "protocol"
       true
       (Count.equal_protocol count.protocol Count.Anthropic_messages_count_tokens)
   | Error _ -> fail "expected native Anthropic count success");
  check string "custom proxy path" "/proxy/messages/count_tokens" path;
  let check_header name value =
    check (option string) name (Some value) (Cohttp.Header.get headers name)
  in
  check_header "x-api-key" "test-key";
  check_header "anthropic-version" "2023-06-01";
  check
    string
    "canonical request body"
    (Backend_anthropic.build_count_tokens_request
       ~config:(config "unused")
       ~messages
       ~tools:[ tool ]
       ())
    body
;;

let test_transport_error () =
  let result, _captured =
    with_mock ~status:`Too_many_requests ~response:"rate limited"
    @@ fun ~sw ~net ~base_url ->
    Count_tokens_sync.count_anthropic ~sw ~net ~config:(config base_url) ~messages ()
  in
  match result with
  | Error (Count.Transport (Http_client.HttpError { code = 429; body; _ })) ->
    check string "provider body" "rate limited" body
  | Ok _ | Error _ -> fail "expected typed HTTP 429"
;;

let test_unsupported () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  match
    Count_tokens_sync.count_anthropic
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~config:(config ~kind:Provider_config.OpenAI_compat "not a URL")
      ~messages
      ()
  with
  | Error
      (Count.Unsupported { protocol = Count.Anthropic_messages_count_tokens; model_id })
    -> check string "model id" "input-count-fixture" model_id
  | Ok _ | Error _ -> fail "expected typed Unsupported before transport"
;;

let test_count_tokens_url () =
  check
    string
    "plain path"
    "https://api.anthropic.com/proxy/messages/count_tokens"
    (Count_tokens_sync.count_tokens_url (config "https://api.anthropic.com"));
  check
    string
    "query string preserved after inserted segment"
    "https://proxy.example/proxy/messages/count_tokens?api-version=2024-06"
    (Count_tokens_sync.count_tokens_url
       (config
          ~request_path:"/proxy/messages?api-version=2024-06"
          "https://proxy.example"))
;;

let () =
  run
    "anthropic-input-token-count"
    [ "request", [ test_case "shared canonical projection" `Quick test_shared_projection ]
    ; ( "transport"
      , [ test_case "native success" `Quick test_transport_success
        ; test_case "typed HTTP error" `Quick test_transport_error
        ; test_case "non-Anthropic unsupported" `Quick test_unsupported
        ; test_case "count-tokens URL insertion" `Quick test_count_tokens_url
        ] )
    ]
;;
