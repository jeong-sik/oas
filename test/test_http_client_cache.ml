(** Connection cache tests for Http_client.

    These tests exercise the reusable TCP/TLS connection cache through both
    the high-level {!Complete} API and direct {!Http_client} calls. *)

open Agent_sdk
open Llm_provider

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "not inet"
  in
  Unix.close s;
  port
;;

let start_mock_server ~sw ~net ?(status = `OK) ?capture_headers response_body =
  let port = fresh_port () in
  let handler _conn req body =
    let request_body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    (match capture_headers with
     | Some seen -> seen := Cohttp.Request.headers req
     | None -> ());
    ignore request_body;
    Cohttp_eio.Server.respond_string ~status ~body:response_body ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let anthropic_response text =
  Printf.sprintf
    {|{"id":"msg-1","type":"message","role":"assistant","model":"mock","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5,"cache_creation_input_tokens":0,"cache_read_input_tokens":0}}|}
    text
;;

let make_anthropic_config base_url =
  Provider_config.make
    ~kind:Provider_config.Anthropic
    ~model_id:"test-model"
    ~base_url
    ~request_path:"/v1/messages"
    ~temperature:0.0
    ~max_tokens:100
    ()
;;

let test_complete_reuses_connection () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "cached") in
    let config = make_anthropic_config url in
    let cache = Http_client.create_cache ~sw () in
    (match
       Complete.complete ~sw ~net:env#net ~config ~messages:[] ~connection_cache:cache ()
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "expected Ok first call");
    let stats_after_first = Http_client.cache_stats cache in
    Alcotest.(check int) "one create after first" 1 stats_after_first.create_count_total;
    Alcotest.(check int) "no reuse yet" 0 stats_after_first.reuse_count_total;
    Alcotest.(check int) "one idle connection" 1 stats_after_first.total_idle;
    (match
       Complete.complete ~sw ~net:env#net ~config ~messages:[] ~connection_cache:cache ()
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "expected Ok second call");
    let stats_after_second = Http_client.cache_stats cache in
    Alcotest.(check int) "still one create" 1 stats_after_second.create_count_total;
    Alcotest.(check int) "one reuse" 1 stats_after_second.reuse_count_total;
    Alcotest.(check int) "still one idle" 1 stats_after_second.total_idle;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_eviction_fiber_closes_idle () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "cached") in
    let config = make_anthropic_config url in
    let cache = Http_client.create_cache ~sw ~clock:env#clock ~idle_ttl_seconds:0.05 () in
    (match
       Complete.complete ~sw ~net:env#net ~config ~messages:[] ~connection_cache:cache ()
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "expected Ok");
    let stats_after_first = Http_client.cache_stats cache in
    Alcotest.(check int) "one idle before eviction" 1 stats_after_first.total_idle;
    Eio.Time.sleep env#clock 0.2;
    let stats_after_wait = Http_client.cache_stats cache in
    Alcotest.(check int) "idle evicted by fiber" 0 stats_after_wait.total_idle;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_omits_connection_close_header () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let captured = ref (Cohttp.Header.of_list []) in
    let url =
      start_mock_server
        ~sw
        ~net:env#net
        ~capture_headers:captured
        (anthropic_response "cached")
    in
    let config = make_anthropic_config url in
    let cache = Http_client.create_cache ~sw () in
    (match
       Complete.complete ~sw ~net:env#net ~config ~messages:[] ~connection_cache:cache ()
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "expected Ok");
    let conn = Cohttp.Header.get !captured "connection" in
    Alcotest.(check (option string)) "no connection close" None conn;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let anthropic_sse_response text =
  Printf.sprintf
    "event: message_start\n\
     data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"msg-1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"mock\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0}}}\n\n\
     event: content_block_start\n\
     data: \
     {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n\
     event: content_block_delta\n\
     data: \
     {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"%s\"}}\n\n\
     event: content_block_stop\n\
     data: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
     event: message_delta\n\
     data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
     event: message_stop\n\
     data: {\"type\":\"message_stop\"}\n\n"
    text
;;

let start_sse_server ~sw ~net response_body =
  let port = fresh_port () in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let headers = Cohttp.Header.of_list [ "content-type", "text/event-stream" ] in
    Cohttp_eio.Server.respond_string ~status:`OK ~headers ~body:response_body ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let drain_request_headers reader =
  let rec consume () =
    match Eio.Buf_read.line reader with
    | "" -> ()
    | _ -> consume ()
    | exception End_of_file -> ()
  in
  consume ()
;;

let resolve_once promise resolver =
  if not (Eio.Promise.is_resolved promise) then Eio.Promise.resolve resolver ()
;;

let observe_client_eof reader promise resolver =
  let rec consume () =
    match Eio.Buf_read.line reader with
    | _ -> consume ()
    | exception End_of_file -> resolve_once promise resolver
    | exception _ -> resolve_once promise resolver
  in
  consume ()
;;

let start_one_shot_lifecycle_server ~sw ~net ~clock ?body_delay_s body =
  let port = fresh_port () in
  let disconnected, notify_disconnected = Eio.Promise.create () in
  let handler flow _addr =
    let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
    drain_request_headers reader;
    Eio.Fiber.fork ~sw (fun () ->
      observe_client_eof reader disconnected notify_disconnected);
    let response_headers =
      Printf.sprintf
        "HTTP/1.1 200 OK\r\n\
         Content-Length: %d\r\n\
         Content-Type: text/plain\r\n\
         Connection: keep-alive\r\n\
         \r\n"
        (String.length body)
    in
    try
      Eio.Flow.copy_string response_headers flow;
      (match body_delay_s with
       | Some delay_s -> Eio.Time.sleep clock delay_s
       | None -> ());
      Eio.Flow.copy_string body flow
    with
    | _ -> resolve_once disconnected notify_disconnected
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:1
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Net.accept_fork ~sw socket ~on_error:(fun _ -> ()) handler);
  Printf.sprintf "http://127.0.0.1:%d/one-shot" port, disconnected
;;

let await_client_disconnect ~clock ~label disconnected =
  try Eio.Time.with_timeout_exn clock 1.0 (fun () -> Eio.Promise.await disconnected) with
  | Eio.Time.Timeout ->
    Alcotest.failf "%s: one-shot client did not close before parent switch release" label
;;

let test_stream_reuses_connection () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_sse_server ~sw ~net:env#net (anthropic_sse_response "streamed") in
    let config = make_anthropic_config url in
    let cache = Http_client.create_cache ~sw () in
    let got = ref [] in
    (match
       Complete.complete_stream
         ~sw
         ~net:env#net
         ~config
         ~messages:[]
         ~connection_cache:cache
         ~on_event:(function
           | Types.ContentBlockDelta { delta = Types.TextDelta s; _ } -> got := s :: !got
           | _ -> ())
         ()
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "expected Ok stream");
    Alcotest.(check (list string)) "received text" [ "streamed" ] (List.rev !got);
    let stats_after_first = Http_client.cache_stats cache in
    Alcotest.(check int) "one create after stream" 1 stats_after_first.create_count_total;
    Alcotest.(check int) "one idle after stream" 1 stats_after_first.total_idle;
    got := [];
    (match
       Complete.complete_stream
         ~sw
         ~net:env#net
         ~config
         ~messages:[]
         ~connection_cache:cache
         ~on_event:(function
           | Types.ContentBlockDelta { delta = Types.TextDelta s; _ } -> got := s :: !got
           | _ -> ())
         ()
     with
     | Ok _ -> ()
     | Error _ -> Alcotest.fail "expected Ok stream second");
    let stats_after_second = Http_client.cache_stats cache in
    Alcotest.(check int) "one reuse" 1 stats_after_second.reuse_count_total;
    Alcotest.(check int) "still one create" 1 stats_after_second.create_count_total;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_per_host_cap_closes_excess () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "cached") in
    let config = make_anthropic_config url in
    let cache = Http_client.create_cache ~sw ~max_idle_per_host:1 () in
    let once () =
      match
        Complete.complete ~sw ~net:env#net ~config ~messages:[] ~connection_cache:cache ()
      with
      | Ok _ -> ()
      | Error _ -> Alcotest.fail "expected Ok concurrent"
    in
    Eio.Fiber.both once once;
    let stats = Http_client.cache_stats cache in
    Alcotest.(check int) "two creates for concurrent" 2 stats.create_count_total;
    Alcotest.(check int) "only one idle due to cap" 1 stats.total_idle;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_one_shot_get_closes_after_response () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url, disconnected =
      start_one_shot_lifecycle_server ~sw ~net:env#net ~clock:env#clock "ok"
    in
    (match Http_client.get_sync ~sw ~net:env#net ~url ~headers:[] () with
     | Ok (200, "ok") -> ()
     | Ok (code, body) -> Alcotest.failf "expected 200/ok, got %d/%S" code body
     | Error _ -> Alcotest.fail "expected Ok response");
    await_client_disconnect ~clock:env#clock ~label:"normal response" disconnected;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_one_shot_get_timeout_closes_connection () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url, disconnected =
      start_one_shot_lifecycle_server
        ~sw
        ~net:env#net
        ~clock:env#clock
        ~body_delay_s:1.0
        "ok"
    in
    (match
       Http_client.get_sync
         ~clock:env#clock
         ~timeout_s:0.05
         ~sw
         ~net:env#net
         ~url
         ~headers:[]
         ()
     with
     | Error (Http_client.TimeoutError { phase = Http_client.Http_operation; _ }) -> ()
     | Error _ -> Alcotest.fail "expected Http_operation timeout"
     | Ok (code, body) -> Alcotest.failf "expected timeout, got %d/%S" code body);
    await_client_disconnect ~clock:env#clock ~label:"timed-out response" disconnected;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_create_cache_rejects_invalid_params () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let expect_invalid_arg f =
      try
        ignore (f ());
        Alcotest.fail "expected Invalid_argument"
      with
      | Invalid_argument _ -> ()
    in
    expect_invalid_arg (fun () -> Http_client.create_cache ~sw ~max_idle_per_host:0 ());
    expect_invalid_arg (fun () -> Http_client.create_cache ~sw ~max_idle_per_host:(-1) ());
    expect_invalid_arg (fun () -> Http_client.create_cache ~sw ~idle_ttl_seconds:0.0 ());
    expect_invalid_arg (fun () ->
      Http_client.create_cache ~sw ~idle_ttl_seconds:(-1.0) ());
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_stream_cancellation_closes_connection () =
  (* Slow SSE server: cancel the fiber mid-body and assert the cached
     connection is closed, not parked back into the cache. *)
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_sse_server ~sw ~net:env#net (anthropic_sse_response "never") in
    let cache = Http_client.create_cache ~sw () in
    let stream () =
      Http_client.with_post_stream
        ~cache
        ~net:env#net
        ~url
        ~headers:[]
        ~body:""
        ~f:(fun reader ->
          (* Read one line then block forever, simulating a stuck consumer. *)
          ignore (Eio.Buf_read.line reader);
          Eio.Time.sleep env#clock 60.0;
          Ok ())
        ()
    in
    (try ignore (Eio.Time.with_timeout_exn env#clock 0.2 stream) with
     | Eio.Time.Timeout -> ());
    let stats = Http_client.cache_stats cache in
    Alcotest.(check int) "cancelled stream does not park connection" 0 stats.total_idle;
    Alcotest.(check int) "one create" 1 stats.create_count_total;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let () =
  Alcotest.run
    "HTTP Client Connection Cache"
    [ ( "reuse"
      , [ Alcotest.test_case
            "sync connection reused"
            `Quick
            test_complete_reuses_connection
        ; Alcotest.test_case
            "per-host cap closes excess"
            `Quick
            test_per_host_cap_closes_excess
        ] )
    ; ( "lifecycle"
      , [ Alcotest.test_case
            "eviction fiber closes idle"
            `Quick
            test_eviction_fiber_closes_idle
        ; Alcotest.test_case
            "one-shot closes after response"
            `Quick
            test_one_shot_get_closes_after_response
        ; Alcotest.test_case
            "one-shot timeout closes connection"
            `Quick
            test_one_shot_get_timeout_closes_connection
        ] )
    ; ( "headers"
      , [ Alcotest.test_case
            "omits connection close"
            `Quick
            test_omits_connection_close_header
        ] )
    ; ( "stream"
      , [ Alcotest.test_case
            "reuses streaming connection"
            `Quick
            test_stream_reuses_connection
        ; Alcotest.test_case
            "cancellation closes connection"
            `Quick
            test_stream_cancellation_closes_connection
        ] )
    ; ( "validation"
      , [ Alcotest.test_case
            "create_cache rejects invalid params"
            `Quick
            test_create_cache_rejects_invalid_params
        ] )
    ]
;;
