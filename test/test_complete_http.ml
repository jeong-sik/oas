(** HTTP-level tests for Complete module using mock cohttp-eio server.
    Tests complete, complete_with_retry, complete_stream.
    No real LLM calls — all responses are canned JSON. *)

open Alcotest
open Llm_provider

(* ── Mock server ─────────────────────────────────────── *)

let anthropic_response ?(id = "msg-1") ?(model = "mock")
    ?(stop_reason = "end_turn") text =
  Printf.sprintf
    {|{"id":"%s","type":"message","role":"assistant","model":"%s","content":[{"type":"text","text":"%s"}],"stop_reason":"%s","usage":{"input_tokens":10,"output_tokens":5,"cache_creation_input_tokens":0,"cache_read_input_tokens":0}}|}
    id model text stop_reason

let openai_response text =
  Printf.sprintf
    {|{"id":"chatcmpl-1","object":"chat.completion","model":"gpt-4","choices":[{"index":0,"message":{"role":"assistant","content":"%s"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5}}|}
    text

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port = match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "not inet" in
  Unix.close s;
  port

let start_mock_server ~sw ~net ?(status = `OK) ?(delay_sec = 0.0)
    ?clock response_body =
  let port = fresh_port () in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    (match clock with
     | Some clk when delay_sec > 0.0 -> Eio.Time.sleep clk delay_sec
     | _ -> ());
    Cohttp_eio.Server.respond_string ~status ~body:response_body ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

(* ── Helper: make Provider_config ────────────────────── *)

let make_config ?(kind = Provider_config.Anthropic) base_url =
  Provider_config.make ~kind ~model_id:"test-model"
    ~base_url ~request_path:"/v1/messages"
    ~temperature:0.0 ~max_tokens:100 ()

let make_openai_config base_url =
  Provider_config.make ~kind:Provider_config.OpenAI_compat
    ~model_id:"gpt-4" ~base_url
    ~request_path:"/v1/chat/completions"
    ~temperature:0.0 ~max_tokens:100 ()

let messages = [Types.user_msg "hello"]

let mock_transport_response text =
  { Types.id = "transport-response";
    model = "transport-model";
    stop_reason = Types.EndTurn;
    content = [Types.Text text];
    usage = None;
    telemetry = None }

let make_transport response : Llm_transport.t =
  {
    complete_sync = (fun _ ->
      { Llm_transport.response; latency_ms = 7 });
    complete_stream = (fun ~on_event:_ _ -> response);
  }

(* ── complete: success ───────────────────────────────── *)

let test_complete_anthropic_ok () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (anthropic_response "mock response") in
    let config = make_config url in
    (match Complete.complete ~sw ~net:env#net ~config ~messages () with
     | Ok resp ->
       check string "model" "mock" resp.model;
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "text" "mock response" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── complete: HTTP error ────────────────────────────── *)

let test_complete_http_error () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        ~status:`Bad_request "bad request body" in
    let config = make_config url in
    (match Complete.complete ~sw ~net:env#net ~config ~messages () with
     | Ok _ -> fail "expected Error"
     | Error (Http_client.HttpError { code; _ }) ->
       check int "status 400" 400 code;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected HttpError")
  with Exit -> ()

(* ── complete: OpenAI compat ─────────────────────────── *)

let test_complete_openai_ok () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (openai_response "openai reply") in
    let config = make_openai_config url in
    (match Complete.complete ~sw ~net:env#net ~config ~messages () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "text" "openai reply" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok for openai")
  with Exit -> ()

(* ── complete with cache ─────────────────────────────── *)

let test_complete_cache_store_and_hit () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (anthropic_response "cached") in
    let config = make_config url in
    let store : (string, Yojson.Safe.t) Hashtbl.t = Hashtbl.create 4 in
    let cache : Cache.t = {
      get = (fun ~key -> Hashtbl.find_opt store key);
      set = (fun ~key ~ttl_sec:_ value -> Hashtbl.replace store key value);
    } in
    (* First call — cache miss, HTTP hit *)
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~cache () with
     | Ok _ ->
       check bool "stored in cache" true (Hashtbl.length store > 0)
     | Error _ -> fail "expected Ok first call");
    (* Second call — cache hit, no HTTP *)
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~cache () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "from cache" "cached" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok second call")
  with Exit -> ()

(* ── complete with metrics ───────────────────────────── *)

let test_complete_metrics () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (anthropic_response "metrics test") in
    let config = make_config url in
    let hit_count = ref 0 in
    let miss_count = ref 0 in
    let start_count = ref 0 in
    let end_count = ref 0 in
    let status_calls = ref [] in
    let metrics : Metrics.t = {
      on_cache_hit = (fun ~model_id:_ -> incr hit_count);
      on_cache_miss = (fun ~model_id:_ -> incr miss_count);
      on_request_start = (fun ~model_id:_ -> incr start_count);
      on_request_end = (fun ~model_id:_ ~latency_ms:_ -> incr end_count);
      on_error = (fun ~model_id:_ ~error:_ -> ());
      on_provider_fallback = (fun ~from_model:_ ~to_model:_ ~reason:_ -> ());
      on_http_status = (fun ~provider ~model_id ~status ->
        status_calls := (provider, model_id, status) :: !status_calls);
    } in
    (* No cache provided → on_cache_miss not called *)
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
     | Ok _ ->
       check int "no cache = no miss" 0 !miss_count;
       check int "start" 1 !start_count;
       check int "end" 1 !end_count;
       check int "no hit" 0 !hit_count;
       (* on_http_status fired once with the actual 200 code *)
       check int "status callback count" 1 (List.length !status_calls);
       (match !status_calls with
        | [(_, _, code)] -> check int "status code" 200 code
        | _ -> fail "expected exactly one status call");
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── complete_with_retry: success first try ──────────── *)

let test_retry_first_try () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (anthropic_response "first try ok") in
    let config = make_config url in
    (match Complete.complete_with_retry ~sw ~net:env#net ~clock
             ~config ~messages () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "text" "first try ok" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── complete: 401 non-retryable ─────────────────────── *)

let test_complete_non_retryable () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        ~status:`Unauthorized "unauthorized" in
    let config = make_config url in
    (match Complete.complete ~sw ~net:env#net ~config ~messages () with
     | Ok _ -> fail "expected Error"
     | Error (Http_client.HttpError { code; _ }) ->
       check int "401" 401 code;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected HttpError")
  with Exit -> ()

(* ── complete: error metrics ─────────────────────────── *)

let test_complete_error_metrics () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        ~status:`Bad_request "bad" in
    let config = make_config url in
    let error_count = ref 0 in
    let status_calls = ref [] in
    let metrics : Metrics.t = {
      on_cache_hit = (fun ~model_id:_ -> ());
      on_cache_miss = (fun ~model_id:_ -> ());
      on_request_start = (fun ~model_id:_ -> ());
      on_request_end = (fun ~model_id:_ ~latency_ms:_ -> ());
      on_error = (fun ~model_id:_ ~error:_ -> incr error_count);
      on_provider_fallback = (fun ~from_model:_ ~to_model:_ ~reason:_ -> ());
      on_http_status = (fun ~provider ~model_id ~status ->
        status_calls := (provider, model_id, status) :: !status_calls);
    } in
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
     | Ok _ -> fail "expected Error"
     | Error _ ->
       check int "error callback" 1 !error_count;
       (* 400 HTTP response must also emit on_http_status before error fires *)
       check int "status callback count" 1 (List.length !status_calls);
       (match !status_calls with
        | [(_, _, 400)] -> ()
        | [(_, _, code)] -> fail (Printf.sprintf "expected 400, got %d" code)
        | _ -> fail "expected exactly one status call");
       Eio.Switch.fail sw Exit)
  with Exit -> ()

let test_complete_transport_http_metrics_ok () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let config = make_openai_config "http://unused.test" in
    let status_calls = ref [] in
    let metrics : Metrics.t = {
      Metrics.noop with
      on_http_status = (fun ~provider ~model_id ~status ->
        status_calls := (provider, model_id, status) :: !status_calls);
    } in
    let transport = make_transport (Ok (mock_transport_response "transport ok")) in
    (match Complete.complete ~sw ~net:env#net ~transport ~config ~messages ~metrics () with
     | Ok _ ->
       (match !status_calls with
        | [("openai", "gpt-4", 200)] ->
          Eio.Switch.fail sw Exit
        | [(_, _, code)] -> fail (Printf.sprintf "expected 200, got %d" code)
        | _ -> fail "expected exactly one transport status call")
     | Error _ -> fail "expected Ok")
  with Exit -> ()

let test_complete_transport_http_metrics_error () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let config = make_openai_config "http://unused.test" in
    let status_calls = ref [] in
    let metrics : Metrics.t = {
      Metrics.noop with
      on_http_status = (fun ~provider ~model_id ~status ->
        status_calls := (provider, model_id, status) :: !status_calls);
    } in
    let transport =
      make_transport (Error (Http_client.HttpError { code = 429; body = "rate limited" }))
    in
    (match Complete.complete ~sw ~net:env#net ~transport ~config ~messages ~metrics () with
     | Ok _ -> fail "expected Error"
     | Error (Http_client.HttpError { code; _ }) ->
       check int "status 429" 429 code;
       (match !status_calls with
        | [("openai", "gpt-4", 429)] ->
          Eio.Switch.fail sw Exit
        | [(_, _, seen)] -> fail (Printf.sprintf "expected 429, got %d" seen)
        | _ -> fail "expected exactly one transport status call")
     | Error _ -> fail "expected HttpError")
  with Exit -> ()

let test_complete_transport_cli_does_not_emit_status () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let config = Provider_config.make
        ~kind:Provider_config.Codex_cli
        ~model_id:"codex-mini"
        ~base_url:"" () in
    let hits = ref 0 in
    let metrics : Metrics.t = {
      Metrics.noop with
      on_http_status = (fun ~provider:_ ~model_id:_ ~status:_ -> incr hits);
    } in
    let transport = make_transport (Ok (mock_transport_response "cli ok")) in
    (match Complete.complete ~sw ~net:env#net ~transport ~config ~messages ~metrics () with
     | Ok _ ->
       check int "cli transport emits no status" 0 !hits;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── Global metrics registry ──────────────────────────── *)

let test_metrics_global_default_is_noop () =
  let g = Metrics.get_global () in
  (* Default state: the noop callbacks should not raise and should be
     distinguishable by reference from a custom instance below. *)
  g.on_cache_hit ~model_id:"m";
  g.on_request_end ~model_id:"m" ~latency_ms:1;
  g.on_http_status ~provider:"ollama" ~model_id:"m" ~status:200;
  (* No side effects observable. *)
  check bool "default global accepts noop calls" true true

let test_metrics_global_set_and_get () =
  let hits = ref 0 in
  let previous = Metrics.get_global () in
  (* Fun.protect guarantees the global is restored even if a check
     assertion raises inside the body, preventing cross-test pollution
     through the shared process-wide sink. *)
  Fun.protect ~finally:(fun () -> Metrics.set_global previous)
    (fun () ->
      let custom : Metrics.t = {
        Metrics.noop with
        on_http_status = (fun ~provider:_ ~model_id:_ ~status:_ -> incr hits);
      } in
      Metrics.set_global custom;
      let g = Metrics.get_global () in
      g.on_http_status ~provider:"ollama" ~model_id:"m" ~status:429;
      g.on_http_status ~provider:"glm" ~model_id:"m" ~status:429;
      check int "global metric fired twice" 2 !hits)

let test_metrics_global_used_when_no_per_call_metrics () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (anthropic_response "global metrics test") in
    let config = make_config url in
    let status_calls = ref [] in
    let previous = Metrics.get_global () in
    let bridge : Metrics.t = {
      Metrics.noop with
      on_http_status = (fun ~provider ~model_id ~status ->
        status_calls := (provider, model_id, status) :: !status_calls);
    } in
    Metrics.set_global bridge;
    Fun.protect ~finally:(fun () -> Metrics.set_global previous)
      (fun () ->
        (* Deliberately do NOT pass ~metrics — global should take effect. *)
        match Complete.complete ~sw ~net:env#net ~config ~messages () with
        | Ok _ ->
          check int "global on_http_status fired once" 1 (List.length !status_calls);
          Eio.Switch.fail sw Exit
        | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── complete_stream: SSE ─────────────────────────────── *)

let anthropic_sse_response text =
  Printf.sprintf
    "event: message_start\ndata: {\"type\":\"message_start\",\"message\":{\"id\":\"msg-1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"mock\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0}}}\n\nevent: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\nevent: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"%s\"}}\n\nevent: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\nevent: message_delta\ndata: {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\nevent: message_stop\ndata: {\"type\":\"message_stop\"}\n\n"
    text

let start_sse_server ~sw ~net response_body =
  let port = fresh_port () in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let headers = Cohttp.Header.of_list [("content-type", "text/event-stream")] in
    Cohttp_eio.Server.respond_string
      ~status:`OK ~headers ~body:response_body ()
  in
  let socket =
    Eio.Net.listen net ~sw ~backlog:8 ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port

let test_complete_stream_ok () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_sse_server ~sw ~net:env#net
        (anthropic_sse_response "streamed text") in
    let config = make_config url in
    let events = ref [] in
    let on_event evt = events := evt :: !events in
    (match Complete.complete_stream ~sw ~net:env#net ~config
             ~messages ~on_event () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "text" "streamed text" text;
       check bool "events received" true (List.length !events > 0);
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "complete_http" [
    "complete", [
      test_case "anthropic ok" `Quick test_complete_anthropic_ok;
      test_case "http error" `Quick test_complete_http_error;
      test_case "openai ok" `Quick test_complete_openai_ok;
      test_case "non-retryable" `Quick test_complete_non_retryable;
    ];
    "cache", [
      test_case "store and hit" `Quick test_complete_cache_store_and_hit;
    ];
    "metrics", [
      test_case "callbacks" `Quick test_complete_metrics;
      test_case "error callback" `Quick test_complete_error_metrics;
      test_case "transport http ok" `Quick test_complete_transport_http_metrics_ok;
      test_case "transport http error" `Quick test_complete_transport_http_metrics_error;
      test_case "transport cli stays silent" `Quick
        test_complete_transport_cli_does_not_emit_status;
      test_case "global default is noop" `Quick test_metrics_global_default_is_noop;
      test_case "global set and get" `Quick test_metrics_global_set_and_get;
      test_case "global used when no per-call metrics" `Quick
        test_metrics_global_used_when_no_per_call_metrics;
    ];
    "retry", [
      test_case "first try ok" `Quick test_retry_first_try;
    ];
    "stream", [
      test_case "sse ok" `Quick test_complete_stream_ok;
    ];
  ]
