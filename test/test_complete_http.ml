(** HTTP-level tests for Complete module using mock cohttp-eio server.
    Tests complete, complete_with_retry, complete_cascade, complete_stream.
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

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop idx =
    if needle_len = 0 then
      true
    else if idx + needle_len > haystack_len then
      false
    else if String.sub haystack idx needle_len = needle then
      true
    else
      loop (idx + 1)
  in
  loop 0

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
    let metrics : Metrics.t = {
      on_cache_hit = (fun ~model_id:_ -> incr hit_count);
      on_cache_miss = (fun ~model_id:_ -> incr miss_count);
      on_request_start = (fun ~model_id:_ -> incr start_count);
      on_request_end = (fun ~model_id:_ ~latency_ms:_ -> incr end_count);
      on_error = (fun ~model_id:_ ~error:_ -> ());
      on_cascade_fallback = (fun ~from_model:_ ~to_model:_ ~reason:_ -> ());
    } in
    (* No cache provided → on_cache_miss not called *)
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
     | Ok _ ->
       check int "no cache = no miss" 0 !miss_count;
       check int "start" 1 !start_count;
       check int "end" 1 !end_count;
       check int "no hit" 0 !hit_count;
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

(* ── complete_cascade: primary success ───────────────── *)

let test_cascade_primary_ok () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (anthropic_response "primary ok") in
    let primary = make_config url in
    let cascade : Complete.cascade = { primary; fallbacks = [] } in
    (match Complete.complete_cascade ~sw ~net:env#net
             ~cascade ~messages () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "primary" "primary ok" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── complete_cascade: fallback ──────────────────────── *)

let test_cascade_fallback () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    (* Primary: returns 500 (retryable) *)
    let bad_url = start_mock_server ~sw ~net:env#net
        ~status:`Internal_server_error "fail" in
    (* Fallback: returns 200 *)
    let good_url = start_mock_server ~sw ~net:env#net
        (anthropic_response "fallback ok") in
    let primary = make_config bad_url in
    let fallback = make_config good_url in
    let cascade : Complete.cascade = { primary; fallbacks = [fallback] } in
    (match Complete.complete_cascade ~sw ~net:env#net
             ~cascade ~messages () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "fallback" "fallback ok" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok from fallback")
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
    let metrics : Metrics.t = {
      on_cache_hit = (fun ~model_id:_ -> ());
      on_cache_miss = (fun ~model_id:_ -> ());
      on_request_start = (fun ~model_id:_ -> ());
      on_request_end = (fun ~model_id:_ ~latency_ms:_ -> ());
      on_error = (fun ~model_id:_ ~error:_ -> incr error_count);
      on_cascade_fallback = (fun ~from_model:_ ~to_model:_ ~reason:_ -> ());
    } in
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
     | Ok _ -> fail "expected Error"
     | Error _ ->
       check int "error callback" 1 !error_count;
       Eio.Switch.fail sw Exit)
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

(* ── complete_stream_cascade ─────────────────────────── *)

let test_stream_cascade_ok () =
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_sse_server ~sw ~net:env#net
        (anthropic_sse_response "cascade stream") in
    let primary = make_config url in
    let cascade : Complete.cascade = { primary; fallbacks = [] } in
    let events = ref [] in
    let on_event evt = events := evt :: !events in
    (match Complete.complete_stream_cascade ~sw ~net:env#net
             ~cascade ~messages ~on_event () with
     | Ok resp ->
       let text = List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content |> String.concat "" in
       check string "text" "cascade stream" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok")
  with Exit -> ()

(* ── cascade_config.complete_named ───────────────────── *)

let test_complete_named_llama () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (openai_response "named cascade") in
    (* custom: provider uses OpenAI compat format *)
    let defaults = [
      Printf.sprintf "custom:qwen@%s" url
    ] in
    (match Cascade_config.complete_named ~sw ~net:env#net ~clock
             ~name:"test" ~defaults
             ~messages ~temperature:0.0 ~max_tokens:100 () with
     | Ok resp ->
       let text = Cascade_config.text_of_response resp in
       check string "text" "named cascade" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok for named cascade")
  with Exit -> ()

(* ── cascade_config.complete_named: no providers ─────── *)

let test_complete_named_no_providers () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let defaults = ["unknown:model"] in
    (match Cascade_config.complete_named ~sw ~net:env#net ~clock
             ~name:"fail" ~defaults
             ~messages () with
     | Ok _ -> fail "expected Error"
     | Error (Http_client.NetworkError { message }) ->
       check bool "mentions cascade" true
         (String.length message > 0);
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected NetworkError")
  with Exit -> ()

(* ── cascade_config.complete_named: timeout ──────────── *)

let test_complete_named_timeout () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        ~delay_sec:5.0 ~clock
        (openai_response "too slow") in
    let defaults = [
      Printf.sprintf "custom:slow@%s" url
    ] in
    (match Cascade_config.complete_named ~sw ~net:env#net ~clock
             ~name:"timeout" ~defaults
             ~messages ~timeout_sec:1 () with
     | Ok _ -> fail "expected timeout"
     | Error (Http_client.NetworkError { message }) ->
       check bool "timeout msg" true
         (String.length message > 0);
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected NetworkError")
  with Exit -> ()

(* ── cascade_config.complete_named: accept validator ─── *)

let test_complete_named_reject () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (openai_response "rejected") in
    let defaults = [
      Printf.sprintf "custom:r@%s" url
    ] in
    (* Reject all responses *)
    let accept_reason _resp = Error "intentional reject from test validator" in
    (match Cascade_config.complete_named ~sw ~net:env#net ~clock
             ~name:"reject" ~defaults
             ~messages ~accept_reason () with
     | Ok _ -> fail "expected rejection"
     | Error (Http_client.NetworkError { message }) ->
       check bool "reason preserved" true
         (message = "intentional reject from test validator");
       check bool "validator reason preserved" true
         (contains_substring
            ~needle:"intentional reject from test validator"
            message);
       Eio.Switch.fail sw Exit
     | Error (Http_client.AcceptRejected { reason }) ->
       check string "reject reason" "intentional reject from test validator"
         reason;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected NetworkError")
  with Exit -> ()

let test_complete_named_reject_legacy_bool_accept () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (openai_response "rejected") in
    let defaults = [
      Printf.sprintf "custom:r@%s" url
    ] in
    let accept _resp = false in
    (match Cascade_config.complete_named ~sw ~net:env#net ~clock
             ~name:"reject-legacy" ~defaults
             ~messages ~accept () with
     | Ok _ -> fail "expected rejection"
     | Error (Http_client.NetworkError { message }) ->
       check bool "generic legacy reason" true
         (contains_substring
            ~needle:"response rejected by accept validator"
            message);
       Eio.Switch.fail sw Exit
     | Error (Http_client.AcceptRejected { reason }) ->
       check bool "legacy reject reason preserved" true
         (contains_substring
            ~needle:"response rejected by accept validator"
            reason);
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected NetworkError")
  with Exit -> ()

(* ── cascade_config.complete_named: config file ──────── *)

let test_complete_named_from_config () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net
        (openai_response "from config") in
    (* Write a temp config file *)
    let config_path = Filename.temp_file "oas_test_" ".json" in
    let oc = open_out config_path in
    Printf.fprintf oc
      {|{"mytest_models": ["custom:m@%s"]}|} url;
    close_out oc;
    Fun.protect
      ~finally:(fun () -> Sys.remove config_path)
      (fun () ->
        match Cascade_config.complete_named ~sw ~net:env#net ~clock
                ~config_path ~name:"mytest"
                ~defaults:["llama:qwen"]
                ~messages () with
        | Ok resp ->
          let text = Cascade_config.text_of_response resp in
          check string "text" "from config" text;
          Eio.Switch.fail sw Exit
        | Error _ -> fail "expected Ok from config")
  with Exit -> ()

(* ── cascade_config.complete_named_stream ─────────────── *)

let openai_sse_response text =
  Printf.sprintf
    "data: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"model\":\"mock\",\"choices\":[{\"index\":0,\"delta\":{\"role\":\"assistant\",\"content\":\"%s\"},\"finish_reason\":null}]}\n\ndata: {\"id\":\"chatcmpl-1\",\"object\":\"chat.completion.chunk\",\"model\":\"mock\",\"choices\":[{\"index\":0,\"delta\":{},\"finish_reason\":\"stop\"}],\"usage\":{\"prompt_tokens\":10,\"completion_tokens\":5}}\n\ndata: [DONE]\n\n"
    text

let test_complete_named_stream_ok () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let url = start_sse_server ~sw ~net:env#net
        (openai_sse_response "named stream") in
    let defaults = [
      Printf.sprintf "custom:s@%s" url
    ] in
    let events = ref [] in
    (match Cascade_config.complete_named_stream ~sw ~net:env#net ~clock
             ~name:"stream" ~defaults
             ~messages ~on_event:(fun e -> events := e :: !events) () with
     | Ok resp ->
       let text = Cascade_config.text_of_response resp in
       check string "text" "named stream" text;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected Ok for named stream")
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
    ];
    "retry", [
      test_case "first try ok" `Quick test_retry_first_try;
    ];
    "cascade", [
      test_case "primary ok" `Quick test_cascade_primary_ok;
      test_case "fallback" `Quick test_cascade_fallback;
    ];
    "stream", [
      test_case "sse ok" `Quick test_complete_stream_ok;
      test_case "cascade" `Quick test_stream_cascade_ok;
    ];
    "named_cascade", [
      test_case "llama" `Quick test_complete_named_llama;
      test_case "no providers" `Quick test_complete_named_no_providers;
      test_case "timeout" `Quick test_complete_named_timeout;
      test_case "reject" `Quick test_complete_named_reject;
      test_case "reject legacy bool accept" `Quick
        test_complete_named_reject_legacy_bool_accept;
      test_case "from config" `Quick test_complete_named_from_config;
      test_case "stream" `Quick test_complete_named_stream_ok;
    ];
  ]
