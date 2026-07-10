(** HTTP-level tests for Complete module using mock cohttp-eio server.
    Tests complete, complete_with_retry, complete_stream.
    No real LLM calls — all responses are canned JSON. *)

open Alcotest
open Llm_provider

(* ── Mock server ─────────────────────────────────────── *)

let anthropic_response ?(id = "msg-1") ?(model = "mock") ?(stop_reason = "end_turn") text =
  Printf.sprintf
    {|{"id":"%s","type":"message","role":"assistant","model":"%s","content":[{"type":"text","text":"%s"}],"stop_reason":"%s","usage":{"input_tokens":10,"output_tokens":5,"cache_creation_input_tokens":0,"cache_read_input_tokens":0}}|}
    id
    model
    text
    stop_reason
;;

let anthropic_empty_response stop_reason =
  Printf.sprintf
    {|{"id":"msg-empty","type":"message","role":"assistant","model":"mock","content":[],"stop_reason":"%s","usage":{"input_tokens":10,"output_tokens":0}}|}
    stop_reason
;;

let anthropic_empty_sse stop_reason =
  Printf.sprintf
    "event: message_start\n\
     data: \
     {\"type\":\"message_start\",\"message\":{\"id\":\"msg-empty\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"mock\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0}}}\n\n\
     event: message_delta\n\
     data: \
     {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"%s\"},\"usage\":{\"output_tokens\":0}}\n\n\
     event: message_stop\n\
     data: {\"type\":\"message_stop\"}\n\n"
    stop_reason
;;

let openai_response text =
  Printf.sprintf
    {|{"id":"chatcmpl-1","object":"chat.completion","model":"gpt-4","choices":[{"index":0,"message":{"role":"assistant","content":"%s"},"finish_reason":"stop"}],"usage":{"prompt_tokens":10,"completion_tokens":5}}|}
    text
;;

let openai_mlx_vlm_response text =
  Printf.sprintf
    {|{"id":"chatcmpl-mlx-1","object":"chat.completion","model":"gpt-4","choices":[{"index":0,"message":{"role":"assistant","content":"%s"},"finish_reason":"stop"}],"usage":{"input_tokens":11,"output_tokens":5,"prompt_tps":21.55,"generation_tps":81.56},"peak_memory":52.66}|}
    text
;;

let openai_responses_tool_call_response () =
  {|{"id":"resp-1","model":"gpt-5.5","status":"completed","output":[
      {"id":"rs-1","type":"reasoning","summary":[{"type":"summary_text","text":"Need a lookup."}]},
      {"id":"fc-1","type":"function_call","call_id":"call_lookup","name":"lookup","arguments":"{\"q\":\"weather\"}"}
    ],
    "usage":{"input_tokens":12,"output_tokens":8,"output_tokens_details":{"reasoning_tokens":3}}}|}
;;

let ollama_tool_call_response () =
  {|{"model":"dashscope-3:8b","done":true,"done_reason":"tool_calls",
     "message":{"role":"assistant","content":"",
       "tool_calls":[
         {"function":{"index":0,"name":"get_temperature","arguments":{"city":"New York"}}},
         {"function":{"index":1,"name":"get_conditions","arguments":{"city":"London"}}}
       ]}}|}
;;

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

let start_mock_server
      ~sw
      ~net
      ?(status = `OK)
      ?(delay_sec = 0.0)
      ?clock
      ?capture_body
      ?on_request
      response_body
  =
  let port = fresh_port () in
  let handler _conn _req body =
    let request_body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    (match capture_body with
     | Some seen -> seen := Some request_body
     | None -> ());
    (match on_request with
     | Some f -> f ()
     | None -> ());
    (match clock with
     | Some clk when delay_sec > 0.0 -> Eio.Time.sleep clk delay_sec
     | _ -> ());
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

let start_header_capture_server ~sw ~net ~seen response_body =
  let port = fresh_port () in
  let handler _conn req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    let headers = Cohttp.Request.headers req in
    seen
    := Some
         ( Cohttp.Header.get headers "traceparent"
         , Cohttp.Header.get headers "tracestate"
         , Cohttp.Header.get headers "x-custom" );
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
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

(* ── Helper: make Provider_config ────────────────────── *)

let make_config ?(kind = Provider_config.Anthropic) base_url =
  Provider_config.make
    ~kind
    ~model_id:"test-model"
    ~base_url
    ~request_path:"/v1/messages"
    ~temperature:0.0
    ~max_tokens:100
    ()
;;

let make_openai_config base_url =
  Provider_config.make
    ~kind:Provider_config.OpenAI_compat
    ~model_id:"gpt-4"
    ~base_url
    ~request_path:"/v1/chat/completions"
    ~temperature:0.0
    ~max_tokens:100
    ()
;;

let messages = [ Types.user_msg "hello" ]

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len
    then false
    else if String.sub text idx sub_len = sub
    then true
    else loop (idx + 1)
  in
  sub_len = 0 || loop 0
;;

let mock_transport_response text =
  { Types.id = "transport-response"
  ; model = "transport-model"
  ; stop_reason = Types.EndTurn
  ; content = [ Types.Text text ]
  ; usage = None
  ; telemetry = None
  }
;;

let make_transport response : Llm_transport.t =
  { complete_sync = (fun _ -> { Llm_transport.response; latency_ms = Some 7 })
  ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _ -> response)
  }
;;

let check_typed_empty_completion expected = function
  | Error
      (Http_client.ProviderFailure
         { kind = Http_client.Empty_completion { stop_reason }; _ }) ->
    check bool "typed stop reason" true (stop_reason = expected)
  | Ok _ -> fail "expected empty completion error, got Ok"
  | Error _ -> fail "expected typed empty completion provider failure"
;;

(* ── complete: success ───────────────────────────────── *)

let test_complete_anthropic_ok () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "mock response") in
    let config = make_config url in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok resp ->
      check string "model" "mock" resp.model;
      let text =
        List.filter_map
          (function
            | Types.Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      check string "text" "mock response" text;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_http_rejects_typed_empty_completion () =
  let run expected wire_stop_reason =
    Eio_main.run
    @@ fun env ->
    try
      Eio.Switch.run
      @@ fun sw ->
      let url =
        start_mock_server ~sw ~net:env#net (anthropic_empty_response wire_stop_reason)
      in
      Complete.complete ~sw ~net:env#net ~config:(make_config url) ~messages ()
      |> check_typed_empty_completion expected;
      Eio.Switch.fail sw Exit
    with
    | Exit -> ()
  in
  List.iter
    (fun (expected, wire) -> run expected wire)
    [ Types.EndTurn, "end_turn"; Types.MaxTokens, "max_tokens" ]
;;

let test_complete_stream_http_rejects_typed_empty_completion () =
  let run expected wire_stop_reason =
    Eio_main.run
    @@ fun env ->
    try
      Eio.Switch.run
      @@ fun sw ->
      let url =
        start_mock_server ~sw ~net:env#net (anthropic_empty_sse wire_stop_reason)
      in
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~config:(make_config url)
        ~messages
        ~on_event:(fun _ -> ())
        ()
      |> check_typed_empty_completion expected;
      Eio.Switch.fail sw Exit
    with
    | Exit -> ()
  in
  List.iter
    (fun (expected, wire) -> run expected wire)
    [ Types.EndTurn, "end_turn"; Types.MaxTokens, "max_tokens" ]
;;

(* ── complete: HTTP error ────────────────────────────── *)

let test_complete_http_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_mock_server ~sw ~net:env#net ~status:`Bad_request "bad request body"
    in
    let config = make_config url in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok _ -> fail "expected Error"
    | Error (Http_client.HttpError { code; _ }) ->
      check int "status 400" 400 code;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected HttpError"
  with
  | Exit -> ()
;;

let test_complete_http_empty_error_body_has_context () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net ~status:`Not_found "" in
    let config =
      Provider_config.make
        ~kind:Provider_config.Anthropic
        ~model_id:"test-model"
        ~base_url:url
        ~request_path:"/v1/messages?api_key=secret"
        ~temperature:0.0
        ~max_tokens:100
        ()
    in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok _ -> fail "expected Error"
    | Error (Http_client.HttpError { code; body }) ->
      check int "status 404" 404 code;
      check
        string
        "diagnostic body"
        (Printf.sprintf
           "empty HTTP 404 response from provider=claude model=test-model base_url=%s \
            request_path=/v1/messages url=%s/v1/messages"
           url
           url)
        body;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected HttpError"
  with
  | Exit -> ()
;;

(* ── complete: Openai compat ─────────────────────────── *)

let test_complete_openai_ok () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (openai_response "openai reply") in
    let config = make_openai_config url in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok resp ->
      let text =
        List.filter_map
          (function
            | Types.Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      check string "text" "openai reply" text;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok for openai"
  with
  | Exit -> ()
;;

let test_complete_openai_responses_sync_ok () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_mock_server ~sw ~net:env#net (openai_responses_tool_call_response ())
    in
    let config =
      Provider_config.make
        ~kind:Provider_config.OpenAI_compat
        ~model_id:"gpt-5.5"
        ~base_url:url
        ~request_path:"/v1/responses"
        ~temperature:0.0
        ~max_tokens:100
        ()
    in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok resp ->
      check bool "stop tool use" true (resp.stop_reason = Types.StopToolUse);
      (match resp.content with
       | [ Types.Thinking { content; _ }; Types.ToolUse { id; name; input } ] ->
         check string "reasoning" "Need a lookup." content;
         check string "tool id" "call_lookup" id;
         check string "tool name" "lookup" name;
         check
           string
           "tool arg"
           "weather"
           (Yojson.Safe.Util.member "q" input |> Yojson.Safe.Util.to_string)
       | _ -> fail "expected reasoning + tool use");
      (match resp.telemetry with
       | Some telemetry ->
         check (option int) "reasoning tokens" (Some 3) telemetry.reasoning_tokens
       | None -> fail "expected telemetry");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok for OpenAI Responses sync"
  with
  | Exit -> ()
;;

let test_complete_openai_responses_json_mode_body () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let captured = ref None in
    let url =
      start_mock_server
        ~sw
        ~net:env#net
        ~capture_body:captured
        (openai_responses_tool_call_response ())
    in
    let config =
      Provider_config.make
        ~kind:Provider_config.OpenAI_compat
        ~model_id:"gpt-5.5"
        ~base_url:url
        ~request_path:"/v1/responses"
        ~response_format_json:true
        ~temperature:0.0
        ~max_tokens:100
        ()
    in
    (match Complete.complete ~sw ~net:env#net ~config ~messages () with
     | Ok _ -> ()
     | Error _ -> fail "expected Ok for OpenAI Responses JSON mode");
    match !captured with
    | Some body ->
      let json = Yojson.Safe.from_string body in
      check
        string
        "responses text.format"
        "json_object"
        Yojson.Safe.Util.(
          json |> member "text" |> member "format" |> member "type" |> to_string);
      Eio.Switch.fail sw Exit
    | None -> fail "server did not capture request body"
  with
  | Exit -> ()
;;

let start_responses_sse_server ~sw ~net response_body =
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

let openai_responses_sse_tool_call_response () =
  "event: response.created\n\
   data: \
   {\"type\":\"response.created\",\"response\":{\"id\":\"resp-stream-1\",\"model\":\"gpt-5.5\",\"status\":\"in_progress\",\"usage\":null}}\n\n\
   event: response.reasoning_summary_text.delta\n\
   data: \
   {\"type\":\"response.reasoning_summary_text.delta\",\"item_id\":\"rs_1\",\"output_index\":0,\"summary_index\":0,\"delta\":\"Need \
   a lookup.\"}\n\n\
   event: response.output_item.added\n\
   data: \
   {\"type\":\"response.output_item.added\",\"output_index\":1,\"item\":{\"id\":\"fc_1\",\"type\":\"function_call\",\"call_id\":\"call_lookup\",\"name\":\"lookup\",\"arguments\":\"\"}}\n\n\
   event: response.function_call_arguments.delta\n\
   data: \
   {\"type\":\"response.function_call_arguments.delta\",\"output_index\":1,\"item_id\":\"fc_1\",\"delta\":\"{\\\"q\\\":\\\"weather\\\"}\"}\n\n\
   event: response.completed\n\
   data: \
   {\"type\":\"response.completed\",\"response\":{\"id\":\"resp-stream-1\",\"model\":\"gpt-5.5\",\"status\":\"completed\",\"output\":[{\"id\":\"rs_1\",\"type\":\"reasoning\",\"summary\":[{\"type\":\"summary_text\",\"text\":\"Need \
   a \
   lookup.\"}],\"encrypted_content\":\"enc_reasoning_1\"},{\"id\":\"fc_1\",\"type\":\"function_call\",\"call_id\":\"call_lookup\",\"name\":\"lookup\",\"arguments\":\"{\\\"q\\\":\\\"weather\\\"}\"}],\"usage\":{\"input_tokens\":12,\"output_tokens\":8,\"input_tokens_details\":{\"cached_tokens\":2}}}}\n\n"
;;

let test_complete_stream_openai_responses_ok () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_responses_sse_server
        ~sw
        ~net:env#net
        (openai_responses_sse_tool_call_response ())
    in
    let config =
      Provider_config.make
        ~kind:Provider_config.OpenAI_compat
        ~model_id:"gpt-5.5"
        ~base_url:url
        ~request_path:"/v1/responses"
        ~temperature:0.0
        ~max_tokens:100
        ()
    in
    let events = ref [] in
    match
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~config
        ~messages
        ~on_event:(fun evt -> events := evt :: !events)
        ()
    with
    | Ok resp ->
      check string "stream id" "resp-stream-1" resp.id;
      check bool "stop tool use" true (resp.stop_reason = Types.StopToolUse);
      check bool "events emitted" true (List.length !events >= 5);
      (match resp.content with
       | [ Types.RedactedThinking raw_reasoning; Types.ToolUse { id; name; input } ] ->
         let reasoning = Yojson.Safe.from_string raw_reasoning in
         check
           string
           "reasoning type"
           "reasoning"
           (Yojson.Safe.Util.member "type" reasoning |> Yojson.Safe.Util.to_string);
         check
           string
           "encrypted reasoning"
           "enc_reasoning_1"
           (Yojson.Safe.Util.member "encrypted_content" reasoning
            |> Yojson.Safe.Util.to_string);
         check string "tool id" "call_lookup" id;
         check string "tool name" "lookup" name;
         check
           string
           "tool arg"
           "weather"
           (Yojson.Safe.Util.member "q" input |> Yojson.Safe.Util.to_string)
       | _ -> fail "expected redacted reasoning + tool use");
      (match resp.usage with
       | Some usage ->
         check int "input tokens" 12 usage.input_tokens;
         check int "output tokens" 8 usage.output_tokens;
         check int "cached tokens" 2 usage.cache_read_input_tokens
       | None -> fail "expected usage");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok for Responses streaming"
  with
  | Exit -> ()
;;

let test_complete_trace_context_headers () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let seen = ref None in
    let url =
      start_header_capture_server ~sw ~net:env#net ~seen (anthropic_response "ok")
    in
    let config =
      { (make_config url) with
        Provider_config.headers = [ "traceparent", "stale"; "x-custom", "yes" ]
      }
    in
    let trace_context =
      [ "traceparent", "00-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-bbbbbbbbbbbbbbbb-01"
      ; "tracestate", "vendor=value"
      ]
    in
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~trace_context () with
     | Ok _ -> ()
     | Error _ -> fail "expected Ok");
    match !seen with
    | Some (traceparent, tracestate, custom) ->
      check
        (option string)
        "traceparent"
        (Some "00-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa-bbbbbbbbbbbbbbbb-01")
        traceparent;
      check (option string) "tracestate" (Some "vendor=value") tracestate;
      check (option string) "custom header preserved" (Some "yes") custom;
      Eio.Switch.fail sw Exit
    | None -> fail "server did not capture headers"
  with
  | Exit -> ()
;;

let test_complete_openai_mlx_vlm_telemetry () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_mock_server
        ~sw
        ~net:env#net
        ~delay_sec:0.02
        ~clock
        (openai_mlx_vlm_response "mlx reply")
    in
    let config = make_openai_config url in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok resp ->
      let text =
        List.filter_map
          (function
            | Types.Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      check string "text" "mlx reply" text;
      (match resp.usage with
       | Some usage ->
         check int "input_tokens" 11 usage.input_tokens;
         check int "output_tokens" 5 usage.output_tokens
       | None -> fail "expected usage");
      (match resp.telemetry with
       | Some t ->
         check
           bool
           "latency patched"
           true
           (Option.value ~default:0 t.request_latency_ms > 0);
         check (option string) "canonical model id" (Some "gpt-4") t.canonical_model_id;
         check (option (float 0.001)) "peak memory" (Some 52.66) t.peak_memory_gb;
         (match t.timings with
          | Some timings ->
            check
              (option (float 0.001))
              "prompt tps"
              (Some 21.55)
              timings.prompt_per_second;
            check
              (option (float 0.001))
              "generation tps"
              (Some 81.56)
              timings.predicted_per_second
          | None -> fail "expected timings")
       | None -> fail "expected telemetry");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok for mlx-vlm openai compat"
  with
  | Exit -> ()
;;

let test_complete_sync_latency_uses_injected_clock () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let clock = Eio_mock.Clock.make () in
    Eio_mock.Clock.set_time clock 10.0;
    let url =
      start_mock_server
        ~sw
        ~net:env#net
        ~on_request:(fun () -> Eio_mock.Clock.set_time clock 11.25)
        (anthropic_response "clocked")
    in
    let config = make_config url in
    match Complete.complete ~sw ~net:env#net ~clock ~config ~messages () with
    | Ok resp ->
      (match resp.telemetry with
       | Some telemetry ->
         check
           (option int)
           "latency from injected clock"
           (Some 1250)
           telemetry.request_latency_ms
       | None -> fail "expected telemetry");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_latency_counter_clamps_negative_injected_clock () =
  let clock = Eio_mock.Clock.make () in
  Eio_mock.Clock.set_time clock 10.0;
  let counter = Complete_common.start_latency_counter ~clock () in
  Eio_mock.Clock.set_time clock 9.0;
  check (option int) "latency clamped" (Some 0) (Complete_common.latency_ms_int counter)
;;

(* ── complete with cache ─────────────────────────────── *)

let test_complete_cache_store_and_hit () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "cached") in
    let config = make_config url in
    let store : (string, Yojson.Safe.t) Hashtbl.t = Hashtbl.create 4 in
    let cache : Cache.t =
      { get = (fun ~key -> Hashtbl.find_opt store key)
      ; set = (fun ~key ~ttl_sec:_ value -> Hashtbl.replace store key value)
      }
    in
    (* First call — cache miss, HTTP hit *)
    (match Complete.complete ~sw ~net:env#net ~config ~messages ~cache () with
     | Ok _ -> check bool "stored in cache" true (Hashtbl.length store > 0)
     | Error _ -> fail "expected Ok first call");
    (* Second call — cache hit, no HTTP *)
    match Complete.complete ~sw ~net:env#net ~config ~messages ~cache () with
    | Ok resp ->
      let text =
        List.filter_map
          (function
            | Types.Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      check string "from cache" "cached" text;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok second call"
  with
  | Exit -> ()
;;

(* ── complete with metrics ───────────────────────────── *)

let test_complete_metrics () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "metrics test") in
    let config = make_config url in
    let hit_count = ref 0 in
    let miss_count = ref 0 in
    let start_count = ref 0 in
    let end_count = ref 0 in
    let status_calls = ref [] in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_cache_hit = (fun ~model_id:_ -> incr hit_count)
      ; on_cache_miss = (fun ~model_id:_ -> incr miss_count)
      ; on_request_start = (fun ~model_id:_ -> incr start_count)
      ; on_request_end = (fun ~model_id:_ ~latency_ms:_ -> incr end_count)
      ; on_error = (fun ~model_id:_ ~error:_ -> ())
      ; on_http_status =
          (fun ~provider ~model_id ~status ->
            status_calls := (provider, model_id, status) :: !status_calls)
      }
    in
    (* No cache provided → on_cache_miss not called *)
    match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
    | Ok _ ->
      check int "no cache = no miss" 0 !miss_count;
      check int "start" 1 !start_count;
      check int "end" 1 !end_count;
      check int "no hit" 0 !hit_count;
      (* on_http_status fired once with the actual 200 code *)
      check int "status callback count" 1 (List.length !status_calls);
      (match !status_calls with
       | [ (_, _, code) ] -> check int "status code" 200 code
       | _ -> fail "expected exactly one status call");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_tool_call_metrics () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (ollama_tool_call_response ()) in
    let config =
      Provider_config.make
        ~kind:Provider_config.Ollama
        ~model_id:"dashscope-3:8b"
        ~base_url:url
        ~request_path:"/api/chat"
        ~temperature:0.0
        ~max_tokens:100
        ()
    in
    let tool_calls = ref [] in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_tool_calls =
          (fun ~provider ~model_id ~count ->
            tool_calls := (provider, model_id, count) :: !tool_calls)
      }
    in
    match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
    | Ok resp ->
      check bool "stop tool use" true (resp.stop_reason = Types.StopToolUse);
      (match !tool_calls with
       | [ (provider, model_id, count) ] ->
         check string "provider" "ollama" provider;
         check string "model" "dashscope-3:8b" model_id;
         check int "tool call count" 2 count
       | _ -> fail "expected one tool-call metric");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

(* ── complete_with_retry: success first try ──────────── *)

let test_retry_first_try () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "first try ok") in
    let config = make_config url in
    match Complete.complete_with_retry ~sw ~net:env#net ~clock ~config ~messages () with
    | Ok resp ->
      let text =
        List.filter_map
          (function
            | Types.Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      check string "text" "first try ok" text;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

(* ── complete: 401 non-retryable ─────────────────────── *)

let test_complete_non_retryable () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net ~status:`Unauthorized "unauthorized" in
    let config = make_config url in
    match Complete.complete ~sw ~net:env#net ~config ~messages () with
    | Ok _ -> fail "expected Error"
    | Error (Http_client.HttpError { code; _ }) ->
      check int "401" 401 code;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected HttpError"
  with
  | Exit -> ()
;;

(* ── complete: error metrics ─────────────────────────── *)

let test_complete_error_metrics () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock_server ~sw ~net:env#net ~status:`Bad_request "bad" in
    let config = make_config url in
    let error_count = ref 0 in
    let status_calls = ref [] in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_cache_hit = (fun ~model_id:_ -> ())
      ; on_cache_miss = (fun ~model_id:_ -> ())
      ; on_request_start = (fun ~model_id:_ -> ())
      ; on_request_end = (fun ~model_id:_ ~latency_ms:_ -> ())
      ; on_error = (fun ~model_id:_ ~error:_ -> incr error_count)
      ; on_http_status =
          (fun ~provider ~model_id ~status ->
            status_calls := (provider, model_id, status) :: !status_calls)
      }
    in
    match Complete.complete ~sw ~net:env#net ~config ~messages ~metrics () with
    | Ok _ -> fail "expected Error"
    | Error _ ->
      check int "error callback" 1 !error_count;
      (* 400 HTTP response must also emit on_http_status before error fires *)
      check int "status callback count" 1 (List.length !status_calls);
      (match !status_calls with
       | [ (_, _, 400) ] -> ()
       | [ (_, _, code) ] -> fail (Printf.sprintf "expected 400, got %d" code)
       | _ -> fail "expected exactly one status call");
      Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_complete_transport_http_metrics_ok () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let config = make_openai_config "http://unused.test" in
    let status_calls = ref [] in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_http_status =
          (fun ~provider ~model_id ~status ->
            status_calls := (provider, model_id, status) :: !status_calls)
      }
    in
    let transport = make_transport (Ok (mock_transport_response "transport ok")) in
    match Complete.complete ~sw ~net:env#net ~transport ~config ~messages ~metrics () with
    | Ok _ ->
      (match !status_calls with
       | [ ("openai_compat", "gpt-4", 200) ] -> Eio.Switch.fail sw Exit
       | [ (_, _, code) ] -> fail (Printf.sprintf "expected 200, got %d" code)
       | _ -> fail "expected exactly one transport status call")
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_transport_http_metrics_error () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let config = make_openai_config "http://unused.test" in
    let status_calls = ref [] in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_http_status =
          (fun ~provider ~model_id ~status ->
            status_calls := (provider, model_id, status) :: !status_calls)
      }
    in
    let transport =
      make_transport (Error (Http_client.HttpError { code = 429; body = "rate limited" }))
    in
    match Complete.complete ~sw ~net:env#net ~transport ~config ~messages ~metrics () with
    | Ok _ -> fail "expected Error"
    | Error (Http_client.HttpError { code; _ }) ->
      check int "status 429" 429 code;
      (match !status_calls with
       | [ ("openai_compat", "gpt-4", 429) ] -> Eio.Switch.fail sw Exit
       | [ (_, _, seen) ] -> fail (Printf.sprintf "expected 429, got %d" seen)
       | _ -> fail "expected exactly one transport status call")
    | Error _ -> fail "expected HttpError"
  with
  | Exit -> ()
;;

let test_complete_transport_mock_emits_status () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let config =
      Provider_config.make
        ~kind:Provider_config.Anthropic
        ~model_id:"codex-mini"
        ~base_url:""
        ()
    in
    let hits = ref 0 in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_http_status = (fun ~provider:_ ~model_id:_ ~status:_ -> incr hits)
      }
    in
    let transport = make_transport (Ok (mock_transport_response "cli ok")) in
    match Complete.complete ~sw ~net:env#net ~transport ~config ~messages ~metrics () with
    | Ok _ ->
      check int "mock transport emits status" 1 !hits;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

(* ── Global metrics registry ──────────────────────────── *)

let test_metrics_global_default_is_noop () =
  let g = Metrics.get_global () in
  (* Default state: the noop callbacks should not raise and should be
     distinguishable by reference from a custom instance below. *)
  g.on_cache_hit ~model_id:"m";
  g.on_request_end ~model_id:"m" ~latency_ms:(Some 1);
  g.on_http_status ~provider:"ollama" ~model_id:"m" ~status:200;
  (* No side effects observable. *)
  check bool "default global accepts noop calls" true true
;;

let test_metrics_global_set_and_get () =
  let hits = ref 0 in
  let previous = Metrics.get_global () in
  (* Fun.protect guarantees the global is restored even if a check
     assertion raises inside the body, preventing cross-test pollution
     through the shared process-wide sink. *)
  Fun.protect
    ~finally:(fun () -> Metrics.set_global previous)
    (fun () ->
       let custom : Metrics.t =
         { Metrics.noop with
           on_http_status = (fun ~provider:_ ~model_id:_ ~status:_ -> incr hits)
         }
       in
       Metrics.set_global custom;
       let g = Metrics.get_global () in
       g.on_http_status ~provider:"ollama" ~model_id:"m" ~status:429;
       g.on_http_status ~provider:"glm" ~model_id:"m" ~status:429;
       check int "global metric fired twice" 2 !hits)
;;

let test_metrics_global_used_when_no_per_call_metrics () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_mock_server ~sw ~net:env#net (anthropic_response "global metrics test")
    in
    let config = make_config url in
    let status_calls = ref [] in
    let previous = Metrics.get_global () in
    let bridge : Metrics.t =
      { Metrics.noop with
        on_http_status =
          (fun ~provider ~model_id ~status ->
            status_calls := (provider, model_id, status) :: !status_calls)
      }
    in
    Metrics.set_global bridge;
    Fun.protect
      ~finally:(fun () -> Metrics.set_global previous)
      (fun () ->
         (* Deliberately do NOT pass ~metrics — global should take effect. *)
         match Complete.complete ~sw ~net:env#net ~config ~messages () with
         | Ok _ ->
           check int "global on_http_status fired once" 1 (List.length !status_calls);
           Eio.Switch.fail sw Exit
         | Error _ -> fail "expected Ok")
  with
  | Exit -> ()
;;

(* ── complete_stream: SSE ─────────────────────────────── *)

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

let anthropic_sse_thinking_signature_tool_response =
  "event: message_start\n\
   data: \
   {\"type\":\"message_start\",\"message\":{\"id\":\"msg-1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"mock\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0}}}\n\n\
   event: content_block_start\n\
   data: \
   {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\",\"signature\":\"\"}}\n\n\
   event: content_block_delta\n\
   data: \
   {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"Need \
   a lookup.\"}}\n\n\
   event: content_block_delta\n\
   data: \
   {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"signature_delta\",\"signature\":\"sig_opaque\"}}\n\n\
   event: content_block_stop\n\
   data: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
   event: content_block_start\n\
   data: \
   {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"tool_use\",\"id\":\"tu_1\",\"name\":\"lookup\",\"input\":{}}}\n\n\
   event: content_block_delta\n\
   data: \
   {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"q\\\":\\\"weather\\\"}\"}}\n\n\
   event: content_block_stop\n\
   data: {\"type\":\"content_block_stop\",\"index\":1}\n\n\
   event: message_delta\n\
   data: \
   {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"tool_use\"},\"usage\":{\"output_tokens\":5}}\n\n\
   event: message_stop\n\
   data: {\"type\":\"message_stop\"}\n\n"
;;

let anthropic_sse_frame_message_start =
  "event: message_start\n\
   data: \
   {\"type\":\"message_start\",\"message\":{\"id\":\"msg-1\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"mock\",\"content\":[],\"stop_reason\":null,\"usage\":{\"input_tokens\":10,\"output_tokens\":0,\"cache_creation_input_tokens\":0,\"cache_read_input_tokens\":0}}}\n\n"
;;

let anthropic_sse_frame_content_block_start =
  "event: content_block_start\n\
   data: \
   {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
;;

let anthropic_sse_frame_delta text =
  Printf.sprintf
    "event: content_block_delta\n\
     data: \
     {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"%s\"}}\n\n"
    text
;;

let anthropic_sse_frame_stop =
  "event: content_block_stop\n\
   data: {\"type\":\"content_block_stop\",\"index\":0}\n\n\
   event: message_delta\n\
   data: \
   {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
   event: message_stop\n\
   data: {\"type\":\"message_stop\"}\n\n"
;;

let anthropic_sse_frame_thinking_block_start =
  "event: content_block_start\n\
   data: \
   {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"thinking\",\"thinking\":\"\"}}\n\n"
;;

let anthropic_sse_frame_thinking_delta text =
  Printf.sprintf
    "event: content_block_delta\n\
     data: \
     {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"thinking_delta\",\"thinking\":\"%s\"}}\n\n"
    text
;;

(* Thinking-only prelude (block 0) followed by a deliverable text answer
   (block 1). Used by the mock-clock cutoff tests: the cutoff must fire
   while still inside the block-0 thinking deltas, and the trailing text
   answer lets the no-advance control finalize [Ok]. *)
let anthropic_thinking_then_answer_frames ~frame_gap_s =
  [ 0.0, anthropic_sse_frame_message_start
  ; frame_gap_s, anthropic_sse_frame_thinking_block_start
  ; frame_gap_s, anthropic_sse_frame_thinking_delta "t1"
  ; frame_gap_s, anthropic_sse_frame_thinking_delta "t2"
  ; frame_gap_s, anthropic_sse_frame_thinking_delta "t3"
  ; ( frame_gap_s
    , "event: content_block_stop\ndata: {\"type\":\"content_block_stop\",\"index\":0}\n\n"
    )
  ; ( frame_gap_s
    , "event: content_block_start\n\
       data: \
       {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
    )
  ; ( frame_gap_s
    , "event: content_block_delta\n\
       data: \
       {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"text_delta\",\"text\":\"answer\"}}\n\n"
    )
  ; ( frame_gap_s
    , "event: content_block_stop\n\
       data: {\"type\":\"content_block_stop\",\"index\":1}\n\n\
       event: message_delta\n\
       data: \
       {\"type\":\"message_delta\",\"delta\":{\"stop_reason\":\"end_turn\"},\"usage\":{\"output_tokens\":5}}\n\n\
       event: message_stop\n\
       data: {\"type\":\"message_stop\"}\n\n" )
  ]
;;

let read_http_request flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
  let _request_line = Eio.Buf_read.line reader in
  let rec read_headers content_length =
    match Eio.Buf_read.line reader with
    | "" -> content_length
    | line ->
      let lower = String.lowercase_ascii line in
      let content_length =
        if String.starts_with ~prefix:"content-length:" lower
        then (
          let value = String.sub line 15 (String.length line - 15) |> String.trim in
          match int_of_string_opt value with
          | Some n -> n
          | None -> content_length)
        else content_length
      in
      read_headers content_length
    | exception End_of_file -> content_length
  in
  let content_length = read_headers 0 in
  if content_length > 0 then ignore (Eio.Buf_read.take content_length reader : string)
;;

let start_raw_sse_server ~sw ~net ~clock delayed_frames =
  let port = fresh_port () in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:8
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Net.accept_fork
      ~sw
      socket
      ~on_error:(fun _ -> ())
      (fun flow _addr ->
         try
           read_http_request flow;
           Eio.Flow.copy_string
             "HTTP/1.1 200 OK\r\n\
              Content-Type: text/event-stream\r\n\
              Connection: close\r\n\
              \r\n"
             flow;
           List.iter
             (fun (delay_sec, frame) ->
                if delay_sec > 0.0 then Eio.Time.sleep clock delay_sec;
                Eio.Flow.copy_string frame flow)
             delayed_frames
         with
         | _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
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

let text_of_response (resp : Types.api_response) =
  List.filter_map
    (function
      | Types.Text s -> Some s
      | _ -> None)
    resp.content
  |> String.concat ""
;;

let test_complete_stream_ok () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_sse_server ~sw ~net:env#net (anthropic_sse_response "streamed text")
    in
    let config = make_config url in
    let events = ref [] in
    let on_event evt = events := evt :: !events in
    match Complete.complete_stream ~sw ~net:env#net ~config ~messages ~on_event () with
    | Ok resp ->
      let text =
        List.filter_map
          (function
            | Types.Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      check string "text" "streamed text" text;
      check bool "events received" true (List.length !events > 0);
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_stream_preserves_thinking_signature () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_sse_server ~sw ~net:env#net anthropic_sse_thinking_signature_tool_response
    in
    let config = make_config url in
    match
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~config
        ~messages
        ~on_event:(fun _ -> ())
        ()
    with
    | Ok resp ->
      check bool "stop tool use" true (resp.stop_reason = Types.StopToolUse);
      (match resp.content with
       | [ Types.Thinking { signature; content }; Types.ToolUse { id; name; input } ] ->
         check bool "thinking signature" true (signature = Some "sig_opaque");
         check string "thinking content" "Need a lookup." content;
         check string "tool id" "tu_1" id;
         check string "tool name" "lookup" name;
         check
           string
           "tool arg"
           "weather"
           (Yojson.Safe.Util.member "q" input |> Yojson.Safe.Util.to_string)
       | _ -> fail "expected thinking + tool use");
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_stream_on_event_exception_is_nonfatal () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_sse_server ~sw ~net:env#net (anthropic_sse_response "streamed text")
    in
    let config = make_config url in
    let calls = ref 0 in
    let on_event _evt =
      incr calls;
      failwith "stream observer failed"
    in
    match Complete.complete_stream ~sw ~net:env#net ~config ~messages ~on_event () with
    | Ok resp ->
      check string "text" "streamed text" (text_of_response resp);
      check bool "callback invoked" true (!calls > 0);
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_stream_transport_on_event_exception_is_nonfatal () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let calls = ref 0 in
  let transport =
    { (make_transport (Ok (mock_transport_response "transport streamed"))) with
      complete_stream =
        (fun ?on_telemetry:_ ~on_event _req ->
          on_event
            (Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "ignored" });
          Ok (mock_transport_response "transport streamed"))
    }
  in
  let on_event _evt =
    incr calls;
    failwith "transport observer failed"
  in
  let config = make_config "http://unused.test" in
  match
    Complete.complete_stream ~sw ~net:env#net ~transport ~config ~messages ~on_event ()
  with
  | Ok resp ->
    check string "text" "transport streamed" (text_of_response resp);
    check int "callback invoked" 1 !calls
  | Error _ -> fail "expected Ok"
;;

let test_complete_stream_active_chunks_can_exceed_idle_timeout_total () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_raw_sse_server
        ~sw
        ~net:env#net
        ~clock:env#clock
        [ 0.0, anthropic_sse_frame_message_start
        ; 0.03, anthropic_sse_frame_content_block_start
        ; 0.03, anthropic_sse_frame_delta "active "
        ; 0.03, anthropic_sse_frame_delta "long "
        ; 0.03, anthropic_sse_frame_delta "stream"
        ; 0.03, anthropic_sse_frame_stop
        ]
    in
    let config = make_config url in
    let events = ref [] in
    let on_event evt = events := evt :: !events in
    match
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~clock:env#clock
        ~stream_idle_timeout_s:0.08
        ~config
        ~messages
        ~on_event
        ()
    with
    | Ok resp ->
      check string "text" "active long stream" (text_of_response resp);
      check bool "events received" true (List.length !events > 0);
      Eio.Switch.fail sw Exit
    | Error err ->
      fail
        (Printf.sprintf
           "expected active stream to complete, got %s"
           (match err with
            | Http_client.NetworkError { message; _ }
            | Http_client.TimeoutError { message; _ } -> message
            | Http_client.HttpError { code; _ } -> Printf.sprintf "HTTP %d" code
            | Http_client.AcceptRejected { reason } -> reason
            | Http_client.ProviderTerminal { message; _ } -> message
            | Http_client.ProviderFailure { message; _ } -> message))
  with
  | Exit -> ()
;;

let test_complete_stream_idle_timeout_still_fires () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_raw_sse_server
        ~sw
        ~net:env#net
        ~clock:env#clock
        [ 0.12, anthropic_sse_frame_message_start
        ; 0.0, anthropic_sse_frame_content_block_start
        ; 0.0, anthropic_sse_frame_delta "late"
        ; 0.0, anthropic_sse_frame_stop
        ]
    in
    let config = make_config url in
    let on_event _evt = () in
    match
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~clock:env#clock
        ~stream_idle_timeout_s:0.03
        ~config
        ~messages
        ~on_event
        ()
    with
    | Error (Http_client.TimeoutError { phase = Http_client.First_token; message }) ->
      let prefix = "stream_idle_timeout_s deadline exceeded" in
      check
        bool
        "stream idle message"
        true
        (String.length message >= String.length prefix
         && String.equal (String.sub message 0 (String.length prefix)) prefix);
      Eio.Switch.fail sw Exit
    | Error (Http_client.TimeoutError { phase; _ }) ->
      fail
        (Printf.sprintf
           "unexpected timeout phase %s"
           (Http_client.timeout_phase_to_label phase))
    | Ok _ -> fail "expected stream idle timeout"
    | Error _ -> fail "expected TimeoutError{phase=First_token}"
  with
  | Exit -> ()
;;

(* Bug #10 regression (38-bug campaign): actively streaming reasoning
   deltas are stream LIVENESS, not idleness. Each hidden-reasoning delta
   advances the injected mock clock by 6s, so the cumulative
   thinking-only span crosses the 10s idle budget on the third delta —
   the exact condition the deleted thinking-only wall-clock cutoff used
   to abort on (TimeoutError{Stream_idle Streaming_thinking}, then MASC
   retried and re-killed the round, losing the turn). Inter-event gaps
   (6s) stay under the 10s idle deadline, so the line-idle timer must
   not fire either: the stream must finalize [Ok] with the answer. *)
let test_complete_stream_long_thinking_is_not_cut_off () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let mock_clock = Eio_mock.Clock.make () in
    let url =
      start_raw_sse_server
        ~sw
        ~net:env#net
        ~clock:env#clock
        (anthropic_thinking_then_answer_frames ~frame_gap_s:0.01)
    in
    let config = make_config url in
    let mock_now = ref 0.0 in
    let on_event = function
      | Types.ContentBlockDelta { delta = Types.ThinkingDelta _; _ } ->
        mock_now := !mock_now +. 6.0;
        Eio_mock.Clock.set_time mock_clock !mock_now
      | _ -> ()
    in
    match
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~clock:mock_clock
        ~stream_idle_timeout_s:10.0
        ~config
        ~messages
        ~on_event
        ()
    with
    | Ok resp ->
      check string "text" "answer" (text_of_response resp);
      Eio.Switch.fail sw Exit
    | Error err ->
      fail
        (Printf.sprintf
           "long thinking must stream to completion, got %s"
           (match err with
            | Http_client.NetworkError { message; _ }
            | Http_client.TimeoutError { message; _ } -> message
            | Http_client.HttpError { code; _ } -> Printf.sprintf "HTTP %d" code
            | Http_client.AcceptRejected { reason } -> reason
            | Http_client.ProviderTerminal { message; _ } -> message
            | Http_client.ProviderFailure { message; _ } -> message))
  with
  | Exit -> ()
;;

let test_complete_stream_metrics () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_sse_server ~sw ~net:env#net (anthropic_sse_response "streamed text")
    in
    let config = make_config url in
    let first_chunks = ref [] in
    let inter_chunks = ref [] in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_streaming_first_chunk =
          (fun ~provider ~model_id ~ttfrc_ms ->
            first_chunks := (provider, model_id, ttfrc_ms) :: !first_chunks)
      ; on_streaming_chunk =
          (fun ~provider ~model_id ~chunk_index ~inter_chunk_ms ->
            inter_chunks
            := (provider, model_id, chunk_index, inter_chunk_ms) :: !inter_chunks)
      }
    in
    let on_event _evt = () in
    match
      Complete.complete_stream ~sw ~net:env#net ~config ~messages ~on_event ~metrics ()
    with
    | Ok _ ->
      check int "first chunk count" 1 (List.length !first_chunks);
      (match !first_chunks with
       | [ (provider, model_id, ttfrc_ms) ] ->
         check string "provider" "anthropic" provider;
         check string "model_id" "test-model" model_id;
         check bool "ttfrc non-negative" true (ttfrc_ms >= 0.0)
       | _ -> fail "expected one first chunk");
      check bool "inter chunk metrics" true (List.length !inter_chunks > 0);
      List.iter
        (fun (provider, model_id, chunk_index, inter_chunk_ms) ->
           check string "inter provider" "anthropic" provider;
           check string "inter model_id" "test-model" model_id;
           check bool "chunk_index non-negative" true (chunk_index >= 0);
           check bool "inter chunk non-negative" true (inter_chunk_ms >= 0.0))
        !inter_chunks;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected Ok"
  with
  | Exit -> ()
;;

let test_complete_stream_unknown_latency_stays_unknown () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_sse_server ~sw ~net:env#net (anthropic_sse_response "streamed text")
    in
    let config = make_config url in
    let telemetry_events = ref [] in
    let first_chunk_metrics = ref 0 in
    let inter_chunk_metrics = ref 0 in
    let metrics : Metrics.t =
      { Metrics.noop with
        on_streaming_first_chunk =
          (fun ~provider:_ ~model_id:_ ~ttfrc_ms:_ -> incr first_chunk_metrics)
      ; on_streaming_chunk =
          (fun ~provider:_ ~model_id:_ ~chunk_index:_ ~inter_chunk_ms:_ ->
            incr inter_chunk_metrics)
      }
    in
    let result =
      Complete_stream.complete_stream_http
        ~sw
        ~net:env#net
        ~latency_counter:Complete_common.Unknown_latency
        ~on_telemetry:(fun evt -> telemetry_events := evt :: !telemetry_events)
        ~metrics
        ~config
        ~messages
        ~tools:[]
        ~on_event:(fun _evt -> ())
        ()
    in
    (match result with
     | Ok resp ->
       (match resp.telemetry with
        | Some t ->
          check (option int) "response latency unknown" None t.request_latency_ms;
          check (option (float 0.001)) "response ttfrc unknown" None t.ttfrc_ms
        | None -> fail "expected response telemetry")
     | Error _ -> fail "expected Ok");
    let first_chunk =
      List.find_map
        (function
          | Telemetry_event.Streaming_first_chunk r -> Some r.ttfrc_ms
          | _ -> None)
        !telemetry_events
    in
    let summary =
      List.find_map
        (function
          | Telemetry_event.Streaming_summary
              { total_ms; inter_chunk_ms_p50; inter_chunk_ms_p95; inter_chunk_ms_max; _ }
            -> Some (total_ms, inter_chunk_ms_p50, inter_chunk_ms_p95, inter_chunk_ms_max)
          | _ -> None)
        !telemetry_events
    in
    check
      (option (option (float 0.001)))
      "first chunk latency event"
      (Some None)
      first_chunk;
    (match summary with
     | Some (total_ms, inter_chunk_ms_p50, inter_chunk_ms_p95, inter_chunk_ms_max) ->
       check (option (float 0.001)) "summary total unknown" None total_ms;
       check (option (float 0.001)) "summary p50 unknown" None inter_chunk_ms_p50;
       check (option (float 0.001)) "summary p95 unknown" None inter_chunk_ms_p95;
       check (option (float 0.001)) "summary max unknown" None inter_chunk_ms_max
     | None -> fail "expected streaming summary");
    check int "first chunk metrics skipped" 0 !first_chunk_metrics;
    check int "inter chunk metrics skipped" 0 !inter_chunk_metrics;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* RFC-OAS-026: drive the [Some t] transport dispatch arm with a transport that
   has NO construction-time idle deadline. The high-level [stream_idle_timeout_s]
   must reach [read_sse] via the request-borne carrier field on
   [Llm_transport.completion_request]; pre-F1 the dispatch dropped it and a
   first-token stall hung until an external watchdog. The sibling idle-timeout
   tests above call [complete_stream] WITHOUT [~transport] (the [None] arm,
   which always armed idle), so they pass even with the dispatch drop — this one
   guards the transport arm specifically. *)
let test_complete_stream_transport_arm_idle_timeout () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let url =
      start_raw_sse_server
        ~sw
        ~net:env#net
        ~clock:env#clock
        [ 0.15, anthropic_sse_frame_message_start
        ; 0.0, anthropic_sse_frame_content_block_start
        ; 0.0, anthropic_sse_frame_delta "late"
        ; 0.0, anthropic_sse_frame_stop
        ]
    in
    let config = make_config url in
    (* No construction-time idle (omit [?stream_idle_timeout_s]); only the
       request-borne deadline carried through the dispatch can arm read_sse. *)
    let transport = Complete.make_http_transport ~clock:env#clock ~sw ~net:env#net () in
    match
      Complete.complete_stream
        ~sw
        ~net:env#net
        ~clock:env#clock
        ~stream_idle_timeout_s:0.03
        ~transport
        ~config
        ~messages
        ~on_event:(fun _ -> ())
        ()
    with
    | Error (Http_client.TimeoutError { phase = Http_client.First_token; _ }) ->
      Eio.Switch.fail sw Exit
    | Ok _ -> fail "expected idle timeout via the Some-t dispatch — carrier dropped?"
    | Error _ -> fail "expected TimeoutError{phase=First_token} via the transport arm"
  with
  | Exit -> ()
;;

(* ── complete: body_timeout_s ─────────────────────────── *)

let test_complete_body_timeout_fires () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Server delays response by 2.0s; caller deadline 0.3s must fire first
       and produce TimeoutError{phase=Non_streaming_body} with a body-deadline message. *)
    let url =
      start_mock_server
        ~sw
        ~net:env#net
        ~clock:env#clock
        ~delay_sec:2.0
        (anthropic_response "should not arrive")
    in
    let config = make_config url in
    let t0 = Unix.gettimeofday () in
    (match
       Complete.complete
         ~sw
         ~net:env#net
         ~clock:env#clock
         ~config
         ~messages
         ~body_timeout_s:0.3
         ()
     with
     | Ok _ -> fail "expected Error (body_timeout_s should have fired)"
     | Error
         (Http_client.TimeoutError { phase = Http_client.Non_streaming_body; message }) ->
       let elapsed = Unix.gettimeofday () -. t0 in
       check bool "fires under server delay" true (elapsed < 1.5);
       check
         bool
         "message identifies body deadline"
         true
         (let prefix = "body_timeout_s deadline exceeded" in
          String.length message >= String.length prefix
          && String.equal (String.sub message 0 (String.length prefix)) prefix)
     | Error _ ->
       fail "unexpected error variant (expected TimeoutError{phase=Non_streaming_body})");
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_complete_body_timeout_does_not_fire_on_fast_response () =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    (* No server delay; generous body_timeout_s must not interfere. *)
    let url = start_mock_server ~sw ~net:env#net (anthropic_response "fast response") in
    let config = make_config url in
    (match
       Complete.complete
         ~sw
         ~net:env#net
         ~clock:env#clock
         ~config
         ~messages
         ~body_timeout_s:60.0
         ()
     with
     | Ok resp ->
       let text =
         List.filter_map
           (function
             | Types.Text s -> Some s
             | _ -> None)
           resp.content
         |> String.concat ""
       in
       check string "text" "fast response" text
     | Error _ -> fail "unexpected error on fast path with body_timeout_s set");
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run
    "complete_http"
    [ ( "complete"
      , [ test_case "anthropic ok" `Quick test_complete_anthropic_ok
        ; test_case
            "HTTP rejects typed empty completion"
            `Quick
            test_complete_http_rejects_typed_empty_completion
        ; test_case "http error" `Quick test_complete_http_error
        ; test_case
            "empty http error body has context"
            `Quick
            test_complete_http_empty_error_body_has_context
        ; test_case "openai ok" `Quick test_complete_openai_ok
        ; test_case
            "openai responses sync ok"
            `Quick
            test_complete_openai_responses_sync_ok
        ; test_case
            "openai responses json mode body"
            `Quick
            test_complete_openai_responses_json_mode_body
        ; test_case
            "openai responses stream ok"
            `Quick
            test_complete_stream_openai_responses_ok
        ; test_case
            "openai mlx-vlm telemetry"
            `Quick
            test_complete_openai_mlx_vlm_telemetry
        ; test_case
            "sync latency uses injected clock"
            `Quick
            test_complete_sync_latency_uses_injected_clock
        ; test_case
            "latency clamps negative injected clock"
            `Quick
            test_latency_counter_clamps_negative_injected_clock
        ; test_case "trace context headers" `Quick test_complete_trace_context_headers
        ; test_case "non-retryable" `Quick test_complete_non_retryable
        ; test_case
            "body_timeout_s fires under server delay"
            `Quick
            test_complete_body_timeout_fires
        ; test_case
            "body_timeout_s no-op on fast response"
            `Quick
            test_complete_body_timeout_does_not_fire_on_fast_response
        ] )
    ; "cache", [ test_case "store and hit" `Quick test_complete_cache_store_and_hit ]
    ; ( "metrics"
      , [ test_case "callbacks" `Quick test_complete_metrics
        ; test_case "tool call callback" `Quick test_complete_tool_call_metrics
        ; test_case "error callback" `Quick test_complete_error_metrics
        ; test_case "transport http ok" `Quick test_complete_transport_http_metrics_ok
        ; test_case
            "transport http error"
            `Quick
            test_complete_transport_http_metrics_error
        ; test_case
            "transport mock emits status"
            `Quick
            test_complete_transport_mock_emits_status
        ; test_case "global default is noop" `Quick test_metrics_global_default_is_noop
        ; test_case "global set and get" `Quick test_metrics_global_set_and_get
        ; test_case
            "global used when no per-call metrics"
            `Quick
            test_metrics_global_used_when_no_per_call_metrics
        ] )
    ; "retry", [ test_case "first try ok" `Quick test_retry_first_try ]
    ; ( "stream"
      , [ test_case "sse ok" `Quick test_complete_stream_ok
        ; test_case
            "HTTP stream rejects typed empty completion"
            `Quick
            test_complete_stream_http_rejects_typed_empty_completion
        ; test_case
            "preserves thinking signature"
            `Quick
            test_complete_stream_preserves_thinking_signature
        ; test_case
            "on_event exceptions are nonfatal"
            `Quick
            test_complete_stream_on_event_exception_is_nonfatal
        ; test_case
            "transport on_event exceptions are nonfatal"
            `Quick
            test_complete_stream_transport_on_event_exception_is_nonfatal
        ; test_case
            "active chunks exceed idle total"
            `Quick
            test_complete_stream_active_chunks_can_exceed_idle_timeout_total
        ; test_case
            "stream idle timeout still fires"
            `Quick
            test_complete_stream_idle_timeout_still_fires
        ; test_case "streaming metrics" `Quick test_complete_stream_metrics
        ; test_case
            "unknown stream latency stays unknown"
            `Quick
            test_complete_stream_unknown_latency_stays_unknown
        ; test_case
            "transport arm idle timeout (RFC-OAS-026)"
            `Quick
            test_complete_stream_transport_arm_idle_timeout
        ; test_case
            "long thinking is not cut off (bug #10 regression)"
            `Quick
            test_complete_stream_long_thinking_is_not_cut_off
        ] )
    ]
;;
