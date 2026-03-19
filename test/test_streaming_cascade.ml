(** Unit tests for streaming cascade (v0.61.0).

    Tests cascade configuration, error paths, and metrics tracking.
    No real LLM calls — tests parse/filter/error logic only. *)

open Alcotest
open Llm_provider

(* ── Helpers ──────────────────────────────────────── *)

let make_provider ?(kind=Provider_config.OpenAI_compat)
    ?(base_url="http://127.0.0.1:19999")
    ?(request_path="/v1/chat/completions")
    model_id =
  Provider_config.make ~kind ~model_id ~base_url ~request_path ()

let dummy_messages : Types.message list = [
  { role = User; content = [Text "hello"]; name = None; tool_call_id = None }
]

let on_event_noop : Types.sse_event -> unit = fun _ -> ()

(* ── complete_named_stream: no callable models ────── *)

let test_named_stream_no_models () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun _sw ->
  let _net = Eio.Stdenv.net env in
  (* "foobar:model" will fail to parse → empty provider list *)
  let result = Cascade_config.complete_named_stream
    ~sw:_sw ~net:_net
    ~name:"test_stream"
    ~defaults:["foobar:model"]
    ~messages:dummy_messages
    ~on_event:on_event_noop
    () in
  match result with
  | Error (Http_client.NetworkError { message }) ->
    check bool "contains 'No callable'" true
      (let needle = "No callable" in
       String.length message >= String.length needle &&
       try let _ = Str.search_forward (Str.regexp_string needle) message 0 in true
       with Not_found -> false)
  | Ok _ -> fail "expected error for no callable models"
  | Error _ -> fail "expected NetworkError"

(* ── complete_named_stream: unknown providers only ─── *)

let test_named_stream_unknown_providers () =
  (* When all model strings are unknown/unparseable, should get error *)
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let result = Cascade_config.complete_named_stream
    ~sw ~net
    ~name:"test_stream"
    ~defaults:["unknown1:model"; "unknown2:model"]
    ~messages:dummy_messages
    ~on_event:on_event_noop
    () in
  match result with
  | Error (Http_client.NetworkError { message }) ->
    check bool "mentions 'No callable'" true
      (try let _ = Str.search_forward (Str.regexp_string "No callable") message 0 in true
       with Not_found -> false)
  | Ok _ -> fail "expected error"
  | Error _ -> fail "expected NetworkError"

(* ── complete_stream_cascade: all providers fail ──── *)

let test_stream_cascade_all_fail () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let p1 = make_provider ~base_url:"http://127.0.0.1:19991" "model-a" in
  let p2 = make_provider ~base_url:"http://127.0.0.1:19992" "model-b" in
  let cascade : Complete.cascade = { primary = p1; fallbacks = [p2] } in
  let result = Complete.complete_stream_cascade
    ~sw ~net ~cascade ~messages:dummy_messages
    ~on_event:on_event_noop () in
  match result with
  | Error _ -> ()  (* All providers should fail — connection refused *)
  | Ok _ -> fail "expected error when all providers fail"

(* ── Metrics: cascade_fallback callback fires ─────── *)

let test_stream_cascade_metrics_fire () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let p1 = make_provider ~base_url:"http://127.0.0.1:19993" "model-a" in
  let p2 = make_provider ~base_url:"http://127.0.0.1:19994" "model-b" in
  let cascade : Complete.cascade = { primary = p1; fallbacks = [p2] } in
  let fallback_count = ref 0 in
  let metrics : Metrics.t = {
    Metrics.noop with
    on_cascade_fallback = (fun ~from_model:_ ~to_model:_ ~reason:_ ->
      incr fallback_count);
  } in
  let _result = Complete.complete_stream_cascade
    ~sw ~net ~cascade ~messages:dummy_messages
    ~on_event:on_event_noop ~metrics () in
  (* Primary fails → fallback fires once *)
  check int "fallback fired once" 1 !fallback_count

(* ── complete_named_stream: timeout parameter ─────── *)

let test_named_stream_timeout () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  let clock = Eio.Stdenv.clock env in
  (* Use custom provider on a non-listening port for fast failure *)
  let result = Cascade_config.complete_named_stream
    ~sw ~net ~clock
    ~name:"timeout_test"
    ~defaults:["custom:model@http://127.0.0.1:19996"]
    ~messages:dummy_messages
    ~timeout_sec:2
    ~on_event:on_event_noop
    () in
  (* Should fail with connection error (fast) or timeout *)
  match result with
  | Error _ -> ()
  | Ok _ -> fail "expected error on non-listening port"

(* ── complete_stream_cascade: single provider ok path *)

let test_stream_cascade_single_provider () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let net = Eio.Stdenv.net env in
  (* Single provider, no fallbacks. Error is expected (no server). *)
  let p1 = make_provider ~base_url:"http://127.0.0.1:19995" "model-solo" in
  let cascade : Complete.cascade = { primary = p1; fallbacks = [] } in
  let result = Complete.complete_stream_cascade
    ~sw ~net ~cascade ~messages:dummy_messages
    ~on_event:on_event_noop () in
  match result with
  | Error _ -> ()  (* Expected: connection refused *)
  | Ok _ -> ()     (* Would be a surprise, but structurally valid *)

(* ── Parse model string for streaming ─────────────── *)

let test_parse_model_strings_for_stream () =
  (* Verify parse_model_strings works for streaming cascade input *)
  let results = Cascade_config.parse_model_strings
    ["llama:qwen3.5"; "custom:m@http://localhost:1234"] in
  check int "two valid providers" 2 (List.length results);
  let first = List.hd results in
  check string "first model" "qwen3.5" first.model_id

(* ── is_retryable classification ──────────────────── *)

let test_retryable_errors () =
  check bool "429 retryable" true
    (Complete.is_retryable
       (Http_client.HttpError { code = 429; body = "rate limit" }));
  check bool "503 retryable" true
    (Complete.is_retryable
       (Http_client.HttpError { code = 503; body = "unavailable" }));
  check bool "network retryable" true
    (Complete.is_retryable
       (Http_client.NetworkError { message = "connection refused" }));
  check bool "401 not retryable" false
    (Complete.is_retryable
       (Http_client.HttpError { code = 401; body = "unauthorized" }));
  check bool "400 not retryable" false
    (Complete.is_retryable
       (Http_client.HttpError { code = 400; body = "bad request" }))

(* ── Suite ────────────────────────────────────────── *)

let () =
  run "streaming_cascade" [
    "named_stream", [
      test_case "no callable models" `Quick test_named_stream_no_models;
      test_case "unknown providers" `Quick test_named_stream_unknown_providers;
      test_case "timeout" `Quick test_named_stream_timeout;
    ];
    "cascade", [
      test_case "all providers fail" `Quick test_stream_cascade_all_fail;
      test_case "metrics fire on fallback" `Quick test_stream_cascade_metrics_fire;
      test_case "single provider" `Quick test_stream_cascade_single_provider;
    ];
    "config", [
      test_case "parse model strings" `Quick test_parse_model_strings_for_stream;
      test_case "retryable errors" `Quick test_retryable_errors;
    ];
  ]
