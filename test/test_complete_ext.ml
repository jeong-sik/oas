(** Extended coverage tests for Complete module — public API only.
    Targets: is_retryable and default_retry_config. *)

open Alcotest
open Llm_provider

let make_config
      ?(kind = Provider_config.OpenAI_compat)
      ?(model_id = "coverage-model")
      ?(base_url = "http://127.0.0.1:8080")
      ?(headers = [])
      ?(request_path = "/v1/chat/completions")
      ?api_key
      ?min_p
      ?top_p
      ?top_k
      ()
  =
  Provider_config.make
    ~kind
    ~model_id
    ~base_url
    ~headers
    ~request_path
    ?api_key
    ?min_p
    ?top_p
    ?top_k
    ()
;;

let make_usage ?(input_tokens = 11) ?(output_tokens = 7) () : Types.api_usage =
  { input_tokens
  ; output_tokens
  ; cache_creation_input_tokens = 0
  ; cache_read_input_tokens = 0
  ; cost_usd = None
  }
;;

let make_response ?(model = "") ?(content = [ Types.Text "ok" ]) ?usage () =
  { Types.id = "resp-complete-ext"
  ; model
  ; stop_reason = EndTurn
  ; content
  ; usage
  ; telemetry = None
  }
;;

type metric_probe =
  { mutable cache_hits : int
  ; mutable cache_misses : int
  ; mutable starts : int
  ; mutable ends : int
  ; mutable errors : string list
  ; mutable statuses : int list
  ; mutable token_usage : (int * int) list
  ; mutable tool_calls : int list
  ; mutable retries : int list
  ; mutable streaming_first_chunks : float list
  }

let metric_probe () =
  { cache_hits = 0
  ; cache_misses = 0
  ; starts = 0
  ; ends = 0
  ; errors = []
  ; statuses = []
  ; token_usage = []
  ; tool_calls = []
  ; retries = []
  ; streaming_first_chunks = []
  }
;;

let metrics_of_probe probe =
  { Metrics.noop with
    on_cache_hit = (fun ~model_id:_ -> probe.cache_hits <- probe.cache_hits + 1)
  ; on_cache_miss = (fun ~model_id:_ -> probe.cache_misses <- probe.cache_misses + 1)
  ; on_request_start = (fun ~model_id:_ -> probe.starts <- probe.starts + 1)
  ; on_request_end = (fun ~model_id:_ ~latency_ms:_ -> probe.ends <- probe.ends + 1)
  ; on_error = (fun ~model_id:_ ~error -> probe.errors <- error :: probe.errors)
  ; on_http_status =
      (fun ~provider:_ ~model_id:_ ~status -> probe.statuses <- status :: probe.statuses)
  ; on_token_usage =
      (fun ~provider:_ ~model_id:_ ~input_tokens ~output_tokens ->
        probe.token_usage <- (input_tokens, output_tokens) :: probe.token_usage)
  ; on_tool_calls =
      (fun ~provider:_ ~model_id:_ ~count ->
        probe.tool_calls <- count :: probe.tool_calls)
  ; on_retry =
      (fun ~provider:_ ~model_id:_ ~attempt -> probe.retries <- attempt :: probe.retries)
  ; on_streaming_first_chunk =
      (fun ~provider:_ ~model_id:_ ~ttfrc_ms ->
        probe.streaming_first_chunks <- ttfrc_ms :: probe.streaming_first_chunks)
  }
;;

let in_memory_cache () =
  let table = Hashtbl.create 4 in
  ({ Cache.get = (fun ~key -> Hashtbl.find_opt table key)
   ; set = (fun ~key ~ttl_sec:_ json -> Hashtbl.replace table key json)
   }
   : Cache.t)
;;

let transport_of_sync sync_response =
  { Llm_transport.complete_sync = (fun _request -> sync_response ())
  ; complete_stream =
      (fun ?on_telemetry:_ ~on_event:_ _request ->
        match sync_response () with
        | { response = Ok resp; _ } -> Ok resp
        | { response = Error err; _ } -> Error err)
  }
;;

let string_of_http_error = function
  | Http_client.HttpError { code; body } -> Printf.sprintf "HTTP %d: %s" code body
  | NetworkError { message; _ } -> message
  | TimeoutError { message; _ } -> message
  | AcceptRejected { reason } -> reason
  | ProviderTerminal { message; _ } -> message
  | ProviderFailure { kind; message } ->
    Http_client.provider_failure_to_string ~kind ~message
;;

(* ── is_retryable ────────────────────────────────────── *)

let test_retryable_429 () =
  check
    bool
    "429"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = "" }))
;;

let test_retryable_500 () =
  check
    bool
    "500"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 500; body = "" }))
;;

let test_retryable_502 () =
  check
    bool
    "502"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 502; body = "" }))
;;

let test_retryable_503 () =
  check
    bool
    "503"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 503; body = "" }))
;;

let test_retryable_529 () =
  check
    bool
    "529"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 529; body = "" }))
;;

let test_not_retryable_400 () =
  check
    bool
    "400"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }))
;;

let test_not_retryable_401 () =
  check
    bool
    "401"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }))
;;

let test_not_retryable_403 () =
  check
    bool
    "403"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 403; body = "" }))
;;

let test_not_retryable_404 () =
  check
    bool
    "404"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }))
;;

let test_not_retryable_422 () =
  check
    bool
    "422"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 422; body = "" }))
;;

let test_retryable_network () =
  check
    bool
    "network error"
    true
    (Complete.is_retryable
       (Http_client.NetworkError { message = "connection refused"; kind = Unknown }))
;;

let test_not_retryable_200 () =
  (* 200 is not an error, but is_retryable should return false *)
  check
    bool
    "200"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 200; body = "" }))
;;

(* ── default_retry_config ────────────────────────────── *)

let test_retry_config_max () =
  check int "max_retries" 3 Complete.default_retry_config.max_retries
;;

let test_retry_config_initial () =
  check (float 0.01) "initial_delay" 1.0 Complete.default_retry_config.initial_delay_sec
;;

let test_retry_config_max_delay () =
  check (float 0.01) "max_delay" 30.0 Complete.default_retry_config.max_delay_sec
;;

let test_retry_config_backoff () =
  check (float 0.01) "backoff" 2.0 Complete.default_retry_config.backoff_multiplier
;;

(* ── Provider defaults / public helpers ───────────────── *)

let test_gemini_url_variants () =
  let keyed =
    make_config
      ~kind:Provider_config.Gemini
      ~model_id:"gemini-3-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta"
      ~api_key:"secret"
      ()
  in
  check
    string
    "sync keyed"
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash:generateContent"
    (Complete.gemini_url ~config:keyed ~stream:false);
  check
    string
    "stream keyed"
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash:streamGenerateContent?alt=sse"
    (Complete.gemini_url ~config:keyed ~stream:true);
  let no_key =
    make_config
      ~kind:Provider_config.Gemini
      ~model_id:"gemini-3-flash"
      ~base_url:"https://generativelanguage.googleapis.com/v1beta"
      ()
  in
  check
    string
    "stream no key"
    "https://generativelanguage.googleapis.com/v1beta/models/gemini-3-flash:streamGenerateContent?alt=sse"
    (Complete.gemini_url ~config:no_key ~stream:true)
;;

let test_sampling_defaults_and_overlay () =
  let defaults = Complete.provider_sampling_defaults Provider_config.OpenAI_compat in
  check
    (option (float 0.001))
    "openai min_p"
    (Some Constants.Sampling.openai_compat_min_p)
    defaults.default_min_p;
  let no_defaults = Complete.provider_sampling_defaults Provider_config.Anthropic in
  check (option (float 0.001)) "anthropic min_p" None no_defaults.default_min_p;
  let local = make_config () in
  let local_defaulted = Complete.apply_sampling_defaults local in
  check
    (option (float 0.001))
    "uncatalogued local OpenAI_compat min_p not defaulted (RFC-OAS-034)"
    None
    local_defaulted.min_p;
  let explicit = make_config ~min_p:0.2 ~top_p:0.7 ~top_k:17 () in
  let explicit_defaulted = Complete.apply_sampling_defaults explicit in
  check
    (option (float 0.001))
    "explicit min_p preserved"
    (Some 0.2)
    explicit_defaulted.min_p;
  check
    (option (float 0.001))
    "explicit top_p preserved"
    (Some 0.7)
    explicit_defaulted.top_p;
  check (option int) "explicit top_k preserved" (Some 17) explicit_defaulted.top_k
;;

(* ── complete wrapper paths ───────────────────────────── *)

let test_complete_transport_success_cache_metrics_and_trace_headers () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let probe = metric_probe () in
  let metrics = metrics_of_probe probe in
  let cache = in_memory_cache () in
  let calls = ref 0 in
  let config =
    make_config
      ~headers:[ "traceparent", "old"; "x-base", "base" ]
      ~model_id:"openai-test"
      ()
  in
  let content =
    [ Types.Text "done"
    ; Types.ToolUse { id = "tool-1"; name = "lookup"; input = `Assoc [] }
    ]
  in
  let response = make_response ~content ~usage:(make_usage ()) () in
  let transport =
    { Llm_transport.complete_sync =
        (fun request ->
          incr calls;
          check
            bool
            "traceparent replaced"
            true
            (List.mem ("traceparent", "new") request.config.headers);
          check
            bool
            "tracestate appended"
            true
            (List.mem ("tracestate", "state") request.config.headers);
          check
            bool
            "base header preserved"
            true
            (List.mem ("x-base", "base") request.config.headers);
          { Llm_transport.response = Ok response; latency_ms = Some 42 })
    ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _request -> Ok response)
    }
  in
  let messages = [ Types.user_msg "hello" ] in
  let first =
    Complete.complete
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~transport
      ~config
      ~messages
      ~trace_context:[ "traceparent", "new"; "tracestate", "state" ]
      ~cache
      ~metrics
      ()
  in
  (match first with
   | Ok resp ->
     check string "model patched" "openai-test" resp.model;
     (match resp.telemetry with
      | Some t ->
        check (option int) "latency patched" (Some 42) t.request_latency_ms;
        check (option string) "canonical model" (Some "openai-test") t.canonical_model_id
      | None -> fail "expected telemetry")
   | Error err -> failf "unexpected complete error: %s" (string_of_http_error err));
  let second =
    Complete.complete
      ~sw
      ~net:(Eio.Stdenv.net env)
      ~transport
      ~config
      ~messages
      ~trace_context:[ "traceparent", "new"; "tracestate", "state" ]
      ~cache
      ~metrics
      ()
  in
  (match second with
   | Ok resp -> check string "cached text" "done" (Types.text_of_response resp)
   | Error err -> failf "unexpected cached error: %s" (string_of_http_error err));
  check int "transport called once" 1 !calls;
  check int "cache miss once" 1 probe.cache_misses;
  check int "cache hit once" 1 probe.cache_hits;
  check int "request start once" 1 probe.starts;
  check int "request end once" 1 probe.ends;
  check (list int) "status 200" [ 200 ] (List.rev probe.statuses);
  check (list (pair int int)) "usage" [ 11, 7 ] (List.rev probe.token_usage);
  check (list int) "tool calls" [ 1 ] (List.rev probe.tool_calls)
;;

let test_complete_with_retry_retries_then_success () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let probe = metric_probe () in
  let metrics = metrics_of_probe probe in
  let attempts = ref 0 in
  let config = make_config ~model_id:"openai-retry" () in
  let response = make_response ~model:"openai-retry" ~usage:(make_usage ()) () in
  let transport =
    transport_of_sync (fun () ->
      incr attempts;
      if !attempts < 3
      then
        { Llm_transport.response =
            Error (Http_client.HttpError { code = 500; body = "temporary" })
        ; latency_ms = Some 3
        }
      else { Llm_transport.response = Ok response; latency_ms = Some 9 })
  in
  let retry_config =
    { Complete.max_retries = 3
    ; initial_delay_sec = 0.0
    ; max_delay_sec = 0.0
    ; backoff_multiplier = 1.0
    }
  in
  (match
     Complete.complete_with_retry
       ~sw
       ~net:(Eio.Stdenv.net env)
       ~transport
       ~clock:(Eio.Stdenv.clock env)
       ~config
       ~messages:[ Types.user_msg "hello" ]
       ~retry_config
       ~metrics
       ()
   with
   | Ok resp -> check string "retry success" "openai-retry" resp.model
   | Error err -> failf "unexpected retry error: %s" (string_of_http_error err));
  check int "attempts" 3 !attempts;
  check (list int) "retry callbacks" [ 1; 2 ] (List.rev probe.retries);
  check (list int) "statuses" [ 500; 500; 200 ] (List.rev probe.statuses);
  check (list string) "errors" [ "HTTP 500"; "HTTP 500" ] (List.rev probe.errors)
;;

let test_complete_stream_transport_success_metrics_and_telemetry () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let probe = metric_probe () in
  let metrics = metrics_of_probe probe in
  let seen_events = ref [] in
  let seen_telemetry = ref [] in
  let config =
    make_config ~model_id:"openai-stream" ~headers:[ "traceparent", "old-stream" ] ()
  in
  let response =
    make_response
      ~content:
        [ Types.Text "streamed"
        ; Types.ToolUse { id = "tool-stream"; name = "search"; input = `Assoc [] }
        ]
      ()
  in
  let transport =
    { Llm_transport.complete_sync =
        (fun _request -> { Llm_transport.response = Ok response; latency_ms = Some 1 })
    ; complete_stream =
        (fun ?on_telemetry ~on_event request ->
          check
            bool
            "stream trace replaced"
            true
            (List.mem ("traceparent", "new-stream") request.config.headers);
          let event = Types.Ping in
          on_event event;
          seen_events := event :: !seen_events;
          (match on_telemetry with
           | Some emit ->
             emit
               (Telemetry_event.Streaming_first_chunk
                  { provider = "openai"
                  ; model = request.config.model_id
                  ; ttfrc_ms = Some 1.5
                  ; requested_at = 0.0
                  })
           | None -> ());
          Ok response)
    }
  in
  (match
     Complete.complete_stream
       ~sw
       ~net:(Eio.Stdenv.net env)
       ~transport
       ~config
       ~messages:[ Types.user_msg "hello stream" ]
       ~trace_context:[ "traceparent", "new-stream" ]
       ~on_event:(fun _event -> ())
       ~on_telemetry:(fun event -> seen_telemetry := event :: !seen_telemetry)
       ~metrics
       ()
   with
   | Ok resp ->
     check string "stream model patched" "openai-stream" resp.model;
     (match resp.telemetry with
      | Some t ->
        check bool "stream latency patched" true (Option.is_some t.request_latency_ms);
        check
          (option string)
          "stream canonical model"
          (Some "openai-stream")
          t.canonical_model_id
      | None -> fail "expected stream telemetry")
   | Error err -> failf "unexpected stream error: %s" (string_of_http_error err));
  check int "event observed" 1 (List.length !seen_events);
  check int "telemetry observed" 1 (List.length !seen_telemetry);
  check (list int) "stream tool calls" [ 1 ] (List.rev probe.tool_calls);
  check
    (list (float 0.001))
    "first chunk metric"
    [ 1.5 ]
    (List.rev probe.streaming_first_chunks)
;;

let () =
  run
    "complete_ext"
    [ ( "is_retryable"
      , [ test_case "429 rate limit" `Quick test_retryable_429
        ; test_case "500 server" `Quick test_retryable_500
        ; test_case "502 bad gateway" `Quick test_retryable_502
        ; test_case "503 unavailable" `Quick test_retryable_503
        ; test_case "529 overloaded" `Quick test_retryable_529
        ; test_case "400 bad request" `Quick test_not_retryable_400
        ; test_case "401 unauthorized" `Quick test_not_retryable_401
        ; test_case "403 forbidden" `Quick test_not_retryable_403
        ; test_case "404 not found" `Quick test_not_retryable_404
        ; test_case "422 unprocessable" `Quick test_not_retryable_422
        ; test_case "200 success" `Quick test_not_retryable_200
        ; test_case "network error" `Quick test_retryable_network
        ] )
    ; ( "default_retry_config"
      , [ test_case "max retries" `Quick test_retry_config_max
        ; test_case "initial delay" `Quick test_retry_config_initial
        ; test_case "max delay" `Quick test_retry_config_max_delay
        ; test_case "backoff" `Quick test_retry_config_backoff
        ] )
    ; ( "helpers"
      , [ test_case "gemini URL variants" `Quick test_gemini_url_variants
        ; test_case
            "sampling defaults and overlay"
            `Quick
            test_sampling_defaults_and_overlay
        ] )
    ; ( "complete"
      , [ test_case
            "transport success cache metrics and trace headers"
            `Quick
            test_complete_transport_success_cache_metrics_and_trace_headers
        ; test_case
            "retry retries then success"
            `Quick
            test_complete_with_retry_retries_then_success
        ; test_case
            "stream transport success metrics and telemetry"
            `Quick
            test_complete_stream_transport_success_metrics_and_telemetry
        ] )
    ]
;;
