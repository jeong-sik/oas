open Alcotest
open Llm_provider

let mock_response text : Types.api_response =
  { id = "resp-1"
  ; model = "mock"
  ; stop_reason = Types.EndTurn
  ; content = [ Types.Text text ]
  ; usage = None
  ; telemetry = None
  }
;;

let scripted_transport scripted_responses request_count : Llm_transport.t =
  let responses = ref scripted_responses in
  { complete_sync =
      (fun _req ->
        incr request_count;
        match !responses with
        | next :: rest ->
          responses := rest;
          { Llm_transport.response = next; latency_ms = Some 1 }
        | [] -> failwith "scripted transport exhausted")
  ; complete_stream =
      (fun ?on_telemetry:_ ~on_event:_ _req ->
        incr request_count;
        match !responses with
        | next :: rest ->
          responses := rest;
          next
        | [] -> failwith "scripted transport exhausted")
  }
;;

let eventful_stream_transport scripted_attempts request_count : Llm_transport.t =
  let attempts = ref scripted_attempts in
  { complete_sync =
      (fun _req -> invalid_arg "eventful_stream_transport.complete_sync is not used")
  ; complete_stream =
      (fun ?on_telemetry:_ ~on_event _req ->
        incr request_count;
        match !attempts with
        | (events, response) :: rest ->
          attempts := rest;
          List.iter on_event events;
          response
        | [] -> invalid_arg "eventful_stream_transport exhausted")
  }
;;

let make_config_for_kind kind base_url =
  Provider_config.make
    ~kind
    ~model_id:"test-model"
    ~base_url
    ~temperature:0.0
    ~max_tokens:100
    ()
;;

let make_config = make_config_for_kind Provider_config.Anthropic
let messages = [ Types.user_msg "hello" ]

let fast_retry_config : Complete.retry_config =
  { max_retries = 2
  ; initial_delay_sec = 0.001
  ; max_delay_sec = 0.002
  ; backoff_multiplier = 2.0
  }
;;

let hard_quota_body =
  {|{"error":{"message":"Insufficient balance or no resource package. Please recharge.","retry_after":5.0}}|}
;;

let provider_malformed_json_prose_body =
  {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}
;;

let provider_parse_error =
  Http_client.ProviderFailure
    { kind = Http_client.Provider_parse_error { parser = Some "glm" }
    ; message = "Unexpected end of input"
    }
;;

let retryable_stream_error =
  Http_client.NetworkError
    { message = "stream connection closed"; kind = Http_client.Unknown }
;;

let test_is_retryable_hard_quota_429 () =
  check
    bool
    "hard quota 429 is not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = hard_quota_body }))
;;

let test_is_retryable_provider_malformed_json_prose_400 () =
  check
    bool
    "provider prose 400 is not retryable"
    false
    (Complete.is_retryable
       (Http_client.HttpError { code = 400; body = provider_malformed_json_prose_body }))
;;

let test_is_retryable_provider_parse_error () =
  check
    bool
    "typed provider parse error is not retryable"
    false
    (Complete.is_retryable
       (Http_client.ProviderFailure
          { kind = Http_client.Provider_parse_error { parser = Some "glm" }
          ; message = "Unexpected end of input"
          }))
;;

let test_complete_with_retry_stops_on_hard_quota_429 () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let request_count = ref 0 in
    let transport =
      scripted_transport
        [ Error (Http_client.HttpError { code = 429; body = hard_quota_body }) ]
        request_count
    in
    let config = make_config "http://unused.test" in
    match
      Complete.complete_with_retry
        ~sw
        ~net:env#net
        ~transport
        ~clock
        ~config
        ~messages
        ~retry_config:fast_retry_config
        ()
    with
    | Ok _ -> fail "expected hard quota failure"
    | Error (Http_client.HttpError { code; _ }) ->
      check int "status" 429 code;
      check int "single request" 1 !request_count;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected HttpError"
  with
  | Exit -> ()
;;

let test_complete_with_retry_stops_on_provider_parse_error () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let request_count = ref 0 in
    let transport =
      scripted_transport
        [ Error provider_parse_error; Ok (mock_response "must not retry") ]
        request_count
    in
    let config = make_config "http://unused.test" in
    match
      Complete.complete_with_retry
        ~sw
        ~net:env#net
        ~transport
        ~clock
        ~config
        ~messages
        ~retry_config:fast_retry_config
        ()
    with
    | Ok _ -> fail "expected provider parse failure"
    | Error (Http_client.ProviderFailure { kind = Provider_parse_error _; _ }) ->
      check int "single request" 1 !request_count;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected provider parse failure"
  with
  | Exit -> ()
;;

let test_complete_stream_with_retry_stops_on_hard_quota_429 () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let request_count = ref 0 in
    let transport =
      scripted_transport
        [ Error (Http_client.HttpError { code = 429; body = hard_quota_body }) ]
        request_count
    in
    let config = make_config "http://unused.test" in
    let on_event _ = () in
    match
      Complete.complete_stream_with_retry
        ~sw
        ~net:env#net
        ~transport
        ~clock
        ~config
        ~messages
        ~retry_config:fast_retry_config
        ~on_event
        ()
    with
    | Ok _ -> fail "expected hard quota failure"
    | Error (Http_client.HttpError { code; _ }) ->
      check int "status" 429 code;
      check int "single request" 1 !request_count;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected HttpError"
  with
  | Exit -> ()
;;

let test_complete_stream_with_retry_stops_on_provider_parse_error () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let request_count = ref 0 in
    let transport =
      scripted_transport
        [ Error provider_parse_error; Ok (mock_response "must not retry") ]
        request_count
    in
    let config = make_config "http://unused.test" in
    let on_event _ = () in
    match
      Complete.complete_stream_with_retry
        ~sw
        ~net:env#net
        ~transport
        ~clock
        ~config
        ~messages
        ~retry_config:fast_retry_config
        ~on_event
        ()
    with
    | Ok _ -> fail "expected provider parse failure"
    | Error (Http_client.ProviderFailure { kind = Provider_parse_error _; _ }) ->
      check int "single request" 1 !request_count;
      Eio.Switch.fail sw Exit
    | Error _ -> fail "expected provider parse failure"
  with
  | Exit -> ()
;;

let semantic_stream_events : (string * Types.sse_event) list =
  [ "text delta", Types.ContentBlockDelta { index = 0; delta = Types.TextDelta "partial" }
  ; ( "thinking delta"
    , Types.ContentBlockDelta { index = 0; delta = Types.ThinkingDelta "partial" } )
  ; ( "tool start"
    , Types.ContentBlockStart
        { index = 0
        ; content_type = "tool_use"
        ; tool_id = Some "call-1"
        ; tool_name = Some "lookup"
        } )
  ; ( "tool argument delta"
    , Types.ContentBlockDelta
        { index = 0; delta = Types.InputJsonDelta {|{"query":"partial"}|} } )
  ]
;;

let provider_kinds : (string * Provider_config.provider_kind) list =
  [ "Anthropic", Provider_config.Anthropic
  ; "OpenAI-compatible", Provider_config.OpenAI_compat
  ; "Gemini", Provider_config.Gemini
  ]
;;

let test_stream_retry_stops_after_semantic_event_for_every_provider () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run
  @@ fun sw ->
  List.iter
    (fun (provider_label, kind) ->
       List.iter
         (fun (event_label, event) ->
            let request_count = ref 0 in
            let observed_events = ref [] in
            let transport =
              eventful_stream_transport
                [ [ event ], Error retryable_stream_error
                ; [ event ], Ok (mock_response "must not be appended")
                ]
                request_count
            in
            let config = make_config_for_kind kind "http://unused.test" in
            let result =
              Complete.complete_stream_with_retry
                ~sw
                ~net:env#net
                ~transport
                ~clock
                ~config
                ~messages
                ~retry_config:fast_retry_config
                ~on_event:(fun observed ->
                  observed_events := observed :: !observed_events)
                ()
            in
            let label = provider_label ^ " / " ^ event_label in
            (match result with
             | Error
                 (Http_client.NetworkError
                    { message = "stream connection closed"; kind = Http_client.Unknown })
               -> ()
             | Error _ -> fail (label ^ ": expected the original typed stream error")
             | Ok _ -> fail (label ^ ": retry appended a second provider attempt"));
            check int (label ^ ": one provider request") 1 !request_count;
            check int (label ^ ": one observable event") 1 (List.length !observed_events))
         semantic_stream_events)
    provider_kinds
;;

let test_stream_retry_remains_available_before_semantic_event () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  Eio.Switch.run
  @@ fun sw ->
  let request_count = ref 0 in
  let transport =
    eventful_stream_transport
      [ [], Error retryable_stream_error; [], Ok (mock_response "retried") ]
      request_count
  in
  let config = make_config "http://unused.test" in
  match
    Complete.complete_stream_with_retry
      ~sw
      ~net:env#net
      ~transport
      ~clock
      ~config
      ~messages
      ~retry_config:fast_retry_config
      ~on_event:(fun _ -> ())
      ()
  with
  | Error _ -> fail "expected a pre-stream retry to succeed"
  | Ok response ->
    check int "two provider requests" 2 !request_count;
    (match response.Types.content with
     | [ Types.Text "retried" ] -> ()
     | _ -> fail "expected the second attempt response")
;;

let () =
  run
    "complete_retry"
    [ ( "classification"
      , [ test_case "hard quota 429" `Quick test_is_retryable_hard_quota_429
        ; test_case
            "provider malformed json prose 400"
            `Quick
            test_is_retryable_provider_malformed_json_prose_400
        ; test_case "provider parse error" `Quick test_is_retryable_provider_parse_error
        ] )
    ; ( "retry loop"
      , [ test_case
            "hard quota stops immediately"
            `Quick
            test_complete_with_retry_stops_on_hard_quota_429
        ; test_case
            "provider parse error stops immediately"
            `Quick
            test_complete_with_retry_stops_on_provider_parse_error
        ; test_case
            "stream hard quota stops immediately"
            `Quick
            test_complete_stream_with_retry_stops_on_hard_quota_429
        ; test_case
            "stream provider parse error stops immediately"
            `Quick
            test_complete_stream_with_retry_stops_on_provider_parse_error
        ; test_case
            "stream semantic event commits attempt across providers"
            `Quick
            test_stream_retry_stops_after_semantic_event_for_every_provider
        ; test_case
            "stream pre-event failure remains retryable"
            `Quick
            test_stream_retry_remains_available_before_semantic_event
        ] )
    ]
;;
