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

let make_config base_url =
  Provider_config.make
    ~kind:Provider_config.Anthropic
    ~model_id:"test-model"
    ~base_url
    ~request_path:"/v1/messages"
    ~temperature:0.0
    ~max_tokens:100
    ()
;;

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
        ] )
    ]
;;
