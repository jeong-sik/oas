open Alcotest
open Llm_provider

let mock_response text : Types.api_response =
  {
    id = "resp-1";
    model = "mock";
    stop_reason = Types.EndTurn;
    content = [Types.Text text];
    usage = None;
    telemetry = None;
  }

let scripted_transport scripted_responses request_count : Llm_transport.t =
  let responses = ref scripted_responses in
  {
    complete_sync =
      (fun _req ->
        incr request_count;
        match !responses with
        | next :: rest ->
            responses := rest;
            { Llm_transport.response = next; latency_ms = 1 }
        | [] ->
            failwith "scripted transport exhausted");
    complete_stream =
      (fun ~on_event:_ _req ->
        failwith "stream transport not used in this test");
  }

let make_config base_url =
  Provider_config.make ~kind:Provider_config.Anthropic
    ~model_id:"test-model" ~base_url ~request_path:"/v1/messages"
    ~temperature:0.0 ~max_tokens:100 ()

let messages = [Types.user_msg "hello"]

let fast_retry_config : Complete.retry_config = {
  max_retries = 2;
  initial_delay_sec = 0.001;
  max_delay_sec = 0.002;
  backoff_multiplier = 2.0;
}

let hard_quota_body =
  {|{"error":{"message":"Insufficient balance or no resource package. Please recharge.","retry_after":5.0}}|}

let malformed_json_body =
  {|{"error":"Value looks like object, but can't find closing '}' symbol"}|}

let test_is_retryable_hard_quota_429 () =
  check bool "hard quota 429 is not retryable" false
    (Complete.is_retryable
       (Http_client.HttpError { code = 429; body = hard_quota_body }))

let test_is_retryable_malformed_json_400 () =
  check bool "malformed json 400 is retryable" true
    (Complete.is_retryable
       (Http_client.HttpError { code = 400; body = malformed_json_body }))

let test_complete_with_retry_stops_on_hard_quota_429 () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let request_count = ref 0 in
    let transport =
      scripted_transport
        [
          Error (Http_client.HttpError { code = 429; body = hard_quota_body });
        ]
        request_count
    in
    let config = make_config "http://unused.test" in
    (match Complete.complete_with_retry ~sw ~net:env#net ~transport ~clock
             ~config ~messages ~retry_config:fast_retry_config () with
     | Ok _ -> fail "expected hard quota failure"
     | Error (Http_client.HttpError { code; _ }) ->
       check int "status" 429 code;
       check int "single request" 1 !request_count;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected HttpError");
  with Exit -> ()

let test_complete_with_retry_retries_malformed_json_400 () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run @@ fun sw ->
    let request_count = ref 0 in
    let transport =
      scripted_transport
        [
          Error (Http_client.HttpError { code = 400; body = malformed_json_body });
          Ok (mock_response "recovered after retry");
        ]
        request_count
    in
    let config = make_config "http://unused.test" in
    (match Complete.complete_with_retry ~sw ~net:env#net ~transport ~clock
             ~config ~messages ~retry_config:fast_retry_config () with
     | Ok resp ->
       let text =
         List.filter_map
           (function Types.Text s -> Some s | _ -> None)
           resp.content
         |> String.concat ""
       in
       check string "response text" "recovered after retry" text;
       check int "two requests" 2 !request_count;
       Eio.Switch.fail sw Exit
     | Error _ -> fail "expected recovery after malformed json")
  with Exit -> ()

let () =
  run "complete_retry"
    [
      ( "classification",
        [
          test_case "hard quota 429" `Quick test_is_retryable_hard_quota_429;
          test_case "malformed json 400" `Quick
            test_is_retryable_malformed_json_400;
        ] );
      ( "retry loop",
        [
          test_case "hard quota stops immediately" `Quick
            test_complete_with_retry_stops_on_hard_quota_429;
          test_case "malformed json retries same provider" `Quick
            test_complete_with_retry_retries_malformed_json_400;
        ] );
    ]
