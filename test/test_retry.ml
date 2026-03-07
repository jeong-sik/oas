open Alcotest
open Agent_sdk

let expect_rate_limited err =
  match err with
  | Retry.RateLimited { message; _ } ->
      check string "rate limited message" "rate limited" message
  | _ ->
      fail "Expected RateLimited"

let expect_auth_error err =
  match err with
  | Retry.AuthError { message } ->
      check string "auth error message" "invalid key" message
  | _ ->
      fail "Expected AuthError"

let expect_server_error err =
  match err with
  | Retry.ServerError { status; _ } ->
      check int "server status" 500 status
  | _ ->
      fail "Expected ServerError"

let expect_overloaded err =
  match err with
  | Retry.Overloaded _ -> ()
  | _ ->
      fail "Expected Overloaded"

let test_classify_error () =
  Retry.classify_error ~status:429 ~body:{|{"error":{"message":"rate limited"}}|}
  |> expect_rate_limited;
  Retry.classify_error ~status:401 ~body:{|{"error":{"message":"invalid key"}}|}
  |> expect_auth_error;
  Retry.classify_error ~status:500 ~body:"internal error"
  |> expect_server_error;
  Retry.classify_error ~status:529 ~body:"overloaded"
  |> expect_overloaded

let test_is_retryable () =
  check bool "rate limited retryable" true
    (Retry.is_retryable (Retry.RateLimited { retry_after = None; message = "" }));
  check bool "overloaded retryable" true
    (Retry.is_retryable (Retry.Overloaded { message = "" }));
  check bool "server retryable" true
    (Retry.is_retryable (Retry.ServerError { status = 500; message = "" }));
  check bool "network retryable" true
    (Retry.is_retryable (Retry.NetworkError { message = "" }));
  check bool "auth not retryable" false
    (Retry.is_retryable (Retry.AuthError { message = "" }));
  check bool "invalid request not retryable" false
    (Retry.is_retryable (Retry.InvalidRequest { message = "" }))

let test_calculate_delay_ranges () =
  Random.init 0;
  let config = Retry.default_config in
  let delay0 = Retry.calculate_delay config 0 in
  check bool "attempt 0 range" true (delay0 >= 0.5 && delay0 <= 1.5);
  let delay1 = Retry.calculate_delay config 1 in
  check bool "attempt 1 range" true (delay1 >= 1.0 && delay1 <= 3.0);
  let delay2 = Retry.calculate_delay config 2 in
  check bool "attempt 2 range" true (delay2 >= 2.0 && delay2 <= 6.0)

let test_provider_constructors () =
  let p = Provider.local_qwen () in
  check string "local_qwen" "qwen3.5" p.model_id;
  let p = Provider.anthropic_sonnet () in
  check string "anthropic_sonnet" "claude-sonnet-4-6" p.model_id;
  let p = Provider.anthropic_haiku () in
  check string "anthropic_haiku" "claude-haiku-4-5-20251001" p.model_id;
  let p = Provider.anthropic_opus () in
  check string "anthropic_opus" "claude-opus-4-6" p.model_id;
  let p = Provider.local_mlx () in
  check string "local_mlx" "qwen3.5" p.model_id;
  let p = Provider.openrouter () in
  check string "openrouter default" "anthropic/claude-sonnet-4-6" p.model_id;
  let p = Provider.openrouter ~model_id:"google/gemini-2.5-pro" () in
  check string "openrouter override" "google/gemini-2.5-pro" p.model_id

let () =
  run "Retry" [
    "classify", [
      test_case "http status mapping" `Quick test_classify_error;
    ];
    "retryability", [
      test_case "retryable predicates" `Quick test_is_retryable;
      test_case "delay ranges" `Quick test_calculate_delay_ranges;
    ];
    "providers", [
      test_case "constructor defaults" `Quick test_provider_constructors;
    ];
  ]
