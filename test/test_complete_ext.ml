(** Extended coverage tests for Complete module — public API only.
    Targets: is_retryable and default_retry_config. *)

open Alcotest
open Llm_provider

(* ── is_retryable ────────────────────────────────────── *)

let test_retryable_429 () =
  check bool "429" true
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = "" }))

let test_retryable_500 () =
  check bool "500" true
    (Complete.is_retryable (Http_client.HttpError { code = 500; body = "" }))

let test_retryable_502 () =
  check bool "502" true
    (Complete.is_retryable (Http_client.HttpError { code = 502; body = "" }))

let test_retryable_503 () =
  check bool "503" true
    (Complete.is_retryable (Http_client.HttpError { code = 503; body = "" }))

let test_retryable_529 () =
  check bool "529" true
    (Complete.is_retryable (Http_client.HttpError { code = 529; body = "" }))

let test_not_retryable_400 () =
  check bool "400" false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }))

let test_not_retryable_401 () =
  check bool "401" false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }))

let test_not_retryable_403 () =
  check bool "403" false
    (Complete.is_retryable (Http_client.HttpError { code = 403; body = "" }))

let test_not_retryable_404 () =
  check bool "404" false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }))

let test_not_retryable_422 () =
  check bool "422" false
    (Complete.is_retryable (Http_client.HttpError { code = 422; body = "" }))

let test_retryable_network () =
  check bool "network error" true
    (Complete.is_retryable
       (Http_client.NetworkError { message = "connection refused"; kind = Unknown }))

let test_not_retryable_200 () =
  (* 200 is not an error, but is_retryable should return false *)
  check bool "200" false
    (Complete.is_retryable (Http_client.HttpError { code = 200; body = "" }))

(* ── default_retry_config ────────────────────────────── *)

let test_retry_config_max () =
  check int "max_retries" 3
    Complete.default_retry_config.max_retries

let test_retry_config_initial () =
  check (float 0.01) "initial_delay" 1.0
    Complete.default_retry_config.initial_delay_sec

let test_retry_config_max_delay () =
  check (float 0.01) "max_delay" 30.0
    Complete.default_retry_config.max_delay_sec

let test_retry_config_backoff () =
  check (float 0.01) "backoff" 2.0
    Complete.default_retry_config.backoff_multiplier

(* ── Runner ──────────────────────────────────────────── *)

let () =
  run "complete_ext" [
    "is_retryable", [
      test_case "429 rate limit" `Quick test_retryable_429;
      test_case "500 server" `Quick test_retryable_500;
      test_case "502 bad gateway" `Quick test_retryable_502;
      test_case "503 unavailable" `Quick test_retryable_503;
      test_case "529 overloaded" `Quick test_retryable_529;
      test_case "400 bad request" `Quick test_not_retryable_400;
      test_case "401 unauthorized" `Quick test_not_retryable_401;
      test_case "403 forbidden" `Quick test_not_retryable_403;
      test_case "404 not found" `Quick test_not_retryable_404;
      test_case "422 unprocessable" `Quick test_not_retryable_422;
      test_case "200 success" `Quick test_not_retryable_200;
      test_case "network error" `Quick test_retryable_network;
    ];
    "default_retry_config", [
      test_case "max retries" `Quick test_retry_config_max;
      test_case "initial delay" `Quick test_retry_config_initial;
      test_case "max delay" `Quick test_retry_config_max_delay;
      test_case "backoff" `Quick test_retry_config_backoff;
    ];
  ]
