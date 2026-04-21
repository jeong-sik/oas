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

let test_classify_error_edge_cases () =
  (* 429 with retry_after field *)
  (match Retry.classify_error ~status:429 ~body:{|{"error":{"message":"slow down","retry_after":2.5}}|} with
   | Retry.RateLimited { retry_after = Some ra; _ } ->
       check (float 0.01) "retry_after parsed" 2.5 ra
   | Retry.RateLimited { retry_after = None; _ } ->
       fail "expected retry_after to be Some"
   | _ -> fail "expected RateLimited");
  (* 422 -> InvalidRequest *)
  (match Retry.classify_error ~status:422 ~body:"validation error" with
   | Retry.InvalidRequest { message } ->
       check string "422 message" "validation error" message
   | _ -> fail "expected InvalidRequest for 422");
  (* 502 -> ServerError *)
  (match Retry.classify_error ~status:502 ~body:"bad gateway" with
   | Retry.ServerError { status; _ } ->
       check int "502 status" 502 status
   | _ -> fail "expected ServerError for 502");
  (* malformed JSON body -> falls back to raw body *)
  (match Retry.classify_error ~status:500 ~body:"not json at all" with
   | Retry.ServerError { message; _ } ->
       check string "raw body fallback" "not json at all" message
   | _ -> fail "expected ServerError with raw body");
  (* 404 -> NotFound *)
  (match Retry.classify_error ~status:404 ~body:"not found" with
   | Retry.NotFound { message } ->
       check string "404 not found" "not found" message
   | _ -> fail "expected NotFound for 404")

let test_is_retryable () =
  check bool "rate limited retryable" true
    (Retry.is_retryable (Retry.RateLimited { retry_after = None; message = "" }));
  check bool "overloaded retryable" true
    (Retry.is_retryable (Retry.Overloaded { message = "" }));
  check bool "server retryable" true
    (Retry.is_retryable (Retry.ServerError { status = 500; message = "" }));
  check bool "network retryable" true
    (Retry.is_retryable (Retry.NetworkError { message = "" }));
  check bool "timeout retryable" true
    (Retry.is_retryable (Retry.Timeout { message = "" }));
  check bool "auth not retryable" false
    (Retry.is_retryable (Retry.AuthError { message = "" }));
  check bool "invalid request not retryable" false
    (Retry.is_retryable (Retry.InvalidRequest { message = "" }));
  check bool "not found not retryable" false
    (Retry.is_retryable (Retry.NotFound { message = "" }))

let test_error_message_all_variants () =
  let cases = [
    (Retry.RateLimited { retry_after = None; message = "slow" }, "Rate limited: slow");
    (Retry.Overloaded { message = "busy" }, "Overloaded: busy");
    (Retry.ServerError { status = 503; message = "down" }, "Server error 503: down");
    (Retry.AuthError { message = "bad key" }, "Auth error: bad key");
    (Retry.InvalidRequest { message = "wrong" }, "Invalid request: wrong");
    (Retry.NotFound { message = "no model" }, "Not found: no model");
    (Retry.NetworkError { message = "dns" }, "Network error: dns");
    (Retry.Timeout { message = "10s" }, "Timeout: 10s");
  ] in
  List.iter (fun (err, expected) ->
    check string "error_message" expected (Retry.error_message err)
  ) cases

let test_is_context_overflow_message () =
  check bool "raw overflow message" true
    (Retry.is_context_overflow_message
       "Invalid request: request (11447 tokens) exceeds the available context size (8192 tokens), try increasing it");
  check bool "raw non-overflow message" false
    (Retry.is_context_overflow_message
       "Invalid request: bad tool schema");
  check bool "json overflow message" true
    (Retry.is_context_overflow_message
       {|{"error":{"message":"This model's maximum context length is 128000 tokens. available context size (32768)"}}|});
  check bool "malformed json falls back to raw body" true
    (Retry.is_context_overflow_message
       {|{"error":"available context size (4096)"|});
  check bool "generic bad request" false
    (Retry.is_context_overflow_message
       {|{"error":{"message":"bad tool schema"}}|})

let test_calculate_delay_ranges () =
  Random.init 0;
  let config = Retry.default_config in
  let delay0 = Retry.calculate_delay config 0 in
  check bool "attempt 0 range" true (delay0 >= 0.5 && delay0 <= 1.5);
  let delay1 = Retry.calculate_delay config 1 in
  check bool "attempt 1 range" true (delay1 >= 1.0 && delay1 <= 3.0);
  let delay2 = Retry.calculate_delay config 2 in
  check bool "attempt 2 range" true (delay2 >= 2.0 && delay2 <= 6.0)

let test_calculate_delay_cap () =
  let config : Retry.retry_config = {
    max_retries = 10;
    initial_delay = 1.0;
    max_delay = 5.0;
    backoff_factor = 2.0;
  } in
  (* At attempt 10, base = 1.0 * 2^10 = 1024, capped at 5.0 *)
  (* With jitter: [5.0 * 0.5, 5.0 * 1.5] = [2.5, 7.5] *)
  for _ = 0 to 9 do
    let delay = Retry.calculate_delay config 10 in
    check bool "capped upper bound" true (delay <= 7.5);
    check bool "capped lower bound" true (delay >= 2.5)
  done

(* --- with_retry tests (require Eio runtime) --- *)

let fast_config : Retry.retry_config = {
  max_retries = 3;
  initial_delay = 0.001;
  max_delay = 0.005;
  backoff_factor = 2.0;
}

let test_with_retry_succeeds_first_try () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  match Retry.with_retry ~clock ~config:fast_config (fun () -> Ok 42) with
  | Ok v -> check int "immediate success" 42 v
  | Error e -> fail (Printf.sprintf "unexpected error: %s" (Retry.error_message e))

let test_with_retry_succeeds_after_retries () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    if !attempt < 3 then Error (Retry.ServerError { status = 500; message = "transient" })
    else Ok "recovered"
  in
  (match Retry.with_retry ~clock ~config:fast_config f with
   | Ok v -> check string "eventual success" "recovered" v
   | Error e -> fail (Printf.sprintf "unexpected error: %s" (Retry.error_message e)));
  check int "took 3 attempts" 3 !attempt

let test_with_retry_non_retryable_stops () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    Error (Retry.AuthError { message = "invalid" })
  in
  let res = Retry.with_retry ~clock ~config:fast_config f in
  (match res with
   | Error (Retry.AuthError _) -> ()
   | _ -> fail "expected AuthError");
  check int "only 1 attempt" 1 !attempt

let test_with_retry_rate_limited_uses_retry_after () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    if !attempt < 2 then
      Error (Retry.RateLimited { retry_after = Some 0.001; message = "slow down" })
    else Ok "ok"
  in
  (match Retry.with_retry ~clock ~config:fast_config f with
   | Ok v -> check string "recovered after rate limit" "ok" v
   | Error e -> fail (Printf.sprintf "unexpected: %s" (Retry.error_message e)));
  check int "2 attempts" 2 !attempt

let test_with_retry_non_retryable_during_loop () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    if !attempt = 1 then
      Error (Retry.ServerError { status = 500; message = "transient" })
    else
      Error (Retry.InvalidRequest { message = "bad input" })
  in
  let res = Retry.with_retry ~clock ~config:fast_config f in
  (match res with
   | Error (Retry.InvalidRequest { message }) ->
       check string "non-retryable in loop" "bad input" message
   | _ -> fail "expected InvalidRequest from loop");
  check int "stopped at 2" 2 !attempt

type mapped_error =
  | Retryable of Retry.api_error
  | HardFail of string

let test_with_retry_map_error_succeeds_after_retries () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    if !attempt < 3 then
      Error (Retryable (Retry.ServerError { status = 500; message = "transient" }))
    else Ok "recovered"
  in
  (match
     Retry.with_retry_map_error ~clock ~config:fast_config
       ~classify:(function Retryable err -> Some err | HardFail _ -> None)
       f
   with
   | Ok v -> check string "eventual success" "recovered" v
   | Error _ -> fail "expected recovery after mapped retry");
  check int "took 3 attempts" 3 !attempt

let test_with_retry_map_error_preserves_original_error () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    Error (HardFail "missing transport")
  in
  (match
     Retry.with_retry_map_error ~clock ~config:fast_config
       ~classify:(function Retryable err -> Some err | HardFail _ -> None)
       f
   with
   | Error (HardFail message) ->
       check string "original error preserved" "missing transport" message
   | _ -> fail "expected original HardFail");
  check int "only 1 attempt" 1 !attempt

let test_with_retry_max_retries_exhausted () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let attempt = ref 0 in
  let f () =
    incr attempt;
    Error (Retry.NetworkError { message = "unreachable" })
  in
  (* 1 initial + 3 retries = 4 total attempts *)
  let res = Retry.with_retry ~clock ~config:fast_config f in
  (match res with
   | Error (Retry.NetworkError _) -> ()
   | _ -> fail "expected NetworkError after exhaustion");
  check int "1 + max_retries attempts" 4 !attempt

let test_provider_constructors () =
  let p = Provider.local_llm () in
  check string "local_llm" "default" p.model_id;
  let p = Provider.anthropic_sonnet () in
  check string "anthropic_sonnet" "claude-sonnet-4-6" p.model_id;
  let p = Provider.anthropic_haiku () in
  check string "anthropic_haiku" "claude-haiku-4-5-20251001" p.model_id;
  let p = Provider.anthropic_opus () in
  check string "anthropic_opus" "claude-opus-4-6" p.model_id;
  let p = Provider.openrouter () in
  check string "openrouter default" "anthropic/claude-sonnet-4-6" p.model_id;
  let p = Provider.openrouter ~model_id:"google/gemini-2.5-pro" () in
  check string "openrouter override" "google/gemini-2.5-pro" p.model_id

let () =
  run "Retry" [
    "classify", [
      test_case "http status mapping" `Quick test_classify_error;
      test_case "edge cases" `Quick test_classify_error_edge_cases;
    ];
    "retryability", [
      test_case "retryable predicates" `Quick test_is_retryable;
      test_case "error_message all variants" `Quick test_error_message_all_variants;
      test_case "context overflow classification" `Quick
        test_is_context_overflow_message;
      test_case "delay ranges" `Quick test_calculate_delay_ranges;
      test_case "delay cap" `Quick test_calculate_delay_cap;
    ];
    "with_retry", [
      test_case "succeeds first try" `Quick test_with_retry_succeeds_first_try;
      test_case "succeeds after retries" `Quick test_with_retry_succeeds_after_retries;
      test_case "rate limited uses retry_after" `Quick test_with_retry_rate_limited_uses_retry_after;
      test_case "non-retryable stops immediately" `Quick test_with_retry_non_retryable_stops;
      test_case "non-retryable during loop" `Quick test_with_retry_non_retryable_during_loop;
      test_case "map_error retries classified errors" `Quick test_with_retry_map_error_succeeds_after_retries;
      test_case "map_error preserves original errors" `Quick test_with_retry_map_error_preserves_original_error;
      test_case "max retries exhausted" `Quick test_with_retry_max_retries_exhausted;
    ];
    "providers", [
      test_case "constructor defaults" `Quick test_provider_constructors;
    ];
  ]
