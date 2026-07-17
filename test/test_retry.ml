open Alcotest
open Agent_sdk
module Retry = Llm_provider.Retry

let expect_rate_limited err =
  match err with
  | Retry.RateLimited { message; _ } ->
    check string "rate limited message" "rate limited" message
  | _ -> fail "Expected RateLimited"
;;

let expect_auth_error err =
  match err with
  | Retry.AuthError { message } -> check string "auth error message" "invalid key" message
  | _ -> fail "Expected AuthError"
;;

let expect_server_error err =
  match err with
  | Retry.ServerError { status; _ } -> check int "server status" 500 status
  | _ -> fail "Expected ServerError"
;;

let expect_overloaded err =
  match err with
  | Retry.Overloaded _ -> ()
  | _ -> fail "Expected Overloaded"
;;

let test_classify_error () =
  Retry.classify_error
    ~retry_after_header:None
    ~status:429
    ~body:{|{"error":{"message":"rate limited"}}|}
  |> expect_rate_limited;
  Retry.classify_error
    ~retry_after_header:None
    ~status:401
    ~body:{|{"error":{"message":"invalid key"}}|}
  |> expect_auth_error;
  Retry.classify_error ~retry_after_header:None ~status:500 ~body:"internal error"
  |> expect_server_error;
  Retry.classify_error ~retry_after_header:None ~status:529 ~body:"overloaded"
  |> expect_overloaded
;;

let test_classify_error_edge_cases () =
  (* 429 with retry_after field *)
  (match
     Retry.classify_error
       ~retry_after_header:None
       ~status:429
       ~body:{|{"error":{"message":"slow down","retry_after":2.5}}|}
   with
   | Retry.RateLimited { retry_after = Some ra; _ } ->
     check (float 0.01) "retry_after parsed" 2.5 ra
   | Retry.RateLimited { retry_after = None; _ } -> fail "expected retry_after to be Some"
   | _ -> fail "expected RateLimited");
  (* 422 -> InvalidRequest *)
  (match
     Retry.classify_error ~retry_after_header:None ~status:422 ~body:"validation error"
   with
   | Retry.InvalidRequest { message } ->
     check string "422 message" "validation error" message
   | _ -> fail "expected InvalidRequest for 422");
  (* 502 -> ServerError *)
  (match
     Retry.classify_error ~retry_after_header:None ~status:502 ~body:"bad gateway"
   with
   | Retry.ServerError { status; _ } -> check int "502 status" 502 status
   | _ -> fail "expected ServerError for 502");
  (* malformed JSON body -> falls back to raw body *)
  (match
     Retry.classify_error ~retry_after_header:None ~status:500 ~body:"not json at all"
   with
   | Retry.ServerError { message; _ } ->
     check string "raw body fallback" "not json at all" message
   | _ -> fail "expected ServerError with raw body");
  (* 404 -> NotFound *)
  match Retry.classify_error ~retry_after_header:None ~status:404 ~body:"not found" with
  | Retry.NotFound { message } -> check string "404 not found" "not found" message
  | _ -> fail "expected NotFound for 404"
;;

let test_classify_error_402_payment_required () =
  let body = {|{"error":{"message":"Insufficient Balance"}}|} in
  let err = Retry.classify_error ~retry_after_header:None ~status:402 ~body in
  (match err with
   | Retry.PaymentRequired { message } ->
     check string "402 message" "Insufficient Balance" message
   | _ -> fail "expected PaymentRequired for 402");
  check bool "402 is not retryable" false (Retry.is_retryable err);
  check
    string
    "402 error_message rendering"
    "Payment required: Insufficient Balance"
    (Retry.error_message err)
;;

let test_classify_error_403_authorization_denied () =
  let body =
    {|{"error":{"message":"You've reached your usage limit for this billing cycle"}}|}
  in
  let err = Retry.classify_error ~retry_after_header:None ~status:403 ~body in
  (match err with
   | Retry.AuthorizationError { message } ->
     check
       string
       "403 provider detail"
       "You've reached your usage limit for this billing cycle"
       message
   | _ -> fail "expected AuthorizationError for 403");
  check bool "403 is not retryable" false (Retry.is_retryable err)
;;

let test_is_retryable () =
  check
    bool
    "rate limited retryable"
    true
    (Retry.is_retryable (Retry.RateLimited { retry_after = None; message = "" }));
  check
    bool
    "overloaded retryable"
    true
    (Retry.is_retryable (Retry.Overloaded { message = "" }));
  check
    bool
    "server retryable"
    true
    (Retry.is_retryable (Retry.ServerError { status = 500; message = "" }));
  check
    bool
    "network retryable"
    true
    (Retry.is_retryable (Retry.NetworkError { message = ""; kind = Unknown }));
  check
    bool
    "timeout retryable"
    true
    (Retry.is_retryable (Retry.Timeout { message = ""; phase = None }));
  check
    bool
    "auth not retryable"
    false
    (Retry.is_retryable (Retry.AuthError { message = "" }));
  check
    bool
    "invalid request not retryable"
    false
    (Retry.is_retryable
       (Retry.InvalidRequest { message = ""; reason = Unknown_invalid_request }));
  check
    bool
    "not found not retryable"
    false
    (Retry.is_retryable (Retry.NotFound { message = "" }));
  check
    bool
    "payment required not retryable"
    false
    (Retry.is_retryable (Retry.PaymentRequired { message = "" }))
;;

let test_invalid_request_reason_boundary () =
  let expect_unknown body =
    match Retry.classify_error ~retry_after_header:None ~status:400 ~body with
    | Retry.InvalidRequest { reason = Unknown_invalid_request; _ } as err ->
      check bool "generic invalid request is not retryable" false (Retry.is_retryable err)
    | _ -> fail "expected Unknown_invalid_request"
  in
  List.iter
    expect_unknown
    [ {|{"error":{"message":"Unexpected character in user.name string exceeds length"}}|}
    ; {|{"error":{"message":"parse error in query parameters"}}|}
    ; {|{"error":{"message":"unexpected token in tool schema"}}|}
    ; "JSON parse error: unexpected token"
    ];
  check
    bool
    "typed parser-boundary invalid request is retryable"
    true
    (Retry.is_retryable
       (Retry.InvalidRequest
          { message = "JSON parse error: unexpected token"
          ; reason = Retry.Json_parse_error
          }))
;;

let test_error_message_all_variants () =
  let cases =
    [ Retry.RateLimited { retry_after = None; message = "slow" }, "Rate limited: slow"
    ; Retry.Overloaded { message = "busy" }, "Overloaded: busy"
    ; Retry.ServerError { status = 503; message = "down" }, "Server error 503: down"
    ; Retry.AuthError { message = "bad key" }, "Auth error: bad key"
    ; Retry.AuthorizationError { message = "forbidden" }, "Authorization error: forbidden"
    ; ( Retry.PaymentRequired { message = "Insufficient Balance" }
      , "Payment required: Insufficient Balance" )
    ; ( Retry.InvalidRequest { message = "wrong"; reason = Unknown_invalid_request }
      , "Invalid request (unknown): wrong" )
    ; Retry.NotFound { message = "no model" }, "Not found: no model"
    ; Retry.NetworkError { message = "dns"; kind = Unknown }, "Network error: dns"
    ; ( Retry.NetworkError
          { message = "failed to resolve hostname: api.z.ai"; kind = Dns_failure }
      , "Network error (dns_failure): failed to resolve hostname: api.z.ai" )
    ; ( Retry.NetworkError { message = "reset"; kind = Connection_refused }
      , "Network error (connection_refused): reset" )
    ; Retry.Timeout { message = "10s"; phase = None }, "Timeout: 10s"
    ]
  in
  List.iter
    (fun (err, expected) ->
       check string "error_message" expected (Retry.error_message err))
    cases
;;

let test_provider_constructors () =
  let p =
    Provider.local_llm
      ~base_url:Llm_provider.Constants.Endpoints.default_url_localhost
      ~model_id:"test-model"
      ()
  in
  check string "local exact model" "test-model" p.model_id;
  let p = Provider.anthropic ~model_id:"claude-sonnet-4-6" () in
  check string "anthropic exact sonnet" "claude-sonnet-4-6" p.model_id;
  let p = Provider.anthropic ~model_id:"claude-haiku-4-5-20251001" () in
  check string "anthropic exact haiku" "claude-haiku-4-5-20251001" p.model_id;
  let p = Provider.anthropic ~model_id:"claude-opus-4-6" () in
  check string "anthropic exact opus" "claude-opus-4-6" p.model_id;
  let p = Provider.openrouter ~model_id:"test-model" () in
  check string "openrouter exact model" "test-model" p.model_id;
  let p = Provider.openrouter ~model_id:"google/gemini-2.5-pro" () in
  check string "openrouter override" "google/gemini-2.5-pro" p.model_id
;;

let () =
  run
    "Retry"
    [ ( "classify"
      , [ test_case "http status mapping" `Quick test_classify_error
        ; test_case "edge cases" `Quick test_classify_error_edge_cases
        ; test_case "402 payment required" `Quick test_classify_error_402_payment_required
        ; test_case
            "403 authorization denied"
            `Quick
            test_classify_error_403_authorization_denied
        ] )
    ; ( "typed_projection"
      , [ test_case "retryable predicates" `Quick test_is_retryable
        ; test_case
            "invalid request reason boundary"
            `Quick
            test_invalid_request_reason_boundary
        ; test_case "error_message all variants" `Quick test_error_message_all_variants
        ] )
    ; "providers", [ test_case "constructor defaults" `Quick test_provider_constructors ]
    ]
;;
