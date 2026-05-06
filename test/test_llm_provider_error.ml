(** Coverage for [Llm_provider.Error.provider_error], its [to_string]
    formatter, and typed transport/retry mapping (oas#1175 - file at 0% coverage in the
    initial coverage measurement (`66.42%`, run `24887636744`)).

    The formatter has one case per variant; the [to_string] group keeps
    one assertion per [provider_error] variant while the mapping group
    covers the conversion boundaries. *)

open Alcotest
open Llm_provider

let test_missing_api_key () =
  check
    string
    "MissingApiKey format"
    "Missing API key env var: OPENAI_API_KEY"
    (Error.to_string (Error.MissingApiKey { var_name = "OPENAI_API_KEY" }))
;;

let test_invalid_config () =
  check
    string
    "InvalidConfig format"
    "Invalid config 'temperature': must be in [0, 2]"
    (Error.to_string
       (Error.InvalidConfig { field = "temperature"; detail = "must be in [0, 2]" }))
;;

let test_parse_error () =
  check
    string
    "ParseError format"
    "Parse error: unexpected token at line 3"
    (Error.to_string (Error.ParseError { detail = "unexpected token at line 3" }))
;;

let test_unknown_variant () =
  check
    string
    "UnknownVariant format"
    "Unknown stop_reason variant: max_tokens_exceeded"
    (Error.to_string
       (Error.UnknownVariant { type_name = "stop_reason"; value = "max_tokens_exceeded" }))
;;

let test_provider_unavailable () =
  check
    string
    "ProviderUnavailable format"
    "Provider 'anthropic' unavailable: HTTP 503 retry-after exhausted"
    (Error.to_string
       (Error.ProviderUnavailable
          { provider = "anthropic"; detail = "HTTP 503 retry-after exhausted" }))
;;

let test_rate_limit () =
  check
    string
    "RateLimit format"
    "Provider 'anthropic' rate limited: quota window exhausted (retry_after: 1.250s)"
    (Error.to_string
       (Error.RateLimit
          { provider = "anthropic"
          ; retry_after = Some 1.25
          ; detail = "quota window exhausted"
          }))
;;

let test_hard_quota () =
  check
    string
    "HardQuota format"
    "Provider 'zai' hard quota exhausted: no remaining balance"
    (Error.to_string
       (Error.HardQuota
          { provider = "zai"; retry_after = None; detail = "no remaining balance" }))
;;

let test_capacity_exhausted () =
  check
    string
    "CapacityExhausted format"
    "Provider capacity exhausted (model): model queue saturated affected=[gemini-3-pro] \
     (retry_after: 7.000s)"
    (Error.to_string
       (Error.CapacityExhausted
          { scope = Error.CapacityModel
          ; affected = [ "gemini-3-pro" ]
          ; retry_after = Some 7.0
          ; detail = "model queue saturated"
          }))
;;

let test_auth_error () =
  check
    string
    "AuthError format"
    "Provider 'openai' auth error: invalid API key"
    (Error.to_string
       (Error.AuthError { provider = "openai"; detail = "invalid API key" }))
;;

let test_server_error () =
  check
    string
    "ServerError format"
    "Provider 'openai' server error 503 (transient=true): down"
    (Error.to_string
       (Error.ServerError
          { provider = "openai"; code = 503; transient = true; detail = "down" }))
;;

let test_network_error () =
  check
    string
    "NetworkError format"
    "Provider 'local' network error (dns_failure): lookup failed"
    (Error.to_string
       (Error.NetworkError
          { provider = "local"; kind = Http_client.Dns_failure; detail = "lookup failed" }))
;;

let test_timeout () =
  check
    string
    "Timeout format"
    "Provider 'gemini' timeout: request exceeded budget"
    (Error.to_string
       (Error.Timeout { provider = "gemini"; detail = "request exceeded budget" }))
;;

let test_invalid_request () =
  check
    string
    "InvalidRequest format"
    "Provider 'anthropic' invalid request: context too long"
    (Error.to_string
       (Error.InvalidRequest { provider = "anthropic"; reason = "context too long" }))
;;

let test_not_found () =
  check
    string
    "NotFound format"
    "Provider 'gemini' not found: model not available"
    (Error.to_string
       (Error.NotFound { provider = "gemini"; detail = "model not available" }))
;;

let test_provider_terminal () =
  check
    string
    "ProviderTerminal format"
    "Provider 'claude_code' terminal max_turns:31/31: turn cap hit"
    (Error.to_string
       (Error.ProviderTerminal
          { provider = "claude_code"
          ; reason = "max_turns:31/31"
          ; detail = "turn cap hit"
          }))
;;

let test_retry_rate_limit_mapping () =
  let err =
    Error.of_retry_api_error
      ~provider:"anthropic"
      (Retry.RateLimited { retry_after = Some 2.5; message = "try later" })
  in
  match err with
  | Error.RateLimit { provider; retry_after; detail } ->
    check string "provider" "anthropic" provider;
    check (option (float 0.001)) "retry_after" (Some 2.5) retry_after;
    check string "detail" "try later" detail
  | _ -> fail "expected RateLimit"
;;

let test_retry_hard_quota_mapping () =
  let err =
    Error.of_retry_api_error
      ~provider:"zai"
      (Retry.RateLimited
         { retry_after = Some 10.0
         ; message = "Insufficient balance or no resource package. Please recharge."
         })
  in
  match err with
  | Error.HardQuota { provider; retry_after; detail } ->
    check string "provider" "zai" provider;
    check (option (float 0.001)) "retry_after" (Some 10.0) retry_after;
    check
      string
      "detail"
      "Insufficient balance or no resource package. Please recharge."
      detail
  | _ -> fail "expected HardQuota"
;;

let test_retry_overloaded_unknown_provider_mapping () =
  let check_unknown_provider provider =
    let err =
      Error.of_retry_api_error ?provider (Retry.Overloaded { message = "busy" })
    in
    match err with
    | Error.CapacityExhausted { scope; affected; retry_after; detail } ->
      check bool "scope" true (scope = Error.CapacityProvider);
      check (list string) "affected" [] affected;
      check (option (float 0.001)) "retry_after" None retry_after;
      check string "detail" "busy" detail
    | _ -> fail "expected CapacityExhausted"
  in
  check_unknown_provider None;
  check_unknown_provider (Some "")
;;

let test_http_capacity_failure_mapping () =
  let err =
    Error.of_http_error
      ~provider:"gemini"
      (Http_client.ProviderFailure
         { kind =
             Http_client.Capacity_exhausted
               { scope = Http_client.Failure_scope_model
               ; retry_after = Some 7.0
               ; model = Some "gemini-3-pro"
               }
         ; message = "model queue saturated"
         })
  in
  match err with
  | Error.CapacityExhausted { scope; affected; retry_after; detail } ->
    check bool "scope" true (scope = Error.CapacityModel);
    check (list string) "affected" [ "gemini-3-pro" ] affected;
    check (option (float 0.001)) "retry_after" (Some 7.0) retry_after;
    check string "detail" "model queue saturated" detail
  | _ -> fail "expected CapacityExhausted"
;;

let test_http_server_error_mapping () =
  let err =
    Error.of_http_error
      ~provider:"openai"
      (Http_client.HttpError { code = 503; body = "down" })
  in
  match err with
  | Error.ServerError { provider; code; transient; detail } ->
    check string "provider" "openai" provider;
    check int "code" 503 code;
    check bool "transient" true transient;
    check string "detail" "down" detail
  | _ -> fail "expected ServerError"
;;

let test_http_terminal_mapping () =
  let err =
    Error.of_http_error
      ~provider:"claude_code"
      (Http_client.ProviderTerminal
         { kind = Http_client.Max_turns { turns = 31; limit = 31 }
         ; message = "turn cap hit"
         })
  in
  match err with
  | Error.ProviderTerminal { provider; reason; detail } ->
    check string "provider" "claude_code" provider;
    check string "reason" "max_turns:31/31" reason;
    check string "detail" "turn cap hit" detail
  | _ -> fail "expected ProviderTerminal"
;;

let test_http_network_error_mapping () =
  let err =
    Error.of_http_error
      ~provider:"local"
      (Http_client.NetworkError { message = "read timed out"; kind = Http_client.Timeout })
  in
  match err with
  | Error.NetworkError { provider; kind; detail } ->
    check string "provider" "local" provider;
    check bool "kind" true (kind = Http_client.Timeout);
    check string "detail" "read timed out" detail
  | _ -> fail "expected NetworkError"
;;

let test_cli_transport_required_mapping () =
  let err =
    Error.of_http_error (Http_client.CliTransportRequired { kind = "codex_cli" })
  in
  match err with
  | Error.InvalidConfig { field; detail } ->
    check string "field" "transport" field;
    check string "detail" "CLI transport required for codex_cli" detail
  | _ -> fail "expected InvalidConfig"
;;

let () =
  run
    "llm_provider_error"
    [ ( "to_string"
      , [ test_case "MissingApiKey" `Quick test_missing_api_key
        ; test_case "InvalidConfig" `Quick test_invalid_config
        ; test_case "ParseError" `Quick test_parse_error
        ; test_case "UnknownVariant" `Quick test_unknown_variant
        ; test_case "ProviderUnavailable" `Quick test_provider_unavailable
        ; test_case "RateLimit" `Quick test_rate_limit
        ; test_case "HardQuota" `Quick test_hard_quota
        ; test_case "CapacityExhausted" `Quick test_capacity_exhausted
        ; test_case "AuthError" `Quick test_auth_error
        ; test_case "ServerError" `Quick test_server_error
        ; test_case "NetworkError" `Quick test_network_error
        ; test_case "Timeout" `Quick test_timeout
        ; test_case "InvalidRequest" `Quick test_invalid_request
        ; test_case "NotFound" `Quick test_not_found
        ; test_case "ProviderTerminal" `Quick test_provider_terminal
        ] )
    ; ( "mapping"
      , [ test_case "Retry RateLimited" `Quick test_retry_rate_limit_mapping
        ; test_case "Retry hard quota" `Quick test_retry_hard_quota_mapping
        ; test_case
            "Retry overloaded unknown provider"
            `Quick
            test_retry_overloaded_unknown_provider_mapping
        ; test_case "HTTP capacity failure" `Quick test_http_capacity_failure_mapping
        ; test_case "HTTP server error" `Quick test_http_server_error_mapping
        ; test_case "HTTP terminal" `Quick test_http_terminal_mapping
        ; test_case "HTTP network error" `Quick test_http_network_error_mapping
        ; test_case "CLI transport required" `Quick test_cli_transport_required_mapping
        ] )
    ]
;;
