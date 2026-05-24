(** Coverage for [Llm_provider.Error.provider_error], its [to_string]
    formatter, and typed transport/retry mapping (oas#1175 - file at 0% coverage in the
    initial coverage measurement (`66.42%`, run `24887636744`)).

    The formatter has one case per variant; the [to_string] group keeps
    one assertion per [provider_error] variant while the mapping group
    covers the conversion boundaries. *)

open Alcotest
open Llm_provider

let contains_substring haystack needle =
  let haystack_len = String.length haystack in
  let needle_len = String.length needle in
  let rec loop idx =
    if needle_len = 0
    then true
    else if idx + needle_len > haystack_len
    then false
    else if String.sub haystack idx needle_len = needle
    then true
    else loop (idx + 1)
  in
  loop 0
;;

let test_missing_api_key () =
  check
    string
    "MissingApiKey format"
    "Missing API key env var: PROVIDER_D_API_KEY"
    (Error.to_string (Error.MissingApiKey { var_name = "PROVIDER_D_API_KEY" }))
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
    "Provider 'provider_a' unavailable: HTTP 503 retry-after exhausted"
    (Error.to_string
       (Error.ProviderUnavailable
          { provider = "provider_a"; detail = "HTTP 503 retry-after exhausted" }))
;;

let test_rate_limit () =
  check
    string
    "RateLimit format"
    "Provider 'provider_a' rate limited: quota window exhausted (retry_after: 1.250s)"
    (Error.to_string
       (Error.RateLimit
          { provider = "provider_a"
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
    "Provider capacity exhausted (model): model queue saturated affected=[provider_f-3-pro] \
     (retry_after: 7.000s)"
    (Error.to_string
       (Error.CapacityExhausted
          { scope = Error.CapacityModel
          ; affected = [ "provider_f-3-pro" ]
          ; retry_after = Some 7.0
          ; detail = "model queue saturated"
          }))
;;

let test_auth_error () =
  check
    string
    "AuthError format"
    "Provider 'provider_d' auth error: invalid API key"
    (Error.to_string
       (Error.AuthError { provider = "provider_d"; detail = "invalid API key" }))
;;

let test_server_error () =
  check
    string
    "ServerError format"
    "Provider 'provider_d' server error 503 (transient=true): down"
    (Error.to_string
       (Error.ServerError
          { provider = "provider_d"; code = 503; transient = true; detail = "down" }))
;;

let test_network_error () =
  check
    string
    "NetworkError format"
    "Provider 'local' network error (dns_failure): lookup failed"
    (Error.to_string
       (Error.NetworkError
          { provider = "local"
          ; kind = Http_client.Dns_failure
          ; timeout_phase = None
          ; detail = "lookup failed"
          }))
;;

let test_timeout () =
  check
    string
    "Timeout format"
    "Provider 'provider_f' timeout: request exceeded budget"
    (Error.to_string
       (Error.Timeout
          { provider = "provider_f"
          ; timeout_phase = None
          ; detail = "request exceeded budget"
          }))
;;

let test_timeout_phase () =
  check
    string
    "Timeout phase format"
    "Provider 'provider_d' timeout phase=stream_idle:streaming_thinking: stalled"
    (Error.to_string
       (Error.Timeout
          { provider = "provider_d"
          ; timeout_phase = Some (Http_client.Stream_idle Http_client.Streaming_thinking)
          ; detail = "stalled"
          }))
;;

let test_invalid_request () =
  check
    string
    "InvalidRequest format"
    "Provider 'provider_a' invalid request: context too long"
    (Error.to_string
       (Error.InvalidRequest { provider = "provider_a"; reason = "context too long" }))
;;

let test_not_found () =
  check
    string
    "NotFound format"
    "Provider 'provider_f' not found: model not available"
    (Error.to_string
       (Error.NotFound { provider = "provider_f"; detail = "model not available" }))
;;

let test_provider_terminal () =
  check
    string
    "ProviderTerminal format"
    "Provider 'cli_tool_d' terminal max_turns:31/31: turn cap hit"
    (Error.to_string
       (Error.ProviderTerminal
          { provider = "cli_tool_d"
          ; reason = "max_turns:31/31"
          ; detail = "turn cap hit"
          }))
;;

let test_capacity_scope_strings () =
  List.iter
    (fun (scope, expected) ->
       check string expected expected (Error.capacity_scope_to_string scope))
    [ Error.CapacityModel, "model"
    ; Error.CapacityAccount, "account"
    ; Error.CapacityRegion, "region"
    ; Error.CapacityProvider, "provider"
    ; Error.CapacityUnknown, "unknown"
    ]
;;

let test_network_error_kind_strings_and_retryability () =
  List.iter
    (fun (kind, expected, retryable) ->
       let err =
         Error.NetworkError
           { provider = "net"
           ; kind
           ; timeout_phase = Some Http_client.Http_operation
           ; detail = "failed"
           }
       in
       check
         bool
         ("contains " ^ expected)
         true
         (contains_substring (Error.to_string err) expected);
       check bool ("retryable " ^ expected) retryable (Error.is_retryable err))
    [ Http_client.Connection_refused, "connection_refused", true
    ; Http_client.Dns_failure, "dns_failure", true
    ; Http_client.Tls_error, "tls_error", false
    ; Http_client.Timeout, "timeout", true
    ; Http_client.Local_resource_exhaustion, "local_resource_exhaustion", false
    ; Http_client.End_of_file, "end_of_file", true
    ; Http_client.Unknown, "unknown", true
    ]
;;

let test_capacity_exhausted_without_suffixes () =
  check
    string
    "CapacityExhausted without affected/retry suffix"
    "Provider capacity exhausted (unknown): no capacity detail"
    (Error.to_string
       (Error.CapacityExhausted
          { scope = Error.CapacityUnknown
          ; affected = []
          ; retry_after = None
          ; detail = "no capacity detail"
          }))
;;

let test_is_retryable_matrix () =
  let retryable =
    [ Error.RateLimit { provider = "p"; retry_after = None; detail = "soft" }
    ; Error.CapacityExhausted
        { scope = Error.CapacityProvider
        ; affected = [ "p" ]
        ; retry_after = None
        ; detail = "busy"
        }
    ; Error.ServerError { provider = "p"; code = 503; transient = true; detail = "x" }
    ; Error.Timeout { provider = "p"; timeout_phase = None; detail = "x" }
    ]
  in
  let terminal =
    [ Error.MissingApiKey { var_name = "KEY" }
    ; Error.InvalidConfig { field = "f"; detail = "bad" }
    ; Error.ParseError { detail = "bad json" }
    ; Error.UnknownVariant { type_name = "kind"; value = "new" }
    ; Error.ProviderUnavailable { provider = "p"; detail = "down" }
    ; Error.HardQuota { provider = "p"; retry_after = None; detail = "quota" }
    ; Error.AuthError { provider = "p"; detail = "denied" }
    ; Error.ServerError { provider = "p"; code = 400; transient = false; detail = "x" }
    ; Error.InvalidRequest { provider = "p"; reason = "bad" }
    ; Error.NotFound { provider = "p"; detail = "missing" }
    ; Error.ProviderTerminal { provider = "p"; reason = "done"; detail = "stop" }
    ]
  in
  List.iter
    (fun err -> check bool (Error.to_string err) true (Error.is_retryable err))
    retryable;
  List.iter
    (fun err -> check bool (Error.to_string err) false (Error.is_retryable err))
    terminal
;;

let test_retry_rate_limit_mapping () =
  let err =
    Error.of_retry_api_error
      ~provider:"provider_a"
      (Retry.RateLimited { retry_after = Some 2.5; message = "try later" })
  in
  match err with
  | Error.RateLimit { provider; retry_after; detail } ->
    check string "provider" "provider_a" provider;
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

let test_retry_api_error_variant_mappings () =
  let cases =
    [ ( Error.of_retry_api_error ~provider:"auth" (Retry.AuthError { message = "bad key" })
      , "Provider 'auth' auth error: bad key" )
    ; ( Error.of_retry_api_error
          ~provider:"bad"
          (Retry.InvalidRequest { message = "bad request" })
      , "Provider 'bad' invalid request: bad request" )
    ; ( Error.of_retry_api_error ~provider:"missing" (Retry.NotFound { message = "gone" })
      , "Provider 'missing' not found: gone" )
    ; ( Error.of_retry_api_error
          ~provider:"ctx"
          (Retry.ContextOverflow { message = "too long"; limit = Some 200_000 })
      , "Provider 'ctx' invalid request: Context overflow (limit: 200000): too long" )
    ; ( Error.of_retry_api_error
          ~provider:"net"
          (Retry.NetworkError { message = "eof"; kind = Http_client.End_of_file })
      , "Provider 'net' network error (end_of_file): eof" )
    ; ( Error.of_retry_api_error
          ~provider:"slow"
          (Retry.Timeout { message = "wall clock" })
      , "Provider 'slow' timeout: wall clock" )
    ; ( Error.of_retry_api_error
          ~provider:"server"
          (Retry.ServerError { status = 418; message = "teapot" })
      , "Provider 'server' server error 418 (transient=true): teapot" )
    ]
  in
  List.iter
    (fun (err, expected) -> check string expected expected (Error.to_string err))
    cases
;;

let test_http_capacity_failure_mapping () =
  let err =
    Error.of_http_error
      ~provider:"provider_f"
      (Http_client.ProviderFailure
         { kind =
             Http_client.Capacity_exhausted
               { scope = Http_client.Failure_scope_model
               ; retry_after = Some 7.0
               ; model = Some "provider_f-3-pro"
               }
         ; message = "model queue saturated"
         })
  in
  match err with
  | Error.CapacityExhausted { scope; affected; retry_after; detail } ->
    check bool "scope" true (scope = Error.CapacityModel);
    check (list string) "affected" [ "provider_f-3-pro" ] affected;
    check (option (float 0.001)) "retry_after" (Some 7.0) retry_after;
    check string "detail" "model queue saturated" detail
  | _ -> fail "expected CapacityExhausted"
;;

let test_http_capacity_failure_scope_mappings () =
  List.iter
    (fun (http_scope, provider, expected_scope, expected_affected) ->
       let err =
         Error.of_http_error
           ~provider
           (Http_client.ProviderFailure
              { kind =
                  Http_client.Capacity_exhausted
                    { scope = http_scope; retry_after = None; model = None }
              ; message = "capacity"
              })
       in
       match err with
       | Error.CapacityExhausted { scope; affected; retry_after; detail } ->
         check bool "scope" true (scope = expected_scope);
         check (list string) "affected" expected_affected affected;
         check (option (float 0.001)) "retry_after" None retry_after;
         check string "detail" "capacity" detail
       | _ -> fail "expected CapacityExhausted")
    [ Http_client.Failure_scope_account, "account", Error.CapacityAccount, [ "account" ]
    ; Http_client.Failure_scope_region, "region", Error.CapacityRegion, [ "region" ]
    ; ( Http_client.Failure_scope_provider
      , "provider"
      , Error.CapacityProvider
      , [ "provider" ] )
    ; Http_client.Failure_scope_unknown, "", Error.CapacityUnknown, []
    ]
;;

let test_http_provider_failure_variant_mappings () =
  let cases =
    [ ( Error.of_http_error
          ~provider:"quota"
          (Http_client.ProviderFailure
             { kind = Http_client.Hard_quota { retry_after = Some 11.0 }
             ; message = "paywall"
             })
      , "Provider 'quota' hard quota exhausted: paywall (retry_after: 11.000s)" )
    ; ( Error.of_http_error
          ~provider:"caps"
          (Http_client.ProviderFailure
             { kind = Http_client.Capability_mismatch { capability = Some "tools" }
             ; message = "not supported"
             })
      , "Provider 'caps' invalid request: missing capability: tools: not supported" )
    ; ( Error.of_http_error
          ~provider:"caps"
          (Http_client.ProviderFailure
             { kind = Http_client.Capability_mismatch { capability = None }
             ; message = "not supported"
             })
      , "Provider 'caps' invalid request: missing provider capability: not supported" )
    ; ( Error.of_http_error
          ~provider:"cli"
          (Http_client.ProviderFailure
             { kind =
                 Http_client.Cli_policy_invalid
                   { tool_name = Some "shell"; rule = Some 2 }
             ; message = "blocked"
             })
      , "Provider 'cli' invalid request: CLI policy rejected shell at rule 2: blocked" )
    ; ( Error.of_http_error
          ~provider:"cli"
          (Http_client.ProviderFailure
             { kind = Http_client.Cli_policy_invalid { tool_name = None; rule = None }
             ; message = "blocked"
             })
      , "Provider 'cli' invalid request: CLI policy rejected unknown_tool: blocked" )
    ; ( Error.of_http_error
          ~provider:"cli"
          (Http_client.ProviderFailure
             { kind = Http_client.Cli_startup_failed { reason = "missing binary" }
             ; message = "codex"
             })
      , "Provider 'cli' unavailable: missing binary: codex" )
    ; ( Error.of_http_error
          (Http_client.ProviderFailure
             { kind = Http_client.Provider_parse_error { parser = Some "openai" }
             ; message = "bad choices"
             })
      , "Parse error: openai: bad choices" )
    ; ( Error.of_http_error
          (Http_client.ProviderFailure
             { kind = Http_client.Provider_parse_error { parser = None }
             ; message = "bad choices"
             })
      , "Parse error: unknown_parser: bad choices" )
    ; ( Error.of_http_error
          ~provider:"mystery"
          (Http_client.ProviderFailure
             { kind = Http_client.Unknown_provider_failure { reason = Some "throttle" }
             ; message = "no slots"
             })
      , "Provider 'mystery' unavailable: throttle: no slots" )
    ; ( Error.of_http_error
          ~provider:"mystery"
          (Http_client.ProviderFailure
             { kind = Http_client.Unknown_provider_failure { reason = None }
             ; message = "no slots"
             })
      , "Provider 'mystery' unavailable: unknown_provider_failure: no slots" )
    ]
  in
  List.iter
    (fun (err, expected) -> check string expected expected (Error.to_string err))
    cases
;;

let test_http_server_error_mapping () =
  let err =
    Error.of_http_error
      ~provider:"provider_d"
      (Http_client.HttpError { code = 503; body = "down" })
  in
  match err with
  | Error.ServerError { provider; code; transient; detail } ->
    check string "provider" "provider_d" provider;
    check int "code" 503 code;
    check bool "transient" true transient;
    check string "detail" "down" detail
  | _ -> fail "expected ServerError"
;;

let test_http_terminal_mapping () =
  let err =
    Error.of_http_error
      ~provider:"cli_tool_d"
      (Http_client.ProviderTerminal
         { kind = Http_client.Max_turns { turns = 31; limit = 31 }
         ; message = "turn cap hit"
         })
  in
  match err with
  | Error.ProviderTerminal { provider; reason; detail } ->
    check string "provider" "cli_tool_d" provider;
    check string "reason" "max_turns:31/31" reason;
    check string "detail" "turn cap hit" detail
  | _ -> fail "expected ProviderTerminal"
;;

let test_http_accept_rejected_and_terminal_other_mapping () =
  let accept =
    Error.of_http_error
      ~provider:"openai"
      (Http_client.AcceptRejected { reason = "schema mismatch" })
  in
  check
    string
    "accept rejected"
    "Provider 'openai' invalid request: accept rejected: schema mismatch"
    (Error.to_string accept);
  let terminal =
    Error.of_http_error
      ~provider:"claude_code"
      (Http_client.ProviderTerminal
         { kind = Http_client.Other "policy_stop"; message = "stopped" })
  in
  check
    string
    "terminal other"
    "Provider 'claude_code' terminal policy_stop: stopped"
    (Error.to_string terminal)
;;

let test_http_network_error_mapping () =
  let err =
    Error.of_http_error
      ~provider:"local"
      (Http_client.NetworkError { message = "read timed out"; kind = Http_client.Timeout })
  in
  match err with
  | Error.NetworkError { provider; kind; timeout_phase; detail } ->
    check string "provider" "local" provider;
    check bool "kind" true (kind = Http_client.Timeout);
    check
      (option string)
      "timeout phase"
      None
      (Option.map Http_client.timeout_phase_to_label timeout_phase);
    check string "detail" "read timed out" detail
  | _ -> fail "expected NetworkError"
;;

let test_http_timeout_error_mapping () =
  let err =
    Error.of_http_error
      ~provider:"provider_d"
      (Http_client.TimeoutError
         { message = "stream stalled"
         ; phase = Http_client.Stream_idle Http_client.Streaming_thinking
         })
  in
  match err with
  | Error.Timeout { provider; timeout_phase; detail } ->
    check string "provider" "provider_d" provider;
    check
      (option string)
      "phase"
      (Some "stream_idle:streaming_thinking")
      (Option.map Http_client.timeout_phase_to_label timeout_phase);
    check string "detail" "stream stalled" detail
  | _ -> fail "expected Timeout"
;;

let test_cli_transport_required_mapping () =
  let err =
    Error.of_http_error (Http_client.CliTransportRequired { kind = "cli_tool_a" })
  in
  match err with
  | Error.InvalidConfig { field; detail } ->
    check string "field" "transport" field;
    check string "detail" "CLI transport required for cli_tool_a" detail
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
        ; test_case "Timeout phase" `Quick test_timeout_phase
        ; test_case "InvalidRequest" `Quick test_invalid_request
        ; test_case "NotFound" `Quick test_not_found
        ; test_case "ProviderTerminal" `Quick test_provider_terminal
        ; test_case "capacity scope strings" `Quick test_capacity_scope_strings
        ; test_case
            "network kinds and retryability"
            `Quick
            test_network_error_kind_strings_and_retryability
        ; test_case
            "CapacityExhausted without suffixes"
            `Quick
            test_capacity_exhausted_without_suffixes
        ; test_case "is_retryable matrix" `Quick test_is_retryable_matrix
        ] )
    ; ( "mapping"
      , [ test_case "Retry RateLimited" `Quick test_retry_rate_limit_mapping
        ; test_case "Retry hard quota" `Quick test_retry_hard_quota_mapping
        ; test_case
            "Retry overloaded unknown provider"
            `Quick
            test_retry_overloaded_unknown_provider_mapping
        ; test_case
            "Retry api_error variants"
            `Quick
            test_retry_api_error_variant_mappings
        ; test_case "HTTP capacity failure" `Quick test_http_capacity_failure_mapping
        ; test_case
            "HTTP capacity scope variants"
            `Quick
            test_http_capacity_failure_scope_mappings
        ; test_case
            "HTTP provider failure variants"
            `Quick
            test_http_provider_failure_variant_mappings
        ; test_case "HTTP server error" `Quick test_http_server_error_mapping
        ; test_case "HTTP terminal" `Quick test_http_terminal_mapping
        ; test_case
            "HTTP accept rejected and terminal other"
            `Quick
            test_http_accept_rejected_and_terminal_other_mapping
        ; test_case "HTTP network error" `Quick test_http_network_error_mapping
        ; test_case "HTTP timeout error" `Quick test_http_timeout_error_mapping
        ; test_case "CLI transport required" `Quick test_cli_transport_required_mapping
        ] )
    ]
;;
