open Base
(** Coverage for [Llm_provider.Error.provider_error] and its
    [to_string] formatter (oas#1175 — file at 0% coverage in the
    initial coverage measurement (`66.42%`, run `24887636744`)).

    The module is 5 line-counted regions and the formatter has one
    case per variant; one test per variant brings the file from 0%
    to 100% with no behavior change. *)

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

let () =
  run
    "llm_provider_error"
    [ ( "to_string"
      , [ test_case "MissingApiKey" `Quick test_missing_api_key
        ; test_case "InvalidConfig" `Quick test_invalid_config
        ; test_case "ParseError" `Quick test_parse_error
        ; test_case "UnknownVariant" `Quick test_unknown_variant
        ; test_case "ProviderUnavailable" `Quick test_provider_unavailable
        ] )
    ]
;;
