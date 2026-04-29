(** Coverage for [Llm_provider.Capability_filter] (oas#1175 ratchet plan).

    [test_llm_provider_cov.ml] already exercises ~13 boolean predicate
    helpers (`requires_tools`, `requires_streaming`, ...) plus
    [fits_context]/[fits_output].  Four shapes were left uncovered and
    are pinned here:

    - [emits_usage_tokens] — last boolean predicate.
    - [check_context]/[check_output] — the [fit_result] variant
      versions of [fits_*].  All three variants ([Fits],
      [Does_not_fit], [Unknown_limit]) need to surface so the
      "structurally unknown" path doesn't silently collapse onto
      [Does_not_fit] in future refactors.
    - [requires_all]/[requires_any] — list combinators.  An empty
      predicate list has degenerate semantics that are easy to flip
      in a future `List.fold_left` refactor; pin them. *)

open Alcotest
open Llm_provider

(* ── emits_usage_tokens ─────────────────────────────── *)

let test_emits_usage_tokens_default () =
  check
    bool
    "default emits usage"
    true
    (Capability_filter.emits_usage_tokens Capabilities.default_capabilities)
;;

let test_emits_usage_tokens_off () =
  let caps = { Capabilities.default_capabilities with emits_usage_tokens = false } in
  check bool "explicit off" false (Capability_filter.emits_usage_tokens caps)
;;

(* ── check_context (fit_result) ─────────────────────── *)

let test_check_context_unknown_limit () =
  (* default has [max_context_tokens = None] → Unknown_limit *)
  let result =
    Capability_filter.check_context ~tokens:1000 Capabilities.default_capabilities
  in
  check bool "Unknown_limit when None" true (result = Capability_filter.Unknown_limit)
;;

let test_check_context_fits () =
  let result =
    Capability_filter.check_context ~tokens:100 Capabilities.anthropic_capabilities
  in
  check bool "Fits when tokens <= max" true (result = Capability_filter.Fits)
;;

let test_check_context_does_not_fit () =
  let result =
    Capability_filter.check_context
      ~tokens:999_999_999
      Capabilities.anthropic_capabilities
  in
  check
    bool
    "Does_not_fit when tokens > max"
    true
    (result = Capability_filter.Does_not_fit)
;;

(* ── check_output (fit_result) ──────────────────────── *)

let test_check_output_unknown_limit () =
  let result =
    Capability_filter.check_output ~tokens:1000 Capabilities.default_capabilities
  in
  check bool "Unknown_limit when None" true (result = Capability_filter.Unknown_limit)
;;

let test_check_output_fits () =
  let result =
    Capability_filter.check_output ~tokens:100 Capabilities.anthropic_capabilities
  in
  check bool "Fits when tokens <= max" true (result = Capability_filter.Fits)
;;

let test_check_output_does_not_fit () =
  let result =
    Capability_filter.check_output ~tokens:999_999 Capabilities.anthropic_capabilities
  in
  check
    bool
    "Does_not_fit when tokens > max"
    true
    (result = Capability_filter.Does_not_fit)
;;

(* ── fits_context Unknown_limit collapses to false ──── *)

let test_fits_context_unknown_is_false () =
  (* [fits_context] returns false when the limit is unknown — pre-fit
     boolean has no Unknown_limit slot, so the helper must err on the
     conservative side rather than admit unknown-as-fitting. *)
  check
    bool
    "fits_context with None max → false"
    false
    (Capability_filter.fits_context ~tokens:1 Capabilities.default_capabilities)
;;

let test_fits_output_unknown_is_false () =
  check
    bool
    "fits_output with None max → false"
    false
    (Capability_filter.fits_output ~tokens:1 Capabilities.default_capabilities)
;;

(* ── requires_all / requires_any combinators ────────── *)

let test_requires_all_empty () =
  (* Empty predicate list → vacuously true (List.for_all on []). *)
  check
    bool
    "[] → true (vacuous)"
    true
    (Capability_filter.requires_all [] Capabilities.default_capabilities)
;;

let test_requires_all_single_pass () =
  check
    bool
    "[trivial-true] → true"
    true
    (Capability_filter.requires_all [ (fun _ -> true) ] Capabilities.default_capabilities)
;;

let test_requires_all_one_fails () =
  check
    bool
    "any failing predicate → false"
    false
    (Capability_filter.requires_all
       [ (fun _ -> true); (fun _ -> false); (fun _ -> true) ]
       Capabilities.default_capabilities)
;;

let test_requires_all_real_predicates () =
  (* requires_tools fails for default; requires_system_prompt passes. *)
  check
    bool
    "tools missing → false"
    false
    (Capability_filter.requires_all
       [ Capability_filter.requires_tools; Capability_filter.requires_system_prompt ]
       Capabilities.default_capabilities)
;;

let test_requires_any_empty () =
  (* Empty predicate list → vacuously false (List.exists on []). *)
  check
    bool
    "[] → false (vacuous)"
    false
    (Capability_filter.requires_any [] Capabilities.default_capabilities)
;;

let test_requires_any_one_passes () =
  check
    bool
    "any passing predicate → true"
    true
    (Capability_filter.requires_any
       [ (fun _ -> false); (fun _ -> true); (fun _ -> false) ]
       Capabilities.default_capabilities)
;;

let test_requires_any_all_fail () =
  check
    bool
    "all failing → false"
    false
    (Capability_filter.requires_any
       [ Capability_filter.requires_tools; Capability_filter.requires_thinking ]
       Capabilities.default_capabilities)
;;

let () =
  run
    "capability_filter"
    [ ( "emits_usage_tokens"
      , [ test_case "default true" `Quick test_emits_usage_tokens_default
        ; test_case "explicit off" `Quick test_emits_usage_tokens_off
        ] )
    ; ( "check_context"
      , [ test_case "Unknown_limit" `Quick test_check_context_unknown_limit
        ; test_case "Fits" `Quick test_check_context_fits
        ; test_case "Does_not_fit" `Quick test_check_context_does_not_fit
        ] )
    ; ( "check_output"
      , [ test_case "Unknown_limit" `Quick test_check_output_unknown_limit
        ; test_case "Fits" `Quick test_check_output_fits
        ; test_case "Does_not_fit" `Quick test_check_output_does_not_fit
        ] )
    ; ( "fits_* unknown collapse"
      , [ test_case "fits_context unknown→false" `Quick test_fits_context_unknown_is_false
        ; test_case "fits_output unknown→false" `Quick test_fits_output_unknown_is_false
        ] )
    ; ( "requires_all"
      , [ test_case "empty → true" `Quick test_requires_all_empty
        ; test_case "single passing" `Quick test_requires_all_single_pass
        ; test_case "one failing" `Quick test_requires_all_one_fails
        ; test_case "real predicates" `Quick test_requires_all_real_predicates
        ] )
    ; ( "requires_any"
      , [ test_case "empty → false" `Quick test_requires_any_empty
        ; test_case "one passing" `Quick test_requires_any_one_passes
        ; test_case "all failing" `Quick test_requires_any_all_fail
        ] )
    ]
;;
