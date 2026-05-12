(** Tests for Disclosure_resolver.resolve.

    The helper is pure: it picks between a resolver callback's output
    and a static fallback. Tests pin all four combinations of the
    [resolver / static] cross-product plus the typical "fallback to
    Full_schema when previous turn had errors" caller pattern. *)

open Alcotest
open Agent_sdk

let ok_result content : Types.tool_result = Ok { Types.content }

let err_result message : Types.tool_result =
  Error { Types.message; recoverable = true; error_class = Some Types.Deterministic }
;;

let test_no_resolver_returns_static () =
  let actual =
    Disclosure_resolver.resolve
      ~resolver:None
      ~static:(Some Tool.Minimal_index)
      ~last_results:[]
  in
  check bool "static passes through" true (actual = Some Tool.Minimal_index)
;;

let test_no_resolver_no_static_returns_none () =
  let actual = Disclosure_resolver.resolve ~resolver:None ~static:None ~last_results:[] in
  check bool "None when neither set" true (actual = None)
;;

let test_resolver_some_override_wins_over_static () =
  let resolver _ = Some Tool.Full_schema in
  let actual =
    Disclosure_resolver.resolve
      ~resolver:(Some resolver)
      ~static:(Some Tool.Minimal_index)
      ~last_results:[]
  in
  check bool "resolver override beats static" true (actual = Some Tool.Full_schema)
;;

let test_resolver_none_falls_through_to_static () =
  let resolver _ = None in
  let actual =
    Disclosure_resolver.resolve
      ~resolver:(Some resolver)
      ~static:(Some Tool.Minimal_index)
      ~last_results:[ ok_result "x" ]
  in
  check bool "resolver=None uses static" true (actual = Some Tool.Minimal_index)
;;

let test_resolver_none_no_static_returns_none () =
  let resolver _ = None in
  let actual =
    Disclosure_resolver.resolve ~resolver:(Some resolver) ~static:None ~last_results:[]
  in
  check bool "double None means None" true (actual = None)
;;

let test_resolver_receives_last_results_verbatim () =
  let seen = ref [] in
  let resolver results =
    seen := results;
    None
  in
  let input = [ ok_result "a"; err_result "bad"; ok_result "c" ] in
  let _ =
    Disclosure_resolver.resolve ~resolver:(Some resolver) ~static:None ~last_results:input
  in
  check int "resolver got all 3 results" 3 (List.length !seen);
  check bool "results passed by value" true (!seen = input)
;;

(* ── Caller pattern: demote to Full_schema on previous-turn error ── *)

let demote_on_error_resolver (results : Types.tool_result list) =
  if List.exists Result.is_error results then Some Tool.Full_schema else None
;;

let test_caller_pattern_no_errors_falls_through () =
  let actual =
    Disclosure_resolver.resolve
      ~resolver:(Some demote_on_error_resolver)
      ~static:(Some Tool.Minimal_index)
      ~last_results:[ ok_result "a"; ok_result "b" ]
  in
  check
    bool
    "all-ok results: stay on Minimal_index"
    true
    (actual = Some Tool.Minimal_index)
;;

let test_caller_pattern_with_error_demotes () =
  let actual =
    Disclosure_resolver.resolve
      ~resolver:(Some demote_on_error_resolver)
      ~static:(Some Tool.Minimal_index)
      ~last_results:[ ok_result "a"; err_result "schema bad" ]
  in
  check bool "error present: demote to Full_schema" true (actual = Some Tool.Full_schema)
;;

let test_caller_pattern_empty_results_no_demote () =
  let actual =
    Disclosure_resolver.resolve
      ~resolver:(Some demote_on_error_resolver)
      ~static:(Some Tool.Minimal_index)
      ~last_results:[]
  in
  check
    bool
    "first turn (empty results): no demote"
    true
    (actual = Some Tool.Minimal_index)
;;

let () =
  run
    "Disclosure_resolver"
    [ ( "resolve"
      , [ test_case "no resolver returns static" `Quick test_no_resolver_returns_static
        ; test_case
            "no resolver and no static returns None"
            `Quick
            test_no_resolver_no_static_returns_none
        ; test_case
            "resolver Some override beats static"
            `Quick
            test_resolver_some_override_wins_over_static
        ; test_case
            "resolver None falls through to static"
            `Quick
            test_resolver_none_falls_through_to_static
        ; test_case
            "resolver None and static None returns None"
            `Quick
            test_resolver_none_no_static_returns_none
        ; test_case
            "resolver receives last_results verbatim"
            `Quick
            test_resolver_receives_last_results_verbatim
        ] )
    ; ( "caller_pattern_demote_on_error"
      , [ test_case
            "all-ok results: keep static"
            `Quick
            test_caller_pattern_no_errors_falls_through
        ; test_case
            "error in results: override with Full_schema"
            `Quick
            test_caller_pattern_with_error_demotes
        ; test_case
            "empty results (first turn): keep static"
            `Quick
            test_caller_pattern_empty_results_no_demote
        ] )
    ]
;;
