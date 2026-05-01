open Base
(** Unit tests for Defaults — env-or-default coercion helpers.

    Each test sets/unsets a unique [OAS_TEST_*] variable so test order
    doesn't matter and we don't clobber the real `OAS_*` namespace. *)

open Agent_sdk
open Alcotest

let setenv k v = Unix.putenv k v

let unsetenv k =
  (* Unix has no portable unsetenv; setting empty triggers the
     "empty → default" branch in env_or, which is what we want. *)
  try Unix.putenv k "" with
  | _ -> ()
;;

(* ── env_or (string) ──────────────────────────────────── *)

let test_env_or_unset () =
  unsetenv "OAS_TEST_STR_UNSET";
  check
    string
    "default returned"
    "fallback"
    (Defaults.env_or "fallback" "OAS_TEST_STR_UNSET")
;;

let test_env_or_empty () =
  setenv "OAS_TEST_STR_EMPTY" "";
  check
    string
    "empty → default"
    "fallback"
    (Defaults.env_or "fallback" "OAS_TEST_STR_EMPTY")
;;

let test_env_or_whitespace () =
  setenv "OAS_TEST_STR_WS" "   ";
  check
    string
    "whitespace → default"
    "fallback"
    (Defaults.env_or "fallback" "OAS_TEST_STR_WS")
;;

let test_env_or_value () =
  setenv "OAS_TEST_STR_VAL" "actual";
  check string "value returned" "actual" (Defaults.env_or "fallback" "OAS_TEST_STR_VAL")
;;

let test_env_or_value_trimmed () =
  setenv "OAS_TEST_STR_TRIM" "  trimmed  ";
  check
    string
    "leading/trailing whitespace stripped"
    "trimmed"
    (Defaults.env_or "fallback" "OAS_TEST_STR_TRIM")
;;

(* ── int_env_or ───────────────────────────────────────── *)

let test_int_env_or_unset () =
  unsetenv "OAS_TEST_INT_UNSET";
  check int "default" 42 (Defaults.int_env_or 42 "OAS_TEST_INT_UNSET")
;;

let test_int_env_or_value () =
  setenv "OAS_TEST_INT_VAL" "100";
  check int "parsed" 100 (Defaults.int_env_or 42 "OAS_TEST_INT_VAL")
;;

let test_int_env_or_zero_falls_back () =
  (* impl requires v > 0 *)
  setenv "OAS_TEST_INT_ZERO" "0";
  check int "0 → default" 42 (Defaults.int_env_or 42 "OAS_TEST_INT_ZERO")
;;

let test_int_env_or_negative_falls_back () =
  setenv "OAS_TEST_INT_NEG" "-5";
  check int "negative → default" 42 (Defaults.int_env_or 42 "OAS_TEST_INT_NEG")
;;

let test_int_env_or_garbage () =
  setenv "OAS_TEST_INT_BAD" "not-a-number";
  check int "garbage → default" 42 (Defaults.int_env_or 42 "OAS_TEST_INT_BAD")
;;

let test_int_env_or_trimmed () =
  setenv "OAS_TEST_INT_TRIM" "  17  ";
  check
    int
    "whitespace trimmed before parse"
    17
    (Defaults.int_env_or 42 "OAS_TEST_INT_TRIM")
;;

(* ── float_env_or ─────────────────────────────────────── *)

let test_float_env_or_unset () =
  unsetenv "OAS_TEST_FLOAT_UNSET";
  check (float 1e-9) "default" 1.5 (Defaults.float_env_or 1.5 "OAS_TEST_FLOAT_UNSET")
;;

let test_float_env_or_value () =
  setenv "OAS_TEST_FLOAT_VAL" "3.14";
  check (float 1e-9) "parsed" 3.14 (Defaults.float_env_or 1.5 "OAS_TEST_FLOAT_VAL")
;;

let test_float_env_or_zero_falls_back () =
  setenv "OAS_TEST_FLOAT_ZERO" "0.0";
  check (float 1e-9) "0 → default" 1.5 (Defaults.float_env_or 1.5 "OAS_TEST_FLOAT_ZERO")
;;

let test_float_env_or_negative () =
  setenv "OAS_TEST_FLOAT_NEG" "-2.5";
  check
    (float 1e-9)
    "negative → default"
    1.5
    (Defaults.float_env_or 1.5 "OAS_TEST_FLOAT_NEG")
;;

let test_float_env_or_garbage () =
  setenv "OAS_TEST_FLOAT_BAD" "abc";
  check
    (float 1e-9)
    "garbage → default"
    1.5
    (Defaults.float_env_or 1.5 "OAS_TEST_FLOAT_BAD")
;;

(* ── bool_env_or ──────────────────────────────────────── *)

let test_bool_env_or_unset () =
  unsetenv "OAS_TEST_BOOL_UNSET";
  check bool "default" true (Defaults.bool_env_or true "OAS_TEST_BOOL_UNSET")
;;

let test_bool_env_or_truthy_variants () =
  List.iter
    (fun raw ->
       let var = "OAS_TEST_BOOL_T_" ^ String.uppercase_ascii raw in
       setenv var raw;
       check bool ("truthy: " ^ raw) true (Defaults.bool_env_or false var))
    [ "1"; "true"; "yes"; "on"; "TRUE"; "Yes"; "ON" ]
;;

let test_bool_env_or_falsy_variants () =
  List.iter
    (fun raw ->
       let var = "OAS_TEST_BOOL_F_" ^ String.uppercase_ascii raw in
       setenv var raw;
       check bool ("falsy: " ^ raw) false (Defaults.bool_env_or true var))
    [ "0"; "false"; "no"; "off"; "FALSE"; "No"; "OFF" ]
;;

let test_bool_env_or_garbage_uses_default () =
  setenv "OAS_TEST_BOOL_BAD" "maybe";
  check
    bool
    "garbage → default(true)"
    true
    (Defaults.bool_env_or true "OAS_TEST_BOOL_BAD");
  setenv "OAS_TEST_BOOL_BAD2" "garbage";
  check
    bool
    "garbage → default(false)"
    false
    (Defaults.bool_env_or false "OAS_TEST_BOOL_BAD2")
;;

let test_bool_env_or_whitespace () =
  setenv "OAS_TEST_BOOL_WS" "  true  ";
  check
    bool
    "whitespace + truthy → true"
    true
    (Defaults.bool_env_or false "OAS_TEST_BOOL_WS")
;;

(* ── allow_test_providers (gate over bool_env_or) ─────── *)

let test_allow_test_providers_default () =
  unsetenv "OAS_ALLOW_TEST_PROVIDERS";
  check bool "default false" false (Defaults.allow_test_providers ())
;;

let test_allow_test_providers_enabled () =
  setenv "OAS_ALLOW_TEST_PROVIDERS" "true";
  check bool "enabled" true (Defaults.allow_test_providers ())
;;

let test_allow_test_providers_disabled_explicitly () =
  setenv "OAS_ALLOW_TEST_PROVIDERS" "false";
  check bool "explicit false" false (Defaults.allow_test_providers ())
;;

(* ── default_context_reducer (smoke) ──────────────────── *)

let test_default_context_reducer_is_pure () =
  (* Just verify the constant is reachable and produces a usable reducer.
     Empty messages → empty output (composed reducers are no-ops on []). *)
  let reducer = Defaults.default_context_reducer in
  let result = Context_reducer.reduce reducer [] in
  check int "no-op on empty" 0 (List.length result)
;;

let () =
  run
    "Defaults"
    [ ( "env_or"
      , [ test_case "unset" `Quick test_env_or_unset
        ; test_case "empty string" `Quick test_env_or_empty
        ; test_case "whitespace only" `Quick test_env_or_whitespace
        ; test_case "real value" `Quick test_env_or_value
        ; test_case "trimmed" `Quick test_env_or_value_trimmed
        ] )
    ; ( "int_env_or"
      , [ test_case "unset" `Quick test_int_env_or_unset
        ; test_case "real value" `Quick test_int_env_or_value
        ; test_case "zero falls back" `Quick test_int_env_or_zero_falls_back
        ; test_case "negative falls back" `Quick test_int_env_or_negative_falls_back
        ; test_case "garbage" `Quick test_int_env_or_garbage
        ; test_case "trimmed" `Quick test_int_env_or_trimmed
        ] )
    ; ( "float_env_or"
      , [ test_case "unset" `Quick test_float_env_or_unset
        ; test_case "real value" `Quick test_float_env_or_value
        ; test_case "zero falls back" `Quick test_float_env_or_zero_falls_back
        ; test_case "negative" `Quick test_float_env_or_negative
        ; test_case "garbage" `Quick test_float_env_or_garbage
        ] )
    ; ( "bool_env_or"
      , [ test_case "unset" `Quick test_bool_env_or_unset
        ; test_case "truthy variants" `Quick test_bool_env_or_truthy_variants
        ; test_case "falsy variants" `Quick test_bool_env_or_falsy_variants
        ; test_case "garbage" `Quick test_bool_env_or_garbage_uses_default
        ; test_case "whitespace" `Quick test_bool_env_or_whitespace
        ] )
    ; ( "allow_test_providers"
      , [ test_case "default false" `Quick test_allow_test_providers_default
        ; test_case "enabled" `Quick test_allow_test_providers_enabled
        ; test_case
            "disabled explicitly"
            `Quick
            test_allow_test_providers_disabled_explicitly
        ] )
    ; ( "default_context_reducer"
      , [ test_case "no-op on empty messages" `Quick test_default_context_reducer_is_pure
        ] )
    ]
;;
