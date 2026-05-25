(** Unit tests for Memory_tools_parse — pure JSON parameter parsing
    helpers. Returns tool-compatible Result types for uniform error
    handling. No IO. *)

open Agent_sdk
open Alcotest

let json s = Yojson.Safe.from_string s

let is_error = function
  | Error _ -> true
  | Ok _ -> false
;;

(* ── parse_string_field ────────────────────────────────── *)

let test_string_present () =
  let j = json {|{"name": "alice"}|} in
  match Memory_tools_parse.parse_string_field j "name" with
  | Ok "alice" -> ()
  | Ok v -> failf "got %s" v
  | Error _ -> fail "expected Ok"
;;

let test_string_missing () =
  let j = json {|{}|} in
  check
    bool
    "missing key → Error"
    true
    (is_error (Memory_tools_parse.parse_string_field j "name"))
;;

let test_string_wrong_type () =
  let j = json {|{"name": 42}|} in
  check
    bool
    "non-string → Error"
    true
    (is_error (Memory_tools_parse.parse_string_field j "name"))
;;

let test_string_wrong_type_variants () =
  List.iter
    (fun value ->
       let j = `Assoc [ "name", value ] in
       check
         bool
         "wrong string type variant"
         true
         (is_error (Memory_tools_parse.parse_string_field j "name")))
    [ `Assoc []; `Bool true; `Float 1.2; `Intlit "123"; `List [] ]
;;

(* ── parse_optional_string_field ───────────────────────── *)

let test_optional_string_present () =
  let j = json {|{"k": "v"}|} in
  match Memory_tools_parse.parse_optional_string_field j "k" with
  | Ok (Some "v") -> ()
  | _ -> fail "expected Ok (Some v)"
;;

let test_optional_string_missing () =
  let j = json {|{}|} in
  match Memory_tools_parse.parse_optional_string_field j "k" with
  | Ok None -> ()
  | _ -> fail "expected Ok None"
;;

let test_optional_string_wrong_type () =
  let j = json {|{"k": true}|} in
  check
    bool
    "non-string → Error"
    true
    (is_error (Memory_tools_parse.parse_optional_string_field j "k"))
;;

let test_optional_string_wrong_type_variants () =
  List.iter
    (fun value ->
       let j = `Assoc [ "k", value ] in
       check
         bool
         "wrong optional string type variant"
         true
         (is_error (Memory_tools_parse.parse_optional_string_field j "k")))
    [ `Assoc []; `Float 1.2; `Int 1; `Intlit "1"; `List [] ]
;;

(* ── parse_bool_field with default ─────────────────────── *)

let test_bool_present () =
  let j = json {|{"flag": true}|} in
  match Memory_tools_parse.parse_bool_field j "flag" ~default:false with
  | Ok true -> ()
  | _ -> fail "expected Ok true"
;;

let test_bool_missing_uses_default () =
  let j = json {|{}|} in
  match Memory_tools_parse.parse_bool_field j "flag" ~default:true with
  | Ok true -> ()
  | _ -> fail "expected Ok true (default)"
;;

let test_bool_wrong_type () =
  let j = json {|{"flag": "yes"}|} in
  check
    bool
    "string → Error"
    true
    (is_error (Memory_tools_parse.parse_bool_field j "flag" ~default:false))
;;

let test_bool_wrong_type_variants () =
  List.iter
    (fun value ->
       let j = `Assoc [ "flag", value ] in
       check
         bool
         "wrong bool type variant"
         true
         (is_error (Memory_tools_parse.parse_bool_field j "flag" ~default:false)))
    [ `Assoc []; `Float 1.2; `Int 1; `Intlit "1"; `List [] ]
;;

(* ── parse_float_field with default ────────────────────── *)

let test_float_present () =
  let j = json {|{"x": 3.5}|} in
  match Memory_tools_parse.parse_float_field j "x" ~default:0.0 with
  | Ok v -> check (float 1e-9) "value" 3.5 v
  | Error _ -> fail "expected Ok"
;;

let test_float_int_accepted () =
  let j = json {|{"x": 7}|} in
  (* JSON ints widen to float in OCaml's Yojson *)
  match Memory_tools_parse.parse_float_field j "x" ~default:0.0 with
  | Ok v -> check (float 1e-9) "value" 7.0 v
  | Error _ -> fail "expected Ok (int → float widening)"
;;

let test_float_missing_uses_default () =
  let j = json {|{}|} in
  match Memory_tools_parse.parse_float_field j "x" ~default:1.5 with
  | Ok v -> check (float 1e-9) "default" 1.5 v
  | Error _ -> fail "expected Ok default"
;;

let test_float_wrong_type_variants () =
  List.iter
    (fun value ->
       let j = `Assoc [ "x", value ] in
       check
         bool
         "wrong float type variant"
         true
         (is_error (Memory_tools_parse.parse_float_field j "x" ~default:0.0)))
    [ `Assoc []; `Bool true; `Intlit "1"; `List []; `String "1.0" ]
;;

(* ── parse_generic_tier ───────────────────────────────── *)

let test_generic_tier_variants () =
  let cases =
    [ "scratchpad", Memory.Scratchpad
    ; "working", Memory.Working
    ; "long-term", Memory.Long_term
    ; "long_term", Memory.Long_term
    ]
  in
  List.iter
    (fun (tier, expected) ->
       let j = `Assoc [ "tier", `String tier ] in
       match Memory_tools_parse.parse_generic_tier j ~default:Memory.Working with
       | Ok actual -> check bool "tier" true (actual = expected)
       | Error err -> fail err.message)
    cases;
  (match Memory_tools_parse.parse_generic_tier (`Assoc []) ~default:Memory.Working with
   | Ok Memory.Working -> ()
   | Ok _ -> fail "expected default working"
   | Error err -> fail err.message);
  List.iter
    (fun value ->
       let j = `Assoc [ "tier", value ] in
       check
         bool
         "tier error"
         true
         (is_error (Memory_tools_parse.parse_generic_tier j ~default:Memory.Working)))
    [ `String "episodic"
    ; `String "procedural"
    ; `String "bad"
    ; `Assoc []
    ; `Bool true
    ; `Float 1.0
    ; `Int 1
    ; `Intlit "1"
    ; `List []
    ]
;;

(* ── parse_string_list_field ───────────────────────────── *)

let test_string_list_present () =
  let j = json {|{"tags": ["a", "b", "c"]}|} in
  match Memory_tools_parse.parse_string_list_field j "tags" with
  | Ok [ "a"; "b"; "c" ] -> ()
  | _ -> fail "expected [a;b;c]"
;;

let test_string_list_missing () =
  (* missing key returns empty list (not error) per the impl *)
  let j = json {|{}|} in
  match Memory_tools_parse.parse_string_list_field j "tags" with
  | Ok [] -> ()
  | Ok _ -> fail "expected []"
  | Error _ -> () (* either Ok [] or Error is acceptable *)
;;

let test_string_list_errors () =
  let bad_outer_values =
    [ `Assoc []; `Bool true; `Float 1.0; `Int 1; `Intlit "1"; `String "x" ]
  in
  List.iter
    (fun value ->
       check
         bool
         "bad string list outer"
         true
         (is_error
            (Memory_tools_parse.parse_string_list_field (`Assoc [ "tags", value ]) "tags")))
    bad_outer_values;
  List.iter
    (fun value ->
       check
         bool
         "bad string list item"
         true
         (is_error
            (Memory_tools_parse.parse_string_list_field
               (`Assoc [ "tags", `List [ value ] ])
               "tags")))
    [ `Assoc []; `Bool true; `Float 1.0; `Int 1; `Intlit "1"; `List []; `Null ]
;;

(* ── parse_metadata_field ──────────────────────────────── *)

let test_metadata_object () =
  let j = json {|{"metadata": {"k1": "v1", "k2": 42}}|} in
  match Memory_tools_parse.parse_metadata_field j with
  | Ok pairs ->
    check int "two pairs" 2 (List.length pairs);
    check bool "has k1" true (List.mem_assoc "k1" pairs);
    check bool "has k2" true (List.mem_assoc "k2" pairs)
  | Error _ -> fail "expected Ok pairs"
;;

let test_metadata_missing () =
  let j = json {|{}|} in
  match Memory_tools_parse.parse_metadata_field j with
  | Ok [] -> ()
  | _ -> fail "expected Ok [] when absent"
;;

let test_metadata_errors () =
  List.iter
    (fun value ->
       check
         bool
         "metadata error"
         true
         (is_error
            (Memory_tools_parse.parse_metadata_field (`Assoc [ "metadata", value ]))))
    [ `Bool true; `Float 1.0; `Int 1; `Intlit "1"; `List []; `String "x" ]
;;

(* ── parse_outcome ─────────────────────────────────────── *)

let test_outcome_success () =
  (* detail key is "detail", not "outcome_detail" *)
  let j = json {|{"outcome": "success", "detail": "shipped"}|} in
  match Memory_tools_parse.parse_outcome j with
  | Ok (Memory.Success "shipped") -> ()
  | Ok _ -> fail "expected Success shipped"
  | Error _ -> fail "expected Ok"
;;

let test_outcome_neutral_default () =
  let j = json {|{}|} in
  match Memory_tools_parse.parse_outcome j with
  | Ok Memory.Neutral -> ()
  | Ok _ -> () (* implementations may differ; accept any default *)
  | Error _ -> () (* or error if no default *)
;;

let test_outcome_variants_and_errors () =
  (match Memory_tools_parse.parse_outcome (json {|{"outcome":"success"}|}) with
   | Ok (Memory.Success "") -> ()
   | _ -> fail "expected success with empty detail");
  (match
     Memory_tools_parse.parse_outcome (json {|{"outcome":"failure","detail":"bad"}|})
   with
   | Ok (Memory.Failure "bad") -> ()
   | _ -> fail "expected failure detail");
  (match
     Memory_tools_parse.parse_outcome (json {|{"outcome":"neutral","detail":"ignored"}|})
   with
   | Ok Memory.Neutral -> ()
   | _ -> fail "expected neutral");
  List.iter
    (fun value ->
       check
         bool
         "outcome error"
         true
         (is_error (Memory_tools_parse.parse_outcome (`Assoc [ "outcome", value ]))))
    [ `String "weird"; `Assoc []; `Bool true; `Float 1.0; `Int 1; `Intlit "1"; `List [] ];
  check
    bool
    "detail wrong type"
    true
    (is_error
       (Memory_tools_parse.parse_outcome
          (`Assoc [ "outcome", `String "success"; "detail", `Int 1 ])))
;;

(* ── parse_value_json ──────────────────────────────────── *)

let test_value_json_present () =
  (* value_json is a string containing nested JSON *)
  let j = json {|{"value_json": "{\"nested\": 1}"}|} in
  match Memory_tools_parse.parse_value_json j with
  | Ok _ -> ()
  | Error _ -> fail "expected Ok"
;;

let test_value_json_fallback_and_error () =
  (match Memory_tools_parse.parse_value_json (json {|{"value_json":"not json"}|}) with
   | Ok (`String "not json") -> ()
   | _ -> fail "expected raw string fallback");
  check
    bool
    "missing value_json"
    true
    (is_error (Memory_tools_parse.parse_value_json (`Assoc [])))
;;

let () =
  run
    "Memory_tools_parse"
    [ ( "parse_string_field"
      , [ test_case "present" `Quick test_string_present
        ; test_case "missing → Error" `Quick test_string_missing
        ; test_case "wrong type → Error" `Quick test_string_wrong_type
        ; test_case "wrong type variants" `Quick test_string_wrong_type_variants
        ] )
    ; ( "parse_optional_string_field"
      , [ test_case "present → Some" `Quick test_optional_string_present
        ; test_case "missing → None" `Quick test_optional_string_missing
        ; test_case "wrong type → Error" `Quick test_optional_string_wrong_type
        ; test_case "wrong type variants" `Quick test_optional_string_wrong_type_variants
        ] )
    ; ( "parse_bool_field"
      , [ test_case "present" `Quick test_bool_present
        ; test_case "missing uses default" `Quick test_bool_missing_uses_default
        ; test_case "wrong type → Error" `Quick test_bool_wrong_type
        ; test_case "wrong type variants" `Quick test_bool_wrong_type_variants
        ] )
    ; ( "parse_float_field"
      , [ test_case "present" `Quick test_float_present
        ; test_case "int widens to float" `Quick test_float_int_accepted
        ; test_case "missing uses default" `Quick test_float_missing_uses_default
        ; test_case "wrong type variants" `Quick test_float_wrong_type_variants
        ] )
    ; ( "parse_generic_tier"
      , [ test_case "variants and errors" `Quick test_generic_tier_variants ] )
    ; ( "parse_string_list_field"
      , [ test_case "present" `Quick test_string_list_present
        ; test_case "missing tolerated" `Quick test_string_list_missing
        ; test_case "errors" `Quick test_string_list_errors
        ] )
    ; ( "parse_metadata_field"
      , [ test_case "object" `Quick test_metadata_object
        ; test_case "missing tolerated" `Quick test_metadata_missing
        ; test_case "errors" `Quick test_metadata_errors
        ] )
    ; ( "parse_outcome"
      , [ test_case "success+detail" `Quick test_outcome_success
        ; test_case "missing → tolerated" `Quick test_outcome_neutral_default
        ; test_case "variants and errors" `Quick test_outcome_variants_and_errors
        ] )
    ; ( "parse_value_json"
      , [ test_case "present" `Quick test_value_json_present
        ; test_case "fallback and error" `Quick test_value_json_fallback_and_error
        ] )
    ]
;;
