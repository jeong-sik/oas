(** Tests for Correction_pipeline — deterministic correction stages. *)

open Agent_sdk

let make_schema params : Types.tool_schema =
  { name = "test_tool"; description = "test"; parameters = params; strict = None }
;;

let int_param name required : Types.tool_param =
  { name; description = ""; param_type = Integer; required }
;;

let str_param name required : Types.tool_param =
  { name; description = ""; param_type = String; required }
;;

let bool_param name required : Types.tool_param =
  { name; description = ""; param_type = Boolean; required }
;;

let array_param name required : Types.tool_param =
  { name; description = ""; param_type = Array; required }
;;

let object_param name required : Types.tool_param =
  { name; description = ""; param_type = Object; required }
;;

let number_param name required : Types.tool_param =
  { name; description = ""; param_type = Number; required }
;;

let has_object_key key = function
  | `Assoc fields -> List.mem_assoc key fields
  | _ -> false
;;

(* ── Stage 1: Coercion ──────────────────────────────────── *)

let test_coercion_string_to_int () =
  let schema = make_schema [ int_param "count" true ] in
  let input = `Assoc [ "count", `String "42" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; corrections } ->
    let count = Yojson.Safe.Util.(corrected |> member "count" |> to_int) in
    Alcotest.(check int) "coerced" 42 count;
    Alcotest.(check bool) "has correction" true (List.length corrections > 0)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_coercion_string_to_bool () =
  let schema = make_schema [ bool_param "flag" true ] in
  let input = `Assoc [ "flag", `String "true" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; _ } ->
    let flag = Yojson.Safe.Util.(corrected |> member "flag" |> to_bool) in
    Alcotest.(check bool) "coerced" true flag
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_coercion_impossible () =
  let schema = make_schema [ int_param "count" true ] in
  let input = `Assoc [ "count", `String "not-a-number" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed _ -> Alcotest.fail "expected Still_invalid"
  | Still_invalid { errors; _ } ->
    Alcotest.(check bool) "has errors" true (List.length errors > 0)
;;

(* ── Stage 2: Default Injection ─────────────────────────── *)

let test_default_missing_optional () =
  let schema = make_schema [ str_param "name" true; str_param "format" false ] in
  let input = `Assoc [ "name", `String "test" ] in
  match
    Correction_pipeline.run ~schema ~stages:Correction_pipeline.zero_default_stages input
  with
  | Fixed { corrected; corrections } ->
    let format = Yojson.Safe.Util.(corrected |> member "format" |> to_string) in
    Alcotest.(check string) "default injected" "" format;
    Alcotest.(check bool)
      "correction logged"
      true
      (List.exists
         (fun c -> c.Correction_pipeline.stage = "default_injection")
         corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_default_pipeline_preserves_missing_optional () =
  let schema = make_schema [ str_param "name" true; str_param "format" false ] in
  let input = `Assoc [ "name", `String "test" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; corrections } ->
    Alcotest.(check bool)
      "format remains absent"
      false
      (has_object_key "format" corrected);
    Alcotest.(check int) "no correction" 0 (List.length corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_default_no_inject_if_present () =
  let schema = make_schema [ str_param "name" true; str_param "format" false ] in
  let input = `Assoc [ "name", `String "test"; "format", `String "compact" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrections; _ } ->
    Alcotest.(check bool)
      "no default injection"
      false
      (List.exists
         (fun c -> c.Correction_pipeline.stage = "default_injection")
         corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_default_null_input () =
  let schema = make_schema [ str_param "opt" false ] in
  match
    Correction_pipeline.run ~schema ~stages:Correction_pipeline.zero_default_stages `Null
  with
  | Fixed { corrected; _ } ->
    (match corrected with
     | `Assoc _ -> () (* null treated as empty object with defaults *)
     | _ -> Alcotest.fail "expected object")
  | Still_invalid _ -> () (* also acceptable if required fields missing *)
;;

(* ── Stage 3: LLM Format Recovery ───────────────────────── *)

let test_llm_format_recovery_known_wrapper () =
  let schema = make_schema [ int_param "count" true; str_param "label" false ] in
  let input = `Assoc [ "input", `Assoc [ "count", `Int 5 ] ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; corrections } ->
    let count = Yojson.Safe.Util.(corrected |> member "count" |> to_int) in
    Alcotest.(check int) "unwrapped count" 5 count;
    Alcotest.(check bool)
      "logged recovery"
      true
      (List.exists
         (fun c -> c.Correction_pipeline.stage = "llm_format_recovery")
         corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed after unwrap"
;;

let test_llm_format_recovery_schema_key_match () =
  (* Wrapper is not a known keyword, but inner keys match schema names. *)
  let schema = make_schema [ str_param "query" true ] in
  let input = `Assoc [ "wrapped", `Assoc [ "query", `String "find repos" ] ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; _ } ->
    let query = Yojson.Safe.Util.(corrected |> member "query" |> to_string) in
    Alcotest.(check string) "unwrapped query" "find repos" query
  | Still_invalid _ -> Alcotest.fail "expected Fixed after unwrap by schema match"
;;

let test_llm_format_recovery_no_match_preserved () =
  (* Unknown wrapper + no schema key overlap = preserve original. *)
  let schema = make_schema [ str_param "name" true ] in
  let input = `Assoc [ "foreign", `Assoc [ "other", `Int 42 ] ] in
  match Correction_pipeline.run ~schema input with
  | Fixed _ -> Alcotest.fail "expected Still_invalid — foreign wrapper preserved"
  | Still_invalid { attempted; _ } ->
    (* llm_format_recovery ran but decided not to unwrap, so no corrections *)
    Alcotest.(check bool)
      "no recovery correction"
      false
      (List.exists
         (fun c -> c.Correction_pipeline.stage = "llm_format_recovery")
         attempted)
;;

let test_llm_format_recovery_before_coercion () =
  (* Unwrap happens first, then coercion can fix the unwrapped fields. *)
  let schema = make_schema [ int_param "count" true ] in
  let input = `Assoc [ "args", `Assoc [ "count", `String "99" ] ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; corrections } ->
    let count = Yojson.Safe.Util.(corrected |> member "count" |> to_int) in
    Alcotest.(check int) "unwrapped then coerced" 99 count;
    (* Should have corrections from BOTH stages *)
    Alcotest.(check bool)
      "has recovery correction"
      true
      (List.exists (fun c -> c.Correction_pipeline.stage = "llm_format_recovery") corrections);
    Alcotest.(check bool)
      "has coercion correction"
      true
      (List.exists (fun c -> c.Correction_pipeline.stage = "coercion") corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

(* ── Stage 4: Format Normalization ──────────────────────── *)

let test_format_trim_whitespace () =
  let schema = make_schema [ str_param "msg" true ] in
  let input = `Assoc [ "msg", `String "  hello  " ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; _ } ->
    let msg = Yojson.Safe.Util.(corrected |> member "msg" |> to_string) in
    Alcotest.(check string) "trimmed" "hello" msg
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

(* ── Combined pipeline ──────────────────────────────────── *)

let test_multi_stage_correction () =
  let schema = make_schema [ int_param "count" true; str_param "label" false ] in
  (* count is string (needs coercion), label is optional and remains absent. *)
  let input = `Assoc [ "count", `String "7" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; corrections } ->
    let count = Yojson.Safe.Util.(corrected |> member "count" |> to_int) in
    Alcotest.(check int) "coerced count" 7 count;
    Alcotest.(check bool) "label remains absent" false (has_object_key "label" corrected);
    Alcotest.(check int) "coercion only" 1 (List.length corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed after multi-stage"
;;

let test_default_pipeline_preserves_execute_union_branch () =
  let schema =
    make_schema
      [ str_param "executable" false
      ; array_param "argv" false
      ; array_param "pipeline" false
      ; object_param "env" false
      ; str_param "cwd" false
      ; number_param "timeout_sec" false
      ]
  in
  let input = `Assoc [ "executable", `String "ls"; "argv", `List [ `String "repos" ] ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrected; _ } ->
    Alcotest.(check bool)
      "pipeline remains absent"
      false
      (has_object_key "pipeline" corrected);
    Alcotest.(check bool) "env remains absent" false (has_object_key "env" corrected);
    Alcotest.(check bool) "cwd remains absent" false (has_object_key "cwd" corrected);
    Alcotest.(check bool)
      "timeout_sec remains absent"
      false
      (has_object_key "timeout_sec" corrected)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_valid_input_no_corrections () =
  let schema = make_schema [ str_param "msg" true ] in
  let input = `Assoc [ "msg", `String "already valid" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed { corrections; _ } ->
    Alcotest.(check int) "no corrections" 0 (List.length corrections)
  | Still_invalid _ -> Alcotest.fail "expected Fixed"
;;

let test_missing_required () =
  let schema = make_schema [ str_param "name" true; str_param "id" true ] in
  let input = `Assoc [ "name", `String "ok" ] in
  match Correction_pipeline.run ~schema input with
  | Fixed _ -> Alcotest.fail "expected Still_invalid — required field missing"
  | Still_invalid { errors; _ } ->
    Alcotest.(check bool)
      "has id error"
      true
      (List.exists (fun e -> e.Tool_input_validation.path = "/id") errors)
;;

(* ── Idempotency ────────────────────────────────────────── *)

let test_idempotent () =
  let schema = make_schema [ int_param "n" true; str_param "s" false ] in
  let input = `Assoc [ "n", `String "5" ] in
  let first = Correction_pipeline.run ~schema input in
  match first with
  | Fixed { corrected; corrections = c1 } ->
    let second = Correction_pipeline.run ~schema corrected in
    (match second with
     | Fixed { corrections = c2; _ } ->
       Alcotest.(check int) "second run no new corrections" 0 (List.length c2);
       Alcotest.(check bool) "first had corrections" true (List.length c1 > 0)
     | Still_invalid _ -> Alcotest.fail "second run failed")
  | Still_invalid _ -> Alcotest.fail "first run failed"
;;

(* ── NonDet feedback ────────────────────────────────────── *)

let test_nondet_feedback () =
  let errors : Tool_input_validation.field_error list =
    [ { path = "/count"; expected = "integer"; actual = "string(\"abc\")" } ]
  in
  let attempted =
    [ { Correction_pipeline.stage = "coercion"
      ; field = "count"
      ; from_value = Some "\"abc\""
      ; to_value = "(failed)"
      }
    ]
  in
  let feedback =
    Correction_pipeline.build_nondet_feedback
      ~tool_name:"test"
      ~args:(`Assoc [ "count", `String "abc" ])
      ~still_invalid:errors
      ~attempted
  in
  Alcotest.(check bool) "non-empty" true (String.length feedback > 0);
  Alcotest.(check bool)
    "contains correction info"
    true
    (try
       let _ = Str.search_forward (Str.regexp_string "coercion") feedback 0 in
       true
     with
     | Not_found -> false)
;;

(* ── Runner ─────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Correction_pipeline"
    [ ( "coercion"
      , [ Alcotest.test_case "string to int" `Quick test_coercion_string_to_int
        ; Alcotest.test_case "string to bool" `Quick test_coercion_string_to_bool
        ; Alcotest.test_case "impossible coercion" `Quick test_coercion_impossible
        ] )
    ; ( "default_injection"
      , [ Alcotest.test_case "missing optional" `Quick test_default_missing_optional
        ; Alcotest.test_case
            "default pipeline preserves missing optional"
            `Quick
            test_default_pipeline_preserves_missing_optional
        ; Alcotest.test_case
            "no inject if present"
            `Quick
            test_default_no_inject_if_present
        ; Alcotest.test_case "null input" `Quick test_default_null_input
        ] )
    ; ( "llm_format_recovery"
      , [ Alcotest.test_case "known wrapper" `Quick test_llm_format_recovery_known_wrapper
        ; Alcotest.test_case
            "schema key match"
            `Quick
            test_llm_format_recovery_schema_key_match
        ; Alcotest.test_case
            "no match preserved"
            `Quick
            test_llm_format_recovery_no_match_preserved
        ; Alcotest.test_case
            "before coercion"
            `Quick
            test_llm_format_recovery_before_coercion
        ] )
    ; ( "format_normalization"
      , [ Alcotest.test_case "trim whitespace" `Quick test_format_trim_whitespace ] )
    ; ( "combined"
      , [ Alcotest.test_case "multi-stage" `Quick test_multi_stage_correction
        ; Alcotest.test_case
            "Execute branch-preserving default pipeline"
            `Quick
            test_default_pipeline_preserves_execute_union_branch
        ; Alcotest.test_case
            "valid = no corrections"
            `Quick
            test_valid_input_no_corrections
        ; Alcotest.test_case "missing required" `Quick test_missing_required
        ] )
    ; "idempotency", [ Alcotest.test_case "run twice = stable" `Quick test_idempotent ]
    ; ( "nondet_feedback"
      , [ Alcotest.test_case "enriched message" `Quick test_nondet_feedback ] )
    ]
;;
