(** Test_checkpoint_validation — Unit tests for DNA validation
    and continuity regression checks.

    LLM 0 — pure string processing, no model calls.
    @since 0.78.0 *)

open Agent_sdk

let test_valid_dna () =
  let dna =
    "goal: Deploy v2 to production\n\
     current task: Run integration tests\n\
     - Check API endpoints\n\
     - Verify database migrations\n\
     context: Sprint 42 release"
  in
  match Checkpoint_validation.validate_dna dna with
  | Ok _ -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "expected Ok, got Error: %s" e)
;;

let test_dna_too_short () =
  match Checkpoint_validation.validate_dna "short" with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for short DNA"
;;

let test_dna_no_markers () =
  let dna = String.make 100 'x' in
  match Checkpoint_validation.validate_dna dna with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for markerless DNA"
;;

let test_dna_mostly_whitespace () =
  let dna = "goal: test\n" ^ String.make 100 ' ' in
  match Checkpoint_validation.validate_dna dna with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for whitespace DNA"
;;

let test_dna_no_structure () =
  let dna = "goal task objective context " ^ String.make 50 'a' in
  match Checkpoint_validation.validate_dna dna with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected Error for structureless DNA"
;;

let test_continuity_perfect () =
  let full = "goal: deploy v2\ncurrent task: run tests\nlast line here" in
  let compressed = full in
  let result =
    Checkpoint_validation.continuity_check
      ~full_context:full
      ~compressed_context:compressed
  in
  let score = Yojson.Safe.Util.(result |> member "retention_score" |> to_float) in
  Alcotest.(check bool) "perfect retention" true (score >= 0.99)
;;

let test_continuity_total_loss () =
  let full = "goal: deploy v2\ncurrent task: run tests\nfinal status ok" in
  let compressed = "completely different unrelated text about cooking" in
  let result =
    Checkpoint_validation.continuity_check
      ~full_context:full
      ~compressed_context:compressed
  in
  let score = Yojson.Safe.Util.(result |> member "retention_score" |> to_float) in
  Alcotest.(check bool) "low retention" true (score < 0.5)
;;

let test_continuity_empty_context () =
  let result =
    Checkpoint_validation.continuity_check ~full_context:"" ~compressed_context:"anything"
  in
  let assessed = Yojson.Safe.Util.(result |> member "assessed" |> to_bool) in
  Alcotest.(check bool) "not assessed when empty" false assessed
;;

let test_overlap_ratio_identical () =
  let r =
    Checkpoint_validation.token_overlap_ratio
      ~source:"hello world test"
      ~target:"hello world test"
  in
  Alcotest.(check bool) "identical = 1.0" true (r >= 0.99)
;;

let test_overlap_ratio_disjoint () =
  let r =
    Checkpoint_validation.token_overlap_ratio
      ~source:"alpha beta gamma"
      ~target:"delta epsilon zeta"
  in
  Alcotest.(check bool) "disjoint = 0.0" true (r < 0.01)
;;

let test_contains_ci () =
  Alcotest.(check bool)
    "found"
    true
    (Checkpoint_validation.contains_substring_ci ~haystack:"Hello World" ~needle:"hello");
  Alcotest.(check bool)
    "not found"
    false
    (Checkpoint_validation.contains_substring_ci ~haystack:"Hello" ~needle:"xyz")
;;

let () =
  Alcotest.run
    "Checkpoint Validation"
    [ ( "validate_dna"
      , [ Alcotest.test_case "valid dna" `Quick test_valid_dna
        ; Alcotest.test_case "too short" `Quick test_dna_too_short
        ; Alcotest.test_case "no markers" `Quick test_dna_no_markers
        ; Alcotest.test_case "mostly whitespace" `Quick test_dna_mostly_whitespace
        ; Alcotest.test_case "no structure" `Quick test_dna_no_structure
        ] )
    ; ( "continuity_check"
      , [ Alcotest.test_case "perfect retention" `Quick test_continuity_perfect
        ; Alcotest.test_case "total loss" `Quick test_continuity_total_loss
        ; Alcotest.test_case "empty context" `Quick test_continuity_empty_context
        ] )
    ; ( "text_utils"
      , [ Alcotest.test_case "overlap identical" `Quick test_overlap_ratio_identical
        ; Alcotest.test_case "overlap disjoint" `Quick test_overlap_ratio_disjoint
        ; Alcotest.test_case "contains_ci" `Quick test_contains_ci
        ] )
    ]
;;
