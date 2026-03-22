(** Tests for Judge module -- LLM-based evaluation and scoring. *)

open Agent_sdk

(* ── Alcotest testable for risk_level ────────────────────── *)

let risk_level_eq (a : Judge.risk_level) (b : Judge.risk_level) =
  match a, b with
  | Low, Low | Medium, Medium | High, High | Critical, Critical -> true
  | _ -> false

let risk_level_pp fmt (r : Judge.risk_level) =
  Format.pp_print_string fmt (match r with
    | Low -> "Low" | Medium -> "Medium"
    | High -> "High" | Critical -> "Critical")

let risk_testable = Alcotest.testable risk_level_pp risk_level_eq

(* ── parse_judgment: valid JSON ──────────────────────────── *)

let test_parse_valid_json () =
  let json = {|{
    "score": 0.75,
    "confidence": 0.9,
    "risk": "high",
    "summary": "Potential security issue detected",
    "evidence": ["SQL injection pattern", "Unsanitized input"],
    "recommended_action": "Review input validation"
  }|} in
  match Judge.parse_judgment json with
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)
  | Ok j ->
    Alcotest.(check (float 0.001)) "score" 0.75 j.score;
    Alcotest.(check (float 0.001)) "confidence" 0.9 j.confidence;
    Alcotest.check risk_testable "risk" Judge.High j.risk;
    Alcotest.(check string) "summary" "Potential security issue detected" j.summary;
    Alcotest.(check int) "evidence count" 2 (List.length j.evidence);
    Alcotest.(check (option string)) "action"
      (Some "Review input validation") j.recommended_action

(* ── parse_judgment: minimal JSON ────────────────────────── *)

let test_parse_minimal_json () =
  let json = {|{"score": 0.2}|} in
  match Judge.parse_judgment json with
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)
  | Ok j ->
    Alcotest.(check (float 0.001)) "score" 0.2 j.score;
    (* confidence defaults to 0.5 when missing *)
    Alcotest.(check (float 0.001)) "confidence" 0.5 j.confidence;
    (* risk derived from score: 0.2 < 0.3 = Low *)
    Alcotest.check risk_testable "risk" Judge.Low j.risk

(* ── parse_judgment: malformed JSON returns Error ────────── *)

let test_parse_malformed_json () =
  let text = "This is not JSON at all" in
  match Judge.parse_judgment text with
  | Ok _ -> Alcotest.fail "Expected Error for non-JSON input"
  | Error msg ->
    Alcotest.(check bool) "has error message" true
      (String.length msg > 0)

(* ── parse_judgment: JSON with markdown fencing ──────────── *)

let test_parse_json_with_fencing () =
  let text = "```json\n{\"score\": 0.4, \"confidence\": 0.8, \"risk\": \"medium\", \"summary\": \"ok\"}\n```" in
  match Judge.parse_judgment text with
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)
  | Ok j ->
    Alcotest.(check (float 0.001)) "score" 0.4 j.score;
    Alcotest.(check (float 0.001)) "confidence" 0.8 j.confidence

(* ── parse_judgment: score clamped to 0-1 ────────────────── *)

let test_parse_clamps_score () =
  let json = {|{"score": 1.5, "confidence": -0.3}|} in
  match Judge.parse_judgment json with
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)
  | Ok j ->
    Alcotest.(check (float 0.001)) "score clamped" 1.0 j.score;
    Alcotest.(check (float 0.001)) "confidence clamped" 0.0 j.confidence

(* ── parse_judgment: null recommended_action ─────────────── *)

let test_parse_null_action () =
  let json = {|{"score": 0.5, "recommended_action": null}|} in
  match Judge.parse_judgment json with
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)
  | Ok j ->
    Alcotest.(check (option string)) "null action" None j.recommended_action

(* ── default_config values ───────────────────────────────── *)

let test_default_config () =
  let cfg = Judge.default_config () in
  Alcotest.(check string) "cascade_name" "judge" cfg.cascade_name;
  Alcotest.(check (float 0.001)) "temperature" 0.2 cfg.temperature;
  Alcotest.(check int) "max_tokens" 2048 cfg.max_tokens;
  Alcotest.(check int) "max_turns" 1 cfg.max_turns;
  Alcotest.(check bool) "output_schema is None"
    true (Option.is_none cfg.output_schema)

(* ── risk_of_score derivation ────────────────────────────── *)

let test_risk_of_score_low () =
  Alcotest.check risk_testable "0.0" Judge.Low (Judge.risk_of_score 0.0);
  Alcotest.check risk_testable "0.29" Judge.Low (Judge.risk_of_score 0.29)

let test_risk_of_score_medium () =
  Alcotest.check risk_testable "0.3" Judge.Medium (Judge.risk_of_score 0.3);
  Alcotest.check risk_testable "0.59" Judge.Medium (Judge.risk_of_score 0.59)

let test_risk_of_score_high () =
  Alcotest.check risk_testable "0.6" Judge.High (Judge.risk_of_score 0.6);
  Alcotest.check risk_testable "0.79" Judge.High (Judge.risk_of_score 0.79)

let test_risk_of_score_critical () =
  Alcotest.check risk_testable "0.8" Judge.Critical (Judge.risk_of_score 0.8);
  Alcotest.check risk_testable "1.0" Judge.Critical (Judge.risk_of_score 1.0)

(* ── judgment yojson roundtrip ───────────────────────────── *)

let test_judgment_roundtrip () =
  let j : Judge.judgment = {
    score = 0.65;
    confidence = 0.85;
    risk = Judge.High;
    summary = "Test summary";
    evidence = ["evidence1"; "evidence2"];
    recommended_action = Some "Fix it";
  } in
  let json = Judge.judgment_to_yojson j in
  match Judge.judgment_of_yojson json with
  | Error e -> Alcotest.fail (Printf.sprintf "Roundtrip failed: %s" e)
  | Ok j2 ->
    Alcotest.(check (float 0.001)) "score" j.score j2.score;
    Alcotest.(check (float 0.001)) "confidence" j.confidence j2.confidence;
    Alcotest.check risk_testable "risk" j.risk j2.risk;
    Alcotest.(check string) "summary" j.summary j2.summary;
    Alcotest.(check int) "evidence count" (List.length j.evidence) (List.length j2.evidence);
    Alcotest.(check (option string)) "action" j.recommended_action j2.recommended_action

let test_judgment_roundtrip_no_action () =
  let j : Judge.judgment = {
    score = 0.1;
    confidence = 0.95;
    risk = Judge.Low;
    summary = "All clear";
    evidence = [];
    recommended_action = None;
  } in
  let json = Judge.judgment_to_yojson j in
  match Judge.judgment_of_yojson json with
  | Error e -> Alcotest.fail (Printf.sprintf "Roundtrip failed: %s" e)
  | Ok j2 ->
    Alcotest.(check (float 0.001)) "score" j.score j2.score;
    Alcotest.(check (option string)) "action" None j2.recommended_action

(* ── risk_level yojson roundtrip ─────────────────────────── *)

let test_risk_level_roundtrip () =
  let levels = [Judge.Low; Medium; High; Critical] in
  List.iter (fun r ->
    let json = Judge.risk_level_to_yojson r in
    match Judge.risk_level_of_yojson json with
    | Error e -> Alcotest.fail (Printf.sprintf "risk roundtrip failed: %s" e)
    | Ok r2 ->
      Alcotest.check risk_testable "risk level" r r2
  ) levels

(* ── parse_judgment: evidence with non-string items ──────── *)

let test_parse_evidence_filters_non_strings () =
  let json = {|{"score": 0.5, "evidence": ["valid", 42, "also valid", null]}|} in
  match Judge.parse_judgment json with
  | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok, got Error: %s" e)
  | Ok j ->
    Alcotest.(check int) "evidence filtered" 2 (List.length j.evidence)

(* ── parse_judgment: risk string variants ────────────────── *)

let test_parse_risk_string_variants () =
  let cases = [
    ({|{"score": 0.5, "risk": "LOW"}|}, Judge.Low);
    ({|{"score": 0.5, "risk": "Medium"}|}, Judge.Medium);
    ({|{"score": 0.5, "risk": "HIGH"}|}, Judge.High);
    ({|{"score": 0.5, "risk": "CRITICAL"}|}, Judge.Critical);
  ] in
  List.iter (fun (json, expected_risk) ->
    match Judge.parse_judgment json with
    | Error e -> Alcotest.fail (Printf.sprintf "Expected Ok for %s, got Error: %s" json e)
    | Ok j ->
      Alcotest.check risk_testable (Printf.sprintf "risk for input") expected_risk j.risk
  ) cases

(* ── parse_judgment: score boundary values ───────────────── *)

let test_parse_score_boundaries () =
  (* Exactly at boundary: 0.3 -> Medium *)
  let json = {|{"score": 0.3}|} in
  (match Judge.parse_judgment json with
   | Error e -> Alcotest.fail e
   | Ok j -> Alcotest.check risk_testable "0.3" Judge.Medium j.risk);
  (* Exactly at boundary: 0.6 -> High *)
  let json = {|{"score": 0.6}|} in
  (match Judge.parse_judgment json with
   | Error e -> Alcotest.fail e
   | Ok j -> Alcotest.check risk_testable "0.6" Judge.High j.risk);
  (* Exactly at boundary: 0.8 -> Critical *)
  let json = {|{"score": 0.8}|} in
  (match Judge.parse_judgment json with
   | Error e -> Alcotest.fail e
   | Ok j -> Alcotest.check risk_testable "0.8" Judge.Critical j.risk)

(* ── Test suite ──────────────────────────────────────────── *)

let () =
  Alcotest.run "Judge" [
    "parse_judgment", [
      Alcotest.test_case "valid JSON" `Quick test_parse_valid_json;
      Alcotest.test_case "minimal JSON" `Quick test_parse_minimal_json;
      Alcotest.test_case "malformed JSON" `Quick test_parse_malformed_json;
      Alcotest.test_case "JSON with fencing" `Quick test_parse_json_with_fencing;
      Alcotest.test_case "clamps score/confidence" `Quick test_parse_clamps_score;
      Alcotest.test_case "null action" `Quick test_parse_null_action;
      Alcotest.test_case "evidence filters non-strings" `Quick test_parse_evidence_filters_non_strings;
      Alcotest.test_case "risk string variants" `Quick test_parse_risk_string_variants;
      Alcotest.test_case "score boundaries" `Quick test_parse_score_boundaries;
    ];
    "default_config", [
      Alcotest.test_case "values" `Quick test_default_config;
    ];
    "risk_of_score", [
      Alcotest.test_case "low" `Quick test_risk_of_score_low;
      Alcotest.test_case "medium" `Quick test_risk_of_score_medium;
      Alcotest.test_case "high" `Quick test_risk_of_score_high;
      Alcotest.test_case "critical" `Quick test_risk_of_score_critical;
    ];
    "serialization", [
      Alcotest.test_case "judgment roundtrip" `Quick test_judgment_roundtrip;
      Alcotest.test_case "judgment roundtrip no action" `Quick test_judgment_roundtrip_no_action;
      Alcotest.test_case "risk_level roundtrip" `Quick test_risk_level_roundtrip;
    ];
  ]
