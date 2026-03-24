(** Tests for Reflexion module — pure functions, no Eio needed. *)

open Alcotest
open Agent_sdk

(* ── String helpers (avoid external deps) ─────────── *)

let contains_sub ~affix s =
  let alen = String.length affix and slen = String.length s in
  if alen = 0 then true
  else if alen > slen then false
  else
    let rec check i =
      if i > slen - alen then false
      else if String.sub s i alen = affix then true
      else check (i + 1)
    in
    check 0

let starts_with_sub ~affix s =
  let alen = String.length affix in
  String.length s >= alen && String.sub s 0 alen = affix

(* ── Helpers ──────────────────────────────────────── *)

let make_response ?(content = [Types.Text "ok"]) () : Types.api_response =
  { id = "r1"; model = "test"; stop_reason = Types.EndTurn;
    content; usage = None }

let pass_evaluator _response = Reflexion.Pass

let fail_evaluator ~diagnosis ?(critique=[]) () _response =
  Reflexion.Fail { diagnosis; critique }

let counting_evaluator ~fail_until =
  let counter = ref 0 in
  fun _response ->
    incr counter;
    if !counter < fail_until then
      Reflexion.Fail { diagnosis = Printf.sprintf "attempt %d failed" !counter;
                       critique = ["try harder"] }
    else
      Reflexion.Pass

(* ── format_reflection ────────────────────────────── *)

let test_format_reflection_pass () =
  let text = Reflexion.format_reflection ~attempt_number:1 Reflexion.Pass in
  check string "pass produces empty" "" text

let test_format_reflection_fail () =
  let verdict = Reflexion.Fail {
    diagnosis = "wrong output";
    critique = ["missed edge case"; "bad formatting"];
  } in
  let text = Reflexion.format_reflection ~attempt_number:2 verdict in
  check bool "contains attempt number" true
    (String.length text > 0 && contains_sub ~affix:"attempt 2" text);
  check bool "contains diagnosis" true
    (contains_sub ~affix:"wrong output" text);
  check bool "contains critique" true
    (contains_sub ~affix:"missed edge case" text)

let test_format_reflection_no_critique () =
  let verdict = Reflexion.Fail {
    diagnosis = "wrong"; critique = [];
  } in
  let text = Reflexion.format_reflection ~attempt_number:1 verdict in
  check bool "contains diagnosis" true
    (contains_sub ~affix:"wrong" text);
  check bool "no critique section" true
    (not (contains_sub ~affix:"Critique" text))

(* ── run: immediate pass ──────────────────────────── *)

let test_run_immediate_pass () =
  let config = Reflexion.default_config ~evaluator:pass_evaluator in
  let run_agent ~reflections:_ = Ok (make_response ()) in
  match Reflexion.run ~config ~run_agent () with
  | Ok r ->
    check bool "passed" true r.passed;
    check int "total_attempts" 1 r.total_attempts;
    check int "attempts list length" 1 (List.length r.attempts)
  | Error _ -> fail "unexpected error"

(* ── run: retry then pass ─────────────────────────── *)

let test_run_retry_then_pass () =
  let evaluator = counting_evaluator ~fail_until:3 in
  let config = Reflexion.default_config ~evaluator in
  let call_count = ref 0 in
  let run_agent ~reflections =
    incr call_count;
    (* Verify reflections accumulate *)
    check int "reflections count" (max 0 (!call_count - 1)) (List.length reflections);
    Ok (make_response ())
  in
  match Reflexion.run ~config ~run_agent () with
  | Ok r ->
    check bool "passed on third attempt" true r.passed;
    check int "total_attempts" 3 r.total_attempts;
    (* First two attempts should have reflections *)
    let first = List.hd r.attempts in
    check bool "first attempt failed" true
      (match first.verdict with Reflexion.Fail _ -> true | _ -> false);
    check bool "first has reflection" true
      (Option.is_some first.reflection_text)
  | Error _ -> fail "unexpected error"

(* ── run: all attempts fail ───────────────────────── *)

let test_run_exhausted () =
  let evaluator = fail_evaluator ~diagnosis:"always wrong" () in
  let config = { (Reflexion.default_config ~evaluator) with max_attempts = 2 } in
  let run_agent ~reflections:_ = Ok (make_response ()) in
  match Reflexion.run ~config ~run_agent () with
  | Ok r ->
    check bool "not passed" false r.passed;
    check int "total_attempts" 2 r.total_attempts
  | Error _ -> fail "unexpected error"

(* ── run: agent error propagates ──────────────────── *)

let test_run_agent_error () =
  let config = Reflexion.default_config ~evaluator:pass_evaluator in
  let run_agent ~reflections:_ =
    Error (Error.Agent (MaxTurnsExceeded { turns = 1; limit = 1 }))
  in
  match Reflexion.run ~config ~run_agent () with
  | Ok _ -> fail "expected error"
  | Error _ -> ()

(* ── run: max_attempts = 1 ────────────────────────── *)

let test_run_single_attempt () =
  let evaluator = fail_evaluator ~diagnosis:"bad" () in
  let config = { (Reflexion.default_config ~evaluator) with max_attempts = 1 } in
  let run_agent ~reflections:_ = Ok (make_response ()) in
  match Reflexion.run ~config ~run_agent () with
  | Ok r ->
    check bool "not passed" false r.passed;
    check int "single attempt" 1 r.total_attempts
  | Error _ -> fail "unexpected error"

(* ── run: memory integration ──────────────────────── *)

let test_run_stores_in_memory () =
  let evaluator = counting_evaluator ~fail_until:2 in
  let config = Reflexion.default_config ~evaluator in
  let memory = Memory.create () in
  let run_agent ~reflections:_ = Ok (make_response ()) in
  match Reflexion.run ~config ~memory ~run_agent () with
  | Ok r ->
    check bool "passed" true r.passed;
    check int "2 attempts" 2 r.total_attempts;
    (* Check that reflection was stored in episodic memory *)
    let episodes = Memory.recall_episodes memory ~min_salience:0.0 () in
    check bool "has episodic entry" true (List.length episodes >= 1);
    let ep = List.hd episodes in
    check bool "id has reflexion prefix" true
      (starts_with_sub ~affix:"reflexion:" ep.id)
  | Error _ -> fail "unexpected error"

(* ── on_stop_evaluator ────────────────────────────── *)

let test_on_stop_evaluator () =
  let config = Reflexion.default_config ~evaluator:pass_evaluator in
  let response = make_response () in
  match Reflexion.on_stop_evaluator ~config response with
  | Reflexion.Pass -> ()
  | Reflexion.Fail _ -> fail "expected pass"

(* ── include_critique toggle ──────────────────────── *)

let test_no_critique_in_reflections () =
  let evaluator = counting_evaluator ~fail_until:2 in
  let config = { (Reflexion.default_config ~evaluator)
                 with include_critique = false } in
  let reflections_received = ref [] in
  let run_agent ~reflections =
    reflections_received := reflections;
    Ok (make_response ())
  in
  match Reflexion.run ~config ~run_agent () with
  | Ok r ->
    check bool "passed" true r.passed;
    (* Second call should have reflection without "Critique:" section *)
    (match !reflections_received with
     | [refl] ->
       check bool "no critique section" true
         (not (contains_sub ~affix:"Critique:" refl))
     | _ -> ())
  | Error _ -> fail "unexpected error"

(* ── Test suite ───────────────────────────────────── *)

let () =
  run "Reflexion" [
    "format_reflection", [
      test_case "pass" `Quick test_format_reflection_pass;
      test_case "fail with critique" `Quick test_format_reflection_fail;
      test_case "fail no critique" `Quick test_format_reflection_no_critique;
    ];
    "run", [
      test_case "immediate pass" `Quick test_run_immediate_pass;
      test_case "retry then pass" `Quick test_run_retry_then_pass;
      test_case "exhausted" `Quick test_run_exhausted;
      test_case "agent error" `Quick test_run_agent_error;
      test_case "single attempt" `Quick test_run_single_attempt;
      test_case "stores in memory" `Quick test_run_stores_in_memory;
      test_case "no critique toggle" `Quick test_no_critique_in_reflections;
    ];
    "hook", [
      test_case "on_stop_evaluator" `Quick test_on_stop_evaluator;
    ];
  ]
