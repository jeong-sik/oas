open Base
(** Unit tests for Durable module (v0.76.0). *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let ok_step name f : Durable.step =
  { name; execute = (fun input -> Ok (f input)); retry_limit = 1 }
;;

let fail_step name msg : Durable.step =
  { name; execute = (fun _ -> Error msg); retry_limit = 1 }
;;

let increment_step name : Durable.step =
  ok_step name (fun j ->
    let open Yojson.Safe.Util in
    `Int (j |> to_int |> succ))
;;

let double_step name : Durable.step =
  ok_step name (fun j ->
    let open Yojson.Safe.Util in
    `Int (j |> to_int |> ( * ) 2))
;;

(* ── Basic execution ─────────────────────────────── *)

let test_single_step () =
  let sm = Durable.create ~name:"single" () in
  let sm = Durable.add_step sm (increment_step "inc") in
  match Durable.execute sm (`Int 10) with
  | Completed { final_output = `Int 11; journal } ->
    check int "journal entries" 1 (List.length journal)
  | state ->
    fail
      (Printf.sprintf
         "expected Completed, got %s"
         (Yojson.Safe.to_string (Durable.execution_state_to_json state)))
;;

let test_multi_step_chain () =
  let sm = Durable.create ~name:"chain" () in
  let sm = Durable.add_step sm (increment_step "inc") in
  let sm = Durable.add_step sm (double_step "double") in
  let sm = Durable.add_step sm (increment_step "inc2") in
  (* 5 -> inc -> 6 -> double -> 12 -> inc2 -> 13 *)
  match Durable.execute sm (`Int 5) with
  | Completed { final_output = `Int 13; journal } ->
    check int "3 journal entries" 3 (List.length journal)
  | state ->
    fail
      (Printf.sprintf
         "expected Completed(13), got %s"
         (Yojson.Safe.to_string (Durable.execution_state_to_json state)))
;;

let test_empty_pipeline () =
  let sm = Durable.create ~name:"empty" () in
  match Durable.execute sm (`String "pass-through") with
  | Completed { final_output = `String "pass-through"; _ } -> ()
  | _ -> fail "expected pass-through completion"
;;

(* ── Step failure ────────────────────────────────── *)

let test_step_failure () =
  let sm = Durable.create ~name:"fail" () in
  let sm = Durable.add_step sm (increment_step "inc") in
  let sm = Durable.add_step sm (fail_step "broken" "kaboom") in
  let sm = Durable.add_step sm (increment_step "never") in
  match Durable.execute sm (`Int 0) with
  | Failed { at_step; error; journal } ->
    check string "at_step" "broken" at_step;
    check string "error" "kaboom" error;
    (* journal should have inc success + broken failure *)
    check int "journal" 2 (List.length journal)
  | _ -> fail "expected Failed"
;;

(* ── Retry ───────────────────────────────────────── *)

let test_retry_succeeds () =
  let call_count = ref 0 in
  let flaky : Durable.step =
    { name = "flaky"
    ; retry_limit = 3
    ; execute =
        (fun input ->
          incr call_count;
          if !call_count < 3 then Error "not yet" else Ok input)
    }
  in
  let sm = Durable.create ~name:"retry" () in
  let sm = Durable.add_step sm flaky in
  match Durable.execute sm (`Int 42) with
  | Completed { final_output = `Int 42; _ } -> check int "called 3 times" 3 !call_count
  | _ -> fail "expected Completed after retries"
;;

let test_retry_exhausted () =
  let flaky : Durable.step =
    { name = "always-fail"; retry_limit = 2; execute = (fun _ -> Error "nope") }
  in
  let sm = Durable.create ~name:"exhaust" () in
  let sm = Durable.add_step sm flaky in
  match Durable.execute sm (`Int 0) with
  | Failed { at_step = "always-fail"; error = "nope"; _ } -> ()
  | _ -> fail "expected Failed after retry exhaustion"
;;

(* ── Resume ──────────────────────────────────────── *)

let test_resume_from_failed () =
  (* First run: step 2 fails *)
  let attempt = ref 0 in
  let step2 : Durable.step =
    { name = "step2"
    ; retry_limit = 1
    ; execute =
        (fun input ->
          incr attempt;
          if !attempt <= 1 then Error "fail-first-time" else Ok input)
    }
  in
  let sm = Durable.create ~name:"resume" () in
  let sm = Durable.add_step sm (increment_step "step1") in
  let sm = Durable.add_step sm step2 in
  let state = Durable.execute sm (`Int 10) in
  (match state with
   | Failed { at_step = "step2"; _ } -> ()
   | _ -> fail "expected Failed at step2");
  (* Resume — step2 should succeed on second attempt *)
  let state2 = Durable.resume sm state in
  match state2 with
  | Completed { final_output = `Int 11; _ } -> ()
  | state ->
    fail
      (Printf.sprintf
         "expected Completed(11), got %s"
         (Yojson.Safe.to_string (Durable.execution_state_to_json state)))
;;

let test_resume_not_started () =
  let sm = Durable.create ~name:"r" () in
  let passthrough : Durable.step =
    { name = "pass"; execute = (fun x -> Ok x); retry_limit = 1 }
  in
  let sm = Durable.add_step sm passthrough in
  match Durable.resume sm Durable.NotStarted with
  | Completed { final_output = `Null; _ } -> ()
  | _ -> fail "expected Completed with Null from NotStarted resume"
;;

let test_resume_completed_is_noop () =
  let state = Durable.Completed { journal = []; final_output = `Int 42 } in
  let sm = Durable.create ~name:"noop" () in
  let result = Durable.resume sm state in
  match result with
  | Completed { final_output = `Int 42; _ } -> ()
  | _ -> fail "resume of Completed should return same state"
;;

(* ── Suspend ─────────────────────────────────────── *)

let test_suspend_in_progress () =
  let state = Durable.InProgress { current_step = "step2"; attempt = 1; journal = [] } in
  let sm = Durable.create ~name:"s" () in
  match Durable.suspend sm state ~reason:"user requested" with
  | Suspended { at_step = "step2"; reason = "user requested"; _ } -> ()
  | _ -> fail "expected Suspended"
;;

let test_suspend_non_in_progress () =
  let state = Durable.NotStarted in
  let sm = Durable.create ~name:"s" () in
  match Durable.suspend sm state ~reason:"why" with
  | NotStarted -> () (* should return state unchanged *)
  | _ -> fail "suspend of NotStarted should be noop"
;;

(* ── Serialization round-trip ────────────────────── *)

let test_serialization_not_started () =
  let state = Durable.NotStarted in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok NotStarted -> ()
  | _ -> fail "round-trip NotStarted"
;;

let test_serialization_completed () =
  let state = Durable.Completed { journal = []; final_output = `String "done" } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Completed { final_output = `String "done"; _ }) -> ()
  | _ -> fail "round-trip Completed"
;;

let test_serialization_failed () =
  let state = Durable.Failed { at_step = "step3"; journal = []; error = "timeout" } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Failed { at_step = "step3"; error = "timeout"; _ }) -> ()
  | _ -> fail "round-trip Failed"
;;

let test_serialization_suspended () =
  let state = Durable.Suspended { at_step = "step2"; journal = []; reason = "paused" } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Suspended { at_step = "step2"; reason = "paused"; _ }) -> ()
  | _ -> fail "round-trip Suspended"
;;

let test_serialization_in_progress () =
  let state = Durable.InProgress { current_step = "step1"; attempt = 2; journal = [] } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (InProgress { current_step = "step1"; attempt = 2; _ }) -> ()
  | _ -> fail "round-trip InProgress"
;;

let test_serialization_with_journal () =
  let je : Durable.journal_entry =
    { step_name = "s1"
    ; started_at = 1000.0
    ; completed_at = Some 1001.0
    ; input_json = `Int 5
    ; output_json = Some (`Int 6)
    ; error = None
    ; attempt = 1
    }
  in
  let state = Durable.Completed { journal = [ je ]; final_output = `Int 6 } in
  let json = Durable.execution_state_to_json state in
  match Durable.execution_state_of_json json with
  | Ok (Completed { journal; final_output = `Int 6 }) ->
    check int "journal len" 1 (List.length journal);
    let j = List.hd journal in
    check string "step_name" "s1" j.step_name;
    check int "attempt" 1 j.attempt
  | _ -> fail "round-trip with journal"
;;

let test_deserialization_invalid () =
  match Durable.execution_state_of_json (`String "garbage") with
  | Error _ -> ()
  | Ok _ -> fail "expected error on invalid json"
;;

(* ── Queries ─────────────────────────────────────── *)

let test_name_and_step_count () =
  let sm = Durable.create ~name:"my-sm" () in
  check string "name" "my-sm" (Durable.name sm);
  check int "initial steps" 0 (Durable.step_count sm);
  let sm = Durable.add_step sm (increment_step "a") in
  let sm = Durable.add_step sm (double_step "b") in
  check int "2 steps" 2 (Durable.step_count sm);
  check (list string) "names" [ "a"; "b" ] (Durable.step_names sm)
;;

let test_is_terminal () =
  check bool "NotStarted" false (Durable.is_terminal NotStarted);
  check
    bool
    "InProgress"
    false
    (Durable.is_terminal (InProgress { current_step = "x"; attempt = 1; journal = [] }));
  check
    bool
    "Suspended"
    false
    (Durable.is_terminal (Suspended { at_step = "x"; journal = []; reason = "r" }));
  check
    bool
    "Completed"
    true
    (Durable.is_terminal (Completed { journal = []; final_output = `Null }));
  check
    bool
    "Failed"
    true
    (Durable.is_terminal (Failed { at_step = "x"; journal = []; error = "e" }))
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "durable"
    [ ( "execution"
      , [ test_case "single step" `Quick test_single_step
        ; test_case "multi step chain" `Quick test_multi_step_chain
        ; test_case "empty pipeline" `Quick test_empty_pipeline
        ; test_case "step failure" `Quick test_step_failure
        ] )
    ; ( "retry"
      , [ test_case "retry succeeds" `Quick test_retry_succeeds
        ; test_case "retry exhausted" `Quick test_retry_exhausted
        ] )
    ; ( "resume"
      , [ test_case "resume from failed" `Quick test_resume_from_failed
        ; test_case "resume not started" `Quick test_resume_not_started
        ; test_case "resume completed is noop" `Quick test_resume_completed_is_noop
        ] )
    ; ( "suspend"
      , [ test_case "suspend in progress" `Quick test_suspend_in_progress
        ; test_case "suspend non-in-progress" `Quick test_suspend_non_in_progress
        ] )
    ; ( "serialization"
      , [ test_case "NotStarted round-trip" `Quick test_serialization_not_started
        ; test_case "Completed round-trip" `Quick test_serialization_completed
        ; test_case "Failed round-trip" `Quick test_serialization_failed
        ; test_case "Suspended round-trip" `Quick test_serialization_suspended
        ; test_case "InProgress round-trip" `Quick test_serialization_in_progress
        ; test_case "with journal" `Quick test_serialization_with_journal
        ; test_case "invalid json" `Quick test_deserialization_invalid
        ] )
    ; ( "queries"
      , [ test_case "name and step count" `Quick test_name_and_step_count
        ; test_case "is_terminal" `Quick test_is_terminal
        ] )
    ]
;;
