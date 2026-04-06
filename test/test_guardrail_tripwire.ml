(** Tests for Guardrail_tripwire — fail-fast parallel validators.
    All tests use Eio_main.run for structured concurrency.
    Uses Eio.Time.sleep (not Unix.sleepf) for cancellation-aware delays. *)

open Agent_sdk

(* ── Helpers ─────────────────────────────────────────────────── *)

let make_messages texts =
  List.map (fun text ->
    Types.{ role = User; content = [Text text]; name = None; tool_call_id = None }
  ) texts

let pass_tripwire name : Guardrail_tripwire.tripwire =
  { name; check = fun _msgs -> Ok () }

let fail_tripwire name reason : Guardrail_tripwire.tripwire =
  { name; check = fun _msgs -> Error reason }

(* ── No tripwires ────────────────────────────────────────────── *)

let test_no_tripwires_action_ok () =
  Eio_main.run @@ fun _env ->
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[]
    ~messages:(make_messages ["test"])
    ~action:(fun () -> Ok "done")
  in
  match result with
  | Ok "done" -> ()
  | _ -> Alcotest.fail "expected Ok done"

let test_no_tripwires_action_error () =
  Eio_main.run @@ fun _env ->
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[]
    ~messages:(make_messages ["test"])
    ~action:(fun () -> Error (Error.Internal "oops"))
  in
  match result with
  | Error (`Action (Error.Internal "oops")) -> ()
  | _ -> Alcotest.fail "expected Action error"

(* ── All tripwires pass, action succeeds ─────────────────────── *)

let test_all_pass () =
  Eio_main.run @@ fun _env ->
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[pass_tripwire "tw1"; pass_tripwire "tw2"]
    ~messages:(make_messages ["clean"])
    ~action:(fun () -> Ok 42)
  in
  match result with
  | Ok 42 -> ()
  | _ -> Alcotest.fail "expected Ok 42"

(* ── Tripwire fires, action not started yet ──────────────────── *)

let test_tripwire_fires_immediate () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let action_ran = ref false in
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[fail_tripwire "safety" "PII detected"]
    ~messages:(make_messages ["my SSN is 123"])
    ~action:(fun () ->
      (* Slow action: sleep so tripwire fires first *)
      Eio.Time.sleep clock 10.0;
      action_ran := true;
      Ok "should not reach")
  in
  (match result with
   | Error (`Tripped (Guardrail_tripwire.Tripped { tripwire_name; reason })) ->
     Alcotest.(check string) "name" "safety" tripwire_name;
     Alcotest.(check string) "reason" "PII detected" reason
   | Error (`Tripped Guardrail_tripwire.All_clear) ->
     Alcotest.fail "got All_clear instead of Tripped"
   | Error (`Action e) ->
     Alcotest.failf "got Action error: %s" (Error.to_string e)
   | Ok _ ->
     Alcotest.fail "expected Tripped, got Ok");
  Alcotest.(check bool) "action cancelled" false !action_ran

(* ── Action completes before tripwire ────────────────────────── *)

let test_action_wins () =
  Eio_main.run @@ fun _env ->
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[
      { name = "slow";
        check = fun _msgs ->
          (* Block until cancelled — but yield to let Eio cancel us *)
          let p, _ = Eio.Promise.create () in
          (try Eio.Promise.await p with Eio.Cancel.Cancelled _ -> ());
          Ok () }
    ]
    ~messages:(make_messages ["test"])
    ~action:(fun () -> Ok "fast")
  in
  match result with
  | Ok "fast" -> ()
  | _ -> Alcotest.fail "expected action to win"

(* ── Multiple tripwires, one fires ───────────────────────────── *)

let test_multiple_one_fires () =
  Eio_main.run @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[
      pass_tripwire "ok1";
      fail_tripwire "bad" "violation";
      pass_tripwire "ok2";
    ]
    ~messages:(make_messages ["test"])
    ~action:(fun () -> Eio.Time.sleep clock 10.0; Ok "slow")
  in
  match result with
  | Error (`Tripped (Guardrail_tripwire.Tripped { tripwire_name; _ })) ->
    Alcotest.(check string) "name" "bad" tripwire_name
  | _ -> Alcotest.fail "expected bad tripwire"

(* ── Action error propagates ─────────────────────────────────── *)

let test_action_error_propagates () =
  Eio_main.run @@ fun _env ->
  let result = Guardrail_tripwire.guarded_action
    ~tripwires:[
      { name = "slow";
        check = fun _msgs ->
          let p, _ = Eio.Promise.create () in
          (try Eio.Promise.await p with Eio.Cancel.Cancelled _ -> ());
          Ok () }
    ]
    ~messages:(make_messages ["test"])
    ~action:(fun () -> Error (Error.Internal "action failed"))
  in
  match result with
  | Error (`Action (Error.Internal "action failed")) -> ()
  | _ -> Alcotest.fail "expected Action error propagation"

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run "guardrail_tripwire" [
    ("no_tripwires", [
      Alcotest.test_case "action_ok" `Quick test_no_tripwires_action_ok;
      Alcotest.test_case "action_error" `Quick test_no_tripwires_action_error;
    ]);
    ("all_pass", [
      Alcotest.test_case "pass" `Quick test_all_pass;
    ]);
    ("tripwire_fires", [
      Alcotest.test_case "immediate" `Quick test_tripwire_fires_immediate;
      Alcotest.test_case "multiple_one_fires" `Quick test_multiple_one_fires;
    ]);
    ("action_wins", [
      Alcotest.test_case "fast_action" `Quick test_action_wins;
    ]);
    ("error_propagation", [
      Alcotest.test_case "action_error" `Quick test_action_error_propagates;
    ]);
  ]
