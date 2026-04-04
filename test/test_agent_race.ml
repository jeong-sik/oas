(** Tests for Agent.t mutable-field race condition fix.

    Verifies that concurrent access to Agent.t mutable fields
    (state, lifecycle, last_tool_calls, consecutive_idle_turns)
    via the Eio.Mutex is safe under parallel Eio fibers. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────── *)

let make_agent env =
  let tools = [
    Tool.create ~name:"echo" ~description:"echo" ~parameters:[]
      (fun input -> Ok { Types.content = Yojson.Safe.to_string input });
  ] in
  Agent.create ~net:(Eio.Stdenv.net env) ~tools ()

(* ── Test: concurrent set_lifecycle from parallel fibers ── *)

let test_concurrent_set_lifecycle () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let agent = make_agent env in
  let n = 50 in
  (* Fork N fibers that all call set_lifecycle concurrently.
     Without the mutex, the read-modify-write of agent.lifecycle
     could lose updates (earlier timestamps overwritten). *)
  let done_count = Atomic.make 0 in
  for i = 1 to n do
    Eio.Fiber.fork ~sw (fun () ->
      let ts = Float.of_int i in
      Agent.set_lifecycle agent ~last_progress_at:ts Running;
      Atomic.incr done_count)
  done;
  (* All fibers complete within the switch scope *)
  Alcotest.(check int) "all fibers completed" n (Atomic.get done_count);
  (* Lifecycle should exist *)
  let lc = Agent.lifecycle agent in
  Alcotest.(check bool) "lifecycle present" true (Option.is_some lc);
  let snap = Option.get lc in
  Alcotest.(check bool) "status Running" true (snap.status = Running);
  (* last_progress_at should be one of the written values (not None) *)
  Alcotest.(check bool) "last_progress_at set" true
    (Option.is_some snap.last_progress_at)

(* ── Test: concurrent update_state from parallel fibers ── *)

let test_concurrent_update_state () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let agent = make_agent env in
  let n = 100 in
  (* Each fiber increments turn_count by 1 via update_state.
     Without the mutex, concurrent read-modify-write would lose
     increments. *)
  for _ = 1 to n do
    Eio.Fiber.fork ~sw (fun () ->
      Agent.update_state agent (fun s ->
        { s with turn_count = s.turn_count + 1 }))
  done;
  (* With Eio cooperative scheduling the fibers may not all interleave,
     but the mutex guarantees correctness regardless. *)
  let st = Agent.state agent in
  Alcotest.(check int) "turn_count after concurrent increments" n st.turn_count

(* ── Test: set_state is mutex-protected ─────────────────── *)

let test_set_state_protected () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  let st = Agent.state agent in
  Agent.set_state agent { st with turn_count = 42 };
  Alcotest.(check int) "turn_count set" 42 (Agent.state agent).turn_count

let test_update_state_cancellation_does_not_poison_mutex () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  let entered_lock, signal_entered = Eio.Promise.create () in
  (try
     Eio.Switch.run @@ fun sw ->
     Eio.Fiber.fork ~sw (fun () ->
       ignore
         (try
            Agent.update_state agent (fun s ->
              Eio.Promise.resolve signal_entered ();
              Eio.Fiber.yield ();
              { s with turn_count = s.turn_count + 1 });
            Ok ()
          with _ -> Error ()));
     Eio.Promise.await entered_lock;
     Eio.Switch.fail sw Exit
   with Exit -> ());
  let st = Agent.state agent in
  (try
     Agent.set_state agent { st with turn_count = 7 }
   with
   | Eio.Mutex.Poisoned exn ->
     Alcotest.failf "agent mutex was poisoned by cancellation: %s"
       (Printexc.to_string exn)
   | exn ->
     Alcotest.failf "unexpected exception after cancellation: %s"
       (Printexc.to_string exn));
  Alcotest.(check int) "mutex usable after cancellation" 7
    (Agent.state agent).turn_count

(* ── Test: set_consecutive_idle_turns is protected ──────── *)

let test_set_consecutive_idle_protected () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  Agent.set_consecutive_idle_turns agent 5;
  (* Read back through check_loop_guard which checks consecutive_idle_turns.
     With max_idle_turns=3 (default) and consecutive_idle_turns=5,
     it should fire the idle guard. *)
  let guard = Agent.check_loop_guard agent in
  match guard with
  | Some (Error.Agent (Error.IdleDetected _)) -> ()
  | _ ->
    Alcotest.fail "expected IdleDetected after setting consecutive_idle_turns=5"

(* ── Test: lifecycle transitions are ordered ────────────── *)

let test_lifecycle_transition_order () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  Agent.set_lifecycle agent ~accepted_at:1.0 Accepted;
  Agent.set_lifecycle agent ~ready_at:2.0 Ready;
  Agent.set_lifecycle agent ~started_at:3.0 Running;
  Agent.set_lifecycle agent ~finished_at:4.0 Completed;
  let snap = Option.get (Agent.lifecycle agent) in
  Alcotest.(check (option (float 0.001))) "accepted_at preserved"
    (Some 1.0) snap.accepted_at;
  Alcotest.(check (option (float 0.001))) "ready_at preserved"
    (Some 2.0) snap.ready_at;
  Alcotest.(check (option (float 0.001))) "started_at preserved"
    (Some 3.0) snap.started_at;
  Alcotest.(check (option (float 0.001))) "finished_at set"
    (Some 4.0) snap.finished_at;
  Alcotest.(check bool) "status Completed" true (snap.status = Completed)

(* ── Test: clone creates independent state ──────────────── *)

let test_clone_independent_state () =
  Eio_main.run @@ fun env ->
  let agent = make_agent env in
  Agent.set_lifecycle agent Running;
  let cloned = Agent.clone agent in
  (* Mutating the clone should not affect the original *)
  Agent.set_state cloned { (Agent.state cloned) with turn_count = 99 };
  Alcotest.(check int) "original unchanged" 0 (Agent.state agent).turn_count;
  Alcotest.(check int) "clone updated" 99 (Agent.state cloned).turn_count

(* ── Runner ───────────────────────────────────────────────── *)

let () =
  Alcotest.run "Agent race condition" [
    "mutex protection", [
      Alcotest.test_case "concurrent set_lifecycle" `Quick
        test_concurrent_set_lifecycle;
      Alcotest.test_case "concurrent update_state" `Quick
        test_concurrent_update_state;
      Alcotest.test_case "set_state protected" `Quick
        test_set_state_protected;
      Alcotest.test_case "update_state cancellation does not poison mutex" `Quick
        test_update_state_cancellation_does_not_poison_mutex;
      Alcotest.test_case "set_consecutive_idle protected" `Quick
        test_set_consecutive_idle_protected;
      Alcotest.test_case "lifecycle transition order" `Quick
        test_lifecycle_transition_order;
      Alcotest.test_case "clone independent state" `Quick
        test_clone_independent_state;
    ];
  ]
