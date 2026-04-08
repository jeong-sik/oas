(** Integration tests for Runtime_server_worker.

    Exercises the major code paths in runtime_server_worker.ml that are
    currently at 5.41% coverage.  Each test creates a temp directory,
    initialises a Runtime_store and Runtime_server_types.state inside
    Eio_main.run, then drives the functions under test through their
    file-backed state machine.

    Functions covered:
    - extract_text:               various content_block patterns
    - make_event:                 seq numbering, timestamp, kind preservation
    - with_store_lock:            basic locking and reentrance
    - persist_event_locked:       session evolution + event append + file roundtrip
    - persist_event:              session-id lookup variant
    - generate_report_and_proof:  report/proof/telemetry/evidence artefacts
    - emit_output_delta:          empty and non-empty delta paths
    - run_participant:            mock/echo provider path *)

open Agent_sdk

(* ── Temp dir helper ───────────────────────────────────────────── *)

let with_temp_dir f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-worker-test-%d-%06x"
         (Unix.getpid ()) (Random.int 0xFFFFFF))
  in
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () ->
      ignore (Sys.command (Printf.sprintf "rm -rf %s" dir)))
    (fun () -> f dir)

(* ── Fixtures ──────────────────────────────────────────────────── *)

let make_start_request ?(session_id = "test-sess-001") ?(goal = "integration test")
    ?(participants = [ "alice"; "bob" ]) () : Runtime.start_request =
  {
    session_id = Some session_id;
    goal;
    participants;
    provider = Some "mock";
    model = Some "test-model";
    permission_mode = None;
    system_prompt = Some "you are a test agent";
    max_turns = Some 5;
    workdir = None;
  }

let session_id = "test-sess-001"

(** Build a store rooted at [dir] and ensure the session tree exists. *)
let make_store dir =
  match Runtime_store.create ~root:dir () with
  | Ok store ->
    (match Runtime_store.ensure_tree store session_id with
     | Ok () -> store
     | Error e ->
       Alcotest.fail
         (Printf.sprintf "ensure_tree failed: %s" (Error.to_string e)))
  | Error e ->
    Alcotest.fail
      (Printf.sprintf "store create failed: %s" (Error.to_string e))

(** Save an initial session via Runtime_projection.initial_session and persist it. *)
let save_initial_session store =
  let req = make_start_request () in
  let session = Runtime_projection.initial_session req in
  (match Runtime_store.save_session store session with
   | Ok () -> ()
   | Error e ->
     Alcotest.fail
       (Printf.sprintf "save_session failed: %s" (Error.to_string e)));
  session

(** Create state inside an Eio context, pointing session_root at [dir]. *)
let make_state net dir =
  let state = Runtime_server_types.create ~net () in
  state.session_root <- Some dir;
  state

(* ── extract_text tests ────────────────────────────────────────── *)

let test_extract_text_empty () =
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = []; usage = None; telemetry = None }
  in
  Alcotest.(check string) "empty content" "" (Runtime_server_worker.extract_text resp)

let test_extract_text_single () =
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = [ Types.Text "hello" ]; usage = None; telemetry = None }
  in
  Alcotest.(check string) "single text" "hello" (Runtime_server_worker.extract_text resp)

let test_extract_text_multiple () =
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = [ Types.Text "line1"; Types.Text "line2" ]; usage = None; telemetry = None }
  in
  Alcotest.(check string) "multi text" "line1\nline2"
    (Runtime_server_worker.extract_text resp)

let test_extract_text_filters_non_text () =
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = [
        Types.Text "before";
        Types.ToolUse { id = "t1"; name = "fn"; input = `Null };
        Types.Thinking { thinking_type = "thinking"; content = "hmm" };
        Types.Text "after";
      ]; usage = None; telemetry = None }
  in
  Alcotest.(check string) "filters non-text" "before\nafter"
    (Runtime_server_worker.extract_text resp)

let test_extract_text_only_non_text () =
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = [
        Types.ToolUse { id = "t1"; name = "fn"; input = `Null };
      ]; usage = None; telemetry = None }
  in
  Alcotest.(check string) "only non-text" "" (Runtime_server_worker.extract_text resp)

let test_extract_text_tool_result () =
  let resp : Types.api_response =
    { id = "r1"; model = "m"; stop_reason = EndTurn;
      content = [
        Types.Text "x";
        Types.ToolResult { tool_use_id = "t1"; content = "result"; is_error = false; json = None };
        Types.Text "y";
      ]; usage = None; telemetry = None }
  in
  Alcotest.(check string) "tool result filtered" "x\ny"
    (Runtime_server_worker.extract_text resp)

(* ── make_event tests ──────────────────────────────────────────── *)

let test_make_event_seq () =
  let session =
    Runtime_projection.initial_session (make_start_request ())
  in
  let event =
    Runtime_server_worker.make_event session
      (Session_started { goal = "test"; participants = [] })
  in
  Alcotest.(check int) "seq = last_seq + 1" 1 event.seq

let test_make_event_ts_positive () =
  let session =
    Runtime_projection.initial_session (make_start_request ())
  in
  let event =
    Runtime_server_worker.make_event session
      (Turn_recorded { actor = Some "alice"; message = "hi" })
  in
  Alcotest.(check bool) "ts > 0" true (event.ts > 0.0)

let test_make_event_kind_preserved () =
  let session =
    Runtime_projection.initial_session (make_start_request ())
  in
  let event =
    Runtime_server_worker.make_event session
      (Turn_recorded { actor = Some "bob"; message = "hello" })
  in
  match event.kind with
  | Turn_recorded { actor = Some "bob"; message = "hello" } -> ()
  | _ -> Alcotest.fail "kind not preserved"

let test_make_event_various_kinds () =
  let session =
    Runtime_projection.initial_session (make_start_request ())
  in
  let kinds : Runtime.event_kind list = [
    Session_started { goal = "g"; participants = ["a"] };
    Turn_recorded { actor = None; message = "m" };
    Agent_spawn_requested {
      participant_name = "p"; role = None; prompt = "do";
      provider = None; model = None; permission_mode = None };
    Agent_became_live {
      participant_name = "p"; summary = None;
      provider = None; model = None; error = None };
    Agent_completed {
      participant_name = "p"; summary = Some "done";
      provider = None; model = None; error = None };
    Agent_failed {
      participant_name = "p"; summary = None;
      provider = None; model = None; error = Some "err" };
    Agent_output_delta { participant_name = "p"; delta = "d" };
    Artifact_attached {
      artifact_id = "a1"; name = "n"; kind = "k";
      mime_type = "text/plain"; path = "/p"; size_bytes = 10 };
    Vote_recorded {
      topic = "t"; options = ["a";"b"]; choice = "a";
      actor = Some "v"; created_at = 0.0 };
    Checkpoint_saved { label = Some "cp"; path = "/cp" };
    Finalize_requested { reason = Some "done" };
    Session_completed { outcome = Some "ok" };
    Session_failed { outcome = Some "err" };
  ] in
  List.iteri (fun i kind ->
    let event = Runtime_server_worker.make_event session kind in
    Alcotest.(check int) (Printf.sprintf "kind %d seq" i) 1 event.seq
  ) kinds

(* ── with_store_lock tests ─────────────────────────────────────── *)

let test_with_store_lock_basic () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let state = Runtime_server_types.create ~net () in
  let result = Runtime_server_worker.with_store_lock state (fun () -> 42) in
  Alcotest.(check int) "lock returns value" 42 result

let test_with_store_lock_exception_passthrough () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let state = Runtime_server_types.create ~net () in
  match
    (try
       let _ =
         Runtime_server_worker.with_store_lock state (fun () ->
           failwith "boom")
       in
       None
     with Failure msg -> Some msg)
  with
  | Some "boom" -> ()
  | Some msg -> Alcotest.fail (Printf.sprintf "unexpected: %s" msg)
  | None -> Alcotest.fail "expected exception"

let test_with_store_lock_sequential () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let state = Runtime_server_types.create ~net () in
  let r = ref 0 in
  Runtime_server_worker.with_store_lock state (fun () -> r := !r + 1);
  Runtime_server_worker.with_store_lock state (fun () -> r := !r + 10);
  Alcotest.(check int) "sequential locks" 11 !r

let test_with_store_lock_cancellation_does_not_poison_mutex () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let state = Runtime_server_types.create ~net () in
  let entered_lock, signal_entered = Eio.Promise.create () in
  (try
     Eio.Switch.run @@ fun sw ->
     Eio.Fiber.fork ~sw (fun () ->
       ignore
         (try
            ignore
              (Runtime_server_worker.with_store_lock state (fun () ->
                 Eio.Promise.resolve signal_entered ();
                 Eio.Fiber.yield ();
                 1));
            Ok ()
          with _ -> Error ()));
     Eio.Promise.await entered_lock;
     Eio.Switch.fail sw Exit
   with Exit -> ());
  let result =
    try Ok (Runtime_server_worker.with_store_lock state (fun () -> 42))
    with
    | Eio.Mutex.Poisoned exn ->
      Error ("store mutex was poisoned by cancellation: " ^ Printexc.to_string exn)
    | exn ->
      Error ("unexpected exception after cancellation: " ^ Printexc.to_string exn)
  in
  match result with
  | Ok value ->
    Alcotest.(check int) "store mutex usable after cancellation" 42 value
  | Error msg ->
    Alcotest.fail msg

(* ── persist_event_locked tests ────────────────────────────────── *)

let test_persist_event_locked_basic () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let session = save_initial_session store in
    let kind : Runtime.event_kind =
      Session_started { goal = session.goal;
                        participants = session.planned_participants }
    in
    match Runtime_server_worker.persist_event_locked store state session kind with
    | Ok (updated_session, event) ->
      Alcotest.(check int) "event seq" 1 event.seq;
      Alcotest.(check int) "session last_seq updated" 1 updated_session.last_seq;
      (match Runtime_store.load_session store session_id with
       | Ok loaded ->
         Alcotest.(check int) "disk last_seq" 1 loaded.last_seq
       | Error e ->
         Alcotest.fail (Printf.sprintf "load failed: %s" (Error.to_string e)))
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "persist_event_locked failed: %s" (Error.to_string e)))

let test_persist_event_locked_multiple () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let session = save_initial_session store in
    let kind1 : Runtime.event_kind =
      Session_started { goal = "test"; participants = ["alice"; "bob"] }
    in
    match Runtime_server_worker.persist_event_locked store state session kind1 with
    | Error e ->
      Alcotest.fail (Printf.sprintf "event 1 failed: %s" (Error.to_string e))
    | Ok (session2, _ev1) ->
      let kind2 : Runtime.event_kind =
        Turn_recorded { actor = Some "alice"; message = "hello" }
      in
      match Runtime_server_worker.persist_event_locked store state session2 kind2 with
      | Error e ->
        Alcotest.fail (Printf.sprintf "event 2 failed: %s" (Error.to_string e))
      | Ok (session3, ev2) ->
        Alcotest.(check int) "second event seq" 2 ev2.seq;
        Alcotest.(check int) "session last_seq after 2" 2 session3.last_seq;
        (match Runtime_store.read_events store session_id () with
         | Ok events ->
           Alcotest.(check int) "2 events on disk" 2 (List.length events)
         | Error e ->
           Alcotest.fail (Printf.sprintf "read_events: %s" (Error.to_string e))))

(* ── persist_event tests ───────────────────────────────────────── *)

let test_persist_event_by_session_id () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    let kind : Runtime.event_kind =
      Session_started { goal = "test"; participants = ["alice"; "bob"] }
    in
    match Runtime_server_worker.persist_event store state session_id kind with
    | Ok (updated, event) ->
      Alcotest.(check int) "event seq" 1 event.seq;
      Alcotest.(check int) "session updated" 1 updated.last_seq
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "persist_event failed: %s" (Error.to_string e)))

let test_persist_event_nonexistent_session () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let kind : Runtime.event_kind =
      Turn_recorded { actor = None; message = "x" }
    in
    match
      Runtime_server_worker.persist_event store state "nonexistent-id" kind
    with
    | Ok _ -> Alcotest.fail "expected error for missing session"
    | Error _ -> ())

let test_persist_event_turn_recorded () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["alice"; "bob"] })
     with
     | Ok _ -> ()
     | Error e ->
       Alcotest.fail (Printf.sprintf "start failed: %s" (Error.to_string e)));
    let kind2 : Runtime.event_kind =
      Turn_recorded { actor = Some "alice"; message = "working on it" }
    in
    match Runtime_server_worker.persist_event store state session_id kind2 with
    | Ok (sess, ev) ->
      Alcotest.(check int) "turn event seq" 2 ev.seq;
      Alcotest.(check int) "turn count" 1 sess.turn_count
    | Error e ->
      Alcotest.fail (Printf.sprintf "turn failed: %s" (Error.to_string e)))

let test_persist_event_spawn () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["alice"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    let kind : Runtime.event_kind =
      Agent_spawn_requested {
        participant_name = "alice"; role = Some "execute";
        prompt = "do work"; provider = Some "mock";
        model = Some "test"; permission_mode = None;
      }
    in
    match Runtime_server_worker.persist_event store state session_id kind with
    | Ok (_sess, ev) ->
      Alcotest.(check int) "spawn seq" 2 ev.seq
    | Error e ->
      Alcotest.fail (Printf.sprintf "spawn failed: %s" (Error.to_string e)))

(* ── generate_report_and_proof tests ───────────────────────────── *)

let test_generate_report_and_proof_basic () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "integration test";
                            participants = ["alice"; "bob"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Turn_recorded { actor = Some "alice"; message = "done" })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match
      Runtime_server_worker.generate_report_and_proof
        store state session_id
    with
    | Ok (final_session, report, proof) ->
      Alcotest.(check string) "report session_id"
        session_id report.session_id;
      Alcotest.(check bool) "report has summary"
        true (List.length report.summary > 0);
      Alcotest.(check bool) "report has markdown"
        true (String.length report.markdown > 0);
      Alcotest.(check string) "proof session_id"
        session_id proof.session_id;
      Alcotest.(check bool) "proof has checks"
        true (List.length proof.checks > 0);
      let report_path = Runtime_store.report_json_path store session_id in
      Alcotest.(check bool) "report.json exists"
        true (Sys.file_exists report_path);
      let proof_path = Runtime_store.proof_json_path store session_id in
      Alcotest.(check bool) "proof.json exists"
        true (Sys.file_exists proof_path);
      Alcotest.(check bool) "session has artifacts"
        true (List.length final_session.artifacts > 0);
      Alcotest.(check bool) "at least 3 artifacts"
        true (List.length final_session.artifacts >= 3)
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "generate_report_and_proof failed: %s"
           (Error.to_string e)))

let test_generate_report_and_proof_empty_events () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    match
      Runtime_server_worker.generate_report_and_proof
        store state session_id
    with
    | Ok (_session, report, proof) ->
      Alcotest.(check string) "report session_id" session_id report.session_id;
      Alcotest.(check string) "proof session_id" session_id proof.session_id
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "report with no events failed: %s"
           (Error.to_string e)))

let test_generate_report_nonexistent_session () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    match
      Runtime_server_worker.generate_report_and_proof
        store state "no-such-session"
    with
    | Ok _ -> Alcotest.fail "expected error for missing session"
    | Error _ -> ())

(* ── emit_output_delta tests ───────────────────────────────────── *)

let test_emit_output_delta_empty () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    match
      Runtime_server_worker.emit_output_delta
        store state session_id "alice" ""
    with
    | Ok () -> ()
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "empty delta failed: %s" (Error.to_string e)))

let test_emit_output_delta_whitespace_only () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    match
      Runtime_server_worker.emit_output_delta
        store state session_id "alice" "   \t\n  "
    with
    | Ok () -> ()
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "whitespace delta failed: %s" (Error.to_string e)))

let test_emit_output_delta_nonempty () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["alice"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match
      Runtime_server_worker.emit_output_delta
        store state session_id "alice" "hello world"
    with
    | Ok () ->
      (match Runtime_store.read_events store session_id () with
       | Ok events ->
         let has_delta =
           List.exists (fun (ev : Runtime.event) ->
             match ev.kind with
             | Agent_output_delta { participant_name = "alice"; delta = "hello world" } ->
               true
             | _ -> false
           ) events
         in
         Alcotest.(check bool) "delta event exists" true has_delta
       | Error e ->
         Alcotest.fail (Printf.sprintf "read_events: %s" (Error.to_string e)))
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "non-empty delta failed: %s" (Error.to_string e)))

let test_emit_output_delta_nonexistent_session () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    match
      Runtime_server_worker.emit_output_delta
        store state "no-such-session" "alice" "text"
    with
    | Ok _ -> Alcotest.fail "expected error for missing session"
    | Error _ -> ())

(* ── run_participant tests (mock/echo path) ────────────────────── *)

let test_run_participant_mock () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["alice"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    let resolution : Runtime_server_resolve.execution_resolution = {
      selected_provider = "mock";
      requested_model = None;
      resolved_provider = None;
      resolved_model = None;
      provider_cfg = None;
    } in
    let detail : Runtime.spawn_agent_request = {
      participant_name = "alice";
      role = Some "execute";
      prompt = "say hello";
      provider = Some "mock";
      model = None;
      system_prompt = None;
      max_turns = None;
    } in
    match
      Runtime_server_worker.run_participant
        store state session_id resolution detail
    with
    | Ok text ->
      Alcotest.(check bool) "mock response non-empty"
        true (String.length text > 0);
      let expected = "Mock runtime response for alice: say hello" in
      Alcotest.(check string) "mock response content" expected text
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "run_participant mock failed: %s" (Error.to_string e)))

let test_run_participant_echo () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["bob"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    let resolution : Runtime_server_resolve.execution_resolution = {
      selected_provider = "echo";
      requested_model = None;
      resolved_provider = None;
      resolved_model = None;
      provider_cfg = None;
    } in
    let detail : Runtime.spawn_agent_request = {
      participant_name = "bob";
      role = None;
      prompt = "echo this message";
      provider = Some "echo";
      model = None;
      system_prompt = None;
      max_turns = None;
    } in
    match
      Runtime_server_worker.run_participant
        store state session_id resolution detail
    with
    | Ok text ->
      Alcotest.(check bool) "echo text non-empty" true (String.length text > 0);
      let expected = "Mock runtime response for bob: echo this message" in
      Alcotest.(check string) "echo response content" expected text
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "run_participant echo failed: %s" (Error.to_string e)))

(* ── persist_event with agent lifecycle events ────────────────── *)

let test_persist_agent_became_live () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["alice"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Agent_spawn_requested {
            participant_name = "alice"; role = Some "execute";
            prompt = "work"; provider = None; model = None;
            permission_mode = None })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match
      Runtime_server_worker.persist_event store state session_id
        (Agent_became_live {
           participant_name = "alice"; summary = None;
           provider = Some "mock"; model = Some "test";
           error = None })
    with
    | Ok (sess, ev) ->
      Alcotest.(check int) "became_live seq" 3 ev.seq;
      let alice_opt =
        List.find_opt (fun (p : Runtime.participant) -> p.name = "alice")
          sess.participants
      in
      (match alice_opt with
       | Some alice ->
         Alcotest.(check bool) "alice is Live"
           true (alice.state = Runtime.Live)
       | None ->
         Alcotest.fail "alice not found in participants")
    | Error e ->
      Alcotest.fail (Printf.sprintf "became_live: %s" (Error.to_string e)))

let test_persist_agent_completed () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "test"; participants = ["alice"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Agent_spawn_requested {
            participant_name = "alice"; role = None; prompt = "x";
            provider = None; model = None; permission_mode = None })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Agent_became_live {
            participant_name = "alice"; summary = None;
            provider = None; model = None; error = None })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match
      Runtime_server_worker.persist_event store state session_id
        (Agent_completed {
           participant_name = "alice"; summary = Some "task done";
           provider = None; model = None; error = None })
    with
    | Ok (sess, ev) ->
      Alcotest.(check int) "completed seq" 4 ev.seq;
      let alice_opt =
        List.find_opt (fun (p : Runtime.participant) -> p.name = "alice")
          sess.participants
      in
      (match alice_opt with
       | Some alice ->
         Alcotest.(check bool) "alice is Done"
           true (alice.state = Runtime.Done)
       | None ->
         Alcotest.fail "alice not found in participants")
    | Error e ->
      Alcotest.fail (Printf.sprintf "completed: %s" (Error.to_string e)))

(* ── Full lifecycle: start -> turns -> complete -> report ───────── *)

let test_full_lifecycle () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    let store = make_store dir in
    let _session = save_initial_session store in
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_started { goal = "full lifecycle";
                            participants = ["alice"] })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Turn_recorded { actor = Some "alice"; message = "working" })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.emit_output_delta
         store state session_id "alice" "partial output"
     with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Artifact_attached {
            artifact_id = "art-1"; name = "result.txt"; kind = "text";
            mime_type = "text/plain"; path = "/artifacts/result.txt";
            size_bytes = 42 })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (match
       Runtime_server_worker.persist_event store state session_id
         (Session_completed { outcome = Some "success" })
     with
     | Ok _ -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match
      Runtime_server_worker.generate_report_and_proof
        store state session_id
    with
    | Ok (final, report, proof) ->
      Alcotest.(check string) "report id" session_id report.session_id;
      Alcotest.(check string) "proof id" session_id proof.session_id;
      Alcotest.(check bool) "completed phase"
        true (final.phase = Runtime.Completed);
      (match Runtime_store.read_events store session_id () with
       | Ok events ->
         Alcotest.(check bool) "events >= 5"
           true (List.length events >= 5)
       | Error e ->
         Alcotest.fail (Printf.sprintf "read_events: %s" (Error.to_string e)))
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "report failed: %s" (Error.to_string e)))

(* ── store_of_state tests ──────────────────────────────────────── *)

let test_store_of_state_none () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  let state = Runtime_server_types.create ~net () in
  match Runtime_server_types.store_of_state state with
  | Ok _store -> ()
  | Error _ -> ()

let test_store_of_state_with_root () =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  with_temp_dir (fun dir ->
    let state = make_state net dir in
    match Runtime_server_types.store_of_state state with
    | Ok store ->
      Alcotest.(check string) "store root" dir store.root
    | Error e ->
      Alcotest.fail
        (Printf.sprintf "store_of_state failed: %s" (Error.to_string e)))

(* ── Test runner ───────────────────────────────────────────────── *)

let () =
  Alcotest.run "Runtime_server_worker integration"
    [
      ( "extract_text",
        [
          Alcotest.test_case "empty content" `Quick test_extract_text_empty;
          Alcotest.test_case "single Text" `Quick test_extract_text_single;
          Alcotest.test_case "multiple Text" `Quick test_extract_text_multiple;
          Alcotest.test_case "filters non-Text" `Quick test_extract_text_filters_non_text;
          Alcotest.test_case "only non-Text" `Quick test_extract_text_only_non_text;
          Alcotest.test_case "tool result filtered" `Quick test_extract_text_tool_result;
        ] );
      ( "make_event",
        [
          Alcotest.test_case "seq = last_seq + 1" `Quick test_make_event_seq;
          Alcotest.test_case "ts > 0" `Quick test_make_event_ts_positive;
          Alcotest.test_case "kind preserved" `Quick test_make_event_kind_preserved;
          Alcotest.test_case "various event_kind" `Quick test_make_event_various_kinds;
        ] );
      ( "with_store_lock",
        [
          Alcotest.test_case "basic" `Quick test_with_store_lock_basic;
          Alcotest.test_case "exception passthrough" `Quick test_with_store_lock_exception_passthrough;
          Alcotest.test_case "sequential" `Quick test_with_store_lock_sequential;
          Alcotest.test_case "cancellation does not poison mutex" `Quick
            test_with_store_lock_cancellation_does_not_poison_mutex;
        ] );
      ( "persist_event_locked",
        [
          Alcotest.test_case "basic persist" `Quick test_persist_event_locked_basic;
          Alcotest.test_case "multiple events" `Quick test_persist_event_locked_multiple;
        ] );
      ( "persist_event",
        [
          Alcotest.test_case "by session_id" `Quick test_persist_event_by_session_id;
          Alcotest.test_case "nonexistent session" `Quick test_persist_event_nonexistent_session;
          Alcotest.test_case "turn recorded" `Quick test_persist_event_turn_recorded;
          Alcotest.test_case "spawn requested" `Quick test_persist_event_spawn;
          Alcotest.test_case "agent became live" `Quick test_persist_agent_became_live;
          Alcotest.test_case "agent completed" `Quick test_persist_agent_completed;
        ] );
      ( "generate_report_and_proof",
        [
          Alcotest.test_case "basic" `Quick test_generate_report_and_proof_basic;
          Alcotest.test_case "empty events" `Quick test_generate_report_and_proof_empty_events;
          Alcotest.test_case "nonexistent session" `Quick test_generate_report_nonexistent_session;
        ] );
      ( "emit_output_delta",
        [
          Alcotest.test_case "empty delta" `Quick test_emit_output_delta_empty;
          Alcotest.test_case "whitespace only" `Quick test_emit_output_delta_whitespace_only;
          Alcotest.test_case "non-empty delta" `Quick test_emit_output_delta_nonempty;
          Alcotest.test_case "nonexistent session" `Quick test_emit_output_delta_nonexistent_session;
        ] );
      ( "run_participant",
        [
          Alcotest.test_case "mock provider" `Quick test_run_participant_mock;
          Alcotest.test_case "echo provider" `Quick test_run_participant_echo;
        ] );
      ( "full lifecycle",
        [
          Alcotest.test_case "start to report" `Quick test_full_lifecycle;
        ] );
      ( "store_of_state",
        [
          Alcotest.test_case "no root" `Quick test_store_of_state_none;
          Alcotest.test_case "with root" `Quick test_store_of_state_with_root;
        ] );
    ]
