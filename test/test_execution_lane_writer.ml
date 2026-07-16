open Alcotest
open Agent_sdk
module Event = Execution_event
module Journal = Execution_journal
module Store = Execution_event_store
module Writer = Execution_lane_writer
module Tx = Journal.Transaction

exception Cancel_waiter
exception Cancel_scope
exception Callback_failed

let require_submit = function
  | Ok ticket -> ticket
  | Error error -> fail (Writer.submit_error_to_string error)
;;

let require_ticket = function
  | Ok receipt -> receipt
  | Error error -> fail (Writer.ticket_error_to_string error)
;;

let require_closed = function
  | Ok () -> ()
  | Error error -> fail (Writer.scope_failure_to_string error)
;;

let require_scope = function
  | Ok value -> value
  | Error error -> fail (Writer.scope_failure_to_string error)
;;

let with_fresh dir f = require_scope (Writer.run ~dir (fun ~sw writer -> f sw writer))

let with_existing dir f =
  require_scope (Writer.resume ~dir (fun ~sw writer -> f sw writer))
;;

let require_codec = function
  | Ok value -> value
  | Error detail -> fail detail
;;

let with_temp_dir env f =
  let native_path = Filename.temp_file "oas-execution-lane-writer-" ".dir" in
  Sys.remove native_path;
  let dir = Eio.Path.(Eio.Stdenv.fs env / native_path) in
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir) (fun () -> f dir)
;;

let make_dir dir = Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 dir

let cursor_at cursor seq =
  match Journal.cursor_to_yojson cursor with
  | `Assoc fields ->
    require_codec
      (Journal.cursor_of_yojson
         (`Assoc (("seq", `Int seq) :: List.remove_assoc "seq" fields)))
  | _ -> fail "journal cursor encoder did not return an object"
;;

let check_cursor message expected actual =
  check
    bool
    message
    true
    (Store.Scope_id.equal (Store.cursor_scope_id expected) (Store.cursor_scope_id actual)
     && Store.cursor_seq expected = Store.cursor_seq actual)
;;

let read_complete_page writer ~after ~limit =
  match Writer.read_page writer ~after ~limit () with
  | Error error -> fail (Writer.read_error_to_string error)
  | Ok page ->
    check bool "requested page reaches its frozen watermark" false page.has_more;
    check
      int
      "page cursor reaches its frozen watermark"
      (Journal.cursor_seq page.high_watermark)
      (Journal.cursor_seq page.next_cursor);
    page.events, page.next_cursor
;;

let provider_attempt ordinal =
  let config =
    Llm_provider.Provider_config.make
      ~kind:Llm_provider.Provider_kind.OpenAI_compat
      ~provider_id:"lane-writer-test"
      ~model_id:"lane-writer-model"
      ~base_url:"https://provider.test"
      ()
  in
  let binding =
    Binding_identity.of_provider_config
      ~transport:(Binding_identity.transport_for_call ~injected:false)
      config
    |> require_codec
  in
  require_codec (Event.provider_attempt ~ordinal binding)
;;

let submit_and_await writer transaction =
  require_ticket (Writer.await (require_submit (Writer.submit writer transaction)))
;;

let check_single_event_group (receipt : _ Writer.receipt) =
  check int "one setup command per durable group" 1 receipt.group_event_count
;;

let open_output writer =
  let opened_run_receipt =
    submit_and_await writer (Tx.start_run ~agent_name:"lane-writer" ())
  in
  let run, opened_run = opened_run_receipt.value in
  let opened_turn_receipt =
    submit_and_await
      writer
      (Tx.open_node
         ~run
         ~parent:(Journal.run_root run)
         ~kind:(Event.Agent_turn { ordinal = 0 })
         ())
  in
  let turn, opened_turn = opened_turn_receipt.value in
  let opened_attempt_receipt =
    submit_and_await writer (Tx.open_node ~run ~parent:turn ~kind:(provider_attempt 0) ())
  in
  let attempt, opened_attempt = opened_attempt_receipt.value in
  let opened_output_receipt =
    submit_and_await
      writer
      (Tx.open_node
         ~run
         ~parent:attempt
         ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Thinking_block })
         ())
  in
  let output, opened_output = opened_output_receipt.value in
  check_single_event_group opened_run_receipt;
  check_single_event_group opened_turn_receipt;
  check_single_event_group opened_attempt_receipt;
  check_single_event_group opened_output_receipt;
  ( run
  , output
  , opened_output_receipt.through
  , [ opened_run; opened_turn; opened_attempt; opened_output ] )
;;

let delta_transaction output index =
  Tx.update_node ~node:output (Event.Output_delta (`Assoc [ "index", `Int index ]))
;;

let rec await_reconciliation_phase writer =
  let observed = Writer.stats writer in
  match observed.admission, observed.worker_phase with
  | Writer.Failed failure, _ -> fail (Writer.scope_failure_to_string failure)
  | Writer.Closed, _ -> fail "writer closed before reconciliation was observed"
  | ( (Writer.Accepting | Writer.Draining)
    , (Writer.Reconciling_group | Writer.Awaiting_reconciliation_wake) ) -> ()
  | ( (Writer.Accepting | Writer.Draining)
    , (Writer.Starting | Writer.Idle | Writer.Committing_group) ) ->
    Eio.Fiber.yield ();
    await_reconciliation_phase writer
;;

let rec await_reconciliation_wait writer ~outcome_count =
  let observed = Writer.stats writer in
  match observed.admission, observed.worker_phase, observed.current_reconciliation with
  | Writer.Failed failure, _, _ -> fail (Writer.scope_failure_to_string failure)
  | Writer.Closed, _, _ -> fail "writer closed before reconciliation wait"
  | ( (Writer.Accepting | Writer.Draining)
    , Writer.Awaiting_reconciliation_wake
    , Some evidence )
    when evidence.outcome_count = outcome_count -> observed
  | ( (Writer.Accepting | Writer.Draining)
    , ( Writer.Starting
      | Writer.Idle
      | Writer.Committing_group
      | Writer.Reconciling_group
      | Writer.Awaiting_reconciliation_wake )
    , _ ) ->
    Eio.Fiber.yield ();
    await_reconciliation_wait writer ~outcome_count
;;

let test_single_command_commits_and_close_drains () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let ticket =
        require_submit (Writer.submit writer (Tx.start_run ~agent_name:"single" ()))
      in
      Writer.close writer;
      (match Writer.submit writer (Tx.start_run ~agent_name:"late" ()) with
       | Error Writer.Admission_closed -> ()
       | Error error -> fail (Writer.submit_error_to_string error)
       | Ok _ -> fail "closed admission accepted another transaction");
      let receipt = require_ticket (Writer.await ticket) in
      check int "single event cursor" 1 (Journal.cursor_seq receipt.through);
      check int "single event group" 1 receipt.group_event_count;
      require_closed (Writer.await_closed writer);
      let observed = Writer.stats writer in
      (match observed.admission with
       | Writer.Closed -> ()
       | Writer.Accepting | Writer.Draining | Writer.Failed _ ->
         fail "drained actor did not reach closed admission");
      check int "accepted" 1 observed.accepted;
      check int "settled" 1 observed.settled;
      check int "empty queue" 0 observed.queue_depth;
      Writer.close writer;
      require_closed (Writer.await_closed writer)))
;;

let test_ready_set_is_one_fifo_durable_group () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let _run, output, setup_through, setup_events = open_output writer in
      check bool "setup values are exact events" true (List.length setup_events = 4);
      let tickets : Event.t Writer.ticket list =
        List.init 32 (fun index ->
          require_submit (Writer.submit writer (delta_transaction output index)))
      in
      let receipts : Event.t Writer.receipt list =
        List.map (fun ticket -> require_ticket (Writer.await ticket)) tickets
      in
      let expected_through = Journal.cursor_seq setup_through + 32 in
      List.iter
        (fun (receipt : Event.t Writer.receipt) ->
           check
             int
             "shared group cursor"
             expected_through
             (Journal.cursor_seq receipt.Writer.through);
           check int "shared group size" 32 receipt.Writer.group_event_count)
        receipts;
      let events, through = read_complete_page writer ~after:setup_through ~limit:32 in
      check int "replayed ready set" 32 (List.length events);
      check int "replayed cursor" expected_through (Journal.cursor_seq through);
      check
        bool
        "FIFO receipts equal durable replay"
        true
        (List.equal
           Event.equal
           (List.map (fun receipt -> receipt.Writer.value) receipts)
           events);
      let observed = Writer.stats writer in
      check int "five physical groups" 5 observed.committed_groups;
      check int "all commands observed" 36 observed.committed_commands;
      check int "all events observed" 36 observed.committed_events;
      require_closed (Writer.close_and_await writer)))
;;

let test_semantic_rejection_does_not_poison_ready_group () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let run, output, setup_through, _events = open_output writer in
      let first = require_submit (Writer.submit writer (delta_transaction output 0)) in
      let rejected =
        require_submit (Writer.submit writer (Tx.finish_run ~run Event.Succeeded))
      in
      let second = require_submit (Writer.submit writer (delta_transaction output 1)) in
      ignore (require_ticket (Writer.await first));
      (match Writer.await rejected with
       | Error
           (Writer.Transaction_rejected
              (Journal.Invariant_violation (Journal.Node_has_open_children node_id))) ->
         check
           bool
           "exact rejected root"
           true
           (Event.Node_id.equal node_id (Journal.run_root run))
       | Error error -> fail (Writer.ticket_error_to_string error)
       | Ok _ -> fail "invalid finish_run entered the durable group");
      ignore (require_ticket (Writer.await second));
      let events, through = read_complete_page writer ~after:setup_through ~limit:2 in
      check int "only valid events committed" 2 (List.length events);
      check int "cursor excludes rejected event" 6 (Journal.cursor_seq through);
      let observed = Writer.stats writer in
      check int "all admitted tickets settled" 7 observed.settled;
      check int "five physical groups" 5 observed.committed_groups;
      check int "only valid commands committed" 6 observed.committed_commands;
      require_closed (Writer.close_and_await writer)))
;;

let test_concurrent_submit_and_close_linearize_without_loss () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let _run, output, setup_through, setup_events = open_output writer in
      let accepted_before_race =
        require_submit (Writer.submit writer (delta_transaction output (-1)))
      in
      let race_size = 32 in
      let submissions = Array.make race_size None in
      let start, release_start = Eio.Promise.create () in
      Eio.Switch.run (fun race_sw ->
        Array.iteri
          (fun index _ ->
             Eio.Fiber.fork ~sw:race_sw (fun () ->
               Eio.Promise.await start;
               submissions.(index)
               <- Some (Writer.submit writer (delta_transaction output index))))
          submissions;
        Eio.Fiber.fork ~sw:race_sw (fun () ->
          Eio.Promise.await start;
          Writer.close writer);
        Eio.Promise.resolve release_start ());
      require_closed (Writer.await_closed writer);
      let accepted_tickets : Event.t Writer.ticket list ref =
        ref [ accepted_before_race ]
      in
      let rejected = ref 0 in
      Array.iter
        (function
          | Some (Ok ticket) -> accepted_tickets := ticket :: !accepted_tickets
          | Some (Error Writer.Admission_closed) -> incr rejected
          | Some (Error error) -> fail (Writer.submit_error_to_string error)
          | None -> fail "racing submitter did not publish its linearized result")
        submissions;
      check
        int
        "every racing submission linearized"
        race_size
        (List.length !accepted_tickets - 1 + !rejected);
      let receipts =
        List.map (fun ticket -> require_ticket (Writer.await ticket)) !accepted_tickets
      in
      let events, through =
        read_complete_page writer ~after:setup_through ~limit:(List.length receipts)
      in
      let receipt_events =
        List.map (fun receipt -> receipt.Writer.value) receipts
        |> List.sort (fun left right -> Int.compare (Event.seq left) (Event.seq right))
      in
      check
        bool
        "every accepted mutation is replayed exactly once"
        true
        (List.equal Event.equal receipt_events events);
      check
        int
        "replay cursor covers every accepted mutation"
        (Journal.cursor_seq setup_through + List.length receipts)
        (Journal.cursor_seq through);
      let observed = Writer.stats writer in
      let expected_accepted = List.length setup_events + List.length receipts in
      check int "exact accepted command count" expected_accepted observed.accepted;
      check int "accepted commands all settled" observed.accepted observed.settled;
      check int "drained queue is empty" 0 observed.queue_depth;
      check int "drained in-flight set is empty" 0 observed.in_flight_commands;
      check
        int
        "accepted commands are all durable"
        observed.accepted
        observed.committed_commands;
      check
        int
        "one event per accepted command"
        observed.committed_commands
        observed.committed_events;
      match Writer.submit writer (Tx.start_run ~agent_name:"after-race" ()) with
      | Error Writer.Admission_closed -> ()
      | Error error -> fail (Writer.submit_error_to_string error)
      | Ok _ -> fail "closed writer accepted a post-linearization transaction"))
;;

let test_cancelled_waiter_does_not_cancel_ticket () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let ticket =
        require_submit (Writer.submit writer (Tx.start_run ~agent_name:"waiter" ()))
      in
      let waiter_cancelled =
        match
          Eio.Cancel.sub (fun context ->
            Eio.Cancel.cancel context Cancel_waiter;
            ignore (Writer.await ticket);
            false)
        with
        | value -> value
        | exception Eio.Cancel.Cancelled Cancel_waiter -> true
        | exception exn -> raise exn
      in
      check bool "awaiting fiber was cancelled" true waiter_cancelled;
      let receipt = require_ticket (Writer.await ticket) in
      check int "accepted mutation survived waiter" 1 (Journal.cursor_seq receipt.through);
      require_closed (Writer.close_and_await writer)))
;;

let test_supervisor_cancellation_settles_accepted_ticket () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let ticket = ref None in
    (match
       with_fresh dir (fun sw writer ->
         ticket
         := Some
              (require_submit
                 (Writer.submit writer (Tx.start_run ~agent_name:"cancelled-scope" ())));
         Eio.Switch.fail sw Cancel_scope)
     with
     | () -> fail "failed supervisor switch returned normally"
     | exception Cancel_scope -> ()
     | exception exn -> raise exn);
    match Writer.await (Option.get !ticket) with
    | Error (Writer.Scope_failed (Writer.Supervisor_cancelled Cancel_scope)) -> ()
    | Error error -> fail (Writer.ticket_error_to_string error)
    | Ok _ -> fail "cancelled supervisor reported a durable ticket")
;;

let test_initialization_failure_is_scope_local () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun root ->
    make_dir root;
    let missing = Eio.Path.(root / "missing" / "scope") in
    let healthy = Eio.Path.(root / "healthy") in
    make_dir healthy;
    let failed, healthy =
      Eio.Fiber.pair
        (fun () ->
           Writer.run ~dir:missing (fun ~sw:_ failed_writer ->
             match Writer.await_ready failed_writer with
             | Error (Writer.Initialization_failed _) -> ()
             | Error error -> fail (Writer.scope_failure_to_string error)
             | Ok () -> fail "missing durability directory became ready"))
        (fun () ->
           Writer.run ~dir:healthy (fun ~sw:_ healthy_writer ->
             require_scope (Writer.await_ready healthy_writer);
             Writer.submit healthy_writer (Tx.start_run ~agent_name:"healthy-scope" ())
             |> require_submit
             |> Writer.await
             |> require_ticket))
    in
    (match failed with
     | Error (Writer.Initialization_failed _) -> ()
     | Error error -> fail (Writer.scope_failure_to_string error)
     | Ok () -> fail "missing durability directory closed successfully");
    match healthy with
    | Error error -> fail (Writer.scope_failure_to_string error)
    | Ok healthy_receipt ->
      check
        int
        "sibling scope remains durable"
        1
        (Journal.cursor_seq healthy_receipt.through))
;;

let test_clean_reopen_continues_exact_cursor () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let first_receipt = ref None in
    with_fresh dir (fun _sw writer ->
      let receipt =
        require_ticket
          (Writer.await
             (require_submit
                (Writer.submit writer (Tx.start_run ~agent_name:"reopen" ()))))
      in
      first_receipt := Some receipt;
      require_closed (Writer.close_and_await writer));
    let first = Option.get !first_receipt in
    with_existing dir (fun _sw writer ->
      let run, _opened = first.value in
      let second =
        require_ticket
          (Writer.await
             (require_submit
                (Writer.submit
                   writer
                   (Tx.open_node
                      ~run
                      ~parent:(Journal.run_root run)
                      ~kind:(Event.Agent_turn { ordinal = 0 })
                      ()))))
      in
      check
        int
        "reopened cursor advances exactly once"
        (Journal.cursor_seq first.through + 1)
        (Journal.cursor_seq second.through);
      let events, through = read_complete_page writer ~after:first.through ~limit:1 in
      check int "one post-reopen event" 1 (List.length events);
      let _node, opened = second.value in
      check
        bool
        "post-reopen identity is exact"
        true
        (Event.equal opened (List.hd events));
      check int "post-reopen replay cursor" 2 (Journal.cursor_seq through);
      require_closed (Writer.close_and_await writer)))
;;

let test_abort_transaction_is_one_durable_terminal_group () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let run, output, setup_through, _events = open_output writer in
      let terminal = Event.Cancelled { reason = Some "scope shutdown"; data = None } in
      let aborted =
        require_ticket
          (Writer.await
             (require_submit (Writer.submit writer (Tx.abort_run ~run terminal))))
      in
      check int "entire open subtree closes together" 4 aborted.group_event_count;
      let events, through = read_complete_page writer ~after:setup_through ~limit:4 in
      check
        bool
        "abort receipt is exact durable tail"
        true
        (List.equal Event.equal aborted.value events);
      check int "terminal watermark" 8 (Journal.cursor_seq through);
      let rejected = require_submit (Writer.submit writer (delta_transaction output 1)) in
      (match Writer.await rejected with
       | Error
           (Writer.Transaction_rejected
              (Journal.Invariant_violation (Journal.Node_already_closed node_id))) ->
         check bool "exact closed node" true (Event.Node_id.equal node_id output)
       | Error error -> fail (Writer.ticket_error_to_string error)
       | Ok _ -> fail "closed output accepted another delta");
      require_closed (Writer.close_and_await writer)))
;;

let test_repeated_unknown_waits_for_typed_external_wake () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let blocker = Eio.Path.(dir / "events.v1.commit") in
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 blocker;
    let durable_event = ref None in
    let durable_through = ref None in
    with_fresh dir (fun _sw writer ->
      let first =
        require_submit
          (Writer.submit writer (Tx.start_run ~agent_name:"unknown-create" ()))
      in
      let waiting = await_reconciliation_wait writer ~outcome_count:2 in
      check int "repeated unknown does not self-wake" 0 waiting.reconciliation_wakes;
      check
        bool
        "no wake source before an external event"
        true
        (Option.is_none waiting.last_reconciliation_wake);
      let second =
        require_submit
          (Writer.submit writer (Tx.start_run ~agent_name:"queued-during-wait" ()))
      in
      Eio.Fiber.yield ();
      let after_submit = Writer.stats writer in
      check
        int
        "submission does not retry filesystem"
        2
        after_submit.reconciliation_unknowns;
      check int "submission is not a durability wake" 0 after_submit.reconciliation_wakes;
      Eio.Path.rmtree ~missing_ok:false blocker;
      check
        bool
        "unknown outcome accepts typed wake"
        true
        (Writer.wake_reconciliation writer ~source:Writer.Durability_health_changed);
      check
        bool
        "a claimed wake cannot be claimed again"
        false
        (Writer.wake_reconciliation writer ~source:Writer.Operator_requested);
      let receipt = require_ticket (Writer.await first) in
      let _run, opened = receipt.value in
      durable_event := Some opened;
      durable_through := Some receipt.through;
      (match Writer.await second with
       | Error
           (Writer.Transaction_rejected
              (Journal.Invariant_violation Journal.Top_level_run_already_exists)) -> ()
       | Error error -> fail (Writer.ticket_error_to_string error)
       | Ok _ -> fail "second top-level run was accepted");
      check int "reconciled command commits once" 1 (Journal.cursor_seq receipt.through);
      check int "reconciled command is one durability group" 1 receipt.group_event_count;
      let observed = Writer.stats writer in
      check int "both unknown outcomes are observed" 2 observed.reconciliation_unknowns;
      check int "reconciliation wake is observed" 1 observed.reconciliation_wakes;
      check
        bool
        "typed wake source is retained"
        true
        (observed.last_reconciliation_wake
         = Some (Writer.External_wake Writer.Durability_health_changed));
      check
        bool
        "successful reconciliation clears current evidence"
        true
        (Option.is_none observed.current_reconciliation);
      require_closed (Writer.close_and_await writer));
    let through = Option.get !durable_through in
    with_existing dir (fun _sw writer ->
      require_scope (Writer.await_ready writer);
      let page =
        match
          Writer.read_page writer ~after:(cursor_at through 0) ~through ~limit:1 ()
        with
        | Ok page -> page
        | Error error -> fail (Writer.read_error_to_string error)
      in
      check
        bool
        "reopen preserves the exact reconciled event"
        true
        (List.equal Event.equal [ Option.get !durable_event ] page.events);
      check_cursor "reopen preserves the reconciled cursor" through page.next_cursor))
;;

let test_close_terminates_unresolved_reconciliation () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let blocker = Eio.Path.(dir / "events.v1.commit") in
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 blocker;
    let writer_ref = ref None in
    let first_waiter = ref None in
    let second_waiter = ref None in
    let outcome =
      Writer.run ~dir (fun ~sw writer ->
        writer_ref := Some writer;
        let first =
          require_submit
            (Writer.submit writer (Tx.start_run ~agent_name:"close-unknown-1" ()))
        in
        let second =
          require_submit
            (Writer.submit writer (Tx.start_run ~agent_name:"close-unknown-2" ()))
        in
        ignore (await_reconciliation_wait writer ~outcome_count:2);
        Eio.Fiber.fork ~sw (fun () -> first_waiter := Some (Writer.await first));
        Eio.Fiber.fork ~sw (fun () -> second_waiter := Some (Writer.await second)))
    in
    (match outcome with
     | Error (Writer.Reconciliation_unresolved_on_close { evidence }) ->
       check int "close performs one final exact reconciliation" 3 evidence.outcome_count
     | Error error -> fail (Writer.scope_failure_to_string error)
     | Ok () -> fail "unresolved reconciliation closed successfully");
    let check_waiter = function
      | Error
          (Writer.Scope_failed (Writer.Reconciliation_unresolved_on_close { evidence }))
        -> check int "ticket retains close evidence" 3 evidence.outcome_count
      | Error error -> fail (Writer.ticket_error_to_string error)
      | Ok _ -> fail "ambiguous ticket reported durable success"
    in
    check_waiter (Option.get !first_waiter);
    check_waiter (Option.get !second_waiter);
    let observed = Writer.stats (Option.get !writer_ref) in
    check int "close wake is observed" 1 observed.reconciliation_wakes;
    check
      bool
      "close wake source is typed"
      true
      (observed.last_reconciliation_wake = Some Writer.Close_requested);
    check int "all ambiguous tickets settle" observed.accepted observed.settled;
    check int "failed close clears queue" 0 observed.queue_depth;
    check int "failed close clears in-flight" 0 observed.in_flight_commands;
    Eio.Path.rmtree ~missing_ok:false blocker;
    require_scope
      (Writer.resume ~dir (fun ~sw:_ writer ->
         require_scope (Writer.await_ready writer);
         match Writer.current_cursor writer with
         | Ok cursor ->
           check int "unresolved close publishes no events" 0 (Journal.cursor_seq cursor)
         | Error error -> fail (Writer.read_error_to_string error))))
;;

let test_each_external_wake_authorizes_one_reconciliation () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let blocker = Eio.Path.(dir / "events.v1.commit") in
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 blocker;
    let ticket_ref = ref None in
    let outcome =
      Writer.run ~dir (fun ~sw:_ writer ->
        let ticket =
          require_submit
            (Writer.submit writer (Tx.start_run ~agent_name:"operator-wake" ()))
        in
        ticket_ref := Some ticket;
        ignore (await_reconciliation_wait writer ~outcome_count:2);
        check
          bool
          "operator wake is accepted"
          true
          (Writer.wake_reconciliation writer ~source:Writer.Operator_requested);
        let waiting = await_reconciliation_wait writer ~outcome_count:3 in
        check int "one external wake causes one retry" 1 waiting.reconciliation_wakes;
        check
          bool
          "operator wake source is retained"
          true
          (waiting.last_reconciliation_wake
           = Some (Writer.External_wake Writer.Operator_requested));
        Eio.Fiber.yield ();
        let stable = Writer.stats writer in
        check
          int
          "no autonomous retry follows the external attempt"
          3
          stable.reconciliation_unknowns)
    in
    (match outcome with
     | Error (Writer.Reconciliation_unresolved_on_close { evidence }) ->
       check int "close owns the next and final retry" 4 evidence.outcome_count
     | Error error -> fail (Writer.scope_failure_to_string error)
     | Ok () -> fail "blocked reconciliation closed successfully");
    match Writer.await (Option.get !ticket_ref) with
    | Error (Writer.Scope_failed (Writer.Reconciliation_unresolved_on_close _)) -> ()
    | Error error -> fail (Writer.ticket_error_to_string error)
    | Ok _ -> fail "blocked ticket reported durable success")
;;

let test_owned_supervisor_drains_same_scope_waiter () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let writer = ref None in
    let ticket = ref None in
    let waiter = ref None in
    require_scope
      (Writer.run ~dir (fun ~sw created ->
         writer := Some created;
         let accepted =
           require_submit
             (Writer.submit created (Tx.start_run ~agent_name:"owned-scope" ()))
         in
         ticket := Some accepted;
         Eio.Fiber.fork ~sw (fun () -> waiter := Some (Writer.await accepted))));
    let receipt = require_ticket (Option.get !waiter) in
    check
      int
      "owned supervisor drains accepted ticket"
      1
      (Journal.cursor_seq receipt.through);
    require_closed (Writer.await_closed (Option.get !writer));
    let observed = Writer.stats (Option.get !writer) in
    check
      int
      "owned supervisor settles accepted ticket"
      observed.accepted
      observed.settled;
    check int "owned supervisor clears queue" 0 observed.queue_depth;
    check int "owned supervisor clears in-flight" 0 observed.in_flight_commands;
    ignore (require_ticket (Writer.await (Option.get !ticket))))
;;

let test_callback_exception_preserves_durable_group_truth () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let writer_ref = ref None in
    let setup_through_ref = ref None in
    let first_receipt = ref None in
    let second_ticket = ref None in
    (match
       with_fresh dir (fun _sw writer ->
         writer_ref := Some writer;
         let _run, output, setup_through, _events = open_output writer in
         setup_through_ref := Some setup_through;
         let first = require_submit (Writer.submit writer (delta_transaction output 1)) in
         let second =
           require_submit (Writer.submit writer (delta_transaction output 2))
         in
         second_ticket := Some second;
         first_receipt := Some (require_ticket (Writer.await first));
         (match Writer.ticket_phase second with
          | Writer.Committing -> ()
          | Writer.Queued | Writer.Reconciling | Writer.Settled ->
            fail "second ticket was not pending inside durable settlement");
         raise Callback_failed)
     with
     | () -> fail "callback failure returned normally"
     | exception Callback_failed -> ()
     | exception exn -> raise exn);
    let first = Option.get !first_receipt in
    let second = require_ticket (Writer.await (Option.get !second_ticket)) in
    check_cursor
      "callback failure keeps one durable group cursor"
      first.through
      second.through;
    check int "callback failure keeps exact durable group size" 2 first.group_event_count;
    check int "callback failure keeps sibling group size" 2 second.group_event_count;
    require_closed (Writer.await_closed (Option.get !writer_ref));
    let observed = Writer.stats (Option.get !writer_ref) in
    check
      int
      "callback failure settles every accepted ticket"
      observed.accepted
      observed.settled;
    check int "callback failure clears queue" 0 observed.queue_depth;
    check int "callback failure clears in-flight" 0 observed.in_flight_commands;
    with_existing dir (fun _sw writer ->
      require_scope (Writer.await_ready writer);
      let page =
        match
          Writer.read_page
            writer
            ~after:(Option.get !setup_through_ref)
            ~through:first.through
            ~limit:2
            ()
        with
        | Ok page -> page
        | Error error -> fail (Writer.read_error_to_string error)
      in
      check
        bool
        "callback failure replays both durable events"
        true
        (List.equal Event.equal [ first.value; second.value ] page.events)))
;;

let test_callback_exception_retains_unresolved_scope_failure () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let blocker = Eio.Path.(dir / "events.v1.commit") in
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 blocker;
    let ticket_ref = ref None in
    (match
       Writer.run ~dir (fun ~sw:_ writer ->
         let ticket =
           require_submit
             (Writer.submit writer (Tx.start_run ~agent_name:"callback-unknown" ()))
         in
         ticket_ref := Some ticket;
         ignore (await_reconciliation_wait writer ~outcome_count:2);
         raise Callback_failed)
     with
     | exception
         Writer.Callback_failed_after_scope_failure
           (Callback_failed, Writer.Reconciliation_unresolved_on_close { evidence }) ->
       check int "callback and close retain exact evidence" 3 evidence.outcome_count
     | exception exn -> raise exn
     | Ok _ | Error _ -> fail "callback failure returned as a normal scope result");
    match Writer.await (Option.get !ticket_ref) with
    | Error (Writer.Scope_failed (Writer.Reconciliation_unresolved_on_close { evidence }))
      -> check int "ticket retains the same close evidence" 3 evidence.outcome_count
    | Error error -> fail (Writer.ticket_error_to_string error)
    | Ok _ -> fail "ambiguous ticket reported durable success")
;;

let test_durable_success_settles_group_before_supervisor_cancellation () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    let writer_ref = ref None in
    let output_ref = ref None in
    let setup_through_ref = ref None in
    let first_ticket = ref None in
    let second_ticket = ref None in
    let first_receipt = ref None in
    (match
       with_fresh dir (fun sw writer ->
         writer_ref := Some writer;
         let _run, output, setup_through, _events = open_output writer in
         output_ref := Some output;
         setup_through_ref := Some setup_through;
         let first = require_submit (Writer.submit writer (delta_transaction output 1)) in
         let second =
           require_submit (Writer.submit writer (delta_transaction output 2))
         in
         first_ticket := Some first;
         second_ticket := Some second;
         first_receipt := Some (require_ticket (Writer.await first));
         (match Writer.ticket_phase second with
          | Writer.Committing -> ()
          | Writer.Queued | Writer.Reconciling | Writer.Settled ->
            fail "second ticket was not pending inside durable settlement");
         Eio.Switch.fail sw Cancel_scope)
     with
     | () -> fail "failed supervisor switch returned normally"
     | exception Cancel_scope -> ()
     | exception exn -> raise exn);
    let first = Option.get !first_receipt in
    let second = require_ticket (Writer.await (Option.get !second_ticket)) in
    check
      int
      "same durable group cursor"
      (Journal.cursor_seq first.through)
      (Journal.cursor_seq second.through);
    check int "same durable group size" first.group_event_count second.group_event_count;
    check int "both commands committed together" 2 first.group_event_count;
    (match Writer.await_closed (Option.get !writer_ref) with
     | Error (Writer.Supervisor_cancelled Cancel_scope) -> ()
     | Error error -> fail (Writer.scope_failure_to_string error)
     | Ok () -> fail "cancelled supervisor reported a normal actor close");
    let observed = Writer.stats (Option.get !writer_ref) in
    check
      int
      "cancelled scope settles every accepted ticket"
      observed.accepted
      observed.settled;
    check int "cancelled scope clears queue" 0 observed.queue_depth;
    check int "cancelled scope clears in-flight" 0 observed.in_flight_commands;
    with_existing dir (fun _sw writer ->
      let output = Option.get !output_ref in
      ignore (submit_and_await writer (delta_transaction output 3));
      let page =
        match
          Writer.read_page
            writer
            ~after:(Option.get !setup_through_ref)
            ~through:first.through
            ~limit:2
            ()
        with
        | Ok page -> page
        | Error error -> fail (Writer.read_error_to_string error)
      in
      check
        bool
        "reopen replays the exact successful group"
        true
        (List.equal Event.equal [ first.value; second.value ] page.events);
      require_closed (Writer.close_and_await writer)))
;;

let test_frozen_pages_remain_lossless_after_close () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    make_dir dir;
    with_fresh dir (fun _sw writer ->
      let _run, output, setup_through, _events = open_output writer in
      let first = require_submit (Writer.submit writer (delta_transaction output 1)) in
      let second = require_submit (Writer.submit writer (delta_transaction output 2)) in
      let first_receipt = require_ticket (Writer.await first) in
      let second_receipt = require_ticket (Writer.await second) in
      let first_page =
        match Writer.read_page writer ~after:setup_through ~limit:1 () with
        | Ok page -> page
        | Error error -> fail (Writer.read_error_to_string error)
      in
      check bool "first frozen page has a remainder" true first_page.has_more;
      check_cursor
        "first page freezes the exact current watermark"
        second_receipt.through
        first_page.high_watermark;
      check_cursor
        "first page advances by the exact first event"
        (cursor_at setup_through (Journal.cursor_seq setup_through + 1))
        first_page.next_cursor;
      let third = submit_and_await writer (delta_transaction output 3) in
      require_closed (Writer.close_and_await writer);
      let frozen_tail =
        match
          Writer.read_page
            writer
            ~after:first_page.next_cursor
            ~through:first_page.high_watermark
            ~limit:1
            ()
        with
        | Ok page -> page
        | Error error -> fail (Writer.read_error_to_string error)
      in
      check
        bool
        "frozen tail excludes later append"
        true
        (List.equal Event.equal [ second_receipt.value ] frozen_tail.events);
      check bool "frozen tail is complete" false frozen_tail.has_more;
      check
        int
        "frozen tail keeps old watermark"
        (Journal.cursor_seq second_receipt.through)
        (Journal.cursor_seq frozen_tail.high_watermark);
      check_cursor
        "frozen tail keeps exact old watermark scope"
        second_receipt.through
        frozen_tail.high_watermark;
      check
        int
        "frozen tail stops at old watermark"
        (Journal.cursor_seq second_receipt.through)
        (Journal.cursor_seq frozen_tail.next_cursor);
      check_cursor
        "frozen tail stops at exact old watermark"
        second_receipt.through
        frozen_tail.next_cursor;
      let later =
        match Writer.read_page writer ~after:first_page.high_watermark ~limit:1 () with
        | Ok page -> page
        | Error error -> fail (Writer.read_error_to_string error)
      in
      check
        bool
        "new watermark retains later append"
        true
        (List.equal Event.equal [ third.value ] later.events);
      check bool "later page is complete" false later.has_more;
      check
        int
        "later page exposes new watermark"
        (Journal.cursor_seq third.through)
        (Journal.cursor_seq later.high_watermark);
      check_cursor
        "later page exposes exact new watermark"
        third.through
        later.high_watermark;
      check
        int
        "later page reaches new watermark"
        (Journal.cursor_seq third.through)
        (Journal.cursor_seq later.next_cursor);
      check_cursor
        "later page reaches exact new watermark"
        third.through
        later.next_cursor;
      check
        bool
        "first receipt remains exact"
        true
        (Event.equal first_receipt.value (List.hd first_page.events))))
;;

let () =
  run
    "execution lane writer"
    [ ( "durability"
      , [ test_case
            "single command commits and close drains"
            `Quick
            test_single_command_commits_and_close_drains
        ; test_case
            "ready set is one FIFO durable group"
            `Quick
            test_ready_set_is_one_fifo_durable_group
        ; test_case
            "semantic rejection does not poison ready group"
            `Quick
            test_semantic_rejection_does_not_poison_ready_group
        ; test_case
            "concurrent submit and close linearize without loss"
            `Quick
            test_concurrent_submit_and_close_linearize_without_loss
        ; test_case
            "cancelled waiter does not cancel ticket"
            `Quick
            test_cancelled_waiter_does_not_cancel_ticket
        ; test_case
            "supervisor cancellation settles accepted ticket"
            `Quick
            test_supervisor_cancellation_settles_accepted_ticket
        ; test_case
            "initialization failure is scope local"
            `Quick
            test_initialization_failure_is_scope_local
        ; test_case
            "clean reopen continues exact cursor"
            `Quick
            test_clean_reopen_continues_exact_cursor
        ; test_case
            "abort transaction is one durable terminal group"
            `Quick
            test_abort_transaction_is_one_durable_terminal_group
        ; test_case
            "repeated unknown waits for typed external wake"
            `Quick
            test_repeated_unknown_waits_for_typed_external_wake
        ; test_case
            "close terminates unresolved reconciliation"
            `Quick
            test_close_terminates_unresolved_reconciliation
        ; test_case
            "each external wake authorizes one reconciliation"
            `Quick
            test_each_external_wake_authorizes_one_reconciliation
        ; test_case
            "owned supervisor drains same-scope waiter"
            `Quick
            test_owned_supervisor_drains_same_scope_waiter
        ; test_case
            "callback exception preserves durable group truth"
            `Quick
            test_callback_exception_preserves_durable_group_truth
        ; test_case
            "callback exception retains unresolved scope failure"
            `Quick
            test_callback_exception_retains_unresolved_scope_failure
        ; test_case
            "durable success settles group before supervisor cancellation"
            `Quick
            test_durable_success_settles_group_before_supervisor_cancellation
        ; test_case
            "frozen pages remain lossless after close"
            `Quick
            test_frozen_pages_remain_lossless_after_close
        ] )
    ]
;;
