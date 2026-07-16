open Alcotest
open Agent_sdk
module Event = Execution_event
module Journal = Execution_journal
module Store = Execution_event_store

exception Cancel_before_append

let require_store = function
  | Ok value -> value
  | Error error -> fail (Store.error_to_string error)
;;

let require_journal = function
  | Ok value -> value
  | Error error -> fail (Journal.error_to_string error)
;;

let create_store ~sw ~dir =
  if not (Eio.Path.is_directory dir) then Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 dir;
  let store, initialization = require_store (Store.create ~sw ~dir ()) in
  (match initialization with
   | Store.Fresh -> ()
   | Store.Recovered_uncommitted_initialization ->
     fail "unexpected uncommitted initialization recovery");
  store
;;

let open_store ~sw ~dir = Store.open_existing ~sw ~dir
let attach_store store = require_store (Store.attach store)

let with_temp_dir env f =
  let native_path = Filename.temp_file "oas-execution-store-" ".dir" in
  Sys.remove native_path;
  let dir = Eio.Path.(Eio.Stdenv.fs env / native_path) in
  Fun.protect ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true dir) (fun () -> f dir)
;;

let make_four_events correlation_id =
  let journal = require_journal (Journal.create ~correlation_id ()) in
  let run, _opened =
    require_journal (Journal.start_run journal ~agent_name:"store-test")
  in
  let turn, _opened =
    require_journal
      (Journal.open_node
         journal
         ~run
         ~parent:(Journal.run_root run)
         ~kind:(Event.Agent_turn { ordinal = 0 }))
  in
  ignore (require_journal (Journal.close_node journal ~node:turn Event.Succeeded));
  ignore (require_journal (Journal.finish_run journal ~run Event.Succeeded));
  Journal.events journal
;;

let check_events message expected actual =
  check bool message true (List.equal Event.equal expected actual)
;;

let require_event = function
  | Ok event -> event
  | Error detail -> fail detail
;;

let rec reverse_failure_data = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (name, value) ->
            if String.equal name "data"
            then (
              match value with
              | `Assoc fields -> name, `Assoc (List.rev fields)
              | _ -> name, reverse_failure_data value)
            else name, reverse_failure_data value)
         fields)
  | `List values -> `List (List.map reverse_failure_data values)
  | value -> value
;;

let rewrite_cursor cursor ~scope_id ~seq =
  let json = Store.cursor_to_yojson cursor in
  match json with
  | `Assoc fields ->
    let fields =
      ("scope_id", `String scope_id)
      :: ("seq", `Int seq)
      :: List.remove_assoc "scope_id" (List.remove_assoc "seq" fields)
    in
    (match Store.cursor_of_yojson (`Assoc fields) with
     | Ok cursor -> cursor
     | Error detail -> fail detail)
  | _ -> fail "cursor encoder did not produce an object"
;;

let test_append_reopen_idempotency_and_paging () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    let expected = ref [] in
    let first_cursor = ref None in
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      expected := events;
      let first, second, remaining =
        match events with
        | first :: second :: remaining -> first, second, remaining
        | _ -> fail "fixture did not produce four events"
      in
      (match Store.append_batch writer ~expected_next_seq:1 [ first ] with
       | Ok Store.Stored -> ()
       | _ -> fail "first batch was not stored");
      (match Store.append_batch writer ~expected_next_seq:2 [ second ] with
       | Ok Store.Stored -> ()
       | _ -> fail "second batch was not stored");
      (match Store.append_batch writer ~expected_next_seq:1 [ first ] with
       | Ok Store.Already_committed -> ()
       | _ -> fail "byte-identical retry was not idempotent");
      let page =
        require_store
          (Store.read_page store ~after:(Store.beginning_cursor store) ~limit:1 ())
      in
      check int "page size" 1 (List.length page.events);
      check int "captured high watermark" 2 (Store.cursor_seq page.high_watermark);
      check int "next cursor" 1 (Store.cursor_seq page.next_cursor);
      check bool "more committed data" true page.has_more;
      check (option int) "earliest retained sequence" (Some 1) page.earliest_available_seq;
      first_cursor := Some page.next_cursor;
      ignore (require_store (Store.append_batch writer ~expected_next_seq:3 remaining));
      let frozen =
        require_store
          (Store.read_page
             store
             ~after:page.next_cursor
             ~through:page.high_watermark
             ~limit:8
             ())
      in
      check int "frozen watermark excludes later appends" 1 (List.length frozen.events);
      check
        int
        "frozen watermark remains stable"
        2
        (Store.cursor_seq frozen.high_watermark);
      check bool "frozen snapshot complete" false frozen.has_more;
      let other = List.hd (make_four_events (Store.correlation_id store)) in
      match Store.append_batch writer ~expected_next_seq:1 [ other ] with
      | Error (Store.Committed_content_conflict { first_seq = 1; last_seq = 1 }) -> ()
      | _ -> fail "different bytes reused a committed sequence");
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      (match recovery with
       | Store.Clean -> ()
       | _ -> fail "clean WAL reported recovery");
      check int "recovered sequence" 4 (Store.last_seq store);
      check_events "full replay" !expected (require_store (Store.load_all store));
      let after = Option.get !first_cursor in
      let page = require_store (Store.read_page store ~after ~limit:8 ()) in
      check int "exclusive second page" 3 (List.length page.events);
      check int "second page cursor" 4 (Store.cursor_seq page.next_cursor);
      check bool "captured page complete" false page.has_more))
;;

let test_idempotent_retry_requires_exact_canonical_bytes () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let journal =
        require_journal (Journal.create ~correlation_id:(Store.correlation_id store) ())
      in
      let run, opened =
        require_journal (Journal.start_run journal ~agent_name:"byte-identity")
      in
      let closed =
        match
          require_journal
            (Journal.abort_run
               journal
               ~run
               (Event.Failed
                  { kind = Event.Internal_failure
                  ; detail = "fixture"
                  ; data = Some (`Assoc [ "first", `Int 1; "second", `Int 2 ])
                  }))
        with
        | [ event ] -> event
        | _ -> fail "single-node failed run did not emit one close event"
      in
      let reordered =
        closed
        |> Event.to_yojson
        |> reverse_failure_data
        |> Event.of_yojson
        |> require_event
      in
      check
        bool
        "semantic event equality ignores object order"
        true
        (Event.equal closed reordered);
      check
        bool
        "canonical bytes retain opaque object order"
        false
        (String.equal (Event.to_json_string closed) (Event.to_json_string reordered));
      ignore
        (require_store
           (Store.append_batch writer ~expected_next_seq:1 [ opened; closed ]));
      match
        Store.append_batch writer ~expected_next_seq:(Event.seq reordered) [ reordered ]
      with
      | Error (Store.Committed_content_conflict { first_seq = 2; last_seq = 2 }) -> ()
      | _ -> fail "semantic-only equality admitted a non-byte-identical retry"))
;;

let test_torn_tail_is_explicitly_truncated () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    let expected = ref [] in
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      expected := events;
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events)));
    let wal = Eio.Path.(dir / "events.v1.wal") in
    Eio.Path.with_open_out ~append:true ~create:`Never wal (fun file ->
      Eio.Flow.copy_string "OASE" file;
      Eio.File.sync file);
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      (match recovery with
       | Store.Recovered
           [ Store.Truncated_uncommitted_tail
               { removed_bytes = 4L; last_committed_seq = 4; _ }
           ] -> ()
       | _ -> fail "incomplete tail was not reported exactly");
      check_events
        "committed prefix survives"
        !expected
        (require_store (Store.load_all store)));
    Eio.Switch.run (fun sw ->
      let _store, recovery = require_store (open_store ~sw ~dir) in
      match recovery with
      | Store.Clean -> ()
      | _ -> fail "repaired WAL was not clean on the next open"))
;;

let test_partial_final_batch_is_rolled_back_to_committed_prefix () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun root ->
    let run_case case_name cutoff_of_sizes =
      let dir = Eio.Path.(root / case_name) in
      Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 dir;
      let prefix_size = ref 0 in
      let prefix_authority = ref "" in
      let full_wal = ref "" in
      let expected_prefix = ref [] in
      Eio.Switch.run (fun sw ->
        let store = create_store ~sw ~dir in
        let writer = attach_store store in
        let events = make_four_events (Store.correlation_id store) in
        let first, rest =
          match events with
          | first :: rest -> first, rest
          | [] -> fail "fixture did not produce events"
        in
        expected_prefix := [ first ];
        ignore (require_store (Store.append_batch writer ~expected_next_seq:1 [ first ]));
        let wal = Eio.Path.(dir / "events.v1.wal") in
        let authority = Eio.Path.(dir / "events.v1.commit") in
        prefix_size := String.length (Eio.Path.load wal);
        prefix_authority := Eio.Path.load authority;
        ignore (require_store (Store.append_batch writer ~expected_next_seq:2 rest));
        full_wal := Eio.Path.load wal);
      let cutoff = cutoff_of_sizes !prefix_size (String.length !full_wal) in
      let truncated = String.sub !full_wal 0 cutoff in
      let wal = Eio.Path.(dir / "events.v1.wal") in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
        Eio.Flow.copy_string truncated file;
        Eio.File.sync file);
      let authority = Eio.Path.(dir / "events.v1.commit") in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) authority (fun file ->
        Eio.Flow.copy_string !prefix_authority file;
        Eio.File.sync file);
      Eio.Switch.run (fun sw ->
        let store, recovery = require_store (open_store ~sw ~dir) in
        (match recovery with
         | Store.Recovered
             [ Store.Truncated_uncommitted_tail
                 { committed_offset; removed_bytes; last_committed_seq = 1 }
             ] ->
           check
             int64
             "rollback begins at committed prefix"
             (Int64.of_int !prefix_size)
             committed_offset;
           check
             int64
             "reported removed partial batch bytes"
             (Int64.of_int (cutoff - !prefix_size))
             removed_bytes
         | _ -> fail "partial final batch was not explicitly rolled back");
        check_events
          "only the committed prefix remains"
          !expected_prefix
          (require_store (Store.load_all store)))
    in
    run_case "mid-batch" (fun prefix full -> prefix + ((full - prefix) / 2));
    run_case "mid-commit" (fun _prefix full -> full - 1))
;;

let test_committed_corruption_is_rejected_without_truncation () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events)));
    let wal = Eio.Path.(dir / "events.v1.wal") in
    let bytes = Bytes.of_string (Eio.Path.load wal) in
    let final_index = Bytes.length bytes - 1 in
    Bytes.set
      bytes
      final_index
      (Char.chr (Char.code (Bytes.get bytes final_index) lxor 1));
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
      Eio.Flow.copy_string (Bytes.unsafe_to_string bytes) file;
      Eio.File.sync file);
    let corrupted_size = (Eio.Path.stat ~follow:true wal).size in
    Eio.Switch.run (fun sw ->
      match open_store ~sw ~dir with
      | Error (Store.Corrupt_store _) -> ()
      | _ -> fail "committed checksum corruption was silently accepted");
    check
      int64
      "corrupt WAL was not truncated"
      (Optint.Int63.to_int64 corrupted_size)
      (Optint.Int63.to_int64 (Eio.Path.stat ~follow:true wal).size))
;;

let test_authoritative_incomplete_frame_is_not_truncated () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    let middle_payload = ref "" in
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      middle_payload := Event.to_json_string (List.nth events 1);
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events)));
    let wal = Eio.Path.(dir / "events.v1.wal") in
    let bytes = Bytes.of_string (Eio.Path.load wal) in
    let payload_offset =
      try
        Str.search_forward (Str.regexp_string !middle_payload) (Bytes.to_string bytes) 0
      with
      | Not_found -> fail "middle event payload was not found in WAL fixture"
    in
    let frame_header_bytes = 4 + 2 + 1 + 1 + 8 + 32 in
    let frame_offset = payload_offset - frame_header_bytes in
    let declared_length = Bytes.length bytes - payload_offset + 1 in
    for index = 0 to 7 do
      let shift = (7 - index) * 8 in
      Bytes.set
        bytes
        (frame_offset + 8 + index)
        (Char.chr ((declared_length lsr shift) land 0xff))
    done;
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
      Eio.Flow.copy_string (Bytes.unsafe_to_string bytes) file;
      Eio.File.sync file);
    let corrupted_size = (Eio.Path.stat ~follow:true wal).size in
    Eio.Switch.run (fun sw ->
      match open_store ~sw ~dir with
      | Error (Store.Corrupt_store _) -> ()
      | _ -> fail "authoritative incomplete frame was treated as an uncommitted tail");
    check
      int64
      "authoritative corruption was not truncated"
      (Optint.Int63.to_int64 corrupted_size)
      (Optint.Int63.to_int64 (Eio.Path.stat ~follow:true wal).size))
;;

let test_foreign_authority_cannot_truncate_wal () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun root ->
    let target = Eio.Path.(root / "target") in
    let foreign = Eio.Path.(root / "foreign") in
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir:target in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events)));
    Eio.Switch.run (fun sw -> ignore (create_store ~sw ~dir:foreign));
    let target_wal = Eio.Path.(target / "events.v1.wal") in
    let target_authority = Eio.Path.(target / "events.v1.commit") in
    let foreign_authority = Eio.Path.(foreign / "events.v1.commit") in
    let original_size = (Eio.Path.stat ~follow:true target_wal).size in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o600) target_authority (fun file ->
      Eio.Flow.copy_string (Eio.Path.load foreign_authority) file;
      Eio.File.sync file);
    Eio.Switch.run (fun sw ->
      match open_store ~sw ~dir:target with
      | Error (Store.Corrupt_store _) -> ()
      | _ -> fail "foreign authority was accepted for the target WAL");
    check
      int64
      "foreign authority did not truncate target WAL"
      (Optint.Int63.to_int64 original_size)
      (Optint.Int63.to_int64 (Eio.Path.stat ~follow:true target_wal).size))
;;

let test_detected_committed_corruption_poisons_writer () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events));
      let first = List.hd events in
      let first_payload = Event.to_json_string first in
      let wal = Eio.Path.(dir / "events.v1.wal") in
      let bytes = Bytes.of_string (Eio.Path.load wal) in
      let payload_offset =
        try
          Str.search_forward (Str.regexp_string first_payload) (Bytes.to_string bytes) 0
        with
        | Not_found -> fail "committed event payload was not found in WAL fixture"
      in
      Bytes.set
        bytes
        payload_offset
        (Char.chr (Char.code (Bytes.get bytes payload_offset) lxor 1));
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
        Eio.Flow.copy_string (Bytes.unsafe_to_string bytes) file;
        Eio.File.sync file);
      (match Store.load_all store with
       | Error (Store.Corrupt_store _) -> ()
       | _ -> fail "same-size committed corruption was not detected");
      (match Store.read_page store ~after:(Store.beginning_cursor store) ~limit:1 () with
       | Error (Store.Store_poisoned _) -> ()
       | _ -> fail "projection read ignored the known poisoned store");
      match Store.append_batch writer ~expected_next_seq:1 [ first ] with
      | Error (Store.Store_poisoned _) -> ()
      | _ -> fail "writer remained usable after committed corruption was detected"))
;;

let test_projection_detects_middle_event_corruption () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events));
      let first = List.hd events in
      let middle = List.nth events 1 in
      let middle_payload = Event.to_json_string middle in
      let wal = Eio.Path.(dir / "events.v1.wal") in
      let bytes = Bytes.of_string (Eio.Path.load wal) in
      let payload_offset =
        try
          Str.search_forward (Str.regexp_string middle_payload) (Bytes.to_string bytes) 0
        with
        | Not_found -> fail "middle event payload was not found in WAL fixture"
      in
      Bytes.set
        bytes
        payload_offset
        (Char.chr (Char.code (Bytes.get bytes payload_offset) lxor 1));
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
        Eio.Flow.copy_string (Bytes.unsafe_to_string bytes) file;
        Eio.File.sync file);
      (match Store.load_all store with
       | Error (Store.Corrupt_store _) -> ()
       | _ -> fail "projection ignored corrupted middle event bytes");
      match Store.append_batch writer ~expected_next_seq:1 [ first ] with
      | Error (Store.Store_poisoned _) -> ()
      | _ -> fail "projection corruption did not fence the writer"))
;;

let test_projection_verifies_middle_event_frame_header () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      ignore (require_store (Store.append_batch writer ~expected_next_seq:1 events));
      let first = List.hd events in
      let middle_payload = Event.to_json_string (List.nth events 1) in
      let wal = Eio.Path.(dir / "events.v1.wal") in
      let bytes = Bytes.of_string (Eio.Path.load wal) in
      let payload_offset =
        try
          Str.search_forward (Str.regexp_string middle_payload) (Bytes.to_string bytes) 0
        with
        | Not_found -> fail "middle event payload was not found in WAL fixture"
      in
      (* The independent v1 wire-format oracle is 4 magic + 2 version + 1 kind
         + 1 reserved + 8 length + 32 SHA-256 bytes. *)
      let event_frame_offset = payload_offset - (4 + 2 + 1 + 1 + 8 + 32) in
      Bytes.set bytes event_frame_offset 'X';
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
        Eio.Flow.copy_string (Bytes.unsafe_to_string bytes) file;
        Eio.File.sync file);
      (match Store.load_all store with
       | Error (Store.Corrupt_store _) -> ()
       | _ -> fail "projection ignored a corrupted middle event frame header");
      match Store.append_batch writer ~expected_next_seq:1 [ first ] with
      | Error (Store.Store_poisoned _) -> ()
      | _ -> fail "event frame header corruption did not fence the writer"))
;;

let test_uncommitted_initialization_is_explicitly_recovered () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 dir;
    let initializing = Eio.Path.(dir / "events.v1.wal.initializing") in
    Eio.Path.with_open_out ~create:(`Exclusive 0o600) initializing (fun file ->
      Eio.Flow.copy_string "incomplete metadata" file;
      Eio.File.sync file);
    Eio.Switch.run (fun sw ->
      match open_store ~sw ~dir with
      | Error Store.Store_initialization_incomplete -> ()
      | _ -> fail "orphan initialization was not reported");
    Eio.Switch.run (fun sw ->
      let store, initialization = require_store (Store.create ~sw ~dir ()) in
      (match initialization with
       | Store.Recovered_uncommitted_initialization -> ()
       | Store.Fresh -> fail "orphan initialization recovery was silent");
      check int "recovered store begins empty" 0 (Store.last_seq store));
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      check int "recovered store reopens" 0 (Store.last_seq store);
      match recovery with
      | Store.Clean -> ()
      | _ -> fail "recovered initialization did not produce a clean WAL"))
;;

let test_initial_commit_authority_is_rebuilt_explicitly () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw -> ignore (create_store ~sw ~dir));
    let authority = Eio.Path.(dir / "events.v1.commit") in
    let initializing = Eio.Path.(dir / "events.v1.commit.initializing") in
    Eio.Path.rename authority initializing;
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      check int "rebuilt authority keeps the empty store" 0 (Store.last_seq store);
      match recovery with
      | Store.Recovered
          [ Store.Discarded_uncommitted_authority; Store.Rebuilt_initial_authority ] -> ()
      | _ -> fail "initial authority recovery was not reported exactly");
    Eio.Switch.run (fun sw ->
      let _store, recovery = require_store (open_store ~sw ~dir) in
      match recovery with
      | Store.Clean -> ()
      | _ -> fail "rebuilt authority was not clean on the next open"))
;;

let test_failed_create_releases_writer_immediately () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 dir;
    let wal = Eio.Path.(dir / "events.v1.wal") in
    Eio.Path.save ~create:(`Exclusive 0o600) wal "preexisting";
    Eio.Switch.run (fun sw ->
      (match Store.create ~sw ~dir () with
       | Error Store.Store_already_exists -> ()
       | _ -> fail "preexisting WAL was not rejected");
      Eio.Path.unlink wal;
      match Store.create ~sw ~dir () with
      | Ok (_store, Store.Fresh) -> ()
      | Error Store.Writer_already_active ->
        fail "failed create leaked its writer claim until switch release"
      | _ -> fail "create did not recover after its prior explicit failure"))
;;

let test_create_unknown_outcome_reconciles_through_open () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 dir;
    let blocker = Eio.Path.(dir / "events.v1.commit.initializing") in
    Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 blocker;
    Eio.Switch.run (fun sw ->
      match Store.create ~sw ~dir () with
      | Error (Store.Commit_outcome_unknown _) -> ()
      | _ -> fail "post-WAL create failure was not classified as outcome unknown");
    Eio.Path.rmtree ~missing_ok:false blocker;
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      check int "reconciled unknown create is empty" 0 (Store.last_seq store);
      match recovery with
      | Store.Recovered [ Store.Rebuilt_initial_authority ] -> ()
      | _ -> fail "unknown create was not reconciled explicitly by open"))
;;

let test_append_failure_rolls_back_before_authority () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      let blocker = Eio.Path.(dir / "events.v1.commit.initializing") in
      Eio.Path.mkdirs ~exists_ok:false ~perm:0o700 blocker;
      (match Store.append_batch writer ~expected_next_seq:1 events with
       | Error (Store.Io_failure _) -> ()
       | _ -> fail "pre-authority append failure did not surface its I/O error");
      check int "failed append did not advance store" 0 (Store.last_seq store);
      Eio.Path.rmtree ~missing_ok:false blocker;
      (match Store.append_batch writer ~expected_next_seq:1 events with
       | Ok Store.Stored -> ()
       | _ -> fail "append could not retry after proven rollback");
      check int "retried append committed" 4 (Store.last_seq store));
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      check int "retried append reopens" 4 (Store.last_seq store);
      match recovery with
      | Store.Clean -> ()
      | _ -> fail "proven append rollback left recovery residue"))
;;

let test_cancelled_append_does_not_mutate_store () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let writer = attach_store store in
      let events = make_four_events (Store.correlation_id store) in
      let wal = Eio.Path.(dir / "events.v1.wal") in
      let authority = Eio.Path.(dir / "events.v1.commit") in
      let wal_before = Eio.Path.load wal in
      let authority_before = Eio.Path.load authority in
      let cancelled =
        match
          Eio.Cancel.sub (fun cancel_context ->
            Eio.Cancel.cancel cancel_context Cancel_before_append;
            ignore (Store.append_batch writer ~expected_next_seq:1 events))
        with
        | () -> false
        | exception Eio.Cancel.Cancelled _ -> true
      in
      check bool "append observed pre-existing cancellation" true cancelled;
      check int "cancelled append did not publish" 0 (Store.last_seq store);
      check string "cancelled append did not mutate WAL" wal_before (Eio.Path.load wal);
      check
        string
        "cancelled append did not replace authority"
        authority_before
        (Eio.Path.load authority);
      match Store.append_batch writer ~expected_next_seq:1 events with
      | Ok Store.Stored -> ()
      | _ -> fail "store did not accept append after cancelled caller detached"))
;;

let test_journal_persists_before_publish_and_replays () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    let run_handle = ref None in
    let opened_event = ref None in
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let journal = require_journal (Journal.create ~store ()) in
      let run, opened =
        require_journal (Journal.start_run journal ~agent_name:"durable-journal")
      in
      run_handle := Some run;
      opened_event := Some opened;
      check int "store committed before call returns" 1 (Store.last_seq store);
      check int "journal published one event" 1 (Journal.length journal));
    Eio.Switch.run (fun sw ->
      let store, recovery = require_store (open_store ~sw ~dir) in
      (match recovery with
       | Store.Clean -> ()
       | _ -> fail "unexpected recovery");
      let journal = require_journal (Journal.create ~store ()) in
      check int "journal reducer replayed" 1 (Journal.length journal);
      check_events
        "replayed exact event"
        [ Option.get !opened_event ]
        (Journal.events journal);
      ignore
        (require_journal
           (Journal.finish_run journal ~run:(Option.get !run_handle) Event.Succeeded));
      check int "finish durably committed" 2 (Store.last_seq store));
    Eio.Switch.run (fun sw ->
      let store, _recovery = require_store (open_store ~sw ~dir) in
      let journal = require_journal (Journal.create ~store ()) in
      check int "finished scope fully replayed" 2 (Journal.length journal);
      match Journal.start_run journal ~agent_name:"second-top-level" with
      | Error (Journal.Invariant_violation Journal.Top_level_run_already_exists) -> ()
      | _ -> fail "recovered scope admitted a second top-level run"))
;;

let test_external_wal_mutation_fails_without_journal_publish () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      let journal = require_journal (Journal.create ~store ()) in
      let run, _opened =
        require_journal (Journal.start_run journal ~agent_name:"mutation-fence")
      in
      let before = Journal.events journal in
      let wal = Eio.Path.(dir / "events.v1.wal") in
      Eio.Path.with_open_out ~create:(`Or_truncate 0o600) wal (fun file ->
        Eio.File.sync file);
      (match Journal.finish_run journal ~run Event.Succeeded with
       | Error (Journal.Persistence_failure (Store.Corrupt_store _)) -> ()
       | _ -> fail "physical WAL drift did not surface as persistence failure");
      check_events "failed persistence did not publish" before (Journal.events journal);
      check int "failed persistence did not advance reducer" 1 (Journal.length journal);
      match Journal.finish_run journal ~run Event.Succeeded with
      | Error (Journal.Persistence_failure (Store.Store_poisoned _)) -> ()
      | _ -> fail "detected physical WAL drift did not fence later journal writes"))
;;

let test_cursor_scope_and_writer_exclusivity () =
  Eio_main.run
  @@ fun env ->
  with_temp_dir env (fun dir ->
    Eio.Switch.run (fun sw ->
      let store = create_store ~sw ~dir in
      (match open_store ~sw ~dir with
       | Error Store.Writer_already_active -> ()
       | _ -> fail "a second in-process writer acquired the same scope");
      ignore (attach_store store);
      (match Store.attach store with
       | Error Store.Store_already_attached -> ()
       | _ -> fail "store minted more than one semantic writer capability");
      let cursor = Store.beginning_cursor store in
      let foreign = rewrite_cursor cursor ~scope_id:"execution-scope-foreign" ~seq:0 in
      (match Store.read_page store ~after:foreign ~limit:1 () with
       | Error Store.Cursor_scope_mismatch -> ()
       | _ -> fail "foreign cursor scope was accepted");
      let ahead =
        rewrite_cursor
          cursor
          ~scope_id:(Store.Scope_id.to_string (Store.scope_id store))
          ~seq:1
      in
      (match Store.read_page store ~after:ahead ~limit:1 () with
       | Error (Store.Cursor_ahead { after_seq = 1; high_watermark = 0 }) -> ()
       | _ -> fail "cursor ahead of the store was accepted");
      match Store.read_page store ~after:cursor ~limit:0 () with
      | Error (Store.Invalid_argument _) -> ()
      | _ -> fail "non-positive page size was accepted"))
;;

let () =
  run
    "Execution event store"
    [ ( "durability"
      , [ test_case
            "append, reopen, idempotency, and paging"
            `Quick
            test_append_reopen_idempotency_and_paging
        ; test_case
            "idempotent retry requires exact canonical bytes"
            `Quick
            test_idempotent_retry_requires_exact_canonical_bytes
        ; test_case
            "torn tail is explicitly truncated"
            `Quick
            test_torn_tail_is_explicitly_truncated
        ; test_case
            "partial final batch rolls back to committed prefix"
            `Quick
            test_partial_final_batch_is_rolled_back_to_committed_prefix
        ; test_case
            "committed corruption is rejected"
            `Quick
            test_committed_corruption_is_rejected_without_truncation
        ; test_case
            "authoritative incomplete frame is not truncated"
            `Quick
            test_authoritative_incomplete_frame_is_not_truncated
        ; test_case
            "foreign authority cannot truncate WAL"
            `Quick
            test_foreign_authority_cannot_truncate_wal
        ; test_case
            "detected committed corruption poisons writer"
            `Quick
            test_detected_committed_corruption_poisons_writer
        ; test_case
            "projection detects middle event corruption"
            `Quick
            test_projection_detects_middle_event_corruption
        ; test_case
            "projection verifies middle event frame header"
            `Quick
            test_projection_verifies_middle_event_frame_header
        ; test_case
            "uncommitted initialization is explicitly recovered"
            `Quick
            test_uncommitted_initialization_is_explicitly_recovered
        ; test_case
            "initial commit authority is rebuilt explicitly"
            `Quick
            test_initial_commit_authority_is_rebuilt_explicitly
        ; test_case
            "failed create releases writer immediately"
            `Quick
            test_failed_create_releases_writer_immediately
        ; test_case
            "create unknown outcome reconciles through open"
            `Quick
            test_create_unknown_outcome_reconciles_through_open
        ; test_case
            "append failure rolls back before authority"
            `Quick
            test_append_failure_rolls_back_before_authority
        ; test_case
            "cancelled append does not mutate store"
            `Quick
            test_cancelled_append_does_not_mutate_store
        ; test_case
            "journal persists before publish and replays"
            `Quick
            test_journal_persists_before_publish_and_replays
        ; test_case
            "external WAL mutation fails without publish"
            `Quick
            test_external_wal_mutation_fails_without_journal_publish
        ; test_case
            "cursor scope and writer exclusivity"
            `Quick
            test_cursor_scope_and_writer_exclusivity
        ] )
    ]
;;
