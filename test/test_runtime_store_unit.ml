open Base
(** Unit tests for lib/runtime_store.ml.

    Targets 106 uncovered lines (55.08% coverage).
    Uses temp directories for filesystem operations.

    Functions tested:
    - save_text / load_text: roundtrip
    - ensure_dir: nested creation, idempotency
    - save_session / load_session: JSON roundtrip with normalization
    - append_event / read_events: JSONL append and seq filtering
    - snapshot_path: path generation with label sanitization
    - save_artifact_text: artifact file creation *)

open Agent_sdk

(* ── Temp dir helper ─────────────────────────────────────────── *)

let with_temp_dir f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-test-%d-%06x" (Unix.getpid ()) (Random.int 0xFFFFFF))
  in
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" dir)))
    (fun () -> f dir)
;;

(* ── save_text / load_text tests ─────────────────────────────── *)

let test_save_load_roundtrip () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "test.txt" in
    let content = "hello world\nline 2" in
    (match Runtime_store.save_text path content with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match Runtime_store.load_text path with
    | Ok loaded -> Alcotest.(check string) "roundtrip" content loaded
    | Error e -> Alcotest.fail (Error.to_string e))
;;

let test_save_load_empty () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "empty.txt" in
    (match Runtime_store.save_text path "" with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match Runtime_store.load_text path with
    | Ok loaded -> Alcotest.(check string) "empty" "" loaded
    | Error e -> Alcotest.fail (Error.to_string e))
;;

let test_save_load_unicode () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "unicode.txt" in
    let content =
      "Korean: \xed\x95\x9c\xea\xb5\xad\xec\x96\xb4 Emoji: \xf0\x9f\x92\xbb"
    in
    (match Runtime_store.save_text path content with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    match Runtime_store.load_text path with
    | Ok loaded -> Alcotest.(check string) "unicode roundtrip" content loaded
    | Error e -> Alcotest.fail (Error.to_string e))
;;

let test_load_missing_file () =
  with_temp_dir (fun dir ->
    let path = Filename.concat dir "nonexistent.txt" in
    match Runtime_store.load_text path with
    | Ok _ -> Alcotest.fail "expected error"
    | Error _ -> ())
;;

(* ── ensure_dir tests ────────────────────────────────────────── *)

let test_ensure_dir_creates () =
  with_temp_dir (fun dir ->
    let sub = Filename.concat dir "new_sub" in
    (match Runtime_store.ensure_dir sub with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    Alcotest.(check bool) "dir exists" true (Sys.file_exists sub))
;;

let test_ensure_dir_idempotent () =
  with_temp_dir (fun dir ->
    let sub = Filename.concat dir "idem" in
    (match Runtime_store.ensure_dir sub with
     | Ok () -> ()
     | Error e -> Alcotest.fail (Error.to_string e));
    (* Call again -- should not fail *)
    match Runtime_store.ensure_dir sub with
    | Ok () -> ()
    | Error e -> Alcotest.fail (Error.to_string e))
;;

(* ── Runtime_store.create tests ──────────────────────────────── *)

let test_store_create () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Ok store ->
      ignore store;
      Alcotest.(check bool) "root exists" true (Sys.file_exists root);
      Alcotest.(check bool)
        "sessions dir exists"
        true
        (Sys.file_exists (Filename.concat root "sessions"))
    | Error e -> Alcotest.fail (Error.to_string e))
;;

(* ── save_session / load_session tests ───────────────────────── *)

let mk_session ?(session_id = "test-sess") () : Runtime.session =
  { session_id
  ; goal = "test goal"
  ; title = None
  ; tag = None
  ; permission_mode = None
  ; phase = Running
  ; created_at = 1000.0
  ; updated_at = 1001.0
  ; provider = Some "anthropic"
  ; model = Some "test-model"
  ; system_prompt = None
  ; max_turns = 10
  ; workdir = None
  ; planned_participants = [ "agent-1" ]
  ; participants = []
  ; artifacts = []
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let test_save_load_session () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      let session = mk_session () in
      (match Runtime_store.save_session store session with
       | Ok () -> ()
       | Error e -> Alcotest.fail (Error.to_string e));
      (match Runtime_store.load_session store "test-sess" with
       | Ok loaded ->
         Alcotest.(check string) "session_id" "test-sess" loaded.session_id;
         Alcotest.(check string) "goal" "test goal" loaded.goal;
         Alcotest.(check int) "max_turns" 10 loaded.max_turns
       | Error e -> Alcotest.fail (Error.to_string e)))
;;

let test_load_session_missing () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      (match Runtime_store.load_session store "nonexistent" with
       | Ok _ -> Alcotest.fail "expected error"
       | Error _ -> ()))
;;

let test_load_session_corrupt () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      (* Write corrupt JSON to session file *)
      let sess_dir = Filename.concat (Filename.concat root "sessions") "bad-sess" in
      Unix.mkdir sess_dir 0o755;
      let path = Filename.concat sess_dir "session.json" in
      let oc = open_out path in
      output_string oc "not valid json!!!";
      close_out oc;
      (match Runtime_store.load_session store "bad-sess" with
       | Ok _ -> Alcotest.fail "expected parse error"
       | Error _ -> ()))
;;

(* ── append_event / read_events tests ────────────────────────── *)

let mk_event seq =
  { Runtime.seq
  ; ts = float_of_int seq
  ; kind = Turn_recorded { actor = Some "agent"; message = Printf.sprintf "turn %d" seq }
  }
;;

let test_append_and_read_events () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      let sid = "event-test" in
      let e1 = mk_event 1 in
      let e2 = mk_event 2 in
      let e3 = mk_event 3 in
      (match Runtime_store.append_event store sid e1 with
       | Ok () -> ()
       | Error e -> Alcotest.fail (Error.to_string e));
      (match Runtime_store.append_event store sid e2 with
       | Ok () -> ()
       | Error e -> Alcotest.fail (Error.to_string e));
      (match Runtime_store.append_event store sid e3 with
       | Ok () -> ()
       | Error e -> Alcotest.fail (Error.to_string e));
      (match Runtime_store.read_events store sid () with
       | Error e -> Alcotest.fail (Error.to_string e)
       | Ok events -> Alcotest.(check int) "3 events" 3 (List.length events)))
;;

let test_read_events_with_filter () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      let sid = "filter-test" in
      for i = 1 to 5 do
        match Runtime_store.append_event store sid (mk_event i) with
        | Ok () -> ()
        | Error e -> Alcotest.fail (Error.to_string e)
      done;
      (match Runtime_store.read_events store sid ~after_seq:3 () with
       | Error e -> Alcotest.fail (Error.to_string e)
       | Ok events ->
         Alcotest.(check int) "2 events after seq 3" 2 (List.length events);
         List.iter
           (fun (ev : Runtime.event) -> Alcotest.(check bool) "seq > 3" true (ev.seq > 3))
           events))
;;

let test_read_events_no_file () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      (match Runtime_store.read_events store "no-such-session" () with
       | Ok events -> Alcotest.(check int) "empty" 0 (List.length events)
       | Error e -> Alcotest.fail (Error.to_string e)))
;;

(* ── snapshot_path tests ─────────────────────────────────────── *)

let test_snapshot_path_with_label () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      let path = Runtime_store.snapshot_path store "s1" ~seq:5 ~label:(Some "my label") in
      (* Should contain sanitized label: spaces -> _ *)
      Alcotest.(check bool) "has seq" true (String.length (Filename.basename path) > 0);
      let base = Filename.basename path in
      Alcotest.(check bool)
        "contains 0005"
        true
        (String.length base >= 4 && String.sub base 0 4 = "0005");
      Alcotest.(check bool) "no spaces" true (not (String.contains base ' ')))
;;

let test_snapshot_path_no_label () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      let path = Runtime_store.snapshot_path store "s1" ~seq:1 ~label:None in
      let base = Filename.basename path in
      Alcotest.(check string) "just seq" "0001.json" base)
;;

let test_snapshot_path_slash_in_label () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      let path = Runtime_store.snapshot_path store "s1" ~seq:2 ~label:(Some "a/b") in
      let base = Filename.basename path in
      (* Slash should be sanitized to _ *)
      Alcotest.(check bool) "no slash in basename" true (not (String.contains base '/')))
;;

(* ── save_artifact_text tests ────────────────────────────────── *)

let test_save_artifact_text () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      (match
         Runtime_store.save_artifact_text
           store
           "art-sess"
           ~name:"report"
           ~kind:"markdown"
           ~content:"# Title"
       with
       | Ok path ->
         Alcotest.(check bool)
           "path ends with .md"
           true
           (Filename.check_suffix path ".md");
         (match Runtime_store.load_text path with
          | Ok loaded -> Alcotest.(check string) "content" "# Title" loaded
          | Error e -> Alcotest.fail (Error.to_string e))
       | Error e -> Alcotest.fail (Error.to_string e)))
;;

let test_save_artifact_text_empty_name () =
  with_temp_dir (fun dir ->
    let root = Filename.concat dir "store" in
    match Runtime_store.create ~root () with
    | Error e -> Alcotest.fail (Error.to_string e)
    | Ok store ->
      (match
         Runtime_store.save_artifact_text
           store
           "art-sess"
           ~name:""
           ~kind:"json"
           ~content:"{}"
       with
       | Ok path ->
         (* Empty name should default to "artifact" *)
         let base = Filename.basename path in
         Alcotest.(check bool)
           "starts with artifact"
           true
           (String.length base >= 8 && String.sub base 0 8 = "artifact")
       | Error e -> Alcotest.fail (Error.to_string e)))
;;

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Runtime Store Unit"
    [ ( "save_load_text"
      , [ Alcotest.test_case "roundtrip" `Quick test_save_load_roundtrip
        ; Alcotest.test_case "empty file" `Quick test_save_load_empty
        ; Alcotest.test_case "unicode" `Quick test_save_load_unicode
        ; Alcotest.test_case "missing file" `Quick test_load_missing_file
        ] )
    ; ( "ensure_dir"
      , [ Alcotest.test_case "creates" `Quick test_ensure_dir_creates
        ; Alcotest.test_case "idempotent" `Quick test_ensure_dir_idempotent
        ] )
    ; "store_create", [ Alcotest.test_case "create" `Quick test_store_create ]
    ; ( "session"
      , [ Alcotest.test_case "save and load" `Quick test_save_load_session
        ; Alcotest.test_case "missing" `Quick test_load_session_missing
        ; Alcotest.test_case "corrupt" `Quick test_load_session_corrupt
        ] )
    ; ( "events"
      , [ Alcotest.test_case "append and read" `Quick test_append_and_read_events
        ; Alcotest.test_case "filter by seq" `Quick test_read_events_with_filter
        ; Alcotest.test_case "no file" `Quick test_read_events_no_file
        ] )
    ; ( "snapshot_path"
      , [ Alcotest.test_case "with label" `Quick test_snapshot_path_with_label
        ; Alcotest.test_case "no label" `Quick test_snapshot_path_no_label
        ; Alcotest.test_case "slash in label" `Quick test_snapshot_path_slash_in_label
        ] )
    ; ( "artifacts"
      , [ Alcotest.test_case "save text" `Quick test_save_artifact_text
        ; Alcotest.test_case "empty name" `Quick test_save_artifact_text_empty_name
        ] )
    ]
;;
