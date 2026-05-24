open Agent_sdk
open Alcotest

let expect_ok label = function
  | Ok value -> value
  | Error err -> fail (Printf.sprintf "%s: %s" label (Error.to_string err))
;;

let with_temp_dir f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-runtime-replay-%d-%06x" (Unix.getpid ()) (Random.int 0xFFFFFF))
  in
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" dir)))
    (fun () -> f dir)
;;

let mk_session ?(artifacts = []) ?(updated_at = 1.0) session_id : Runtime.session =
  { session_id
  ; goal = "runtime replay"
  ; title = None
  ; tag = None
  ; permission_mode = None
  ; phase = Runtime.Running
  ; created_at = updated_at -. 0.5
  ; updated_at
  ; provider = Some "mock"
  ; model = Some "model"
  ; system_prompt = None
  ; max_turns = 10
  ; workdir = None
  ; planned_participants = []
  ; participants = []
  ; artifacts
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let mk_event seq message : Runtime.event =
  { seq
  ; ts = float_of_int seq
  ; kind = Runtime.Turn_recorded { actor = Some "user"; message }
  }
;;

let save_artifact store session_id ~artifact_id ~name content =
  let path =
    Runtime_store.save_artifact_text store session_id ~name ~kind:"json" ~content
    |> expect_ok "save artifact"
  in
  ({ Runtime.artifact_id
   ; name
   ; kind = "json"
   ; mime_type = "application/json"
   ; path = Some path
   ; inline_content = None
   ; size_bytes = String.length content
   ; created_at = 1.0
   }
   : Runtime.artifact)
;;

let save_run ?(artifacts = []) store session_id ~updated_at events =
  Runtime_store.save_session store (mk_session ~artifacts ~updated_at session_id)
  |> expect_ok "save session";
  List.iter
    (fun event ->
       Runtime_store.append_event store session_id event |> expect_ok "append event")
    events
;;

let test_sync_windows_from_selected_runs () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let artifact =
      save_artifact store "run-a" ~artifact_id:"art-run-a" ~name:"report" {|{"ok":true}|}
    in
    save_run
      ~artifacts:[ artifact ]
      store
      "run-a"
      ~updated_at:10.0
      [ mk_event 1 "old-a"; mk_event 2 "new-a" ];
    save_run store "run-b" ~updated_at:20.0 [ mk_event 1 "old-b"; mk_event 3 "new-b" ];
    let set =
      Runtime_replay.sync_windows_from_store
        ~after_seq:1
        store
        [ Runtime_store.Last_n_runs 2 ]
      |> expect_ok "sync windows"
    in
    check int "windows" 2 (List.length set.windows);
    check
      (list string)
      "stream order"
      [ "run-a"; "run-b" ]
      (List.map (fun (window : Runtime_sync.window) -> window.stream_id) set.windows);
    check
      (list int)
      "event counts"
      [ 1; 1 ]
      (List.map
         (fun (window : Runtime_sync.window) -> List.length window.events)
         set.windows);
    let first = List.hd set.windows in
    check int "cursor" 1 first.cursor.after_seq;
    check int "next cursor" 2 first.next_cursor.after_seq;
    check (list string) "artifact refs" [ "art-run-a" ] first.artifact_refs;
    List.iter
      (fun window ->
         match Runtime_sync.validate_window window with
         | Ok () -> ()
         | Error detail -> fail detail)
      set.windows)
;;

let test_sync_windows_json_reports_selector_failures_and_dedupes_runs () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    save_run store "run-a" ~updated_at:10.0 [ mk_event 1 "old-a"; mk_event 2 "new-a" ];
    save_run store "run-b" ~updated_at:20.0 [ mk_event 1 "old-b"; mk_event 2 "new-b" ];
    let json =
      Runtime_replay.sync_windows_json_from_store
        store
        [ Runtime_store.Last_n_runs 1
        ; Runtime_store.Session "run-b"
        ; Runtime_store.Session "missing"
        ]
      |> expect_ok "sync window json"
    in
    let open Yojson.Safe.Util in
    check int "one deduped window" 1 (json |> member "windows" |> to_list |> List.length);
    check int "one failure" 1 (json |> member "failures" |> to_list |> List.length);
    check
      string
      "missing failure"
      "missing"
      (json |> member "failures" |> to_list |> List.hd |> member "session_id" |> to_string))
;;

let () =
  Alcotest.run
    "runtime_replay"
    [ ( "sync_windows"
      , [ test_case
            "selected runs to sync windows"
            `Quick
            test_sync_windows_from_selected_runs
        ; test_case
            "json reports failures and dedupes runs"
            `Quick
            test_sync_windows_json_reports_selector_failures_and_dedupes_runs
        ] )
    ]
;;
