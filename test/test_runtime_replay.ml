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

let mk_checkpoint_event seq ?label path : Runtime.event =
  { seq; ts = float_of_int seq; kind = Runtime.Checkpoint_saved { label; path } }
;;

let mk_message text : Types.message =
  { role = Types.User
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let mk_checkpoint ?(messages = []) ?(created_at = 1.0) ?(turn_count = 0) session_id
  : Checkpoint.t
  =
  { version = Checkpoint.checkpoint_version
  ; session_id
  ; agent_name = "runtime-replay-agent"
  ; model = "claude-sonnet-4-6"
  ; system_prompt = Some "replay"
  ; messages
  ; usage = Types.empty_usage
  ; turn_count
  ; created_at
  ; tools = []
  ; tool_choice = None
  ; disable_parallel_tool_use = false
  ; temperature = None
  ; top_p = None
  ; top_k = None
  ; min_p = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; response_format = Types.Off
  ; thinking_budget = None
  ; cache_system_prompt = false
  ; context = Context.create_sync ()
  ; mcp_sessions = []
  ; working_context = None
  }
;;

let save_checkpoint_file root name checkpoint =
  let path = Filename.concat root name in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc (Checkpoint.to_string checkpoint));
  path
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

let test_checkpoint_delta_projection_from_selected_runs () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let base =
      mk_checkpoint
        ~created_at:10.0
        ~turn_count:1
        ~messages:[ mk_message "base" ]
        "checkpoint-run"
    in
    let target =
      mk_checkpoint
        ~created_at:20.0
        ~turn_count:2
        ~messages:[ mk_message "base"; mk_message "target" ]
        "checkpoint-run"
    in
    let base_path = save_checkpoint_file root "base-checkpoint.json" base in
    let target_path = save_checkpoint_file root "target-checkpoint.json" target in
    save_run
      store
      "run-a"
      ~updated_at:10.0
      [ mk_checkpoint_event 1 ~label:"base" base_path ];
    save_run
      store
      "run-b"
      ~updated_at:20.0
      [ mk_checkpoint_event 1 ~label:"target" target_path ];
    let projection =
      Runtime_replay.checkpoint_delta_projection_from_store
        store
        [ Runtime_store.Last_n_runs 2 ]
      |> expect_ok "checkpoint projection"
    in
    check int "entries" 2 (List.length projection.entries);
    check int "failures" 0 (List.length projection.failures);
    match projection.entries with
    | [ Runtime_replay.Full_checkpoint { checkpoint; checkpoint_ref = base_ref }
      ; Runtime_replay.Delta_checkpoint
          { base = delta_base; target = delta_target; delta }
      ] ->
      check string "base path" base_path base_ref.path;
      check string "delta base path" base_path delta_base.path;
      check string "delta target path" target_path delta_target.path;
      let rebuilt = Checkpoint.apply_delta checkpoint delta |> expect_ok "apply delta" in
      check
        string
        "rebuilt target"
        (Yojson.Safe.to_string (Checkpoint.to_json target))
        (Yojson.Safe.to_string (Checkpoint.to_json rebuilt))
    | _ -> fail "expected full checkpoint followed by delta checkpoint")
;;

let test_checkpoint_delta_projection_reports_corrupt_checkpoint () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let checkpoint = mk_checkpoint ~messages:[ mk_message "valid" ] "checkpoint-run" in
    let valid_path = save_checkpoint_file root "valid-checkpoint.json" checkpoint in
    let corrupt_path = Filename.concat root "corrupt-checkpoint.json" in
    let oc = open_out corrupt_path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc "not checkpoint json");
    save_run
      store
      "run-a"
      ~updated_at:10.0
      [ mk_checkpoint_event 1 valid_path; mk_checkpoint_event 2 corrupt_path ];
    let projection =
      Runtime_replay.checkpoint_delta_projection_from_store
        store
        [ Runtime_store.Session "run-a" ]
      |> expect_ok "checkpoint projection"
    in
    check int "valid entry" 1 (List.length projection.entries);
    check int "one failure" 1 (List.length projection.failures);
    match projection.failures with
    | [ failure ] -> check string "corrupt path" corrupt_path failure.path
    | _ -> fail "expected one corrupt checkpoint failure")
;;

let test_checkpoint_delta_projection_dedupes_overlapping_checkpoint_paths () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let checkpoint = mk_checkpoint ~messages:[ mk_message "same" ] "checkpoint-run" in
    let path = save_checkpoint_file root "same-checkpoint.json" checkpoint in
    save_run
      store
      "run-a"
      ~updated_at:10.0
      [ mk_checkpoint_event 1 path; mk_checkpoint_event 2 path ];
    let json =
      Runtime_replay.checkpoint_delta_projection_json_from_store
        store
        [ Runtime_store.Last_n_runs 1; Runtime_store.Session "run-a" ]
      |> expect_ok "checkpoint projection json"
    in
    let open Yojson.Safe.Util in
    check
      int
      "one projected checkpoint"
      1
      (json |> member "entries" |> to_list |> List.length);
    check
      string
      "projection kind"
      "checkpoint_delta_v1"
      (json |> member "projection" |> to_string))
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
    ; ( "checkpoint_delta_projection"
      , [ test_case
            "selected checkpoints project full plus delta"
            `Quick
            test_checkpoint_delta_projection_from_selected_runs
        ; test_case
            "corrupt checkpoint is a partial failure"
            `Quick
            test_checkpoint_delta_projection_reports_corrupt_checkpoint
        ; test_case
            "overlapping checkpoint paths are deduped"
            `Quick
            test_checkpoint_delta_projection_dedupes_overlapping_checkpoint_paths
        ] )
    ]
;;
