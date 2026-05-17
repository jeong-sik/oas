(** Tests for Memory_file_backend — file-backed long-term memory. *)

open Agent_sdk

(* ── Helpers ─────────────────────────────────────────────────── *)

let with_tmp_dir env f =
  let fs = Eio.Stdenv.fs env in
  let tmp_dir = Eio.Path.(fs / Printf.sprintf "/tmp/oas_test_mem_%d" (Unix.getpid ())) in
  (try Eio.Path.mkdirs ~exists_ok:true ~perm:0o755 tmp_dir with
   | _ -> ());
  Fun.protect
    ~finally:(fun () ->
      try
        let entries = Eio.Path.read_dir tmp_dir in
        List.iter
          (fun name ->
             try Eio.Path.unlink Eio.Path.(tmp_dir / name) with
             | _ -> ())
          entries;
        Eio.Path.rmdir tmp_dir
      with
      | _ -> ())
    (fun () -> f tmp_dir)
;;

(* ── Create ──────────────────────────────────────────────────── *)

let test_create () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Ok t -> Alcotest.(check int) "empty on create" 0 (Memory_file_backend.entry_count t)
  | Error e -> Alcotest.failf "create failed: %s" (Error.to_string e)
;;

(* ── Round-trip ──────────────────────────────────────────────── *)

let test_persist_retrieve () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    (match backend.persist ~key:"k1" (`String "hello") with
     | Ok () -> ()
     | Error reason -> Alcotest.failf "persist: %s" reason);
    let v = backend.retrieve ~key:"k1" in
    (match v with
     | Some (`String "hello") -> ()
     | Some j -> Alcotest.failf "wrong value: %s" (Yojson.Safe.to_string j)
     | None -> Alcotest.fail "key not found");
    Alcotest.(check int) "1 entry" 1 (Memory_file_backend.entry_count t)
;;

let test_retrieve_result_distinguishes_missing_and_corrupt () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    (match Memory_file_backend.retrieve_result t ~key:"missing" with
     | Error Memory_file_backend.Missing_key -> ()
     | Ok json -> Alcotest.failf "expected missing, got %s" (Yojson.Safe.to_string json)
     | Error err ->
       Alcotest.failf
         "expected Missing_key, got %s"
         (Memory_file_backend.retrieve_error_to_string err));
    Eio.Path.save ~create:(`Or_truncate 0o644) Eio.Path.(dir / "626164.json") "{bad";
    (match Memory_file_backend.retrieve_result t ~key:"bad" with
     | Error (Memory_file_backend.Corrupt_json _) -> ()
     | Ok json -> Alcotest.failf "expected corrupt, got %s" (Yojson.Safe.to_string json)
     | Error err ->
       Alcotest.failf
         "expected Corrupt_json, got %s"
         (Memory_file_backend.retrieve_error_to_string err));
    let backend = Memory_file_backend.to_backend t in
    Alcotest.(check bool)
      "legacy backend still maps corrupt to None"
      true
      (backend.retrieve ~key:"bad" = None);
    (match Memory_file_backend.to_retrieve_result t ~key:"bad" with
     | Error (Memory.Backend_error reason) ->
       Alcotest.(check bool)
         "memory error preserves corrupt_json"
         true
         (String.starts_with ~prefix:"corrupt_json:" reason)
     | Ok json ->
       Alcotest.failf "expected memory backend error, got %s" (Yojson.Safe.to_string json)
     | Error err ->
       Alcotest.failf
         "expected Backend_error, got %s"
         (Memory.retrieve_error_to_string err))
;;

let test_persist_overwrite () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    ignore (backend.persist ~key:"k" (`Int 1));
    ignore (backend.persist ~key:"k" (`Int 2));
    (match backend.retrieve ~key:"k" with
     | Some (`Int 2) -> ()
     | _ -> Alcotest.fail "overwrite failed");
    Alcotest.(check int) "still 1 entry" 1 (Memory_file_backend.entry_count t)
;;

(* ── Remove ──────────────────────────────────────────────────── *)

let test_remove () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    ignore (backend.persist ~key:"k" (`Bool true));
    (match backend.remove ~key:"k" with
     | Ok () -> ()
     | Error reason -> Alcotest.failf "remove: %s" reason);
    Alcotest.(check bool) "gone" true (backend.retrieve ~key:"k" = None);
    Alcotest.(check int) "0 entries" 0 (Memory_file_backend.entry_count t)
;;

let test_remove_nonexistent () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    (match backend.remove ~key:"nope" with
     | Ok () -> ()
     | Error reason -> Alcotest.failf "remove nonexistent: %s" reason)
;;

(* ── Batch persist ───────────────────────────────────────────── *)

let test_batch_persist () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    let pairs = List.init 5 (fun i -> Printf.sprintf "key_%d" i, `Int i) in
    (match backend.batch_persist pairs with
     | Ok () -> ()
     | Error reason -> Alcotest.failf "batch: %s" reason);
    Alcotest.(check int) "5 entries" 5 (Memory_file_backend.entry_count t);
    (* Verify each *)
    List.iter
      (fun (k, v) ->
         match backend.retrieve ~key:k with
         | Some retrieved ->
           Alcotest.(check string)
             k
             (Yojson.Safe.to_string v)
             (Yojson.Safe.to_string retrieved)
         | None -> Alcotest.failf "missing: %s" k)
      pairs
;;

(* ── Query by prefix ─────────────────────────────────────────── *)

let test_query_prefix () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    ignore (backend.persist ~key:"user:alice" (`String "a"));
    ignore (backend.persist ~key:"user:bob" (`String "b"));
    ignore (backend.persist ~key:"system:config" (`String "c"));
    let results = backend.query ~prefix:"user:" ~limit:10 in
    Alcotest.(check int) "2 user: results" 2 (List.length results);
    let keys = List.map fst results in
    Alcotest.(check bool) "alice" true (List.mem "user:alice" keys);
    Alcotest.(check bool) "bob" true (List.mem "user:bob" keys)
;;

let test_query_limit () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    for i = 0 to 9 do
      ignore (backend.persist ~key:(Printf.sprintf "item:%02d" i) (`Int i))
    done;
    let results = backend.query ~prefix:"item:" ~limit:3 in
    Alcotest.(check int) "limited to 3" 3 (List.length results)
;;

let test_query_empty_prefix () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    ignore (backend.persist ~key:"a" (`Int 1));
    ignore (backend.persist ~key:"b" (`Int 2));
    let results = backend.query ~prefix:"" ~limit:100 in
    Alcotest.(check int) "all 2" 2 (List.length results)
;;

(* ── Clear ───────────────────────────────────────────────────── *)

let test_clear () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    ignore (backend.persist ~key:"a" (`Int 1));
    ignore (backend.persist ~key:"b" (`Int 2));
    (match Memory_file_backend.clear t with
     | Ok () -> ()
     | Error e -> Alcotest.failf "clear: %s" (Error.to_string e));
    Alcotest.(check int) "0 after clear" 0 (Memory_file_backend.entry_count t)
;;

(* ── Keys ────────────────────────────────────────────────────── *)

let test_keys_sorted () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    ignore (backend.persist ~key:"c" `Null);
    ignore (backend.persist ~key:"a" `Null);
    ignore (backend.persist ~key:"b" `Null);
    let ks = Memory_file_backend.keys t in
    Alcotest.(check (list string)) "sorted" [ "a"; "b"; "c" ] ks
;;

(* ── Special characters in keys ──────────────────────────────── *)

let test_special_chars_key () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok t ->
    let backend = Memory_file_backend.to_backend t in
    let key = "user/path:with spaces&special=chars" in
    ignore (backend.persist ~key (`String "ok"));
    (match backend.retrieve ~key with
     | Some (`String "ok") -> ()
     | _ -> Alcotest.fail "special chars key round-trip failed")
;;

(* ── Memory.t integration ────────────────────────────────────── *)

let test_memory_integration () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok file_store ->
    let backend = Memory_file_backend.to_backend file_store in
    let mem = Memory.create ~long_term:backend () in
    (* Store in long-term via Memory API *)
    (match Memory.store mem ~tier:Long_term "session_data" (`String "important") with
     | Ok () -> ()
     | Error reason -> Alcotest.failf "memory store: %s" reason);
    (* Recall via Memory API *)
    (match Memory.recall mem ~tier:Long_term "session_data" with
     | Some (`String "important") -> ()
     | _ -> Alcotest.fail "memory recall failed");
    (* Verify it's on disk *)
    (match backend.retrieve ~key:"session_data" with
     | Some (`String "important") -> ()
     | _ -> Alcotest.fail "not persisted to disk")
;;

let test_memory_episodic_integration () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok file_store ->
    let backend = Memory_file_backend.to_episodic_backend file_store in
    let writer = Memory.create ~episodic:backend () in
    Memory.store_episode
      writer
      { id = "ep1"
      ; timestamp = 100.0
      ; participants = [ "keeper" ]
      ; action = "fixed issue"
      ; outcome = Success "merged"
      ; salience = 0.8
      ; metadata = []
      };
    let reader = Memory.create ~episodic:backend () in
    (match Memory.recall_episode reader "ep1" with
     | Some ep -> Alcotest.(check string) "action" "fixed issue" ep.action
     | None -> Alcotest.fail "episode not persisted");
    Alcotest.(check int) "stored file" 1 (Memory_file_backend.entry_count file_store)
;;

let test_memory_procedural_integration () =
  Eio_main.run
  @@ fun env ->
  with_tmp_dir env
  @@ fun dir ->
  match Memory_file_backend.create dir with
  | Error e -> Alcotest.failf "create: %s" (Error.to_string e)
  | Ok file_store ->
    let backend = Memory_file_backend.to_procedural_backend file_store in
    let writer = Memory.create ~procedural:backend () in
    Memory.store_procedure
      writer
      { id = "pr1"
      ; pattern = "deploy"
      ; action = "rollback"
      ; success_count = 1
      ; failure_count = 1
      ; confidence = 0.5
      ; last_used = 100.0
      ; metadata = []
      };
    let updater = Memory.create ~procedural:backend () in
    Memory.record_success updater "pr1";
    let reader = Memory.create ~procedural:backend () in
    (match Memory.best_procedure reader ~pattern:"deploy" with
     | Some proc ->
       Alcotest.(check int) "success persisted" 2 proc.success_count;
       Alcotest.(check (float 0.01)) "confidence" 0.667 proc.confidence
     | None -> Alcotest.fail "procedure not persisted");
    Alcotest.(check int) "stored file" 1 (Memory_file_backend.entry_count file_store)
;;

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "memory_file_backend"
    [ "create", [ Alcotest.test_case "create" `Quick test_create ]
    ; ( "persist_retrieve"
      , [ Alcotest.test_case "round_trip" `Quick test_persist_retrieve
        ; Alcotest.test_case
            "typed retrieve errors"
            `Quick
            test_retrieve_result_distinguishes_missing_and_corrupt
        ; Alcotest.test_case "overwrite" `Quick test_persist_overwrite
        ] )
    ; ( "remove"
      , [ Alcotest.test_case "remove" `Quick test_remove
        ; Alcotest.test_case "remove_nonexistent" `Quick test_remove_nonexistent
        ] )
    ; "batch", [ Alcotest.test_case "batch_persist" `Quick test_batch_persist ]
    ; ( "query"
      , [ Alcotest.test_case "prefix" `Quick test_query_prefix
        ; Alcotest.test_case "limit" `Quick test_query_limit
        ; Alcotest.test_case "empty_prefix" `Quick test_query_empty_prefix
        ] )
    ; "clear", [ Alcotest.test_case "clear" `Quick test_clear ]
    ; "keys", [ Alcotest.test_case "sorted" `Quick test_keys_sorted ]
    ; "special", [ Alcotest.test_case "special_chars" `Quick test_special_chars_key ]
    ; ( "integration"
      , [ Alcotest.test_case "memory_t" `Quick test_memory_integration
        ; Alcotest.test_case "episodic_memory_t" `Quick test_memory_episodic_integration
        ; Alcotest.test_case
            "procedural_memory_t"
            `Quick
            test_memory_procedural_integration
        ] )
    ]
;;
