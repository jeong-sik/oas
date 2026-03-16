(** Tests for A2a_task_store — file-backed A2A task persistence. *)

open Alcotest
open Agent_sdk

let make_task ?(id = "task_001") ?(state = A2a_task.Submitted) () : A2a_task.task =
  let now = Unix.gettimeofday () in
  { id; state;
    messages = [{ role = TaskUser; parts = [Text_part "hello"]; metadata = [] }];
    artifacts = [];
    metadata = [];
    created_at = now;
    updated_at = now }

let with_temp_store f =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Printf.sprintf "/tmp/oas_test_task_store_%d_%d"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.0) mod 100_000) in
  let path = Eio.Path.(fs / dir) in
  match A2a_task_store.create path with
  | Error e -> fail (Error.to_string e)
  | Ok store ->
    Fun.protect
      ~finally:(fun () ->
        (* cleanup *)
        (try
           let entries = Eio.Path.read_dir path in
           List.iter (fun name ->
             try Eio.Path.unlink Eio.Path.(path / name)
             with _ -> ()
           ) entries;
           Eio.Path.unlink path
         with _ -> ()))
      (fun () -> f store)

(* ── Store and retrieve ─────────────────────────────────── *)

let test_store_and_get () =
  with_temp_store @@ fun store ->
  let task = make_task () in
  (match A2a_task_store.store_task store task with
   | Ok () -> ()
   | Error e -> fail (Error.to_string e));
  match A2a_task_store.get_task store task.id with
  | Some t -> check string "id matches" task.id t.id
  | None -> fail "task not found in cache"

let test_store_overwrites () =
  with_temp_store @@ fun store ->
  let task = make_task () in
  (match A2a_task_store.store_task store task with
   | Ok () -> ()
   | Error e -> fail (Error.to_string e));
  let updated = { task with state = Working; updated_at = Unix.gettimeofday () } in
  (match A2a_task_store.store_task store updated with
   | Ok () -> ()
   | Error e -> fail (Error.to_string e));
  match A2a_task_store.get_task store task.id with
  | Some t ->
    check string "state updated" "working" (A2a_task.task_state_to_string t.state)
  | None -> fail "task not found"

(* ── List tasks ─────────────────────────────────────────── *)

let test_list_tasks () =
  with_temp_store @@ fun store ->
  let t1 = make_task ~id:"task_a" () in
  let t2 = make_task ~id:"task_b" () in
  (match A2a_task_store.store_task store t1 with Ok () -> () | Error e -> fail (Error.to_string e));
  (match A2a_task_store.store_task store t2 with Ok () -> () | Error e -> fail (Error.to_string e));
  let tasks = A2a_task_store.list_tasks store in
  check int "count" 2 (List.length tasks)

(* ── Delete ─────────────────────────────────────────────── *)

let test_delete () =
  with_temp_store @@ fun store ->
  let task = make_task () in
  (match A2a_task_store.store_task store task with Ok () -> () | Error e -> fail (Error.to_string e));
  (match A2a_task_store.delete_task store task.id with
   | Ok () -> ()
   | Error e -> fail (Error.to_string e));
  check bool "deleted" true (Option.is_none (A2a_task_store.get_task store task.id))

let test_delete_nonexistent () =
  with_temp_store @@ fun store ->
  match A2a_task_store.delete_task store "no_such_task" with
  | Error _ -> ()  (* expected *)
  | Ok () -> fail "should have failed"

(* ── Reload ─────────────────────────────────────────────── *)

let test_reload_restores () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Printf.sprintf "/tmp/oas_test_reload_%d_%d"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.0) mod 100_000) in
  let path = Eio.Path.(fs / dir) in
  (match A2a_task_store.create path with
   | Error e -> fail (Error.to_string e)
   | Ok store ->
     let task = make_task ~id:"persist_me" () in
     (match A2a_task_store.store_task store task with
      | Ok () -> ()
      | Error e -> fail (Error.to_string e));

     (* Create a new store from the same directory — simulates restart *)
     match A2a_task_store.create path with
     | Error e -> fail (Error.to_string e)
     | Ok store2 ->
       (* Cache is empty in new store *)
       check bool "empty before reload" true
         (Option.is_none (A2a_task_store.get_task store2 "persist_me"));
       (* Reload from disk *)
       (match A2a_task_store.reload store2 with
        | Ok () -> ()
        | Error e -> fail (Error.to_string e));
       match A2a_task_store.get_task store2 "persist_me" with
       | Some t -> check string "id survives reload" "persist_me" t.id
       | None -> fail "task not found after reload");
  (* cleanup *)
  (try
     let entries = Sys.readdir dir in
     Array.iter (fun name ->
       try Sys.remove (Filename.concat dir name) with _ -> ()
     ) entries;
     Sys.rmdir dir
   with _ -> ())

(* ── GC ─────────────────────────────────────────────────── *)

let test_gc_removes_old_terminal () =
  with_temp_store @@ fun store ->
  let old_task = { (make_task ~id:"old_done" ~state:Completed ()) with
    updated_at = Unix.gettimeofday () -. 200.0 } in
  let recent_task = make_task ~id:"recent" () in
  (match A2a_task_store.store_task store old_task with Ok () -> () | Error e -> fail (Error.to_string e));
  (match A2a_task_store.store_task store recent_task with Ok () -> () | Error e -> fail (Error.to_string e));
  match A2a_task_store.gc ~max_age_s:100.0 store with
  | Ok removed ->
    check int "removed 1" 1 removed;
    check bool "old removed" true (Option.is_none (A2a_task_store.get_task store "old_done"));
    check bool "recent kept" true (Option.is_some (A2a_task_store.get_task store "recent"))
  | Error e -> fail (Error.to_string e)

let test_gc_keeps_non_terminal () =
  with_temp_store @@ fun store ->
  let working_task = { (make_task ~id:"working" ~state:Working ()) with
    updated_at = Unix.gettimeofday () -. 200.0 } in
  (match A2a_task_store.store_task store working_task with Ok () -> () | Error e -> fail (Error.to_string e));
  match A2a_task_store.gc ~max_age_s:100.0 store with
  | Ok removed ->
    check int "none removed" 0 removed;
    check bool "still there" true (Option.is_some (A2a_task_store.get_task store "working"))
  | Error e -> fail (Error.to_string e)

(* ── Validation ─────────────────────────────────────────── *)

let test_invalid_task_id_empty () =
  with_temp_store @@ fun store ->
  let task = { (make_task ()) with id = "" } in
  match A2a_task_store.store_task store task with
  | Error _ -> ()  (* expected *)
  | Ok () -> fail "should reject empty id"

let test_invalid_task_id_slash () =
  with_temp_store @@ fun store ->
  let task = { (make_task ()) with id = "a/b" } in
  match A2a_task_store.store_task store task with
  | Error _ -> ()  (* expected *)
  | Ok () -> fail "should reject slash in id"

let test_invalid_task_id_null () =
  with_temp_store @@ fun store ->
  let task = { (make_task ()) with id = "a\000b" } in
  match A2a_task_store.store_task store task with
  | Error _ -> ()  (* expected *)
  | Ok () -> fail "should reject null byte in id"

(* ── Corrupted JSON skip ────────────────────────────────── *)

let test_reload_skips_bad_json () =
  Eio_main.run @@ fun env ->
  let fs = Eio.Stdenv.fs env in
  let dir = Printf.sprintf "/tmp/oas_test_bad_json_%d_%d"
    (Unix.getpid ()) (int_of_float (Unix.gettimeofday () *. 1000.0) mod 100_000) in
  let path = Eio.Path.(fs / dir) in
  (match A2a_task_store.create path with
   | Error e -> fail (Error.to_string e)
   | Ok store ->
     (* Write a valid task *)
     let task = make_task ~id:"good" () in
     (match A2a_task_store.store_task store task with
      | Ok () -> ()
      | Error e -> fail (Error.to_string e));
     (* Write a corrupted file directly *)
     Eio.Path.save ~create:(`Or_truncate 0o644)
       Eio.Path.(path / "bad.json") "not valid json{{{";
     (* Reload should skip the bad file *)
     (match A2a_task_store.reload store with
      | Ok () ->
        let tasks = A2a_task_store.list_tasks store in
        check int "only good task" 1 (List.length tasks)
      | Error e -> fail (Error.to_string e)));
  (* cleanup *)
  (try
     let entries = Sys.readdir dir in
     Array.iter (fun name ->
       try Sys.remove (Filename.concat dir name) with _ -> ()
     ) entries;
     Sys.rmdir dir
   with _ -> ())

(* ── Concurrent writes (same task) ──────────────────────── *)

let test_concurrent_writes () =
  with_temp_store @@ fun store ->
  let task1 = make_task ~id:"concurrent" () in
  let task2 = { task1 with state = Working; updated_at = Unix.gettimeofday () +. 1.0 } in
  (* Write both — last write wins *)
  (match A2a_task_store.store_task store task1 with Ok () -> () | Error e -> fail (Error.to_string e));
  (match A2a_task_store.store_task store task2 with Ok () -> () | Error e -> fail (Error.to_string e));
  match A2a_task_store.get_task store "concurrent" with
  | Some t ->
    check string "last write wins" "working" (A2a_task.task_state_to_string t.state)
  | None -> fail "task not found"

(* ── Multiple tasks round-trip ──────────────────────────── *)

let test_multiple_tasks_roundtrip () =
  with_temp_store @@ fun store ->
  let ids = List.init 10 (fun i -> Printf.sprintf "task_%03d" i) in
  List.iter (fun id ->
    let task = make_task ~id () in
    match A2a_task_store.store_task store task with
    | Ok () -> ()
    | Error e -> fail (Error.to_string e)
  ) ids;
  let tasks = A2a_task_store.list_tasks store in
  check int "all stored" 10 (List.length tasks);
  List.iter (fun id ->
    check bool ("exists: " ^ id) true
      (Option.is_some (A2a_task_store.get_task store id))
  ) ids

(* ── Test suite ─────────────────────────────────────────── *)

let () =
  run "A2a_task_store" [
    "store", [
      test_case "store and get" `Quick test_store_and_get;
      test_case "store overwrites" `Quick test_store_overwrites;
      test_case "list tasks" `Quick test_list_tasks;
      test_case "multiple round-trip" `Quick test_multiple_tasks_roundtrip;
      test_case "concurrent writes" `Quick test_concurrent_writes;
    ];
    "delete", [
      test_case "delete" `Quick test_delete;
      test_case "delete nonexistent" `Quick test_delete_nonexistent;
    ];
    "reload", [
      test_case "reload restores" `Quick test_reload_restores;
      test_case "reload skips bad json" `Quick test_reload_skips_bad_json;
    ];
    "gc", [
      test_case "removes old terminal" `Quick test_gc_removes_old_terminal;
      test_case "keeps non-terminal" `Quick test_gc_keeps_non_terminal;
    ];
    "validation", [
      test_case "empty id" `Quick test_invalid_task_id_empty;
      test_case "slash in id" `Quick test_invalid_task_id_slash;
      test_case "null byte in id" `Quick test_invalid_task_id_null;
    ];
  ]
