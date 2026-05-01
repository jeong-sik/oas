open Base
(** Memory workflow example: 5-tier memory with persistence and recall.

    Demonstrates:
    - Memory.create with Scratchpad / Working / Episodic / Procedural / Long_term tiers
    - store, recall, recall_exact, promote, forget
    - Tier fallback chain (Scratchpad -> Working -> Long_term)
    - Episodic recall with participant/outcome filters
    - Procedural recall with confidence filters
    - long_term_backend for file-based persistence
    - clear_scratchpad for per-turn cleanup
    - stats for debugging

    This example runs standalone (no LLM needed).

    Usage:
      dune exec examples/memory_workflow.exe *)

open Agent_sdk

let json_s s = `String s
let json_i i = `Int i

let string_of_outcome = function
  | Memory.Success detail -> "success:" ^ detail
  | Memory.Failure detail -> "failure:" ^ detail
  | Memory.Neutral -> "neutral"
;;

(* ── File-based long-term backend ────────────────────── *)

let file_backend dir : Memory.long_term_backend =
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path_of key = Filename.concat dir (key ^ ".json") in
  let do_persist ~key value =
    try
      let oc = open_out (path_of key) in
      output_string oc (Yojson.Safe.to_string value);
      close_out oc;
      Ok ()
    with
    | exn -> Error (Printexc.to_string exn)
  in
  { persist = do_persist
  ; retrieve =
      (fun ~key ->
        let p = path_of key in
        if Sys.file_exists p
        then (
          let ic = open_in p in
          let content = really_input_string ic (in_channel_length ic) in
          close_in ic;
          Some (Yojson.Safe.from_string content))
        else None)
  ; remove =
      (fun ~key ->
        try
          let p = path_of key in
          if Sys.file_exists p then Sys.remove p;
          Ok ()
        with
        | exn -> Error (Printexc.to_string exn))
  ; batch_persist =
      (fun pairs ->
        try
          List.iter
            (fun (k, v) ->
               match do_persist ~key:k v with
               | Ok () -> ()
               | Error e -> failwith e)
            pairs;
          Ok ()
        with
        | Failure e -> Error e)
  ; query =
      (fun ~prefix ~limit ->
        let files =
          try Sys.readdir dir with
          | _ -> [||]
        in
        Array.to_list files
        |> List.filter_map (fun f ->
          if Filename.check_suffix f ".json"
          then (
            let key = Filename.chop_suffix f ".json" in
            if
              String.length key >= String.length prefix
              && String.sub key 0 (String.length prefix) = prefix
            then Some key
            else None)
          else None)
        |> List.filteri (fun i _ -> i < limit)
        |> List.filter_map (fun key ->
          let p = path_of key in
          if Sys.file_exists p
          then (
            let ic = open_in p in
            let content = really_input_string ic (in_channel_length ic) in
            close_in ic;
            Some (key, Yojson.Safe.from_string content))
          else None))
  }
;;

(* ── Main ────────────────────────────────────────────── *)

let print_stats mem label =
  let s, w, ep, pr, l = Memory.stats mem in
  Printf.printf
    "  [%s] scratchpad=%d working=%d episodic=%d procedural=%d long_term=%d\n"
    label
    s
    w
    ep
    pr
    l
;;

let () =
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) "oas_mem_demo" in
  let backend = file_backend tmp_dir in
  let mem = Memory.create ~long_term:backend () in
  Printf.printf "=== Memory Workflow Demo ===\n\n";
  (* 1. Store in different tiers *)
  Printf.printf "1. Storing data across tiers:\n";
  ignore (Memory.store mem ~tier:Scratchpad "temp_result" (json_s "intermediate"));
  ignore (Memory.store mem ~tier:Working "user_pref" (json_s "dark_mode"));
  ignore (Memory.store mem ~tier:Long_term "session_count" (json_i 42));
  print_stats mem "after stores";
  (* 2. Recall with fallback *)
  Printf.printf "\n2. Recall with tier fallback:\n";
  let v1 = Memory.recall mem ~tier:Scratchpad "user_pref" in
  Printf.printf
    "  recall 'user_pref' from Scratchpad: %s (falls back to Working)\n"
    (match v1 with
     | Some v -> Yojson.Safe.to_string v
     | None -> "None");
  let v2 = Memory.recall_exact mem ~tier:Scratchpad "user_pref" in
  Printf.printf
    "  recall_exact 'user_pref' from Scratchpad: %s (no fallback)\n"
    (match v2 with
     | Some v -> Yojson.Safe.to_string v
     | None -> "None");
  (* 3. Promote from Scratchpad to Working *)
  Printf.printf "\n3. Promoting 'temp_result' from Scratchpad to Working:\n";
  let promoted = Memory.promote mem "temp_result" in
  Printf.printf "  promoted: %b\n" promoted;
  print_stats mem "after promote";
  (* 4. Clear scratchpad (simulating turn boundary) *)
  Printf.printf "\n4. Clearing scratchpad (turn boundary):\n";
  ignore (Memory.store mem ~tier:Scratchpad "ephemeral" (json_s "gone soon"));
  print_stats mem "before clear";
  Memory.clear_scratchpad mem;
  print_stats mem "after clear";
  (* 5. Working entries *)
  Printf.printf "\n5. Working entries:\n";
  let entries = Memory.working_entries mem in
  List.iter
    (fun (k, v) -> Printf.printf "  %s = %s\n" k (Yojson.Safe.to_string v))
    entries;
  (* 6. Episodic recall *)
  Printf.printf "\n6. Episodic recall:\n";
  Memory.store_episode
    mem
    { id = "ep_success"
    ; timestamp = Unix.gettimeofday () -. 30.0
    ; participants = [ "alice"; "reviewer" ]
    ; action = "deploy v0.75.0"
    ; outcome = Memory.Success "all green"
    ; salience = 0.9
    ; metadata = [ "env", `String "prod" ]
    };
  Memory.store_episode
    mem
    { id = "ep_failure"
    ; timestamp = Unix.gettimeofday () -. 10.0
    ; participants = [ "alice"; "ci" ]
    ; action = "deploy v0.76.0"
    ; outcome = Memory.Failure "smoke test failed"
    ; salience = 0.8
    ; metadata = [ "env", `String "staging" ]
    };
  let alice_episodes =
    Memory.recall_episodes mem ~filter:(fun ep -> List.mem "alice" ep.participants) ()
  in
  List.iter
    (fun (ep : Memory.episode) ->
       Printf.printf
         "  alice episode: %s (%s, salience=%.2f)\n"
         ep.id
         (string_of_outcome ep.outcome)
         ep.salience)
    alice_episodes;
  let failures =
    Memory.recall_episodes
      mem
      ~filter:(fun ep ->
        match ep.outcome with
        | Memory.Failure _ -> true
        | Memory.Success _ | Memory.Neutral -> false)
      ()
  in
  List.iter
    (fun (ep : Memory.episode) ->
       Printf.printf "  failed episode: %s action=%s\n" ep.id ep.action)
    failures;
  (* 7. Procedural recall *)
  Printf.printf "\n7. Procedural recall:\n";
  Memory.store_procedure
    mem
    { id = "pr_deploy"
    ; pattern = "deploy failed"
    ; action = "rollback, inspect smoke logs, retry"
    ; success_count = 6
    ; failure_count = 1
    ; confidence = 6.0 /. 7.0
    ; last_used = Unix.gettimeofday () -. 300.0
    ; metadata = [ "team", `String "release" ]
    };
  Memory.store_procedure
    mem
    { id = "pr_test"
    ; pattern = "unit test flake"
    ; action = "rerun focused suite once"
    ; success_count = 2
    ; failure_count = 2
    ; confidence = 0.5
    ; last_used = Unix.gettimeofday () -. 600.0
    ; metadata = [ "team", `String "ci" ]
    };
  (match
     Memory.find_procedure mem ~pattern:"deploy" ~min_confidence:0.7 ~touch:true ()
   with
   | Some proc ->
     Printf.printf
       "  best deploy procedure: %s (confidence=%.2f)\n"
       proc.action
       proc.confidence
   | None -> Printf.printf "  no deploy procedure found\n");
  (* 8. Long-term persistence verification *)
  Printf.printf "\n8. Long-term persistence:\n";
  let fresh_mem = Memory.create ~long_term:backend () in
  let recovered = Memory.recall fresh_mem ~tier:Long_term "session_count" in
  Printf.printf
    "  recovered 'session_count' from new Memory: %s\n"
    (match recovered with
     | Some v -> Yojson.Safe.to_string v
     | None -> "None");
  (* 9. Forget *)
  Printf.printf "\n9. Forget 'user_pref' from Working:\n";
  ignore (Memory.forget mem ~tier:Working "user_pref");
  let gone = Memory.recall_exact mem ~tier:Working "user_pref" in
  Printf.printf
    "  after forget: %s\n"
    (match gone with
     | Some _ -> "still there"
     | None -> "gone");
  print_stats mem "final";
  (* Cleanup *)
  (try
     Array.iter (fun f -> Sys.remove (Filename.concat tmp_dir f)) (Sys.readdir tmp_dir);
     Unix.rmdir tmp_dir
   with
   | _ -> ());
  Printf.printf "\nDone.\n"
;;
