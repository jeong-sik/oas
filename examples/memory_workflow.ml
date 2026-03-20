(** Memory workflow example: 3-tier memory with persistence.

    Demonstrates:
    - Memory.create with Scratchpad / Working / Long_term tiers
    - store, recall, recall_exact, promote, forget
    - Tier fallback chain (Scratchpad -> Working -> Long_term)
    - long_term_backend for file-based persistence
    - clear_scratchpad for per-turn cleanup
    - stats for debugging

    This example runs standalone (no LLM needed).

    Usage:
      dune exec examples/memory_workflow.exe *)

open Agent_sdk

let json_s s = `String s
let json_i i = `Int i

(* ── File-based long-term backend ────────────────────── *)

let file_backend dir : Memory.long_term_backend =
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path_of key = Filename.concat dir (key ^ ".json") in
  {
    persist = (fun ~key value ->
      let oc = open_out (path_of key) in
      output_string oc (Yojson.Safe.to_string value);
      close_out oc);
    retrieve = (fun ~key ->
      let p = path_of key in
      if Sys.file_exists p then begin
        let ic = open_in p in
        let content = really_input_string ic (in_channel_length ic) in
        close_in ic;
        Some (Yojson.Safe.from_string content)
      end else None);
    remove = (fun ~key ->
      let p = path_of key in
      if Sys.file_exists p then Sys.remove p);
  }

(* ── Main ────────────────────────────────────────────── *)

let print_stats mem label =
  let (s, w, ep, pr, l) = Memory.stats mem in
  Printf.printf "  [%s] scratchpad=%d working=%d episodic=%d procedural=%d long_term=%d\n" label s w ep pr l

let () =
  let tmp_dir = Filename.concat (Filename.get_temp_dir_name ()) "oas_mem_demo" in
  let backend = file_backend tmp_dir in
  let mem = Memory.create ~long_term:backend () in

  Printf.printf "=== Memory Workflow Demo ===\n\n";

  (* 1. Store in different tiers *)
  Printf.printf "1. Storing data across tiers:\n";
  Memory.store mem ~tier:Scratchpad "temp_result" (json_s "intermediate");
  Memory.store mem ~tier:Working "user_pref" (json_s "dark_mode");
  Memory.store mem ~tier:Long_term "session_count" (json_i 42);
  print_stats mem "after stores";

  (* 2. Recall with fallback *)
  Printf.printf "\n2. Recall with tier fallback:\n";
  let v1 = Memory.recall mem ~tier:Scratchpad "user_pref" in
  Printf.printf "  recall 'user_pref' from Scratchpad: %s (falls back to Working)\n"
    (match v1 with Some v -> Yojson.Safe.to_string v | None -> "None");
  let v2 = Memory.recall_exact mem ~tier:Scratchpad "user_pref" in
  Printf.printf "  recall_exact 'user_pref' from Scratchpad: %s (no fallback)\n"
    (match v2 with Some v -> Yojson.Safe.to_string v | None -> "None");

  (* 3. Promote from Scratchpad to Working *)
  Printf.printf "\n3. Promoting 'temp_result' from Scratchpad to Working:\n";
  let promoted = Memory.promote mem "temp_result" in
  Printf.printf "  promoted: %b\n" promoted;
  print_stats mem "after promote";

  (* 4. Clear scratchpad (simulating turn boundary) *)
  Printf.printf "\n4. Clearing scratchpad (turn boundary):\n";
  Memory.store mem ~tier:Scratchpad "ephemeral" (json_s "gone soon");
  print_stats mem "before clear";
  Memory.clear_scratchpad mem;
  print_stats mem "after clear";

  (* 5. Working entries *)
  Printf.printf "\n5. Working entries:\n";
  let entries = Memory.working_entries mem in
  List.iter (fun (k, v) ->
    Printf.printf "  %s = %s\n" k (Yojson.Safe.to_string v)
  ) entries;

  (* 6. Long-term persistence verification *)
  Printf.printf "\n6. Long-term persistence:\n";
  let fresh_mem = Memory.create ~long_term:backend () in
  let recovered = Memory.recall fresh_mem ~tier:Long_term "session_count" in
  Printf.printf "  recovered 'session_count' from new Memory: %s\n"
    (match recovered with Some v -> Yojson.Safe.to_string v | None -> "None");

  (* 7. Forget *)
  Printf.printf "\n7. Forget 'user_pref' from Working:\n";
  Memory.forget mem ~tier:Working "user_pref";
  let gone = Memory.recall_exact mem ~tier:Working "user_pref" in
  Printf.printf "  after forget: %s\n"
    (match gone with Some _ -> "still there" | None -> "gone");
  print_stats mem "final";

  (* Cleanup *)
  (try
     Array.iter (fun f -> Sys.remove (Filename.concat tmp_dir f))
       (Sys.readdir tmp_dir);
     Unix.rmdir tmp_dir
   with _ -> ());
  Printf.printf "\nDone.\n"
