(** Unit tests for Memory module (v0.65.0). *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let json_s s = `String s
let json_i i = `Int i

let check_ok label = function
  | Ok () -> ()
  | Error reason -> failf "%s: %s" label reason
;;

let check_missing_key label = function
  | Error Memory.Missing_key -> ()
  | Ok json -> failf "%s: expected missing key, got %s" label (Yojson.Safe.to_string json)
  | Error err -> failf "%s: %s" label (Memory.retrieve_error_to_string err)
;;

let episode ?(id = "ep-1") ?(timestamp = 10.0) ?(salience = 0.8) action =
  { Memory.id
  ; timestamp
  ; participants = [ "agent"; "user" ]
  ; action
  ; outcome = Memory.Success "done"
  ; salience
  ; metadata = [ "source", `String "test" ]
  }
;;

let procedure
      ?(id = "proc-1")
      ?(pattern = "deploy service")
      ?(confidence = 0.7)
      ?(success_count = 7)
      ?(failure_count = 3)
      action
  =
  { Memory.id
  ; pattern
  ; action
  ; success_count
  ; failure_count
  ; confidence
  ; last_used = 1.0
  ; metadata = [ "team", `String "runtime" ]
  }
;;

(* ── Basic store/recall ───────────────────────────── *)

let test_store_and_recall_scratchpad () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Scratchpad "key1" (json_s "val1"));
  match Memory.recall mem ~tier:Scratchpad "key1" with
  | Some (`String "val1") -> ()
  | _ -> fail "expected val1 in scratchpad"
;;

let test_store_and_recall_working () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "key1" (json_s "work"));
  match Memory.recall mem ~tier:Working "key1" with
  | Some (`String "work") -> ()
  | _ -> fail "expected work in working"
;;

let test_recall_missing () =
  let mem = Memory.create () in
  check
    bool
    "missing key"
    true
    (Option.is_none (Memory.recall mem ~tier:Scratchpad "nope"))
;;

let test_result_helpers () =
  check
    string
    "missing string"
    "missing_key"
    (Memory.retrieve_error_to_string Memory.Missing_key);
  check
    string
    "backend error string"
    "backend_error: corrupt"
    (Memory.retrieve_error_to_string (Memory.Backend_error "corrupt"));
  let mem = Memory.create () in
  check_missing_key
    "missing exact working"
    (Memory.recall_exact_result mem ~tier:Working "missing");
  check_missing_key
    "missing recall working"
    (Memory.recall_result mem ~tier:Working "missing")
;;

(* ── Tier fallback ────────────────────────────────── *)

let test_scratchpad_falls_back_to_working () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "shared" (json_s "from_working"));
  (* Recall from Scratchpad tier, should fall back to Working *)
  match Memory.recall mem ~tier:Scratchpad "shared" with
  | Some (`String "from_working") -> ()
  | _ -> fail "expected fallback to working"
;;

let test_working_falls_back_to_long_term () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Long_term "deep" (json_s "lt_val"));
  match Memory.recall mem ~tier:Working "deep" with
  | Some (`String "lt_val") -> ()
  | _ -> fail "expected fallback to long_term"
;;

let test_recall_exact_no_fallback () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "only_work" (json_s "here"));
  check
    bool
    "exact scratchpad miss"
    true
    (Option.is_none (Memory.recall_exact mem ~tier:Scratchpad "only_work"))
;;

(* ── Promote ──────────────────────────────────────── *)

let test_promote_scratchpad_to_working () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Scratchpad "temp" (json_i 42));
  let promoted = Memory.promote mem "temp" in
  check bool "promoted" true promoted;
  (* Should be in Working now *)
  (match Memory.recall_exact mem ~tier:Working "temp" with
   | Some (`Int 42) -> ()
   | _ -> fail "expected in working after promote");
  (* Should be gone from Scratchpad *)
  check
    bool
    "scratchpad cleared"
    true
    (Option.is_none (Memory.recall_exact mem ~tier:Scratchpad "temp"))
;;

let test_promote_missing_key () =
  let mem = Memory.create () in
  check bool "promote missing" false (Memory.promote mem "ghost")
;;

(* ── Forget ───────────────────────────────────────── *)

let test_forget_working () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "bye" (json_s "gone"));
  ignore (Memory.forget mem ~tier:Working "bye");
  check
    bool
    "forgotten"
    true
    (Option.is_none (Memory.recall_exact mem ~tier:Working "bye"))
;;

(* ── Clear scratchpad ─────────────────────────────── *)

let test_clear_scratchpad () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Scratchpad "a" (json_i 1));
  ignore (Memory.store mem ~tier:Scratchpad "b" (json_i 2));
  ignore (Memory.store mem ~tier:Working "c" (json_i 3));
  Memory.clear_scratchpad mem;
  let s, w, _, _, _ = Memory.stats mem in
  check int "scratchpad empty" 0 s;
  check int "working intact" 1 w
;;

(* ── Working entries ──────────────────────────────── *)

let test_working_entries () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "x" (json_s "1"));
  ignore (Memory.store mem ~tier:Working "y" (json_s "2"));
  ignore (Memory.store mem ~tier:Scratchpad "z" (json_s "3"));
  let entries = Memory.working_entries mem in
  check int "2 working entries" 2 (List.length entries)
;;

(* ── Stats ────────────────────────────────────────── *)

let test_stats () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Scratchpad "s1" (json_i 1));
  ignore (Memory.store mem ~tier:Scratchpad "s2" (json_i 2));
  ignore (Memory.store mem ~tier:Working "w1" (json_i 3));
  ignore (Memory.store mem ~tier:Long_term "l1" (json_i 4));
  let s, w, _, _, l = Memory.stats mem in
  check int "scratchpad" 2 s;
  check int "working" 1 w;
  check int "long_term" 1 l
;;

(* ── Long-term backend ────────────────────────────── *)

let test_long_term_backend () =
  let store = Hashtbl.create 4 in
  let backend : Memory.long_term_backend =
    { persist =
        (fun ~key value ->
          Hashtbl.replace store key value;
          Ok ())
    ; retrieve = (fun ~key -> Hashtbl.find_opt store key)
    ; remove =
        (fun ~key ->
          Hashtbl.remove store key;
          Ok ())
    ; batch_persist =
        (fun pairs ->
          List.iter (fun (k, v) -> Hashtbl.replace store k v) pairs;
          Ok ())
    ; query =
        (fun ~prefix ~limit ->
          Hashtbl.fold
            (fun k v acc ->
               if
                 String.length k >= String.length prefix
                 && String.sub k 0 (String.length prefix) = prefix
               then (k, v) :: acc
               else acc)
            store
            []
          |> List.filteri (fun i _ -> i < limit))
    }
  in
  let mem = Memory.create ~long_term:backend () in
  ignore (Memory.store mem ~tier:Long_term "lt_key" (json_s "persisted"));
  (* Backend should have it *)
  (match Hashtbl.find_opt store "lt_key" with
   | Some (`String "persisted") -> ()
   | _ -> fail "backend should have the value");
  (* Recall via long_term tier *)
  (match Memory.recall mem ~tier:Long_term "lt_key" with
   | Some (`String "persisted") -> ()
   | _ -> fail "recall should find it");
  (* Forget should remove from backend *)
  ignore (Memory.forget mem ~tier:Long_term "lt_key");
  check bool "backend removed" true (not (Hashtbl.mem store "lt_key"))
;;

let test_long_term_backend_set_after_create () =
  let store = Hashtbl.create 4 in
  let backend : Memory.long_term_backend =
    { persist =
        (fun ~key value ->
          Hashtbl.replace store key value;
          Ok ())
    ; retrieve = (fun ~key -> Hashtbl.find_opt store key)
    ; remove =
        (fun ~key ->
          Hashtbl.remove store key;
          Ok ())
    ; batch_persist =
        (fun pairs ->
          List.iter (fun (k, v) -> Hashtbl.replace store k v) pairs;
          Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create () in
  Memory.set_long_term_backend mem backend;
  ignore (Memory.store mem ~tier:Long_term "late" (json_i 99));
  match Hashtbl.find_opt store "late" with
  | Some (`Int 99) -> ()
  | _ -> fail "late backend should work"
;;

let test_legacy_backend_batch_and_remove () =
  let store = Hashtbl.create 4 in
  let removed = ref [] in
  let backend =
    Memory.legacy_backend
      ~persist:(fun ~key value -> Hashtbl.replace store key value)
      ~retrieve:(fun ~key -> Hashtbl.find_opt store key)
      ~remove:(fun ~key ->
        removed := key :: !removed;
        Hashtbl.remove store key)
  in
  check_ok "legacy persist" (backend.persist ~key:"one" (json_i 1));
  check_ok "legacy batch" (backend.batch_persist [ "two", json_i 2; "three", json_i 3 ]);
  (match backend.retrieve ~key:"two" with
   | Some (`Int 2) -> ()
   | _ -> fail "batch should persist values");
  check int "legacy query empty" 0 (List.length (backend.query ~prefix:"" ~limit:10));
  check_ok "legacy remove" (backend.remove ~key:"one");
  check (list string) "remove callback called" [ "one" ] !removed
;;

let test_long_term_backend_errors_and_result_fallback () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _ -> Error "disk full")
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Error "permission denied")
    ; batch_persist = (fun _ -> Error "batch failed")
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let retrieve_result ~key =
    match key with
    | "broken" -> Error (Memory.Backend_error "bad json")
    | _ -> Error Memory.Missing_key
  in
  let mem =
    Memory.create ~long_term:backend ~long_term_retrieve_result:retrieve_result ()
  in
  (match Memory.store mem ~tier:Long_term "broken" (json_s "cached") with
   | Error "disk full" -> ()
   | Ok () -> fail "store should surface backend persist error"
   | Error reason -> failf "unexpected persist error: %s" reason);
  (match Memory.recall_exact_result mem ~tier:Long_term "broken" with
   | Error (Memory.Backend_error "bad json") -> ()
   | Ok json -> failf "expected backend error, got %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected retrieve error: %s" (Memory.retrieve_error_to_string err));
  (match Memory.recall_result mem ~tier:Scratchpad "broken" with
   | Error (Memory.Backend_error "bad json") -> ()
   | Ok json ->
     failf "expected scratchpad fallback error, got %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected fallback error: %s" (Memory.retrieve_error_to_string err));
  match Memory.forget mem ~tier:Long_term "broken" with
  | Error "permission denied" -> ()
  | Ok () -> fail "forget should surface backend remove error"
  | Error reason -> failf "unexpected remove error: %s" reason
;;

let test_query_long_term_prefix () =
  let store = Hashtbl.create 4 in
  let backend : Memory.long_term_backend =
    { persist =
        (fun ~key value ->
          Hashtbl.replace store key value;
          Ok ())
    ; retrieve = (fun ~key -> Hashtbl.find_opt store key)
    ; remove =
        (fun ~key ->
          Hashtbl.remove store key;
          Ok ())
    ; batch_persist =
        (fun pairs ->
          List.iter (fun (k, v) -> Hashtbl.replace store k v) pairs;
          Ok ())
    ; query =
        (fun ~prefix ~limit ->
          Hashtbl.fold
            (fun k v acc -> if String.starts_with ~prefix k then (k, v) :: acc else acc)
            store
            []
          |> List.sort (fun (a, _) (b, _) -> String.compare a b)
          |> List.filteri (fun i _ -> i < limit))
    }
  in
  let mem = Memory.create ~long_term:backend () in
  ignore (Memory.store mem ~tier:Long_term "world:mission" (json_s "ready"));
  ignore (Memory.store mem ~tier:Long_term "world:crew" (json_s "awake"));
  ignore (Memory.store mem ~tier:Long_term "other:key" (json_s "skip"));
  let results = Memory.query mem ~tier:Long_term ~prefix:"world" ~limit:10 in
  let keys = List.map fst results in
  check int "world entries" 2 (List.length results);
  check bool "mission" true (List.mem "world:mission" keys);
  check bool "crew" true (List.mem "world:crew" keys)
;;

let test_query_limits_and_deduplicates_backend_first () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _ -> Ok ())
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _ -> Ok ())
    ; query =
        (fun ~prefix ~limit:_ ->
          [ prefix ^ ":dup", json_s "backend"
          ; prefix ^ ":backend-only", json_s "backend-only"
          ; prefix ^ ":extra", json_s "extra"
          ])
    }
  in
  let mem = Memory.create () in
  check_ok "context dup" (Memory.store mem ~tier:Long_term "world:dup" (json_s "ctx"));
  check_ok
    "context only"
    (Memory.store mem ~tier:Long_term "world:context-only" (json_s "ctx-only"));
  Memory.set_long_term_backend mem backend;
  check
    int
    "zero limit"
    0
    (List.length (Memory.query mem ~tier:Long_term ~prefix:"world" ~limit:0));
  let results = Memory.query mem ~tier:Long_term ~prefix:"world" ~limit:3 in
  check
    (list string)
    "backend order and duplicate removed"
    [ "world:dup"; "world:backend-only"; "world:extra" ]
    (List.map fst results);
  match List.assoc_opt "world:dup" results with
  | Some (`String "backend") -> ()
  | _ -> fail "backend duplicate should win"
;;

let test_scratchpad_entries () =
  let mem = Memory.create () in
  check_ok "scratch a" (Memory.store mem ~tier:Scratchpad "a" (json_i 1));
  check_ok "scratch b" (Memory.store mem ~tier:Scratchpad "b" (json_i 2));
  check_ok "working c" (Memory.store mem ~tier:Working "c" (json_i 3));
  let keys = Memory.scratchpad_entries mem |> List.map fst |> List.sort String.compare in
  check (list string) "scratchpad keys" [ "a"; "b" ] keys
;;

(* ── Episodic facade ───────────────────────────────── *)

let make_episodic_backend () =
  let store = Hashtbl.create 4 in
  let backend : Memory.episodic_backend =
    { persist_episode = (fun ep -> Hashtbl.replace store ep.Memory.id ep)
    ; retrieve_episode = (fun ~id -> Hashtbl.find_opt store id)
    ; remove_episode = (fun ~id -> Hashtbl.remove store id)
    ; all_episodes = (fun () -> Hashtbl.to_seq_values store |> List.of_seq)
    }
  in
  backend, store
;;

let test_episodic_backend_recall_query_and_forget () =
  let backend, store = make_episodic_backend () in
  let mem = Memory.create ~episodic:backend () in
  let ep = episode ~id:"ep:alpha" "deploy alpha" in
  Memory.store_episode mem ep;
  check bool "backend persisted" true (Hashtbl.mem store "ep:alpha");
  let fresh = Memory.create ~episodic:backend () in
  (match Memory.recall_episode fresh "ep:alpha" with
   | Some found -> check string "backend recall" "deploy alpha" found.Memory.action
   | None -> fail "backend episode should be recalled");
  (match Memory.recall fresh ~tier:Episodic "ep:alpha" with
   | Some (`Assoc _) -> ()
   | _ -> fail "generic episodic recall should return episode json");
  (match Memory.recall_exact_result fresh ~tier:Episodic "ep:alpha" with
   | Ok (`Assoc _) -> ()
   | Ok json -> failf "expected episode object, got %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected episodic error: %s" (Memory.retrieve_error_to_string err));
  let queried = Memory.query fresh ~tier:Episodic ~prefix:"ep:" ~limit:5 in
  check (list string) "episode query" [ "ep:alpha" ] (List.map fst queried);
  Memory.forget_episode fresh "ep:alpha";
  check bool "backend removed" false (Hashtbl.mem store "ep:alpha")
;;

let test_episodic_recall_sort_filter_boost_and_count () =
  let mem = Memory.create () in
  Memory.store_episode mem (episode ~id:"ep:low" ~timestamp:1.0 ~salience:0.2 "old");
  Memory.store_episode mem (episode ~id:"ep:high" ~timestamp:9.0 ~salience:0.7 "new");
  Memory.boost_salience mem "ep:low" 0.9;
  Memory.boost_salience mem "missing" 0.5;
  check int "episode count" 2 (Memory.episode_count mem);
  let recalled =
    Memory.recall_episodes
      mem
      ~now:10.0
      ~decay_rate:0.0
      ~min_salience:0.5
      ~limit:1
      ~filter:(fun ep -> String.starts_with ~prefix:"ep:" ep.Memory.id)
      ()
  in
  match recalled with
  | [ ep ] ->
    check string "boosted first" "ep:low" ep.Memory.id;
    check (float 0.0001) "salience capped" 1.0 ep.Memory.salience
  | _ -> fail "expected one boosted episode"
;;

(* ── Procedural facade ─────────────────────────────── *)

let make_procedural_backend () =
  let store = Hashtbl.create 4 in
  let backend : Memory.procedural_backend =
    { persist_procedure = (fun proc -> Hashtbl.replace store proc.Memory.id proc)
    ; retrieve_procedure = (fun ~id -> Hashtbl.find_opt store id)
    ; remove_procedure = (fun ~id -> Hashtbl.remove store id)
    ; all_procedures = (fun () -> Hashtbl.to_seq_values store |> List.of_seq)
    }
  in
  backend, store
;;

let test_procedural_backend_matching_touch_and_updates () =
  let backend, store = make_procedural_backend () in
  let mem = Memory.create ~procedural:backend () in
  Memory.store_procedure
    mem
    (procedure
       ~id:"deploy-safe"
       ~pattern:"deploy service safely"
       ~confidence:0.9
       "run smoke tests");
  Memory.store_procedure
    mem
    (procedure
       ~id:"deploy-risky"
       ~pattern:"deploy service quickly"
       ~confidence:0.4
       "skip checks");
  check int "procedure count" 2 (Memory.procedure_count mem);
  let matches =
    Memory.matching_procedures
      mem
      ~pattern:"deploy"
      ~min_confidence:0.5
      ~filter:(fun proc -> proc.Memory.action <> "skip checks")
      ()
  in
  check
    (list string)
    "filtered matches"
    [ "deploy-safe" ]
    (List.map (fun p -> p.Memory.id) matches);
  let before = (Option.get (Hashtbl.find_opt store "deploy-safe")).Memory.last_used in
  Unix.sleepf 0.001;
  (match Memory.find_procedure mem ~pattern:"deploy" ~touch:true () with
   | Some proc ->
     check string "found best" "deploy-safe" proc.Memory.id;
     check bool "touch updates last_used" true (proc.Memory.last_used > before)
   | None -> fail "expected best procedure");
  Memory.record_success mem "deploy-safe";
  Memory.record_failure mem "deploy-safe";
  Memory.record_success mem "missing";
  (match Memory.find_procedure mem ~pattern:"deploy service safely" () with
   | Some proc ->
     check int "success incremented" 8 proc.Memory.success_count;
     check int "failure incremented" 4 proc.Memory.failure_count;
     check (float 0.0001) "confidence recomputed" (8.0 /. 12.0) proc.Memory.confidence
   | None -> fail "procedure should still exist");
  (match Memory.best_procedure mem ~pattern:"deploy" with
   | Some proc -> check string "best procedure" "deploy-safe" proc.Memory.id
   | None -> fail "expected best procedure");
  Memory.forget_procedure mem "deploy-safe";
  check bool "backend procedure removed" false (Hashtbl.mem store "deploy-safe")
;;

let test_procedural_generic_recall_and_query () =
  let backend, _store = make_procedural_backend () in
  let proc =
    procedure ~id:"ops:restart" ~pattern:"restart service" "restart then tail logs"
  in
  backend.persist_procedure proc;
  let mem = Memory.create ~procedural:backend () in
  (match Memory.recall mem ~tier:Procedural "ops:restart" with
   | Some (`Assoc _) -> ()
   | _ -> fail "generic procedural recall should return procedure json");
  (match Memory.recall_exact_result mem ~tier:Procedural "ops:restart" with
   | Ok (`Assoc _) -> ()
   | Ok json -> failf "expected procedure object, got %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected procedural error: %s" (Memory.retrieve_error_to_string err));
  let queried = Memory.query mem ~tier:Procedural ~prefix:"ops:" ~limit:5 in
  check (list string) "procedure query" [ "ops:restart" ] (List.map fst queried)
;;

(* ── Context access ───────────────────────────────── *)

let test_context_access () =
  let ctx = Context.create () in
  let mem = Memory.create ~ctx () in
  ignore (Memory.store mem ~tier:Working "via_mem" (json_s "hello"));
  (* Should be visible in the underlying context *)
  let ctx_out = Memory.context mem in
  match Context.get_scoped ctx_out Session "via_mem" with
  | Some (`String "hello") -> ()
  | _ -> fail "context should have session:via_mem"
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "memory"
    [ ( "basic"
      , [ test_case "store/recall scratchpad" `Quick test_store_and_recall_scratchpad
        ; test_case "store/recall working" `Quick test_store_and_recall_working
        ; test_case "recall missing" `Quick test_recall_missing
        ; test_case "result helpers" `Quick test_result_helpers
        ] )
    ; ( "fallback"
      , [ test_case "scratchpad -> working" `Quick test_scratchpad_falls_back_to_working
        ; test_case "working -> long_term" `Quick test_working_falls_back_to_long_term
        ; test_case "recall_exact no fallback" `Quick test_recall_exact_no_fallback
        ] )
    ; ( "promote"
      , [ test_case "scratchpad to working" `Quick test_promote_scratchpad_to_working
        ; test_case "missing key" `Quick test_promote_missing_key
        ] )
    ; "forget", [ test_case "forget working" `Quick test_forget_working ]
    ; ( "lifecycle"
      , [ test_case "clear scratchpad" `Quick test_clear_scratchpad
        ; test_case "working entries" `Quick test_working_entries
        ; test_case "scratchpad entries" `Quick test_scratchpad_entries
        ; test_case "stats" `Quick test_stats
        ] )
    ; ( "long_term"
      , [ test_case "backend persist/retrieve/remove" `Quick test_long_term_backend
        ; test_case
            "set backend after create"
            `Quick
            test_long_term_backend_set_after_create
        ; test_case "legacy backend" `Quick test_legacy_backend_batch_and_remove
        ; test_case
            "backend errors and result fallback"
            `Quick
            test_long_term_backend_errors_and_result_fallback
        ; test_case "query prefix" `Quick test_query_long_term_prefix
        ; test_case
            "query limit and dedupe"
            `Quick
            test_query_limits_and_deduplicates_backend_first
        ] )
    ; ( "episodic"
      , [ test_case
            "backend recall query and forget"
            `Quick
            test_episodic_backend_recall_query_and_forget
        ; test_case
            "recall sort filter boost and count"
            `Quick
            test_episodic_recall_sort_filter_boost_and_count
        ] )
    ; ( "procedural"
      , [ test_case
            "backend matching touch and updates"
            `Quick
            test_procedural_backend_matching_touch_and_updates
        ; test_case
            "generic recall and query"
            `Quick
            test_procedural_generic_recall_and_query
        ] )
    ; "context", [ test_case "context access" `Quick test_context_access ]
    ]
;;
