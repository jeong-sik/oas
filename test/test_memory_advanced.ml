(** Advanced tests for Memory module — concurrency, property-based, failures.

    Extends test_memory.ml with:
    - Eio concurrent access (fiber safety)
    - QCheck property-based invariants
    - Long-term backend failure handling
    - Large-scale store/recall *)

open Alcotest
open Agent_sdk

let json_s s = `String s
let json_i i = `Int i

(* ── Concurrent access (Eio) ─────────────────────────── *)

let test_concurrent_different_keys () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let mem = Memory.create () in
  Eio.Fiber.both
    (fun () -> ignore (Memory.store mem ~tier:Working "key_a" (json_s "val_a")))
    (fun () -> ignore (Memory.store mem ~tier:Working "key_b" (json_s "val_b")));
  ignore sw;
  (match Memory.recall mem ~tier:Working "key_a" with
   | Some (`String "val_a") -> ()
   | _ -> fail "key_a missing");
  match Memory.recall mem ~tier:Working "key_b" with
  | Some (`String "val_b") -> ()
  | _ -> fail "key_b missing"
;;

let test_concurrent_same_key () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let mem = Memory.create () in
  Eio.Fiber.both
    (fun () -> ignore (Memory.store mem ~tier:Working "key" (json_i 1)))
    (fun () -> ignore (Memory.store mem ~tier:Working "key" (json_i 2)));
  ignore sw;
  (* Last write wins; just verify no crash and some value is present *)
  match Memory.recall mem ~tier:Working "key" with
  | Some (`Int n) -> check bool "value is 1 or 2" true (n = 1 || n = 2)
  | _ -> fail "key missing after concurrent write"
;;

let test_concurrent_promote_no_deadlock () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Scratchpad "promo" (json_i 42));
  ignore (Memory.store mem ~tier:Scratchpad "keep" (json_s "here"));
  Eio.Fiber.both
    (fun () ->
       let _ = Memory.promote mem "promo" in
       ())
    (fun () -> ignore (Memory.store mem ~tier:Working "other" (json_s "val")));
  ignore sw;
  (* Promoted key should be in Working *)
  match Memory.recall_exact mem ~tier:Working "promo" with
  | Some (`Int 42) -> ()
  | _ -> fail "promote failed"
;;

(* ── Property-based tests (QCheck) ───────────────────── *)

let test_prop_store_recall_identity () =
  let prop =
    QCheck.Test.make
      ~count:100
      ~name:"store/recall identity"
      QCheck.(pair string string)
      (fun (key, value) ->
         let mem = Memory.create () in
         ignore (Memory.store mem ~tier:Scratchpad key (json_s value));
         Memory.recall mem ~tier:Scratchpad key = Some (json_s value))
  in
  QCheck_alcotest.to_alcotest prop
;;

let test_prop_forget_removes () =
  let prop =
    QCheck.Test.make ~count:100 ~name:"forget removes key" QCheck.string (fun key ->
      let mem = Memory.create () in
      ignore (Memory.store mem ~tier:Working key (json_i 1));
      ignore (Memory.forget mem ~tier:Working key);
      Option.is_none (Memory.recall_exact mem ~tier:Working key))
  in
  QCheck_alcotest.to_alcotest prop
;;

let test_prop_stats_consistent () =
  let prop =
    QCheck.Test.make
      ~count:50
      ~name:"stats consistent"
      QCheck.(list_size (Gen.int_range 0 20) string)
      (fun keys ->
         let mem = Memory.create () in
         List.iter (fun k -> ignore (Memory.store mem ~tier:Scratchpad k (json_i 1))) keys;
         let unique_keys = List.sort_uniq String.compare keys |> List.length in
         let s, _, _, _, _ = Memory.stats mem in
         s = unique_keys)
  in
  QCheck_alcotest.to_alcotest prop
;;

(* ── Long-term backend failure handling ──────────────── *)

let test_backend_persist_error () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _value -> Error "disk full")
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _ -> Error "disk full")
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  (* Persist failure returns Error instead of raising *)
  match Memory.store mem ~tier:Long_term "key" (json_s "val") with
  | Error reason -> check string "persist error reason" "disk full" reason
  | Ok () -> fail "expected Error from persist"
;;

let test_backend_retrieve_returns_none () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _value -> Ok ())
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _ -> Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  ignore (Memory.store mem ~tier:Long_term "key" (json_s "val"));
  (* Backend returns None — local cache may or may not have it *)
  let result = Memory.recall mem ~tier:Long_term "key" in
  (* No crash is the main assertion *)
  ignore result
;;

let test_backend_remove_error () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _value -> Ok ())
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Error "no perms")
    ; batch_persist = (fun _ -> Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  ignore (Memory.store mem ~tier:Long_term "key" (json_s "val"));
  (* Remove failure returns Error instead of raising *)
  match Memory.forget mem ~tier:Long_term "key" with
  | Error reason -> check string "remove error reason" "no perms" reason
  | Ok () -> fail "expected Error from forget"
;;

let test_tier_fallbacks_query_dedupe_and_typed_results () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _value -> Ok ())
    ; retrieve =
        (fun ~key ->
          match key with
          | "lt" -> Some (json_s "from-backend")
          | _ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _ -> Ok ())
    ; query =
        (fun ~prefix ~limit:_ ->
          if prefix = "k"
          then [ "k1", json_s "backend"; "k2", json_s "backend-only" ]
          else [])
    }
  in
  let retrieve_result ~key =
    match key with
    | "lt" -> Ok (json_s "typed")
    | "bad" -> Error (Memory.Backend_error "corrupt")
    | _ -> Error Memory.Missing_key
  in
  let mem =
    Memory.create ~long_term:backend ~long_term_retrieve_result:retrieve_result ()
  in
  ignore (Memory.store mem ~tier:Working "scratch-fallback" (json_s "working"));
  ignore (Memory.store mem ~tier:Long_term "k1" (json_s "context"));
  check
    (option string)
    "scratch falls back to working"
    (Some "working")
    (Option.map
       (function
         | `String value -> value
         | _ -> "wrong")
       (Memory.recall mem ~tier:Scratchpad "scratch-fallback"));
  (match Memory.recall_result mem ~tier:Scratchpad "lt" with
   | Ok (`String "typed") -> ()
   | Ok json -> failf "unexpected recall_result json: %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected recall_result error: %s" (Memory.retrieve_error_to_string err));
  (match Memory.recall_exact_result mem ~tier:Long_term "bad" with
   | Error (Memory.Backend_error "corrupt") -> ()
   | Ok json -> failf "expected backend error, got %s" (Yojson.Safe.to_string json)
   | Error err -> failf "unexpected error: %s" (Memory.retrieve_error_to_string err));
  check
    (list string)
    "deduped query"
    [ "k1"; "k2" ]
    (Memory.query mem ~tier:Long_term ~prefix:"k" ~limit:10 |> List.map fst);
  check
    int
    "zero limit"
    0
    (Memory.query mem ~tier:Long_term ~prefix:"k" ~limit:0 |> List.length)
;;

let test_episodic_and_procedural_backend_fallbacks () =
  let backend_episode : Memory.episode =
    { id = "ep-backend"
    ; timestamp = 10.0
    ; participants = [ "tester" ]
    ; action = "backend episode"
    ; outcome = Neutral
    ; salience = 0.8
    ; metadata = []
    }
  in
  let backend_proc : Memory.procedure =
    { id = "pr-backend"
    ; pattern = "deploy"
    ; action = "verify"
    ; success_count = 2
    ; failure_count = 0
    ; confidence = 1.0
    ; last_used = 10.0
    ; metadata = []
    }
  in
  let stored_episodes = ref [ backend_episode ] in
  let stored_procedures = ref [ backend_proc ] in
  let episodic : Memory.episodic_backend =
    { persist_episode = (fun ep -> stored_episodes := ep :: !stored_episodes)
    ; retrieve_episode =
        (fun ~id ->
          List.find_opt (fun (ep : Memory.episode) -> ep.id = id) !stored_episodes)
    ; remove_episode =
        (fun ~id ->
          stored_episodes
          := List.filter (fun (ep : Memory.episode) -> ep.id <> id) !stored_episodes)
    ; all_episodes = (fun () -> !stored_episodes)
    }
  in
  let procedural : Memory.procedural_backend =
    { persist_procedure = (fun proc -> stored_procedures := proc :: !stored_procedures)
    ; retrieve_procedure =
        (fun ~id ->
          List.find_opt (fun (proc : Memory.procedure) -> proc.id = id) !stored_procedures)
    ; remove_procedure =
        (fun ~id ->
          stored_procedures
          := List.filter
               (fun (proc : Memory.procedure) -> proc.id <> id)
               !stored_procedures)
    ; all_procedures = (fun () -> !stored_procedures)
    }
  in
  let mem = Memory.create ~episodic ~procedural () in
  (match Memory.recall_exact_result mem ~tier:Episodic "ep-backend" with
   | Ok (`Assoc _) -> ()
   | Ok json -> failf "expected episode json, got %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected episode error: %s" (Memory.retrieve_error_to_string err));
  (match Memory.recall_exact_result mem ~tier:Procedural "pr-backend" with
   | Ok (`Assoc _) -> ()
   | Ok json -> failf "expected procedure json, got %s" (Yojson.Safe.to_string json)
   | Error err ->
     failf "unexpected procedure error: %s" (Memory.retrieve_error_to_string err));
  check
    (list string)
    "episodic query"
    [ "ep-backend" ]
    (Memory.query mem ~tier:Episodic ~prefix:"ep-" ~limit:10 |> List.map fst);
  check
    (list string)
    "procedural query"
    [ "pr-backend" ]
    (Memory.query mem ~tier:Procedural ~prefix:"pr-" ~limit:10 |> List.map fst);
  Memory.forget_episode mem "ep-backend";
  Memory.forget_procedure mem "pr-backend";
  check
    bool
    "episode removed"
    true
    (Option.is_none (Memory.recall_episode mem "ep-backend"));
  check
    bool
    "procedure removed"
    true
    (Option.is_none (Memory.best_procedure mem ~pattern:"deploy"))
;;

(* ── Large-scale tests ───────────────────────────────── *)

let test_1000_keys () =
  let mem = Memory.create () in
  for i = 0 to 999 do
    ignore (Memory.store mem ~tier:Working (Printf.sprintf "key_%d" i) (json_i i))
  done;
  (* Spot-check *)
  (match Memory.recall mem ~tier:Working "key_0" with
   | Some (`Int 0) -> ()
   | _ -> fail "key_0 missing");
  (match Memory.recall mem ~tier:Working "key_999" with
   | Some (`Int 999) -> ()
   | _ -> fail "key_999 missing");
  let _, w, _, _, _ = Memory.stats mem in
  check int "1000 working entries" 1000 w
;;

let test_overwrite_preserves_latest () =
  let mem = Memory.create () in
  for i = 0 to 99 do
    ignore (Memory.store mem ~tier:Scratchpad "counter" (json_i i))
  done;
  match Memory.recall mem ~tier:Scratchpad "counter" with
  | Some (`Int 99) -> ()
  | Some (`Int n) -> failf "expected 99, got %d" n
  | _ -> fail "counter missing"
;;

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run
    "memory_advanced"
    [ ( "concurrent"
      , [ test_case "different keys" `Quick test_concurrent_different_keys
        ; test_case "same key" `Quick test_concurrent_same_key
        ; test_case "promote no deadlock" `Quick test_concurrent_promote_no_deadlock
        ] )
    ; ( "property"
      , [ test_prop_store_recall_identity ()
        ; test_prop_forget_removes ()
        ; test_prop_stats_consistent ()
        ] )
    ; ( "backend_failure"
      , [ test_case "persist error" `Quick test_backend_persist_error
        ; test_case "retrieve returns None" `Quick test_backend_retrieve_returns_none
        ; test_case "remove error" `Quick test_backend_remove_error
        ; test_case
            "fallbacks query dedupe and typed results"
            `Quick
            test_tier_fallbacks_query_dedupe_and_typed_results
        ; test_case
            "episodic and procedural backend fallbacks"
            `Quick
            test_episodic_and_procedural_backend_fallbacks
        ] )
    ; ( "large_scale"
      , [ test_case "1000 keys" `Quick test_1000_keys
        ; test_case "overwrite preserves latest" `Quick test_overwrite_preserves_latest
        ] )
    ]
;;
