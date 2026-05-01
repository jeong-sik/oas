open Base
(** Unit tests for Memory module (v0.65.0). *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let json_s s = `String s
let json_i i = `Int i

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
        ; test_case "stats" `Quick test_stats
        ] )
    ; ( "long_term"
      , [ test_case "backend persist/retrieve/remove" `Quick test_long_term_backend
        ; test_case
            "set backend after create"
            `Quick
            test_long_term_backend_set_after_create
        ; test_case "query prefix" `Quick test_query_long_term_prefix
        ] )
    ; "context", [ test_case "context access" `Quick test_context_access ]
    ]
;;
