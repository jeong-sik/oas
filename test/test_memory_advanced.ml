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
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let mem = Memory.create () in
  Eio.Fiber.both
    (fun () -> Memory.store mem ~tier:Working "key_a" (json_s "val_a"))
    (fun () -> Memory.store mem ~tier:Working "key_b" (json_s "val_b"));
  ignore sw;
  (match Memory.recall mem ~tier:Working "key_a" with
   | Some (`String "val_a") -> ()
   | _ -> fail "key_a missing");
  (match Memory.recall mem ~tier:Working "key_b" with
   | Some (`String "val_b") -> ()
   | _ -> fail "key_b missing")

let test_concurrent_same_key () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let mem = Memory.create () in
  Eio.Fiber.both
    (fun () -> Memory.store mem ~tier:Working "key" (json_i 1))
    (fun () -> Memory.store mem ~tier:Working "key" (json_i 2));
  ignore sw;
  (* Last write wins; just verify no crash and some value is present *)
  (match Memory.recall mem ~tier:Working "key" with
   | Some (`Int n) -> check bool "value is 1 or 2" true (n = 1 || n = 2)
   | _ -> fail "key missing after concurrent write")

let test_concurrent_promote_no_deadlock () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  let mem = Memory.create () in
  Memory.store mem ~tier:Scratchpad "promo" (json_i 42);
  Memory.store mem ~tier:Scratchpad "keep" (json_s "here");
  Eio.Fiber.both
    (fun () -> let _ = Memory.promote mem "promo" in ())
    (fun () -> Memory.store mem ~tier:Working "other" (json_s "val"));
  ignore sw;
  (* Promoted key should be in Working *)
  (match Memory.recall_exact mem ~tier:Working "promo" with
   | Some (`Int 42) -> ()
   | _ -> fail "promote failed")

(* ── Property-based tests (QCheck) ───────────────────── *)

let test_prop_store_recall_identity () =
  let prop =
    QCheck.Test.make ~count:100 ~name:"store/recall identity"
      QCheck.(pair string string)
      (fun (key, value) ->
         let mem = Memory.create () in
         Memory.store mem ~tier:Scratchpad key (json_s value);
         Memory.recall mem ~tier:Scratchpad key = Some (json_s value))
  in
  QCheck_alcotest.to_alcotest prop

let test_prop_forget_removes () =
  let prop =
    QCheck.Test.make ~count:100 ~name:"forget removes key"
      QCheck.string
      (fun key ->
         let mem = Memory.create () in
         Memory.store mem ~tier:Working key (json_i 1);
         Memory.forget mem ~tier:Working key;
         Option.is_none (Memory.recall_exact mem ~tier:Working key))
  in
  QCheck_alcotest.to_alcotest prop

let test_prop_stats_consistent () =
  let prop =
    QCheck.Test.make ~count:50 ~name:"stats consistent"
      QCheck.(list_size (Gen.int_range 0 20) string)
      (fun keys ->
         let mem = Memory.create () in
         List.iter (fun k ->
           Memory.store mem ~tier:Scratchpad k (json_i 1)) keys;
         let unique_keys =
           List.sort_uniq String.compare keys |> List.length in
         let (s, _, _) = Memory.stats mem in
         s = unique_keys)
  in
  QCheck_alcotest.to_alcotest prop

(* ── Long-term backend failure handling ──────────────── *)

let test_backend_persist_raises () =
  let backend : Memory.long_term_backend = {
    persist = (fun ~key:_ _value -> raise (Failure "disk full"));
    retrieve = (fun ~key:_ -> None);
    remove = (fun ~key:_ -> ());
  } in
  let mem = Memory.create ~long_term:backend () in
  (* Persist failure should propagate *)
  let raised = ref false in
  (try Memory.store mem ~tier:Long_term "key" (json_s "val")
   with Failure _ -> raised := true);
  check bool "persist raises" true !raised

let test_backend_retrieve_returns_none () =
  let backend : Memory.long_term_backend = {
    persist = (fun ~key:_ _value -> ());
    retrieve = (fun ~key:_ -> None);
    remove = (fun ~key:_ -> ());
  } in
  let mem = Memory.create ~long_term:backend () in
  Memory.store mem ~tier:Long_term "key" (json_s "val");
  (* Backend returns None — local cache may or may not have it *)
  let result = Memory.recall mem ~tier:Long_term "key" in
  (* No crash is the main assertion *)
  ignore result

let test_backend_remove_raises () =
  let backend : Memory.long_term_backend = {
    persist = (fun ~key:_ _value -> ());
    retrieve = (fun ~key:_ -> None);
    remove = (fun ~key:_ -> raise (Failure "no perms"));
  } in
  let mem = Memory.create ~long_term:backend () in
  Memory.store mem ~tier:Long_term "key" (json_s "val");
  let raised = ref false in
  (try Memory.forget mem ~tier:Long_term "key"
   with Failure _ -> raised := true);
  check bool "remove raises" true !raised

(* ── Large-scale tests ───────────────────────────────── *)

let test_1000_keys () =
  let mem = Memory.create () in
  for i = 0 to 999 do
    Memory.store mem ~tier:Working
      (Printf.sprintf "key_%d" i) (json_i i)
  done;
  (* Spot-check *)
  (match Memory.recall mem ~tier:Working "key_0" with
   | Some (`Int 0) -> ()
   | _ -> fail "key_0 missing");
  (match Memory.recall mem ~tier:Working "key_999" with
   | Some (`Int 999) -> ()
   | _ -> fail "key_999 missing");
  let (_, w, _) = Memory.stats mem in
  check int "1000 working entries" 1000 w

let test_overwrite_preserves_latest () =
  let mem = Memory.create () in
  for i = 0 to 99 do
    Memory.store mem ~tier:Scratchpad "counter" (json_i i)
  done;
  match Memory.recall mem ~tier:Scratchpad "counter" with
  | Some (`Int 99) -> ()
  | Some (`Int n) -> failf "expected 99, got %d" n
  | _ -> fail "counter missing"

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run "memory_advanced" [
    "concurrent", [
      test_case "different keys" `Quick test_concurrent_different_keys;
      test_case "same key" `Quick test_concurrent_same_key;
      test_case "promote no deadlock" `Quick
        test_concurrent_promote_no_deadlock;
    ];
    "property", [
      test_prop_store_recall_identity ();
      test_prop_forget_removes ();
      test_prop_stats_consistent ();
    ];
    "backend_failure", [
      test_case "persist raises" `Quick test_backend_persist_raises;
      test_case "retrieve returns None" `Quick
        test_backend_retrieve_returns_none;
      test_case "remove raises" `Quick test_backend_remove_raises;
    ];
    "large_scale", [
      test_case "1000 keys" `Quick test_1000_keys;
      test_case "overwrite preserves latest" `Quick
        test_overwrite_preserves_latest;
    ];
  ]
