(** Tests for strengthened long_term_backend: Result, batch_persist, query. *)

open Alcotest
open Agent_sdk

let json_s s = `String s
let json_i i = `Int i

(* ── Helpers ─────────────────────────────────────────── *)

(** In-memory backend with full support. *)
let make_backend () =
  let store = Hashtbl.create 16 in
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
          |> List.sort (fun (a, _) (b, _) -> String.compare a b)
          |> List.filteri (fun i _ -> i < limit))
    }
  in
  store, backend
;;

(* ── batch_persist ───────────────────────────────────── *)

let test_batch_persist_stores_all () =
  let store, backend = make_backend () in
  let mem = Memory.create ~long_term:backend () in
  ignore mem;
  let pairs = [ "k1", json_s "v1"; "k2", json_i 2; "k3", json_s "v3" ] in
  (match backend.batch_persist pairs with
   | Ok () -> ()
   | Error e -> fail (Printf.sprintf "batch_persist failed: %s" e));
  check
    (option string)
    "k1"
    (Some "\"v1\"")
    (Option.map Yojson.Safe.to_string (Hashtbl.find_opt store "k1"));
  check
    (option int)
    "k2"
    (Some 2)
    (match Hashtbl.find_opt store "k2" with
     | Some (`Int n) -> Some n
     | _ -> None);
  check
    (option string)
    "k3"
    (Some "\"v3\"")
    (Option.map Yojson.Safe.to_string (Hashtbl.find_opt store "k3"))
;;

let test_batch_persist_empty () =
  let _store, backend = make_backend () in
  match backend.batch_persist [] with
  | Ok () -> ()
  | Error e -> fail (Printf.sprintf "empty batch failed: %s" e)
;;

let test_batch_persist_error () =
  let fail_backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _v -> Ok ())
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _pairs -> Error "batch write failed")
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  match fail_backend.batch_persist [ "a", json_s "b" ] with
  | Error "batch write failed" -> ()
  | Ok () -> fail "expected error"
  | Error other -> fail (Printf.sprintf "unexpected error: %s" other)
;;

let test_batch_persist_overwrites () =
  let store, backend = make_backend () in
  Hashtbl.replace store "k1" (json_i 1);
  (match backend.batch_persist [ "k1", json_i 99 ] with
   | Ok () -> ()
   | Error e -> fail e);
  check
    int
    "overwritten"
    99
    (match Hashtbl.find_opt store "k1" with
     | Some (`Int n) -> n
     | _ -> -1)
;;

(* ── query ───────────────────────────────────────────── *)

let test_query_prefix_match () =
  let store, backend = make_backend () in
  Hashtbl.replace store "user:alice" (json_s "a");
  Hashtbl.replace store "user:bob" (json_s "b");
  Hashtbl.replace store "session:1" (json_s "s");
  let results = backend.query ~prefix:"user:" ~limit:10 in
  check int "2 user keys" 2 (List.length results);
  let keys = List.map fst results |> List.sort String.compare in
  check (list string) "sorted keys" [ "user:alice"; "user:bob" ] keys
;;

let test_query_limit () =
  let store, backend = make_backend () in
  for i = 0 to 9 do
    Hashtbl.replace store (Printf.sprintf "item:%02d" i) (json_i i)
  done;
  let results = backend.query ~prefix:"item:" ~limit:3 in
  check int "limited to 3" 3 (List.length results)
;;

let test_query_no_match () =
  let store, backend = make_backend () in
  Hashtbl.replace store "alpha" (json_s "a");
  let results = backend.query ~prefix:"beta" ~limit:10 in
  check int "no match" 0 (List.length results)
;;

let test_query_empty_prefix () =
  let store, backend = make_backend () in
  Hashtbl.replace store "a" (json_i 1);
  Hashtbl.replace store "b" (json_i 2);
  let results = backend.query ~prefix:"" ~limit:100 in
  check int "all keys" 2 (List.length results)
;;

(* ── typed retrieve_result ───────────────────────────── *)

let test_recall_exact_result_success_and_missing () =
  let store, backend = make_backend () in
  let mem = Memory.create ~long_term:backend () in
  Hashtbl.replace store "present" (json_s "value");
  (match Memory.recall_exact_result mem ~tier:Long_term "present" with
   | Ok (`String "value") -> ()
   | Ok other -> fail ("unexpected value: " ^ Yojson.Safe.to_string other)
   | Error err -> fail ("unexpected error: " ^ Memory.retrieve_error_to_string err));
  match Memory.recall_exact_result mem ~tier:Long_term "missing" with
  | Error Memory.Missing_key -> ()
  | Ok _ -> fail "expected missing key"
  | Error err -> fail ("unexpected error: " ^ Memory.retrieve_error_to_string err)
;;

let test_recall_result_surfaces_backend_error () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _v -> Ok ())
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _ -> Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem =
    Memory.create
      ~long_term:backend
      ~long_term_retrieve_result:(fun ~key:_ ->
        Error (Memory.Backend_error "corrupt_json: invalid"))
      ()
  in
  match Memory.recall_result mem ~tier:Working "falls-through" with
  | Error (Memory.Backend_error reason) ->
    check string "backend error reason" "corrupt_json: invalid" reason
  | Ok _ -> fail "expected backend error"
  | Error err -> fail ("unexpected error: " ^ Memory.retrieve_error_to_string err)
;;

(* ── persist error propagation ───────────────────────── *)

let test_persist_error_propagates () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _v -> Error "write denied")
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Ok ())
    ; batch_persist = (fun _ -> Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  match Memory.store mem ~tier:Long_term "x" (json_s "y") with
  | Error reason ->
    check
      bool
      "reason contains write denied"
      true
      (String.length reason > 0
       &&
       let needle = "write denied" in
       try
         for i = 0 to String.length reason - String.length needle do
           if String.sub reason i (String.length needle) = needle then raise Exit
         done;
         false
       with
       | Exit -> true)
  | Ok () -> fail "expected Error from store"
;;

let test_remove_error_propagates () =
  let backend : Memory.long_term_backend =
    { persist = (fun ~key:_ _v -> Ok ())
    ; retrieve = (fun ~key:_ -> None)
    ; remove = (fun ~key:_ -> Error "delete denied")
    ; batch_persist = (fun _ -> Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  ignore (Memory.store mem ~tier:Long_term "x" (json_s "y"));
  match Memory.forget mem ~tier:Long_term "x" with
  | Error _ -> ()
  | Ok () -> fail "expected Error from forget"
;;

(* ── Suite ───────────────────────────────────────────── *)

let () =
  run
    "memory_lt_backend"
    [ ( "batch_persist"
      , [ test_case "stores all" `Quick test_batch_persist_stores_all
        ; test_case "empty batch" `Quick test_batch_persist_empty
        ; test_case "batch error" `Quick test_batch_persist_error
        ; test_case "overwrites" `Quick test_batch_persist_overwrites
        ] )
    ; ( "query"
      , [ test_case "prefix match" `Quick test_query_prefix_match
        ; test_case "limit" `Quick test_query_limit
        ; test_case "no match" `Quick test_query_no_match
        ; test_case "empty prefix" `Quick test_query_empty_prefix
        ] )
    ; ( "retrieve_result"
      , [ test_case
            "success and missing"
            `Quick
            test_recall_exact_result_success_and_missing
        ; test_case "backend error" `Quick test_recall_result_surfaces_backend_error
        ] )
    ; ( "error_propagation"
      , [ test_case "persist error" `Quick test_persist_error_propagates
        ; test_case "remove error" `Quick test_remove_error_propagates
        ] )
    ]
;;
