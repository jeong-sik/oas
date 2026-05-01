open Base
(** Unit tests for Audit module (v0.76.0). *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let make_entry
      ?(id = "e1")
      ?(ts = 1000.0)
      ?(agent = "agent-a")
      ?(action = "tool_call")
      ?(dp = None)
      ?(verdict = None)
      ?(detail = `Null)
      ()
  : Audit.entry
  =
  { id; timestamp = ts; agent_name = agent; action; decision_point = dp; verdict; detail }
;;

(* ── Basic record/count ──────────────────────────── *)

let test_record_and_count () =
  let log = Audit.create () in
  check int "empty" 0 (Audit.count log);
  Audit.record log (make_entry ());
  check int "one entry" 1 (Audit.count log);
  Audit.record log (make_entry ~id:"e2" ~ts:2000.0 ());
  check int "two entries" 2 (Audit.count log)
;;

(* ── Query by agent ──────────────────────────────── *)

let test_query_by_agent () =
  let log = Audit.create () in
  Audit.record log (make_entry ~agent:"alice" ());
  Audit.record log (make_entry ~id:"e2" ~agent:"bob" ());
  Audit.record log (make_entry ~id:"e3" ~agent:"alice" ());
  let alice = Audit.query log ~agent:"alice" () in
  check int "alice entries" 2 (List.length alice);
  let bob = Audit.query log ~agent:"bob" () in
  check int "bob entries" 1 (List.length bob)
;;

(* ── Query by action ─────────────────────────────── *)

let test_query_by_action () =
  let log = Audit.create () in
  Audit.record log (make_entry ~action:"tool_call" ());
  Audit.record log (make_entry ~id:"e2" ~action:"handoff" ());
  Audit.record log (make_entry ~id:"e3" ~action:"tool_call" ());
  let tool_calls = Audit.query log ~action:"tool_call" () in
  check int "tool_call entries" 2 (List.length tool_calls)
;;

(* ── Query by since ──────────────────────────────── *)

let test_query_by_since () =
  let log = Audit.create () in
  Audit.record log (make_entry ~ts:100.0 ());
  Audit.record log (make_entry ~id:"e2" ~ts:200.0 ());
  Audit.record log (make_entry ~id:"e3" ~ts:300.0 ());
  let recent = Audit.query log ~since:200.0 () in
  check int "since 200" 2 (List.length recent)
;;

(* ── Combined query ──────────────────────────────── *)

let test_query_combined () =
  let log = Audit.create () in
  Audit.record log (make_entry ~agent:"alice" ~action:"tool_call" ~ts:100.0 ());
  Audit.record log (make_entry ~id:"e2" ~agent:"alice" ~action:"handoff" ~ts:200.0 ());
  Audit.record log (make_entry ~id:"e3" ~agent:"bob" ~action:"tool_call" ~ts:300.0 ());
  let r = Audit.query log ~agent:"alice" ~action:"tool_call" () in
  check int "alice + tool_call" 1 (List.length r)
;;

(* ── Latest ──────────────────────────────────────── *)

let test_latest () =
  let log = Audit.create () in
  Audit.record log (make_entry ~id:"e1" ~ts:100.0 ());
  Audit.record log (make_entry ~id:"e2" ~ts:200.0 ());
  Audit.record log (make_entry ~id:"e3" ~ts:300.0 ());
  let top2 = Audit.latest log 2 in
  check int "latest 2" 2 (List.length top2);
  (* Newest first — e3 should be first *)
  let ids = List.map (fun (e : Audit.entry) -> e.id) top2 in
  check (list string) "order" [ "e3"; "e2" ] ids
;;

(* ── Max capacity eviction ───────────────────────── *)

let test_max_capacity () =
  let log = Audit.create ~max_entries:3 () in
  Audit.record log (make_entry ~id:"e1" ~ts:100.0 ());
  Audit.record log (make_entry ~id:"e2" ~ts:200.0 ());
  Audit.record log (make_entry ~id:"e3" ~ts:300.0 ());
  check int "at capacity" 3 (Audit.count log);
  (* Add one more — oldest should be evicted *)
  Audit.record log (make_entry ~id:"e4" ~ts:400.0 ());
  check int "still 3" 3 (Audit.count log);
  (* e1 (oldest) should be gone, e4 (newest) should be present *)
  let ids = List.map (fun (e : Audit.entry) -> e.id) (Audit.query log ()) in
  check bool "e1 evicted" true (not (List.mem "e1" ids));
  check bool "e4 present" true (List.mem "e4" ids)
;;

let test_no_max_unlimited () =
  let log = Audit.create () in
  for i = 1 to 100 do
    Audit.record log (make_entry ~id:(string_of_int i) ~ts:(float_of_int i) ())
  done;
  check int "100 entries" 100 (Audit.count log)
;;

(* ── Export to JSON ──────────────────────────────── *)

let test_to_json () =
  let dp = Policy.BeforeToolCall { tool_name = "search"; agent_name = "alice" } in
  let log = Audit.create () in
  Audit.record
    log
    (make_entry
       ~dp:(Some dp)
       ~verdict:(Some Policy.Allow)
       ~detail:(`String "searched")
       ());
  let json = Audit.to_json log in
  match json with
  | `List [ entry ] ->
    let open Yojson.Safe.Util in
    check string "agent" "agent-a" (entry |> member "agent_name" |> to_string);
    check string "action" "tool_call" (entry |> member "action" |> to_string);
    (* verdict should be present *)
    let v = entry |> member "verdict" |> to_string in
    check string "verdict" "Allow" v
  | _ -> fail "expected single-entry list"
;;

let test_entries_to_json () =
  let entries = [ make_entry ~id:"e1" (); make_entry ~id:"e2" ~agent:"bob" () ] in
  match Audit.entries_to_json entries with
  | `List items -> check int "2 items" 2 (List.length items)
  | _ -> fail "expected list"
;;

(* ── Empty queries ───────────────────────────────── *)

let test_query_empty_log () =
  let log = Audit.create () in
  check int "empty query" 0 (List.length (Audit.query log ()));
  check int "empty latest" 0 (List.length (Audit.latest log 10))
;;

(* ── Suite ────────────────────────────────────────── *)

let () =
  run
    "audit"
    [ "basic", [ test_case "record and count" `Quick test_record_and_count ]
    ; ( "query"
      , [ test_case "by agent" `Quick test_query_by_agent
        ; test_case "by action" `Quick test_query_by_action
        ; test_case "by since" `Quick test_query_by_since
        ; test_case "combined" `Quick test_query_combined
        ; test_case "empty log" `Quick test_query_empty_log
        ] )
    ; "latest", [ test_case "latest entries" `Quick test_latest ]
    ; ( "capacity"
      , [ test_case "max capacity eviction" `Quick test_max_capacity
        ; test_case "no max unlimited" `Quick test_no_max_unlimited
        ] )
    ; ( "export"
      , [ test_case "to_json" `Quick test_to_json
        ; test_case "entries_to_json" `Quick test_entries_to_json
        ] )
    ]
;;
