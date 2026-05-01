(** Tests for Sse_parser — SSE line-level parser. *)

open Alcotest
open Agent_sdk
module Sse_parser = Llm_provider.Sse_parser

let parse_string input =
  let events = ref [] in
  let reader =
    Eio_main.run
    @@ fun _env ->
    let flow = Eio.Flow.string_source input in
    let buf_reader = Eio.Buf_read.of_flow ~max_size:(1024 * 1024) flow in
    Sse_parser.parse_lines buf_reader ~on_event:(fun evt -> events := evt :: !events);
    !events
  in
  List.rev reader
;;

(* ── Basic event parsing ────────────────────────────────── *)

let test_simple_data () =
  let events = parse_string "data: hello world\n\n" in
  match events with
  | [ evt ] ->
    check (option string) "no event type" None evt.event_type;
    check string "data" "hello world" evt.data
  | _ -> fail (Printf.sprintf "expected 1 event, got %d" (List.length events))
;;

let test_event_type () =
  let events = parse_string "event: message\ndata: payload\n\n" in
  match events with
  | [ evt ] ->
    check (option string) "event type" (Some "message") evt.event_type;
    check string "data" "payload" evt.data
  | _ -> fail "expected 1 event"
;;

let test_multiline_data () =
  let events = parse_string "data: line1\ndata: line2\ndata: line3\n\n" in
  match events with
  | [ evt ] -> check string "joined data" "line1\nline2\nline3" evt.data
  | _ -> fail "expected 1 event"
;;

let test_id_field () =
  let events = parse_string "id: evt-42\ndata: test\n\n" in
  match events with
  | [ evt ] ->
    check (option string) "id" (Some "evt-42") evt.id;
    check string "data" "test" evt.data
  | _ -> fail "expected 1 event"
;;

let test_retry_field () =
  let events = parse_string "retry: 5000\ndata: test\n\n" in
  match events with
  | [ evt ] ->
    check (option int) "retry" (Some 5000) evt.retry;
    check string "data" "test" evt.data
  | _ -> fail "expected 1 event"
;;

(* ── Multiple events ────────────────────────────────────── *)

let test_multiple_events () =
  let events = parse_string "data: first\n\ndata: second\n\n" in
  check int "count" 2 (List.length events);
  check string "first" "first" (List.nth events 0).data;
  check string "second" "second" (List.nth events 1).data
;;

(* ── Comments and empty lines ───────────────────────────── *)

let test_comment_ignored () =
  let events = parse_string ": this is a comment\ndata: actual\n\n" in
  match events with
  | [ evt ] -> check string "data" "actual" evt.data
  | _ -> fail "expected 1 event"
;;

let test_empty_data_skipped () =
  let events = parse_string "\n\n" in
  check int "no events from empty" 0 (List.length events)
;;

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run
    "Sse_parser"
    [ ( "basic"
      , [ test_case "simple data" `Quick test_simple_data
        ; test_case "event type" `Quick test_event_type
        ; test_case "multiline data" `Quick test_multiline_data
        ; test_case "id field" `Quick test_id_field
        ; test_case "retry field" `Quick test_retry_field
        ] )
    ; "multiple", [ test_case "multiple events" `Quick test_multiple_events ]
    ; ( "edge cases"
      , [ test_case "comment ignored" `Quick test_comment_ignored
        ; test_case "empty data skipped" `Quick test_empty_data_skipped
        ] )
    ]
;;
