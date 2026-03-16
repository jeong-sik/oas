(** Tests for Log module — structured logging. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────────── *)

let log = Log.create ~module_name:"test" ()

let setup () =
  Log.clear_sinks ();
  Log.set_global_level Debug

(* ── Tests ────────────────────────────────────────────────────── *)

let test_level_roundtrip () =
  let levels = [Log.Debug; Log.Info; Log.Warn; Log.Error] in
  List.iter (fun level ->
    let s = Log.level_to_string level in
    match Log.level_of_string s with
    | Ok l ->
      Alcotest.(check string) "roundtrip"
        (Log.show_level level) (Log.show_level l)
    | Error msg -> Alcotest.fail msg
  ) levels

let test_level_yojson_roundtrip () =
  let levels = [Log.Debug; Log.Info; Log.Warn; Log.Error] in
  List.iter (fun level ->
    let json = Log.level_to_yojson level in
    match Log.level_of_yojson json with
    | Ok l ->
      Alcotest.(check string) "yojson roundtrip"
        (Log.show_level level) (Log.show_level l)
    | Error msg -> Alcotest.fail msg
  ) levels

let test_level_of_string_error () =
  match Log.level_of_string "invalid" with
  | Ok _ -> Alcotest.fail "expected error"
  | Error _ -> ()

let test_collector_sink () =
  setup ();
  let (sink, get) = Log.collector_sink () in
  Log.add_sink sink;
  Log.info log "hello" [Log.S ("key", "val")];
  let records = get () in
  Alcotest.(check int) "one record" 1 (List.length records);
  let r = List.hd records in
  Alcotest.(check string) "message" "hello" r.message;
  Alcotest.(check string) "module" "test" r.module_name

let test_level_filtering () =
  setup ();
  Log.set_global_level Warn;
  let (sink, get) = Log.collector_sink () in
  Log.add_sink sink;
  Log.debug log "skipped" [];
  Log.info log "skipped" [];
  Log.warn log "included" [];
  Log.error log "included" [];
  let records = get () in
  Alcotest.(check int) "two records" 2 (List.length records)

let test_field_to_json () =
  let cases = [
    (Log.S ("name", "alice"), ("name", `String "alice"));
    (Log.I ("count", 42), ("count", `Int 42));
    (Log.F ("ratio", 0.5), ("ratio", `Float 0.5));
    (Log.B ("ok", true), ("ok", `Bool true));
    (Log.J ("data", `List [`Int 1]), ("data", `List [`Int 1]));
  ] in
  List.iter (fun (field, (ek, ev)) ->
    let (k, v) = Log.field_to_json field in
    Alcotest.(check string) "key" ek k;
    Alcotest.(check string) "value"
      (Yojson.Safe.to_string ev)
      (Yojson.Safe.to_string v)
  ) cases

let test_record_to_json () =
  let r : Log.record = {
    ts = 1700000000.0;
    level = Info;
    module_name = "api";
    message = "request";
    fields = [Log.I ("status", 200)];
    trace_id = Some "abc123";
    span_id = None;
  } in
  let json = Log.record_to_json r in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "msg" "request" (json |> member "msg" |> to_string);
  Alcotest.(check string) "level" "info" (json |> member "level" |> to_string);
  Alcotest.(check string) "module" "api" (json |> member "module" |> to_string);
  Alcotest.(check int) "status" 200 (json |> member "status" |> to_int);
  Alcotest.(check string) "trace_id" "abc123" (json |> member "trace_id" |> to_string)

let test_multiple_sinks () =
  setup ();
  let (sink1, get1) = Log.collector_sink () in
  let (sink2, get2) = Log.collector_sink () in
  Log.add_sink sink1;
  Log.add_sink sink2;
  Log.info log "fanout" [];
  Alcotest.(check int) "sink1" 1 (List.length (get1 ()));
  Alcotest.(check int) "sink2" 1 (List.length (get2 ()))

let test_with_trace_context () =
  setup ();
  let (sink, get) = Log.collector_sink () in
  Log.add_sink sink;
  let traced = Log.with_trace_id log ~trace_id:"t1" in
  let traced = Log.with_span_id traced ~span_id:"s1" in
  Log.info traced "traced" [];
  let records = get () in
  let r = List.hd records in
  Alcotest.(check (option string)) "trace_id" (Some "t1") r.trace_id;
  Alcotest.(check (option string)) "span_id" (Some "s1") r.span_id

let test_empty_fields () =
  setup ();
  let (sink, get) = Log.collector_sink () in
  Log.add_sink sink;
  Log.info log "bare" [];
  let records = get () in
  let r = List.hd records in
  Alcotest.(check int) "no fields" 0 (List.length r.fields)

let test_all_levels () =
  setup ();
  let (sink, get) = Log.collector_sink () in
  Log.add_sink sink;
  Log.debug log "d" [];
  Log.info  log "i" [];
  Log.warn  log "w" [];
  Log.error log "e" [];
  let records = get () in
  Alcotest.(check int) "four records" 4 (List.length records);
  let messages = List.map (fun (r : Log.record) -> r.message) records in
  Alcotest.(check (list string)) "order" ["d"; "i"; "w"; "e"] messages

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run "Log" [
    "level", [
      Alcotest.test_case "roundtrip" `Quick test_level_roundtrip;
      Alcotest.test_case "yojson roundtrip" `Quick test_level_yojson_roundtrip;
      Alcotest.test_case "error on invalid" `Quick test_level_of_string_error;
    ];
    "sink", [
      Alcotest.test_case "collector" `Quick test_collector_sink;
      Alcotest.test_case "level filtering" `Quick test_level_filtering;
      Alcotest.test_case "multiple sinks" `Quick test_multiple_sinks;
    ];
    "field", [
      Alcotest.test_case "to_json" `Quick test_field_to_json;
    ];
    "record", [
      Alcotest.test_case "to_json" `Quick test_record_to_json;
      Alcotest.test_case "empty fields" `Quick test_empty_fields;
    ];
    "context", [
      Alcotest.test_case "trace/span" `Quick test_with_trace_context;
    ];
    "integration", [
      Alcotest.test_case "all levels" `Quick test_all_levels;
    ];
  ]
