(** Tests for otel_tracer.ml — OpenTelemetry-compatible tracer. *)

open Alcotest
open Agent_sdk

(* -- Helpers ------------------------------------------------------------ *)

let default_attrs ?(name = "test_op") ?(agent = "test-agent") ?(turn = 1)
    ?(kind = Tracing.Agent_run) ?(extra = []) () : Tracing.span_attrs =
  { kind; name; agent_name = agent; turn; extra }

let is_hex s =
  String.length s > 0
  && String.to_seq s
     |> Seq.for_all (fun c ->
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))

let json_assoc_field key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

(* ── Span Lifecycle ──────────────────────────────────────────────── *)

let test_start_span_name_format () =
  Otel_tracer.reset ();
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ~name:"chat" ())
  in
  check string "name is kind/name" "api_call/chat" span.name;
  Otel_tracer.end_span span ~ok:true

let test_start_span_attributes () =
  Otel_tracer.reset ();
  let span =
    Otel_tracer.start_span
      (default_attrs ~agent:"my-agent" ~turn:3 ~name:"do_thing"
         ~extra:[ ("custom_key", "custom_val") ] ())
  in
  let attrs = span.attributes in
  check bool "has gen_ai.agent.name" true
    (List.mem ("gen_ai.agent.name", "my-agent") attrs);
  check bool "has gen_ai.turn" true
    (List.mem ("gen_ai.turn", "3") attrs);
  check bool "has gen_ai.operation.name" true
    (List.mem ("gen_ai.operation.name", "do_thing") attrs);
  check bool "has extra attr" true
    (List.mem ("custom_key", "custom_val") attrs);
  Otel_tracer.end_span span ~ok:true

let test_start_span_kind_mapping () =
  Otel_tracer.reset ();
  let span =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Api_call ())
  in
  check int "Api_call maps to Client (3)"
    3 (Otel_tracer.otel_span_kind_to_int span.kind);
  Otel_tracer.end_span span ~ok:true;
  Otel_tracer.reset ();
  let span2 =
    Otel_tracer.start_span (default_attrs ~kind:Tracing.Agent_run ())
  in
  check int "Agent_run maps to Internal (1)"
    1 (Otel_tracer.otel_span_kind_to_int span2.kind);
  Otel_tracer.end_span span2 ~ok:true

let test_end_span_sets_status () =
  Otel_tracer.reset ();
  let span_ok = Otel_tracer.start_span (default_attrs ~name:"ok_op" ()) in
  Otel_tracer.end_span span_ok ~ok:true;
  check (option bool) "ok=true sets Some true" (Some true) span_ok.status;
  check bool "end_time_ns is set" true (span_ok.end_time_ns <> None);
  let span_err = Otel_tracer.start_span (default_attrs ~name:"err_op" ()) in
  Otel_tracer.end_span span_err ~ok:false;
  check (option bool) "ok=false sets Some false" (Some false) span_err.status

let test_end_span_moves_to_completed () =
  Otel_tracer.reset ();
  let span = Otel_tracer.start_span (default_attrs ()) in
  check int "active=1 after start" 1 (Otel_tracer.active_count ());
  check int "completed=0 after start" 0 (Otel_tracer.completed_count ());
  Otel_tracer.end_span span ~ok:true;
  check int "active=0 after end" 0 (Otel_tracer.active_count ());
  check int "completed=1 after end" 1 (Otel_tracer.completed_count ())

let test_start_span_hex_ids () =
  Otel_tracer.reset ();
  let span = Otel_tracer.start_span (default_attrs ()) in
  check int "trace_id is 32 chars" 32 (String.length span.trace_id);
  check int "span_id is 16 chars" 16 (String.length span.span_id);
  check bool "trace_id is valid hex" true (is_hex span.trace_id);
  check bool "span_id is valid hex" true (is_hex span.span_id);
  Otel_tracer.end_span span ~ok:true

(* ── Span Hierarchy ──────────────────────────────────────────────── *)

let test_child_inherits_trace_id () =
  Otel_tracer.reset ();
  let parent = Otel_tracer.start_span (default_attrs ~name:"parent" ()) in
  let child = Otel_tracer.start_span (default_attrs ~name:"child" ()) in
  check string "child inherits parent trace_id"
    parent.trace_id child.trace_id;
  Otel_tracer.end_span child ~ok:true;
  Otel_tracer.end_span parent ~ok:true

let test_child_has_parent_span_id () =
  Otel_tracer.reset ();
  let parent = Otel_tracer.start_span (default_attrs ~name:"parent" ()) in
  let child = Otel_tracer.start_span (default_attrs ~name:"child" ()) in
  check (option string) "child parent_span_id = parent span_id"
    (Some parent.span_id) child.parent_span_id;
  Otel_tracer.end_span child ~ok:true;
  Otel_tracer.end_span parent ~ok:true

let test_root_span_no_parent () =
  Otel_tracer.reset ();
  let root = Otel_tracer.start_span (default_attrs ~name:"root" ()) in
  check (option string) "root has no parent_span_id"
    None root.parent_span_id;
  Otel_tracer.end_span root ~ok:true

(* ── Events & Attributes ────────────────────────────────────────── *)

let test_add_event () =
  Otel_tracer.reset ();
  let span = Otel_tracer.start_span (default_attrs ()) in
  Otel_tracer.add_event span "something happened";
  check int "events list has 1 entry" 1 (List.length span.events);
  let evt = List.hd span.events in
  check string "event name" "something happened" evt.event_name;
  check bool "timestamp > 0" true (Int64.compare evt.timestamp_ns 0L > 0);
  Otel_tracer.end_span span ~ok:true

let test_add_attrs () =
  Otel_tracer.reset ();
  let span = Otel_tracer.start_span (default_attrs ()) in
  let initial_len = List.length span.attributes in
  Otel_tracer.add_attrs span [ ("extra_k", "extra_v") ];
  check int "attrs grew by 1" (initial_len + 1) (List.length span.attributes);
  check bool "contains new attr" true
    (List.mem ("extra_k", "extra_v") span.attributes);
  Otel_tracer.end_span span ~ok:true

let test_multiple_events_order () =
  Otel_tracer.reset ();
  let span = Otel_tracer.start_span (default_attrs ()) in
  Otel_tracer.add_event span "first";
  Otel_tracer.add_event span "second";
  Otel_tracer.add_event span "third";
  check int "3 events" 3 (List.length span.events);
  let names = List.map (fun (e : Otel_tracer.otel_event) -> e.event_name) span.events in
  check (list string) "insertion order preserved"
    [ "first"; "second"; "third" ] names;
  Otel_tracer.end_span span ~ok:true

(* ── Buffer Management ───────────────────────────────────────────── *)

let test_flush_returns_completed () =
  Otel_tracer.reset ();
  let s1 = Otel_tracer.start_span (default_attrs ~name:"s1" ()) in
  Otel_tracer.end_span s1 ~ok:true;
  let s2 = Otel_tracer.start_span (default_attrs ~name:"s2" ()) in
  Otel_tracer.end_span s2 ~ok:true;
  let flushed = Otel_tracer.flush () in
  check int "flush returns 2 spans" 2 (List.length flushed);
  check int "completed_count is 0 after flush" 0 (Otel_tracer.completed_count ())

let test_reset_clears_all () =
  Otel_tracer.reset ();
  let s = Otel_tracer.start_span (default_attrs ~name:"active" ()) in
  let s2 = Otel_tracer.start_span (default_attrs ~name:"done" ()) in
  Otel_tracer.end_span s2 ~ok:true;
  check bool "has active+completed" true
    (Otel_tracer.active_count () > 0 || Otel_tracer.completed_count () > 0);
  Otel_tracer.reset ();
  check int "active=0 after reset" 0 (Otel_tracer.active_count ());
  check int "completed=0 after reset" 0 (Otel_tracer.completed_count ());
  ignore s

let test_counts () =
  Otel_tracer.reset ();
  check int "initially active=0" 0 (Otel_tracer.active_count ());
  check int "initially completed=0" 0 (Otel_tracer.completed_count ());
  let s1 = Otel_tracer.start_span (default_attrs ~name:"a" ()) in
  let s2 = Otel_tracer.start_span (default_attrs ~name:"b" ()) in
  check int "active=2" 2 (Otel_tracer.active_count ());
  Otel_tracer.end_span s2 ~ok:true;
  check int "active=1" 1 (Otel_tracer.active_count ());
  check int "completed=1" 1 (Otel_tracer.completed_count ());
  Otel_tracer.end_span s1 ~ok:true;
  check int "active=0" 0 (Otel_tracer.active_count ());
  check int "completed=2" 2 (Otel_tracer.completed_count ())

let test_double_flush () =
  Otel_tracer.reset ();
  let s = Otel_tracer.start_span (default_attrs ()) in
  Otel_tracer.end_span s ~ok:true;
  let first = Otel_tracer.flush () in
  check int "first flush has 1" 1 (List.length first);
  let second = Otel_tracer.flush () in
  check int "second flush is empty" 0 (List.length second)

(* ── JSON Serialization ──────────────────────────────────────────── *)

let test_span_to_json_fields () =
  Otel_tracer.reset ();
  let span = Otel_tracer.start_span (default_attrs ~name:"json_test" ()) in
  Otel_tracer.end_span span ~ok:true;
  let json = Otel_tracer.span_to_json span in
  let required_fields =
    [ "traceId"; "spanId"; "name"; "kind";
      "startTimeUnixNano"; "endTimeUnixNano";
      "status"; "attributes"; "events" ]
  in
  List.iter (fun field ->
    check bool (Printf.sprintf "has field '%s'" field) true
      (json_assoc_field field json <> None)
  ) required_fields

let test_to_otlp_json_structure () =
  Otel_tracer.reset ();
  let s = Otel_tracer.start_span (default_attrs ~name:"otlp_test" ()) in
  Otel_tracer.end_span s ~ok:true;
  let cfg : Otel_tracer.config =
    { service_name = "test-svc"; endpoint = None }
  in
  let json = Otel_tracer.to_otlp_json cfg in
  check bool "has resourceSpans" true
    (json_assoc_field "resourceSpans" json <> None);
  let rs = match json_assoc_field "resourceSpans" json with
    | Some (`List [ item ]) -> item
    | _ -> failwith "expected single resourceSpans item"
  in
  check bool "has resource" true
    (json_assoc_field "resource" rs <> None);
  check bool "has scopeSpans" true
    (json_assoc_field "scopeSpans" rs <> None)

let test_attrs_to_json_format () =
  let json = Otel_tracer.attrs_to_json [ ("mykey", "myval") ] in
  match json with
  | `List [ `Assoc fields ] ->
    let key_val = List.assoc_opt "key" fields in
    check (option string) "key field"
      (Some "mykey")
      (match key_val with Some (`String s) -> Some s | _ -> None);
    let value_obj = List.assoc_opt "value" fields in
    let str_val = match value_obj with
      | Some obj -> json_assoc_field "stringValue" obj
      | None -> None
    in
    check (option string) "stringValue field"
      (Some "myval")
      (match str_val with Some (`String s) -> Some s | _ -> None)
  | _ -> fail "expected list with one assoc"

let test_status_to_json () =
  Otel_tracer.reset ();
  (* UNSET: status = None *)
  let s_unset = Otel_tracer.start_span (default_attrs ~name:"unset" ()) in
  let j_unset = Otel_tracer.status_to_json s_unset in
  check (option int) "UNSET code=0" (Some 0)
    (match json_assoc_field "code" j_unset with
     | Some (`Int n) -> Some n | _ -> None);
  Otel_tracer.end_span s_unset ~ok:true;
  (* OK: status = Some true *)
  let j_ok = Otel_tracer.status_to_json s_unset in
  check (option int) "OK code=1" (Some 1)
    (match json_assoc_field "code" j_ok with
     | Some (`Int n) -> Some n | _ -> None);
  check bool "OK has no message field" true
    (json_assoc_field "message" j_ok = None);
  (* ERROR: status = Some false *)
  let s_err = Otel_tracer.start_span (default_attrs ~name:"err" ()) in
  Otel_tracer.end_span s_err ~ok:false;
  let j_err = Otel_tracer.status_to_json s_err in
  check (option int) "ERROR code=2" (Some 2)
    (match json_assoc_field "code" j_err with
     | Some (`Int n) -> Some n | _ -> None);
  check (option string) "ERROR message" (Some "error")
    (match json_assoc_field "message" j_err with
     | Some (`String s) -> Some s | _ -> None)

(* ── First-class Module ──────────────────────────────────────────── *)

let test_create_produces_valid_tracer () =
  Otel_tracer.reset ();
  let tracer : Tracing.t = Otel_tracer.create () in
  let module T = (val tracer : Tracing.TRACER) in
  let span = T.start_span (default_attrs ~name:"fc_test" ()) in
  T.add_event span "fc event";
  T.end_span span ~ok:true;
  check int "completed via first-class module" 1 (Otel_tracer.completed_count ())

let test_with_span_helper () =
  Otel_tracer.reset ();
  let tracer : Tracing.t = Otel_tracer.create () in
  let result =
    Tracing.with_span tracer (default_attrs ~name:"with_span_test" ())
      (fun _t -> 42)
  in
  check int "with_span returns body result" 42 result;
  check int "span was completed" 1 (Otel_tracer.completed_count ());
  (* Test exception path *)
  Otel_tracer.reset ();
  let raised = ref false in
  (try
     Tracing.with_span tracer (default_attrs ~name:"with_span_exn" ())
       (fun _t -> failwith "boom")
     |> ignore
   with Failure _ -> raised := true);
  check bool "exception was re-raised" true !raised;
  check int "span completed on exception" 1 (Otel_tracer.completed_count ())

(* ── Entry point ─────────────────────────────────────────────────── *)

let () =
  run "Otel_tracer" [
    "span_lifecycle", [
      test_case "start_span name format" `Quick test_start_span_name_format;
      test_case "start_span attributes" `Quick test_start_span_attributes;
      test_case "start_span kind mapping" `Quick test_start_span_kind_mapping;
      test_case "end_span sets status" `Quick test_end_span_sets_status;
      test_case "end_span moves to completed" `Quick test_end_span_moves_to_completed;
      test_case "hex IDs valid" `Quick test_start_span_hex_ids;
    ];
    "span_hierarchy", [
      test_case "child inherits trace_id" `Quick test_child_inherits_trace_id;
      test_case "child has parent_span_id" `Quick test_child_has_parent_span_id;
      test_case "root has no parent" `Quick test_root_span_no_parent;
    ];
    "events_and_attributes", [
      test_case "add_event" `Quick test_add_event;
      test_case "add_attrs" `Quick test_add_attrs;
      test_case "multiple events preserve order" `Quick test_multiple_events_order;
    ];
    "buffer_management", [
      test_case "flush returns completed" `Quick test_flush_returns_completed;
      test_case "reset clears all" `Quick test_reset_clears_all;
      test_case "counts reflect state" `Quick test_counts;
      test_case "double flush empty" `Quick test_double_flush;
    ];
    "json_serialization", [
      test_case "span_to_json has required fields" `Quick test_span_to_json_fields;
      test_case "to_otlp_json structure" `Quick test_to_otlp_json_structure;
      test_case "attrs_to_json format" `Quick test_attrs_to_json_format;
      test_case "status_to_json codes" `Quick test_status_to_json;
    ];
    "first_class_module", [
      test_case "create produces valid tracer" `Quick test_create_produces_valid_tracer;
      test_case "with_span helper" `Quick test_with_span_helper;
    ];
  ]
