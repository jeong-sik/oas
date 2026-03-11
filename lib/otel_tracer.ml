(** OpenTelemetry-compatible tracer for Agent SDK.
    Implements Tracing.TRACER and exports spans as OTLP JSON.

    Self-contained: no external opentelemetry dependency.
    Generates W3C trace/span IDs, maps to GenAI semantic conventions,
    and serializes completed spans to OTLP JSON for HTTP export. *)

(* -- OTel span kind --------------------------------------------------- *)

type otel_span_kind = Internal | Client | Server | Producer | Consumer

let otel_span_kind_to_int = function
  | Internal -> 1
  | Client   -> 3
  | Server   -> 2
  | Producer -> 4
  | Consumer -> 5

(* -- Event / Span types ----------------------------------------------- *)

type otel_event = {
  event_name: string;
  timestamp_ns: Int64.t;
  attributes: (string * string) list;
}

type span = {
  trace_id: string;
  span_id: string;
  parent_span_id: string option;
  name: string;
  kind: otel_span_kind;
  start_time_ns: Int64.t;
  mutable end_time_ns: Int64.t option;
  mutable status: bool option;
  mutable attributes: (string * string) list;
  mutable events: otel_event list;
}

(* -- Global state ----------------------------------------------------- *)

let _mutex = Mutex.create ()

let with_lock f =
  Mutex.lock _mutex;
  Fun.protect f ~finally:(fun () -> Mutex.unlock _mutex)
let _current_spans : span list ref = ref []
let _completed_spans : span list ref = ref []

(* -- Config ----------------------------------------------------------- *)

type config = {
  service_name: string;
  endpoint: string option;
}

let default_config = {
  service_name = "agent-sdk";
  endpoint = None;
}

(* -- Random hex ID generation ----------------------------------------- *)

let hex_chars = "0123456789abcdef"

let hex_of_random n =
  let buf = Bytes.create (n * 2) in
  for i = 0 to n - 1 do
    let byte = Random.int 256 in
    Bytes.set buf (i * 2) hex_chars.[byte lsr 4];
    Bytes.set buf (i * 2 + 1) hex_chars.[byte land 0x0f]
  done;
  Bytes.to_string buf

let gen_trace_id () = hex_of_random 16  (* 32-char hex = 128-bit *)
let gen_span_id () = hex_of_random 8    (* 16-char hex = 64-bit *)

(* -- Timestamp -------------------------------------------------------- *)

let now_ns () =
  let t = Unix.gettimeofday () in
  Int64.of_float (t *. 1_000_000_000.0)

(* -- Span kind mapping ------------------------------------------------ *)

let map_span_kind = function
  | Tracing.Agent_run   -> Internal
  | Tracing.Api_call    -> Client
  | Tracing.Tool_exec   -> Internal
  | Tracing.Hook_invoke -> Internal

(* -- Semantic convention attributes ----------------------------------- *)

let semantic_attrs (attrs : Tracing.span_attrs) =
  [ ("gen_ai.agent.name", attrs.agent_name);
    ("gen_ai.turn", string_of_int attrs.turn);
    ("gen_ai.operation.name", attrs.name) ]
  @ attrs.extra

(* -- Span name -------------------------------------------------------- *)

let span_kind_to_string = function
  | Tracing.Agent_run   -> "agent_run"
  | Tracing.Api_call    -> "api_call"
  | Tracing.Tool_exec   -> "tool_exec"
  | Tracing.Hook_invoke -> "hook_invoke"

let make_span_name (attrs : Tracing.span_attrs) =
  Printf.sprintf "%s/%s" (span_kind_to_string attrs.kind) attrs.name

(* -- TRACER implementation -------------------------------------------- *)

let start_span (attrs : Tracing.span_attrs) : span =
  let new_trace_id = gen_trace_id () in
  let span_id = gen_span_id () in
  with_lock @@ fun () ->
  let parent = match !_current_spans with
    | p :: _ -> Some p
    | []     -> None
  in
  let trace_id = match parent with
    | Some p -> p.trace_id
    | None   -> new_trace_id
  in
  let parent_span_id = match parent with
    | Some p -> Some p.span_id
    | None   -> None
  in
  let s = {
    trace_id;
    span_id;
    parent_span_id;
    name = make_span_name attrs;
    kind = map_span_kind attrs.kind;
    start_time_ns = now_ns ();
    end_time_ns = None;
    status = None;
    attributes = semantic_attrs attrs;
    events = [];
  } in
  _current_spans := s :: !_current_spans;
  s

let end_span (s : span) ~ok =
  with_lock @@ fun () ->
  s.end_time_ns <- Some (now_ns ());
  s.status <- Some ok;
  (* Pop from current stack -- remove the first matching span *)
  _current_spans := (
    let found = ref false in
    List.filter (fun sp ->
      if not !found && sp.span_id = s.span_id then begin
        found := true; false
      end else true
    ) !_current_spans
  );
  _completed_spans := s :: !_completed_spans

let add_event (s : span) (msg : string) =
  with_lock @@ fun () ->
  let evt = {
    event_name = msg;
    timestamp_ns = now_ns ();
    attributes = [];
  } in
  s.events <- s.events @ [evt]

let add_attrs (s : span) (attrs : (string * string) list) =
  with_lock @@ fun () ->
  s.attributes <- s.attributes @ attrs

(* -- JSON export ------------------------------------------------------ *)

let attrs_to_json (attrs : (string * string) list) : Yojson.Safe.t =
  `List (List.map (fun (k, v) ->
    `Assoc [
      ("key", `String k);
      ("value", `Assoc [("stringValue", `String v)])
    ]
  ) attrs)

let event_to_json (evt : otel_event) : Yojson.Safe.t =
  `Assoc [
    ("timeUnixNano", `String (Int64.to_string evt.timestamp_ns));
    ("name", `String evt.event_name);
    ("attributes", attrs_to_json evt.attributes);
  ]

let status_to_json (s : span) : Yojson.Safe.t =
  match s.status with
  | None       -> `Assoc [("code", `Int 0)]  (* UNSET *)
  | Some true  -> `Assoc [("code", `Int 1)]  (* OK *)
  | Some false -> `Assoc [("code", `Int 2); ("message", `String "error")]  (* ERROR *)

let span_to_json (s : span) : Yojson.Safe.t =
  let end_ns = match s.end_time_ns with
    | Some ns -> ns
    | None    -> s.start_time_ns
  in
  let base = [
    ("traceId", `String s.trace_id);
    ("spanId", `String s.span_id);
    ("name", `String s.name);
    ("kind", `Int (otel_span_kind_to_int s.kind));
    ("startTimeUnixNano", `String (Int64.to_string s.start_time_ns));
    ("endTimeUnixNano", `String (Int64.to_string end_ns));
    ("status", status_to_json s);
    ("attributes", attrs_to_json s.attributes);
    ("events", `List (List.map event_to_json s.events));
  ] in
  let with_parent = match s.parent_span_id with
    | Some pid -> ("parentSpanId", `String pid) :: base
    | None     -> base
  in
  `Assoc with_parent

let to_otlp_json (cfg : config) : Yojson.Safe.t =
  let spans = with_lock
    (fun () -> List.rev !_completed_spans) in
  `Assoc [
    ("resourceSpans", `List [
      `Assoc [
        ("resource", `Assoc [
          ("attributes", attrs_to_json [
            ("service.name", cfg.service_name)
          ])
        ]);
        ("scopeSpans", `List [
          `Assoc [
            ("scope", `Assoc [
              ("name", `String "agent_sdk.otel_tracer");
              ("version", `String "0.1.0");
            ]);
            ("spans", `List (List.map span_to_json spans));
          ]
        ])
      ]
    ])
  ]

(* -- Buffer management ------------------------------------------------ *)

let flush () =
  with_lock @@ fun () ->
  let spans = List.rev !_completed_spans in
  _completed_spans := [];
  spans

let reset () =
  with_lock @@ fun () ->
  _current_spans := [];
  _completed_spans := []

let completed_count () =
  with_lock @@ fun () ->
  List.length !_completed_spans

let active_count () =
  with_lock @@ fun () ->
  List.length !_current_spans

(* -- First-class module constructor ----------------------------------- *)

let create ?(config : config = default_config) () : Tracing.t =
  ignore config;
  (module struct
    type nonrec span = span
    let start_span = start_span
    let end_span = end_span
    let add_event = add_event
    let add_attrs = add_attrs
  end)
