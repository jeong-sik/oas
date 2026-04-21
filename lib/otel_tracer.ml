(** OpenTelemetry-compatible tracer for Agent SDK.
    Implements Tracing.TRACER and exports spans as OTLP JSON.

    Self-contained: no external opentelemetry dependency.
    Generates W3C trace/span IDs, maps to GenAI semantic conventions,
    and serializes completed spans to OTLP JSON for HTTP export.

    v0.43.0: Instance-based state — each [create] call returns an
    independent tracer with its own span stack. The global functions
    ([start_span], [flush], etc.) delegate to a shared global instance
    for backward compatibility. *)

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
  end_time_ns: Int64.t option;
  status: bool option;
  attributes: (string * string) list;
  events: otel_event list;
}

(* -- Config ----------------------------------------------------------- *)

type config = {
  service_name: string;
  endpoint: string option;
}

let default_config = {
  service_name = "agent-sdk";
  endpoint = None;
}

(* -- Metric types ----------------------------------------------------- *)

type metric_type = Counter | Gauge | Histogram

type metric_entry = {
  m_name: string;
  m_value: float;
  m_type: metric_type;
}

(* -- Instance type ---------------------------------------------------- *)

type mutex_impl =
  | Stdlib_mu of Mutex.t
  | Eio_mu of Eio.Mutex.t

type instance = {
  config: config;
  mu: mutex_impl;
  mutable current_spans: span list;
  mutable completed_spans: span list;
  mutable metrics: metric_entry list;
}

(* -- Random hex ID generation ----------------------------------------- *)

(* Trace/span IDs need uniqueness, not cryptographic strength.
   OCaml 5.x Random is domain-safe and self-seeded from OS entropy,
   so we avoid opening /dev/urandom on every span start. *)

let hex_chars = "0123456789abcdef"

let hex_of_prng n =
  let buf = Buffer.create (n * 2) in
  for _ = 1 to n do
    let byte = Random.int 256 in
    Buffer.add_char buf hex_chars.[byte lsr 4];
    Buffer.add_char buf hex_chars.[byte land 0x0f]
  done;
  Buffer.contents buf

let gen_trace_id () = hex_of_prng 16  (* 32-char hex = 128-bit *)
let gen_span_id () = hex_of_prng 8    (* 16-char hex = 64-bit *)

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

(* -- Instance operations ---------------------------------------------- *)

let inst_with_lock inst f =
  match inst.mu with
  | Eio_mu mu -> Eio.Mutex.use_rw ~protect:true mu f
  | Stdlib_mu mu ->
    Mutex.lock mu;
    Fun.protect f ~finally:(fun () -> Mutex.unlock mu)

let inst_start_span inst (attrs : Tracing.span_attrs) : span =
  let new_trace_id = gen_trace_id () in
  let span_id = gen_span_id () in
  inst_with_lock inst @@ fun () ->
  let parent = match inst.current_spans with
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
  inst.current_spans <- s :: inst.current_spans;
  s

let inst_end_span inst (s : span) ~ok =
  inst_with_lock inst @@ fun () ->
  let target = ref None in
  Printf.printf "DEBUG: end_span for %s, current_spans size=%d\n%!" s.span_id (List.length inst.current_spans);
  inst.current_spans <- (
    List.filter_map (fun sp ->
      if sp.span_id = s.span_id then begin
        let updated = { sp with end_time_ns = Some (now_ns ()); status = Some ok } in
        target := Some updated;
        None
      end else Some sp
    ) inst.current_spans
  );
  match !target with
  | Some completed -> inst.completed_spans <- completed :: inst.completed_spans
  | None -> ()

let inst_add_event inst (s : span) (msg : string) =
  inst_with_lock inst @@ fun () ->
  let evt = {
    event_name = msg;
    timestamp_ns = now_ns ();
    attributes = [];
  } in
  Printf.printf "DEBUG: add_event for %s, current_spans size=%d\n%!" s.span_id (List.length inst.current_spans);
  inst.current_spans <- List.map (fun sp ->
    if sp.span_id = s.span_id then { sp with events = Util.snoc sp.events evt }
    else sp
  ) inst.current_spans

let inst_add_attrs inst (s : span) (attrs : (string * string) list) =
  inst_with_lock inst @@ fun () ->
  inst.current_spans <- List.map (fun sp ->
    if sp.span_id = s.span_id then { sp with attributes = Util.snoc_list sp.attributes attrs }
    else sp
  ) inst.current_spans

let inst_flush inst =
  inst_with_lock inst @@ fun () ->
  let spans = List.rev inst.completed_spans in
  inst.completed_spans <- [];
  spans

let inst_reset inst =
  inst_with_lock inst @@ fun () ->
  inst.current_spans <- [];
  inst.completed_spans <- []

let inst_completed_count inst =
  inst_with_lock inst @@ fun () ->
  List.length inst.completed_spans

let inst_active_count inst =
  inst_with_lock inst @@ fun () ->
  List.length inst.current_spans

(* -- Instance metric operations --------------------------------------- *)

let inst_record_metric inst ~name ~value ~metric_type =
  inst_with_lock inst @@ fun () ->
  inst.metrics <- Util.snoc inst.metrics { m_name = name; m_value = value; m_type = metric_type }

let inst_get_metrics inst =
  inst_with_lock inst @@ fun () ->
  List.map (fun m -> (m.m_name, m.m_value, m.m_type)) inst.metrics

let inst_clear_metrics inst =
  inst_with_lock inst @@ fun () ->
  inst.metrics <- []

let metric_type_to_string = function
  | Counter -> "counter"
  | Gauge -> "gauge"
  | Histogram -> "histogram"

(* -- Global instance (backward compat) -------------------------------- *)

let _global : instance = {
  config = default_config;
  mu = Stdlib_mu (Mutex.create ());
  current_spans = [];
  completed_spans = [];
  metrics = [];
}

let start_span attrs = inst_start_span _global attrs
let end_span s ~ok = inst_end_span _global s ~ok
let add_event s msg = inst_add_event _global s msg
let add_attrs s attrs = inst_add_attrs _global s attrs
let flush () = inst_flush _global
let reset () = inst_reset _global
let completed_count () = inst_completed_count _global
let active_count () = inst_active_count _global
let record_metric ~name ~value ~metric_type =
  inst_record_metric _global ~name ~value ~metric_type
let get_metrics () = inst_get_metrics _global
let clear_metrics () = inst_clear_metrics _global

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

let metric_entry_to_json (m : metric_entry) : Yojson.Safe.t =
  let ts = now_ns () in
  let data_point = `Assoc [
    ("asDouble", `Float m.m_value);
    ("timeUnixNano", `String (Int64.to_string ts));
  ] in
  let metric_body = match m.m_type with
    | Counter ->
      [("sum", `Assoc [
        ("dataPoints", `List [data_point]);
        ("isMonotonic", `Bool true);
        ("aggregationTemporality", `Int 2);
      ])]
    | Gauge ->
      [("gauge", `Assoc [
        ("dataPoints", `List [data_point]);
      ])]
    | Histogram ->
      [("histogram", `Assoc [
        ("dataPoints", `List [data_point]);
        ("aggregationTemporality", `Int 2);
      ])]
  in
  `Assoc (("name", `String m.m_name) :: metric_body)

let to_otlp_json (cfg : config) : Yojson.Safe.t =
  let spans, metrics = inst_with_lock _global
    (fun () -> (List.rev _global.completed_spans, _global.metrics)) in
  let resource = `Assoc [
    ("attributes", attrs_to_json [
      ("service.name", cfg.service_name)
    ])
  ] in
  let resource_spans = `Assoc [
    ("resource", resource);
    ("scopeSpans", `List [
      `Assoc [
        ("scope", `Assoc [
          ("name", `String "agent_sdk.otel_tracer");
          ("version", `String Sdk_version.version);
        ]);
        ("spans", `List (List.map span_to_json spans));
      ]
    ])
  ] in
  let base = [("resourceSpans", `List [resource_spans])] in
  let with_metrics =
    if metrics = [] then base
    else
      let resource_metrics = `Assoc [
        ("resource", resource);
        ("scopeMetrics", `List [
          `Assoc [
            ("scope", `Assoc [
              ("name", `String "agent_sdk.otel_tracer");
              ("version", `String Sdk_version.version);
            ]);
            ("metrics", `List (List.map metric_entry_to_json metrics));
          ]
        ])
      ] in
      base @ [("resourceMetrics", `List [resource_metrics])]
  in
  `Assoc with_metrics

(* -- Instance creation ------------------------------------------------ *)

let create_instance ?(config = default_config) () : instance =
  { config; mu = Eio_mu (Eio.Mutex.create ());
    current_spans = []; completed_spans = []; metrics = [] }

let create_instance_eio ?(config = default_config) () : instance =
  create_instance ~config ()

let tracer_of_instance inst : Tracing.t =
  (module struct
    type nonrec span = span
    let start_span = inst_start_span inst
    let end_span = inst_end_span inst
    let add_event = inst_add_event inst
    let add_attrs = inst_add_attrs inst
    let trace_id s = Some s.trace_id
    let span_id s = Some s.span_id
  end)

(* -- First-class module constructors ---------------------------------- *)

let create ?(config : config = default_config) () : Tracing.t =
  tracer_of_instance (create_instance ~config ())

let create_eio ?(config : config = default_config) () : Tracing.t =
  tracer_of_instance (create_instance_eio ~config ())
