(** OpenTelemetry-compatible tracing with OTLP JSON export.

    Provides span lifecycle management, event recording, and
    OTLP JSON serialization. Supports both Stdlib.Mutex (non-Eio)
    and Eio.Mutex backends.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Types} *)

type otel_span_kind = Internal | Client | Server | Producer | Consumer

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

type config = {
  service_name: string;
  endpoint: string option;
}

(** {1 Metric types} *)

type metric_type = Counter | Gauge | Histogram

type metric_entry = {
  m_name: string;
  m_value: float;
  m_type: metric_type;
}

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

(** {1 Config} *)

val default_config : config
val otel_span_kind_to_int : otel_span_kind -> int

(** {1 Utilities} *)

val now_ns : unit -> Int64.t
val map_span_kind : Tracing.span_kind -> otel_span_kind
val semantic_attrs : Tracing.span_attrs -> (string * string) list
val span_kind_to_string : Tracing.span_kind -> string
val make_span_name : Tracing.span_attrs -> string

(** {1 Instance operations} *)

val create_instance : ?config:config -> unit -> instance
val create_instance_eio : ?config:config -> unit -> instance
val inst_start_span : instance -> Tracing.span_attrs -> span
val inst_end_span : instance -> span -> ok:bool -> unit
val inst_add_event : instance -> span -> string -> unit
val inst_add_attrs : instance -> span -> (string * string) list -> unit
val inst_flush : instance -> span list
val inst_reset : instance -> unit
val inst_completed_count : instance -> int
val inst_active_count : instance -> int

(** {1 Instance metric operations} *)

val inst_record_metric : instance -> name:string -> value:float -> metric_type:metric_type -> unit
(** Record a metric (counter, gauge, or histogram) on the instance. *)

val inst_get_metrics : instance -> (string * float * metric_type) list
(** Retrieve all recorded metrics as [(name, value, type)] triples. *)

val inst_clear_metrics : instance -> unit
(** Clear all recorded metrics from the instance. *)

val metric_type_to_string : metric_type -> string
(** Convert metric type to its OTLP string representation. *)

(** {1 Global operations} *)

val start_span : Tracing.span_attrs -> span
val end_span : span -> ok:bool -> unit
val add_event : span -> string -> unit
val add_attrs : span -> (string * string) list -> unit
val flush : unit -> span list
val reset : unit -> unit
val completed_count : unit -> int
val active_count : unit -> int

(** {1 Global metric operations} *)

val record_metric : name:string -> value:float -> metric_type:metric_type -> unit
(** Record a metric on the global instance. *)

val get_metrics : unit -> (string * float * metric_type) list
(** Retrieve all metrics from the global instance. *)

val clear_metrics : unit -> unit
(** Clear all metrics from the global instance. *)

(** {1 JSON serialization} *)

val attrs_to_json : (string * string) list -> Yojson.Safe.t
val event_to_json : otel_event -> Yojson.Safe.t
val status_to_json : span -> Yojson.Safe.t
val span_to_json : span -> Yojson.Safe.t
val metric_entry_to_json : metric_entry -> Yojson.Safe.t
val to_otlp_json : config -> Yojson.Safe.t

(** {1 First-class module constructors} *)

val tracer_of_instance : instance -> Tracing.t
val create : ?config:config -> unit -> Tracing.t
val create_eio : ?config:config -> unit -> Tracing.t
