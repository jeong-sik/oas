(** Eval Metrics OTel Bridge — emit [Eval.run_metrics] as OTel span events.

    OAS's [Otel_tracer] is span-based (no native counter/gauge API).
    This bridge records eval metrics as a dedicated span with structured
    attributes following the [oas.eval.*] namespace convention from
    RFC-OAS-002.

    Metric data is encoded as span attributes so OTel collectors can
    extract counters/gauges via attribute-based rules.  The bridge also
    provides {!to_metrics_json} for direct JSON export.

    @stability Evolving
    @since 0.100.0 *)

(** {1 Accumulated metric record} *)

(** A single OTel-style metric with name, value, and type tag. *)
type otel_metric =
  { name : string
  ; value : float
  ; metric_type : string (** "counter" | "gauge" *)
  }

(** All metrics extracted from a run. *)
type metrics_snapshot =
  { agent_name : string
  ; run_id : string
  ; verdict_passed_total : int
  ; verdict_failed_total : int
  ; coverage : float
  ; turns_total : int option
  ; tool_calls_total : int option
  ; tool_errors_total : int option
  ; api_calls_total : int option
  ; failed_api_calls_total : int option
  }

(** {1 Extraction} *)

(** Extract metric values from [run_metrics]. *)
val extract : Eval.run_metrics -> metrics_snapshot

(** {1 Metric type mapping} *)

(** Map string metric type tags to [Otel_tracer.metric_type]. *)
val otel_metric_type_to_tracer : string -> Otel_tracer.metric_type

(** {1 OTel metric emission} *)

(** Emit eval metrics on the given [Otel_tracer.instance] using the
    native metric API ([Otel_tracer.inst_record_metric]).  Also creates
    a summary span named ["eval_metrics"] for correlation. *)
val emit_run_metrics : Otel_tracer.instance -> Eval.run_metrics -> unit

(** {1 JSON export} *)

(** Produce a JSON array of metric objects for external consumption.
    Each object has [name], [value], and [type] fields. *)
val to_metrics_json : metrics_snapshot -> Yojson.Safe.t

(** Convert snapshot to a flat list of [otel_metric] records. *)
val to_metric_list : metrics_snapshot -> otel_metric list
