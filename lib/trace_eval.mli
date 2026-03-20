(** Trace-level evaluation — aggregate span metrics and run health checks.

    Operates on {!Otel_tracer.span} lists produced by the OpenTelemetry
    tracer.  Used by {!Eval} to attach trace summaries to run metrics. *)

(** {1 Types} *)

(** Aggregate statistics for a set of spans. *)
type summary = {
  total_spans: int;
  agent_runs: int;
  api_calls: int;
  tool_execs: int;
  hook_invokes: int;
  failed_spans: int;
  failed_api_calls: int;
  failed_tool_execs: int;
  total_events: int;
  average_duration_ms: float option;
  longest_span_name: string option;
}

(** A single validation check with optional detail. *)
type check = {
  name: string;
  passed: bool;
  detail: string option;
}

(** Evaluation result combining summary and checks. *)
type evaluation = {
  ok: bool;
  summary: summary;
  checks: check list;
}

(** {1 Helpers} *)

(** Classify a span by its name prefix. *)
val classify_span :
  Otel_tracer.span ->
  [> `Agent_run | `Api_call | `Tool_exec | `Hook_invoke | `Other ]

(** Compute elapsed duration of a span in milliseconds.
    Returns [0.0] if the span has no [end_time_ns]. *)
val duration_ms : Otel_tracer.span -> float

(** {1 Analysis} *)

(** Compute aggregate summary from a list of spans. *)
val summarize : Otel_tracer.span list -> summary

(** Evaluate spans against configurable thresholds.
    @param max_failed_api_calls maximum allowed API call failures (default 0)
    @param max_failed_tool_execs maximum allowed tool execution failures (default 0)
    @param max_span_duration_ms optional upper bound on the longest span duration *)
val evaluate :
  ?max_failed_api_calls:int ->
  ?max_failed_tool_execs:int ->
  ?max_span_duration_ms:float ->
  Otel_tracer.span list -> evaluation

(** Evaluate the globally flushed spans from {!Otel_tracer.flush}. *)
val evaluate_flushed :
  ?max_failed_api_calls:int ->
  ?max_failed_tool_execs:int ->
  ?max_span_duration_ms:float ->
  unit -> evaluation
