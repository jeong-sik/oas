(** Trace-based evaluation of {!Otel_tracer} spans.

    Summarizes and validates spans collected during an agent run. *)

(** {1 Types} *)

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

type check = {
  name: string;
  passed: bool;
  detail: string option;
}

type evaluation = {
  ok: bool;
  summary: summary;
  checks: check list;
}

(** {1 Helpers} *)

val classify_span : Otel_tracer.span -> [> `Agent_run | `Api_call | `Tool_exec | `Hook_invoke | `Other ]
val duration_ms : Otel_tracer.span -> float

(** {1 Analysis} *)

(** Summarize a list of spans into aggregate counts and durations. *)
val summarize : Otel_tracer.span list -> summary

(** Evaluate spans against configurable budgets. *)
val evaluate :
  ?max_failed_api_calls:int ->
  ?max_failed_tool_execs:int ->
  ?max_span_duration_ms:float ->
  Otel_tracer.span list -> evaluation

(** Flush the global {!Otel_tracer} and evaluate the collected spans. *)
val evaluate_flushed :
  ?max_failed_api_calls:int ->
  ?max_failed_tool_execs:int ->
  ?max_span_duration_ms:float ->
  unit -> evaluation
