(** Evaluation framework for quantitative agent run assessment.

    Collects metrics, compares baselines vs candidates, and checks
    thresholds for regression detection.

    @stability Evolving
    @since 0.93.0 *)

(** {1 Metric value} *)

(** A metric value: typed scalar. *)
type metric_value =
  | Int_val of int
  | Float_val of float
  | Bool_val of bool
  | String_val of string

val metric_value_to_yojson : metric_value -> Yojson.Safe.t
val metric_value_of_yojson : Yojson.Safe.t -> (metric_value, string) result
val show_metric_value : metric_value -> string
val pp_metric_value : Format.formatter -> metric_value -> unit

(** Convert to float if possible. String_val returns None. *)
val metric_value_to_float : metric_value -> float option

(** {1 Metric} *)

(** A named metric with optional unit and tags. *)
type metric = {
  name: string;
  value: metric_value;
  unit_: string option;
  tags: (string * string) list;
}

val metric_to_yojson : metric -> Yojson.Safe.t
val metric_of_yojson : Yojson.Safe.t -> (metric, string) result
val show_metric : metric -> string
val pp_metric : Format.formatter -> metric -> unit

(** {1 Metric comparison policy} *)

type metric_goal =
  | Higher
  | Lower
  | Exact

type metric_spec = {
  name: string;
  goal: metric_goal;
  tolerance_pct: float option;
}

(** {1 Run metrics} *)

(** Finalized metrics from an agent run. *)
type run_metrics = {
  run_id: string;
  agent_name: string;
  timestamp: float;
  metrics: metric list;
  harness_verdicts: Harness.verdict list;
  trace_summary: Trace_eval.summary option;
}

val run_metrics_to_yojson : run_metrics -> Yojson.Safe.t
val run_metrics_of_yojson : Yojson.Safe.t -> (run_metrics, string) result
val show_run_metrics : run_metrics -> string
val pp_run_metrics : Format.formatter -> run_metrics -> unit

(** {1 Collector} *)

(** Mutable collector accumulating metrics during a run. *)
type collector

(** Create a new collector. *)
val create_collector : agent_name:string -> run_id:string -> collector

(** Record a metric. *)
val record : collector -> metric -> unit

(** Add a harness verdict. *)
val add_verdict : collector -> Harness.verdict -> unit

(** Set the trace evaluation summary. *)
val set_trace_summary : collector -> Trace_eval.summary -> unit

(** Finalize into immutable [run_metrics]. *)
val finalize : collector -> run_metrics

(** {1 Comparison} *)

type change_direction = Regression | Improvement | Unchanged

type metric_delta = {
  metric_name: string;
  baseline_value: metric_value;
  candidate_value: metric_value;
  direction: change_direction;
  delta_pct: float option;
}

type comparison = {
  baseline: run_metrics;
  candidate: run_metrics;
  regressions: metric_delta list;
  improvements: metric_delta list;
  unchanged: metric_delta list;
}

(** Default threshold percentage (5.0%) for classifying deltas. *)
val default_delta_threshold_pct : float

(** Compute direction and delta percentage between two values. *)
val compute_delta :
  ?threshold_pct:float ->
  baseline_val:metric_value ->
  candidate_val:metric_value ->
  unit ->
  change_direction * float option

(** Compare two runs, classifying each metric as regression/improvement/unchanged. *)
val compare : baseline:run_metrics -> candidate:run_metrics -> comparison

(** Compare two runs using per-metric goals instead of the legacy
    "lower is better" default. Metrics without a matching spec fall back
    to the legacy behavior. *)
val compare_with_specs :
  specs:metric_spec list ->
  baseline:run_metrics ->
  candidate:run_metrics ->
  comparison

(** {1 Threshold checking} *)

type threshold = {
  metric_name: string;
  max_value: metric_value option;
  min_value: metric_value option;
}

(** Check run metrics against thresholds, producing a harness verdict. *)
val check_thresholds : run_metrics -> threshold list -> Harness.verdict

(** {1 Metric lookup} *)

(** Find a metric by name. *)
val find_metric : run_metrics -> string -> metric option

(** Find a metric value by name. *)
val find_metric_value : run_metrics -> string -> metric_value option

(** {1 Statistical regression detection} *)

(** Compare multiple baseline runs against candidate runs.
    Uses Welch's t-test per metric. Returns list of regressed
    metric names with optional Cohen's d effect sizes. *)
val compare_statistical :
  baselines:run_metrics list ->
  candidates:run_metrics list ->
  (string * float option) list
