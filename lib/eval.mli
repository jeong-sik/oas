(** Evaluation framework: metrics, collectors, comparisons, thresholds.

    Provides a collector pattern for recording metrics during agent runs,
    and comparison/threshold tools for regression detection. *)

(** {1 Metric types} *)

type metric_value =
  | Int_val of int
  | Float_val of float
  | Bool_val of bool
  | String_val of string

type metric = {
  name: string;
  value: metric_value;
  unit_: string option;
  tags: (string * string) list;
}

type run_metrics = {
  run_id: string;
  agent_name: string;
  timestamp: float;
  metrics: metric list;
  harness_verdicts: Harness.verdict list;
  trace_summary: Trace_eval.summary option;
}

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

type threshold = {
  metric_name: string;
  max_value: metric_value option;
  min_value: metric_value option;
}

type collector = {
  agent_name: string;
  run_id: string;
  mutable metrics: metric list;
  mutable harness_verdicts: Harness.verdict list;
  mutable trace_summary: Trace_eval.summary option;
}

(** {1 Metric value operations} *)

val metric_value_to_yojson : metric_value -> Yojson.Safe.t
val metric_value_of_yojson : Yojson.Safe.t -> (metric_value, string) result
val show_metric_value : metric_value -> string
val pp_metric_value : Format.formatter -> metric_value -> unit
val metric_value_to_float : metric_value -> float option

(** {1 Metric serialization} *)

val metric_to_yojson : metric -> Yojson.Safe.t
val metric_of_yojson : Yojson.Safe.t -> (metric, string) result
val show_metric : metric -> string
val pp_metric : Format.formatter -> metric -> unit

(** {1 Run metrics serialization} *)

val run_metrics_to_yojson : run_metrics -> Yojson.Safe.t
val run_metrics_of_yojson : Yojson.Safe.t -> (run_metrics, string) result
val show_run_metrics : run_metrics -> string
val pp_run_metrics : Format.formatter -> run_metrics -> unit

(** {1 Collector API} *)

val create_collector : agent_name:string -> run_id:string -> collector
val record : collector -> metric -> unit
val add_verdict : collector -> Harness.verdict -> unit
val set_trace_summary : collector -> Trace_eval.summary -> unit
val finalize : collector -> run_metrics

(** {1 Comparison and thresholds} *)

val default_delta_threshold_pct : float
val compute_delta :
  ?threshold_pct:float ->
  baseline_val:metric_value ->
  candidate_val:metric_value ->
  unit ->
  change_direction * float option
val compare : baseline:run_metrics -> candidate:run_metrics -> comparison
val check_thresholds : run_metrics -> threshold list -> Harness.verdict

(** {1 Metric lookup} *)

val find_metric : run_metrics -> string -> metric option
val find_metric_value : run_metrics -> string -> metric_value option
