(** Evaluation framework for quantitative agent run assessment.

    Collects metrics, compares baselines vs candidates, and checks
    thresholds for regression detection.

    @stability Evolving
    @since 0.93.1 *)

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

(** {1 Metric} *)

(** A named metric with optional unit and tags. *)
type metric =
  { name : string
  ; value : metric_value
  ; unit_ : string option
  ; tags : (string * string) list
  }

type metric_identity =
  { name : string
  ; unit_ : string option
  ; tags : (string * string) list
  }

val metric_to_yojson : metric -> Yojson.Safe.t

(** Decode a metric, rejecting duplicate object fields, duplicate tag names,
    non-object tags, and non-string tag values. Decoded tags are canonicalized
    by name and value. *)
val metric_of_yojson : Yojson.Safe.t -> (metric, string) result

val show_metric : metric -> string
val pp_metric : Format.formatter -> metric -> unit

(** {1 Metric comparison policy} *)

type numeric_tolerance =
  | Relative_pct of float
  | Absolute_int of int64
  | Absolute_float of float

type metric_policy =
  | Higher_is_better of numeric_tolerance
  | Lower_is_better of numeric_tolerance
  | Exact_numeric of numeric_tolerance
  | Exact_value

type metric_spec =
  { identity : metric_identity
  ; policy : metric_policy
  }

type metric_side =
  | Expected
  | Baseline
  | Candidate

type comparison_error =
  | Duplicate_metric_spec of metric_identity
  | Duplicate_baseline_metric of metric_identity
  | Duplicate_candidate_metric of metric_identity
  | Missing_baseline_metric of metric_identity
  | Missing_candidate_metric of metric_identity
  | Duplicate_metric_tag of
      { identity : metric_identity
      ; side : metric_side
      ; tag_name : string
      }
  | Invalid_numeric_tolerance of
      { identity : metric_identity
      ; tolerance : numeric_tolerance
      }
  | Incompatible_metric_values of
      { identity : metric_identity
      ; policy : metric_policy
      ; baseline_value : metric_value
      ; candidate_value : metric_value
      }
  | Non_finite_metric_value of
      { identity : metric_identity
      ; side : metric_side
      ; value : float
      }
  | Non_finite_numeric_result of metric_identity
  | Relative_tolerance_zero_baseline of
      { identity : metric_identity
      ; candidate_value : metric_value
      }

(** {1 Run metrics} *)

(** Finalized metrics from an agent run. *)
type run_metrics =
  { run_id : string
  ; agent_name : string
  ; timestamp : float
  ; metrics : metric list
  ; harness_verdicts : Harness.verdict list
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

(** Finalize into immutable [run_metrics]. *)
val finalize : collector -> run_metrics

(** {1 Comparison} *)

type change_direction =
  | Regression
  | Improvement
  | Unchanged

type metric_delta =
  { identity : metric_identity
  ; baseline_value : metric_value
  ; candidate_value : metric_value
  ; direction : change_direction
  ; delta_pct : float option
  }

type comparison =
  { baseline : run_metrics
  ; candidate : run_metrics
  ; regressions : metric_delta list
  ; improvements : metric_delta list
  ; unchanged : metric_delta list
  }

(** Compare exactly the metrics selected by [specs]. Metric identity includes
    unit and tags, numeric policies require same-kind finite values, and every
    tolerance must be finite and non-negative. Relative tolerance is undefined
    for a non-zero change from a zero baseline; use [Absolute_int] or
    [Absolute_float] explicitly. *)
val compare_with_specs
  :  specs:metric_spec list
  -> baseline:run_metrics
  -> candidate:run_metrics
  -> (comparison, comparison_error) result

(** {1 Threshold checking} *)

type threshold =
  { identity : metric_identity
  ; max_value : metric_value option
  ; min_value : metric_value option
  }

type threshold_error =
  | Duplicate_threshold of metric_identity
  | Duplicate_threshold_metric of metric_identity
  | Missing_threshold_metric of metric_identity
  | Empty_threshold of metric_identity
  | Incompatible_threshold_value of
      { identity : metric_identity
      ; metric_value : metric_value
      ; threshold_value : metric_value
      }
  | Non_finite_threshold_value of
      { identity : metric_identity
      ; value : float
      }
  | Invalid_threshold_range of metric_identity
  | Duplicate_threshold_identity_tag of
      { identity : metric_identity
      ; tag_name : string
      }

(** Check run metrics against unique, present, same-kind finite thresholds.
    Violation evidence identifies the canonical name/unit/tags tuple. *)
val check_thresholds
  :  run_metrics
  -> threshold list
  -> (Harness.verdict, threshold_error) result
