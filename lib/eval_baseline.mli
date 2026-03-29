(** Eval baseline: golden-file comparison for regression detection.

    @since 0.68.0

    @stability Evolving
    @since 0.93.1 *)

type baseline = {
  run_metrics: Eval.run_metrics;
  created_at: float;
  description: string;
}

type metric_diff =
  | Unchanged
  | Improved of { name: string; baseline_val: float; current_val: float; delta_pct: float }
  | Regressed of { name: string; baseline_val: float; current_val: float; delta_pct: float }
  | Added of { name: string; value: Eval.metric_value }
  | Removed of { name: string; value: Eval.metric_value }

type comparison = {
  diffs: metric_diff list;
  regressions: int;
  improvements: int;
  passed: bool;
}

val save : path:string -> baseline -> (unit, string) result
val load : path:string -> (baseline, string) result
val create : description:string -> Eval.run_metrics -> baseline

(** Compare current run against baseline.
    [tolerance_pct] (default 5.0) is the threshold for regression detection. *)
val compare :
  ?tolerance_pct:float ->
  baseline:baseline ->
  current:Eval.run_metrics ->
  unit ->
  comparison

(** pass\@k: fraction of runs where all harness verdicts pass. *)
val pass_at_k : Eval.run_metrics list -> float

val show_diff : metric_diff -> string
