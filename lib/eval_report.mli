(** Structured evaluation report.

    @since 0.68.0 *)

type t = {
  agent_name: string;
  run_count: int;
  evaluated_runs: int;
  skipped_runs: int;
  pass_at_k: float;
  comparison: Eval_baseline.comparison option;
  verdict: [`Pass | `Fail | `NoBaseline];
  summary: string;
}

(** Generate a report from optional baseline and run metrics. *)
val generate : ?baseline:Eval_baseline.baseline -> Eval.run_metrics list -> t

val to_json : t -> Yojson.Safe.t
val to_string : t -> string
