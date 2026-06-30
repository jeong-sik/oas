(** Code-snippet tool strategy evaluation gate.

    This module intentionally does not execute snippets.  It records the
    quantitative adoption criteria from RFC-OAS-004 and issue #522 so the
    experimental strategy cannot be promoted without a reproducible 10+ task
    comparison against JSON tool-call mode.

    @stability Experimental *)

val experimental_env_var : string

type mode_metrics =
  { turns : int
  ; llm_calls : int
  ; tokens : int
  ; passed : bool
  }

type comparison =
  { task_name : string
  ; json_mode : mode_metrics
  ; snippet_mode : mode_metrics
  ; call_reduction_pct : float
  ; tokens_saved : int
  }

type gate =
  { min_tasks : int
  ; min_avg_call_reduction_pct : float
  ; require_no_pass_regression : bool
  }

type gate_result =
  { passed : bool
  ; task_count : int
  ; avg_call_reduction_pct : float
  ; json_passes : int
  ; snippet_passes : int
  ; failures : string list
  }

type comparison_error =
  | Empty_task_name
  | Negative_metric of
      { task_name : string
      ; mode : string
      ; field : string
      ; value : int
      }
  | Nonpositive_json_llm_calls of string

val default_gate : gate
val show_comparison_error : comparison_error -> string

val compare_task
  :  task_name:string
  -> json_mode:mode_metrics
  -> snippet_mode:mode_metrics
  -> (comparison, comparison_error) result

val evaluate : ?gate:gate -> comparison list -> gate_result
val metrics_of_gate_result : gate_result -> Eval.metric list
val verdict_of_gate_result : gate_result -> Harness.verdict

(** [is_experiment_enabled ?getenv ()] reads the experiment flag through the
    canonical environment boundary by default. [?getenv] lets tests/callers
    avoid reading process env directly. *)
val is_experiment_enabled : ?getenv:(string -> string option) -> unit -> bool

val require_experiment_enabled
  :  ?getenv:(string -> string option)
  -> unit
  -> (unit, string) result
