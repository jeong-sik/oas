(** Statistical utilities for eval regression detection.

    Provides summary statistics, confidence intervals, and
    Welch's t-test for comparing two samples.

    @since 0.79.0 *)

(** Summary statistics for a sample. *)
type stats = {
  n: int;
  mean: float;
  std_dev: float;
  min_val: float;
  max_val: float;
}

(** Compute summary statistics for a list of floats.
    Returns [None] for empty lists. *)
val summary_stats : float list -> stats option

(** Compute a confidence interval using the normal approximation.
    Returns [(lower, upper)] bounds.
    [confidence] should be between 0 and 1 (e.g. 0.95 for 95% CI).
    Returns [None] if sample size < 2. *)
val confidence_interval : float list -> confidence:float -> (float * float) option

(** Detect regression using Welch's t-test.
    Returns [true] if the current sample is significantly worse
    (higher values) than baseline at the 0.05 significance level.
    Returns [false] if either sample has fewer than 2 elements. *)
val is_regression : baseline:float list -> current:float list -> bool

(** Compute Cohen's d effect size between two samples.
    Returns [None] if either sample has fewer than 2 elements or pooled std_dev is 0. *)
val effect_size : float list -> float list -> float option
