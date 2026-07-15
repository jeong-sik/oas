(** Per-agent cost tracking and advisory reporting.

    @since 0.62.0

    @stability Evolving
    @since 0.93.1 *)

(** Structured cost report. *)
type cost_report =
  { total_usd : float
  ; input_tokens : int
  ; output_tokens : int
  ; cache_creation_tokens : int
  ; cache_read_tokens : int
  ; cache_miss_input_tokens : int
  ; api_calls : int
  ; avg_cost_per_call : float
  ; pricing_gap : Types.pricing_gap option
  }

(** Generate a structured cost report from usage stats. *)
val report : Types.usage_stats -> cost_report

(** Format a cost report as a human-readable string. *)
val report_to_string : cost_report -> string
