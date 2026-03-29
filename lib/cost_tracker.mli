(** Per-agent cost tracking and budget enforcement.

    @since 0.62.0

    @stability Evolving
    @since 0.93.1 *)

(** Structured cost report. *)
type cost_report = {
  total_usd: float;
  input_tokens: int;
  output_tokens: int;
  cache_creation_tokens: int;
  cache_read_tokens: int;
  api_calls: int;
  avg_cost_per_call: float;
}

(** Check whether accumulated cost exceeds [config.max_cost_usd].

    Returns [Some (Error.Agent (CostBudgetExceeded _))] when over budget,
    [None] when within budget or no limit is set. *)
val check_budget : Types.agent_config -> Types.usage_stats -> Error.sdk_error option

(** Generate a structured cost report from usage stats. *)
val report : Types.usage_stats -> cost_report

(** Format a cost report as a human-readable string. *)
val report_to_string : cost_report -> string
