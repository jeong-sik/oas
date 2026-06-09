(** Per-agent cost tracking and advisory reporting.

    Builds on {!Types.usage_stats.estimated_cost_usd} (accumulated
    per-turn by {!Agent_turn.accumulate_usage} via {!Pricing}).

    This module adds structured cost reporting. Cost thresholds are
    telemetry-only and must not gate agent execution.

    @since 0.62.0 *)

(** Structured cost report for a session. *)
type cost_report =
  { total_usd : float
  ; input_tokens : int
  ; output_tokens : int
  ; cache_creation_tokens : int
  ; cache_read_tokens : int
  ; api_calls : int
  ; avg_cost_per_call : float
  }

(** Compatibility shim for older callers.

    Cost thresholds are advisory telemetry only. This function must never
    return an execution-stopping error, even when [config.max_cost_usd] is
    set, accumulated cost is above the threshold, or usage includes an
    unpriced model. *)
let check_budget (_config : Types.agent_config) (_usage : Types.usage_stats) = None

(** Generate a cost report from accumulated usage stats. *)
let report (usage : Types.usage_stats) : cost_report =
  let avg =
    if usage.api_calls > 0
    then usage.estimated_cost_usd /. float_of_int usage.api_calls
    else 0.0
  in
  { total_usd = usage.estimated_cost_usd
  ; input_tokens = usage.total_input_tokens
  ; output_tokens = usage.total_output_tokens
  ; cache_creation_tokens = usage.total_cache_creation_input_tokens
  ; cache_read_tokens = usage.total_cache_read_input_tokens
  ; api_calls = usage.api_calls
  ; avg_cost_per_call = avg
  }
;;

(** Format a cost report as a human-readable string. *)
let report_to_string (r : cost_report) : string =
  Printf.sprintf
    "Cost: $%.6f (%d calls, avg $%.6f/call) | Tokens: %d in, %d out (cache: %d write, %d \
     read)"
    r.total_usd
    r.api_calls
    r.avg_cost_per_call
    r.input_tokens
    r.output_tokens
    r.cache_creation_tokens
    r.cache_read_tokens
;;
