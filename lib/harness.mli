(** Test harness framework for agent verification.

    Provides pluggable verification layers: Behavioral, Adversarial,
    Performance, Regression, Swiss Cheese, and Composability.
    Harnesses compose via the Swiss Cheese combinator for
    multi-layer independent verification. *)

(** {1 Common types} *)

type verdict = {
  passed: bool;
  score: float option;
  evidence: string list;
  detail: string option;
}

type 'obs layer = {
  name: string;
  check: 'obs -> bool;
  evidence: 'obs -> string;
}

type 'obs layer_result = {
  layer_name: string;
  layer_passed: bool;
  layer_evidence: string;
}

type 'obs swiss_verdict = {
  all_passed: bool;
  layer_results: 'obs layer_result list;
  coverage: float;
}

(** {1 Behavioral harness} *)

module Behavioral : sig
  type expectation =
    | ToolSelected of string list
    | CompletesWithin of int
    | ContainsText of string
    | All of expectation list

  type observation = {
    tools_called: string list;
    turn_count: int;
    final_response: string;
    messages: Types.message list;
  }

  val observe :
    Agent.t ->
    (Types.api_response, Error.sdk_error) result ->
    observation

  val evaluate : observation -> expectation -> verdict
end

(** {1 Adversarial harness} *)

module Adversarial : sig
  type adversarial_input =
    | MalformedJson of string
    | PromptInjection of string
    | ToolError of { tool_name: string; error: string }
    | OversizedInput of { size: int }

  type expectation =
    | GracefulError
    | NoToolExecution
    | ErrorContains of string

  type observation = {
    result: (Types.api_response, Error.sdk_error) result;
    tools_executed: string list;
    error_message: string option;
  }

  val evaluate : observation -> expectation -> verdict
end

(** {1 Performance harness} *)

module Performance : sig
  type observation = {
    latencies_ms: float list;
    total_tokens: int;
    total_cost_usd: float;
    turn_count: int;
  }

  type expectation = {
    max_p95_latency_ms: float option;
    max_total_tokens: int option;
    max_cost_usd: float option;
    max_turns: int option;
  }

  val default_expectation : expectation
  val p95 : float list -> float
  val evaluate : observation -> expectation -> verdict
end

(** {1 Regression harness (golden file)} *)

module Regression : sig
  type match_mode =
    | ExactMatch
    | StructuralMatch of (Yojson.Safe.t -> Yojson.Safe.t -> bool)
    | FuzzyMatch of { threshold: float }

  type observation = {
    output_json: Yojson.Safe.t;
    output_text: string;
  }

  val evaluate : mode:match_mode -> observation -> string -> verdict
end

(** {1 Swiss Cheese combinator} *)

module Swiss_cheese : sig
  val evaluate_layers : 'obs layer list -> 'obs -> 'obs swiss_verdict
  val require_all : 'obs layer list -> 'obs -> verdict
  val require_n : int -> 'obs layer list -> 'obs -> verdict
end

(** {1 Composability harness} *)

module Composability : sig
  type scenario =
    | SingleAgent
    | Handoff of { parent: string; targets: string list }
    | Orchestrated of { agents: string list }
    | Pipeline of { stages: string list }

  type expectation =
    | HandoffOccurred of string
    | AllAgentsCompleted
    | ContextPropagated of string
    | TurnCountBelow of int

  type observation = {
    agents_involved: string list;
    handoffs_observed: (string * string) list;
    all_completed: bool;
    context_keys: string list;
    total_turns: int;
  }

  val evaluate : observation -> expectation -> verdict
end
