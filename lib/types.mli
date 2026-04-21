(** Core types for Anthropic Agent SDK.

    LLM-level types (role, message, content_block, etc.) are re-exported from
    {!Llm_provider.Types}. Agent-specific types remain local.

    @stability Stable
    @since 0.93.1 *)

(* ================================================================ *)
(* Re-export all LLM provider types                                  *)
(* ================================================================ *)
include module type of struct include Llm_provider.Types end

(* ================================================================ *)
(* OAS-specific additions                                            *)
(* ================================================================ *)

val tool_choice_of_json : Yojson.Safe.t -> (tool_choice, Error.sdk_error) result
val response_format_of_json :
  Yojson.Safe.t -> (response_format, Error.sdk_error) result

(* ================================================================ *)
(* Agent-specific types                                              *)
(* ================================================================ *)

(** Model identifier — a plain string.
    Use {!Model_registry.resolve_model_id} to resolve aliases. *)
type model = string
[@@deriving yojson, show]

(** Resolve a model alias to its canonical API model ID. *)
val model_to_string : model -> string

type tiered_memory = {
  long_term: string option;
  mid_term: string option;
  short_term: string option;
}

(** Agent configuration *)
type agent_config = {
  name: string;
  model: model;
  system_prompt: string option;
  max_tokens: int option;
  max_turns: int;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  response_format: response_format;
  thinking_budget: int option;
  tool_choice: tool_choice option;
  disable_parallel_tool_use: bool;
  cache_system_prompt: bool;
  cache_extended_ttl: bool;
  (** When [true] and [cache_system_prompt = true], sets the Anthropic
      prompt cache TTL to 1 hour instead of the default 5 minutes.
      Write cost is 2x but the cache persists 12x longer — intended
      for long-running agents that go idle 5+ minutes between turns.
      @since 0.151.0 *)
  max_input_tokens: int option;
  max_total_tokens: int option;
  initial_messages: message list;
  max_cost_usd: float option;
  context_compact_ratio: float option;
  context_prepare_ratio: float option;
  context_handoff_ratio: float option;
  priority: Llm_provider.Request_priority.t option;  (** @since 0.96.0 *)
  yield_on_tool: bool;  (** Release LLM slot during tool execution. @since 0.100.0 *)
  exit_condition: (int -> bool [@opaque]) option;  (** Custom exit predicate called with turn_count after each turn. When it returns true the agent loop exits cleanly. @since 0.115.0 *)
}
[@@deriving show]

val default_config : agent_config

(** Usage tracking *)
type usage_stats = {
  total_input_tokens: int;
  total_output_tokens: int;
  total_cache_creation_input_tokens: int;
  total_cache_read_input_tokens: int;
  api_calls: int;
  estimated_cost_usd: float;
}
[@@deriving show]

val empty_usage : usage_stats
val add_usage : usage_stats -> api_usage -> usage_stats

(** Agent state *)
type agent_state = {
  config: agent_config;
  messages: message list;
  turn_count: int;
  usage: usage_stats;
}
[@@deriving show]
