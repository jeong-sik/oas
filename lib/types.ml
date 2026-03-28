(** Core types for Anthropic Agent SDK.

    LLM-level types (role, message, content_block, etc.) are defined in
    {!Llm_provider.Types} and re-exported here for backward compatibility.
    Agent-specific types (model, agent_config, agent_state) remain local. *)

(* ================================================================ *)
(* Re-export all LLM provider types                                  *)
(* ================================================================ *)
include Llm_provider.Types

(* ================================================================ *)
(* tool_choice JSON parsing -- depends on OAS Error module           *)
(* ================================================================ *)

let tool_choice_of_json json =
  let open Yojson.Safe.Util in
  try
    match json |> member "type" |> to_string with
    | "auto" -> Ok Auto
    | "any" -> Ok Any
    | "tool" ->
      let name = json |> member "name" |> to_string in
      Ok (Tool name)
    | "none" -> Ok None_
    | other -> Error (Error.Serialization (UnknownVariant { type_name = "tool_choice"; value = other }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Invalid tool_choice JSON: %s" msg }))

(* ================================================================ *)
(* Agent-specific types (internal to OAS)                             *)
(* ================================================================ *)

(** Model identifier — a plain string.
    Use {!Model_registry.resolve_model_id} to resolve aliases like
    "sonnet" → "claude-sonnet-4-6-20250514". *)
type model = string
[@@deriving yojson, show]

(** Resolve a model alias to its canonical API model ID.
    Delegates to {!Model_registry.resolve_model_id}. *)
let model_to_string = Model_registry.resolve_model_id

(** Agent configuration *)
type agent_config = {
  name: string;
  model: model;
  system_prompt: string option;
  max_tokens: int;
  max_turns: int;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  response_format_json: bool;
  thinking_budget: int option; (* For Claude 3.7+ extended thinking *)
  tool_choice: tool_choice option;
  disable_parallel_tool_use: bool; (* Anthropic: tool_choice.disable_parallel_tool_use, OpenAI: parallel_tool_calls=false *)
  cache_system_prompt: bool; (* Wrap system prompt with cache_control ephemeral *)
  max_input_tokens: int option; (* Token budget: max cumulative input tokens *)
  max_total_tokens: int option; (* Token budget: max cumulative total tokens *)
  initial_messages: message list; (* Seed conversation with prior history on first run *)
  max_cost_usd: float option; (* Cost budget: max cumulative estimated cost in USD. @since 0.62.0 *)
  context_compact_ratio: float option;   (** Ratio of context budget at which to compact. Default 0.8 *)
  context_prepare_ratio: float option;   (** Ratio at which to start preparing for compaction. Default 0.6 *)
  context_handoff_ratio: float option;   (** Ratio at which to trigger handoff. Default 0.95 *)
}
[@@deriving show]

let default_config = {
  name = "agent";
  model = Model_registry.default_model_id;
  system_prompt = None;
  max_tokens = 4096;
  max_turns = 10;
  temperature = None;
  top_p = None;
  top_k = None;
  min_p = None;
  enable_thinking = None;
  response_format_json = false;
  thinking_budget = None;
  tool_choice = None;
  disable_parallel_tool_use = false;
  cache_system_prompt = false;
  max_input_tokens = None;
  max_total_tokens = None;
  initial_messages = [];
  max_cost_usd = None;
  context_compact_ratio = None;
  context_prepare_ratio = None;
  context_handoff_ratio = None;
}

(* Usage tracking *)
type usage_stats = {
  total_input_tokens: int;
  total_output_tokens: int;
  total_cache_creation_input_tokens: int;
  total_cache_read_input_tokens: int;
  api_calls: int;
  estimated_cost_usd: float;
}
[@@deriving show]

let empty_usage = {
  total_input_tokens = 0;
  total_output_tokens = 0;
  total_cache_creation_input_tokens = 0;
  total_cache_read_input_tokens = 0;
  api_calls = 0;
  estimated_cost_usd = 0.0;
}

let add_usage stats (u : api_usage) =
  { total_input_tokens = stats.total_input_tokens + u.input_tokens;
    total_output_tokens = stats.total_output_tokens + u.output_tokens;
    total_cache_creation_input_tokens = stats.total_cache_creation_input_tokens + u.cache_creation_input_tokens;
    total_cache_read_input_tokens = stats.total_cache_read_input_tokens + u.cache_read_input_tokens;
    api_calls = stats.api_calls + 1;
    estimated_cost_usd = stats.estimated_cost_usd }

(** Agent state *)
type agent_state = {
  config: agent_config;
  messages: message list;
  turn_count: int;
  usage: usage_stats;
}
[@@deriving show]
