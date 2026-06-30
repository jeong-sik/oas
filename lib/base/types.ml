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
    | other ->
      Error
        (Error.Serialization (UnknownVariant { type_name = "tool_choice"; value = other }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error
      (Error.Serialization
         (JsonParseError { detail = Printf.sprintf "Invalid tool_choice JSON: %s" msg }))
;;

let response_format_of_json json =
  let open Yojson.Safe.Util in
  try
    match json with
    | `Bool enabled -> Ok (response_format_of_json_mode enabled)
    | `Null -> Ok Off
    | `Assoc _ ->
      (match json |> member "type" |> to_string with
       | "off" -> Ok Off
       | "json_mode" -> Ok JsonMode
       | "json_schema" ->
         (match json |> member "schema" with
          | `Null ->
            Error
              (Error.Serialization
                 (JsonParseError
                    { detail = "Invalid response_format JSON: missing schema" }))
          | schema -> Ok (JsonSchema schema))
       | other ->
         Error
           (Error.Serialization
              (UnknownVariant { type_name = "response_format"; value = other })))
    | _ ->
      Error
        (Error.Serialization
           (JsonParseError { detail = "Invalid response_format JSON: expected object" }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error
      (Error.Serialization
         (JsonParseError
            { detail = Printf.sprintf "Invalid response_format JSON: %s" msg }))
;;

(* ================================================================ *)
(* Agent-specific types (internal to OAS)                             *)
(* ================================================================ *)

(** Model identifier — a plain string.
    Use {!Model_registry.resolve_model_id} to resolve aliases like
    "sonnet" → "claude-sonnet-4-6-20250514". *)
type model = string [@@deriving yojson, show]

(** Resolve a model alias to its canonical API model ID.
    Delegates to {!Model_registry.resolve_model_id}. *)
let model_to_string = Model_registry.resolve_model_id

(** Default proactive context compaction watermark. Pipeline code must read this
    value through [agent_config.context_compact_ratio] resolution rather than
    process-global environment state, so the agent config remains the SSOT. *)
let default_context_compact_ratio = 0.9

(** Default budget ratio for the context reducer's normal compaction path.

    This is intentionally distinct from {!default_context_compact_ratio}: the
    budget ratio controls how much of [max_tokens] the reducer may consume
    during normal compaction, while the watermark ratio triggers proactive
    compaction in the pipeline. *)
let default_context_compact_budget_ratio = 0.8

let valid_context_ratio ratio = ratio > 0.0 && ratio < 1.0

(** Require [ratio] to be a valid context ratio, or raise [Invalid_argument].

    [name] is included in the error message so callers can identify which
    argument failed validation. *)
let require_context_ratio ~name ratio =
  if valid_context_ratio ratio
  then ratio
  else invalid_arg (Printf.sprintf "%s must be > 0.0 and < 1.0" name)
;;

(** Agent configuration *)
type agent_config =
  { name : string
  ; model : model
  ; system_prompt : string option
  ; max_tokens : int option
    (** [None] = resolve from model capabilities at request time *)
  ; max_turns : int
  ; temperature : float option
  ; top_p : float option
  ; top_k : int option
  ; min_p : float option
  ; enable_thinking : bool option
  ; preserve_thinking : bool option
  ; response_format : response_format
  ; thinking_budget : int option (* For Claude 3.7+ extended thinking *)
  ; tool_choice : tool_choice option
  ; disable_parallel_tool_use : bool
    (* Anthropic: tool_choice.disable_parallel_tool_use, Openai: parallel_tool_calls=false *)
  ; cache_system_prompt : bool (* Wrap system prompt with cache_control ephemeral *)
  ; cache_extended_ttl : bool (* true=1h TTL (2x write cost), false=5min default *)
  ; initial_messages : message list
    (* Seed conversation with prior history on first run *)
  ; context_compact_ratio : float option
    (** Proactive compaction watermark: ratio of the context window at which the
        pipeline triggers compaction. Default:
        {!default_context_compact_ratio}. This is the watermark ratio, not the
        reducer's budget ratio; see {!default_context_compact_budget_ratio}. *)
  ; context_prepare_ratio : float option
    (** Ratio at which to start preparing for compaction. Must be in (0.0, 1.0).
        Default 0.6. *)
  ; context_handoff_ratio : float option
    (** Ratio at which to trigger handoff. Must be in (0.0, 1.0). Default 0.95. *)
  ; priority : Llm_provider.Request_priority.t option
    (** Scheduling priority for LLM requests. @since 0.96.0 *)
  ; yield_on_tool : bool
    (** Release LLM slot during tool execution, re-acquire before next turn. @since 0.100.0 *)
  ; exit_condition : ((int -> bool)[@opaque]) option
    (** Custom exit predicate called with turn_count after each turn. @since 0.115.0 *)
  ; ensure_final_text : bool
    (** When [true], a run must not terminate with tool activity but no
        user-facing final text (downstream renders this as "Tool-only turn
        ended without a final reply"). If the run is about to end that way —
        either a terminal turn with no visible text, or [max_turns] reached
        after a tool turn — the agent performs exactly ONE additional model
        turn with the tool set withheld, so the model itself authors a textual
        answer. This is convergence, not a cap: it adds no turn/token limit and
        the answer is LLM-authored. Default [false] preserves prior behavior. *)
  ; call_time_pruner_keep_recent : int
    (** Number of most recent turns whose tool results are NOT stubbed by the
        call-time pruner in [Agent_turn.prepare_messages].  Default 2. *)
  ; call_time_pruner_keep_last : int
    (** Maximum number of most recent turns retained by the call-time pruner
        in [Agent_turn.prepare_messages].  Default 100. *)
  }
[@@deriving show]

let default_config_value ?getenv () =
  { name = "agent"
  ; model = Model_registry.default_model_id_value ?getenv ()
  ; system_prompt = None
  ; max_tokens = None
  ; max_turns = 10
  ; temperature = None
  ; top_p = None
  ; top_k = None
  ; min_p = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; response_format = Off
  ; thinking_budget = None
  ; tool_choice = None
  ; disable_parallel_tool_use = false
  ; cache_system_prompt = false
  ; cache_extended_ttl = false
  ; initial_messages = []
  ; context_compact_ratio = None
  ; context_prepare_ratio = None
  ; context_handoff_ratio = None
  ; priority = None
  ; yield_on_tool = false
  ; exit_condition = None
  ; ensure_final_text = false
  ; call_time_pruner_keep_recent = 2
  ; call_time_pruner_keep_last = 100
  }
;;

let default_config = default_config_value ()

(** Usage tracking accumulated across provider calls. Per-response usage stays
    in [Llm_provider.Types.api_usage].

    [unpriced_model] is [Some model_id] when at least one accumulated turn
    ran a model with no entry in [Pricing.pricing_for_model_opt], so
    [estimated_cost_usd] under-reports.  Only the first such model_id is
    recorded for stable telemetry; cost thresholds never gate execution. *)
type usage_stats =
  { total_input_tokens : int
  ; total_output_tokens : int
  ; total_cache_creation_input_tokens : int
  ; total_cache_read_input_tokens : int
  ; api_calls : int
  ; estimated_cost_usd : float
  ; unpriced_model : string option
  }
[@@deriving show]

let empty_usage =
  { total_input_tokens = 0
  ; total_output_tokens = 0
  ; total_cache_creation_input_tokens = 0
  ; total_cache_read_input_tokens = 0
  ; api_calls = 0
  ; estimated_cost_usd = 0.0
  ; unpriced_model = None
  }
;;

let add_usage stats (u : api_usage) =
  { total_input_tokens = stats.total_input_tokens + u.input_tokens
  ; total_output_tokens = stats.total_output_tokens + u.output_tokens
  ; total_cache_creation_input_tokens =
      stats.total_cache_creation_input_tokens + u.cache_creation_input_tokens
  ; total_cache_read_input_tokens =
      stats.total_cache_read_input_tokens + u.cache_read_input_tokens
  ; api_calls = stats.api_calls + 1
  ; estimated_cost_usd = stats.estimated_cost_usd
  ; unpriced_model = stats.unpriced_model
  }
;;

(** Agent state *)
type agent_state =
  { config : agent_config
  ; messages : message list
  ; turn_count : int
  ; usage : usage_stats
  }
[@@deriving show]
