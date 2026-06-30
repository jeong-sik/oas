(** Shared turn logic for sync and streaming paths.

    Contains helper functions that both [Agent.run_turn_with_trace] and
    [Agent.run_turn_stream_with_trace] call, eliminating code duplication.

    These functions take explicit parameters (not [Agent.t]) to avoid
    circular module dependency: [Agent -> Agent_turn] is fine,
    [Agent_turn -> Agent] is not.

    @stability Internal
    @since 0.93.1 *)

(** {1 Idle detection} *)

(** Fingerprint of a single tool call for idle detection. *)
type tool_call_fingerprint =
  { fp_name : string
  ; fp_input : string
  }

(** Optional normalizer applied before fingerprinting a tool call.

    Use this when the execution path accepts public aliases or normalizes
    arguments before dispatch, so idle detection compares the same semantic
    call the executor will run rather than only the raw model spelling.

    Returning the original [(name, input)] preserves legacy exact behavior. *)
type tool_call_normalizer = name:string -> input:Yojson.Safe.t -> string * Yojson.Safe.t

(** Granularity at which two fingerprints are considered the "same"
    for idle detection.

    - [Exact] (default, pre-0.161 behaviour): both [fp_name] and
      [fp_input] must match byte-for-byte.
    - [Name_only]: only [fp_name] is compared. Catches polling loops
      that alternate arguments (e.g. [status(x)] -> [status(y)] ->
      [status(x)]) and cross-tool polls at the tool-name level.
    - [Name_and_subset keys]: placeholder for future argument-subset
      matching — currently behaves as [Name_only]. The [keys] list is
      carried through for typecheck stability, but its semantics
      (JSON field extraction) are left for a follow-up leaf. See #896.

    @since 0.161.0 *)
type idle_granularity =
  | Exact
  | Name_only
  | Name_and_subset of string list

(** Compute fingerprints from content blocks containing [ToolUse]. *)
val compute_fingerprints
  :  ?normalize_tool_call:tool_call_normalizer
  -> Types.content_block list
  -> tool_call_fingerprint list

(** Return [true] when [current] fingerprints match [prev] at the
    given granularity. Default [?granularity] is [Exact] — preserves
    the pre-0.161 semantics for every existing caller.
    @since 0.161.0 [?granularity] added (#896). *)
val is_idle
  :  ?granularity:idle_granularity
  -> tool_call_fingerprint list option
  -> tool_call_fingerprint list
  -> bool

(** {1 Turn preparation} *)

(** Pre-processed inputs for an LLM turn. *)
type turn_preparation =
  { tools_json : Yojson.Safe.t list option
  ; effective_messages : Types.message list
  ; effective_guardrails : Guardrails.t
  ; visible_tool_names : string list
    (** Names of the tools that survived guardrails + operator policy
        + tool_filter_override + tool_selector. This is exactly the
        list the LLM sees this turn — not the agent's full tool
        registry. Useful for [Event_bus.TurnReady] subscribers and
        deterministic substrate observability. Empty list when no
        tools are presented to the LLM.

        Order matches [tools_json]: tool_selector ordering is
        preserved when present, otherwise the guardrail-filtered
        order from [Tool_set.to_list].

        @since 0.162.0 *)
  ; runtime_mcp_policy : Llm_provider.Llm_transport.runtime_mcp_policy option
    (** Request-scoped runtime MCP policy for this prepared turn.

        This is narrowed by the same effective guardrails that produced
        [visible_tool_names] before dispatch. A raw agent-level policy must not
        be used directly after per-turn [tool_filter_override], or CLI runtime
        MCP tools can remain broader than [TurnReady] and inline tool
        visibility claim.

        @since 0.194.1 *)
  }

(** Prepare tool schemas, applying operator policy and optional
    [tool_filter_override].

    When [tool_selector] is provided, the visible tool set is narrowed
    by [Tool_selector.select] using the last user message as context
    before converting to JSON schemas.

    Priority: [turn_params.tool_filter_override] > [operator_policy] > [guardrails]
    Then: [tool_selector] narrows the guardrails-filtered set.

    [disclosure_level] controls how each surviving tool's schema is
    serialized. Default [Tool.Full_schema] preserves legacy behavior
    byte-for-byte. See {!Tool.disclosure_level}.

    @since 0.94.0 added [operator_policy] parameter
    @since 0.100.0 added [tool_selector] and [messages] parameters
    @since 0.194.0 added [disclosure_level] parameter *)
val prepare_tools
  :  guardrails:Guardrails.t
  -> operator_policy:Guardrails.tool_filter option
  -> policy_channel:Policy_channel.t option
  -> tools:Tool_set.t
  -> turn_params:Hooks.turn_params
  -> ?tool_selector:Tool_selector.strategy
  -> ?messages:Types.message list
  -> ?disclosure_level:Tool.disclosure_level
  -> unit
  -> Yojson.Safe.t list option * string list * Guardrails.t
(** Returns [(tools_json, visible_tool_names, effective_guardrails)].
    [visible_tool_names] mirrors the order of [tools_json] and is empty
    when no tools survive filtering.

    @since 0.162.0 third tuple element added (visible_tool_names) *)

(** Reduce messages and inject extra system context. *)

val apply_context_reducer
  :  preserve_thinking:bool
  -> messages:Types.message list
  -> context_reducer:Context_reducer.t option
  -> Types.message list

val prepare_messages
  :  ?config:Types.agent_config
  -> messages:Types.message list
  -> context_reducer:Context_reducer.t option
  -> turn_params:Hooks.turn_params
  -> unit
  -> Types.message list

(** Full turn preparation: tools + messages + guardrails.

    @since 0.94.0 added [operator_policy] parameter
    @since 0.100.0 added [tool_selector] parameter
    @since 0.185.0 added optional [config] parameter so the call-time
      pruner can read [call_time_pruner_keep_recent] /
      [call_time_pruner_keep_last] from the agent configuration.
      Omitting it preserves the historical defaults [2] / [100]. *)
val prepare_turn
  :  ?config:Types.agent_config
  -> guardrails:Guardrails.t
  -> operator_policy:Guardrails.tool_filter option
  -> policy_channel:Policy_channel.t option
  -> tools:Tool_set.t
  -> messages:Types.message list
  -> context_reducer:Context_reducer.t option
  -> turn_params:Hooks.turn_params
  -> ?tool_selector:Tool_selector.strategy
  -> ?disclosure_level:Tool.disclosure_level
  -> unit
  -> turn_preparation

(** {1 Usage accumulation} *)

(** Accumulate response usage into running totals, including cost estimation. *)
val accumulate_usage
  :  current_usage:Types.usage_stats
  -> provider:Provider.config option
  -> response_usage:Types.api_usage option
  -> Types.usage_stats

(** {1 Turn params resolution} *)

(** Resolve per-turn parameters by invoking the [BeforeTurnParams] hook. *)
val resolve_turn_params
  :  hooks:Hooks.hooks
  -> messages:Types.message list
  -> max_turns:int
  -> turn:int
  -> invoke_hook:
       (hook_name:string
        -> (Hooks.hook_event -> Hooks.hook_decision) option
        -> Hooks.hook_event
        -> Hooks.hook_decision)
  -> Hooks.turn_params

(** {1 Context injection} *)

(** Filter extra messages to avoid consecutive same-role entries. *)
val filter_valid_messages
  :  messages:Types.message list
  -> Types.message list
  -> Types.message list

(** Apply context injector after tool execution, updating context and messages. *)
val apply_context_injection
  :  context:Context.t
  -> messages:Types.message list
  -> injector:Hooks.context_injector
  -> tool_uses:Types.content_block list
  -> results:Agent_tools.tool_execution_result list
  -> Types.message list

(** {1 Idle state tracking} *)

type idle_state =
  { last_tool_calls : tool_call_fingerprint list option
  ; consecutive_idle_turns : int
  }

type idle_result =
  { new_state : idle_state
  ; is_idle : bool
  }

(** Update idle detection state after a tool-use turn. *)
val update_idle_detection
  :  idle_state:idle_state
  -> tool_uses:Types.content_block list
  -> idle_result

(** Update idle detection state after a tool-use turn, normalizing each
    [ToolUse] before fingerprinting. *)
val update_idle_detection_with_normalizer
  :  normalize_tool_call:tool_call_normalizer
  -> idle_state:idle_state
  -> tool_uses:Types.content_block list
  -> idle_result

(** Reset idle detection state after a non-tool-use turn or an idle Skip. *)
val reset_idle_detection : unit -> idle_result

(** {1 Tool result construction} *)

(** Default per-tool-result character cap (50,000).
    @since 0.127.0 *)
val default_max_tool_result_chars : int

(** Convert tool execution results into [ToolResult] content blocks.

    When [~relocation] is provided, results exceeding the store's
    threshold are persisted to disk and replaced with a preview.
    The {!Content_replacement_state} records each decision so that
    subsequent turns re-apply the same preview without I/O.

    When [~max_result_chars] > 0 (default {!default_max_tool_result_chars}),
    individual results exceeding that limit are truncated at creation time
    with a marker showing the original size.  This acts as a hard safety
    net after relocation.  Pass [~max_result_chars:0] to disable.

    Order: relocation first, then truncation.

    @since 0.127.0 added [max_result_chars] parameter
    @since 0.128.0 added [relocation] parameter *)
val make_tool_results
  :  ?max_result_chars:int
  -> ?event_bus:Event_bus.t
  -> ?correlation_id:string
  -> ?run_id:string
  -> ?relocation:Tool_result_store.t * Content_replacement_state.t
  -> Agent_tools.tool_execution_result list
  -> Types.content_block list
