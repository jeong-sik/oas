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
type tool_call_fingerprint = {
  fp_name: string;
  fp_input: string;
}

(** Compute fingerprints from content blocks containing [ToolUse]. *)
val compute_fingerprints : Types.content_block list -> tool_call_fingerprint list

(** Return [true] when [current] fingerprints match [prev] exactly. *)
val is_idle : tool_call_fingerprint list option -> tool_call_fingerprint list -> bool

(** {1 Turn preparation} *)

(** Pre-processed inputs for an LLM turn. *)
type turn_preparation = {
  tools_json: Yojson.Safe.t list option;
  effective_messages: Types.message list;
  effective_guardrails: Guardrails.t;
}

(** Prepare tool schemas, applying operator policy and optional
    [tool_filter_override].

    When [tool_selector] is provided, the visible tool set is narrowed
    by [Tool_selector.select] using the last user message as context
    before converting to JSON schemas.

    Priority: [turn_params.tool_filter_override] > [operator_policy] > [guardrails]
    Then: [tool_selector] narrows the guardrails-filtered set.

    @since 0.94.0 added [operator_policy] parameter
    @since 0.100.0 added [tool_selector] and [messages] parameters *)
val prepare_tools :
  guardrails:Guardrails.t ->
  operator_policy:Guardrails.tool_filter option ->
  policy_channel:Policy_channel.t option ->
  tools:Tool_set.t ->
  turn_params:Hooks.turn_params ->
  ?tool_selector:Tool_selector.strategy ->
  ?messages:Types.message list ->
  unit ->
  Yojson.Safe.t list option * Guardrails.t

(** Reduce messages and inject extra system context. *)
val prepare_messages :
  messages:Types.message list ->
  context_reducer:Context_reducer.t option ->
  turn_params:Hooks.turn_params ->
  Types.message list

(** Full turn preparation: tools + messages + guardrails.

    @since 0.94.0 added [operator_policy] parameter
    @since 0.100.0 added [tool_selector] parameter *)
val prepare_turn :
  guardrails:Guardrails.t ->
  operator_policy:Guardrails.tool_filter option ->
  policy_channel:Policy_channel.t option ->
  tools:Tool_set.t ->
  messages:Types.message list ->
  context_reducer:Context_reducer.t option ->
  turn_params:Hooks.turn_params ->
  ?tool_selector:Tool_selector.strategy ->
  unit ->
  turn_preparation

(** {1 Usage accumulation} *)

(** Accumulate response usage into running totals, including cost estimation. *)
val accumulate_usage :
  current_usage:Types.usage_stats ->
  provider:Provider.config option ->
  response_usage:Types.api_usage option ->
  Types.usage_stats

(** {1 Turn params resolution} *)

(** Resolve per-turn parameters by invoking the [BeforeTurnParams] hook. *)
val resolve_turn_params :
  hooks:Hooks.hooks ->
  messages:Types.message list ->
  invoke_hook:(hook_name:string ->
    (Hooks.hook_event -> Hooks.hook_decision) option ->
    Hooks.hook_event ->
    Hooks.hook_decision) ->
  Hooks.turn_params

(** {1 Context injection} *)

(** Filter extra messages to avoid consecutive same-role entries. *)
val filter_valid_messages :
  messages:Types.message list ->
  Types.message list ->
  Types.message list

(** Apply context injector after tool execution, updating context and messages. *)
val apply_context_injection :
  context:Context.t ->
  messages:Types.message list ->
  injector:Hooks.context_injector ->
  tool_uses:Types.content_block list ->
  results:Agent_tools.tool_execution_result list ->
  Types.message list

(** {1 Token budget} *)

(** Check input/total token budgets; return an error if exceeded. *)
val check_token_budget :
  Types.agent_config -> Types.usage_stats -> Error.sdk_error option

(** {1 Idle state tracking} *)

type idle_state = {
  last_tool_calls: tool_call_fingerprint list option;
  consecutive_idle_turns: int;
}

type idle_result = {
  new_state: idle_state;
  is_idle: bool;
}

(** Update idle detection state after a tool-use turn. *)
val update_idle_detection :
  idle_state:idle_state ->
  tool_uses:Types.content_block list ->
  idle_result

(** {1 Tool result construction} *)

(** Convert tool execution results into [ToolResult] content blocks. *)
val make_tool_results :
  Agent_tools.tool_execution_result list -> Types.content_block list
