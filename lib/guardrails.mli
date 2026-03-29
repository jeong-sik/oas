(** Guardrails for tool filtering and execution limits.

    @stability Stable
    @since 0.93.1 *)

(** Tool filter: controls which tools are visible to the LLM *)
type tool_filter =
  | AllowAll
  | AllowList of string list
  | DenyList of string list
  | Custom of (Types.tool_schema -> bool)

(** Source of a tool policy, for audit logging. *)
type policy_source = Agent | Operator
[@@deriving show]

type t = {
  tool_filter: tool_filter;
  max_tool_calls_per_turn: int option;
}

val default : t
val is_allowed : t -> Types.tool_schema -> bool
val filter_tools : t -> Tool.t list -> Tool.t list
val exceeds_limit : t -> int -> bool

(** Merge an operator-level tool policy with agent-level guardrails.
    When [operator] is [Some], it overrides the agent's [tool_filter].
    Returns the merged guardrails and the source that determined the
    effective [tool_filter].

    @since 0.94.0 *)
val merge_operator_policy :
  operator:tool_filter option -> agent:t -> t * policy_source
