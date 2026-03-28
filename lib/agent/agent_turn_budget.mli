(** Agent_turn_budget — Self-extending turn budget with guardrails.

    Agents start with an initial turn budget and can request more via
    the [extend_turns] tool.  Each extension request is gated by:

    - Absolute ceiling (hard cap, never exceeded)
    - Per-extension cap (max turns per single request)
    - Total extension count (max number of extend calls)

    Cost and idle checks are delegated to the caller via the
    agent ref (reads [agent_state.usage] and [consecutive_idle_turns]).

    @since 0.78.0

    @stability Internal
    @since 0.93.0 *)

type extension_result = {
  granted: int;
  new_max: int;
  ceiling: int;
  extensions_so_far: int;
  reason: string;
}

type denial_reason =
  | Ceiling_reached
  | Extension_limit_reached
  | Per_extend_cap_exceeded
  | Agent_idle
  | Cost_exceeded

val denial_reason_to_string : denial_reason -> string

(** Budget tracker. *)
type t

(** Create a turn budget.

    @param initial Starting max_turns (the agent begins with this many)
    @param ceiling Absolute hard cap (extend_turns cannot go beyond this)
    @param max_per_extend Max turns grantable per single extend call (default 20)
    @param max_extensions Max number of extend calls per session (default 10) *)
val create :
  initial:int ->
  ceiling:int ->
  ?max_per_extend:int ->
  ?max_extensions:int ->
  unit -> t

(** Try to extend the turn budget. Returns Ok with details or Error with reason. *)
val try_extend :
  t ->
  additional:int ->
  reason:string ->
  (extension_result, denial_reason) result

(** Current effective max_turns. *)
val current_max : t -> int

(** Build the [extend_turns] Tool.t.

    The tool reads the agent state via [agent_ref] to check idle turns
    and cost budget.  The ref is initially [None] and must be set to
    [Some agent] after [Agent.create].

    @param agent_ref Mutable ref to the agent (set after creation)
    @param budget The turn budget tracker
    @param max_idle_before_extend Deny if consecutive_idle_turns >= this (default 2) *)
val make_tool :
  agent_ref:Agent_types.t option ref ->
  budget:t ->
  ?max_idle_before_extend:int ->
  unit -> Tool.t

(** JSON summary for metrics/logging. *)
val stats_json : t -> Yojson.Safe.t
