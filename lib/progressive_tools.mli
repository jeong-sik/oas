(** Progressive tool disclosure — reveal tools in phases.

    Controls which tools are available to the LLM at each turn,
    following the Gather-Act-Verify pattern from Claude Agent SDK.

    Integrates with {!Hooks.BeforeTurnParams} hooks via {!as_hook},
    using [tool_filter_override] in [AdjustParams] to restrict
    tools per turn.

    @since 0.43.0 *)

(** {1 Strategy} *)

(** Strategy for disclosing tools across turns.
    - [Phase_based]: each phase specifies a turn threshold and
      the tools available from that turn onward.
    - [Gather_act_verify]: three fixed phases at turns 1-2, 3-5, 6+. *)
type disclosure_strategy =
  | Phase_based of { phases: (int * string list) list }
  | Gather_act_verify of {
      gather_tools: string list;
      act_tools: string list;
      verify_tools: string list;
    }

(** {1 Query} *)

(** Compute the set of allowed tool names for a given turn. *)
val tools_for_turn : disclosure_strategy -> int -> string list

(** {1 Hook Integration} *)

(** Wrap a disclosure strategy as a {!Hooks.hook} that sets
    [tool_filter_override] via [AdjustParams] on [BeforeTurnParams]
    events, passing through all other events. *)
val as_hook : disclosure_strategy -> Hooks.hook
