(** Progressive tool disclosure -- reveal tools in phases.

    Controls which tools are available to the LLM at each turn,
    following the Gather-Act-Verify pattern.

    Integrates with {!Hooks.BeforeTurnParams} via {!as_hook}.

    @since 0.43.0 *)

(** {1 Strategy} *)

type disclosure_strategy =
  | Phase_based of { phases: (int * string list) list }
  | Gather_act_verify of {
      gather_tools: string list;
      act_tools: string list;
      verify_tools: string list;
    }

(** {1 Resolution} *)

(** Return the list of allowed tool names for the given turn number. *)
val tools_for_turn : disclosure_strategy -> int -> string list

(** {1 Hook integration} *)

(** Wrap a disclosure strategy as a {!Hooks.hook} that sets
    [tool_filter_override] via [AdjustParams]. *)
val as_hook : disclosure_strategy -> Hooks.hook
