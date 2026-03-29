(** Progressive tool disclosure — reveal tools in phases.

    Controls which tools are available to the LLM at each turn,
    following the Gather-Act-Verify pattern from Claude Agent SDK.

    Integrates with {!Hooks.BeforeTurnParams} hooks via {!as_hook},
    using [tool_filter_override] in [AdjustParams] to restrict
    tools per turn.

    @since 0.43.0

    @stability Evolving
    @since 0.93.1 *)

(** {1 Strategy} *)

(** Strategy for disclosing tools across turns.
    - [Phase_based]: each phase specifies a turn threshold and
      the tools available from that turn onward.
    - [Gather_act_verify]: three fixed phases at turns 1-2, 3-5, 6+.
    - [Retrieval_based]: BM25-indexed tool retrieval per turn context.
      Falls back to [fallback_tools] if no result exceeds [confidence_threshold].

    @since 0.43.0 (Phase_based, Gather_act_verify)
    @since 0.89.0 (Retrieval_based) *)
type disclosure_strategy =
  | Phase_based of { phases: (int * string list) list }
  | Gather_act_verify of {
      gather_tools: string list;
      act_tools: string list;
      verify_tools: string list;
    }
  | Retrieval_based of {
      index: Tool_index.t;
      confidence_threshold: float;
      fallback_tools: string list;
      always_include: string list;
    }

(** {1 Query} *)

(** Compute the set of allowed tool names for a given turn.
    For [Retrieval_based], uses [context] to search the index.
    If no context is provided, returns [always_include] + [fallback_tools]. *)
val tools_for_turn : disclosure_strategy -> int -> ?context:string -> unit -> string list

(** {1 Hook Integration} *)

(** Wrap a disclosure strategy as a {!Hooks.hook} that sets
    [tool_filter_override] via [AdjustParams] on [BeforeTurnParams]
    events, passing through all other events.

    For [Retrieval_based], extracts context from the last user message. *)
val as_hook : disclosure_strategy -> Hooks.hook
