(** Tool selector: 2-stage tool routing for large tool catalogs.

    When an agent has 20+ tools, sending all schemas to the LLM
    degrades selection accuracy. Tool_selector narrows the candidate
    set before the LLM composes arguments.

    Determinism boundary:
    - [All], [TopK_bm25], [Categorical] with [`Bm25] are fully
      deterministic (same input produces same output).
    - [TopK_llm], [Categorical] with [`Llm] are non-deterministic
      but bounded by [always_include] and catalog validation.

    @since 0.100.0 *)

(** Selection strategy. *)
type strategy =
  | All
    (** Current behavior: send all tools to LLM. No filtering.
        Use when tool count <= 15 or accuracy is acceptable. *)
  | TopK_bm25 of {
      k: int;
      (** Number of tools to select (recommended 3-5). *)
      always_include: string list;
      (** Tool names always included regardless of score.
          Use for essential tools (e.g., "done", "handoff").
          Recommendation: keep [always_include] < k/2. *)
    }
    (** BM25-based deterministic selection.
        Uses [Tool_index] internally. No LLM call, < 1ms latency.
        Suitable for keyword-matchable tool descriptions. *)
  | TopK_llm of {
      k: int;
      always_include: string list;
      selector_config: Types.agent_config option;
      (** Optional separate config for the selector LLM call.
          If [None], uses lightweight defaults (low max_tokens, temperature=0). *)
    }
    (** LLM-based selection. Not yet implemented (Phase 3).
        @raises Failure when called. *)
  | Categorical of {
      groups: (string * string list) list;
      (** [(group_name, tool_name list)] pairs.
          e.g., [("git", ["git_commit"; "git_push"; "git_diff"])] *)
      classifier: [ `Bm25 | `Llm ];
      (** How to pick the relevant group(s). *)
      always_include: string list;
    }
    (** Group-based selection. Not yet implemented (Phase 3).
        @raises Failure when called with [`Llm] classifier. *)

(** Select tools relevant to the current turn context.

    @param strategy How to select
    @param context The user's current query/message text
    @param tools Full tool catalog
    @return Filtered tool list (subset of [tools], preserving order) *)
val select :
  strategy:strategy ->
  context:string ->
  tools:Tool.t list ->
  Tool.t list

(** Convenience: return selected tool names only (for logging/debugging). *)
val select_names :
  strategy:strategy ->
  context:string ->
  tools:Tool.t list ->
  string list

(** Default strategy based on tool count.
    - [<= 15] tools -> [All]
    - [> 15] tools  -> [TopK_bm25 { k = 5; always_include = \[\] }] *)
val auto : tools:Tool.t list -> strategy
