(** Tool selector: 2-stage tool routing for large tool catalogs.

    When an agent has 20+ tools, sending all schemas to the LLM
    degrades selection accuracy. Tool_selector narrows the candidate
    set before the LLM composes arguments.

    Determinism boundary:
    - [All], [TopK_bm25], [Categorical] with [`Bm25] are fully deterministic
      (same input produces same output).
    - [TopK_llm], [Categorical] with [`Llm classifier_fn] are
      non-deterministic when the supplied classifier performs I/O, but bounded
      by [always_include] and catalog validation.

    @since 0.100.0 *)

(** Selection strategy. *)
type strategy =
  | All
  (** Current behavior: send all tools to LLM. No filtering.
        Use when tool count <= 15 or accuracy is acceptable. *)
  | TopK_bm25 of
      { k : int (** Number of tools to select (recommended 3-5). *)
      ; always_include : string list
        (** Tool names always included regardless of score.
          Use for essential tools (e.g., "done", "handoff").
          Recommendation: keep [always_include] < k/2. *)
      ; confidence_threshold : float option
        (** If the top BM25 score is below this threshold, union
          [fallback_tools] with the BM25 results.
          Distinct from {!Tool_index.config.min_score}: [min_score] filters
          individual docs from results, while [confidence_threshold] triggers
          a fallback when the best match is weak.
          [None] disables fallback. *)
      ; fallback_tools : string list
        (** Tools to include when BM25 confidence is low (top score < threshold).
          Typically set to a curated policy-allowed subset.
          Note: fallback tools are unioned with the top-k results, so the
          total result count may exceed [k] when fallback is triggered. *)
      }
  (** BM25-based deterministic selection.
        Uses [Tool_index] internally. No LLM call, < 1ms latency.
        Suitable for keyword-matchable tool descriptions. *)
  | TopK_llm of
      { k : int (** Number of tools to select after LLM reranking. *)
      ; bm25_prefilter_n : int
        (** Stage 1: pass this many BM25 top candidates to [rerank_fn].
          Recommended: 10-30. Higher = better recall, more LLM tokens. *)
      ; always_include : string list
        (** Tool names always included regardless of rerank result. *)
      ; confidence_threshold : float
        (** BM25 top score below this threshold skips the LLM call
          and returns BM25 top-k directly.
          Avoids wasting LLM calls on queries with no good match. *)
      ; rerank_fn : context:string -> candidates:(string * string) list -> string list
        (** LLM reranking closure. Receives [(name, description)] pairs
          from BM25 pre-filter. Returns selected names in priority order.

          If this function raises, the selector fails closed by selecting no
          reranked tools; [always_include] still applies. Invalid names in the
          return are dropped.

          Use {!default_rerank_fn} for a ready-made implementation against
          a single {!Llm_provider.Provider_config.t}. *)
      }
  (** 2-stage LLM-based selection: BM25 pre-filter then LLM reranking.

        For [TopK_llm], {!select} may perform I/O via [rerank_fn].
        The function is not idempotent for this strategy -- callers must
        not cache or retry based on output equality assumptions.
        [always_include] provides a deterministic lower bound.

        @since 0.101.0 *)
  | Categorical of
      { groups : (string * string list) list
        (** [(group_name, tool_name list)] pairs.
          e.g., [("git", ["git_commit"; "git_push"; "git_diff"])] *)
      ; classifier :
          [ `Bm25
          | `Llm of context:string -> candidates:(string * string) list -> string list
          ]
        (** How to pick relevant group names.

          [`Llm classifier_fn] receives [(group_name, group_description)] pairs.
          Returned names are validated against [groups]; invalid names are
          dropped. If [classifier_fn] raises, group selection fails closed and
          only [always_include] remains. *)
      ; always_include : string list
      }
  (** Group-based selection.

        [`Bm25] classification builds a deterministic index from group names
        and grouped tool descriptions. [`Llm classifier_fn] lets callers plug
        in the same provider-bound closure style as {!TopK_llm} without this
        module inferring providers from process-global state. *)

(** Deprecated compatibility exception. Current built-in strategies are
    implemented; callers should not rely on this exception for selector
    capability detection. *)
exception Unsupported_configuration of string

(** Select tools relevant to the current turn context.

    Internally builds a {!Tool_index} via [of_tools], which sets
    [aliases = \[\]]. To use aliases for BM25 augmentation, build
    the index via {!Tool_index.build} directly and use the lower-level
    {!Tool_index.retrieve} API.

    @param strategy How to select
    @param context The user's current query/message text
    @param tools Full tool catalog
    @return Filtered tool list (subset of [tools], preserving order) *)
val select : strategy:strategy -> context:string -> tools:Tool.t list -> Tool.t list

(** Convenience: return selected tool names only (for logging/debugging). *)
val select_names : strategy:strategy -> context:string -> tools:Tool.t list -> string list

(** Like {!select} but accepts a pre-built {!Tool_index.t}.

    Avoids rebuilding the BM25 index on every call. Callers that
    invoke selection per-turn with the same tool set should build
    the index once via {!Tool_index.of_tools} (or {!Tool_index.build}
    for alias support) and reuse it.

    For [All] and [Categorical] strategies the index is ignored
    (they either return everything or build their own group index).

    @since 0.136.1 *)
val select_with_index
  :  strategy:strategy
  -> index:Tool_index.t
  -> context:string
  -> tools:Tool.t list
  -> Tool.t list

(** Like {!select_names} but accepts a pre-built index.
    @since 0.136.1 *)
val select_names_with_index
  :  strategy:strategy
  -> index:Tool_index.t
  -> context:string
  -> tools:Tool.t list
  -> string list

(** Default strategy based on tool count.
    - [<= 15] tools -> [All]
    - [> 15] tools  -> [TopK_bm25 { k = 5; always_include = \[\] }] *)
val auto : tools:Tool.t list -> strategy

(** {1 Default LLM reranker} *)

(** Construct a rerank closure for use with [TopK_llm].

    Uses {!Llm_provider.Complete.complete} against a single provider for the
    LLM call. Captures [sw] and [net] in the closure -- must be called inside
    [Eio.Switch.run]. The provider must return a JSON string array of tool
    names. On LLM failure or malformed JSON, returns [[]] so upstream selection
    keeps only [always_include].

    OAS expects one resolved provider per call. Callers that want
    failover should resolve the winning provider externally and pass it
    here.

    Usage:
    {[
      Eio.Switch.run @@ fun sw ->
        let rerank = Tool_selector.default_rerank_fn
          ~sw ~net ~provider:some_provider ~k:5 () in
        let agent = Builder.create ~net ~model
          |> Builder.with_tool_selector
               (TopK_llm { k = 5; bm25_prefilter_n = 20;
                 always_include = ["heartbeat"];
                 confidence_threshold = 0.5;
                 rerank_fn = rerank })
          |> Builder.build_safe |> Result.get_ok in
        Agent.run ~sw agent
    ]}

    @since 0.101.0
    @since 0.142.0 Single-provider API. *)
val default_rerank_fn
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> provider:Llm_provider.Provider_config.t
  -> k:int
  -> unit
  -> context:string
  -> candidates:(string * string) list
  -> string list
