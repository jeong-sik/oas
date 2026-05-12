(** Per-turn {!Tool.disclosure_level} resolution: combine an optional
    callback that inspects the most recent tool results with a static
    fallback.

    @since 0.195.0 *)

(** [resolve ~resolver ~static ~last_results] returns the effective
    disclosure level.

    - [resolver = None]: return [static].
    - [resolver = Some f]: when [f last_results = Some override],
      return [Some override]; otherwise return [static].

    The function performs no effects beyond invoking [resolver].  If the
    resolver raises or performs side effects, those propagate to the caller.
    Callers that need TTL, sticky promotion, or session state should encode
    that in [resolver]. *)
val resolve
  :  resolver:(Types.tool_result list -> Tool.disclosure_level option) option
  -> static:Tool.disclosure_level option
  -> last_results:Types.tool_result list
  -> Tool.disclosure_level option
