(** Retry_classify — provider-agnostic retry policy classification.

    Extracted from [Complete] (lines 1082-1132 in the pre-split
    [complete.ml]) so the retry-decision surface can be reused by
    callers that build their own retry loop without depending on the
    full [Complete] module.  [Complete] keeps re-exports of
    [retry_config], [default_retry_config], and [is_retryable] for
    backward compatibility with [Complete_cascade] and the existing
    test suite that imports through [Complete].

    Pure module: no I/O, no async.  Maps [Http_client] errors to the
    shared [Retry] policy taxonomy. *)

type retry_config =
  { max_retries : int
  ; initial_delay_sec : float
  ; max_delay_sec : float
  ; backoff_multiplier : float
  }

(** Pulled from [Constants.Retry] so tuning lives in one place. *)
val default_retry_config : retry_config

(** Adapter into the shared [Retry] module's config shape.  Used by
    [Complete.complete_with_retry] and by any caller that wants to feed
    [Retry.is_retryable] through. *)
val shared_retry_config_of_complete : retry_config -> Retry.retry_config

(** Translates an [Http_client.http_error] into a [Retry.api_error]
    where retryability is meaningful.

    Returns [None] for terminal/wiring conditions:

    - [AcceptRejected] / [CliTransportRequired] — wiring bugs, not
      transient; retry would not summon a missing transport.
    - [ProviderTerminal] — provider hit its own terminal condition
      (e.g. cli_tool_d internal max_turns); retry would re-trigger
      the same deterministic exit.
    - [ProviderFailure] — provider/runtime failures are semantic
      cascade inputs, not local retry inputs; retrying the same
      lane would hide the typed reason from downstream policy. *)
val classify_retry_error : Http_client.http_error -> Retry.api_error option

(** Convenience wrapper: [true] iff [classify_retry_error] yields a
    retryable [Retry.api_error].  Used by [Complete_cascade] and
    direct callers that just need a yes/no signal. *)
val is_retryable : Http_client.http_error -> bool
