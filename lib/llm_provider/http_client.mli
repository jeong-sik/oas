(** HTTP client for LLM provider endpoints.

    Eio + cohttp-eio with TLS via {!Api_common.make_https}.
    Network and HTTP errors are captured as {!http_error},
    so callers do not need [try/with] around HTTP operations.

    @stability Internal
    @since 0.93.1 *)

(** Structured classification of network errors.
    Enables consumers to pattern-match on error kind instead of
    parsing message strings.

    @since 0.171.0 *)
type network_error_kind =
  | Connection_refused
  (** Remote endpoint actively refused the connection (ECONNREFUSED). *)
  | Dns_failure (** Hostname resolution failed or returned no results. *)
  | Tls_error (** TLS handshake or certificate validation failed. *)
  | Timeout (** Connection or read timed out (ETIMEDOUT). *)
  | Local_resource_exhaustion
  (** Local OS resource limits reached (EMFILE, ENFILE, ENOBUFS, EADDRNOTAVAIL). *)
  | End_of_file (** Peer closed the connection unexpectedly. *)
  | Unknown (** Unclassified network error. *)

(** Last observed streaming state when an inter-line idle deadline fired.

    This is deliberately transport-generic.  Provider-specific parsers
    translate chunks into OAS SSE events first; the timeout evidence only
    records the broad activity the stream was in when progress stopped. *)
type stream_idle_state =
  | Awaiting_first_event
  | Awaiting_first_delta
  | Streaming_answer
  | Streaming_thinking
  | Streaming_tool_call
  | Streaming_heartbeat
  | Streaming_substrate
  | Streaming_done
  | Streaming_unknown
[@@deriving yojson, show]

(** Typed timeout source.

    [NetworkError { kind = Timeout; _ }] still exists for low-level OS or
    legacy timeouts.  New call-site-owned deadlines should surface as
    {!TimeoutError} with one of these phases so downstream policy can
    distinguish admission checks, scheduler queueing, first-token wait,
    streaming idleness, whole-call wall clocks, capacity backpressure,
    transport/body deadlines, CLI stdout idleness,
    and generic caller budgets. *)
type timeout_phase =
  | Admission
  (** Pre-flight provider/capability/admission checks before a request is
      allowed to spend body budget. *)
  | Queue
  (** Waiting for an internal scheduler, slot, or provider queue before the
      request starts producing provider output. *)
  | First_token
  (** Request was accepted and submitted, but no first user-visible token or
      delta arrived before the deadline. *)
  | Wall_clock
  (** Whole operation wall-clock deadline, independent of streaming progress. *)
  | Capacity_backpressure
  (** Provider or local capacity pressure rejected or delayed the request
      before normal request execution. *)
  | Http_operation
  | Non_streaming_body
  | Stream_body
  | Stream_idle of stream_idle_state
  | Provider_step
  | Cli_stdout_idle
  | Caller_budget
  | Unknown_timeout
[@@deriving yojson, show]

(** Provider-internal terminal condition reported via structured exit.

    Distinct from {!network_error_kind}: the subprocess/API ran to
    completion and emitted a structured stop reason on stdout.  Burying
    these as [NetworkError] loses the information that downstream
    callers and per-provider budgets need to handle the condition
    gracefully (e.g. [Max_turns] should checkpoint and resume on the
    next cycle rather than fall back to another provider).

    @since 0.178.0 *)
type provider_terminal_kind =
  | Max_turns of
      { turns : int
      ; limit : int
      }
  (** Provider's internal turn budget exhausted.  Maps to
          {!Error.MaxTurnsExceeded} at the agent runtime layer.  For
          cli_tool_d 0.x the [limit] equals the CLI default
          [--max-turns] (currently 31) when the caller does not
          override it. *)
  | Other of string
  (** Forward-compatible bucket for unrecognized subtypes
          (e.g. [error_during_execution], [error_max_thinking_tokens]).
          Carries the raw subtype string so consumers can log it
          without flag day for new variants. *)

(** Scope attached to provider failure classifications.

    This is deliberately about the failed lane, not about retry policy.
    Retry policy lives above this transport layer. *)
type provider_failure_scope =
  | Failure_scope_model
  | Failure_scope_account
  | Failure_scope_region
  | Failure_scope_provider
  | Failure_scope_unknown

(** Provider/runtime failure surfaced by a transport after it has parsed
    provider-specific HTTP/CLI details at the edge.

    Downstream code should pattern-match on this type instead of parsing
    stderr, HTTP bodies, or vendor-specific status strings. *)
type provider_failure_kind =
  | Capacity_exhausted of
      { scope : provider_failure_scope
      ; retry_after : float option
      ; model : string option
      }
  | Hard_quota of { retry_after : float option }
  | Capability_mismatch of { capability : string option }
  | Cli_policy_invalid of
      { tool_name : string option
      ; rule : int option
      }
  | Cli_startup_failed of { reason : string }
  | Provider_parse_error of { parser : string option }
  | Unknown_provider_failure of { reason : string option }

(** Transport-level error. *)
type http_error =
  | HttpError of
      { code : int
      ; body : string
      }
  | NetworkError of
      { message : string
      ; kind : network_error_kind
      }
  | TimeoutError of
      { message : string
      ; phase : timeout_phase
      }
  | AcceptRejected of { reason : string }
  (** Provider kind requires a non-HTTP transport (CLI subprocess)
          but the caller did not inject one.  Distinct from
          {!NetworkError} so callers can treat it as a configuration
          bug rather than a transient failure. *)
  | ProviderTerminal of
      { kind : provider_terminal_kind
      ; message : string
      }
  (** Provider reported a structured terminal condition on its
          completion stream.  Distinct from {!NetworkError} so callers
          and the agent runtime can map it to the right semantic
          ({!Error.MaxTurnsExceeded}, {!Retry.InvalidRequest}, …)
          rather than treat it as a transient network failure.

          @since 0.178.0 *)
  | ProviderFailure of
      { kind : provider_failure_kind
      ; message : string
      }
  (** Provider/runtime failure classified at the transport edge.
      Examples: model capacity exhaustion from a CLI stderr stream,
      invalid CLI policy, or a request that requires a capability the
      transport cannot provide. *)

val provider_failure_kind_to_string : provider_failure_kind -> string
val provider_failure_to_string : kind:provider_failure_kind -> message:string -> string
val stream_idle_state_to_label : stream_idle_state -> string
val timeout_phase_of_stream_idle_state : stream_idle_state -> timeout_phase
val timeout_phase_to_label : timeout_phase -> string

(** Default wall-clock timeout (seconds) applied to synchronous HTTP
    operations when a clock is supplied.  Streaming variants use this
    only to bound the connect + initial-response-headers phase. *)
val default_http_timeout_s : float

(** GET a URL synchronously, returning the full response.
    Returns [(status_code, body_string)] on success.

    When [clock] is supplied the entire operation (connect + response
    + body read) is bounded by [timeout_s] (default
    {!default_http_timeout_s}); a timeout owned by this wrapper
    surfaces as [TimeoutError { phase = Http_operation; _ }] which is
    classified as retryable by {!Retry.is_retryable}. *)
val get_sync
  :  ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> url:string
  -> headers:(string * string) list
  -> unit
  -> (int * string, http_error) result

(** POST JSON body synchronously, returning the full response.
    Returns [(status_code, body_string)] on success.

    When [clock] is supplied the entire operation is bounded by
    [timeout_s] (default {!default_http_timeout_s}); a timeout owned by
    this wrapper surfaces as
    [TimeoutError { phase = Http_operation; _ }]. *)
val post_sync
  :  ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> url:string
  -> headers:(string * string) list
  -> body:string
  -> unit
  -> (int * string, http_error) result

(** POST JSON body for SSE/NDJSON streaming.
    Returns [Ok reader] on HTTP 200 (10 MB buffer).
    Returns [Error] on non-200 or network failure.

    The connection is bound to [sw]; prefer {!with_post_stream} to
    ensure the connection fd is released when the stream is consumed.

    When [clock] is supplied only the connect + initial response
    headers are bounded by [connect_timeout_s] (default
    {!default_http_timeout_s}); body consumption through the returned
    reader is the caller's responsibility to timebox. *)
val post_stream
  :  ?clock:_ Eio.Time.clock
  -> ?connect_timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> url:string
  -> headers:(string * string) list
  -> body:string
  -> unit
  -> (Eio.Buf_read.t, http_error) result

(** Like {!post_stream} but manages connection lifetime internally.
    [f] receives the reader; when [f] returns the connection is closed
    and its fd is released immediately.

    [connect_timeout_s] bounds only the connect + initial response
    headers phase when [clock] is supplied; a stall there surfaces as
    [TimeoutError { phase = Http_operation; _ }].

    Body consumption in [f] runs OUTSIDE [catch_network]. A body-phase
    [Eio.Time.Timeout] (first-token / prefill wait, inter-chunk idle)
    is therefore NOT mapped to [Http_operation] here. Stream-state-aware
    callers (see {!Complete_stream.body_logic}) catch it inside [f] and
    emit the precise phase (prefill → [First_token], inter-chunk →
    [Stream_idle]); callers that let it propagate get
    [TimeoutError { phase = Unknown_timeout; _ }] as a safe default. *)
val with_post_stream
  :  ?clock:_ Eio.Time.clock
  -> ?connect_timeout_s:float
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> url:string
  -> headers:(string * string) list
  -> body:string
  -> f:(Eio.Buf_read.t -> 'a)
  -> unit
  -> ('a, http_error) result

(** Read SSE-formatted lines from a reader.

    Field lines are parsed per the W3C EventSource grammar
    ("name[:[ ]value]" — at most one leading space stripped from the
    value), so both [data: x] and [data:x] dispatch. [event:] sets the
    current event type; [data:] payloads (including empty ones) go to
    [on_data]; [id]/[retry] and unknown field names are ignored; a
    blank line resets the event type. Returns normally on
    [End_of_file].

    [on_data] runs OUTSIDE the idle-timeout window: it must not block —
    a parked handler silences the idle deadline for the whole stream.

    When both [clock] and [idle_timeout] are supplied, raises
    [Eio.Time.Timeout] if no line arrives within [idle_timeout]
    seconds. The deadline resets after each successful meaningful
    line, so this bounds inter-event idle — not total stream
    duration. SSE keepalive comments (lines starting with [:]) are
    skipped inside the same timeout window — they do NOT reset the
    deadline, so a stream of pure keepalives still trips
    [idle_timeout]. Supplying [idle_timeout] WITHOUT [clock] raises
    [Invalid_argument]: that combination used to silently disarm the
    deadline. Wrapped by {!with_post_stream} the timeout should be
    caught by the caller and surfaced as
    [TimeoutError { phase = Stream_idle state; _ }] so downstream
    policy can see which stream state stalled. *)
val read_sse
  :  ?clock:_ Eio.Time.clock
  -> ?idle_timeout:float
  -> reader:Eio.Buf_read.t
  -> on_data:(event_type:string option -> string -> unit)
  -> unit
  -> unit

(** Read NDJSON-formatted lines from a reader (one JSON object per
    line). Blank lines are skipped so a trailing newline does not
    yield an empty payload. Returns normally on [End_of_file].

    When both [clock] and [idle_timeout] are supplied, raises
    [Eio.Time.Timeout] if no line arrives within [idle_timeout]
    seconds. The deadline resets after each successful line, so this
    bounds inter-line idle — not total stream duration. Supplying
    [idle_timeout] WITHOUT [clock] raises [Invalid_argument] (silent
    disarm removed). The raised timeout should be caught by the caller
    and surfaced as [TimeoutError { phase = Stream_idle state; _ }] so
    downstream policy can see which stream state stalled. *)
val read_ndjson
  :  ?clock:_ Eio.Time.clock
  -> ?idle_timeout:float
  -> reader:Eio.Buf_read.t
  -> on_line:(string -> unit)
  -> unit
  -> unit

(** [true] when the error indicates local resource exhaustion
    (ephemeral port depletion, FD limit).  Cascading to another
    provider cannot help — the bottleneck is the local machine. *)
val is_local_resource_exhaustion : http_error -> bool

(** Inject ["stream": true] into a JSON body string.
    Any caller-supplied [stream] is replaced to avoid duplicate object keys. *)
val inject_stream_param : string -> string

(** Inject [{"stream_options": {"include_usage": true}}] into a JSON body
    string. OpenAI-compatible providers omit token usage from streaming
    responses unless this flag is set. Use only for OpenAI-compatible
    kinds; native-usage providers (Anthropic, Ollama, Gemini) must not
    receive it. Any caller-supplied [stream_options] is replaced to avoid
    double-injection; a non-object or unparseable body is returned
    unchanged. *)
val inject_stream_options_include_usage : string -> string
