(** Structured API errors and retry logic with exponential backoff + jitter.

    @stability Internal
    @since 0.93.1 *)

(** {1 Error types} *)

type api_error =
  | RateLimited of
      { retry_after : float option
      ; message : string
      }
  | Overloaded of { message : string }
  | ServerError of
      { status : int
      ; message : string
      }
  | AuthError of { message : string }
  | InvalidRequest of { message : string }
  | NotFound of { message : string }
  | ContextOverflow of
      { message : string
      ; limit : int option
      }
  | NetworkError of
      { message : string
      ; kind : Http_client.network_error_kind
      }
  | Timeout of { message : string }

type retry_config =
  { max_retries : int
  ; initial_delay : float
  ; max_delay : float
  ; backoff_factor : float
  }

val default_config : retry_config

(** {1 Error classification} *)

val is_retryable : api_error -> bool
val error_message : api_error -> string
val is_context_overflow_message : string -> bool

(** True when a [RateLimited] (429) error is non-retryable due to a
    persistent account-state condition rather than transient throttling.

    Two categories are detected:
    - Quota / balance exhaustion: balance 0, credit depleted, monthly
      quota reached, resource exhausted. Resolved by recharging or
      waiting for the next billing cycle.
    - Account access disabled: admin-disabled allocation, suspended /
      disabled account. Resolved by re-enabling the account or contacting
      the provider; recharging alone does not unblock.

    In either case retrying the exact same request will never succeed
    until the underlying account state is changed out-of-band.

    Only [RateLimited] messages are inspected; other variants return [false].

    Consumers (e.g. cascade health trackers) use this to apply an immediate
    long cooldown instead of transient backoff.

    @since 0.156.0 *)
val is_hard_quota : api_error -> bool

(** Extract the available context token limit from a context overflow error message.
    Parses both "available context size (N)" and "input token budget exceeded: U/N"
    formats. Returns [None] if the message is not an overflow error or the limit
    cannot be parsed.

    @since 0.102.0 *)
val parse_context_overflow_limit : string -> int option

(** Alias for {!parse_context_overflow_limit}.  Preferred name for SSOT consumers. *)
val extract_context_limit : string -> int option

(** Case-insensitive substring search. SSOT for cross-module use.
    @since 0.185.0 *)
val contains_substring_ci : haystack:string -> needle:string -> bool

val classify_error : status:int -> body:string -> api_error

(** {1 Retry execution} *)

val calculate_delay : retry_config -> int -> float

val with_retry
  :  clock:_ Eio.Time.clock
  -> ?config:retry_config
  -> (unit -> ('a, api_error) result)
  -> ('a, api_error) result

(** Retry a function while preserving its original error type.
    [classify] returns [Some api_error] for errors that should use the shared
    retry / backoff policy, or [None] to fail fast without retry.

    @stability Internal
    @since 0.163.0 *)
val with_retry_map_error
  :  clock:_ Eio.Time.clock
  -> ?config:retry_config
  -> classify:('e -> api_error option)
  -> (unit -> ('a, 'e) result)
  -> ('a, 'e) result
