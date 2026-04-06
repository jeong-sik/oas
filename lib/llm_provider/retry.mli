(** Structured API errors and retry logic with exponential backoff + jitter.

    @stability Internal
    @since 0.93.1 *)

(** {1 Error types} *)

type api_error =
  | RateLimited of { retry_after: float option; message: string }
  | Overloaded of { message: string }
  | ServerError of { status: int; message: string }
  | AuthError of { message: string }
  | InvalidRequest of { message: string }
  | ContextOverflow of { message: string; limit: int option }
  | NetworkError of { message: string }
  | Timeout of { message: string }

type retry_config = {
  max_retries: int;
  initial_delay: float;
  max_delay: float;
  backoff_factor: float;
}

val default_config : retry_config

(** {1 Error classification} *)

val is_retryable : api_error -> bool
val error_message : api_error -> string
val is_context_overflow_message : string -> bool

(** Extract the available context token limit from a context overflow error message.
    Parses both "available context size (N)" and "input token budget exceeded: U/N"
    formats. Returns [None] if the message is not an overflow error or the limit
    cannot be parsed.

    @since 0.102.0 *)
val parse_context_overflow_limit : string -> int option

(** Alias for {!parse_context_overflow_limit}.  Preferred name for SSOT consumers. *)
val extract_context_limit : string -> int option

val classify_error : status:int -> body:string -> api_error

(** {1 Retry execution} *)

val calculate_delay : retry_config -> int -> float

val with_retry :
  clock:_ Eio.Time.clock ->
  ?config:retry_config ->
  (unit -> ('a, api_error) result) ->
  ('a, api_error) result

val with_cascade :
  clock:_ Eio.Time.clock ->
  ?config:retry_config ->
  primary:(unit -> ('a, api_error) result) ->
  fallbacks:(unit -> ('a, api_error) result) list ->
  unit ->
  ('a, api_error) result
