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
