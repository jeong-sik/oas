(** Structured provider/API error evidence and typed classification.

    @stability Internal
    @since 0.93.1 *)

(** {1 Error types} *)

type invalid_request_reason =
  | Json_parse_error
  | Unknown_invalid_request

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
  | AuthError of { message : string } (** Authentication failed (HTTP 401). *)
  | AuthorizationError of { message : string }
  (** Authorization was refused (HTTP 403). *)
  | PaymentRequired of { message : string }
  | InvalidRequest of
      { message : string
      ; reason : invalid_request_reason
      }
  | NotFound of { message : string }
  | ContextOverflow of
      { message : string
      ; limit : int option
      }
  | NetworkError of
      { message : string
      ; kind : Http_client.network_error_kind
      }
  | Timeout of
      { message : string
      ; phase : Http_client.timeout_phase option
      }

(** {1 Error classification} *)

val is_retryable : api_error -> bool
val error_message : api_error -> string

(** Merge a provider's structured [retry_after] evidence: the JSON body's
    [error.retry_after] field wins when present (provider-specific, more
    precise); the transport's parsed [Retry-After] response header
    ({!Http_client.parse_retry_after_seconds}) is the fallback. [None] when
    neither is present or parseable. *)
val resolve_retry_after : body:string -> header:float option -> float option

(** [retry_after_header] carries the transport's parsed [Retry-After]
    response header, if any ({!Http_client.HttpError.retry_after_header}).
    Required (not optional-with-default) so every call site states
    explicitly whether header evidence was available, rather than
    silently defaulting to [None]. On the 429 branch it is merged via
    {!resolve_retry_after}; on other branches it is unused (those statuses
    do not carry an HTTP-level retry hint in this classification). *)
val classify_error
  :  retry_after_header:float option
  -> status:int
  -> body:string
  -> api_error
