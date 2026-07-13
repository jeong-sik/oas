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
val classify_error : status:int -> body:string -> api_error
