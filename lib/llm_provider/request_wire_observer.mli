(** Caller-owned observation of one final serialized provider request.

    OAS invokes the observer after provider-specific serialization and every
    stream-field injection have completed, after the exact serialized-body
    admission check, and immediately before HTTP dispatch. The observation
    contains only structural identity, byte length, and a SHA-256 digest; the
    request body, prompts, tool arguments, headers, and credentials are never
    exposed.

    Observation is diagnostic and non-authoritative. Caller rejection or an
    ordinary callback exception is reported as typed failure evidence but does
    not rewrite the provider result. Request admission remains owned by the
    typed provider configuration (for example [max_request_body_bytes]).

    @stability Evolving
    @since 0.230.0 *)

type observation =
  { capture_id : string option
  ; provider : string
  ; model : string
  ; http_codec : string
  ; stream : bool
  ; body_bytes : int
  ; body_sha256 : string
  }
[@@deriving yojson, show]

type rejection = { reason : string } [@@deriving yojson, show]

(** A synchronous offer into caller-owned observation state. Callers should
    keep the callback bounded because it runs immediately before dispatch. *)
type try_observe = observation -> (unit, rejection) result

type failure_cause =
  | Observer_rejected of rejection
  | Observer_raised of
      { message : string
      ; backtrace : string
      }
[@@deriving yojson, show]

type failure =
  { observation : observation
  ; cause : failure_cause
  }
[@@deriving yojson, show]

(** Construct metadata for the exact [body]. *)
val observation
  :  capture_id:string option
  -> provider:string
  -> model:string
  -> http_codec:string
  -> stream:bool
  -> body:string
  -> observation

(** Offer one observation exactly once. Reserved exceptions propagate;
    rejection and ordinary exceptions become typed failure evidence. *)
val observe : try_observe -> observation -> (unit, failure) result
