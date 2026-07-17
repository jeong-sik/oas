(** Immutable ownership boundary for one fully prepared completion request.

    A routing layer calls {!prepare} only after every message, tool, provider,
    and transport projection is complete.  The resulting value retains that
    exact {!Llm_transport.completion_request}; it never rebuilds the request.

    The identity is an in-process witness for one prepared value.  It is
    intentionally opaque, has no string projection, and makes no durable or
    cross-process identity claim.  {!same_identity} is true only for evidence
    derived from the same prepared value, not merely structurally equal
    requests.

    This module is a low-level prerequisite for Agent-owned fit admission.
    Regular Agent callers do not need to construct transport requests. *)

type t
type identity
type measured

type measurement_evidence = private
  { request_identity : identity
  ; measurement : Count_tokens_sync.completion_request_measurement
  }

type 'a prepared_request_use = private
  { measurement_evidence : measurement_evidence
  ; value : 'a
  }

(** Retain one exact immutable request without copying or normalizing it. *)
val prepare : Llm_transport.completion_request -> t

(** Return the opaque in-process identity of this prepared value. *)
val identity : t -> identity

(** Compare two in-process prepared-value identities. *)
val same_identity : identity -> identity -> bool

(** Measure the exact request retained by [prepared].  Success returns an
    opaque measured state, preventing the typed dispatch path from being used
    with independently reconstructed request fields.

    Provider errors remain the exact typed errors from
    {!Count_tokens_sync.measure_completion_request}. *)
val measure
  :  ?connection_cache:Http_client.cache
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> t
  -> (measured, Count_tokens_sync.completion_request_error) result

(** Inspect the typed measurement and its prepared-value identity. *)
val measurement_evidence : measured -> measurement_evidence

(** Invoke [f] once with the exact request retained before measurement.  The
    returned value binds that continuation use to the same measurement
    identity.

    This is prepared-request use evidence only.  It does not claim that [f]
    dispatched provider I/O.  The function adds no retry, timeout, truncation,
    fallback, or exception translation. *)
val with_request
  :  measured
  -> f:(Llm_transport.completion_request -> 'a)
  -> 'a prepared_request_use
