(** Private immutable ownership boundary for one projected completion request.

    This module is a Dune [private_module].  Its raw request accessor therefore
    exists only in the library-private CMI and cannot become SDK dispatch
    authority.  Public admission surfaces must wrap its values in opaque types
    and expose typed facts and decisions only. *)

type t
type identity
type measured

type measurement_evidence = private
  { request_identity : identity
  ; measurement : Count_tokens_sync.completion_request_measurement
  }

(** Retain one exact immutable transport request. *)
val prepare : Llm_transport.completion_request -> t

(** Project the synchronous request used by the public Complete wrapper. *)
val prepare_sync
  :  config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?trace_context:(string * string) list
  -> unit
  -> t

(** Project the streaming request used by the public Complete wrapper. *)
val prepare_stream
  :  config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?trace_context:(string * string) list
  -> ?capture_id:string
  -> ?stream_idle_timeout_s:float
  -> unit
  -> t

(** Private raw accessor.  This declaration is never installed in the public
    [agent_sdk.llm_provider] module graph. *)
val request : t -> Llm_transport.completion_request

val identity : t -> identity
val same_identity : identity -> identity -> bool

(** Measure the exact retained request.  No fit policy, retry, truncation,
    completion dispatch, or caller continuation is performed. *)
val measure
  :  ?connection_cache:Http_client.cache
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> t
  -> (measured, Count_tokens_sync.completion_request_error) result

val measurement_evidence : measured -> measurement_evidence
