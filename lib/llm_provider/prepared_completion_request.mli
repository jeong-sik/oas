(** Private state machine for one canonical completion request.

    A measurement cannot be detached from one request and attached to another:
    [measured] owns the exact [t], and [admitted] owns that [measured] value.
    The public {!Complete} module exposes these states opaquely. *)

type t
type measured
type admitted

type context_fit =
  { input_tokens : int
  ; reserved_output_tokens : int
  ; max_context_tokens : int
  }

type fit_error =
  | Context_limit_unknown of { model_id : string }
  | Invalid_context_limit of
      { model_id : string
      ; max_context_tokens : int
      }
  | Output_reservation_unknown of { model_id : string }
  | Context_window_exceeded of context_fit

val prepare
  :  config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> ?trace_context:(string * string) list
  -> ?capture_id:string
  -> ?stream_idle_timeout_s:float
  -> unit
  -> t

val request : t -> Llm_transport.completion_request

val measure
  :  ?connection_cache:Http_client.cache
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> t
  -> (measured, Count_tokens_sync.completion_request_error) result

val measurement : measured -> Count_tokens_sync.completion_request_measurement
val admit : measured -> (admitted, fit_error) result
val admitted_request : admitted -> t
val admitted_fit : admitted -> context_fit
