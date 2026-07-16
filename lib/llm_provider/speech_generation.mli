(** Typed text-to-speech round trips for catalog-declared speech models. *)

type output_format =
  | Mp3
  | Opus
  | Aac
  | Flac
  | Wav
  | Pcm

type voice =
  | Named of string
  | Custom_id of string

type response =
  { format : output_format
  ; audio : string
  }

(** Generate speech through the OpenAI Speech API.

    The model must declare [task = "speech"] in the exact provider catalog.
    The response contains the raw audio bytes in the requested format. Omission
    of [timeout_s] installs no OAS deadline. *)
val generate
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> ?connection_cache:Http_client.cache
  -> config:Provider_config.t
  -> text:string
  -> voice:voice
  -> format:output_format
  -> unit
  -> (response, Http_client.http_error) result
