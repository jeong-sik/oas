(** Typed image-generation round trips for catalog-declared image models.

    The selected model must declare [task = "image_generation"] in the model
    catalog (or in an explicit capability override). OAS never infers the task
    from a model id, endpoint, or provider label. *)

type source =
  | Remote_url of string
  | Inline_base64 of
      { media_type : string
      ; data : string
      }

type image = { source : source }

type usage =
  { input_tokens : int
  ; output_tokens : int
  ; total_tokens : int
  }

type filter_role =
  | User
  | Assistant
  | History
  | Other of string

type content_filter =
  { role : filter_role option
  ; level : int
  }

type response =
  { created_at : int option
  ; images : image list
  ; usage : usage option
  ; content_filter : content_filter list
  }

(** Generate images through an exact provider config.

    [Glm] uses the Z.AI image-generation wire. [OpenAI_compat] uses the OpenAI
    Image API wire and explicitly requests PNG output. Other provider kinds are
    rejected before I/O. The caller owns any [timeout_s]; omission installs no
    OAS deadline. *)
val generate
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?timeout_s:float
  -> ?connection_cache:Http_client.cache
  -> config:Provider_config.t
  -> prompt:string
  -> unit
  -> (response, Http_client.http_error) result
