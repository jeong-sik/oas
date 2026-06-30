(** Shared helpers, constants, and content-block serialization.

    @stability Internal
    @since 0.93.1 *)

val default_base_url : string
val api_version : string
val max_response_body : int
val max_stdio_buffer : int

(** Default per-request wall-clock timeout for LLM HTTP calls (seconds).
    Used by [Api.create_message] to bound HTTP stalls. *)
val default_request_timeout_s : float

val synthesize_tool_use_id : name:string -> Yojson.Safe.t -> string
val string_is_blank : string -> bool
val text_blocks_to_string : Types.content_block list -> string
val json_of_string_or_raw : string -> Yojson.Safe.t

(** Raise [Invalid_argument] for a media source carrier unsupported by a backend.
    Backends should use this helper instead of silently reinterpreting [data] as
    another carrier. *)
val unsupported_media_source
  :  backend:string
  -> block:string
  -> Types.media_source_kind
  -> 'a

(** Convert a base64 media block into a data URL. Non-base64 carriers fail
    closed with {!unsupported_media_source}. *)
val base64_media_data_url
  :  backend:string
  -> block:string
  -> media_type:string
  -> data:string
  -> Types.media_source_kind
  -> string

(** Return the raw base64 payload for backends whose wire format separates the
    media type from the base64 data. Non-base64 carriers fail closed. *)
val base64_media_payload
  :  backend:string
  -> block:string
  -> data:string
  -> Types.media_source_kind
  -> string

(** {2 Content block JSON conversion} *)

val content_block_to_json : Types.content_block -> Yojson.Safe.t

type content_block_decode_error =
  | Missing_content_block_type
  | Unsupported_content_block_type of string
  | Missing_content_block_field of
      { block_type : string
      ; field : string
      }
  | Unsupported_media_source_kind of
      { block_type : string
      ; source_type : string
      }

val content_block_decode_error_to_string : content_block_decode_error -> string

val content_block_of_json_result
  :  Yojson.Safe.t
  -> (Types.content_block, content_block_decode_error) result

val content_block_of_json : Yojson.Safe.t -> Types.content_block option
val merge_tool_result_followup_user_messages : Types.message list -> Types.message list
val message_to_json : Types.message -> Yojson.Safe.t
val kimi_message_to_json : Types.message -> Yojson.Safe.t

(** {2 TLS} *)

type https_init_error =
  | Ca_certs_unavailable of string
  | Tls_config_unavailable of string

val https_init_error_to_string : https_init_error -> string

val make_https_result
  :  unit
  -> ( Uri.t -> [> `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t -> Tls_eio.t
       , https_init_error )
       result

val make_https
  :  unit
  -> (Uri.t -> [> `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t -> Tls_eio.t)
       option
