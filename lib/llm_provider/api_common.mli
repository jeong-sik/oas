(** Shared helpers, constants, and content-block serialization. *)

val default_base_url : string
val api_version : string
val max_response_body : int
val max_stdio_buffer : int
val synthesize_tool_use_id : name:string -> Yojson.Safe.t -> string

val string_is_blank : string -> bool
val text_blocks_to_string : Types.content_block list -> string
val json_of_string_or_raw : string -> Yojson.Safe.t

(** {2 Content block JSON conversion} *)

val content_block_to_json : Types.content_block -> Yojson.Safe.t
val content_block_of_json : Yojson.Safe.t -> Types.content_block option
val message_to_json : Types.message -> Yojson.Safe.t

(** {2 TLS} *)

val make_https :
  unit ->
  (Uri.t ->
   [> `Close | `Flow | `R | `Shutdown | `W ] Eio.Resource.t ->
   Tls_eio.t) option
