(** Ollama native API backend.

    Builds requests for [/api/chat] endpoint with [think] parameter
    except for Gemma 4 chat-template thinking, and parses native API responses.

    @since 0.113.0 *)

val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string

val build_request_with_receipt
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string Provider_request_artifact.t

val parse_ollama_response : string -> (Types.api_response, string) result
