(** Provider-reported input-token count contract.

    This module owns the typed protocol vocabulary, provider-native input-count
    dispatch, and response decoding. It does not estimate counts or impose
    runtime policy.

    @stability Internal *)

type protocol =
  | Anthropic_messages_count_tokens
  | Openai_responses_input_tokens
  | Gemini_count_tokens
[@@deriving show, eq]

type count = private
  { input_tokens : int
  ; protocol : protocol
  ; model_id : string
  }
[@@deriving show, eq]

type error =
  | Unsupported of
      { protocol : protocol
      ; model_id : string
      }
  | Transport of Http_client.http_error
  | Invalid_response of
      { protocol : protocol
      ; model_id : string
      ; detail : string
      }

(** Decode one successful provider response.

    [model_id] is carried byte-for-byte as caller-owned identity. It is never
    inspected to select a protocol. All protocols accept zero and reject
    negative or non-integral token counts. *)
val decode_response
  :  protocol:protocol
  -> model_id:string
  -> string
  -> (count, error) result

(** Preserve an existing typed HTTP failure or decode the successful body. *)
val decode_transport_result
  :  protocol:protocol
  -> model_id:string
  -> (string, Http_client.http_error) result
  -> (count, error) result

(** Count one Anthropic Messages input through the provider's native
    [/v1/messages/count_tokens] endpoint.

    The request reuses {!Backend_anthropic}'s completion input projection.
    Non-Anthropic configs fail with [Unsupported] before any I/O. *)
val count_anthropic
  :  ?connection_cache:Http_client.cache
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> (count, error) result
