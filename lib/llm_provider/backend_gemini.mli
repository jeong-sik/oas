(** Gemini native API request building and response parsing.

    Uses the Gemini [contents/parts] wire format instead of the
    OpenAI-compatible [/chat/completions] wrapper.  Enables native
    thinking (thinkingConfig), function calling, and multimodal input.

    Pure functions operating on {!Llm_provider.Types}.

    @since 0.72.0

    @stability Internal
    @since 0.93.1 *)

exception Gemini_api_error of string

type request_artifact

val request_payload : request_artifact -> string
val request_output_token_receipt : request_artifact -> Types.output_token_receipt

val build_request_artifact
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> request_artifact

(** Build a Gemini [generateContent] request body from {!Provider_config.t}.
    Returns a JSON string.  URL construction (including [?key=]) is handled
    by {!Complete}; this function only produces the body. *)
val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string

(** Parse a Gemini [generateContent] response JSON into {!Types.api_response}. *)
val parse_response : Yojson.Safe.t -> Types.api_response

(** Opaque carrier payload for Gemini [thoughtSignature] values attached to
    function-call parts. Provider request builders can preserve this through
    {!Types.RedactedThinking} without widening the public [ToolUse] type. *)
val gemini_thought_signature_payload
  :  tool_use_id:string
  -> thought_signature:string
  -> string

(** Exact Gemini response-part kind targeted by an adjacent opaque
    [thoughtSignature] carrier. The carrier and target stay adjacent through
    history replay; a broken adjacency fails closed in the request builder.
    @since 0.212.0 *)
type gemini_part_signature_target =
  | Gemini_text_part
  | Gemini_thought_part

(** Opaque carrier payload for a Gemini [thoughtSignature] attached to the
    immediately following text or thought part.
    @since 0.212.0 *)
val gemini_part_thought_signature_payload
  :  target:gemini_part_signature_target
  -> thought_signature:string
  -> string

(** Extract [contents] list and optional [systemInstruction] from messages.
    Exposed for unit-testing the OAS-to-Gemini message mapping. *)
val contents_of_messages : Types.message list -> Yojson.Safe.t list * Yojson.Safe.t option
