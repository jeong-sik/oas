(** Gemini native API request building and response parsing.

    Uses the Gemini [contents/parts] wire format instead of the
    OpenAI-compatible [/chat/completions] wrapper.  Enables native
    thinking (thinkingConfig), function calling, and multimodal input.

    Pure functions operating on {!Llm_provider.Types}.

    @since 0.72.0 *)

exception Gemini_api_error of string

(** Build a Gemini [generateContent] request body from {!Provider_config.t}.
    Returns a JSON string.  URL construction (including [?key=]) is handled
    by {!Complete}; this function only produces the body. *)
val build_request :
  ?stream:bool ->
  config:Provider_config.t ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  unit ->
  string

(** Parse a Gemini [generateContent] response JSON into {!Types.api_response}. *)
val parse_response : Yojson.Safe.t -> Types.api_response

(** Extract [contents] list and optional [systemInstruction] from messages.
    Exposed for unit-testing the OAS-to-Gemini message mapping. *)
val contents_of_messages :
  Types.message list -> Yojson.Safe.t list * Yojson.Safe.t option
