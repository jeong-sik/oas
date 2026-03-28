(** ZhipuAI GLM native backend.

    Uses OpenAI-compatible wire format with GLM-specific extensions:
    - [thinking] parameter: [{"type":"enabled","clear_thinking":true}]
    - [reasoning_content] in response and streaming delta
    - String error codes (e.g., ["1305"])

    Auth: Bearer token from API key directly, or JWT generated from
    [{id}.{secret}] format keys (JWT requires [digestif], not yet added).

    Ref: docs.z.ai/api-reference/llm/chat-completion

    @since 0.83.0

    @stability Internal
    @since 0.93.0 *)

open Types

exception Glm_api_error of string

(** Build a GLM chat completion request body.
    Delegates to {!Backend_openai.build_request} and injects
    GLM-specific [thinking] parameter when [enable_thinking] is set. *)
val build_request :
  ?stream:bool ->
  config:Provider_config.t ->
  messages:message list ->
  ?tools:Yojson.Safe.t list ->
  unit -> string

(** Parse a GLM chat completion response.
    Handles GLM-specific string error codes and extracts
    [reasoning_content] as {!Types.Thinking} content block. *)
val parse_response : string -> api_response

(** Extract [reasoning_content] from GLM response body and prepend
    as a {!Types.Thinking} content block to the parsed response. *)
val extract_reasoning_content : api_response -> string -> api_response

(** Parse a GLM SSE streaming chunk.
    Delegates to {!Streaming.parse_openai_sse_chunk}. *)
val parse_stream_chunk : string -> Streaming.openai_chunk option
