(** ZhipuAI Glm native backend.

    Uses OpenAI-compatible wire format with Glm-specific extensions:
    - [thinking] parameter: [{"type":"enabled","clear_thinking":true}]
    - [reasoning_content] in response and streaming delta
    - String error codes (e.g., ["1305"])

    Auth: Bearer token from API key directly, or JWT generated from
    [{id}.{secret}] format keys (JWT requires [digestif], not yet added).

    Ref: docs.z.ai/api-reference/llm/chat-completion

    @since 0.83.0

    @stability Internal
    @since 0.93.1 *)

open Types

(** Semantic classification of a Glm API error from its documented code. *)
type glm_error_class =
  | Glm_quota_exceeded
  | Glm_rate_limited
  | Glm_auth_error
  | Glm_server_error
  | Glm_invalid_request

type glm_error_origin =
  | Provider_response
  | Response_parse

type glm_error =
  { code : string option
  ; message : string
  ; error_class : glm_error_class
  ; is_retryable : bool
  ; origin : glm_error_origin
  }

exception Glm_api_error of glm_error

type request_artifact

val request_payload : request_artifact -> string
val request_output_token_receipt : request_artifact -> Types.output_token_receipt

(** Classify a Glm error code + message into a semantic class.
    Code-based classification takes priority; message keywords are fallback. *)
val classify_glm_error : code:string -> glm_error_class * bool

(** Map a Glm error class to the equivalent HTTP status code.
    Used by complete.ml to normalize provider-specific codes
    into the shared HTTP error path. *)
val http_code_of_glm_error_class : glm_error_class -> int

(** Build a Glm chat completion request body.
    Delegates to {!Backend_openai.build_request} and injects
    Glm-specific [thinking] parameter when [enable_thinking] is set. *)
val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string

val build_request_artifact
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> request_artifact

(** Parse a Glm chat completion response.
    Handles Glm-specific string error codes and extracts
    [reasoning_content] as {!Types.Thinking} content block. *)
val parse_response : string -> api_response

(** Extract [reasoning_content] from Glm response body and prepend
    as a {!Types.Thinking} content block to the parsed response. *)
val extract_reasoning_content : api_response -> string -> api_response

(** Parse a Glm SSE streaming chunk.
    Delegates to {!Streaming.parse_openai_sse_chunk}. *)
val parse_stream_chunk : string -> Streaming.openai_sse_parse_result
