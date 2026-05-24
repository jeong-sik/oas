(** ZhipuAI Provider_k native backend.

    Uses Provider_d-compatible wire format with Provider_k-specific extensions:
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

(** Semantic classification of a Provider_k API error.
    Determined by error code (structured) with message fallback. *)
type provider_k_error_class =
  | Provider_k_quota_exceeded
  | Provider_k_rate_limited
  | Provider_k_auth_error
  | Provider_k_server_error
  | Provider_k_invalid_request

type provider_k_error =
  { code : string
  ; message : string
  ; error_class : provider_k_error_class
  ; is_retryable : bool
  }

exception Provider_k_api_error of provider_k_error

(** Classify a Provider_k error code + message into a semantic class.
    Code-based classification takes priority; message keywords are fallback. *)
val classify_provider_k_error
  :  code:string
  -> message:string
  -> provider_k_error_class * bool

(** Map a Provider_k error class to the equivalent HTTP status code.
    Used by complete.ml to normalize provider-specific codes
    into the shared HTTP error path. *)
val http_code_of_provider_k_error_class : provider_k_error_class -> int

(** Build a Provider_k chat completion request body.
    Delegates to {!Backend_provider_d.build_request} and injects
    Provider_k-specific [thinking] parameter when [enable_thinking] is set. *)
val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string

(** Parse a Provider_k chat completion response.
    Handles Provider_k-specific string error codes and extracts
    [reasoning_content] as {!Types.Thinking} content block. *)
val parse_response : string -> api_response

(** Extract [reasoning_content] from Provider_k response body and prepend
    as a {!Types.Thinking} content block to the parsed response. *)
val extract_reasoning_content : api_response -> string -> api_response

(** Parse a Provider_k SSE streaming chunk.
    Delegates to {!Streaming.parse_provider_d_sse_chunk}. *)
val parse_stream_chunk : string -> Streaming.provider_d_chunk option
