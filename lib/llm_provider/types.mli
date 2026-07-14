(** Unified LLM provider types.

    Single source of truth for message, response, tool, and streaming types.
    Downstream consumers link against this module directly.

    @since 0.42.0

    @stability Internal
    @since 0.93.1 *)

(** {1 Message Types} *)

(** Role in a conversation.
    4-variant superset: System and Tool are required by multi-agent
    coordinators that inject system prompts and relay tool results. *)
type role =
  | System
  | User
  | Assistant
  | Tool
[@@deriving yojson, show]

val role_to_string : role -> string
val role_of_string : string -> role option

(** {1 Tool Types} *)

type param_type =
  | String
  | Integer
  | Number
  | Boolean
  | Array
  | Object
[@@deriving yojson, show]

val param_type_to_string : param_type -> string

(** Tool execution result types. *)
type tool_output =
  { content : string
  ; _meta : Yojson.Safe.t option
    (** Optional structured metadata forwarded to the MCP [tool_result._meta]
        field. [None] omits the field on the wire. *)
  }

type tool_error_class =
  | Transient
  | Deterministic
  | Unknown
[@@deriving yojson, show]

(** Provider-neutral classification of a failed tool execution. The class is
    produced by the execution boundary and survives agent history/checkpoint
    projection; provider wire serializers deliberately omit it. *)
type tool_failure_kind =
  | Validation_error
  | Recoverable_tool_error
  | Non_retryable_tool_error
  | Reported_tool_error
  | Unattributed_tool_error
  (** A persisted failure whose original execution boundary did not record
        provenance. This preserves missing historical evidence without
        relabelling it as provider-reported. *)
[@@deriving yojson, show]

type tool_failure_provenance =
  { failure_kind : tool_failure_kind
  ; error_class : tool_error_class option
  }
[@@deriving show]

(** A tool result has one authoritative outcome. Provider serializers derive
    their wire-level [is_error] flag from this value. *)
type tool_result_outcome =
  | Tool_succeeded
  | Tool_failed of tool_failure_provenance
[@@deriving show]

val tool_failure_kind_is_recoverable : tool_failure_kind -> bool
val tool_result_outcome_is_error : tool_result_outcome -> bool

type tool_error =
  { message : string
  ; recoverable : bool
  ; error_class : tool_error_class option
  }

type tool_result = (tool_output, tool_error) result

(** Lower an authoritative outcome to the hook/event-facing tool result. *)
val tool_result_of_outcome : content:string -> tool_result_outcome -> tool_result

type tool_param =
  { name : string
  ; description : string
  ; param_type : param_type
  ; required : bool
  }
[@@deriving yojson, show]

val param_type_of_string : string -> (param_type, string) result
val tool_param_to_json : tool_param -> Yojson.Safe.t
val tool_param_of_json : Yojson.Safe.t -> (tool_param, string) result
val params_to_input_schema : tool_param list -> Yojson.Safe.t

type tool_schema =
  { name : string
  ; description : string
  ; parameters : tool_param list
  ; strict : bool option
    (** Per-function JSON Schema strict validation. [Some true] opts the tool
        into strict mode (OpenAI, DeepSeek Beta, Kimi, MiMo); [None] omits the
        field so providers apply their default. *)
  }
[@@deriving yojson, show]

val tool_schema_to_json : tool_schema -> Yojson.Safe.t
val tool_schema_of_json : Yojson.Safe.t -> (tool_schema, string) result
val result_all : ('a, 'e) result list -> ('a list, 'e) result

type tool_choice =
  | Auto
  | Any
  | Tool of string
  | None_
[@@deriving show]

val tool_choice_to_json : tool_choice -> Yojson.Safe.t

type response_format =
  | Off
  | JsonMode
  | JsonSchema of Yojson.Safe.t
[@@deriving show]

val response_format_to_json : response_format -> Yojson.Safe.t
val response_format_of_json_mode : bool -> response_format

(** {1 Content Types} *)

(** Closed set of supported media source carriers. Unsupported provider/source
    combinations must fail closed instead of reinterpreting [data] as base64. *)
type media_source_kind =
  | Base64
  | Url
  | File_id
[@@deriving show]

val media_source_kind_to_string : media_source_kind -> string
val media_source_kind_of_string : string -> media_source_kind option

type reasoning_detail =
  { raw : Yojson.Safe.t
  ; text : string option
  }
[@@deriving show]

type content_block =
  | Text of string
  | Thinking of
      { content : string
      ; signature : string option
        (** [Some s]: Anthropic cryptographic signature, replayed byte-exact on
            tool turns. [None]: provider reasoning without a verification
            signature (OpenAI-compatible / Gemini / GLM / Ollama). *)
      }
  | ReasoningDetails of
      { reasoning_content : string option
      ; details : reasoning_detail list
      }
  | RedactedThinking of string
  | ToolUse of
      { id : string
      ; name : string
      ; input : Yojson.Safe.t
      }
  | ToolResult of
      { tool_use_id : string
      ; content : string
      ; outcome : tool_result_outcome
      ; json : Yojson.Safe.t option (** Structured payload when parseable. *)
      ; content_blocks : content_block list option
        (** Structured multi-block result (e.g. text + image). When [Some],
            providers that accept an array tool_result content serialize the
            blocks; [content] stays the canonical string fallback. *)
      }
  | Image of
      { media_type : string
      ; data : string
      ; source_type : media_source_kind
      }
  | Document of
      { media_type : string
      ; data : string
      ; source_type : media_source_kind
      }
  | Audio of
      { media_type : string
      ; data : string
      ; source_type : media_source_kind
      }
[@@deriving show]

(** [reasoning_details_text ~reasoning_content ~details] projects provider
    reasoning details to their textual reasoning channel. Non-empty
    [reasoning_content] wins; otherwise the function concatenates
    [details[].text] in order and ignores raw-only detail payloads. It never
    serializes [raw] as a fallback. *)
val reasoning_details_text
  :  reasoning_content:string option
  -> details:reasoning_detail list
  -> string

(** Message metadata: extensible typed key-value pairs attached to a message. *)
type metadata = (string * Yojson.Safe.t) list [@@deriving show]

(** Checkpoint-only conversation metadata owned by OAS. The run boundary is
    deliberately absent from provider payloads; it lets crash recovery avoid
    correlating tool failures across distinct external user runs. *)
module Conversation_metadata : sig
  type run_boundary =
    | Absent
    | Present
    | Invalid
    | Duplicate

  val run_boundary_entry : string * Yojson.Safe.t
  val run_boundary : metadata
  val classify_run_boundary : metadata -> run_boundary

  (** Whether a follow-up User message may be folded into the preceding Tool
      message for providers that require a single user-role span. *)
  val is_mergeable_followup : metadata -> bool
end

(** Exact producer binding for stored reasoning artifacts. Replay requires the
    same concrete provider instance, canonical request model, and typed replay
    contract. A fallback, endpoint change, or dialect override therefore drops
    foreign reasoning instead of cross-injecting it. *)
module Reasoning_source : sig
  type provider_instance [@@deriving show]

  type t =
    { provider_kind : Provider_kind.t
    ; provider_instance : provider_instance
    ; canonical_model_id : string
    ; replay_contract : Reasoning_replay_contract.t
    }
  [@@deriving show]

  type classification =
    | Absent
    | Present of t
    | Invalid
    | Duplicate
  [@@deriving show]

  val provider_instance : base_url:string -> request_path:string -> provider_instance

  val create
    :  provider_kind:Provider_kind.t
    -> provider_instance:provider_instance
    -> canonical_model_id:string
    -> replay_contract:Reasoning_replay_contract.t
    -> (t, string) result

  val equal : t -> t -> bool
  val entry : t -> string * Yojson.Safe.t
  val metadata : t -> metadata
  val add : t -> metadata -> (metadata, string) result
  val classify : metadata -> classification
end

type message =
  { role : role
  ; content : content_block list
  ; name : string option
  ; tool_call_id : string option
  ; metadata : metadata
  }
[@@deriving show]

(** {1 Response Types} *)

type stop_reason =
  | EndTurn
  | StopToolUse
  | MaxTokens
  | StopSequence
  | Refusal
  | ContentFilter
  | RepetitionTruncation
  | PauseTurn
  | Compaction
  | ContextWindowExceeded
  | UnmatchedToolCalls
  | Unknown of string
[@@deriving show]

val stop_reason_of_string : string -> stop_reason

(** Canonical wire serialization — the exact inverse of {!stop_reason_of_string}.
    SSOT for stop-reason wire strings; consumers must delegate here rather than
    re-spell the literals. *)
val stop_reason_to_string : stop_reason -> string

(** Stable, low-cardinality telemetry label. Identical to
    {!stop_reason_to_string} except [Unknown _] collapses to ["unknown"]. *)
val stop_reason_to_metric_label : stop_reason -> string

(** API usage from a single provider response. Accumulated multi-call usage
    belongs in agent-level usage stats. *)
type api_usage =
  { input_tokens : int
  ; output_tokens : int
  ; cache_creation_input_tokens : int
  ; cache_read_input_tokens : int
  ; cost_usd : float option
  }
[@@deriving show, yojson]

type inference_timings =
  { prompt_n : int option
  ; prompt_ms : float option
  ; prompt_per_second : float option
  ; predicted_n : int option
  ; predicted_ms : float option
  ; predicted_per_second : float option
  ; cache_n : int option
  }
[@@deriving show, yojson]

type inference_telemetry =
  { system_fingerprint : string option
  ; timings : inference_timings option
  ; reasoning_tokens : int option
  ; request_latency_ms : int option
  ; peak_memory_gb : float option
  ; provider_kind : Provider_kind.t option
    (** Set by the inference pipeline to record which provider produced the
        response. The on-wire JSON form stays the lowercase canonical string
        (["ollama"], ["anthropic"], ["openai_compat"], …) via
        {!Provider_kind.to_yojson}. *)
  ; reasoning_effort : string option
    (** e.g. "none", "low", "medium", "high" — as sent to provider *)
  ; canonical_model_id : string option
    (** Model ID used for the API request after alias resolution (e.g. "glm-4.7") *)
  ; reasoning_source : Reasoning_source.t option
    (** Exact replay provenance stamped by the live inference boundary. *)
  ; effective_context_window : int option
    (** Model's context window in tokens, from capabilities *)
  ; provider_internal_action_count : int option
    (** Telemetry-only count of provider-native actions that are not surfaced as OAS tool calls. *)
  ; ttfrc_ms : float option
    (** Time-to-first-response-chunk in milliseconds (wall-clock). *)
  ; prefill_ms : float option (** Prompt evaluation (prefill) duration in milliseconds. *)
  }
[@@deriving show, yojson]

type output_token_envelope =
  | Openai_chat_max_tokens
  | Openai_responses_max_output_tokens
  | Anthropic_messages_max_tokens
  | Gemini_generation_config_max_output_tokens
  | Ollama_options_num_predict
[@@deriving show, eq]

val output_token_envelope_to_yojson : output_token_envelope -> Yojson.Safe.t

val output_token_envelope_of_yojson
  :  Yojson.Safe.t
  -> (output_token_envelope, string) result

type output_token_policy =
  | Omitted
  | Explicit
  | Explicit_clamped
  | Required_catalog_fallback
  | Required_capability_override_fallback
[@@deriving show, eq]

val output_token_policy_to_yojson : output_token_policy -> Yojson.Safe.t
val output_token_policy_of_yojson : Yojson.Safe.t -> (output_token_policy, string) result

(** Typed provenance of the validation ceiling consulted for the receipt.
    [Provider_default] is the provider-config fallback used only when neither a
    capability override nor a model-catalog entry resolves.  Required request
    envelopes do not inject that validation fallback as a request value. *)
type output_token_ceiling_source =
  | Catalog_model
  | Declared_capability_override
  | Provider_default
[@@deriving show, eq]

val output_token_ceiling_source_to_yojson : output_token_ceiling_source -> Yojson.Safe.t

val output_token_ceiling_source_of_yojson
  :  Yojson.Safe.t
  -> (output_token_ceiling_source, string) result

type output_token_ceiling = private
  { value : int
  ; source : output_token_ceiling_source
  }

val output_token_ceiling
  :  value:int
  -> source:output_token_ceiling_source
  -> output_token_ceiling

(** An invariant-checked observation of one output-token decision.  Exact
    payload/receipt provenance is established only by the opaque request
    artifact returned by a backend; a receipt value by itself is a read
    projection, not proof that a particular payload was built or sent. *)
type output_token_receipt

type required_output_token_error = Required_output_token_ceiling_missing
[@@deriving show, eq]

val optional_output_token_receipt
  :  envelope:output_token_envelope
  -> requested:int option
  -> ceiling:output_token_ceiling option
  -> output_token_receipt

val required_output_token_receipt
  :  output_token_receipt
  -> (output_token_receipt, required_output_token_error) result

val output_token_receipt_envelope : output_token_receipt -> output_token_envelope
val output_token_receipt_requested : output_token_receipt -> int option
val output_token_receipt_effective : output_token_receipt -> int option
val output_token_receipt_policy : output_token_receipt -> output_token_policy
val output_token_receipt_ceiling : output_token_receipt -> int option

val output_token_receipt_ceiling_source
  :  output_token_receipt
  -> output_token_ceiling_source option

val output_token_receipt_to_yojson : output_token_receipt -> Yojson.Safe.t

val output_token_receipt_of_yojson
  :  Yojson.Safe.t
  -> (output_token_receipt, string) result

val equal_output_token_receipt : output_token_receipt -> output_token_receipt -> bool
val pp_output_token_receipt : Format.formatter -> output_token_receipt -> unit
val show_output_token_receipt : output_token_receipt -> string

(** Default/zero inference telemetry value owned by the telemetry type module.
    Callers should record-update this value instead of duplicating every field. *)
val default_inference_telemetry : inference_telemetry

type api_response =
  { id : string
  ; model : string
  ; stop_reason : stop_reason
  ; content : content_block list
  ; usage : api_usage option
  ; telemetry : inference_telemetry option
  }
[@@deriving show]

type assistant_message_error =
  | Reasoning_source_telemetry_missing
  | Reasoning_source_missing
[@@deriving show]

(** Convert a provider response into the Assistant history message callers
    should append. Reasoning-bearing responses require exact replay
    provenance; plain responses remain metadata-free. *)
val assistant_message_of_response
  :  api_response
  -> (message, assistant_message_error) result

(** {1 SSE Streaming Types} *)

type content_delta =
  | TextDelta of string
  | ThinkingDelta of string
  | ThinkingSignatureDelta of string
  | ReasoningDetailsDelta of
      { reasoning_content : string option
      ; details : reasoning_detail list
      }
  | InputJsonDelta of string
  (** Incremental fragment of a tool-call arguments JSON string. The
          accumulator appends successive fragments to the block buffer. *)
  | InputJsonSnapshot of string
  (** A whole tool-call arguments value serialized in a single delta, used
          by providers that stream [arguments] as a JSON object/array instead of
          string fragments. The accumulator replaces the block buffer rather
          than appending, so a provider that re-emits the same complete value
          does not concatenate it into invalid JSON. *)
  | MediaDelta of
      { media_type : string
      ; source_type : media_source_kind
      ; data : string
      }
  (** A chunk of a streamed media (image/document/audio) content block.
            Carries block-level [media_type] and [source_type] with the [data]
            payload so no new {!ContentBlockStart} fields are required. *)

type sse_event =
  | MessageStart of
      { id : string
      ; model : string
      ; usage : api_usage option
      }
  | ContentBlockStart of
      { index : int
      ; content_type : string
      ; tool_id : string option
      ; tool_name : string option
      }
  | ContentBlockDelta of
      { index : int
      ; delta : content_delta
      }
  | ContentBlockStop of { index : int }
  | MessageDelta of
      { stop_reason : stop_reason option
      ; usage : api_usage option
      }
  | MessageStop
  | Ping
  | SSEError of
      { message : string
      ; error_type : string option
      ; raw : string
      }
  (** A provider-reported error delivered mid-stream. [error_type] is the
            provider's error-object [type] discriminator (e.g.
            ["rate_limit_exceeded"]) and [raw] the original error JSON, so the
            consumer can converge onto the same classification path as an
            initial HTTP error instead of collapsing to [NetworkError {Unknown}]. *)
  | SSEParseFailed of
      { raw : string
      ; reason : string
      }
  (** A chunk's JSON could not be parsed (Yojson Json_error or
            Type_error). Previously the parser returned [None] and the
            chunk was silently dropped; consumers that then saw
            [MessageStop] would never know the response was incomplete.
            Emit this event so the accumulator can mark the stream as
            corrupted and the caller can route to a different
            provider instead of presenting a phantom completion. *)
  | SSEUnknownEventType of
      { event_type : string
      ; raw : string
      }
  | Connected
  | Timeout of string
  (** The chunk parsed cleanly but [event_type] did not match any
            documented variant. Likely a provider that added a new event
            type the OAS adapter has not yet learned. Emit explicitly so
            the consumer can decide (log + skip vs fail-fast) instead of
            silent data loss. *)
  | StreamIncomplete of { reason : string }
  (** The provider signalled the turn was cut off before a natural stop (an
      OpenAI Responses [response.incomplete]). Any in-progress tool call is
      partial, so the accumulator drops tool blocks at finalize rather than
      surfacing a dangling/executable ToolUse. [reason] is the provider's
      incomplete reason (e.g. ["max_output_tokens"], ["content_filter"]). This
      covers incomplete reasons beyond [max_output_tokens], which the
      [stop_reason = MaxTokens] check alone misses. *)

(** Terminal error captured while accumulating an SSE stream. The accumulator
    stores this typed value (not a flattened string) so a provider-reported
    error routes through the same [Http_client.HttpError {code; body}] ->
    [Retry.classify_error] path the non-streaming boundary uses, while a wire /
    parse failure stays an unclassifiable network error. *)
type stream_error =
  | Stream_provider_error of
      { message : string
      ; error_type : string option
      ; raw : string
      }
  | Stream_parse_failed of
      { reason : string
      ; raw : string
      }
  | Stream_unknown_event of
      { event_type : string
      ; raw : string
      }

(** {1 Convenience Constructors} *)

val make_message
  :  ?name:string
  -> ?tool_call_id:string
  -> ?metadata:(string * Yojson.Safe.t) list
  -> role:role
  -> content_block list
  -> message

val text_block : string -> content_block

val image_block
  :  ?source_type:media_source_kind
  -> media_type:string
  -> data:string
  -> unit
  -> content_block

val document_block
  :  ?source_type:media_source_kind
  -> media_type:string
  -> data:string
  -> unit
  -> content_block

val audio_block
  :  ?source_type:media_source_kind
  -> media_type:string
  -> data:string
  -> unit
  -> content_block

val text_message : role -> string -> message
val user_msg_blocks : content_block list -> message
val system_msg : string -> message
val user_msg : string -> message
val assistant_msg : string -> message
val try_parse_json : string -> Yojson.Safe.t option

val tool_result_msg
  :  tool_use_id:string
  -> content:string
  -> ?outcome:tool_result_outcome
  -> ?json:Yojson.Safe.t
  -> unit
  -> message

(** {1 Tool Result Validation}

    Minimal structural validation for tool result payloads. *)

type tool_result_validation_error =
  | Expected_object of string
  | Expected_array of string
  | Empty_content of string
  | Json_parse_failed of string

(** Validate that a ToolResult's payload matches a minimal expected shape.
    Returns [Ok ()] when the result passes, or a descriptive error.
    Foundation for P0's full JSON Schema validation loop. *)
val validate_tool_result_shape
  :  expect_object:bool
  -> expect_array:bool
  -> content_block
  -> (unit, tool_result_validation_error) result

val text_of_content : content_block list -> string
val text_of_message : message -> string
val text_of_response : api_response -> string

(** End-user-visible assistant text projection.

    Unlike {!text_of_content}, these helpers only include [Text] blocks. They
    intentionally exclude [Thinking], [RedactedThinking], [ToolUse],
    [ToolResult], and media blocks so downstream answer surfaces do not leak
    reasoning or execution payloads. *)
val visible_text_of_content : content_block list -> string

val visible_text_of_message : message -> string
val visible_text_of_response : api_response -> string

(** {1 Usage Helpers}

    @since 0.78.0 *)

(** Zero-valued usage sentinel for accumulation. *)
val zero_api_usage : api_usage

(** Extract usage from a response, preserving [None] when the provider did not
    report usage. *)
val usage_of_response : api_response -> api_usage option

(** Billable token total: [input_tokens + output_tokens]. Cache tokens are
    excluded — this counts the metered request/response pair, not full context. *)
val total_tokens : api_usage -> int
