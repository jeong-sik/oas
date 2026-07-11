(** Unified LLM provider types.

    Single source of truth for message, response, tool, and streaming types.
    Downstream consumers link against this module directly.

    @since 0.42.0 *)

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

let role_to_string = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
;;

let role_of_string = function
  | "system" -> Some System
  | "user" -> Some User
  | "assistant" -> Some Assistant
  | "tool" -> Some Tool
  | _ -> None
;;

(** {1 Tool Types} *)

(** Tool parameter schema *)
type param_type =
  | String
  | Integer
  | Number
  | Boolean
  | Array
  | Object
[@@deriving yojson, show]

let param_type_to_string = function
  | String -> "string"
  | Integer -> "integer"
  | Number -> "number"
  | Boolean -> "boolean"
  | Array -> "array"
  | Object -> "object"
;;

(** Tool execution result types.
    Defined before content_block/message/api_response to avoid
    field-name shadowing on the [content] record field. *)
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

type tool_error =
  { message : string
  ; recoverable : bool
  ; error_class : tool_error_class option
  }

type tool_result = (tool_output, tool_error) result

type tool_param =
  { name : string
  ; description : string
  ; param_type : param_type
  ; required : bool
  }
[@@deriving yojson, show]

let param_type_of_string = function
  | "string" -> Ok String
  | "integer" -> Ok Integer
  | "number" -> Ok Number
  | "boolean" -> Ok Boolean
  | "array" -> Ok Array
  | "object" -> Ok Object
  | other -> Error other
;;

let tool_param_to_json (p : tool_param) : Yojson.Safe.t =
  `Assoc
    [ "name", `String p.name
    ; "description", `String p.description
    ; "param_type", `String (param_type_to_string p.param_type)
    ; "required", `Bool p.required
    ]
;;

let tool_param_of_json (json : Yojson.Safe.t) : (tool_param, string) result =
  let open Yojson.Safe.Util in
  match param_type_of_string (json |> member "param_type" |> to_string) with
  | Error s -> Error (Printf.sprintf "unknown param_type: %s" s)
  | Ok param_type ->
    Ok
      { name = json |> member "name" |> to_string
      ; description = json |> member "description" |> to_string
      ; param_type
      ; required = json |> member "required" |> to_bool
      }
;;

let params_to_input_schema (params : tool_param list) : Yojson.Safe.t =
  let properties =
    List.map
      (fun (p : tool_param) ->
         ( p.name
         , `Assoc
             [ "type", `String (param_type_to_string p.param_type)
             ; "description", `String p.description
             ] ))
      params
  in
  let required =
    List.filter_map
      (fun (p : tool_param) -> if p.required then Some (`String p.name) else None)
      params
  in
  `Assoc
    [ "type", `String "object"
    ; "properties", `Assoc properties
    ; "required", `List required
    ]
;;

(** Tool definition *)
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

let tool_schema_to_json (s : tool_schema) : Yojson.Safe.t =
  `Assoc
    ([ "name", `String s.name
     ; "description", `String s.description
     ; "parameters", `List (List.map tool_param_to_json s.parameters)
     ]
     (* Emit "strict" only when set so [None] round-trips to an absent field
        and providers keep their own default. *)
     @
     match s.strict with
     | Some b -> [ "strict", `Bool b ]
     | None -> [])
;;

let result_all items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok item :: rest -> loop (item :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] items
;;

let tool_schema_of_json (json : Yojson.Safe.t) : (tool_schema, string) result =
  let open Yojson.Safe.Util in
  match
    json |> member "parameters" |> to_list |> List.map tool_param_of_json |> result_all
  with
  | Error e -> Error e
  | Ok parameters ->
    Ok
      { name = json |> member "name" |> to_string
      ; description = json |> member "description" |> to_string
      ; parameters
      ; strict = json |> member "strict" |> to_bool_option
      }
;;

(** Tool choice mode *)
type tool_choice =
  | Auto
  | Any
  | Tool of string
  | None_ (** Disables tool use. Anthropic: {type:none}, Openai: "none" *)
[@@deriving show]

let tool_choice_to_json = function
  | Auto -> `Assoc [ "type", `String "auto" ]
  | Any -> `Assoc [ "type", `String "any" ]
  | Tool name -> `Assoc [ "type", `String "tool"; "name", `String name ]
  | None_ -> `Assoc [ "type", `String "none" ]
;;

type response_format =
  | Off
  | JsonMode
  | JsonSchema of Yojson.Safe.t
[@@deriving show]

let response_format_of_json_mode enabled = if enabled then JsonMode else Off

let response_format_to_json = function
  | Off -> `Assoc [ "type", `String "off" ]
  | JsonMode -> `Assoc [ "type", `String "json_mode" ]
  | JsonSchema schema -> `Assoc [ "type", `String "json_schema"; "schema", schema ]
;;

(** {1 Content Types} *)

(** Closed set of supported media source carriers. *)
type media_source_kind =
  | Base64
  | Url
  | File_id
[@@deriving show]

let media_source_kind_to_string = function
  | Base64 -> "base64"
  | Url -> "url"
  | File_id -> "file_id"
;;

let media_source_kind_of_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "base64" -> Some Base64
  | "url" -> Some Url
  | "file_id" -> Some File_id
  | _ -> None
;;

type reasoning_detail =
  { raw : Yojson.Safe.t
  ; text : string option
  }
[@@deriving show]

(** Content block types -- inline records for clarity *)
type content_block =
  | Text of string
  | Thinking of
      { content : string
      ; signature : string option
        (** [Some s]: Anthropic cryptographic signature, replayed byte-exact on
            tool turns (never sanitized or re-encoded). [None]: provider
            reasoning that carries no verification signature
            (OpenAI-compatible / Gemini / GLM / Ollama). Replaces the former
            [thinking_type : string], which conflated this signature with a
            free-form provider label ("reasoning" / "thinking" /
            "reasoning_summary") that no consumer read. *)
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
      ; is_error : bool
      ; json : Yojson.Safe.t option
        (** Parsed JSON payload when available. Consumers
                        should prefer [json] over [content] for structured access.
                        [content] remains the canonical string for API serialization. *)
      ; content_blocks : content_block list option
        (** Structured multi-block result (e.g. text + image). When [Some],
                        providers that accept an array tool_result content serialize
                        the blocks; [content] stays the canonical string fallback. *)
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

let reasoning_details_text
      ~(reasoning_content : string option)
      ~(details : reasoning_detail list)
  : string
  =
  let details_text =
    details
    |> List.filter_map (fun (detail : reasoning_detail) -> detail.text)
    |> String.concat ""
  in
  match reasoning_content with
  | Some content -> if content = "" then details_text else content
  | None -> details_text
;;

(** Message metadata: extensible typed key-value pairs attached to a message.
    Keys are caller-defined strings; values are JSON payloads. *)
type metadata = (string * Yojson.Safe.t) list [@@deriving show]

(** A single message in the conversation.
    [name] identifies the speaker (e.g. tool result source).
    [tool_call_id] links a tool result back to its tool_use request. *)
type message =
  { role : role
  ; content : content_block list
  ; name : string option [@default None]
  ; tool_call_id : string option [@default None]
  ; metadata : metadata [@default []]
  }
[@@deriving show]

(** {1 Response Types} *)

(** Stop reason from API.
    2025-2026 extended: Refusal, ContentFilter, RepetitionTruncation,
    PauseTurn, Compaction, ContextWindowExceeded. *)
type stop_reason =
  | EndTurn
  | StopToolUse
  | MaxTokens
  | StopSequence
  | Refusal (** Policy refusal (Anthropic, OpenAI, Gemini SAFETY). *)
  | ContentFilter (** Provider content-policy filter terminated generation. *)
  | RepetitionTruncation (** Provider repetition guard terminated generation. *)
  | PauseTurn (** Anthropic long-running turn pause. *)
  | Compaction (** Anthropic context compaction. *)
  | ContextWindowExceeded (** Anthropic context window exceeded. *)
  | UnmatchedToolCalls
  (** Internal fail-closed response shape: a provider claimed a tool turn
          but no executable tool block was assembled. This is not a provider
          terminal reason and is constructed only after wire reconciliation. *)
  | Unknown of string
[@@deriving show]

let stop_reason_of_string = function
  | "end_turn" -> EndTurn
  | "tool_use" -> StopToolUse
  | "max_tokens" -> MaxTokens
  | "stop_sequence" -> StopSequence
  | "refusal" -> Refusal
  | "content_filter" -> ContentFilter
  | "repetition_truncation" -> RepetitionTruncation
  | "pause_turn" -> PauseTurn
  | "compaction" -> Compaction
  | "model_context_window_exceeded" -> ContextWindowExceeded
  | "unmatched_tool_calls" -> UnmatchedToolCalls
  | other -> Unknown other
;;

(* Canonical wire serialization of [stop_reason]: the exact inverse of
   [stop_reason_of_string]. [stop_reason_of_string (stop_reason_to_string r) = r]
   holds for every constructor (with the inherent caveat that [Unknown s]
   collapses to its decoded constructor when [s] is itself a known wire token).
   SSOT for stop-reason wire strings — callers must delegate here instead of
   re-spelling the literals, which previously drifted across modules
   (e.g. "tool_use" vs "stop_tool_use"). *)
let stop_reason_to_string = function
  | EndTurn -> "end_turn"
  | StopToolUse -> "tool_use"
  | MaxTokens -> "max_tokens"
  | StopSequence -> "stop_sequence"
  | Refusal -> "refusal"
  | ContentFilter -> "content_filter"
  | RepetitionTruncation -> "repetition_truncation"
  | PauseTurn -> "pause_turn"
  | Compaction -> "compaction"
  | ContextWindowExceeded -> "model_context_window_exceeded"
  | UnmatchedToolCalls -> "unmatched_tool_calls"
  | Unknown s -> s
;;

(* Stable, low-cardinality telemetry label for [stop_reason]. Identical to
   [stop_reason_to_string] except [Unknown _] collapses to the constant
   ["unknown"] so provider-supplied raw strings cannot explode metric-label
   cardinality. Use for Otel/metric labels; use [stop_reason_to_string] for
   wire/round-trip serialization. The explicit constructor list (rather than a
   wildcard) forces a compile error if a new [stop_reason] variant is added. *)
let stop_reason_to_metric_label = function
  | Unknown _ -> "unknown"
  | ( EndTurn
    | StopToolUse
    | MaxTokens
    | StopSequence
    | Refusal
    | ContentFilter
    | RepetitionTruncation
    | PauseTurn
    | Compaction
    | ContextWindowExceeded
    | UnmatchedToolCalls ) as r -> stop_reason_to_string r
;;

(** API usage from a single response *)
type api_usage =
  { input_tokens : int
  ; output_tokens : int
  ; cache_creation_input_tokens : int
  ; cache_read_input_tokens : int
  ; cost_usd : float option
  }
[@@deriving show, yojson]

(** Provider-reported inference timing from a single API call.
    llama-server populates all fields; cloud providers return [None]. *)
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

(** Provider request envelope that carries the output-token budget.
    This identifies the wire contract, not the provider brand: providers
    using an OpenAI-compatible endpoint share the corresponding envelope. *)
type output_token_envelope =
  | Openai_chat_max_tokens
  | Openai_responses_max_output_tokens
  | Anthropic_messages_max_tokens
  | Gemini_generation_config_max_output_tokens
  | Ollama_options_num_predict
[@@deriving show, yojson]

type output_token_policy =
  | Omitted
  | Explicit
  | Explicit_clamped
  | Required_catalog_fallback
[@@deriving show, yojson]

type output_token_ceiling_source =
  | Catalog_model
  | Declared_capability_override
[@@deriving show, yojson]

type output_token_ceiling =
  { value : int
  ; source : output_token_ceiling_source
  }
[@@deriving show]

let output_token_ceiling ~value ~source =
  if value <= 0
  then invalid_arg "output_token_ceiling: value must be positive"
  else { value; source }
;;

type output_token_resolution =
  | Omitted_resolution of { ceiling : output_token_ceiling option }
  | Explicit_resolution of
      { value : int
      ; ceiling : output_token_ceiling option
      }
  | Explicit_clamped_resolution of
      { requested : int
      ; ceiling : output_token_ceiling
      }
  | Required_catalog_fallback_resolution of { ceiling : output_token_ceiling }
[@@deriving show]

(** Construction-controlled receipt for the output-token value serialized on
    the provider wire. The flat JSON projection exposes requested/effective,
    policy, ceiling, and envelope while the internal sum type prevents invalid
    combinations in OCaml. *)
type output_token_receipt =
  { envelope : output_token_envelope
  ; resolution : output_token_resolution
  }
[@@deriving show]

type required_output_token_error = Required_output_token_catalog_ceiling_missing
[@@deriving show, eq]

let output_token_receipt_requested receipt =
  match receipt.resolution with
  | Omitted_resolution _ | Required_catalog_fallback_resolution _ -> None
  | Explicit_resolution { value; _ } -> Some value
  | Explicit_clamped_resolution { requested; _ } -> Some requested
;;

let output_token_receipt_effective receipt =
  match receipt.resolution with
  | Omitted_resolution _ -> None
  | Explicit_resolution { value; _ } -> Some value
  | Explicit_clamped_resolution { ceiling; _ }
  | Required_catalog_fallback_resolution { ceiling } -> Some ceiling.value
;;

let output_token_receipt_policy receipt =
  match receipt.resolution with
  | Omitted_resolution _ -> Omitted
  | Explicit_resolution _ -> Explicit
  | Explicit_clamped_resolution _ -> Explicit_clamped
  | Required_catalog_fallback_resolution _ -> Required_catalog_fallback
;;

let output_token_receipt_ceiling receipt =
  let ceiling =
    match receipt.resolution with
    | Omitted_resolution { ceiling } | Explicit_resolution { ceiling; _ } -> ceiling
    | Explicit_clamped_resolution { ceiling; _ }
    | Required_catalog_fallback_resolution { ceiling } -> Some ceiling
  in
  Option.map (fun value -> value.value) ceiling
;;

let output_token_receipt_ceiling_source receipt =
  let ceiling =
    match receipt.resolution with
    | Omitted_resolution { ceiling } | Explicit_resolution { ceiling; _ } -> ceiling
    | Explicit_clamped_resolution { ceiling; _ }
    | Required_catalog_fallback_resolution { ceiling } -> Some ceiling
  in
  Option.map (fun value -> value.source) ceiling
;;

let optional_output_token_receipt ~envelope ~requested ~ceiling =
  (match requested with
   | Some value when value < 0 ->
     invalid_arg "optional_output_token_receipt: requested value must be non-negative"
   | None | Some _ -> ());
  let resolution =
    match requested, ceiling with
    | None, ceiling -> Omitted_resolution { ceiling }
    | Some requested, Some ceiling when requested > ceiling.value ->
      Explicit_clamped_resolution { requested; ceiling }
    | Some value, ceiling -> Explicit_resolution { value; ceiling }
  in
  { envelope; resolution }
;;

let required_output_token_receipt receipt =
  match receipt with
  | { resolution =
        Omitted_resolution { ceiling = Some ({ source = Catalog_model; _ } as ceiling) }
    ; _
    } -> Ok { receipt with resolution = Required_catalog_fallback_resolution { ceiling } }
  | { resolution =
        Omitted_resolution
          { ceiling = Some { source = Declared_capability_override; _ } | None }
    ; _
    } -> Error Required_output_token_catalog_ceiling_missing
  | receipt -> Ok receipt
;;

let output_token_receipt_to_yojson receipt =
  let option_int_to_yojson = function
    | Some value -> `Int value
    | None -> `Null
  in
  `Assoc
    [ "requested", option_int_to_yojson (output_token_receipt_requested receipt)
    ; "effective", option_int_to_yojson (output_token_receipt_effective receipt)
    ; "policy", output_token_policy_to_yojson (output_token_receipt_policy receipt)
    ; "ceiling", option_int_to_yojson (output_token_receipt_ceiling receipt)
    ; ( "ceiling_source"
      , match output_token_receipt_ceiling_source receipt with
        | Some source -> output_token_ceiling_source_to_yojson source
        | None -> `Null )
    ; "envelope", output_token_envelope_to_yojson receipt.envelope
    ]
;;

let output_token_receipt_of_yojson json =
  let open Yojson.Safe.Util in
  let token_value_option = function
    | `Null -> Ok None
    | `Int value when value >= 0 -> Ok (Some value)
    | `Int _ -> Error "output_token_receipt: token values must be non-negative"
    | _ -> Error "output_token_receipt: expected integer or null"
  in
  let ceiling_option = function
    | `Null -> Ok None
    | `Int value when value > 0 -> Ok (Some value)
    | `Int _ -> Error "output_token_receipt: ceiling must be positive"
    | _ -> Error "output_token_receipt: expected integer or null"
  in
  let ( let* ) result f = Result.bind result f in
  try
    let* requested = token_value_option (member "requested" json) in
    let* effective = token_value_option (member "effective" json) in
    let* ceiling = ceiling_option (member "ceiling" json) in
    let* ceiling_source =
      match member "ceiling_source" json with
      | `Null -> Ok None
      | source_json ->
        Result.map
          (fun source -> Some source)
          (output_token_ceiling_source_of_yojson source_json)
    in
    let* ceiling =
      match ceiling, ceiling_source with
      | None, None -> Ok None
      | Some value, Some source -> Ok (Some (output_token_ceiling ~value ~source))
      | Some _, None | None, Some _ ->
        Error "output_token_receipt: ceiling and ceiling_source must appear together"
    in
    let* policy = output_token_policy_of_yojson (member "policy" json) in
    let* envelope = output_token_envelope_of_yojson (member "envelope" json) in
    match policy, requested, effective, ceiling with
    | Omitted, None, None, ceiling ->
      Ok { envelope; resolution = Omitted_resolution { ceiling } }
    | Explicit, Some requested, Some effective, ceiling
      when requested = effective
           &&
           match ceiling with
           | Some cap -> effective <= cap.value
           | None -> true ->
      Ok { envelope; resolution = Explicit_resolution { value = effective; ceiling } }
    | Explicit_clamped, Some requested, Some effective, Some ceiling
      when effective = ceiling.value && requested > ceiling.value ->
      Ok { envelope; resolution = Explicit_clamped_resolution { requested; ceiling } }
    | Required_catalog_fallback, None, Some effective, Some ceiling
      when ceiling.source = Catalog_model && effective = ceiling.value ->
      Ok { envelope; resolution = Required_catalog_fallback_resolution { ceiling } }
    | _ -> Error "output_token_receipt: inconsistent requested/effective policy fields"
  with
  | Yojson.Safe.Util.Type_error (message, _) -> Error message
;;

(** Per-call inference telemetry assembled from provider responses and
    transport measurements. Request-side output-token receipts are delivered
    separately by {!Complete.complete}'s observer so a cached or injected
    response cannot impersonate an OAS-built wire request. *)
type inference_telemetry =
  { system_fingerprint : string option
  ; timings : inference_timings option
  ; reasoning_tokens : int option
  ; reasoning_tokens_estimated : bool
  ; request_latency_ms : int option
  ; peak_memory_gb : float option
  ; provider_kind : Provider_kind.t option
  ; reasoning_effort : string option
  ; canonical_model_id : string option
  ; effective_context_window : int option
  ; provider_internal_action_count : int option
  ; ttfrc_ms : float option
  ; prefill_ms : float option
  }
[@@deriving show, yojson]

let default_inference_telemetry : inference_telemetry =
  { system_fingerprint = None
  ; timings = None
  ; reasoning_tokens = None
  ; reasoning_tokens_estimated = false
  ; request_latency_ms = None
  ; peak_memory_gb = None
  ; provider_kind = None
  ; reasoning_effort = None
  ; canonical_model_id = None
  ; effective_context_window = None
  ; provider_internal_action_count = None
  ; ttfrc_ms = None
  ; prefill_ms = None
  }
;;

(** API response *)
type api_response =
  { id : string
  ; model : string
  ; stop_reason : stop_reason
  ; content : content_block list
  ; usage : api_usage option
  ; telemetry : inference_telemetry option
  }
[@@deriving show]

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
          does not concatenate it into invalid JSON (e.g.
          [{"limit":10}{"limit":10}]). *)
  | MediaDelta of
      { media_type : string
      ; source_type : media_source_kind
      ; data : string
      }
  (** A chunk of a streamed media (image/document/audio) content block.
            Carries the block-level [media_type] and [source_type] alongside the
            [data] payload so the SSE layer needs no new {!ContentBlockStart}
            fields; the accumulator records the metadata (idempotent across
            chunks) and concatenates [data]. *)

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
        (** Provider error-object [type] (e.g. ["rate_limit_exceeded"]),
                the streaming-time discriminator. Lets a mid-stream error
                converge onto the same classification path as an initial HTTP
                error instead of collapsing to [NetworkError {Unknown}].
                [None] when the provider omits it. *)
      ; raw : string
        (** Original error payload JSON, carried verbatim so the consumer
                can feed it to [Retry.classify_error] (retry_after, hard-quota
                detection) exactly as the non-streaming path does. *)
      }
  | SSEParseFailed of
      { raw : string
      ; reason : string
      }
  | SSEUnknownEventType of
      { event_type : string
      ; raw : string
      }
  | Connected
  | Timeout of string
  | StreamIncomplete of { reason : string }

(** Terminal error captured while accumulating an SSE stream.

    The accumulator stores this typed value (not a flattened string) so the
    consumer can route a provider-reported error through the same
    [Http_client.HttpError {code; body}] -> [Retry.classify_error] path the
    non-streaming boundary uses, while a genuine wire/parse failure stays an
    unclassifiable network error. Replaces the prior [string] carrier that
    collapsed rate-limit / auth / server errors into one [NetworkError
    {Unknown}] bucket. *)
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

(** {1 Convenience Constructors}

    Convenience constructors for consumers that work with flat [string]
    messages and need to convert to [content_block list]. *)

(** Create a message with default [None] for optional fields. *)
let make_message ?name ?tool_call_id ?(metadata = []) ~role content =
  { role; content; name; tool_call_id; metadata }
;;

(** Create a text content block. *)
let text_block text = Text text

(** Create a base64-backed image content block by default. *)
let image_block ?(source_type = Base64) ~media_type ~data () =
  Image { media_type; data; source_type }
;;

(** Create a base64-backed document content block by default. *)
let document_block ?(source_type = Base64) ~media_type ~data () =
  Document { media_type; data; source_type }
;;

(** Create a base64-backed audio content block by default. *)
let audio_block ?(source_type = Base64) ~media_type ~data () =
  Audio { media_type; data; source_type }
;;

(** Create a text-only message. *)
let text_message role text = make_message ~role [ Text text ]

(** Create a user message from arbitrary content blocks. *)
let user_msg_blocks blocks = make_message ~role:User blocks

(** Create a system message. *)
let system_msg text = text_message System text

(** Create a user message. *)
let user_msg text = text_message User text

(** Create an assistant message. *)
let assistant_msg text = text_message Assistant text

(** Try to parse content as JSON, returning None on failure. *)
let try_parse_json (s : string) : Yojson.Safe.t option =
  if String.length s = 0
  then None
  else (
    match Yojson.Safe.from_string s with
    | json -> Some json
    | exception Yojson.Json_error _ -> None)
;;

(** Create a tool result message.
    When [json] is not provided, attempts to parse [content] as JSON
    so downstream consumers can access structured data without re-parsing. *)
let tool_result_msg ~tool_use_id ~content ?(is_error = false) ?json () =
  let json =
    match json with
    | Some _ -> json
    | None -> try_parse_json content
  in
  make_message
    ~tool_call_id:tool_use_id
    ~role:Tool
    [ ToolResult { tool_use_id; content; is_error; json; content_blocks = None } ]
;;

(** {1 Tool Result Validation}

    Minimal structural validation for tool result payloads.
    P0 Verification Loop will extend this with full JSON Schema checking. *)

type tool_result_validation_error =
  | Expected_object of string (** Expected JSON object, got other type *)
  | Expected_array of string (** Expected JSON array, got other type *)
  | Empty_content of string (** Tool returned empty content *)
  | Json_parse_failed of string (** Content is not valid JSON *)
[@@deriving show]

(** Validate that a ToolResult's payload matches a minimal expected shape.
    Returns [Ok ()] when the result passes, or a descriptive error.
    This is the foundation for P0's full JSON Schema validation loop. *)
let validate_tool_result_shape
      ~expect_object:(expect_obj : bool)
      ~expect_array:(expect_arr : bool)
      (block : content_block)
  : (unit, tool_result_validation_error) result
  =
  match block with
  | ToolResult { content; json; _ } ->
    if String.length (String.trim content) = 0
    then Error (Empty_content "ToolResult content is empty")
    else if expect_obj || expect_arr
    then (
      match json with
      | None ->
        (* content was not parseable as JSON *)
        Error (Json_parse_failed "ToolResult content is not valid JSON")
      | Some json_value ->
        if expect_obj && not expect_arr
        then (
          match json_value with
          | `Assoc _ -> Ok ()
          | _ -> Error (Expected_object "ToolResult JSON is not an object"))
        else if expect_arr && not expect_obj
        then (
          match json_value with
          | `List _ -> Ok ()
          | _ -> Error (Expected_array "ToolResult JSON is not an array"))
        else
          (* Both allowed — any JSON is fine *)
          Ok ())
    else Ok ()
  | _ -> Ok ()
;;

(** Extract text from content blocks, concatenating with newlines.
    Drops Thinking, Image, ToolUse, etc. *)
let text_of_content content =
  content
  |> List.filter_map (function
    | Text s -> Some s
    | ToolResult { content; _ } -> Some content
    | _ -> None)
  |> String.concat "\n"
;;

(** Extract text from a message. *)
let text_of_message (msg : message) = text_of_content msg.content

(** Extract text from an api_response. *)
let text_of_response (resp : api_response) = text_of_content resp.content

(** Extract end-user-visible assistant text from content blocks.
    This is intentionally narrower than [text_of_content]: tool results are
    model-visible execution payloads, and Thinking blocks are provider reasoning
    payloads. Neither belongs in an answer-text projection. *)
let visible_text_of_content content =
  content
  |> List.filter_map (function
    | Text s -> Some s
    | Thinking _
    | ReasoningDetails _
    | RedactedThinking _
    | ToolUse _
    | ToolResult _
    | Image _
    | Document _
    | Audio _ -> None)
  |> String.concat "\n"
;;

(** Extract end-user-visible assistant text from a message. *)
let visible_text_of_message (msg : message) = visible_text_of_content msg.content

(** Extract end-user-visible assistant text from an api_response. *)
let visible_text_of_response (resp : api_response) = visible_text_of_content resp.content

(** {1 Usage Helpers} *)

let zero_api_usage =
  { input_tokens = 0
  ; output_tokens = 0
  ; cache_creation_input_tokens = 0
  ; cache_read_input_tokens = 0
  ; cost_usd = None
  }
;;

let usage_of_response (resp : api_response) = resp.usage
let total_tokens (usage : api_usage) = usage.input_tokens + usage.output_tokens
