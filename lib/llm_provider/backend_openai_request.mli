(** OpenAI-compatible request body building.

    This module owns provider-config -> Chat Completions JSON request
    construction. {!Backend_openai} re-exports the public surface for
    backwards compatibility while response parsing and message serialization
    stay in their existing modules.

    @stability Internal *)

val warn_capability_drop : model_id:string -> field:string -> unit
val effective_tool_choice : Provider_config.t -> Yojson.Safe.t option
val effective_tools : Provider_config.t -> Yojson.Safe.t list -> Yojson.Safe.t list
val structured_schema_of_config : Provider_config.t -> Yojson.Safe.t option
val capabilities_of_config : Provider_config.t -> Capabilities.capabilities

(** Resolve the output-token budget emitted on the wire: caller override
    clamped to the capability ceiling (one-shot WARN on clamp), the model
    capability when the caller sends none, then the unknown-model fallback.
    Chat Completions emits the value as [max_tokens], the Responses envelope
    as [max_output_tokens] — the field name is per-envelope, the resolution
    policy is single-sourced here. *)
val effective_max_output_tokens : Provider_config.t -> int

(** Prepend the sampling [(field, value)] to [body] unless the reasoning
    dialect suppresses that parameter, in which case the field is dropped with a
    one-shot WARN per ([model_id], [field]). *)
val add_sampling_field
  :  Reasoning_dialect.t
  -> Provider_config.t
  -> Capabilities.sampling_parameter
  -> Yojson.Safe.t
  -> (string * Yojson.Safe.t) list
  -> (string * Yojson.Safe.t) list

(** Shared tool_choice emission gate for the Chat and Responses envelopes:
    explicit forcing ([Any] / [Tool _]) is always emitted (validation fails
    closed on unsupported forcing), advisory [Auto] only when the model
    supports tool_choice ([supports_tool_choice_override] wins over the
    capability record). *)
val should_emit_tool_choice : Provider_config.t -> bool

val openai_json_schema_payload : Yojson.Safe.t -> Yojson.Safe.t
val response_format_to_openai_json : Types.response_format -> Yojson.Safe.t option
val response_format_of_config : Provider_config.t -> Yojson.Safe.t option

(** [build_request_assoc] is {!build_request} before the final
    [Yojson.Safe.to_string]; sibling backends (e.g. {!Backend_glm}) mutate the
    Assoc directly instead of parsing the serialized string back. *)
val build_request_assoc
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> Yojson.Safe.t

val build_request
  :  ?stream:bool
  -> config:Provider_config.t
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> string
