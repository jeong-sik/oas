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
type role = System | User | Assistant | Tool
[@@deriving yojson, show]

val role_to_string : role -> string
val role_of_string : string -> role option

(** {1 Tool Types} *)

type param_type = String | Integer | Number | Boolean | Array | Object
[@@deriving yojson, show]

val param_type_to_string : param_type -> string

(** Tool execution result types. *)
type tool_output = { content: string }
type tool_error = { message: string; recoverable: bool }
type tool_result = (tool_output, tool_error) result

type tool_param = {
  name: string;
  description: string;
  param_type: param_type;
  required: bool;
}
[@@deriving yojson, show]

type tool_schema = {
  name: string;
  description: string;
  parameters: tool_param list;
}
[@@deriving yojson, show]

type tool_choice =
  | Auto
  | Any
  | Tool of string
  | None_
[@@deriving show]

val tool_choice_to_json : tool_choice -> Yojson.Safe.t

(** {1 Content Types} *)

type content_block =
  | Text of string
  | Thinking of { thinking_type: string; content: string }
  | RedactedThinking of string
  | ToolUse of { id: string; name: string; input: Yojson.Safe.t }
  | ToolResult of { tool_use_id: string; content: string; is_error: bool }
  | Image of { media_type: string; data: string; source_type: string }
  | Document of { media_type: string; data: string; source_type: string }
  | Audio of { media_type: string; data: string; source_type: string }
[@@deriving show]

type message = {
  role: role;
  content: content_block list;
  name: string option;
  tool_call_id: string option;
}
[@@deriving show]

(** {1 Response Types} *)

type stop_reason =
  | EndTurn
  | StopToolUse
  | MaxTokens
  | StopSequence
  | Unknown of string
[@@deriving show]

val stop_reason_of_string : string -> stop_reason

type api_usage = {
  input_tokens: int;
  output_tokens: int;
  cache_creation_input_tokens: int;
  cache_read_input_tokens: int;
  cost_usd: float option;
}
[@@deriving show, yojson]

type inference_timings = {
  prompt_n: int option;
  prompt_ms: float option;
  prompt_per_second: float option;
  predicted_n: int option;
  predicted_ms: float option;
  predicted_per_second: float option;
  cache_n: int option;
}
[@@deriving show, yojson]

type inference_telemetry = {
  system_fingerprint: string option;
  timings: inference_timings option;
  reasoning_tokens: int option;
  request_latency_ms: int;
}
[@@deriving show, yojson]

type api_response = {
  id: string;
  model: string;
  stop_reason: stop_reason;
  content: content_block list;
  usage: api_usage option;
  telemetry: inference_telemetry option;
}
[@@deriving show]

(** {1 SSE Streaming Types} *)

type content_delta =
  | TextDelta of string
  | ThinkingDelta of string
  | InputJsonDelta of string

type sse_event =
  | MessageStart of { id: string; model: string; usage: api_usage option }
  | ContentBlockStart of { index: int; content_type: string;
                          tool_id: string option; tool_name: string option }
  | ContentBlockDelta of { index: int; delta: content_delta }
  | ContentBlockStop of { index: int }
  | MessageDelta of { stop_reason: stop_reason option; usage: api_usage option }
  | MessageStop
  | Ping
  | SSEError of string

(** {1 Convenience Constructors} *)

val make_message : ?name:string -> ?tool_call_id:string -> role:role -> content_block list -> message
val text_message : role -> string -> message
val system_msg : string -> message
val user_msg : string -> message
val assistant_msg : string -> message
val tool_result_msg : tool_use_id:string -> content:string -> ?is_error:bool -> unit -> message
val text_of_content : content_block list -> string
val text_of_message : message -> string
val text_of_response : api_response -> string

(** {1 Usage Helpers}

    @since 0.78.0 *)

(** Zero-valued usage sentinel for accumulation. *)
val zero_api_usage : api_usage

(** Extract usage from a response, defaulting to {!zero_api_usage}. *)
val usage_of_response : api_response -> api_usage
