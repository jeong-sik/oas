(** Unified LLM provider types -- shared between OAS and MASC.

    Single source of truth for message, response, tool, and streaming types.
    Both agent_sdk and masc-mcp consume these types directly.

    @since 0.42.0 *)

(** {1 Message Types} *)

(** Role in a conversation.
    4-variant superset: OAS uses User/Assistant, MASC uses all four. *)
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
}
[@@deriving show]

type api_response = {
  id: string;
  model: string;
  stop_reason: stop_reason;
  content: content_block list;
  usage: api_usage option;
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

val text_message : role -> string -> message
val system_msg : string -> message
val user_msg : string -> message
val assistant_msg : string -> message
val tool_result_msg : tool_use_id:string -> content:string -> ?is_error:bool -> unit -> message
val text_of_content : content_block list -> string
val text_of_message : message -> string
