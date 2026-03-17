(** Core types for Anthropic Agent SDK. *)

(** Supported Claude models *)
type model =
  | Claude_opus_4_6
  | Claude_sonnet_4_6
  | Claude_opus_4_5
  | Claude_sonnet_4
  | Claude_haiku_4_5
  | Claude_3_7_sonnet
  | Custom of string
[@@deriving yojson, show]

val model_to_string : model -> string

(** Message role *)
type role = User | Assistant
[@@deriving yojson, show]

val role_to_string : role -> string

(** Tool parameter schema *)
type param_type = String | Integer | Number | Boolean | Array | Object
[@@deriving yojson, show]

val param_type_to_string : param_type -> string

(** Tool execution result types *)
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

(** Tool definition *)
type tool_schema = {
  name: string;
  description: string;
  parameters: tool_param list;
}
[@@deriving yojson, show]

(** Tool choice mode *)
type tool_choice =
  | Auto
  | Any
  | Tool of string
  | None_
[@@deriving show]

val tool_choice_to_json : tool_choice -> Yojson.Safe.t
val tool_choice_of_json : Yojson.Safe.t -> (tool_choice, Error.sdk_error) result

(** Content block types *)
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

(** A single message in the conversation *)
type message = {
  role: role;
  content: content_block list;
}
[@@deriving show]

(** Stop reason from API *)
type stop_reason =
  | EndTurn
  | StopToolUse
  | MaxTokens
  | StopSequence
  | Unknown of string
[@@deriving show]

val stop_reason_of_string : string -> stop_reason

(** API usage from a single response *)
type api_usage = {
  input_tokens: int;
  output_tokens: int;
  cache_creation_input_tokens: int;
  cache_read_input_tokens: int;
}
[@@deriving show]

(** API response *)
type api_response = {
  id: string;
  model: string;
  stop_reason: stop_reason;
  content: content_block list;
  usage: api_usage option;
}
[@@deriving show]

(** Agent configuration *)
type agent_config = {
  name: string;
  model: model;
  system_prompt: string option;
  max_tokens: int;
  max_turns: int;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  response_format_json: bool;
  thinking_budget: int option;
  tool_choice: tool_choice option;
  disable_parallel_tool_use: bool;
  cache_system_prompt: bool;
  max_input_tokens: int option;
  max_total_tokens: int option;
}
[@@deriving show]

val default_config : agent_config

(** SSE streaming event types *)
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

(** Usage tracking *)
type usage_stats = {
  total_input_tokens: int;
  total_output_tokens: int;
  total_cache_creation_input_tokens: int;
  total_cache_read_input_tokens: int;
  api_calls: int;
  estimated_cost_usd: float;
}
[@@deriving show]

val empty_usage : usage_stats
val add_usage : usage_stats -> api_usage -> usage_stats

(** Agent state *)
type agent_state = {
  config: agent_config;
  messages: message list;
  turn_count: int;
  usage: usage_stats;
}
[@@deriving show]
