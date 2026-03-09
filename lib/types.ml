(** Core types for Anthropic Agent SDK *)

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

let model_to_string = function
  | Claude_opus_4_6 -> "claude-opus-4-6-20250514"
  | Claude_sonnet_4_6 -> "claude-sonnet-4-6-20250514"
  | Claude_opus_4_5 -> "claude-opus-4-5-20251101"
  | Claude_sonnet_4 -> "claude-sonnet-4-20250514"
  | Claude_haiku_4_5 -> "claude-haiku-4-5-20251001"
  | Claude_3_7_sonnet -> "claude-3-7-sonnet-20250219"
  | Custom s -> s

(** Message role *)
type role = User | Assistant
[@@deriving yojson, show]

let role_to_string = function User -> "user" | Assistant -> "assistant"

(** Tool parameter schema *)
type param_type = String | Integer | Number | Boolean | Array | Object
[@@deriving yojson, show]

let param_type_to_string = function
  | String -> "string"
  | Integer -> "integer"
  | Number -> "number"
  | Boolean -> "boolean"
  | Array -> "array"
  | Object -> "object"

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
[@@deriving show]

let tool_choice_to_json = function
  | Auto -> `Assoc [("type", `String "auto")]
  | Any -> `Assoc [("type", `String "any")]
  | Tool name -> `Assoc [("type", `String "tool"); ("name", `String name)]

(** Content block types - Tuple Style for safety *)
type content_block =
  | Text of string
  | Thinking of string * string (* signature, content *)
  | RedactedThinking of string (* data *)
  | ToolUse of string * string * Yojson.Safe.t (* id, name, input *)
  | ToolResult of string * string * bool (* tool_use_id, content, is_error *)
  | Image of { media_type: string; data: string; source_type: string }
    (* source_type = "base64" *)
  | Document of { media_type: string; data: string; source_type: string }
    (* PDF, etc. *)
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

let stop_reason_of_string = function
  | "end_turn" -> EndTurn
  | "tool_use" -> StopToolUse
  | "max_tokens" -> MaxTokens
  | "stop_sequence" -> StopSequence
  | other -> Unknown other

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
  thinking_budget: int option; (* For Claude 3.7+ extended thinking *)
  tool_choice: tool_choice option;
  cache_system_prompt: bool; (* Wrap system prompt with cache_control ephemeral *)
}
[@@deriving show]

let default_config = {
  name = "agent";
  model = Claude_sonnet_4_6;
  system_prompt = None;
  max_tokens = 4096;
  max_turns = 10;
  temperature = None;
  thinking_budget = None;
  tool_choice = None;
  cache_system_prompt = false;
}

(* SSE streaming event types *)
type content_delta =
  | TextDelta of string
  | ThinkingDelta of string
  | InputJsonDelta of string

type sse_event =
  | MessageStart of { id: string; model: string; usage: (int * int) option }
  | ContentBlockStart of { index: int; content_type: string;
                          tool_id: string option; tool_name: string option }
  | ContentBlockDelta of { index: int; delta: content_delta }
  | ContentBlockStop of { index: int }
  | MessageDelta of { stop_reason: stop_reason option; usage: (int * int) option }
  | MessageStop
  | Ping
  | SSEError of string

(* Usage tracking *)
type usage_stats = {
  total_input_tokens: int;
  total_output_tokens: int;
  total_cache_creation_input_tokens: int;
  total_cache_read_input_tokens: int;
  api_calls: int;
}
[@@deriving show]

let empty_usage = {
  total_input_tokens = 0;
  total_output_tokens = 0;
  total_cache_creation_input_tokens = 0;
  total_cache_read_input_tokens = 0;
  api_calls = 0;
}

let add_usage stats (u : api_usage) =
  { total_input_tokens = stats.total_input_tokens + u.input_tokens;
    total_output_tokens = stats.total_output_tokens + u.output_tokens;
    total_cache_creation_input_tokens = stats.total_cache_creation_input_tokens + u.cache_creation_input_tokens;
    total_cache_read_input_tokens = stats.total_cache_read_input_tokens + u.cache_read_input_tokens;
    api_calls = stats.api_calls + 1 }

(** Agent state *)
type agent_state = {
  config: agent_config;
  messages: message list;
  turn_count: int;
  usage: usage_stats;
}
[@@deriving show]
