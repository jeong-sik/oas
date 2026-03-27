(** Unified LLM provider types -- shared between OAS and MASC.

    Single source of truth for message, response, tool, and streaming types.
    Both agent_sdk and masc-mcp consume these types directly.

    @since 0.42.0 -- Phase 1: type extraction from OAS *)

(** {1 Message Types} *)

(** Role in a conversation.
    4-variant: System and Tool added for MASC compatibility.
    OAS uses User/Assistant; MASC uses all four. *)
type role = System | User | Assistant | Tool
[@@deriving yojson, show]

let role_to_string = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"

let role_of_string = function
  | "system" -> Some System
  | "user" -> Some User
  | "assistant" -> Some Assistant
  | "tool" -> Some Tool
  | _ -> None

(** {1 Tool Types} *)

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

(** Tool execution result types.
    Defined before content_block/message/api_response to avoid
    field-name shadowing on the [content] record field. *)
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
  | None_  (** Disables tool use. Anthropic: {type:none}, OpenAI: "none" *)
[@@deriving show]

let tool_choice_to_json = function
  | Auto -> `Assoc [("type", `String "auto")]
  | Any -> `Assoc [("type", `String "any")]
  | Tool name -> `Assoc [("type", `String "tool"); ("name", `String name)]
  | None_ -> `Assoc [("type", `String "none")]

(** {1 Content Types} *)

(** Content block types -- inline records for clarity *)
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

(** A single message in the conversation.
    [name] identifies the speaker (e.g. tool result source).
    [tool_call_id] links a tool result back to its tool_use request. *)
type message = {
  role: role;
  content: content_block list;
  name: string option; [@default None]
  tool_call_id: string option; [@default None]
}
[@@deriving show]

(** {1 Response Types} *)

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
  cost_usd: float option;
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

(** {1 Convenience Constructors}

    These bridge the gap for MASC migration (Phase 3):
    MASC uses flat [string] messages, OAS uses [content_block list].
    These constructors make the transition mechanical. *)

(** Create a message with default [None] for optional fields. *)
let make_message ?name ?tool_call_id ~role content =
  { role; content; name; tool_call_id }

(** Create a text-only message. *)
let text_message role text = make_message ~role [Text text]

(** Create a system message. *)
let system_msg text = text_message System text

(** Create a user message. *)
let user_msg text = text_message User text

(** Create an assistant message. *)
let assistant_msg text = text_message Assistant text

(** Create a tool result message. *)
let tool_result_msg ~tool_use_id ~content ?(is_error=false) () =
  make_message ~tool_call_id:tool_use_id ~role:Tool
    [ToolResult { tool_use_id; content; is_error }]

(** Extract text from content blocks, concatenating with newlines.
    Drops Thinking, Image, ToolUse, etc. *)
let text_of_content content =
  content
  |> List.filter_map (function
       | Text s -> Some s
       | ToolResult { content; _ } -> Some content
       | _ -> None)
  |> String.concat "\n"

(** Extract text from a message. *)
let text_of_message (msg : message) = text_of_content msg.content

(** Extract text from an api_response. *)
let text_of_response (resp : api_response) = text_of_content resp.content

(** {1 Usage Helpers} *)

let zero_api_usage =
  { input_tokens = 0;
    output_tokens = 0;
    cache_creation_input_tokens = 0;
    cache_read_input_tokens = 0;
    cost_usd = None }

let usage_of_response (resp : api_response) =
  match resp.usage with Some u -> u | None -> zero_api_usage
