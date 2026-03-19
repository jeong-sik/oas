(** Agent state checkpoint — versioned JSON serialization.

    Captures conversation state (messages, usage, config) as a pure
    value. This module handles serialization only; file I/O is left
    to the caller. *)

open Types

val checkpoint_version : int

type t = {
  version: int;
  session_id: string;
  agent_name: string;
  model: model;
  system_prompt: string option;
  messages: message list;
  usage: usage_stats;
  turn_count: int;
  created_at: float;
  tools: tool_schema list;
  tool_choice: tool_choice option;
  disable_parallel_tool_use: bool;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  response_format_json: bool;
  thinking_budget: int option;
  cache_system_prompt: bool;
  max_input_tokens: int option;
  max_total_tokens: int option;
  context: Context.t;
  mcp_sessions: Mcp_session.info list;
  working_context: Yojson.Safe.t option;
}

(** {1 Serialization} *)

val usage_to_json : usage_stats -> Yojson.Safe.t
val usage_of_json : Yojson.Safe.t -> usage_stats
val tool_param_to_json : tool_param -> Yojson.Safe.t
val tool_param_of_json : Yojson.Safe.t -> (tool_param, Error.sdk_error) result
val tool_schema_to_json : tool_schema -> Yojson.Safe.t
val tool_schema_of_json : Yojson.Safe.t -> (tool_schema, Error.sdk_error) result
val message_of_json : Yojson.Safe.t -> (message, Error.sdk_error) result
val content_block_of_json_strict : Yojson.Safe.t -> (content_block, Error.sdk_error) result

val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
val to_string : t -> string
val of_string : string -> (t, Error.sdk_error) result

(** {1 Queries} *)

val message_count : t -> int
val token_usage : t -> usage_stats
