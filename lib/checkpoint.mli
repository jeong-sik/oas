(** Agent state checkpoint -- versioned JSON serialization.

    Captures full conversation state (messages, usage, config) as a
    pure value for persist/restore. This module handles serialization
    only; file I/O is left to the caller. *)

(** {1 Version} *)

(** Current checkpoint format version. *)
val checkpoint_version : int

(** {1 Checkpoint type} *)

type t = {
  version: int;
  session_id: string;
  agent_name: string;
  model: Types.model;
  system_prompt: string option;
  messages: Types.message list;
  usage: Types.usage_stats;
  turn_count: int;
  created_at: float;
  tools: Types.tool_schema list;
  tool_choice: Types.tool_choice option;
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

(** Serialize checkpoint to JSON. *)
val to_json : t -> Yojson.Safe.t

(** Deserialize checkpoint from JSON. Supports versions 1-4. *)
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result

(** Serialize checkpoint to a JSON string. *)
val to_string : t -> string

(** Deserialize checkpoint from a JSON string. *)
val of_string : string -> (t, Error.sdk_error) result

(** {1 Queries} *)

(** Number of messages in the checkpoint. *)
val message_count : t -> int

(** Usage stats from the checkpoint. *)
val token_usage : t -> Types.usage_stats
