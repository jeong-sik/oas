(** Agent state checkpoint -- versioned JSON serialization.

    Captures full conversation state (messages, usage, config) as a
    pure value for persist/restore. This module handles serialization
    only; file I/O is left to the caller.

    @stability Stable
    @since 0.93.1 *)

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

type message_splice = {
  start_index: int;
  delete_count: int;
  insert: Types.message list;
}

type identity_patch = {
  session_id: string;
  agent_name: string;
  model: Types.model;
  created_at: float;
}

type sampling_patch = {
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  thinking_budget: int option;
}

type limits_patch = {
  disable_parallel_tool_use: bool;
  response_format_json: bool;
  cache_system_prompt: bool;
  max_input_tokens: int option;
  max_total_tokens: int option;
}

type delta_op =
  | Replace_identity of identity_patch
  | Replace_system_prompt of string option
  | Splice_messages of message_splice
  | Replace_usage of Types.usage_stats
  | Replace_turn_count of int
  | Replace_tools of Types.tool_schema list
  | Replace_tool_choice of Types.tool_choice option
  | Replace_sampling of sampling_patch
  | Replace_limits of limits_patch
  | Patch_context of Context.diff
  | Replace_mcp_sessions of Mcp_session.info list
  | Replace_working_context of Yojson.Safe.t option

type delta = {
  delta_version: int;
  base_checkpoint_version: int;
  base_checkpoint_hash: string;
  result_checkpoint_hash: string;
  created_at: float;
  operations: delta_op list;
}

type delta_restore_mode =
  | Delta_applied
  | Full_restore

type delta_restore_result = {
  checkpoint: t;
  mode: delta_restore_mode;
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

(** Serialize a checkpoint delta sidecar to JSON. *)
val delta_to_json : delta -> Yojson.Safe.t

(** Deserialize a checkpoint delta sidecar from JSON. *)
val delta_of_json : Yojson.Safe.t -> (delta, Error.sdk_error) result

(** Whether the delta checkpoint feature flag is enabled.
    Checks [OAS_DELTA_CHECKPOINT] first, falls back to the deprecated
    [MASC_DELTA_CHECKPOINT] for backward compatibility. *)
val delta_enabled : unit -> bool

(** Compute a delta from a base checkpoint to a target checkpoint. *)
val compute_delta : t -> t -> delta

(** Apply a delta to a base checkpoint. *)
val apply_delta : t -> delta -> (t, Error.sdk_error) result

(** Apply delta when enabled and valid, otherwise fall back to the full checkpoint. *)
val restore_with_delta_fallback :
  ?metrics:Metrics.t ->
  base:t ->
  delta:delta ->
  full_checkpoint:t ->
  unit ->
  (delta_restore_result, Error.sdk_error) result

(** {1 Queries} *)

(** Number of messages in the checkpoint. *)
val message_count : t -> int

(** Usage stats from the checkpoint. *)
val token_usage : t -> Types.usage_stats

(** {1 Usage stats JSON helpers} *)

(** Serialize usage_stats to JSON. Reusable by downstream modules
    (e.g., swarm checkpoint telemetry). *)
val usage_to_json : Types.usage_stats -> Yojson.Safe.t

(** Deserialize usage_stats from JSON. *)
val usage_of_json : Yojson.Safe.t -> Types.usage_stats
