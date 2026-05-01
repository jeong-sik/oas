open Base
(** Session-scoped persistent store for tool result content.

    Moves large tool results to the filesystem and returns previews.
    Replaces {!Context_offload} with tool_use_id-keyed storage,
    read-back API, and session-scoped directory layout.

    Layout: [<storage_dir>/<session_id>/tool-results/<tool_use_id>.txt]

    All I/O goes through {!Fs_result} for atomic writes and
    Result-based error handling.  Fail-open: on write failure,
    the caller gets [Error _] and can keep original content.

    @stability Evolving
    @since 0.128.0 *)

(** {1 Configuration} *)

type config =
  { storage_dir : string
    (** Base directory.  Consumer-provided (the application's data root). *)
  ; session_id : string (** Session identifier.  Used as subdirectory name. *)
  ; threshold_chars : int
    (** Per-result persist threshold.  Content exceeding this is
        written to disk.  Default: 50,000.  Pass [0] to disable. *)
  ; preview_chars : int (** Preview length kept in the message.  Default: 2,000. *)
  ; aggregate_budget : int
    (** Per-message aggregate budget in chars.  When total tool result
        content in a single message exceeds this, the largest fresh
        results are persisted until under budget.
        Default: 200,000.  Pass [0] to disable.
        @since 0.129.0 *)
  }

(** Default per-result threshold: 50,000 chars. *)
val default_threshold_chars : int

(** Default preview length: 2,000 chars. *)
val default_preview_chars : int

(** Default per-message aggregate budget: 200,000 chars.
    @since 0.129.0 *)
val default_aggregate_budget : int

(** Apply environment variable overrides to a config.

    Reads:
    - [OAS_TOOL_RESULT_THRESHOLD] -> {!config.threshold_chars}
    - [OAS_TOOL_RESULT_PREVIEW_LEN] -> {!config.preview_chars}
    - [OAS_TOOL_RESULT_AGGREGATE_BUDGET] -> {!config.aggregate_budget}

    Non-numeric or absent values keep the original config field.
    @since 0.130.0 *)
val config_with_env_overrides : config -> config

(** {1 Store lifecycle} *)

(** Abstract store handle.  Not thread-safe; each agent owns one. *)
type t

(** Create a store.  Ensures session directory exists.
    Returns [Error] if directory creation fails. *)
val create : config -> (t, Error.sdk_error) result

(** Access store configuration. *)
val config : t -> config

(** {1 Persist / Read} *)

(** Persist content to disk.  Returns the preview string.

    Skips write if a file for [tool_use_id] already exists
    (idempotent on replay / microcompact).

    Preview generation: takes first [config.preview_chars] bytes,
    finds last newline in [[preview_chars*0.5, preview_chars]] and
    cuts there for readability.  Appends a size marker. *)
val persist
  :  t
  -> tool_use_id:string
  -> content:string
  -> (string, Error.sdk_error) result

(** Read back full content for a tool_use_id.
    Returns [Error] if file does not exist or I/O fails. *)
val read : t -> tool_use_id:string -> (string, Error.sdk_error) result

(** Check whether content has been persisted for this tool_use_id.
    Pure filesystem stat, no I/O read. *)
val has : t -> tool_use_id:string -> bool

(** {1 Security} *)

(** Sanitize a tool_use_id for safe use as a filename.
    Strips all characters except [a-zA-Z0-9_-].
    Returns [Error] if the result is empty.
    @since 0.129.0 *)
val sanitize_tool_use_id : string -> (string, Error.sdk_error) result

(** {1 Preview} *)

(** Generate preview from content.  Pure function, no I/O.
    Exported for testing and for callers that need the preview
    without persisting. *)
val generate_preview : preview_chars:int -> string -> string

(** {1 Cleanup} *)

(** Remove all stored results for this session.
    Deletes the session subdirectory and its contents. *)
val cleanup : t -> (unit, Error.sdk_error) result
