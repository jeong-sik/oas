(** Content replacement state for prompt-cache-preserving tool result relocation.

    Implements the "decision freezing" pattern from Claude Code's
    [ContentReplacementState]:

    - Once a tool_use_id enters [seen_ids], its replacement decision
      is frozen forever.
    - On subsequent turns, frozen IDs get their cached preview
      re-applied (if replaced) or kept as-is (if not).
    - No I/O, byte-identical output on replay.

    {b Monotonicity invariant}: once [is_frozen t id] returns [true],
    it will always return [true] regardless of subsequent operations.

    This preserves prompt cache prefixes across turns for all providers
    (Anthropic, OpenAI, Ollama 0.5+).

    @stability Evolving
    @since 0.128.0 *)

(** {1 Types} *)

(** A single replacement record. *)
type replacement = {
  tool_use_id: string;
  preview: string;        (** Exact preview string the model sees *)
  original_chars: int;    (** Size of original content before relocation *)
}

(** {1 State lifecycle} *)

type t
(** Abstract replacement state.
    Not thread-safe; each agent/session owns one instance. *)

(** Create empty state. *)
val create : unit -> t

(** {1 Query} *)

(** Number of tool_use_ids tracked (both replaced and kept). *)
val seen_count : t -> int

(** Is this tool_use_id already frozen (either replaced or kept)? *)
val is_frozen : t -> string -> bool

(** Look up cached replacement for a frozen ID.
    Returns [None] if the ID was kept (not replaced) or unknown. *)
val lookup_replacement : t -> string -> replacement option

(** {1 Record decisions} *)

(** Record a replacement decision.
    @raise Invalid_argument if [tool_use_id] is already frozen. *)
val record_replacement : t -> replacement -> unit

(** Record a "keep" decision — tool_use_id was seen but content
    was below threshold, so it stays in the message.
    @raise Invalid_argument if [tool_use_id] is already frozen. *)
val record_kept : t -> string -> unit

(** {1 Apply to messages} *)

(** Apply frozen replacement decisions to content blocks.

    For each [ToolResult] block in the list:
    - If its [tool_use_id] is in [replacements]: replace content
      with cached preview (byte-identical, no I/O).
    - If its [tool_use_id] is in [seen_ids] but not replaced: keep as-is.
    - Otherwise: include in the returned [fresh] list.

    Returns [(modified_blocks, fresh_tool_use_ids)]. *)
val apply_frozen :
  t -> Types.content_block list ->
  Types.content_block list * string list

(** {1 Serialization} *)

(** Serialize state to JSON for checkpoint persistence.
    The JSON representation includes both [seen_ids] and [replacements]. *)
val to_json : t -> Yojson.Safe.t

(** Deserialize state from JSON.
    Returns [Error] on malformed input. *)
val of_json : Yojson.Safe.t -> (t, Error.sdk_error) result
