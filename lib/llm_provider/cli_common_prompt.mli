(** Shared prompt-shaping helpers for non-interactive CLI transports.

    All CLI transports (Claude Code, Gemini CLI, Codex CLI) flatten an
    OAS message history into a single prompt string plus an optional
    system prompt. This module centralises that logic so bug fixes
    propagate across transports. *)

val text_of_block : Types.content_block -> string option
(** [text_of_block block] extracts plain text from a [Text] block,
    or [None] for non-text blocks (ToolUse, ToolResult, Image, …). *)

val string_of_role : Types.role -> string
(** Human-readable label for a role, used to prefix earlier turns
    when flattening multi-turn history. *)

val prompt_of_messages : Types.message list -> string
(** Flatten messages into a single prompt string. The last message
    becomes the bare tail; earlier messages are prefixed with their
    role label and joined by blank lines. Returns [""] for an empty
    list. *)

val non_system_messages : Types.message list -> Types.message list
(** Drop [System] messages. CLI transports convey the system prompt
    via a dedicated [--system-prompt] flag, so it should not be
    duplicated inside the flattened prompt string. *)

val system_prompt_of :
  req_config:Provider_config.t ->
  Types.message list ->
  string option
(** Extract a system prompt: prefer [req_config.system_prompt],
    then fall back to the first [System] message. *)
