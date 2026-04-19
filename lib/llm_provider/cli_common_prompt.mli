(** Shared prompt-shaping helpers for non-interactive CLI transports.

    All CLI transports (Claude Code, Gemini CLI, Codex CLI) flatten an
    OAS message history into a single prompt string plus an optional
    system prompt. This module centralises that logic so bug fixes
    propagate across transports. *)

val text_of_block : Types.content_block -> string option
(** [text_of_block block] extracts plain text from a [Text] block,
    or [None] for non-text blocks (ToolUse, ToolResult, Image, …). *)

val render_block_with_tools : Types.content_block -> string option
(** Like {!text_of_block} but also renders [ToolUse] as
    [\[tool_use id=X name=Y\] <json input>] and [ToolResult] as
    [\[tool_result id=X\] <content>] (or [\[tool_result (error) id=X\]
    …]).  Thinking, Image, Document and Audio blocks are still dropped
    — the CLI should never see its own thinking looped back in the
    next turn's prompt. *)

val string_of_role : Types.role -> string
(** Human-readable label for a role, used to prefix earlier turns
    when flattening multi-turn history. *)

val prompt_of_messages :
  ?include_tool_blocks:bool ->
  Types.message list ->
  string
(** Flatten messages into a single prompt string. The last message
    becomes the bare tail; earlier messages are prefixed with their
    role label and joined by blank lines. Returns [""] for an empty
    list.

    When [include_tool_blocks] is [true] (default [false]), each
    content block is rendered through {!render_block_with_tools}
    instead of {!text_of_block}, so prior [ToolUse]/[ToolResult] pairs
    survive the flattening and the CLI can reconstruct the tool
    history in its next turn. *)

val non_system_messages : Types.message list -> Types.message list
(** Drop [System] messages from the conversational turn history. CLI
    transports may pass the extracted system prompt separately or fold
    it back into the final prompt via {!prompt_with_system_prompt}. *)

val system_prompt_of :
  req_config:Provider_config.t ->
  Types.message list ->
  string option
(** Extract a system prompt: prefer [req_config.system_prompt],
    then fall back to the first [System] message. *)

val prompt_with_system_prompt :
  prompt:string ->
  system_prompt:string option ->
  string
(** Prepend a textual [System] section to [prompt] when a dedicated
    CLI flag is unavailable or unsupported. *)
