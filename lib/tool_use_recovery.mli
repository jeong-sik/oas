(** Tool use recovery — extract ToolUse blocks from Text content when a
    provider emits tool-call intent as text instead of a proper ToolUse
    block.

    Delegates JSON normalization to {!Llm_provider.Lenient_json}.

    @since 0.136.0 *)

open Types

(** Find the first balanced top-level JSON object in a string,
    respecting string literal escapes. Returns [Some (start, length)]
    or [None]. Exposed for testing. *)
val find_json_object : string -> (int * int) option

(** Try to parse a JSON object from a string. Strips markdown fences,
    locates the first balanced object, and runs {!Lenient_json.parse}
    for transform-based recovery. Returns [None] if no JSON found. *)
val try_parse_json_object : string -> Yojson.Safe.t option

(** Match a JSON value against known tool-call shapes and extract
    [(name, input)]. Handles Anthropic-style [{name, input}], OpenAI
    [{name, arguments}] (including double-stringified [arguments]),
    OpenAI [{tool_calls: [...]}] wrapper, and bare [{function: ...}]. *)
val extract_name_and_input : Yojson.Safe.t -> (string * Yojson.Safe.t) option

(** Scan content blocks and replace recoverable Text blocks with
    ToolUse. Returns [(new_content, recovered_count)]. *)
val recover_content_blocks
  :  valid_tool_names:string list
  -> content_block list
  -> content_block list * int

(** Top-level recovery. Promotes Text-embedded tool calls to ToolUse
    blocks only when:
    - [valid_tool_names] is non-empty,
    - the response has no ToolUse blocks,
    - at least one Text block matches a recognized tool-call shape
      whose [name] is in [valid_tool_names].

    Returns the response unchanged otherwise. Adjusts [stop_reason]
    from [EndTurn] to [StopToolUse] on recovery. *)
val recover_response : valid_tool_names:string list -> api_response -> api_response
