(** Canonical tool-result projection (RFC-OAS-024, WP8 Increment 1).

    Increment 1 ships only the {e result} projection — the one piece wired into
    a live consumer (the turn pipeline, see [Pipeline_stage_prepare]). The call
    projection ([provider_tool_call] / [tool_calls_of_response]) and the
    reasoning-link types are deferred to Increment 2, where a per-provider parse
    path will consume them; shipping them now would be a fan-in==0 surface
    (RFC-OAS-024 §7).

    Pure, total function over [Types.content_block]. It is {b not} a second
    in-memory SSOT: [content_block] remains the canonical representation and the
    projected value is derived from it at the provider boundary. Depends only on
    [{Types; Yojson}] and references no execution, policy, or coordinator
    concept (RFC-OAS-024 §1).

    Lane A (Keystone K): no [id_origin]. [call_id] is the block's id verbatim,
    already the native wire id or a synthesized id depending on the provider
    parse path; ids are neither re-synthesized nor re-classified here. *)

type provider_tool_result =
  { call_id : string (** Correlates with the originating tool call. *)
  ; content : string (** Canonical string payload (mirror of [ToolResult.content]). *)
  ; content_blocks : Types.content_block list option
    (** Mirror of [ToolResult.content_blocks] (multi-block result). *)
  ; structured_content : Yojson.Safe.t option
    (** Projection of [ToolResult.json] (WP4 parsed payload), verbatim — not a
        fresh parse, and not [provider_config.output_schema] (RFC-OAS-024 D7). *)
  ; is_error : bool
  }

let tool_result_of_block (block : Types.content_block) : provider_tool_result option =
  match block with
  | Types.ToolResult { tool_use_id; content; is_error; json; content_blocks } ->
    Some
      { call_id = tool_use_id
      ; content
      ; content_blocks
      ; structured_content = json
      ; is_error
      }
  | Types.Text _
  | Types.Thinking _
  | Types.RedactedThinking _
  | Types.ToolUse _
  | Types.Image _
  | Types.Document _
  | Types.Audio _ -> None
;;
