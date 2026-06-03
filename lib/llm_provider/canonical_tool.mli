(** Canonical tool-result projection (RFC-OAS-024, WP8 — Increment 1).

    A typed read-projection of the tool-{e result} dimension of
    {!Types.content_block}. It is {b not} a second in-memory SSOT:
    [content_block] remains the canonical representation and every value here is
    derived from it at the provider boundary.

    Scope (RFC-OAS-024 §7): Increment 1 ships only [tool_result_of_block], which
    is wired into the live turn pipeline. The call projection
    ([provider_tool_call] / [tool_calls_of_response]) and the reasoning-link
    types are deferred to Increment 2, where a per-provider parse path consumes
    them — shipping them ahead of a consumer would be a fan-in==0 surface. The
    Keystone (K: [id_origin]) and module-merge (M) sign-offs in RFC-OAS-024 §0
    gate that call-projection increment, not this result-only slice.

    Boundary (RFC-OAS-024 §1): OAS-owned provider canonicalization only. Depends
    solely on [{Types; Yojson}] and references no execution, policy, or
    coordinator concept. The downstream consumer is named only as an unnamed
    external role.

    @stability Evolving *)

(** A single tool result projected at the provider boundary. Lane A: [call_id]
    is the originating tool-use id verbatim (native or synthesized upstream),
    never re-synthesized here. *)
type provider_tool_result =
  { call_id : string (** Correlates with {!provider_tool_call.call_id} (Increment 2). *)
  ; content : string (** Canonical string payload (mirror of [ToolResult.content]). *)
  ; content_blocks : Types.content_block list option
    (** Mirror of [ToolResult.content_blocks] (multi-block result). *)
  ; structured_content : Yojson.Safe.t option
    (** Projection of [ToolResult.json] (WP4 parsed payload), verbatim. Not a
          fresh parse, and {b not} [provider_config.output_schema] which is a
          request-level concern (RFC-OAS-024 D7). *)
  ; is_error : bool
  }

(** Project a single content block into a tool result. Returns [None] for any
    block that is not a [ToolResult]. Pure and total. *)
val tool_result_of_block : Types.content_block -> provider_tool_result option
