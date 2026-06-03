(** Canonical tool-call projection (RFC-OAS-024, WP8 — lane A).

    A typed, closed {e read projection} of the tool-call / tool-result
    dimension of {!Types.content_block}. It is {b not} a second in-memory
    SSOT: [content_block] remains the canonical representation and every
    value here is derived from it at the provider boundary.

    Boundary (RFC-OAS-024 §1): this module is OAS-owned provider
    canonicalization only. It depends solely on [{Types; Provider_kind;
    Yojson}] and references no execution, policy, or coordinator concept.
    MASC appears only as a named external consumer in prose.

    Lane A (Keystone K, RESOLVED): there is no [id_origin] field or type.
    A coordinator deduplicates on [call_id] equality regardless of whether
    the id was a native wire id or synthesized via
    {!Api_common.synthesize_tool_use_id}; carrying provenance would gate no
    behaviour, so it is omitted.

    @stability Evolving
    @since 0.200.11 *)

(** Kind of reasoning attached to a tool call. Closed variant — no string
    carrier. Adding a provider reasoning shape forces a compile error at
    every [match]. *)
type reasoning_kind =
  | Thinking (** Anthropic [thinking] block. *)
  | Redacted_thinking (** Anthropic [redacted_thinking] block. *)
  | Reasoning_content (** OpenAI-compat / GLM / DeepSeek [reasoning_content] field. *)

type reasoning_state =
  { kind : reasoning_kind
  ; content : string
  ; tokens : int option (** Reasoning tokens, when the provider reports them. *)
  }

(** Per-call reasoning link. Three-way (RFC-OAS-024 D6) so that "the model
    emitted no reasoning" is not collapsed with "reasoning was disabled by
    request config" — [option] is forbidden here because that distinction is
    the whole requirement. *)
type reasoning_link =
  | No_reasoning (** Provider supports reasoning but emitted none for this call. *)
  | Suppressed (** Disabled by request config (e.g. [enable_thinking = false]). *)
  | Available of reasoning_state

(** A single tool call projected at the provider boundary. *)
type provider_tool_call =
  { call_id : string
    (** Id a coordinator uses to correlate result with call. Native wire id
          when the provider supplies one, otherwise the output of
          {!Api_common.synthesize_tool_use_id} (with the [_idx] suffix for the
          Ollama synthesized fallback). *)
  ; provider_kind : Provider_kind.t
    (** Provider that emitted this call. Threaded at projection time
          (RFC-OAS-024 D5); never re-derived from optional telemetry. The type
          identifies a {e family} ([OpenAI_compat]), not a vendor. *)
  ; name : string
    (** Tool name. Stays [string] at the RFC-OAS-008 boundary; not migrated
          to a variant here. *)
  ; arguments : Yojson.Safe.t
  ; order_index : int
    (** Appearance order among tool calls in the same response: the index
          after filtering [content] to [ToolUse] blocks (RFC-OAS-024 D3), not
          the all-block stream counter. Stable across stream reconstruction. *)
  ; reasoning : reasoning_link
  }

(** A single tool result projected at the provider boundary. *)
type provider_tool_result =
  { call_id : string (** Correlates with {!provider_tool_call.call_id}. *)
  ; content : string (** Canonical string payload (mirror of [ToolResult.content]). *)
  ; content_blocks : Types.content_block list option
    (** Mirror of [ToolResult.content_blocks] (multi-block result). *)
  ; structured_content : Yojson.Safe.t option
    (** Projection of [ToolResult.json] (WP4 parsed payload). Not a fresh
          parse, and {b not} [provider_config.output_schema] which is a
          request-level concern (RFC-OAS-024 D7). *)
  ; is_error : bool
  }

(** Project a single content block into a tool result. Returns [None] for any
    block that is not a [ToolResult]. Pure and total. *)
val tool_result_of_block : Types.content_block -> provider_tool_result option

(** Project the tool calls of a response, in appearance order, tagging each
    with [provider_kind] and a [reasoning_link]. Pure and total.

    [reasoning_suppressed] is the request-config signal (e.g.
    [enable_thinking = Some false]) used to choose {!Suppressed} over
    {!No_reasoning} when a call carries no reasoning.

    Increment 1 wires {!tool_result_of_block} into the turn pipeline; this
    function is unit-tested here and wired into per-provider parse paths in a
    later increment (RFC-OAS-024 §7 Increment 2+). *)
val tool_calls_of_response
  :  provider_kind:Provider_kind.t
  -> reasoning_suppressed:bool
  -> Types.api_response
  -> provider_tool_call list
