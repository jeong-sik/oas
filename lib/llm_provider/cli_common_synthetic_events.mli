(** Replay a completed [api_response] as a sequence of SSE events.

    Used by CLI transports whose underlying binary does not support
    native streaming (Gemini CLI, Codex CLI) so consumers receive the
    same event shape as a truly streamed response. *)

(** Emit [MessageStart] → per-block [ContentBlockStart]/[ContentBlockDelta]/
    [ContentBlockStop] → [MessageDelta] → [MessageStop], in order. *)
val replay : on_event:(Types.sse_event -> unit) -> Types.api_response -> unit
