(** SSE event parsing for Anthropic and OpenAI streaming APIs.

    Pure functions — no I/O or agent_sdk coupling.

    @stability Internal
    @since 0.93.0 *)

open Types

(** {1 Anthropic SSE} *)

val parse_sse_event : string option -> string -> sse_event option
val emit_synthetic_events : api_response -> (sse_event -> unit) -> unit

(** {1 OpenAI SSE} *)

type openai_tool_call_delta = {
  tc_index: int;
  tc_id: string option;
  tc_name: string option;
  tc_arguments: string option;
}

type openai_chunk = {
  chunk_id: string;
  chunk_model: string;
  delta_content: string option;
  delta_reasoning: string option;
  delta_tool_calls: openai_tool_call_delta list;
  finish_reason: string option;
  chunk_usage: api_usage option;
}

type openai_stream_state = {
  mutable thinking_block_started: bool;
  mutable thinking_block_index: int;
  mutable text_block_started: bool;
  mutable text_block_index: int;
  tool_block_indices: (int, int) Hashtbl.t;
  mutable next_block_index: int;
}

val parse_openai_sse_chunk : string -> openai_chunk option
val create_openai_stream_state : unit -> openai_stream_state
val openai_chunk_to_events : openai_stream_state -> openai_chunk -> sse_event list

(** {1 Gemini SSE}

    Gemini [streamGenerateContent?alt=sse] emits SSE chunks with
    [{candidates: [{content: {parts: [...]}}]}] structure per chunk.
    We reuse {!openai_stream_state} for block tracking since the
    state management pattern is identical. *)

type gemini_chunk = {
  gem_model: string;
  gem_parts: Yojson.Safe.t list;
  gem_finish_reason: string option;
  gem_usage: api_usage option;
}

val parse_gemini_sse_chunk : string -> gemini_chunk option
val gemini_chunk_to_events : openai_stream_state -> gemini_chunk -> sse_event list
