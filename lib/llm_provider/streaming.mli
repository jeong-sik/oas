(** SSE event parsing for Anthropic and OpenAI streaming APIs.

    Pure functions — no I/O or agent_sdk coupling.

    @stability Internal
    @since 0.93.1 *)

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

(** {1 Ollama NDJSON Streaming}

    Ollama [/api/chat] with [stream:true] emits one JSON object per
    line (newline-delimited JSON, NDJSON). The final line carries
    [done:true] together with [done_reason] and the
    [prompt_eval_count] / [prompt_eval_duration] /
    [eval_count] / [eval_duration] timing fields that the OpenAI
    compat path on [/v1/chat/completions] strips out.

    State management reuses {!openai_stream_state} since the block
    tracking pattern is identical. Tool calls in Ollama typically
    arrive fully-formed in the [done:true] line, so the streaming
    consumer treats them as a single delta rather than incremental.

    @since 0.171.0 *)

type ollama_tool_call_delta = {
  oll_tc_index: int;
  oll_tc_id: string option;
  oll_tc_name: string option;
  oll_tc_arguments: string option;  (** JSON string of arguments. *)
}

type ollama_chunk = {
  oll_model: string;
  oll_delta_content: string option;
  oll_delta_thinking: string option;
  oll_tool_calls: ollama_tool_call_delta list;
  oll_done_reason: string option;
  oll_is_done: bool;
  oll_usage: api_usage option;
  oll_timings: inference_timings option;
}

(** Parse one NDJSON line into an {!ollama_chunk}. Returns [None]
    when the line is not valid JSON or is missing the expected
    [message]/[done] keys. *)
val parse_ollama_ndjson_chunk : string -> ollama_chunk option

(** Convert a parsed {!ollama_chunk} into {!sse_event} list.
    Synthesises [ContentBlockStart] events on first occurrence of
    text / thinking content and on each new tool_call. The terminal
    [done:true] chunk also emits [MessageDelta] carrying the
    stop_reason and any token-count usage. *)
val ollama_chunk_to_events :
  openai_stream_state -> ollama_chunk -> sse_event list
