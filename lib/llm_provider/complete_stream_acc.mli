(** Stream accumulator: gather SSE events into a {!Types.api_response}.

    Depends only on [Types] -- no provider/backend/transport references.

    @since 0.79.0

    @stability Internal
    @since 0.93.0 *)

(** Mutable accumulator that collects SSE stream events into content blocks.
    Use {!create_stream_acc} to create, {!accumulate_event} to feed events,
    and {!finalize_stream_acc} to produce the final response. *)
type stream_acc = {
  id: string ref;
  model: string ref;
  input_tokens: int ref;
  output_tokens: int ref;
  cache_creation: int ref;
  cache_read: int ref;
  stop_reason: Types.stop_reason ref;
  sse_error: string option ref;
  block_texts: (int, Buffer.t) Hashtbl.t;
  block_types: (int, string) Hashtbl.t;
  block_tool_ids: (int, string) Hashtbl.t;
  block_tool_names: (int, string) Hashtbl.t;
}

(** Create a fresh accumulator with empty defaults. *)
val create_stream_acc : unit -> stream_acc

(** Feed a single SSE event into the accumulator.
    Updates id, model, tokens, content blocks in-place. *)
val accumulate_event : stream_acc -> Types.sse_event -> unit

(** Produce the final {!Types.api_response} from the accumulated state.
    Returns [Error msg] if an SSE error was recorded during the stream;
    content blocks are ordered by their stream index. *)
val finalize_stream_acc : stream_acc -> (Types.api_response, string) result
