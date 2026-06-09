(** Stream accumulator: gather SSE events into a {!Types.api_response}.

    Depends only on [Types] -- no provider/backend/transport references.

    @since 0.79.0

    @stability Internal
    @since 0.93.1 *)

(** Mutable accumulator that collects SSE stream events into content blocks.
    Use {!create_stream_acc} to create, {!accumulate_event} to feed events,
    and {!finalize_stream_acc} to produce the final response. *)
type stream_acc =
  { id : string ref
  ; model : string ref
  ; input_tokens : int ref
  ; output_tokens : int ref
  ; cache_creation : int ref
  ; cache_read : int ref
  ; stop_reason : Types.stop_reason ref
  ; sse_error : Types.stream_error option ref
  ; saw_terminal : bool ref
    (** Set once a provider end-of-response signal is seen ([MessageStop] or a
        [MessageDelta] with [stop_reason = Some _]). [false] at finalize means
        the stream closed mid-response (truncation / phantom completion). *)
  ; block_texts : (int, Buffer.t) Hashtbl.t
  ; block_types : (int, string) Hashtbl.t
  ; block_tool_ids : (int, string) Hashtbl.t
  ; block_tool_names : (int, string) Hashtbl.t
  }

(** Create a fresh accumulator with empty defaults. *)
val create_stream_acc : unit -> stream_acc

(** Feed a single SSE event into the accumulator.
    Updates id, model, tokens, content blocks in-place. *)
val accumulate_event : stream_acc -> Types.sse_event -> unit

(** [true] once the stream observed a provider end-of-response signal
    ([MessageStop] or a [MessageDelta] with [stop_reason = Some _]). The consumer
    uses this to reject a socket that closed mid-stream as a truncated
    completion instead of presenting a phantom [Ok]. *)
val saw_terminal : stream_acc -> bool

(** Produce the final {!Types.api_response} from the accumulated state.
    Returns [Error stream_error] if an SSE error was recorded during the stream
    (typed so the consumer can route a provider-reported error through the same
    classification path as a non-streaming error); content blocks are ordered by
    their stream index. *)
val finalize_stream_acc : stream_acc -> (Types.api_response, Types.stream_error) result
