(** SSE streaming client for multi-provider LLM APIs.

    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Pure SSE event parsing and synthetic emission are delegated to
    {!Llm_provider.Streaming}.  The HTTP streaming client remains here
    due to agent_state/Provider/Error coupling.

    @stability Evolving
    @since 0.93.0 *)

(** {1 Re-exported Pure Functions} *)

(** Parse a raw SSE event into a typed {!Types.sse_event}. *)
val parse_sse_event : string option -> string -> Types.sse_event option

(** Emit synthetic SSE events from a completed API response. *)
val emit_synthetic_events : Types.api_response -> (Types.sse_event -> unit) -> unit

(** {1 Stream Accumulation} *)

(** Mutable accumulator for building an {!Types.api_response} from
    a sequence of SSE events. *)
type stream_acc = {
  msg_id: string ref;
  msg_model: string ref;
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

(** Create a fresh accumulator. *)
val create_stream_acc : unit -> stream_acc

(** Feed a single SSE event into the accumulator. *)
val accumulate_event : stream_acc -> Types.sse_event -> unit

(** Finalize the accumulator into a complete API response.
    Returns [Error msg] if an SSE error was recorded during the stream. *)
val finalize_stream_acc : stream_acc -> (Types.api_response, string) result

(** {1 HTTP Error Mapping} *)

(** Map an HTTP client error to an {!Error.sdk_error}. *)
val map_http_error : Llm_provider.Http_client.http_error -> Error.sdk_error

(** {1 Streaming API Call} *)

(** Create a streaming LLM message.
    Supports Anthropic (native SSE), OpenAI-compatible (SSE),
    and custom providers (sync fallback with synthetic events).

    Does not accept [retry_config]: SSE streams deliver partial results
    incrementally; retrying mid-stream would discard data. *)
val create_message_stream :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  ?base_url:string ->
  ?provider:Provider.config ->
  config:Types.agent_state ->
  messages:Types.message list ->
  ?tools:Yojson.Safe.t list ->
  on_event:(Types.sse_event -> unit) ->
  unit ->
  (Types.api_response, Error.sdk_error) result
