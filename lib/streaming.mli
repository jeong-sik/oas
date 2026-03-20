(** SSE streaming client for multi-provider LLM APIs.

    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Pure SSE event parsing and synthetic emission are delegated to
    {!Llm_provider.Streaming}. *)

(** {1 Re-exported helpers} *)

val parse_sse_event : string option -> string -> Types.sse_event option
val emit_synthetic_events : Types.api_response -> (Types.sse_event -> unit) -> unit

(** {1 Stream accumulation} *)

(** Mutable accumulator for assembling a complete [api_response] from
    incremental SSE events. *)
type stream_acc = {
  msg_id: string ref;
  msg_model: string ref;
  input_tokens: int ref;
  output_tokens: int ref;
  cache_creation: int ref;
  cache_read: int ref;
  stop_reason: Types.stop_reason ref;
  block_texts: (int, Buffer.t) Hashtbl.t;
  block_types: (int, string) Hashtbl.t;
  block_tool_ids: (int, string) Hashtbl.t;
  block_tool_names: (int, string) Hashtbl.t;
}

val create_stream_acc : unit -> stream_acc
val accumulate_event : stream_acc -> Types.sse_event -> unit
val finalize_stream_acc : stream_acc -> Types.api_response

(** {1 Error mapping} *)

val map_http_error : Llm_provider.Http_client.http_error -> Error.sdk_error

(** {1 Streaming API call} *)

(** Streaming variant of {!Api.create_message}.

    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Custom providers fall back to sync + synthetic events. *)
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
