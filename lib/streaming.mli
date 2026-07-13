(** SSE streaming client for multi-provider LLM APIs.

    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Pure SSE event parsing and synthetic emission are delegated to
    {!Llm_provider.Streaming}.  The HTTP streaming client remains here
    due to agent_state/Provider/Error coupling.

    @stability Evolving
    @since 0.93.1 *)

(** {1 Re-exported Pure Functions} *)

(** Parse a raw SSE event into a typed {!Types.sse_event}. *)
val parse_sse_event : string option -> string -> Types.sse_event option

(** Emit synthetic SSE events from a completed API response. *)
val emit_synthetic_events : Types.api_response -> (Types.sse_event -> unit) -> unit

(** {1 Stream Accumulation} *)

(** Canonical streaming accumulator surface. *)
include module type of Llm_provider.Complete_stream_acc

(** {1 HTTP Error Mapping} *)

(** Map an HTTP client error to an {!Error.sdk_error}. *)
val map_http_error
  :  ?accept_rejected:Http_error_sdk.accept_rejected
  -> Llm_provider.Http_client.http_error
  -> Error.sdk_error

(** {1 Streaming API Call} *)

(** Create a streaming LLM message for an explicitly selected provider.
    Anthropic and OpenAI-compatible SSE codecs are implemented. A custom
    provider without a streaming codec returns [UnsupportedProvider]; a
    completed synchronous response is never presented as a live stream.

    Performs exactly one provider stream attempt. Partial events and a terminal
    typed failure are returned unchanged; any later attempt belongs to a new
    caller-owned stream. *)
val create_message_stream_detailed
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?idle_timeout:float
  -> provider:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> on_event:(Types.sse_event -> unit)
  -> unit
  -> (Types.api_response, Provider_failure_attribution.detailed_error) result

val create_message_stream
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?clock:_ Eio.Time.clock
  -> ?idle_timeout:float
  -> provider:Provider.config
  -> config:Types.agent_state
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> on_event:(Types.sse_event -> unit)
  -> unit
  -> (Types.api_response, Error.sdk_error) result
