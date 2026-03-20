(** LLM endpoint discovery -- probes local llama-server instances.

    Queries OpenAI-compatible endpoints (/v1/models, /props, /slots, /health)
    and returns typed status. Both OAS and MASC can use this to discover
    available LLM capacity.

    @since 0.53.0 *)

(** Model info from /v1/models *)
type model_info = {
  id: string;
  owned_by: string;
}

(** Server properties from /props *)
type server_props = {
  total_slots: int;
  ctx_size: int;
  model: string;
}

(** Slot utilization from /slots *)
type slot_status = {
  total: int;
  busy: int;
  idle: int;
}

(** Full status of a single endpoint *)
type endpoint_status = {
  url: string;
  healthy: bool;
  models: model_info list;
  props: server_props option;
  slots: slot_status option;
  capabilities: Capabilities.capabilities;
}

(** Probe a list of endpoint URLs and return their current status.
    Non-reachable endpoints are returned with [healthy = false].
    Does not raise; all errors are captured in the returned records. *)
val discover :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  endpoints:string list ->
  endpoint_status list

(** Parse LLM_ENDPOINTS env var (comma-separated) or return default
    [["http://127.0.0.1:8085"]]. *)
val endpoints_from_env : unit -> string list

(** JSON serialization for endpoint_status. *)
val endpoint_status_to_json : endpoint_status -> Yojson.Safe.t

(** Aggregate summary across multiple endpoints. *)
val summary_to_json : endpoint_status list -> Yojson.Safe.t

(** Extract max context size from endpoint status.
    Returns [ctx_size] from [props] if available, else checks [capabilities.max_context_tokens]. *)
val max_context_of_status : endpoint_status -> int option
