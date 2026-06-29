(** LLM endpoint discovery -- probes local llama-server instances.

    Queries OpenAI-compatible endpoints (/v1/models, /props, /slots, /health)
    and returns typed status. Consumers can use this to discover
    available LLM capacity.

    @since 0.53.0

    @stability Internal
    @since 0.93.1 *)

(** Model info from /v1/models — re-exported from {!Discovery_parse}
    so existing [Discovery.model_info] consumers do not need to switch
    imports. *)
type model_info = Discovery_parse.model_info =
  { id : string
  ; owned_by : string
  }

(** Server properties from /props — re-exported from
    {!Discovery_parse}. *)
type server_props = Discovery_parse.server_props =
  { total_slots : int
  ; ctx_size : int
  ; model : string
  ; supports_tools : bool option
  }

(** Slot utilization from /slots — re-exported from
    {!Discovery_parse}. *)
type slot_status = Discovery_parse.slot_status =
  { total : int
  ; busy : int
  ; idle : int
  }

(** Full status of a single endpoint *)
type endpoint_status =
  { url : string
  ; healthy : bool
  ; models : model_info list
  ; props : server_props option
  ; slots : slot_status option
  ; capabilities : Capabilities.capabilities
  }

(** Probe a list of endpoint URLs and return their current status.
    Non-reachable endpoints are returned with [healthy = false].
    Does not raise; all errors are captured in the returned records. *)
val discover
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> endpoints:string list
  -> endpoint_status list

(** Environment variable consulted by {!resolve_default_endpoint}. *)
val local_llm_url_env_var : string

(** Environment variable parsed by {!parse_llm_endpoints_env}. *)
val llm_endpoints_env_var : string

(** Environment variable consulted by {!resolve_ollama_endpoint}. *)
val ollama_host_env_var : string

(** Compile-time fallback for {!resolve_ollama_endpoint}. *)
val default_ollama_endpoint : string

(** Canonical local LLM endpoint fallback.
    This is intentionally not an environment snapshot. Use
    {!resolve_default_endpoint} when runtime configuration should read
    [OAS_LOCAL_LLM_URL]. *)
val default_endpoint : string

(** Call-time resolver for the canonical local LLM endpoint.
    Reads [OAS_LOCAL_LLM_URL] at call time, falling back to
    {!Constants.Endpoints.default_url}. Prefer this over {!default_endpoint}
    when the value must reflect environment changes after module init. [getenv]
    defaults to {!Cli_common_env.get}; tests may inject it to avoid mutating the
    process environment. *)
val resolve_default_endpoint : ?getenv:(string -> string option) -> unit -> string

(** Ollama endpoint fallback. This is intentionally not an environment
    snapshot. Use {!resolve_ollama_endpoint} when runtime configuration should
    read [OLLAMA_HOST]. *)
val ollama_endpoint : string

(** Call-time resolver for the Ollama endpoint.
    Reads [OLLAMA_HOST] at call time. Prefer this over {!ollama_endpoint}
    when the value must reflect environment changes after module init. [getenv]
    defaults to {!Cli_common_env.get}; tests may inject it to avoid mutating the
    process environment. *)
val resolve_ollama_endpoint : ?getenv:(string -> string option) -> unit -> string

(** Parse the [LLM_ENDPOINTS] env var as a comma-separated list of
    URLs, trimming whitespace and dropping empty entries.  Returns
    [[]] when the variable is unset, empty, or contains only empty
    entries after trimming.

    Single source of truth for [LLM_ENDPOINTS] parsing — consumers
    needing defaulting (e.g. fall back to {!default_endpoint}) or the
    Ollama append (see {!endpoints_from_env}) should layer their
    semantics on top of this raw list.

    [getenv] defaults to {!Cli_common_env.get}; tests may inject it to avoid
    mutating the process environment.

    @since 0.161.0 *)
val parse_llm_endpoints_env : ?getenv:(string -> string option) -> unit -> string list

(** Parse LLM_ENDPOINTS env var (comma-separated) and append the call-time
    Ollama endpoint if not already included.
    Falls back to [[resolve_default_endpoint (); resolve_ollama_endpoint ()]]. *)
val endpoints_from_env : ?getenv:(string -> string option) -> unit -> string list

(** JSON serialization for endpoint_status. *)
val endpoint_status_to_json : endpoint_status -> Yojson.Safe.t

(** Aggregate summary across multiple endpoints. *)
val summary_to_json : endpoint_status list -> Yojson.Safe.t

(** Extract max context size from endpoint status.
    Returns [ctx_size] from [props] if available, else checks [capabilities.max_context_tokens]. *)
val max_context_of_status : endpoint_status -> int option

(** Scan local ports for healthy llama-server instances.
    Probes each port via [/health] and returns URLs of healthy endpoints.
    Default port range: 8085-8090.
    @since 0.86.0 *)

(** Default ports to scan for local llama-server instances. *)
val default_scan_ports : unit -> int list

val scan_local_endpoints
  :  ?ports:int list
  -> sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> unit
  -> string list

(** {1 Discovered Context State}

    Shared state updated by probing.  Consumers read
    [discovered_per_slot_context] to get the effective per-request
    context limit derived from the last successful probe.

    Per-endpoint tracking: [discovered_endpoint_contexts] and
    [discovered_context_for_url] provide URL-specific context sizes.

    @since 0.100.8 *)

(** Max per-slot context tokens across all healthy endpoints.
    Computed as [ctx_size / total_slots] for each endpoint, then
    takes the max.  Returns [None] if no probe completed or the
    latest probe yielded no valid slot/context data. *)
val discovered_per_slot_context : unit -> int option

(** Per-endpoint per-slot context from last probe.
    Returns [(url, per_slot_ctx)] for each healthy endpoint
    that reported valid slot/context info. *)
val discovered_endpoint_contexts : unit -> (string * int) list

(** Look up per-slot context for a specific endpoint URL.
    URL is trimmed before lookup.  Returns [None] if not found. *)
val discovered_context_for_url : string -> int option

(** Look up the endpoint URL that has [model_id] loaded.
    Uses the model-to-endpoint index from the last {!refresh_and_sync}.
    Returns [None] if the model was not found on any probed endpoint. *)
val endpoint_for_model : string -> string option

(** Return the first model_id from the discovered model_endpoints index.
    Useful for resolving "auto" model IDs to concrete names discovered
    from the server's /v1/models endpoint.  Returns [None] if no models
    have been discovered yet. *)
val first_discovered_model_id : unit -> string option

(** Return the first model_id discovered on a specific endpoint [url].
    Prevents cross-provider contamination when resolving "auto" model IDs:
    e.g. [ollama:auto] resolves only from the Ollama endpoint, not from
    llama-server models that happen to appear first in the global index. *)
val first_discovered_model_id_for_url : string -> string option

(** Look up [model_id] and return [(url, per_slot_ctx)].
    Combines the model-to-endpoint index with per-endpoint context data.
    Returns [None] if the model is not found or its endpoint has no
    context data in the current snapshot. *)
val context_for_model : string -> (string * int) option

(** Probe [endpoints], update the shared per-slot context state,
    and return the full status list.  Replaces [discover] when
    the caller also wants the context state kept current. *)
val refresh_and_sync
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> endpoints:string list
  -> endpoint_status list
