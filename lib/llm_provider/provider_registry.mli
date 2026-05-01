(** Extensible provider registry with capability-aware queries.

    Formalizes the historical hardcoded provider list into a mutable
    registry. Providers can be registered at startup and queried by
    name or capability predicate.

    @since 0.69.0

    @stability Internal
    @since 0.93.1 *)

(** Provider defaults: how to connect to a provider. *)
type provider_defaults =
  { kind : Provider_config.provider_kind
  ; base_url : string
  ; api_key_env : string
  ; request_path : string
  }

(** A registered provider entry.
    [max_context] is the default context window size in tokens.
    @since 0.78.0 max_context added *)
type entry =
  { name : string
  ; defaults : provider_defaults
  ; max_context : int
  ; capabilities : Capabilities.capabilities
  ; is_available : unit -> bool
  }

(** Mutable provider registry. *)
type t

(** Create an empty registry. *)
val create : unit -> t

(** Register a provider. Overwrites if name already exists. *)
val register : t -> entry -> unit

(** Remove a provider by name. No-op if not found. *)
val unregister : t -> string -> unit

(** Look up a provider by name. *)
val find : t -> string -> entry option

(** All registered providers. *)
val all : t -> entry list

(** Providers where [is_available()] returns [true]. *)
val available : t -> entry list

(** Providers whose capabilities satisfy the given predicate. *)
val find_capable : t -> (Capabilities.capabilities -> bool) -> entry list

(** Check whether a command is discoverable from PATH without shelling out. *)
val command_in_path : ?path:string -> string -> bool

(** Default registry pre-populated with known direct providers plus
    non-interactive CLI transports ([claude_code], [gemini_cli],
    [kimi], [kimi_cli], [codex_cli], and compat alias [cc]).
    Availability is determined by API-key env vars for direct providers
    and PATH discovery for CLI transports. *)
val default : unit -> t

(** Best-effort canonical provider name for a concrete provider config.
    Unlike [Provider_config.string_of_provider_kind], this keeps
    registry-level distinctions that share a wire kind but differ by
    endpoint (for example [glm] vs [glm-coding], or [openai] vs
    [openrouter]). Falls back to a stable kind-derived label when the
    config does not match a known registry entry exactly. *)
val provider_name_of_config : Provider_config.t -> string

(** Initial LLM_ENDPOINTS URLs parsed from the environment at module load.
    For current active endpoints, use [active_llama_endpoints]. *)
val llama_all_endpoints : string list

(** Pick the next llama endpoint via round-robin.
    Distributes load transparently when multiple local servers are running.
    After [refresh_llama_endpoints], rotates across discovered endpoints.
    @since 0.78.0 *)
val next_llama_endpoint : unit -> string

(** Peek at the current llama endpoint without advancing the round-robin.
    Returns the endpoint that [next_llama_endpoint] will return on its
    next call, but without the [fetch_and_add] side effect.
    Returns [""] when no endpoints are configured.
    @since 0.100.8 *)
val current_llama_endpoint : unit -> string

(** Refresh the llama endpoint list by scanning local ports 8085-8090.
    If [LLM_ENDPOINTS] env var is set, uses that as source (no scan).
    Otherwise probes ports and keeps only healthy endpoints.
    Returns the new endpoint list. Call after Eio scheduler is available.
    @since 0.86.0 *)
val refresh_llama_endpoints
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> unit
  -> string list

(** Current active endpoint list (snapshot after last refresh).
    @since 0.86.0 *)
val active_llama_endpoints : unit -> string list

(** Per-slot context tokens from the last discovery probe.
    Returns [None] if no probe has completed yet.
    Delegates to {!Discovery.discovered_per_slot_context}.
    @since 0.100.8 *)
val discovered_max_context : unit -> int option

(** Per-slot context for a specific endpoint URL.
    Delegates to {!Discovery.discovered_context_for_url}.
    Returns [None] when no valid context has been discovered for this URL
    (either not probed or probed without reporting valid context properties).
    @since 0.100.8 *)
val discovered_endpoint_max_context : string -> int option
