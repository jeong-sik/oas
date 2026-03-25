(** Extensible provider registry with capability-aware queries.

    Formalizes the hardcoded provider list from {!Cascade_config} into
    a mutable registry. Providers can be registered at startup and
    queried by name or capability predicate.

    @since 0.69.0 *)

(** Provider defaults: how to connect to a provider. *)
type provider_defaults = {
  kind: Provider_config.provider_kind;
  base_url: string;
  api_key_env: string;
  request_path: string;
}

(** A registered provider entry.
    [max_context] is the default context window size in tokens.
    @since 0.78.0 max_context added *)
type entry = {
  name: string;
  defaults: provider_defaults;
  max_context: int;
  capabilities: Capabilities.capabilities;
  is_available: unit -> bool;
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

(** Default registry pre-populated with known providers
    (llama, claude, gemini, glm, openrouter).
    Availability is determined by checking the API key env var. *)
val default : unit -> t

(** Initial LLM_ENDPOINTS URLs parsed from the environment at module load.
    For current active endpoints, use [active_llama_endpoints]. *)
val llama_all_endpoints : string list

(** Pick the next llama endpoint via round-robin.
    Distributes load transparently when multiple local servers are running.
    After [refresh_llama_endpoints], rotates across discovered endpoints.
    @since 0.78.0 *)
val next_llama_endpoint : unit -> string

(** Refresh the llama endpoint list by scanning local ports 8085-8090.
    If [LLM_ENDPOINTS] env var is set, uses that as source (no scan).
    Otherwise probes ports and keeps only healthy endpoints.
    Returns the new endpoint list. Call after Eio scheduler is available.
    @since 0.86.0 *)
val refresh_llama_endpoints :
  sw:Eio.Switch.t ->
  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
  unit -> string list

(** Current active endpoint list (snapshot after last refresh).
    @since 0.86.0 *)
val active_llama_endpoints : unit -> string list
