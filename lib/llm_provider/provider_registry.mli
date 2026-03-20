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

(** Default registry pre-populated with known providers
    (llama, claude, gemini, glm, openrouter).
    Availability is determined by checking the API key env var. *)
val default : unit -> t
