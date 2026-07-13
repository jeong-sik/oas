(** Read-only runtime provider bindings.

    This module exposes the provider catalog / registry facts that embedding
    applications need without exposing registry mutation. It is intentionally
    coordinator-neutral: downstream applications may project local policy over
    these bindings, but OAS owns provider/model identity and capability truth.

    @since 0.194.0 *)

type provider_kind = Llm_provider.Provider_config.provider_kind
type capabilities = Provider.capabilities

type transport =
  | Http
  | Managed

type auth =
  | No_auth
  | Api_key_env of string
  | Oauth_cached_login
  | Setup_token_env of string

type t =
  { id : string
  ; aliases : string list
  ; kind : provider_kind
  ; transport : transport
  ; command : string option
  ; base_url : string
  ; request_path : string
  ; api_key_env : string
  ; auth : auth
  ; default_model : string option
  ; max_context : int option
  ; capabilities : capabilities
  ; available : bool
  ; credential_scope : string option
  }

(** Return all known runtime bindings from the default provider registry,
    including any process/env provider catalog overlay. *)
val all : unit -> t list

(** Find a runtime binding by provider id or alias. Lookup is
    case-insensitive and whitespace-trimmed. *)
val find : string -> t option

(** Find a runtime binding only from the process/env provider catalog overlay.
    Built-in registry providers are intentionally excluded. *)
val find_catalog : string -> t option

(** Return all known binding ids and selector aliases. This is a display /
    diagnostics surface; callers should use {!find} for resolution. *)
val known_labels : unit -> string list

(** Resolve the runtime binding that owns a concrete provider config.

    Catalog endpoint matches are resolved before registry provider-name
    fallbacks, so catalog-provided OpenAI-compatible providers remain
    OAS-owned even when the endpoint is local. *)
val binding_for_provider_config : Llm_provider.Provider_config.t -> t option

(** Best-effort runtime provider id for a concrete provider config.

    When the endpoint matches a catalog or registry binding, returns that
    binding id. Otherwise returns a stable kind-derived label such as
    ["openai_compat"]; it never invents a fake provider id. *)
val provider_id_of_provider_config : Llm_provider.Provider_config.t -> string

(** Best-effort runtime provider id for an Agent SDK {!Provider.config}.
    Custom providers are reported by their registered name, without a
    ["custom:"] display prefix. *)
val provider_id_of_config : Provider.config -> string

(** Resolve OAS-owned provider capabilities for a concrete provider config.

    Catalog/registry provider capabilities are preferred. Non-CLI providers
    then honor model-specific capability overrides when available. *)
val capabilities_for_provider_config
  :  Llm_provider.Provider_config.t
  -> Llm_provider.Capabilities.capabilities

(** Resolve an exact caller model or the binding's catalog-declared default.
    Missing model identity is an explicit configuration error; OAS never
    invents a provider-specific model or expands aliases. *)
val resolve_model : t -> requested_model:string option -> (string, Error.sdk_error) result

(** Resolve an exact provider id or catalog-declared alias together with the
    public Agent SDK provider configuration. Unknown selectors return [None];
    this function does not invent a fallback provider or reinterpret model
    aliases as provider selectors. *)
val resolve
  :  ?model:string
  -> string
  -> (t * Provider.config, Error.sdk_error) result option

(** Convert a binding into the low-level provider config used by OAS
    transports. *)
val to_provider_config
  :  ?model:string
  -> t
  -> (Llm_provider.Provider_config.t, Error.sdk_error) result
