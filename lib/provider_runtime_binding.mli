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
  | Cli
  | Managed
  | Custom_openai_compat

type auth =
  | No_auth
  | Api_key_env of string
  | Cli_cached_login
  | Oauth_cached_login
  | Setup_token_env of string
  | File of string
  | Exec of string

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
  ; non_interactive : bool
  ; interactive_required : bool
  ; daemon_safe : bool
  ; credential_scope : string option
  }

(** Return all known runtime bindings from the default provider registry,
    including any process/env provider catalog overlay. *)
val all : unit -> t list

(** Find a runtime binding by provider id or alias. Lookup is
    case-insensitive and whitespace-trimmed. *)
val find : string -> t option

(** Resolve the model that should be used for a binding. [requested_model]
    wins when non-empty, followed by the binding catalog default, then OAS
    provider defaults. *)
val resolve_model : t -> requested_model:string option -> string

(** Convert a binding into the low-level provider config used by OAS
    transports. *)
val to_provider_config : ?model:string -> t -> Llm_provider.Provider_config.t
