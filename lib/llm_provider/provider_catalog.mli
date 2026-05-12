(** External provider catalog overlay.

    Provider entries describe connection and runtime metadata without teaching
    OAS about any downstream coordinator. The catalog augments
    {!Provider_registry.default}: built-in entries remain as seed data, while
    file/runtime catalog entries overwrite or add provider ids at process
    startup.

    @since 0.194.0 *)

type transport =
  | Http
  | Cli
  | Managed
  | Custom_openai_compat
[@@deriving show]

type auth_mode =
  | No_auth
  | Api_key_env of string
  | Cli_cached_login
  | Oauth_cached_login
  | Setup_token_env of string
  | File of string
  | Exec of string
[@@deriving show]

type entry =
  { id : string
  ; aliases : string list
  ; kind : Provider_config.provider_kind
  ; transport : transport
  ; command : string option
  ; base_url : string
  ; request_path : string
  ; api_key_env : string
  ; auth : auth_mode
  ; default_model : string option
  ; max_context : int option
  ; capabilities : Capabilities.capabilities
  ; non_interactive : bool
  ; interactive_required : bool
  ; daemon_safe : bool
  ; credential_scope : string option
  }

type t = entry list

(** Parse a provider catalog JSON document.

    Expected top-level shape:

    {[
      {
        "schema_version": 1,
        "providers": [
          {
            "id": "vllm-local",
            "kind": "openai_compat",
            "transport": "http",
            "base_url": "http://127.0.0.1:8000",
            "request_path": "/v1/chat/completions",
            "auth": {"type": "none"},
            "capabilities_base": "openai_chat",
            "capabilities": {"supports_tools": true}
          }
        ]
      }
    ]} *)
val of_json : Yojson.Safe.t -> (t, string) result

val load_file : string -> (t, string) result
val load_runtime_file : string -> t option

(** Find an entry by id or alias. *)
val lookup : t -> string -> entry option

(** Return an entry's explicit default model by id or alias. *)
val default_model_for_provider : t -> string -> string option

(** Process-wide catalog overlay.

    Resolution order:
    + 1. runtime override installed with {!set_global}
    + 2. [OAS_PROVIDER_CATALOG] JSON file, loaded lazily once
    + 3. no overlay *)
val global : unit -> t option

val set_global : t -> unit
val clear_global : unit -> unit
