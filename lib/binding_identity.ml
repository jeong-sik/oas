(** Opaque identity of one resolved OAS provider binding. *)

module PC = Llm_provider.Provider_config
module PK = Llm_provider.Provider_kind
module Secret = Llm_provider.Secret

type transport =
  | Http
  | Managed
  | Injected

type provider_identity =
  | Registered of string
  | Unregistered of PC.provider_kind

type auth_scheme =
  | No_auth
  | Api_key
  | Oauth_cached_login
  | Setup_token
  | Provider_defined

type t =
  { provider : provider_identity
  ; model_id : string
  ; transport : transport
  ; endpoint : Uri.t
  ; request_path : Uri.t
  ; auth_scheme : auth_scheme
  ; credential_scope : string option
  ; credential_identity : Secret.identity option
  }

let canonical_uri value = Uri.of_string value |> Uri.canonicalize

let provider_and_binding config =
  match Provider_runtime_binding.binding_for_provider_config config with
  | Some binding -> Registered binding.id, Some binding
  | None -> Unregistered config.PC.kind, None
;;

let auth_scheme_of_binding (binding : Provider_runtime_binding.t) =
  match binding.auth with
  | Provider_runtime_binding.No_auth -> No_auth
  | Provider_runtime_binding.Api_key_env _ -> Api_key
  | Provider_runtime_binding.Oauth_cached_login -> Oauth_cached_login
  | Provider_runtime_binding.Setup_token_env _ -> Setup_token
;;

let transport_for_call ~injected config =
  match Provider_runtime_binding.binding_for_provider_config config with
  | Some { transport = Provider_runtime_binding.Managed; _ } -> Managed
  | Some { transport = Provider_runtime_binding.Http; _ } | None ->
    if injected then Injected else Http
;;

let of_provider_config ~transport config =
  let provider, binding = provider_and_binding config in
  let auth_scheme, credential_scope =
    match binding with
    | Some binding -> auth_scheme_of_binding binding, binding.credential_scope
    | None -> (if Secret.is_empty config.PC.api_key then No_auth else Api_key), None
  in
  { provider
  ; model_id = config.PC.model_id
  ; transport
  ; endpoint = canonical_uri config.PC.base_url
  ; request_path = canonical_uri config.PC.request_path
  ; auth_scheme
  ; credential_scope
  ; credential_identity = Secret.identity config.PC.api_key
  }
;;

let of_resolved_provider
      ~transport
      ~(provider : Provider.config)
      ~base_url
      ~request_path
      ~api_key
  =
  let api_key_secret = Secret.of_string api_key in
  let non_custom_identity kind =
    let config =
      PC.make ~kind ~model_id:provider.model_id ~base_url ~api_key ~request_path ()
    in
    let provider_identity, binding = provider_and_binding config in
    let auth_scheme, credential_scope =
      match binding with
      | Some binding -> auth_scheme_of_binding binding, binding.credential_scope
      | None -> (if Secret.is_empty api_key_secret then No_auth else Api_key), None
    in
    provider_identity, auth_scheme, credential_scope
  in
  let provider_identity, auth_scheme, credential_scope =
    match provider.provider with
    | Provider.Custom_registered { name } ->
      (match Provider_runtime_binding.find name with
       | Some binding ->
         Registered binding.id, auth_scheme_of_binding binding, binding.credential_scope
       | None -> Registered name, Provider_defined, None)
    | Provider.Anthropic -> non_custom_identity PC.Anthropic
    | Provider.Local _ | Provider.OpenAICompat _ -> non_custom_identity PC.OpenAI_compat
  in
  { provider = provider_identity
  ; model_id = provider.model_id
  ; transport
  ; endpoint = canonical_uri base_url
  ; request_path = canonical_uri request_path
  ; auth_scheme
  ; credential_scope
  ; credential_identity = Secret.identity api_key_secret
  }
;;

let equal_provider left right =
  match left, right with
  | Registered left, Registered right -> String.equal left right
  | Unregistered left, Unregistered right -> left = right
  | Registered _, Unregistered _ | Unregistered _, Registered _ -> false
;;

let equal_credential left right =
  match left, right with
  | None, None -> true
  | Some left, Some right -> Secret.equal_identity left right
  | None, Some _ | Some _, None -> false
;;

let equal left right =
  equal_provider left.provider right.provider
  && String.compare left.model_id right.model_id = 0
  && left.transport = right.transport
  && Uri.equal left.endpoint right.endpoint
  && Uri.equal left.request_path right.request_path
  && left.auth_scheme = right.auth_scheme
  && Option.equal String.equal left.credential_scope right.credential_scope
  && equal_credential left.credential_identity right.credential_identity
;;

let hash_provider = function
  | Registered id -> Hashtbl.hash (0, id)
  | Unregistered kind -> Hashtbl.hash (1, PK.to_string kind)
;;

let hash_credential = function
  | None -> None
  | Some identity -> Some (Secret.hash_identity identity)
;;

let hash identity =
  Hashtbl.hash
    ( hash_provider identity.provider
    , identity.model_id
    , identity.transport
    , Uri.to_string identity.endpoint
    , Uri.to_string identity.request_path
    , identity.auth_scheme
    , identity.credential_scope
    , hash_credential identity.credential_identity )
;;

let has_credential_identity identity = Option.is_some identity.credential_identity

let transport_to_string = function
  | Http -> "http"
  | Managed -> "managed"
  | Injected -> "injected"
;;

let auth_scheme_to_string = function
  | No_auth -> "none"
  | Api_key -> "api_key"
  | Oauth_cached_login -> "oauth_cached_login"
  | Setup_token -> "setup_token"
  | Provider_defined -> "provider_defined"
;;

let redacted_uri uri =
  let redacted_query =
    match Uri.query uri with
    | [] -> []
    | _ -> [ "<redacted>", [ "<redacted>" ] ]
  in
  uri
  |> fun uri ->
  Uri.with_userinfo uri None
  |> fun uri ->
  Uri.with_query uri redacted_query
  |> fun uri -> Uri.with_fragment uri None |> Uri.to_string
;;

let provider_to_yojson = function
  | Registered id -> `Assoc [ "registration", `String "registered"; "id", `String id ]
  | Unregistered kind ->
    `Assoc [ "registration", `String "unregistered"; "kind", `String (PK.to_string kind) ]
;;

let option_string_to_yojson = function
  | Some value -> `String value
  | None -> `Null
;;

let credential_fingerprint_to_yojson = function
  | Some identity -> `String (Secret.identity_fingerprint identity)
  | None -> `Null
;;

let to_redacted_yojson identity =
  `Assoc
    [ "provider", provider_to_yojson identity.provider
    ; "model", `String identity.model_id
    ; "transport", `String (transport_to_string identity.transport)
    ; "endpoint", `String (redacted_uri identity.endpoint)
    ; "request_path", `String (redacted_uri identity.request_path)
    ; "auth_scheme", `String (auth_scheme_to_string identity.auth_scheme)
    ; "credential_scope", option_string_to_yojson identity.credential_scope
    ; ( "credential_fingerprint"
      , credential_fingerprint_to_yojson identity.credential_identity )
    ]
;;
