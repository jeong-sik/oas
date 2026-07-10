(** Provider abstraction for local and cloud LLM endpoints *)

type provider =
  | Local of { base_url : string }
  | Anthropic
  | OpenAICompat of
      { base_url : string
      ; auth_header : string option
      ; path : string
      ; static_token : string option
      }
  | Custom_registered of { name : string }

type config =
  { provider : provider
  ; model_id : string
  ; api_key_env : string
  }

type request_kind =
  | Anthropic_messages
  | Openai_chat_completions
  | Custom of string

type modality =
  | Text
  | Image
  | Audio
  | Video
  | Multimodal

include Llm_provider.Capabilities

type inference_contract =
  { provider : provider
  ; model_id : string
  ; modality : modality
  ; task : task option
  }

type model_spec =
  { provider : provider
  ; model_id : string
  ; api_key_env : string
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
  }

let default_openai_compat_capabilities () = openai_compat_chat_capabilities

let uses_native_glm_capabilities ~base_url ~model_id =
  Llm_provider.Provider_config.is_zai_glm_config
    (Llm_provider.Provider_config.make
       ~kind:Llm_provider.Provider_config.OpenAI_compat
       ~model_id
       ~base_url
       ())
;;

(* Shared by the [Local] and [OpenAICompat] branches of [capabilities_for_model]:
   both send requests over the OpenAI-compatible envelope and must resolve
   capabilities identically. [Provider_config.catalog_entry_requires_endpoint_declaration]
   now treats catalog entries whose [base_label] is ["glm"] as requiring an
   endpoint declaration, so a raw OpenAI-compatible or [Local] endpoint can no
   longer inherit GLM reasoning/tool capabilities from a bare model-id match.
   Declared Z.AI GLM endpoints are detected via
   [Provider_config.is_zai_glm_config] (endpoint + model id, typed SSOT) and
   keep the full catalog capabilities. *)
let openai_compat_capabilities_for ~base_url ~model_id =
  let is_native_glm = uses_native_glm_capabilities ~base_url ~model_id in
  if is_native_glm
  then (
    match Llm_provider.Capabilities.for_model_id model_id with
    | Some caps -> caps
    | None -> default_openai_compat_capabilities ())
  else (
    let config =
      Llm_provider.Provider_config.make
        ~kind:Llm_provider.Provider_config.OpenAI_compat
        ~model_id
        ~base_url
        ()
    in
    match Llm_provider.Provider_config.capabilities_for_config_model config with
    | Some caps -> caps
    | None -> default_openai_compat_capabilities ())
;;

let provider_name = function
  | Local _ -> "local"
  | Anthropic -> "anthropic"
  | OpenAICompat _ -> "openai_compat"
  | Custom_registered { name } -> "custom:" ^ name
;;

let modality_to_string = function
  | Text -> "text"
  | Image -> "image"
  | Audio -> "audio"
  | Video -> "video"
  | Multimodal -> "multimodal"
;;

let modality_of_capabilities (caps : capabilities) =
  let non_text_count =
    List.fold_left
      (fun acc supported -> if supported then acc + 1 else acc)
      0
      [ caps.supports_image_input; caps.supports_audio_input; caps.supports_video_input ]
  in
  if caps.supports_multimodal_inputs || non_text_count > 1
  then Multimodal
  else if caps.supports_image_input
  then Image
  else if caps.supports_audio_input
  then Audio
  else if caps.supports_video_input
  then Video
  else Text
;;

let task_to_string = Llm_provider.Capability_vocab.task_to_string

let modality_supported (caps : capabilities) = function
  | Text -> true
  | Image -> caps.supports_image_input
  | Audio -> caps.supports_audio_input
  | Video -> caps.supports_video_input
  | Multimodal ->
    let non_text_count =
      List.fold_left
        (fun acc supported -> if supported then acc + 1 else acc)
        0
        [ caps.supports_image_input
        ; caps.supports_audio_input
        ; caps.supports_video_input
        ]
    in
    caps.supports_multimodal_inputs || non_text_count > 1
;;

(* ── Provider Registry: runtime registration for custom providers ── *)

type provider_impl =
  { name : string
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
  ; build_body :
      config:Types.agent_state
      -> messages:Types.message list
      -> ?tools:Yojson.Safe.t list
      -> unit
      -> string
  ; parse_response : string -> Types.api_response
  ; resolve : config -> (string * string * (string * string) list, Error.sdk_error) result
  }

let registry : (string, provider_impl) Hashtbl.t = Hashtbl.create 8
let registry_mu = Eio.Mutex.create ()

let register_provider impl =
  Eio.Mutex.use_rw ~protect:true registry_mu (fun () ->
    Hashtbl.replace registry impl.name impl)
;;

let find_builtin_provider name = function
  | impl :: rest ->
    let rec loop current remaining =
      if current.name = name
      then Some current
      else (
        match remaining with
        | next :: tail -> loop next tail
        | [] -> None)
    in
    loop impl rest
  | [] -> None
;;

let first_present_env env_names =
  let rec loop = function
    | [] -> None
    | env_name :: rest ->
      (match Llm_provider.Cli_common_env.get env_name with
       | Some value -> Some (env_name, value)
       | None -> loop rest)
  in
  loop env_names
;;

let kimi_direct_base_url () =
  match Llm_provider.Cli_common_env.get "KIMI_BASE_URL" with
  | Some url -> url
  | None -> "https://api.kimi.com/coding"
;;

let kimi_direct_request_path = "/v1/messages"

(** Non-auth headers for openrouter (anthropic-compatible).
    Auth header ("x-api-key") is NOT included — callers merge
    [auth_headers_only_for_kind] at HTTP request time. *)
let kimi_direct_headers _key =
  [ "Content-Type", "application/json"; "anthropic-version", "2023-06-01" ]
;;

let kimi_provider_impl : provider_impl =
  { name = "kimi"
  ; request_kind = Anthropic_messages
  ; request_path = kimi_direct_request_path
  ; capabilities = Llm_provider.Capabilities.kimi_capabilities
  ; build_body =
      (fun ~config ~messages ?tools () ->
        Yojson.Safe.to_string
          (`Assoc
              (Api_anthropic.build_body_assoc
                 ~config
                 ~messages
                 ~message_to_json:Llm_provider.Api_common.kimi_message_to_json
                 ~provider_kind:Llm_provider.Provider_config.Kimi
                 ?tools
                 ~stream:false
                 ())))
  ; parse_response =
      (fun body_str -> Api_anthropic.parse_response (Yojson.Safe.from_string body_str))
  ; resolve =
      (fun cfg ->
        let env_names =
          if String.trim cfg.api_key_env <> ""
          then [ cfg.api_key_env; "KIMI_API_KEY" ]
          else [ "KIMI_API_KEY" ]
        in
        match first_present_env env_names with
        | Some (_env_name, key) ->
          Ok (kimi_direct_base_url (), key, kimi_direct_headers key)
        | None ->
          let var_name =
            match env_names with
            | preferred :: _ -> preferred
            | [] -> "KIMI_API_KEY"
          in
          Error (Error.Config (MissingEnvVar { var_name })))
  }
;;

let builtin_provider_impls = [ kimi_provider_impl ]

let find_provider name =
  match find_builtin_provider name builtin_provider_impls with
  | Some impl -> Some impl
  | None -> Eio.Mutex.use_ro registry_mu (fun () -> Hashtbl.find_opt registry name)
;;

let registered_providers () =
  let dynamic =
    Eio.Mutex.use_ro registry_mu (fun () ->
      Hashtbl.fold (fun name _ acc -> name :: acc) registry [])
  in
  builtin_provider_impls
  |> List.fold_left
       (fun acc impl -> if List.mem impl.name acc then acc else impl.name :: acc)
       dynamic
;;

let capabilities_for_model ~(provider : provider) ~(model_id : string) =
  match provider with
  | Anthropic ->
    (* Base [anthropic_capabilities] is a conservative 200K record;
         the per-model overrides (claude-opus-4, claude-sonnet-4, etc.)
         live in [Llm_provider.Capabilities.for_model_id] and carry the
         real 1M windows and output-token ceilings. The [Local] and
         [OpenAICompat] branches already consult that table; the
         Anthropic branch must too, otherwise every Sonnet/Opus 4 agent
         resolves to the wrong window and proactive compaction fires at
         ~150K instead of ~750K. *)
    (match Llm_provider.Capabilities.for_model_id model_id with
     | Some caps -> caps
     | None -> anthropic_capabilities)
  | Local { base_url } ->
    (* Local llama-server/vLLM/LM Studio endpoints use the OpenAI-compatible
       request envelope, but a bare model id is not an endpoint declaration for
       thinking/reasoning/tool/schema dialects. Route through the same helper
       as [OpenAICompat] so local compat endpoints get identical fail-closed
       and GLM-native-detection treatment as raw OpenAICompat providers. *)
    openai_compat_capabilities_for ~base_url ~model_id
  | OpenAICompat { base_url; _ } -> openai_compat_capabilities_for ~base_url ~model_id
  | Custom_registered { name } ->
    (match find_provider name with
     | Some impl -> impl.capabilities
     | None -> default_capabilities)
;;

let request_kind = function
  | Anthropic -> Anthropic_messages
  | Local _ | OpenAICompat _ -> Openai_chat_completions
  | Custom_registered { name } ->
    (match find_provider name with
     | Some impl -> impl.request_kind
     | None -> Custom name)
;;

let request_path = function
  | Anthropic -> "/v1/messages"
  | Local { base_url = _ } -> "/v1/chat/completions"
  | OpenAICompat { path; _ } -> path
  | Custom_registered { name } ->
    (match find_provider name with
     | Some impl -> impl.request_path
     | None -> "/v1/chat/completions")
;;

let capabilities_for_config (cfg : config) =
  capabilities_for_model ~provider:cfg.provider ~model_id:cfg.model_id
;;

(** Resolve a positive [max_context_tokens] from an optional provider
    config, falling back to [fallback] when the config is [None] or
    the capability reports [None]/[<= 0]. Shared by
    [Pipeline.proactive_context_window_tokens] and
    [Builder.with_context_thresholds] so both call sites agree on the
    "provider → capabilities → max_context_tokens" resolution step.
    Callers still own the literal fallback value because the two sites
    disagree on it intentionally (Pipeline uses a stricter 128K; Builder
    uses a looser 200K that plays well with broader token caps). *)
let resolve_max_context_tokens ~fallback (cfg_opt : config option) =
  match cfg_opt with
  | Some cfg ->
    let caps = capabilities_for_config cfg in
    (match caps.max_context_tokens with
     | Some n when n > 0 -> n
     | _ -> fallback)
  | None -> fallback
;;

let validate_inference_contract ~capabilities (contract : inference_contract) =
  if modality_supported capabilities contract.modality
  then Ok ()
  else
    Error
      (Error.Config
         (InvalidConfig
            { field = "modality"
            ; detail =
                Printf.sprintf
                  "Model '%s' for provider '%s' does not support modality '%s'"
                  contract.model_id
                  (provider_name contract.provider)
                  (modality_to_string contract.modality)
            }))
;;

let build_inference_contract ~provider ~model_id ~(capabilities : capabilities) =
  let contract =
    { provider
    ; model_id
    ; modality = modality_of_capabilities capabilities
    ; (* Catalog-declared only (models.toml [task] field via capabilities);
         undeclared models carry no task — the former model-id substring
         classifier was removed. *)
      task = capabilities.task
    }
  in
  match validate_inference_contract ~capabilities contract with
  | Ok () -> contract
  | Error err ->
    invalid_arg ("BUG: inferred invalid inference contract: " ^ Error.to_string err)
;;

let inference_contract_of_model_spec (spec : model_spec) =
  build_inference_contract
    ~provider:spec.provider
    ~model_id:spec.model_id
    ~capabilities:spec.capabilities
;;

let inference_contract_of_config (cfg : config) =
  let capabilities = capabilities_for_config cfg in
  build_inference_contract ~provider:cfg.provider ~model_id:cfg.model_id ~capabilities
;;

let model_spec_of_config (cfg : config) =
  let capabilities = capabilities_for_config cfg in
  let spec =
    { provider = cfg.provider
    ; model_id = cfg.model_id
    ; api_key_env = cfg.api_key_env
    ; request_kind = request_kind cfg.provider
    ; request_path = request_path cfg.provider
    ; capabilities
    }
  in
  let _ = inference_contract_of_model_spec spec in
  spec
;;

let resolve (cfg : config) =
  match cfg.provider with
  | Local { base_url } -> Ok (base_url, "", [ "Content-Type", "application/json" ])
  | Anthropic ->
    (match Llm_provider.Cli_common_env.get cfg.api_key_env with
     | Some key ->
       (* Auth header ("x-api-key") is NOT included in the returned headers
          list.  Callers must merge [auth_headers_only_for_kind] at HTTP
          request time so that [Provider_config.t.headers] never carries
          sensitive tokens. *)
       Ok
         ( "https://api.anthropic.com"
         , key
         , [ "anthropic-version", "2023-06-01"; "Content-Type", "application/json" ] )
     | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))
  | OpenAICompat { base_url; auth_header = _; static_token; _ } ->
    (match static_token with
     | Some key when String.trim key <> "" ->
       (* Auth header is NOT included in the returned headers list.
          Callers must merge [auth_headers_only_for_kind] at HTTP request
          time so that [Provider_config.t.headers] never carries tokens. *)
       Ok (base_url, key, [ "Content-Type", "application/json" ])
     | _ ->
       (match Llm_provider.Cli_common_env.get cfg.api_key_env with
        | Some key -> Ok (base_url, key, [ "Content-Type", "application/json" ])
        | None -> Ok (base_url, "", [ "Content-Type", "application/json" ])))
  | Custom_registered { name } ->
    (match find_provider name with
     | Some impl -> impl.resolve cfg
     | None ->
       Error
         (Error.Config
            (InvalidConfig
               { field = "provider"
               ; detail = Printf.sprintf "Custom provider '%s' not registered" name
               })))
;;

let local_llm () =
  { provider = Local { base_url = Defaults.resolve_local_llm_url () }
  ; model_id = "default"
  ; api_key_env = "DUMMY_KEY"
  }
;;

let anthropic_sonnet () =
  { provider = Anthropic
  ; model_id = "claude-sonnet-4-6"
  ; api_key_env = "ANTHROPIC_API_KEY"
  }
;;

let anthropic_haiku () =
  { provider = Anthropic
  ; model_id = "claude-haiku-4-5-20251001"
  ; api_key_env = "ANTHROPIC_API_KEY"
  }
;;

let anthropic_opus () =
  { provider = Anthropic
  ; model_id = "claude-opus-4-6"
  ; api_key_env = "ANTHROPIC_API_KEY"
  }
;;

let openrouter ?(model_id = "anthropic/claude-sonnet-4-6") () =
  { provider =
      OpenAICompat
        { base_url = "https://openrouter.ai/api/v1"
        ; auth_header = Some "Authorization"
        ; path = "/chat/completions"
        ; static_token = None
        }
  ; model_id
  ; api_key_env = "OPENROUTER_API_KEY"
  }
;;

(* ── Pricing: per-model cost estimation ────────────────────────── *)

(* Pricing is sourced from the external model catalog (models.toml) through
   [Llm_provider.Pricing] — the same module the live api/streaming/complete
   cost-annotation path already uses. This module previously carried a parallel
   hand-maintained [Util.string_contains] cascade over model-id substrings
   (RFC-OAS-018 §1 names lib/provider.ml as a "13-literal leak site"). That copy
   duplicated the catalog's pricing data and diverged from it: because
   string_contains matches anywhere and the branches were hand-ordered, the bare
   "gpt" branch shadowed "gpt-4.1", mis-pricing it 2.5/10.0 instead of the
   catalog's 2.0/8.0. Delegating here deletes the duplicate cascade so models.toml
   is the single source of truth for pricing, fixes the gpt-4.1 shadow, and
   makes the agent_turn/structured accumulators consistent with the live cost
   path.

   The delegate now keeps a small built-in fallback table for common cloud
   models so that installed SDKs without an external catalog still get non-zero
   pricing (addressing the #2098 review follow-up). The pricing delegate strips
   OpenRouter-style provider prefixes (e.g. ["anthropic/claude-sonnet-4-6"]) and
   uses exact or delimiter-anchored matching for catalog/fallback entries so
   broad aliases such as ["gpt"] do not price unknown future families.
   Unknown-model behavior is otherwise unchanged: [pricing_for_model_opt]
   returns [None] for an unrecognized model and [pricing_for_model] still
   collapses that to [zero_pricing] (the existing $0 contract — RFC-OAS-018
   Phase 2 fail-closed is deferred). *)
type pricing = Llm_provider.Pricing.pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

let zero_pricing = Llm_provider.Pricing.zero_pricing
let pricing_for_model_opt = Llm_provider.Pricing.pricing_for_model_opt
let pricing_for_model = Llm_provider.Pricing.pricing_for_model

let pricing_for_provider ~(provider : provider) ~(model_id : string) =
  match provider with
  | Local _ -> zero_pricing
  (* Cloud providers are priced by model id. Enumerated explicitly (no [_]
     catch-all) so a future provider variant forces a pricing decision at
     compile time rather than silently inheriting model-id pricing — e.g. a
     new local-like backend that should be zero-priced. *)
  | Anthropic | OpenAICompat _ | Custom_registered _ -> pricing_for_model model_id
;;

let estimate_cost
      ~(pricing : pricing)
      ~input_tokens
      ~output_tokens
      ?(cache_creation_input_tokens = 0)
      ?(cache_read_input_tokens = 0)
      ()
  =
  (* Regular input tokens (excluding cache tokens — those are billed separately) *)
  let regular_input =
    input_tokens - cache_creation_input_tokens - cache_read_input_tokens
  in
  let regular_input = max 0 regular_input in
  let rate = pricing.input_per_million /. 1_000_000.0 in
  let input_cost = Float.of_int regular_input *. rate in
  let cache_write_cost =
    Float.of_int cache_creation_input_tokens *. rate *. pricing.cache_write_multiplier
  in
  let cache_read_cost =
    Float.of_int cache_read_input_tokens *. rate *. pricing.cache_read_multiplier
  in
  let output_cost =
    Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0
  in
  input_cost +. cache_write_cost +. cache_read_cost +. output_cost
;;

(* ── Convenience: create config for a Custom_registered provider ── *)

let custom_provider ~name ?(model_id = "custom") ?(api_key_env = "DUMMY_KEY") () =
  { provider = Custom_registered { name }; model_id; api_key_env }
;;

(** Well-known env var names per provider kind.
    Used as fallback when [Provider_config.t.api_key] is empty.
    Delegates to {!Llm_provider.Provider_kind.default_api_key_env}
    — the sum type owns this convention so adding a new variant
    updates both call sites from one edit. *)
let default_api_key_env_of_kind (kind : Llm_provider.Provider_config.provider_kind)
  : string
  =
  match Llm_provider.Provider_kind.default_api_key_env kind with
  | Some env -> env
  | None -> ""
;;

let api_key_env_candidates = function
  | "OLLAMA_CLOUD_API_KEY" -> [ "OLLAMA_CLOUD_API_KEY"; "OLLAMA_API_KEY" ]
  | env_name -> [ env_name ]
;;

let api_key_from_env env_name =
  api_key_env_candidates env_name
  |> List.find_map Llm_provider.Cli_common_env.get
  |> Option.value ~default:""
;;

let headers_with_auth_for_kind
      ~(kind : Llm_provider.Provider_config.provider_kind)
      ~api_key
  =
  let base = [ "Content-Type", "application/json" ] in
  match String.trim api_key with
  | "" -> base
  | key ->
    (match kind with
     | Anthropic | Kimi ->
       [ "x-api-key", key
       ; "anthropic-version", "2023-06-01"
       ; "Content-Type", "application/json"
       ]
     | Gemini -> base
     | OpenAI_compat | Ollama | Glm | DashScope ->
       ("Authorization", "Bearer " ^ key) :: base)
;;

(** Return only the auth-specific headers for a given provider kind.
    Unlike {!headers_with_auth_for_kind} which returns the full header list
    (including Content-Type), this returns only the authentication header
    so it can be merged with existing non-auth headers at request time.
    This keeps [Provider_config.t.headers] free of sensitive tokens. *)
let auth_headers_only_for_kind
      ~(kind : Llm_provider.Provider_config.provider_kind)
      ~api_key
  =
  match String.trim api_key with
  | "" -> []
  | key ->
    (match kind with
     | Anthropic | Kimi -> [ "x-api-key", key ]
     | Gemini -> [ "x-goog-api-key", key ]
     | OpenAI_compat | Ollama | Glm | DashScope -> [ "Authorization", "Bearer " ^ key ])
;;

(** Convert a [Llm_provider.Provider_config.t] into a
    [Provider.config] (for Agent Builder).  Keeps the conversion
    internal to OAS so consumers don't need their own adapters.

    When [api_key] is empty, falls back to the well-known env var
    name for the provider kind (e.g. [ANTHROPIC_API_KEY]). *)
let config_of_provider_config (pc : Llm_provider.Provider_config.t) : config =
  (* When pc.api_key is non-empty it is the *resolved* API key value
     (not an env var name).  Pass it via static_token + auth_header
     so that resolve() includes it in the Authorization header without
     a second Sys.getenv_opt lookup (which would fail because the
     value is not an env var name).  When pc.api_key is empty, fall
     back to api_key_env so resolve() can look up the env var. *)
  let has_key = not (Llm_provider.Secret.is_empty pc.api_key) in
  let auth_header = if has_key then Some "Authorization" else None in
  let static_token =
    if has_key then Some (Llm_provider.Secret.header_value pc.api_key) else None
  in
  let provider =
    match pc.kind with
    | Anthropic -> Anthropic
    | Kimi -> Custom_registered { name = "kimi" }
    | Gemini ->
      OpenAICompat
        { base_url = pc.base_url; auth_header; path = pc.request_path; static_token }
    | Glm ->
      OpenAICompat
        { base_url = pc.base_url; auth_header; path = pc.request_path; static_token }
    | OpenAI_compat | Ollama | DashScope ->
      if Llm_provider.Provider_config.is_local pc
      then Local { base_url = pc.base_url }
      else
        OpenAICompat
          { base_url = pc.base_url; auth_header; path = pc.request_path; static_token }
  in
  let api_key_env = default_api_key_env_of_kind pc.kind in
  { provider; model_id = pc.model_id; api_key_env }
;;

(** Forward adapter: build a {!Llm_provider.Provider_config.t} from an
    agent state and optional {!config}.  Used when a caller needs to
    bridge the legacy {!create_message} surface to the consolidated
    {!Llm_provider.Complete.complete} surface.

    Sampling params, tool_choice, thinking controls are pulled from
    [state.config].  Provider kind, model_id, headers, request_path,
    and api_key are resolved from [provider_opt] + env vars.  When
    [provider_opt] is [None], falls back to Anthropic using
    [ANTHROPIC_API_KEY] (matching {!create_message}'s existing default).

    [OpenAICompat] provider collapses to [OpenAI_compat] kind — the
    legacy {!config} variant does not distinguish arbitrary
    OpenAI-compatible endpoints from named providers carrying their
    own kind.  Callers that require kind + arbitrary URL should
    construct {!Llm_provider.Provider_config.t} directly via
    {!Llm_provider.Provider_config.make}.

    [Custom_registered {name}] preserves the registry-declared
    {!Llm_provider.Provider_config.provider_kind}
    (Gemini/Glm/Ollama/Claude_code/etc.) by looking [name] up in
    {!Llm_provider.Provider_registry.default} and using
    [entry.defaults.kind] and [entry.defaults.request_path].
    Returns [Error InvalidConfig] when [name] is not registered.

    @since 0.155.0
    @since 0.161.0 — Custom_registered kind preservation *)
let provider_config_of_agent
      ~(state : Types.agent_state)
      ~(base_url : string)
      (provider_opt : config option)
  : (Llm_provider.Provider_config.t, Error.sdk_error) result
  =
  let cfg = state.config in
  let build
        ~kind
        ~resolved_base_url
        ~api_key
        ~headers
        ~request_path
        ~model_id
        ?supports_structured_output_override
        ?model_capabilities_override
        ()
    =
    Ok
      (Llm_provider.Provider_config.make
         ~kind
         ~model_id
         ~base_url:resolved_base_url
         ~api_key
         ~headers
         ~request_path
         ?max_tokens:cfg.max_tokens
         ?temperature:cfg.temperature
         ?top_p:cfg.top_p
         ?top_k:cfg.top_k
         ?min_p:cfg.min_p
         ?enable_thinking:cfg.enable_thinking
         ?preserve_thinking:cfg.preserve_thinking
         ?thinking_budget:cfg.thinking_budget
         ?tool_choice:cfg.tool_choice
         ?system_prompt:cfg.system_prompt
         ~disable_parallel_tool_use:cfg.disable_parallel_tool_use
         ~response_format:cfg.response_format
         ~cache_system_prompt:cfg.cache_system_prompt
         ?supports_structured_output_override
         ?model_capabilities_override
         ())
  in
  match provider_opt with
  | Some p ->
    (match p.provider with
     | Custom_registered { name } ->
       (* Preserve the registry-declared provider_kind
              (Gemini/Glm/Ollama/Claude_code/etc.) instead of flattening
              to OpenAI_compat.

              Source of truth is {!Llm_provider.Provider_registry.default},
              which carries [entry.defaults.{kind, base_url, api_key_env,
              request_path}] per registered name. We read api_key from
              env directly rather than going through {!resolve}, because
              [resolve] dispatches via a separate {!Provider.registry}
              Hashtbl that is not guaranteed to contain the registry
              entries defined in {!Llm_provider.Provider_registry}. *)
       let registry = Llm_provider.Provider_registry.default () in
       (match Llm_provider.Provider_registry.find registry name with
        | None ->
          Error
            (Error.Config
               (InvalidConfig
                  { field = "provider"
                  ; detail =
                      Printf.sprintf
                        "Custom_registered provider '%s' not found in \
                         Provider_registry.default"
                        name
                  }))
        | Some entry ->
          (* If a provider-qualified model catalog row exists for this
             registered provider, let the row be authoritative instead of
             masking it with the provider default.  This prevents a blanket
             provider-level capability record (e.g. ollama_cloud_capabilities)
             from overriding per-model facts such as structured-output
             opt-ins/outs in models.toml.  When there is no matching row, fall
             back to the registry entry's declared capabilities so explicit
             Provider_catalog endpoint declarations still win. *)
          let registry_caps_override =
            match
              Llm_provider.Capabilities.for_provider_model_id
                ~allow_bare_fallback:false
                ~provider_label:name
                ~model_id:p.model_id
            with
            | Some _ -> None
            | None -> Some entry.capabilities
          in
          let registry_so_override =
            Option.map
              (fun caps -> caps.supports_structured_output)
              registry_caps_override
          in
          (match find_provider name with
           | Some impl ->
             (match impl.resolve p with
              | Error e -> Error e
              | Ok (resolved_base_url, api_key, headers) ->
                build
                  ~kind:entry.defaults.kind
                  ~resolved_base_url
                  ~api_key
                  ~headers
                  ~request_path:entry.defaults.request_path
                  ~model_id:p.model_id
                  ?supports_structured_output_override:registry_so_override
                  ?model_capabilities_override:registry_caps_override
                  ())
           | None ->
             let api_key =
               if entry.defaults.api_key_env = ""
               then ""
               else api_key_from_env entry.defaults.api_key_env
             in
             (* Auth headers are NOT included here.  Callers merge
                [auth_headers_only_for_kind] at HTTP request time so that
                [Provider_config.t.headers] never carries sensitive tokens. *)
             let headers = [ "Content-Type", "application/json" ] in
             build
               ~kind:entry.defaults.kind
               ~resolved_base_url:entry.defaults.base_url
               ~api_key
               ~headers
               ~request_path:entry.defaults.request_path
               ~model_id:p.model_id
               ?supports_structured_output_override:registry_so_override
               ?model_capabilities_override:registry_caps_override
               ()))
     | Anthropic | Local _ | OpenAICompat _ ->
       (match resolve p with
        | Error e -> Error e
        | Ok (url, api_key, headers) ->
          let kind : Llm_provider.Provider_config.provider_kind =
            match p.provider with
            | Anthropic -> Anthropic
            | Local _ | OpenAICompat _ -> OpenAI_compat
            | Custom_registered _ ->
              invalid_arg "provider kind: Custom_registered excluded by outer match"
          in
          let sanitized_api_key =
            match p.provider with
            | Local _ -> ""
            | Anthropic | OpenAICompat _ -> api_key
            | Custom_registered _ ->
              invalid_arg
                "provider sanitized_api_key: Custom_registered excluded by outer match"
          in
          build
            ~kind
            ~resolved_base_url:url
            ~api_key:sanitized_api_key
            ~headers
            ~request_path:(request_path p.provider)
            ~model_id:p.model_id
            ()))
  | None ->
    let fallback_provider : config =
      { provider = Anthropic
      ; model_id = Types.model_to_string cfg.model
      ; api_key_env = "ANTHROPIC_API_KEY"
      }
    in
    (match resolve fallback_provider with
     | Error e -> Error e
     | Ok (_resolved_url, api_key, headers) ->
       build
         ~kind:Anthropic
         ~resolved_base_url:base_url
         ~api_key
         ~headers
         ~request_path:(request_path Anthropic)
         ~model_id:(Types.model_to_string cfg.model)
         ())
;;
