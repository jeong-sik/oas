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
  ; provider_kind : Llm_provider.Provider_config.provider_kind
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
  ; resolve : config -> (string * string * (string * string) list, Error.sdk_error) result
  }

let registry : (string, provider_impl) Hashtbl.t = Hashtbl.create 8
let registry_mu = Mutex.create ()
let with_registry_lock f = Mutex.protect registry_mu f

let register_provider impl =
  with_registry_lock (fun () -> Hashtbl.replace registry impl.name impl)
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

let declared_provider_defaults name =
  let registry = Llm_provider.Provider_registry.default () in
  match Llm_provider.Provider_registry.find registry name with
  | Some entry -> Ok entry.defaults
  | None ->
    Error
      (Error.Config
         (InvalidConfig
            { field = "provider"
            ; detail = Printf.sprintf "Provider %S has no registry declaration" name
            }))
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
  ; provider_kind = Llm_provider.Provider_config.Kimi
  ; request_kind = Anthropic_messages
  ; request_path = kimi_direct_request_path
  ; capabilities = Llm_provider.Capabilities.kimi_capabilities
  ; resolve =
      (fun cfg ->
        match declared_provider_defaults "kimi" with
        | Error _ as error -> error
        | Ok defaults ->
          let credential_env =
            let configured = String.trim cfg.api_key_env in
            if configured = "" then defaults.api_key_env else configured
          in
          (match Llm_provider.Cli_common_env.get credential_env with
           | Some key -> Ok (defaults.base_url, key, kimi_direct_headers key)
           | None -> Error (Error.Config (MissingEnvVar { var_name = credential_env }))))
  }
;;

let builtin_provider_impls = [ kimi_provider_impl ]

let find_provider name =
  match find_builtin_provider name builtin_provider_impls with
  | Some impl -> Some impl
  | None -> with_registry_lock (fun () -> Hashtbl.find_opt registry name)
;;

let registered_providers () =
  let dynamic =
    with_registry_lock (fun () ->
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
         Anthropic branch must too, otherwise consumers observe the wrong
         provider capability for every Sonnet/Opus 4 agent. *)
    (match Llm_provider.Capabilities.for_model_id model_id with
     | Some caps -> caps
     | None -> anthropic_capabilities)
  | Local _ ->
    (* Local llama-server/vLLM/LM Studio endpoints use the OpenAI-compatible
       request envelope, but a bare model id does not declare a vendor-specific
       thinking/reasoning/tool/schema dialect. *)
    default_openai_compat_capabilities ()
  | OpenAICompat _ -> default_openai_compat_capabilities ()
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

let local_llm ~base_url ~model_id () =
  { provider = Local { base_url }; model_id; api_key_env = "" }
;;

let anthropic ~model_id () =
  { provider = Anthropic; model_id; api_key_env = "ANTHROPIC_API_KEY" }
;;

let openrouter ~model_id () =
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

(* Pricing is sourced exclusively from the model catalog.  Unknown models stay
   [None]; this boundary does not infer a free price from provider or model-id
   text. *)
type pricing = Llm_provider.Pricing.pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float option
  ; cache_read_multiplier : float option
  }

type cache_price_component = Llm_provider.Pricing.cache_price_component =
  | Cache_creation
  | Cache_read

type cost_estimate = Llm_provider.Pricing.cost_estimate =
  | Estimated of float
  | Incomplete of cache_price_component list

let pricing_for_model_opt = Llm_provider.Pricing.pricing_for_model_opt
let estimate_cost = Llm_provider.Pricing.estimate_cost

(* ── Convenience: create config for a Custom_registered provider ── *)

let custom_provider ~name ~model_id ?(api_key_env = "") () =
  { provider = Custom_registered { name }; model_id; api_key_env }
;;

let api_key_from_declared_env env_name =
  if env_name = ""
  then Ok ""
  else (
    match Llm_provider.Cli_common_env.get env_name with
    | Some value when String.trim value <> "" -> Ok value
    | None | Some _ -> Error (Error.Config (MissingEnvVar { var_name = env_name })))
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

(** Forward adapter: build a {!Llm_provider.Provider_config.t} from an
    agent state and optional {!config}.  Used when a caller needs to
    bridge the legacy {!create_message} surface to the consolidated
    {!Llm_provider.Complete.complete} surface.

    Sampling params, tool_choice, thinking controls are pulled from
    [state.config]. Provider kind, model_id, headers, request_path,
    and api_key are resolved from an explicit [provider_opt]. [None]
    is a configuration error; this adapter never selects a provider.

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
      ~base_url:_
      (provider_opt : config option)
  : (Llm_provider.Provider_config.t, Error.sdk_error) result
  =
  let cfg = state.config in
  let build
        ~kind
        ?provider_id
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
         ?provider_id
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
         ?reasoning_effort:cfg.reasoning_effort
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
          (match find_provider name with
           | None ->
             Error
               (Error.Config
                  (InvalidConfig
                     { field = "provider"
                     ; detail =
                         Printf.sprintf
                           "Custom_registered provider '%s' is neither declared nor \
                            registered at runtime"
                           name
                     }))
           | Some impl ->
             (match impl.resolve p with
              | Error e -> Error e
              | Ok (resolved_base_url, api_key, headers) ->
                let kind = impl.provider_kind in
                let model_capabilities_override =
                  match
                    Llm_provider.Capabilities.for_provider_model_id
                      ~allow_bare_fallback:false
                      ~provider_label:name
                      ~model_id:p.model_id
                  with
                  | Some _ -> None
                  | None -> Some impl.capabilities
                in
                let supports_structured_output_override =
                  Option.map
                    (fun (caps : capabilities) -> caps.supports_structured_output)
                    model_capabilities_override
                in
                build
                  ~kind
                  ~provider_id:name
                  ~resolved_base_url
                  ~api_key
                  ~headers
                  ~request_path:impl.request_path
                  ~model_id:p.model_id
                  ?supports_structured_output_override
                  ?model_capabilities_override
                  ()))
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
                ~provider_label:entry.name
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
                  ~provider_id:entry.name
                  ~resolved_base_url
                  ~api_key
                  ~headers
                  ~request_path:entry.defaults.request_path
                  ~model_id:p.model_id
                  ?supports_structured_output_override:registry_so_override
                  ?model_capabilities_override:registry_caps_override
                  ())
           | None ->
             (match api_key_from_declared_env entry.defaults.api_key_env with
              | Error e -> Error e
              | Ok api_key ->
                (* Auth headers are NOT included here. Callers merge
                   [auth_headers_only_for_kind] at HTTP request time so that
                   [Provider_config.t.headers] never carries sensitive tokens. *)
                let headers = [ "Content-Type", "application/json" ] in
                build
                  ~kind:entry.defaults.kind
                  ~provider_id:entry.name
                  ~resolved_base_url:entry.defaults.base_url
                  ~api_key
                  ~headers
                  ~request_path:entry.defaults.request_path
                  ~model_id:p.model_id
                  ?supports_structured_output_override:registry_so_override
                  ?model_capabilities_override:registry_caps_override
                  ())))
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
    Error
      (Error.Config
         (InvalidConfig
            { field = "provider"
            ; detail = "An explicit provider configuration is required"
            }))
;;

let provider_config_with_agent_config
      ~(config : Types.agent_config)
      (provider_config : Llm_provider.Provider_config.t)
  =
  let model_id = Types.model_to_string config.model in
  let response_format = config.response_format in
  let output_schema =
    if response_format = provider_config.response_format
    then provider_config.output_schema
    else Llm_provider.Provider_config.output_schema_of_response_format response_format
  in
  let max_context, model_capabilities_override, supports_structured_output_override =
    if model_id = provider_config.model_id
    then
      ( provider_config.max_context
      , provider_config.model_capabilities_override
      , provider_config.supports_structured_output_override )
    else (
      let target_model =
        { provider_config with
          model_id
        ; max_context = None
        ; model_capabilities_override = None
        ; supports_structured_output_override = None
        }
      in
      let max_context =
        Option.bind
          (Llm_provider.Provider_config.capabilities_for_config_model target_model)
          (fun capabilities -> capabilities.max_context_tokens)
      in
      max_context, None, None)
  in
  { provider_config with
    model_id
  ; max_context
  ; model_capabilities_override
  ; supports_structured_output_override
  ; max_tokens = config.max_tokens
  ; temperature = config.temperature
  ; top_p = config.top_p
  ; top_k = config.top_k
  ; min_p = config.min_p
  ; system_prompt = config.system_prompt
  ; enable_thinking = config.enable_thinking
  ; preserve_thinking = config.preserve_thinking
  ; thinking_budget = config.thinking_budget
  ; reasoning_effort = config.reasoning_effort
  ; tool_choice = config.tool_choice
  ; disable_parallel_tool_use = config.disable_parallel_tool_use
  ; response_format
  ; output_schema
  ; cache_system_prompt = config.cache_system_prompt
  }
;;
