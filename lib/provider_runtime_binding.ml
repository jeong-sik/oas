(** Read-only runtime provider bindings. *)

module PC = Llm_provider.Provider_catalog
module PR = Llm_provider.Provider_registry
module PConfig = Llm_provider.Provider_config

type provider_kind = PConfig.provider_kind
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

let normalize value = String.trim value |> String.lowercase_ascii

let trim_non_empty value =
  let trimmed = String.trim value in
  if trimmed = "" then None else Some trimmed
;;

let transport_of_catalog = function
  | PC.Http -> Http
  | PC.Managed -> Managed
;;

let auth_of_catalog = function
  | PC.No_auth -> No_auth
  | PC.Api_key_env env -> Api_key_env env
  | PC.Oauth_cached_login -> Oauth_cached_login
  | PC.Setup_token_env env -> Setup_token_env env
;;

let command_of_kind = function
  | PConfig.Anthropic
  | PConfig.Kimi
  | PConfig.OpenAI_compat
  | PConfig.Ollama
  | PConfig.Gemini
  | PConfig.Glm
  | PConfig.DashScope -> None
;;

let transport_of_kind _kind = Http

let auth_of_defaults (defaults : PR.provider_defaults) =
  match trim_non_empty defaults.api_key_env with
  | Some env -> Api_key_env env
  | None -> No_auth
;;

let public_capabilities (caps : Llm_provider.Capabilities.capabilities)
  : Provider.capabilities
  =
  { max_context_tokens = caps.max_context_tokens
  ; max_output_tokens = caps.max_output_tokens
  ; supports_tools = caps.supports_tools
  ; supports_tool_choice = caps.supports_tool_choice
  ; supports_required_tool_choice = caps.supports_required_tool_choice
  ; supports_named_tool_choice = caps.supports_named_tool_choice
  ; supports_parallel_tool_calls = caps.supports_parallel_tool_calls
  ; supports_runtime_mcp_tools = caps.supports_runtime_mcp_tools
  ; supports_runtime_tool_events = caps.supports_runtime_tool_events
  ; assistant_tool_content_format = caps.assistant_tool_content_format
  ; supports_reasoning = caps.supports_reasoning
  ; supports_extended_thinking = caps.supports_extended_thinking
  ; supports_reasoning_budget = caps.supports_reasoning_budget
  ; accepted_reasoning_efforts = caps.accepted_reasoning_efforts
  ; thinking_control_format = caps.thinking_control_format
  ; preserve_thinking_control_format = caps.preserve_thinking_control_format
  ; reasoning_output_format = caps.reasoning_output_format
  ; reasoning_streaming_format = caps.reasoning_streaming_format
  ; reasoning_replay_override = caps.reasoning_replay_override
  ; supports_response_format_json = caps.supports_response_format_json
  ; supports_structured_output = caps.supports_structured_output
  ; supports_multimodal_inputs = caps.supports_multimodal_inputs
  ; supports_image_input = caps.supports_image_input
  ; supports_audio_input = caps.supports_audio_input
  ; supports_video_input = caps.supports_video_input
  ; modality_priority = caps.modality_priority
  ; task = caps.task
  ; supports_native_streaming = caps.supports_native_streaming
  ; supports_system_prompt = caps.supports_system_prompt
  ; supports_caching = caps.supports_caching
  ; supports_prompt_caching = caps.supports_prompt_caching
  ; prompt_cache_alignment = caps.prompt_cache_alignment
  ; supports_top_k = caps.supports_top_k
  ; supports_min_p = caps.supports_min_p
  ; supports_seed = caps.supports_seed
  ; supports_seed_with_images = caps.supports_seed_with_images
  ; supports_computer_use = caps.supports_computer_use
  ; supports_code_execution = caps.supports_code_execution
  ; emits_usage_tokens = caps.emits_usage_tokens
  ; supported_models = caps.supported_models
  }
;;

let registry_lookup_available registry id =
  match PR.find registry (normalize id) with
  | Some entry -> entry.is_available ()
  | None -> false
;;

let registry_lookup_max_context registry id fallback =
  match PR.find registry (normalize id) with
  | Some entry when entry.max_context > 0 -> Some entry.max_context
  | _ -> fallback
;;

let model_catalog_default_for_base_label base_label =
  let base_label = normalize base_label in
  match Llm_provider.Model_catalog.global () with
  | None -> None
  | Some catalog ->
    List.find_map
      (fun (entry : Llm_provider.Model_catalog.model_entry) ->
         match entry.base_label with
         | Some base when String.equal (normalize base) base_label ->
           trim_non_empty entry.id_prefix
         | Some _other_base -> None
         | None -> None)
      catalog
;;

let registry_default_model provider_id =
  match normalize provider_id with
  | "claude" | "anthropic" -> None
  | "ollama" -> Some "default"
  | provider_id ->
    (match model_catalog_default_for_base_label provider_id with
     | Some _ as model -> model
     | None -> Some provider_id)
;;

let binding_of_catalog_entry registry (entry : PC.entry) =
  { id = normalize entry.id
  ; aliases = List.map normalize entry.aliases
  ; kind = entry.kind
  ; transport = transport_of_catalog entry.transport
  ; command = entry.command
  ; base_url = entry.base_url
  ; request_path = entry.request_path
  ; api_key_env = entry.api_key_env
  ; auth = auth_of_catalog entry.auth
  ; default_model = entry.default_model
  ; max_context = registry_lookup_max_context registry entry.id entry.max_context
  ; capabilities = public_capabilities entry.capabilities
  ; available = registry_lookup_available registry entry.id
  ; credential_scope = entry.credential_scope
  }
;;

let binding_of_registry_entry (entry : PR.entry) =
  { id = normalize entry.name
  ; aliases = []
  ; kind = entry.defaults.kind
  ; transport = transport_of_kind entry.defaults.kind
  ; command = command_of_kind entry.defaults.kind
  ; base_url = entry.defaults.base_url
  ; request_path = entry.defaults.request_path
  ; api_key_env = entry.defaults.api_key_env
  ; auth = auth_of_defaults entry.defaults
  ; default_model = registry_default_model entry.name
  ; max_context = (if entry.max_context > 0 then Some entry.max_context else None)
  ; capabilities = public_capabilities entry.capabilities
  ; available = entry.is_available ()
  ; credential_scope = None
  }
;;

let catalog_entries () = Option.value (PC.global ()) ~default:[]

let catalog_names entries =
  entries
  |> List.concat_map (fun (entry : PC.entry) -> entry.id :: entry.aliases)
  |> List.map normalize
;;

let sort_bindings bindings = List.sort (fun a b -> String.compare a.id b.id) bindings

let builtin_provider_aliases =
  [ "anthropic", "claude"
  ; "claude", "claude"
  ; "anthropic", "claude"
  ; "moonshot", "kimi"
  ; "kimi", "kimi"
  ; "gemini", "gemini"
  ; "glm", "glm"
  ; "zai", "glm"
  ; "zhipu", "glm"
  ; "glm-coding", "glm-coding"
  ; "zai-coding", "glm-coding"
  ; "zhipu-coding", "glm-coding"
  ; "openrouter", "openrouter"
  ; "deepseek", "deepseek"
  ; "groq", "groq"
  ; "dashscope", "dashscope"
  ; "qwen", "dashscope"
  ]
;;

let builtin_alias_target label = List.assoc_opt (normalize label) builtin_provider_aliases

let binding_by_exact_label registry normalized =
  match PC.global () with
  | Some catalog ->
    (match PC.lookup catalog normalized with
     | Some entry -> Some (binding_of_catalog_entry registry entry)
     | None -> Option.map binding_of_registry_entry (PR.find registry normalized))
  | None -> Option.map binding_of_registry_entry (PR.find registry normalized)
;;

let all () =
  let registry = PR.default () in
  let catalog = catalog_entries () in
  let catalog_name_set = catalog_names catalog in
  let from_catalog = List.map (binding_of_catalog_entry registry) catalog in
  let from_registry =
    PR.all registry
    |> List.filter (fun (entry : PR.entry) ->
      not (List.mem (normalize entry.name) catalog_name_set))
    |> List.map binding_of_registry_entry
  in
  sort_bindings (from_catalog @ from_registry)
;;

let find label =
  let normalized = normalize label in
  if normalized = ""
  then None
  else (
    let registry = PR.default () in
    match binding_by_exact_label registry normalized with
    | Some _ as binding -> binding
    | None ->
      (match builtin_alias_target normalized with
       | Some target -> binding_by_exact_label registry target
       | None -> None))
;;

let find_catalog label =
  let normalized = normalize label in
  if normalized = ""
  then None
  else (
    let registry = PR.default () in
    match PC.global () with
    | Some catalog ->
      Option.map (binding_of_catalog_entry registry) (PC.lookup catalog normalized)
    | None -> None)
;;

let known_labels () =
  let binding_labels =
    all ()
    |> List.concat_map (fun binding -> binding.id :: binding.aliases)
    |> List.map normalize
  in
  let alias_labels = List.map fst builtin_provider_aliases in
  List.sort_uniq String.compare (binding_labels @ alias_labels)
;;

let normalize_endpoint_url value =
  let trimmed = String.trim value in
  if trimmed = ""
  then trimmed
  else (
    let rec strip_trailing_slash s =
      let len = String.length s in
      if len > 1 && s.[len - 1] = '/'
      then strip_trailing_slash (String.sub s 0 (len - 1))
      else s
    in
    strip_trailing_slash trimmed)
;;

let config_endpoint_matches ~kind ~base_url ~request_path (cfg : PConfig.t) =
  kind = cfg.kind
  && String.equal (normalize_endpoint_url base_url) (normalize_endpoint_url cfg.base_url)
  && String.equal (String.trim request_path) (String.trim cfg.request_path)
;;

let catalog_entry_matches_config cfg (entry : PC.entry) =
  config_endpoint_matches
    ~kind:entry.kind
    ~base_url:entry.base_url
    ~request_path:entry.request_path
    cfg
;;

let registry_entry_matches_config cfg (entry : PR.entry) =
  config_endpoint_matches
    ~kind:entry.defaults.kind
    ~base_url:entry.defaults.base_url
    ~request_path:entry.defaults.request_path
    cfg
;;

let catalog_binding_for_provider_config registry cfg =
  match PC.global () with
  | None -> None
  | Some catalog ->
    catalog
    |> List.find_opt (catalog_entry_matches_config cfg)
    |> Option.map (binding_of_catalog_entry registry)
;;

let registry_binding_for_provider_config registry cfg =
  registry
  |> PR.all
  |> List.find_opt (registry_entry_matches_config cfg)
  |> Option.map binding_of_registry_entry
;;

let binding_for_provider_config (cfg : PConfig.t) =
  let registry = PR.default () in
  match catalog_binding_for_provider_config registry cfg with
  | Some binding -> Some binding
  | None -> registry_binding_for_provider_config registry cfg
;;

let provider_id_of_provider_config cfg =
  match binding_for_provider_config cfg with
  | Some binding -> binding.id
  | None -> PR.provider_name_of_config cfg
;;

let provider_id_of_config (cfg : Provider.config) =
  match cfg.provider with
  | Provider.Local _ -> "local"
  | Provider.Anthropic -> "claude"
  | Provider.Custom_registered { name } ->
    (match find name with
     | Some binding -> binding.id
     | None -> normalize name)
  | Provider.OpenAICompat { base_url; path; _ } ->
    let provider_config =
      PConfig.make
        ~kind:PConfig.OpenAI_compat
        ~model_id:cfg.model_id
        ~base_url
        ~request_path:path
        ()
    in
    provider_id_of_provider_config provider_config
;;

let base_capabilities_of_kind = function
  | PConfig.Ollama -> Llm_provider.Capabilities.ollama_capabilities
  | PConfig.Anthropic -> Llm_provider.Capabilities.anthropic_capabilities
  | PConfig.Kimi -> Llm_provider.Capabilities.kimi_capabilities
  | PConfig.OpenAI_compat -> Llm_provider.Capabilities.openai_compat_chat_capabilities
  | PConfig.Gemini -> Llm_provider.Capabilities.gemini_capabilities
  | PConfig.Glm -> Llm_provider.Capabilities.glm_capabilities
  | PConfig.DashScope -> Llm_provider.Capabilities.dashscope_capabilities
;;

let registry_capabilities_for_provider_config (cfg : PConfig.t) =
  let registry = PR.default () in
  let catalog_entry =
    Option.bind (PC.global ()) (List.find_opt (catalog_entry_matches_config cfg))
  in
  match catalog_entry with
  | Some entry -> entry.PC.capabilities
  | None ->
    (match PR.all registry |> List.find_opt (registry_entry_matches_config cfg) with
     | Some entry -> entry.PR.capabilities
     | None ->
       let provider_name = PR.provider_name_of_config cfg in
       (match PR.find registry provider_name with
        | Some entry -> entry.PR.capabilities
        | None -> base_capabilities_of_kind cfg.kind))
;;

let capabilities_for_provider_config (cfg : PConfig.t) =
  let caps = registry_capabilities_for_provider_config cfg in
  let caps =
    match PConfig.capabilities_for_config_model cfg with
    | Some model_caps -> model_caps
    | None ->
      (match
         Llm_provider.Capabilities.for_provider_model_id
           ~allow_bare_fallback:false
           ~provider_label:(provider_id_of_provider_config cfg)
           ~model_id:cfg.model_id
       with
       | Some model_caps -> model_caps
       | None -> caps)
  in
  match cfg.supports_tool_choice_override with
  | Some supports_tool_choice ->
    { caps with
      supports_tool_choice
    ; supports_required_tool_choice = supports_tool_choice
    ; supports_named_tool_choice = supports_tool_choice
    }
  | None -> caps
;;

let resolve_model binding ~requested_model =
  let normalize_requested_model model =
    match binding.kind with
    | PConfig.Anthropic -> Model_registry.resolve_model_id model
    | PConfig.Kimi
    | PConfig.OpenAI_compat
    | PConfig.Ollama
    | PConfig.Gemini
    | PConfig.Glm
    | PConfig.DashScope -> model
  in
  let fallback_default_model () =
    match binding.kind with
    | PConfig.Ollama -> "default"
    | PConfig.Anthropic ->
      Model_registry.default_model_id_value () |> Model_registry.resolve_model_id
    | PConfig.Kimi
    | PConfig.OpenAI_compat
    | PConfig.Gemini
    | PConfig.Glm
    | PConfig.DashScope -> binding.id
  in
  match Option.bind requested_model trim_non_empty with
  | Some model -> normalize_requested_model model
  | None ->
    (match binding.default_model with
     | Some model when String.trim model <> "" -> normalize_requested_model model
     | Some _blank_default_model -> fallback_default_model ()
     | None -> fallback_default_model ())
;;

let to_provider_config ?model binding =
  let model_id = resolve_model binding ~requested_model:model in
  let request_path = trim_non_empty binding.request_path in
  let max_context = binding.max_context in
  PConfig.make
    ~kind:binding.kind
    ~model_id
    ~base_url:binding.base_url
    ~supports_structured_output_override:binding.capabilities.supports_structured_output
    ~model_capabilities_override:binding.capabilities
    ?request_path
    ?max_context
    ()
;;

let is_local ?model binding =
  to_provider_config ?model binding |> Llm_provider.Provider_config.is_local
;;
