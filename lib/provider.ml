(** Provider abstraction for local and cloud LLM endpoints *)

type provider =
  | Local of { base_url: string }
  | Anthropic
  | OpenAICompat of {
      base_url: string;
      auth_header: string option;
      path: string;
      static_token: string option;
    }
  | Custom_registered of { name: string }

type config = {
  provider: provider;
  model_id: string;
  api_key_env: string;
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

type inference_contract = {
  provider: provider;
  model_id: string;
  modality: modality;
  task: string option;
}

type model_spec = {
  provider: provider;
  model_id: string;
  api_key_env: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
}

let string_contains = Util.string_contains
let contains_substring_ci = Util.contains_substring_ci

(** Check if a model needs extended OpenAI capabilities
    (reasoning, top_k, min_p). Currently triggers on qwen family models. *)
let needs_extended_capabilities model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  string_contains ~needle:"qwen" normalized

let default_openai_compat_capabilities model_id =
  if needs_extended_capabilities model_id then
    openai_chat_extended_capabilities
  else
    openai_chat_capabilities

let uses_native_glm_capabilities ~base_url ~model_id =
  Llm_provider.Zai_catalog.is_zai_base_url base_url
  && Llm_provider.Zai_catalog.is_glm_model_id model_id

let provider_name = function
  | Local _ -> "local"
  | Anthropic -> "anthropic"
  | OpenAICompat _ -> "openai_compat"
  | Custom_registered { name } -> "custom:" ^ name

let modality_to_string = function
  | Text -> "text"
  | Image -> "image"
  | Audio -> "audio"
  | Video -> "video"
  | Multimodal -> "multimodal"

let modality_of_capabilities (caps : capabilities) =
  let non_text_count =
    List.fold_left (fun acc supported -> if supported then acc + 1 else acc) 0 [
      caps.supports_image_input;
      caps.supports_audio_input;
      caps.supports_video_input;
    ]
  in
  if caps.supports_multimodal_inputs || non_text_count > 1 then Multimodal
  else if caps.supports_image_input then Image
  else if caps.supports_audio_input then Audio
  else if caps.supports_video_input then Video
  else Text

let task_of_model_id model_id =
  let has needle = contains_substring_ci ~haystack:model_id ~needle in
  if has "whisper" || has "transcribe" || has "transcription" || has "stt" then
    Some "transcription"
  else if has "tts" || has "text-to-speech" || has "voice" then
    Some "speech"
  else if has "imagegen" || has "image-gen" || has "gpt-image" || has "cogview"
          || has "glm-image"
          || has "seedream" || has "flux" then
    Some "image_generation"
  else if has "video-gen" || has "veo" || has "kling" || has "sora" || has "wan"
          || has "cogvideox" || has "vidu" then
    Some "video_generation"
  else
    None

let modality_supported (caps : capabilities) = function
  | Text -> true
  | Image -> caps.supports_image_input
  | Audio -> caps.supports_audio_input
  | Video -> caps.supports_video_input
  | Multimodal ->
    let non_text_count =
      List.fold_left (fun acc supported -> if supported then acc + 1 else acc) 0 [
        caps.supports_image_input;
        caps.supports_audio_input;
        caps.supports_video_input;
      ]
    in
    caps.supports_multimodal_inputs || non_text_count > 1

(* ── Provider Registry: runtime registration for custom providers ── *)

type provider_impl = {
  name: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
  build_body:
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    unit -> string;
  parse_response: string -> Types.api_response;
  resolve: config -> (string * string * (string * string) list, Error.sdk_error) result;
}

let registry : (string, provider_impl) Hashtbl.t = Hashtbl.create 8
let registry_mu = Eio.Mutex.create ()

let register_provider impl =
  Eio.Mutex.use_rw ~protect:true registry_mu (fun () ->
    Hashtbl.replace registry impl.name impl)

let find_provider name =
  Eio.Mutex.use_ro registry_mu (fun () ->
    Hashtbl.find_opt registry name)

let registered_providers () =
  Eio.Mutex.use_ro registry_mu (fun () ->
    Hashtbl.fold (fun name _ acc -> name :: acc) registry [])

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
  | Local _ ->
      (* Local (llama-server) uses OpenAI-compatible API.
         Resolve capabilities by model_id, fall back to openai_chat. *)
      (match Llm_provider.Capabilities.for_model_id model_id with
       | Some caps -> caps
       | None -> openai_chat_capabilities)
  | OpenAICompat { base_url; _ } ->
      if Llm_provider.Zai_catalog.is_glm_model_id model_id
         && not (uses_native_glm_capabilities ~base_url ~model_id)
      then
        default_openai_compat_capabilities model_id
      else
        (match Llm_provider.Capabilities.for_model_id model_id with
         | Some caps -> caps
         | None -> default_openai_compat_capabilities model_id)
  | Custom_registered { name } ->
      (match find_provider name with
       | Some impl -> impl.capabilities
       | None -> default_capabilities)

let request_kind = function
  | Anthropic -> Anthropic_messages
  | Local _ | OpenAICompat _ -> Openai_chat_completions
  | Custom_registered { name } ->
      (match find_provider name with
       | Some impl -> impl.request_kind
       | None -> Custom name)

let request_path = function
  | Anthropic -> "/v1/messages"
  | Local { base_url = _ } -> "/v1/chat/completions"
  | OpenAICompat { path; _ } -> path
  | Custom_registered { name } ->
      (match find_provider name with
       | Some impl -> impl.request_path
       | None -> "/v1/chat/completions")

let capabilities_for_config (cfg : config) =
  capabilities_for_model ~provider:cfg.provider ~model_id:cfg.model_id

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

let validate_inference_contract ~capabilities (contract : inference_contract) =
  if modality_supported capabilities contract.modality then Ok ()
  else
    Error (Error.Config (InvalidConfig {
      field = "modality";
      detail =
        Printf.sprintf "Model '%s' for provider '%s' does not support modality '%s'"
          contract.model_id
          (provider_name contract.provider)
          (modality_to_string contract.modality);
    }))

let build_inference_contract ~provider ~model_id ~(capabilities : capabilities) =
  let contract = {
    provider;
    model_id;
    modality = modality_of_capabilities capabilities;
    task = task_of_model_id model_id;
  } in
  match validate_inference_contract ~capabilities contract with
  | Ok () -> contract
  | Error err ->
    invalid_arg
      ("BUG: inferred invalid inference contract: " ^ Error.to_string err)

let inference_contract_of_model_spec (spec : model_spec) =
  build_inference_contract
    ~provider:spec.provider
    ~model_id:spec.model_id
    ~capabilities:spec.capabilities

let inference_contract_of_config (cfg : config) =
  let capabilities = capabilities_for_config cfg in
  build_inference_contract
    ~provider:cfg.provider
    ~model_id:cfg.model_id
    ~capabilities

let model_spec_of_config (cfg : config) =
  let capabilities = capabilities_for_config cfg in
  let spec = {
    provider = cfg.provider;
    model_id = cfg.model_id;
    api_key_env = cfg.api_key_env;
    request_kind = request_kind cfg.provider;
    request_path = request_path cfg.provider;
    capabilities;
  } in
  let _ = inference_contract_of_model_spec spec in
  spec

let resolve (cfg : config) =
  match cfg.provider with
  | Local { base_url } ->
    Ok (base_url, "dummy", [("Content-Type", "application/json")])
  | Anthropic ->
    (match Sys.getenv_opt cfg.api_key_env with
     | Some key -> Ok ("https://api.anthropic.com", key,
       [("x-api-key", key);
        ("anthropic-version", "2023-06-01");
        ("Content-Type", "application/json")])
     | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))
  | OpenAICompat { base_url; auth_header; static_token; _ } ->
    (match static_token with
     | Some key when String.trim key <> "" ->
         let headers =
           match auth_header with
           | Some header -> [ (header, "Bearer " ^ key); ("Content-Type", "application/json") ]
           | None -> [ ("Content-Type", "application/json") ]
         in
         Ok (base_url, key, headers)
     | _ ->
         (match auth_header with
          | None ->
              Ok (base_url, "", [ ("Content-Type", "application/json") ])
          | Some header ->
              (match Sys.getenv_opt cfg.api_key_env with
               | Some key ->
                   Ok
                     ( base_url,
                       key,
                       [ (header, "Bearer " ^ key); ("Content-Type", "application/json") ] )
               | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))))
  | Custom_registered { name } ->
    (match find_provider name with
     | Some impl -> impl.resolve cfg
     | None -> Error (Error.Config (InvalidConfig {
         field = "provider";
         detail = Printf.sprintf "Custom provider '%s' not registered" name })))

let local_llm () = {
  provider = Local { base_url = Defaults.local_llm_url };
  model_id = "default";
  api_key_env = "DUMMY_KEY";
}

(* Backward-compatible aliases — use local_llm instead *)
let local_qwen () = local_llm ()
let local_mlx () = local_llm ()

let anthropic_sonnet () = {
  provider = Anthropic;
  model_id = "claude-sonnet-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

let anthropic_haiku () = {
  provider = Anthropic;
  model_id = "claude-haiku-4-5-20251001";
  api_key_env = "ANTHROPIC_API_KEY";
}

let anthropic_opus () = {
  provider = Anthropic;
  model_id = "claude-opus-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

let openrouter ?(model_id="anthropic/claude-sonnet-4-6") () = {
  provider = OpenAICompat {
    base_url = "https://openrouter.ai/api/v1";
    auth_header = Some "Authorization";
    path = "/chat/completions";
    static_token = None;
  };
  model_id;
  api_key_env = "OPENROUTER_API_KEY";
}

(* ── Cascade: multi-provider failover ──────────────────────────── *)

type cascade = {
  primary: config;
  fallbacks: config list;
}

let cascade ~primary ~fallbacks = { primary; fallbacks }

(* ── Pricing: per-model cost estimation ────────────────────────── *)

type pricing = {
  input_per_million: float;
  output_per_million: float;
  cache_write_multiplier: float;  (* cache creation tokens cost input_rate * this *)
  cache_read_multiplier: float;   (* cache read tokens cost input_rate * this *)
}

let zero_pricing =
  { input_per_million = 0.0; output_per_million = 0.0;
    cache_write_multiplier = 1.0; cache_read_multiplier = 1.0 }

let pricing_for_model_opt model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  (* Anthropic cache pricing: write = 1.25x input, read = 0.1x input.
     OpenAI/local: no cache pricing (multipliers are 1.0 and 1.0 for no-op). *)
  let anthropic_cache = (1.25, 0.1) in
  let no_cache = (1.0, 1.0) in
  let result =
    if string_contains ~needle:"opus-4-6" normalized then
      Some ((15.0, 75.0), anthropic_cache)
    else if string_contains ~needle:"opus-4-5" normalized then
      Some ((15.0, 75.0), anthropic_cache)
    else if string_contains ~needle:"sonnet-4-6" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"sonnet-4" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"haiku-4-5" normalized then
      Some ((0.8, 4.0), anthropic_cache)
    else if string_contains ~needle:"claude-3-7-sonnet" normalized then
      Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"gpt-4o-mini" normalized then
      Some ((0.15, 0.6), no_cache)
    else if string_contains ~needle:"gpt-4o" normalized then
      Some ((2.5, 10.0), no_cache)
    else if string_contains ~needle:"gpt-4.1" normalized then
      Some ((2.0, 8.0), no_cache)
    else if string_contains ~needle:"o3-mini" normalized then
      Some ((1.1, 4.4), no_cache)
    else
      None
  in
  match result with
  | Some ((input_per_million, output_per_million), (cw, cr)) ->
    Some { input_per_million; output_per_million;
           cache_write_multiplier = cw; cache_read_multiplier = cr }
  | None -> None

let pricing_for_model model_id =
  Option.value ~default:zero_pricing (pricing_for_model_opt model_id)

let pricing_for_provider ~(provider : provider) ~(model_id : string) =
  match provider with
  | Local _ -> zero_pricing
  | _ -> pricing_for_model model_id

let estimate_cost ~(pricing : pricing)
    ~input_tokens ~output_tokens
    ?(cache_creation_input_tokens=0) ?(cache_read_input_tokens=0) () =
  (* Regular input tokens (excluding cache tokens — those are billed separately) *)
  let regular_input = input_tokens - cache_creation_input_tokens - cache_read_input_tokens in
  let regular_input = max 0 regular_input in
  let rate = pricing.input_per_million /. 1_000_000.0 in
  let input_cost = Float.of_int regular_input *. rate in
  let cache_write_cost =
    Float.of_int cache_creation_input_tokens *. rate *. pricing.cache_write_multiplier in
  let cache_read_cost =
    Float.of_int cache_read_input_tokens *. rate *. pricing.cache_read_multiplier in
  let output_cost = Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0 in
  input_cost +. cache_write_cost +. cache_read_cost +. output_cost

(* ── Convenience: create config for a Custom_registered provider ── *)

let custom_provider ~name ?(model_id="custom") ?(api_key_env="DUMMY_KEY") () = {
  provider = Custom_registered { name };
  model_id;
  api_key_env;
}

(** Well-known env var names per provider kind.
    Used as fallback when [Provider_config.t.api_key] is empty. *)
let default_api_key_env_of_kind
    (kind : Llm_provider.Provider_config.provider_kind) : string =
  match kind with
  | Anthropic -> "ANTHROPIC_API_KEY"
  | Gemini -> "GEMINI_API_KEY"
  | Glm -> "ZAI_API_KEY"
  | OpenAI_compat | Ollama | Claude_code -> ""

(** Convert a [Provider_config.t] (from Cascade_config) into a
    [Provider.config] (for Agent Builder).  Keeps the conversion
    internal to OAS so consumers don't need their own adapters.

    When [api_key] is empty, falls back to the well-known env var
    name for the provider kind (e.g. [ANTHROPIC_API_KEY]). *)
let config_of_provider_config (pc : Llm_provider.Provider_config.t) : config =
  let provider = match pc.kind with
    | Anthropic -> Anthropic
    | Gemini ->
      OpenAICompat { base_url = pc.base_url; auth_header = None;
                     path = pc.request_path; static_token = None }
    | Glm ->
      OpenAICompat { base_url = pc.base_url; auth_header = None;
                     path = pc.request_path; static_token = None }
    | OpenAI_compat | Ollama ->
      if Llm_provider.Provider_config.is_local pc
      then Local { base_url = pc.base_url }
      else OpenAICompat { base_url = pc.base_url; auth_header = None;
                          path = pc.request_path; static_token = None }
    | Claude_code ->
      OpenAICompat { base_url = pc.base_url; auth_header = None;
                     path = pc.request_path; static_token = None }
  in
  let api_key_env =
    if pc.api_key <> "" then pc.api_key
    else default_api_key_env_of_kind pc.kind
  in
  { provider; model_id = pc.model_id; api_key_env }
