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
  ; task : string option
  }

type model_spec =
  { provider : provider
  ; model_id : string
  ; api_key_env : string
  ; request_kind : request_kind
  ; request_path : string
  ; capabilities : capabilities
  }

let string_contains = Util.string_contains
let contains_substring_ci = Util.contains_substring_ci

(** Check if a model needs extended OpenAI capabilities
    (reasoning, top_k, min_p). Currently triggers on qwen family models. *)
let needs_extended_capabilities model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  string_contains ~needle:"qwen" normalized
;;

let default_openai_compat_capabilities model_id =
  if needs_extended_capabilities model_id
  then openai_chat_extended_capabilities
  else openai_chat_capabilities
;;

let uses_native_glm_capabilities ~base_url ~model_id =
  Llm_provider.Zai_catalog.is_zai_base_url base_url
  && Llm_provider.Zai_catalog.is_glm_model_id model_id
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

let task_of_model_id model_id =
  let has needle = contains_substring_ci ~haystack:model_id ~needle in
  if has "whisper" || has "transcribe" || has "transcription" || has "stt"
  then Some "transcription"
  else if has "tts" || has "text-to-speech" || has "voice"
  then Some "speech"
  else if
    has "imagegen"
    || has "image-gen"
    || has "gpt-image"
    || has "cogview"
    || has "glm-image"
    || has "seedream"
    || has "flux"
  then Some "image_generation"
  else if
    has "video-gen"
    || has "veo"
    || has "kling"
    || has "sora"
    || has "wan"
    || has "cogvideox"
    || has "vidu"
  then Some "video_generation"
  else None
;;

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
      (match Util.trim_non_empty_opt (Sys.getenv_opt env_name) with
       | Some value -> Some (env_name, value)
       | None -> loop rest)
  in
  loop env_names
;;

let kimi_direct_base_url () =
  match Util.trim_non_empty_opt (Sys.getenv_opt "KIMI_BASE_URL") with
  | Some url -> url
  | None -> "https://api.kimi.com/coding"
;;

let kimi_direct_request_path = "/v1/messages"

let kimi_direct_headers key =
  [ "Content-Type", "application/json"
  ; "x-api-key", key
  ; "anthropic-version", "2023-06-01"
  ]
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
  | Local _ ->
    (* Local (llama-server) uses OpenAI-compatible API.
         Resolve capabilities by model_id, fall back to openai_chat. *)
    (match Llm_provider.Capabilities.for_model_id model_id with
     | Some caps -> caps
     | None -> openai_chat_capabilities)
  | OpenAICompat { base_url; _ } ->
    if
      Llm_provider.Zai_catalog.is_glm_model_id model_id
      && not (uses_native_glm_capabilities ~base_url ~model_id)
    then default_openai_compat_capabilities model_id
    else (
      match Llm_provider.Capabilities.for_model_id model_id with
      | Some caps -> caps
      | None -> default_openai_compat_capabilities model_id)
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
    ; task = task_of_model_id model_id
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
  | Local { base_url } -> Ok (base_url, "dummy", [ "Content-Type", "application/json" ])
  | Anthropic ->
    (match Sys.getenv_opt cfg.api_key_env with
     | Some key ->
       Ok
         ( "https://api.anthropic.com"
         , key
         , [ "x-api-key", key
           ; "anthropic-version", "2023-06-01"
           ; "Content-Type", "application/json"
           ] )
     | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))
  | OpenAICompat { base_url; auth_header; static_token; _ } ->
    (match static_token with
     | Some key when String.trim key <> "" ->
       let headers =
         match auth_header with
         | Some header -> [ header, "Bearer " ^ key; "Content-Type", "application/json" ]
         | None -> [ "Content-Type", "application/json" ]
       in
       Ok (base_url, key, headers)
     | _ ->
       (match auth_header with
        | None -> Ok (base_url, "", [ "Content-Type", "application/json" ])
        | Some header ->
          (match Sys.getenv_opt cfg.api_key_env with
           | Some key ->
             Ok
               ( base_url
               , key
               , [ header, "Bearer " ^ key; "Content-Type", "application/json" ] )
           | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))))
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
  { provider = Local { base_url = Defaults.local_llm_url }
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

type pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float (* cache creation tokens cost input_rate * this *)
  ; cache_read_multiplier : float (* cache read tokens cost input_rate * this *)
  }

let zero_pricing =
  { input_per_million = 0.0
  ; output_per_million = 0.0
  ; cache_write_multiplier = 1.0
  ; cache_read_multiplier = 1.0
  }
;;

let pricing_for_model_opt model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  (* Anthropic cache pricing: write = 1.25x input, read = 0.1x input.
     Newer OpenAI text models expose cached input at 0.1x input.
     Local/free models keep no-op cache multipliers. *)
  let anthropic_cache = 1.25, 0.1 in
  let openai_cached_input = 1.0, 0.1 in
  let no_cache = 1.0, 1.0 in
  let result =
    if string_contains ~needle:"opus-4-6" normalized
    then Some ((15.0, 75.0), anthropic_cache)
    else if string_contains ~needle:"opus-4-5" normalized
    then Some ((15.0, 75.0), anthropic_cache)
    else if string_contains ~needle:"sonnet-4-6" normalized
    then Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"sonnet-4" normalized
    then Some ((3.0, 15.0), anthropic_cache)
    else if string_contains ~needle:"haiku-4-5" normalized
    then Some ((0.8, 4.0), anthropic_cache)
    else if string_contains ~needle:"claude-3-7-sonnet" normalized
    then
      Some ((3.0, 15.0), anthropic_cache)
      (* OpenAI API text-token pricing, confirmed from official model docs
       2026-04-25. GPT-5.3-Codex-Spark is intentionally not covered here:
       its Codex rate card labels it research preview with non-final rates. *)
    else if string_contains ~needle:"gpt-5.3-codex-spark" normalized
    then None
    else if string_contains ~needle:"gpt-5.5" normalized
    then Some ((5.0, 30.0), openai_cached_input)
    else if string_contains ~needle:"gpt-5.4-mini" normalized
    then Some ((0.75, 4.5), openai_cached_input)
    else if string_contains ~needle:"gpt-5.4" normalized
    then Some ((2.5, 15.0), openai_cached_input)
    else if string_contains ~needle:"gpt-5.3-codex" normalized
    then Some ((1.75, 14.0), openai_cached_input)
    else if string_contains ~needle:"gpt-5.2" normalized
    then Some ((1.75, 14.0), openai_cached_input)
    else if string_contains ~needle:"gpt-4o-mini" normalized
    then Some ((0.15, 0.6), no_cache)
    else if string_contains ~needle:"gpt-4o" normalized
    then Some ((2.5, 10.0), no_cache)
    else if string_contains ~needle:"gpt-4.1" normalized
    then Some ((2.0, 8.0), no_cache)
    else if string_contains ~needle:"o3-mini" normalized
    then Some ((1.1, 4.4), no_cache)
    else None
  in
  match result with
  | Some ((input_per_million, output_per_million), (cw, cr)) ->
    Some
      { input_per_million
      ; output_per_million
      ; cache_write_multiplier = cw
      ; cache_read_multiplier = cr
      }
  | None -> None
;;

let pricing_for_model model_id =
  Option.value ~default:zero_pricing (pricing_for_model_opt model_id)
;;

let pricing_for_provider ~(provider : provider) ~(model_id : string) =
  match provider with
  | Local _ -> zero_pricing
  | _ -> pricing_for_model model_id
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
  let has_key = pc.api_key <> "" in
  let auth_header = if has_key then Some "Authorization" else None in
  let static_token = if has_key then Some pc.api_key else None in
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
    | Claude_code ->
      OpenAICompat
        { base_url = pc.base_url; auth_header; path = pc.request_path; static_token }
    | Gemini_cli | Kimi_cli | Codex_cli ->
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
  let build ~kind ~resolved_base_url ~api_key ~headers ~request_path ~model_id =
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
         ?thinking_budget:cfg.thinking_budget
         ?tool_choice:cfg.tool_choice
         ?system_prompt:cfg.system_prompt
         ~disable_parallel_tool_use:cfg.disable_parallel_tool_use
         ~response_format:cfg.response_format
         ~cache_system_prompt:cfg.cache_system_prompt
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
                  ~model_id:p.model_id)
           | None ->
             let api_key =
               if entry.defaults.api_key_env = ""
               then ""
               else (
                 match Sys.getenv_opt entry.defaults.api_key_env with
                 | Some k -> k
                 | None -> "")
             in
             build
               ~kind:entry.defaults.kind
               ~resolved_base_url:entry.defaults.base_url
               ~api_key
               ~headers:[]
               ~request_path:entry.defaults.request_path
               ~model_id:p.model_id))
     | Anthropic | Local _ | OpenAICompat _ ->
       (match resolve p with
        | Error e -> Error e
        | Ok (url, api_key, headers) ->
          let kind : Llm_provider.Provider_config.provider_kind =
            match p.provider with
            | Anthropic -> Anthropic
            | Local _ | OpenAICompat _ -> OpenAI_compat
            | Custom_registered _ -> assert false
          in
          let sanitized_api_key =
            match p.provider with
            | Local _ -> ""
            | Anthropic | OpenAICompat _ -> api_key
            | Custom_registered _ -> assert false
          in
          build
            ~kind
            ~resolved_base_url:url
            ~api_key:sanitized_api_key
            ~headers
            ~request_path:(request_path p.provider)
            ~model_id:p.model_id))
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
         ~model_id:(Types.model_to_string cfg.model))
;;
