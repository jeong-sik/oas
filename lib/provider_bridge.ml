(** Bridge between legacy [Provider.config] and [Llm_provider.Provider_config.t].

    Allows agent_sdk code and consumers to convert from the old 5-variant
    provider system to the new 2-kind standalone config.

    @since 0.53.0 *)

let is_glm_model_or_alias model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  Llm_provider.Zai_catalog.is_glm_model_id normalized
  ||
  match normalized with
  | "auto"
  | "flash"
  | "turbo"
  | "vision"
  | "v"
  | "vision-flash"
  | "vf"
  | "air"
  | "ocr" -> true
  | _ -> false

(** Resolve "auto" / aliases to concrete model IDs for legacy Provider.config
    input. Previously delegated to {!Llm_provider.Cascade_model_resolve}, now
    inlined here because cascade orchestration no longer lives in OAS.

    Local providers use {!Llm_provider.Discovery.first_discovered_model_id}
    for "auto"; cloud providers use environment-variable defaults. *)
let env_or default var =
  match Sys.getenv_opt var with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> default

let resolve_glm_model_id model_id =
  Llm_provider.Zai_catalog.resolve_glm_alias
    ~default_model:(env_or "glm-5.1" "ZAI_DEFAULT_MODEL")
    model_id

let resolve_glm_coding_model_id model_id =
  Llm_provider.Zai_catalog.resolve_glm_coding_alias
    ~default_model:(env_or "glm-4.7" "ZAI_CODING_DEFAULT_MODEL")
    model_id

let resolve_auto_model_id provider_name model_id =
  match provider_name with
  | "llama" | "ollama" ->
    if model_id = "auto" then
      match Llm_provider.Discovery.first_discovered_model_id () with
      | Some id -> id
      | None -> env_or model_id "OLLAMA_DEFAULT_MODEL"
    else model_id
  | "glm" -> resolve_glm_model_id model_id
  | "glm-coding" -> resolve_glm_coding_model_id model_id
  | "gemini" ->
    if model_id = "auto" then env_or "gemini-2.5-flash" "GEMINI_DEFAULT_MODEL"
    else model_id
  | "claude" ->
    if model_id = "auto" then env_or "claude-sonnet-4-6-20250514" "ANTHROPIC_DEFAULT_MODEL"
    else model_id
  | "openai" ->
    if model_id = "auto" then env_or "gpt-4.1" "OPENAI_DEFAULT_MODEL"
    else model_id
  | "openrouter" ->
    if model_id = "auto" then env_or model_id "OPENROUTER_DEFAULT_MODEL"
    else model_id
  | _ -> model_id

let to_provider_config (legacy : Provider.config) : (Llm_provider.Provider_config.t, Error.sdk_error) result =
  match Provider.resolve legacy with
  | Error e -> Error e
  | Ok (base_url, api_key, headers) ->
      let m_lower = String.lowercase_ascii legacy.model_id in
      let is_gemini_model =
        String.length m_lower >= 6 && String.sub m_lower 0 6 = "gemini"
      in
      let is_glm_model =
        is_glm_model_or_alias m_lower
      in
      let is_zai_provider =
        Llm_provider.Zai_catalog.is_zai_base_url base_url
      in
      let kind = match Provider.request_kind legacy.provider with
        | Provider.Anthropic_messages ->
            Llm_provider.Provider_config.Anthropic
        | Provider.Openai_chat_completions
        | Provider.Custom _ ->
            if is_gemini_model then Llm_provider.Provider_config.Gemini
            else if is_zai_provider && is_glm_model then Llm_provider.Provider_config.Glm
            else Llm_provider.Provider_config.OpenAI_compat
      in
      let request_path = Provider.request_path legacy.provider in
      let provider_name = match kind with
        | Llm_provider.Provider_config.Anthropic -> "claude"
        | Claude_code -> "claude"
        | Gemini -> "gemini"
        | Glm ->
            if Llm_provider.Zai_catalog.is_coding_base_url base_url then
              "glm-coding"
            else
              "glm"
        | OpenAI_compat -> "llama"
        | Ollama -> "ollama"
        | Gemini_cli -> "gemini_cli"
        | Codex_cli -> "codex_cli"
      in
      let resolved_model_id =
        resolve_auto_model_id provider_name legacy.model_id
      in
      Ok (Llm_provider.Provider_config.make
            ~kind
            ~model_id:resolved_model_id
            ~base_url
            ~api_key
            ~headers
            ~request_path
            ())

