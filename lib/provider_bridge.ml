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

let is_kimi_coding_base_url base_url =
  match Uri.of_string base_url with
  | exception _ -> false
  | uri ->
      let host_matches =
        match Uri.host uri with
        | Some host -> String.lowercase_ascii host = "api.kimi.com"
        | None -> false
      in
      let path = Uri.path uri |> String.lowercase_ascii in
      host_matches
      && (path = "/coding" || path = "/coding/"
         || Util.string_contains ~needle:"/coding/" path)

(** Resolve "auto" / aliases to concrete model IDs for legacy Provider.config
    input. Inlined here because higher-level routing lives outside OAS.

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
    ~default_model:(env_or "glm-5.1" "ZAI_CODING_DEFAULT_MODEL")
    model_id

(** Resolve "auto" / aliases to concrete model IDs, dispatched on the typed
    {!Llm_provider.Provider_config.provider_kind} instead of a stringified name.

    Local providers consult {!Llm_provider.Discovery.first_discovered_model_id}
    for "auto"; cloud providers fall back to environment-variable defaults.

    Parse, don't validate: callers hand in the concrete variant so dead
    branches ([openai], [openrouter] in the pre-typed version) cannot exist. *)
let resolve_auto_model_id
    ~base_url
    (kind : Llm_provider.Provider_config.provider_kind)
    model_id =
  let open Llm_provider.Provider_config in
  match kind with
  | Ollama | OpenAI_compat ->
      (* Local llama-server and OpenAI-compatible endpoints share the
         "auto" -> discovery -> OLLAMA_DEFAULT_MODEL fallback. Cloud-only
         OpenAI-compatible backends still traverse this branch. *)
      if model_id = "auto" then
        match Llm_provider.Discovery.first_discovered_model_id () with
        | Some id -> id
        | None -> env_or model_id "OLLAMA_DEFAULT_MODEL"
      else model_id
  | Glm ->
      if Llm_provider.Zai_catalog.is_coding_base_url base_url then
        resolve_glm_coding_model_id model_id
      else
        resolve_glm_model_id model_id
  | Gemini ->
      if model_id = "auto" then
        env_or "gemini-2.5-flash" "GEMINI_DEFAULT_MODEL"
      else model_id
  | Kimi ->
      if model_id = "auto" then
        env_or "kimi-for-coding" "KIMI_DEFAULT_MODEL"
      else model_id
  | Anthropic | Claude_code ->
      if model_id = "auto" then
        env_or "claude-sonnet-4-6-20250514" "ANTHROPIC_DEFAULT_MODEL"
      else model_id
  | Gemini_cli | Kimi_cli | Codex_cli -> model_id

let to_provider_config (legacy : Provider.config) :
    (Llm_provider.Provider_config.t, Error.sdk_error) result =
  match Provider.resolve legacy with
  | Error e -> Error e
  | Ok (base_url, api_key, headers) ->
      let m_lower = String.lowercase_ascii legacy.model_id in
      let is_gemini_model =
        String.length m_lower >= 6 && String.sub m_lower 0 6 = "gemini"
      in
      let is_glm_model = is_glm_model_or_alias m_lower in
      let is_zai_provider = Llm_provider.Zai_catalog.is_zai_base_url base_url in
      let is_kimi_provider = is_kimi_coding_base_url base_url in
      let kind =
        match Provider.request_kind legacy.provider with
        | Provider.Anthropic_messages ->
            if is_kimi_provider then Llm_provider.Provider_config.Kimi
            else Llm_provider.Provider_config.Anthropic
        | Provider.Openai_chat_completions
        | Provider.Custom _ ->
            if is_gemini_model then Llm_provider.Provider_config.Gemini
            else if is_zai_provider && is_glm_model then
              Llm_provider.Provider_config.Glm
            else
              Llm_provider.Provider_config.OpenAI_compat
      in
      let request_path = Provider.request_path legacy.provider in
      let resolved_model_id =
        resolve_auto_model_id ~base_url kind legacy.model_id
      in
      Ok
        (Llm_provider.Provider_config.make ~kind ~model_id:resolved_model_id
           ~base_url ~api_key ~headers ~request_path ())
