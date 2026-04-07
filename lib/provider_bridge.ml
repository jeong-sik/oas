(** Bridge between legacy [Provider.config] and [Llm_provider.Provider_config.t].

    Allows agent_sdk code and consumers to convert from the old 5-variant
    provider system to the new 2-kind standalone config.

    @since 0.53.0 *)

let to_provider_config (legacy : Provider.config) : (Llm_provider.Provider_config.t, Error.sdk_error) result =
  match Provider.resolve legacy with
  | Error e -> Error e
  | Ok (base_url, api_key, headers) ->
      let m_lower = String.lowercase_ascii legacy.model_id in
      let is_gemini_model =
        String.length m_lower >= 6 && String.sub m_lower 0 6 = "gemini"
      in
      let is_glm_model =
        String.length m_lower >= 3 && String.sub m_lower 0 3 = "glm"
      in
      let kind = match Provider.request_kind legacy.provider with
        | Provider.Anthropic_messages ->
            Llm_provider.Provider_config.Anthropic
        | Provider.Openai_chat_completions
        | Provider.Custom _ ->
            if is_gemini_model then Llm_provider.Provider_config.Gemini
            else if is_glm_model then Llm_provider.Provider_config.Glm
            else Llm_provider.Provider_config.OpenAI_compat
      in
      let request_path = Provider.request_path legacy.provider in
      let provider_name = match kind with
        | Llm_provider.Provider_config.Anthropic -> "claude"
        | Claude_code -> "claude"
        | Gemini -> "gemini"
        | Glm -> "glm"
        | OpenAI_compat -> "llama"
        | Ollama -> "ollama"
      in
      let resolved_model_id =
        Llm_provider.Cascade_model_resolve.resolve_auto_model_id
          provider_name legacy.model_id
      in
      Ok (Llm_provider.Provider_config.make
            ~kind
            ~model_id:resolved_model_id
            ~base_url
            ~api_key
            ~headers
            ~request_path
            ())

(** Convert a legacy [Provider.cascade] to [Llm_provider.Complete.cascade].
    Returns [Error] if any provider in the cascade fails to resolve. *)
let cascade_to_provider_config (legacy : Provider.cascade) :
    (Llm_provider.Complete.cascade, Error.sdk_error) result =
  match to_provider_config legacy.primary with
  | Error e -> Error e
  | Ok primary ->
      let rec convert_fallbacks acc = function
        | [] -> Ok (List.rev acc)
        | fb :: rest ->
            match to_provider_config fb with
            | Error e -> Error e
            | Ok cfg -> convert_fallbacks (cfg :: acc) rest
      in
      match convert_fallbacks [] legacy.fallbacks with
      | Error e -> Error e
      | Ok fallbacks ->
          Ok { Llm_provider.Complete.primary; fallbacks }
