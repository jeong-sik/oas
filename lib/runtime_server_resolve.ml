open Runtime

type execution_resolution = {
  selected_provider: string;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  provider_cfg: Provider.config option;
}

let provider_runtime_name selected (cfg : Provider.config option) =
  match cfg with
  | None -> selected
  | Some cfg -> (
      match cfg.provider with
      | Provider.Local _ -> "local"
      | Provider.Anthropic -> "anthropic"
      | Provider.OpenAICompat _ -> "openai-compat"
      | Provider.Custom_registered { name } -> "custom:" ^ name)

let resolve_provider ?provider ?model () =
  let selected =
    match provider with
    | Some value when String.trim value <> "" -> String.lowercase_ascii (String.trim value)
    | _ -> Defaults.fallback_provider
  in
  let base =
    match selected with
    | "mock" | "echo" -> None
    | "local-qwen" -> Some (Provider.local_qwen ())
    | "local-mlx" -> Some (Provider.local_mlx ())
    | "sonnet" -> Some (Provider.anthropic_sonnet ())
    | "haiku" -> Some (Provider.anthropic_haiku ())
    | "opus" -> Some (Provider.anthropic_opus ())
    | "openrouter" -> Some (Provider.openrouter ())
    | other ->
        Some
          {
            Provider.provider = Local { base_url = Defaults.local_qwen_url };
            model_id = other;
            api_key_env = "LOCAL_LLM_KEY";
          }
  in
  match base with
  | None -> None
  | Some cfg ->
      Some
        {
          cfg with
          model_id =
            (match model with Some value when String.trim value <> "" -> value | _ -> cfg.model_id);
        }

let resolve_execution (session : session) (detail : spawn_agent_request) =
  let first_some = Util.first_some in
  let selected_provider =
    match detail.provider with
    | Some value when String.trim value <> "" ->
        String.lowercase_ascii (String.trim value)
    | _ -> (
        match session.provider with
        | Some value when String.trim value <> "" ->
            String.lowercase_ascii (String.trim value)
        | _ -> Defaults.fallback_provider)
  in
  let requested_model =
    match detail.model with
    | Some value when String.trim value <> "" -> Some (String.trim value)
    | _ -> None
  in
  match selected_provider with
  | "mock" | "echo" ->
      {
        selected_provider;
        requested_model;
        resolved_provider = Some selected_provider;
        resolved_model = first_some requested_model session.model;
        provider_cfg = None;
      }
  | _ ->
      let provider_cfg =
        resolve_provider ~provider:selected_provider
          ?model:(first_some requested_model session.model) ()
      in
      {
        selected_provider;
        requested_model;
        resolved_provider =
          Some (provider_runtime_name selected_provider provider_cfg);
        resolved_model =
          (match provider_cfg with
          | Some cfg -> Some cfg.model_id
          | None -> first_some requested_model session.model);
        provider_cfg;
      }
