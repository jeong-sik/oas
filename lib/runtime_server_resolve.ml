open Runtime

let ( let* ) = Result.bind

type execution_resolution = {
  selected_provider: string;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  provider_cfg: Provider.config option;
}

let unsupported_provider detail =
  Error (Error.Config (Error.UnsupportedProvider { detail }))

let ensure_test_provider_enabled selected =
  if Defaults.allow_test_providers () then Ok ()
  else
    unsupported_provider
      (Printf.sprintf
         "provider %S is test-only; set OAS_ALLOW_TEST_PROVIDERS=1 to enable it explicitly"
         selected)

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
    | "mock" | "echo" ->
        let* () = ensure_test_provider_enabled selected in
        Ok None
    | "local" -> Ok (Some (Provider.local_llm ()))
    | "sonnet" -> Ok (Some (Provider.anthropic_sonnet ()))
    | "haiku" -> Ok (Some (Provider.anthropic_haiku ()))
    | "opus" -> Ok (Some (Provider.anthropic_opus ()))
    | "openrouter" -> Ok (Some (Provider.openrouter ()))
    | other ->
        unsupported_provider
          (Printf.sprintf
             "unknown provider %S; valid: local, sonnet, haiku, opus, openrouter%s"
             other
             (if Defaults.allow_test_providers ()
              then ", mock, echo"
              else ""))
  in
  match base with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some cfg) ->
      Ok (Some
        {
          cfg with
          model_id =
            (match model with Some value when String.trim value <> "" -> value | _ -> cfg.model_id);
        })

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
      let* () = ensure_test_provider_enabled selected_provider in
      Ok {
        selected_provider;
        requested_model;
        resolved_provider = Some selected_provider;
        resolved_model = first_some requested_model session.model;
        provider_cfg = None;
      }
  | _ ->
      match resolve_provider ~provider:selected_provider
              ?model:(first_some requested_model session.model) () with
      | Error _ as e -> e
      | Ok provider_cfg ->
        Ok {
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
