open Runtime
open Result_syntax

type execution_resolution =
  { selected_provider : string
  ; requested_model : string option
  ; resolved_provider : string option
  ; resolved_model : string option
  ; provider_cfg : Provider.config option
  }

let unsupported_provider detail =
  Error (Error.Config (Error.UnsupportedProvider { detail }))
;;

let ensure_test_provider_enabled selected =
  if Defaults.allow_test_providers ()
  then Ok ()
  else
    unsupported_provider
      (Printf.sprintf
         "provider %S is test-only; set OAS_ALLOW_TEST_PROVIDERS=1 to enable it \
          explicitly"
         selected)
;;

let provider_runtime_name selected (cfg : Provider.config option) =
  match cfg with
  | None -> selected
  | Some cfg ->
    (match cfg.provider with
     | Provider.Local _ -> "local"
     | Provider.Provider_a -> "provider_a"
     | Provider.OpenAICompat _ -> "provider_d-compat"
     | Provider.Custom_registered { name } -> "custom:" ^ name)
;;

let registry_valid_provider_detail registry =
  let names =
    Llm_provider.Provider_registry.all registry
    |> List.map (fun (entry : Llm_provider.Provider_registry.entry) -> entry.name)
    |> List.sort_uniq String.compare
  in
  let names = "local" :: names in
  let names =
    if Defaults.allow_test_providers () then "mock" :: "echo" :: names else names
  in
  String.concat ", " (List.sort_uniq String.compare names)
;;

let provider_config_of_registry_entry
      ~provider_name
      ~model_id
      (entry : Llm_provider.Provider_registry.entry)
  =
  match entry.defaults.kind with
  | Llm_provider.Provider_config.Provider_a ->
    { Provider.provider = Provider.Provider_a
    ; model_id
    ; api_key_env = entry.defaults.api_key_env
    }
  | Llm_provider.Provider_config.Provider_d_compat ->
    { Provider.provider =
        Provider.OpenAICompat
          { base_url = entry.defaults.base_url
          ; auth_header =
              (if String.trim entry.defaults.api_key_env = ""
               then None
               else Some "Authorization")
          ; path = entry.defaults.request_path
          ; static_token = None
          }
    ; model_id
    ; api_key_env = entry.defaults.api_key_env
    }
  | _ ->
    { Provider.provider = Provider.Custom_registered { name = provider_name }
    ; model_id
    ; api_key_env = entry.defaults.api_key_env
    }
;;

let catalog_default_model provider_name =
  match Llm_provider.Provider_catalog.global () with
  | None -> None
  | Some catalog ->
    Llm_provider.Provider_catalog.default_model_for_provider catalog provider_name
;;

let effective_model_id ~provider_name ~entry ?model () =
  match model with
  | Some value when String.trim value <> "" -> value
  | _ ->
    (match catalog_default_model provider_name with
     | Some model_id when String.trim model_id <> "" -> model_id
     | _ ->
       (match entry.Llm_provider.Provider_registry.defaults.kind with
        | Llm_provider.Provider_config.Ollama -> "default"
        | _ -> Model_registry.default_model_id))
;;

let resolve_from_registry registry ~provider_name ?model () =
  match Llm_provider.Provider_registry.find registry provider_name with
  | Some entry ->
    let model_id = effective_model_id ~provider_name ~entry ?model () in
    Ok (Some (provider_config_of_registry_entry ~provider_name ~model_id entry))
  | None ->
    let resolved_model = Model_registry.resolve_model_id provider_name in
    if not (String.equal resolved_model provider_name)
    then (
      match Llm_provider.Provider_registry.find registry "agent_llm_a" with
      | Some entry ->
        Ok
          (Some
             (provider_config_of_registry_entry
                ~provider_name:"agent_llm_a"
                ~model_id:resolved_model
                entry))
      | None ->
        unsupported_provider
          "provider alias resolved to an Provider_a model but provider catalog has no \
           \"agent_llm_a\" entry")
    else
      unsupported_provider
        (Printf.sprintf
           "unknown provider %S; valid: %s"
           provider_name
           (registry_valid_provider_detail registry))
;;

let resolve_provider ?provider ?model () =
  let selected =
    match provider with
    | Some value when String.trim value <> "" ->
      String.lowercase_ascii (String.trim value)
    | _ -> Defaults.fallback_provider
  in
  let registry = Llm_provider.Provider_registry.default () in
  let base =
    match selected with
    | "mock" | "echo" ->
      let* () = ensure_test_provider_enabled selected in
      Ok None
    | "local" -> Ok (Some (Provider.local_llm ()))
    | other -> resolve_from_registry registry ~provider_name:other ?model ()
  in
  match base with
  | Error _ as e -> e
  | Ok None -> Ok None
  | Ok (Some cfg) ->
    Ok
      (Some
         { cfg with
           model_id =
             (match model with
              | Some value when String.trim value <> "" -> value
              | _ -> cfg.model_id)
         })
;;

let resolve_execution (session : session) (detail : spawn_agent_request) =
  let first_some = Util.first_some in
  let selected_provider =
    match detail.provider with
    | Some value when String.trim value <> "" ->
      String.lowercase_ascii (String.trim value)
    | _ ->
      (match Util.trim_non_empty_opt session.provider with
       | Some value -> String.lowercase_ascii value
       | None -> Defaults.fallback_provider)
  in
  let requested_model = Util.trim_non_empty_opt detail.model in
  match selected_provider with
  | "mock" | "echo" ->
    let* () = ensure_test_provider_enabled selected_provider in
    Ok
      { selected_provider
      ; requested_model
      ; resolved_provider = Some selected_provider
      ; resolved_model = first_some requested_model session.model
      ; provider_cfg = None
      }
  | _ ->
    (match
       resolve_provider
         ~provider:selected_provider
         ?model:(first_some requested_model session.model)
         ()
     with
     | Error _ as e -> e
     | Ok provider_cfg ->
       Ok
         { selected_provider
         ; requested_model
         ; resolved_provider = Some (provider_runtime_name selected_provider provider_cfg)
         ; resolved_model =
             (match provider_cfg with
              | Some cfg -> Some cfg.model_id
              | None -> first_some requested_model session.model)
         ; provider_cfg
         })
;;
