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
    Option.value
      ~default:selected
      (Provider_runtime_binding.provider_id_of_legacy_config cfg)
;;

let runtime_valid_provider_detail () =
  let names =
    Provider_runtime_binding.all ()
    |> List.concat_map (fun (binding : Provider_runtime_binding.t) ->
      binding.id :: binding.aliases)
    |> List.sort_uniq String.compare
  in
  let names = Provider_runtime_binding.known_aliases () @ ("local" :: names) in
  let names =
    if Defaults.allow_test_providers () then "mock" :: "echo" :: names else names
  in
  String.concat ", " (List.sort_uniq String.compare names)
;;

let request_path_or_default (binding : Provider_runtime_binding.t) =
  let request_path = String.trim binding.request_path in
  if request_path <> ""
  then request_path
  else Llm_provider.Provider_config.request_path_default_for_kind binding.kind
;;

let legacy_provider_config_of_binding ?model (binding : Provider_runtime_binding.t) =
  let model_id = Provider_runtime_binding.resolve_model binding ~requested_model:model in
  match binding.kind with
  | Llm_provider.Provider_config.Anthropic ->
    { Provider.provider = Provider.Anthropic
    ; model_id
    ; api_key_env = binding.api_key_env
    }
  | Llm_provider.Provider_config.OpenAI_compat ->
    { Provider.provider =
        Provider.OpenAICompat
          { base_url = binding.base_url
          ; auth_header =
              (if String.trim binding.api_key_env = "" then None else Some "Authorization")
          ; path = request_path_or_default binding
          ; static_token = None
          }
    ; model_id
    ; api_key_env = binding.api_key_env
    }
  | _ ->
    { Provider.provider = Provider.Custom_registered { name = binding.id }
    ; model_id
    ; api_key_env = binding.api_key_env
    }
;;

let resolve_from_runtime_binding ~provider_name ?model () =
  match Provider_runtime_binding.find provider_name with
  | Some binding -> Ok (Some (legacy_provider_config_of_binding ?model binding))
  | None ->
    let resolved_model = Model_registry.resolve_model_id provider_name in
    if not (String.equal resolved_model provider_name)
    then (
      match Provider_runtime_binding.find "anthropic" with
      | Some binding ->
        Ok (Some (legacy_provider_config_of_binding ~model:resolved_model binding))
      | None ->
        unsupported_provider
          "provider alias resolved to an Anthropic model but provider catalog has no \
           \"agent_llm_a\" entry")
    else
      unsupported_provider
        (Printf.sprintf
           "unknown provider %S; valid: %s"
           provider_name
           (runtime_valid_provider_detail ()))
;;

let resolve_provider ?provider ?model () =
  let selected =
    match provider with
    | Some value when String.trim value <> "" ->
      String.lowercase_ascii (String.trim value)
    | _ -> Defaults.fallback_provider
  in
  let base =
    match selected with
    | "mock" | "echo" ->
      let* () = ensure_test_provider_enabled selected in
      Ok None
    | "local" -> Ok (Some (Provider.local_llm ()))
    | other -> resolve_from_runtime_binding ~provider_name:other ?model ()
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
