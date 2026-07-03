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
  | Some cfg -> Provider_runtime_binding.provider_id_of_config cfg
;;

let valid_provider_detail () =
  let names = "local" :: Provider_runtime_binding.known_labels () in
  let names =
    if Defaults.allow_test_providers () then "mock" :: "echo" :: names else names
  in
  String.concat ", " (List.sort_uniq String.compare names)
;;

let provider_config_of_binding ~model_id (binding : Provider_runtime_binding.t) =
  { Provider.provider = Provider.Custom_registered { name = binding.id }
  ; model_id
  ; api_key_env = binding.api_key_env
  }
;;

let model_for_provider provider model ~default =
  match Util.trim_non_empty_opt model with
  | Some requested ->
    (match provider with
     | Provider.Anthropic -> Model_registry.resolve_model_id requested
     | Provider.Local _ | Provider.OpenAICompat _ | Provider.Custom_registered _ ->
       requested)
  | None -> default
;;

let with_requested_model ?model (cfg : Provider.config) =
  { cfg with
    Provider.model_id = model_for_provider cfg.provider model ~default:cfg.model_id
  }
;;

let builtin_claude_config ?model () =
  let model_id =
    model_for_provider
      Provider.Anthropic
      model
      ~default:(Model_registry.default_model_id_value ())
  in
  { Provider.provider = Provider.Anthropic
  ; model_id
  ; api_key_env =
      Provider.default_api_key_env_of_kind Llm_provider.Provider_config.Anthropic
  }
;;

let resolve_from_bindings ~provider_name ?model () =
  match Provider_runtime_binding.find provider_name with
  | Some binding ->
    let model_id =
      Provider_runtime_binding.resolve_model binding ~requested_model:model
    in
    Ok (Some (provider_config_of_binding ~model_id binding))
  | None ->
    let resolved_model = Model_registry.resolve_model_id provider_name in
    if not (String.equal resolved_model provider_name)
    then (
      let model_id =
        model_for_provider Provider.Anthropic model ~default:resolved_model
      in
      match Provider_runtime_binding.find "claude" with
      | Some binding -> Ok (Some (provider_config_of_binding ~model_id binding))
      | None ->
        unsupported_provider
          "provider alias resolved to an Anthropic model but provider bindings have no \
           \"claude\" entry")
    else
      unsupported_provider
        (Printf.sprintf
           "unknown provider %S; valid: %s"
           provider_name
           (valid_provider_detail ()))
;;

let resolve_selected_provider ~implicit_fallback selected ?model () =
  match selected with
  | "mock" | "echo" ->
    let* () = ensure_test_provider_enabled selected in
    Ok None
  | "local" -> Ok (Some (with_requested_model ?model (Provider.local_llm ())))
  | ("claude" | "anthropic") when implicit_fallback ->
    Ok (Some (builtin_claude_config ?model ()))
  | other -> resolve_from_bindings ~provider_name:other ?model ()
;;

let resolve_provider ?provider ?model () =
  let selected, implicit_fallback =
    match provider with
    | Some value when String.trim value <> "" ->
      String.lowercase_ascii (String.trim value), false
    | _ -> Defaults.resolve_fallback_provider (), true
  in
  let base = resolve_selected_provider ~implicit_fallback selected ?model () in
  match base with
  | Error _ as e -> e
  | Ok _ as ok -> ok
;;

let resolve_execution (session : session) (detail : spawn_agent_request) =
  let first_some = Util.first_some in
  let selected_provider, implicit_fallback =
    match detail.provider with
    | Some value when String.trim value <> "" ->
      String.lowercase_ascii (String.trim value), false
    | _ ->
      (match Util.trim_non_empty_opt session.provider with
       | Some value -> String.lowercase_ascii value, false
       | None -> Defaults.resolve_fallback_provider (), true)
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
       resolve_selected_provider
         ~implicit_fallback
         selected_provider
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
