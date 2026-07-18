open Agent_types

let sdk_error_of_http_error = Http_error_sdk.of_http_error

let notify_attribution callback attribution =
  Option.iter (fun notify -> notify attribution) callback
;;

let binding_identity_error ?on_provider_failure detail =
  let error = Error.Config (InvalidConfig { field = "model_id"; detail }) in
  let detailed = Provider_failure_attribution.of_provider_configuration_error error in
  notify_attribution on_provider_failure detailed.provider_failure;
  detailed.error
;;

let binding_identity_for_call agent provider_config =
  let transport =
    Binding_identity.transport_for_call ~injected:(Option.is_some agent.options.transport)
  in
  Binding_identity.of_provider_config ~transport provider_config
;;

let invalid_request message =
  Error.Api
    (Llm_provider.Retry.InvalidRequest
       { message; reason = Llm_provider.Retry.Unknown_invalid_request })
;;

let measurement_error ~binding = function
  | Llm_provider.Count_tokens_sync.Input_count_failed
      (Llm_provider.Input_token_count.Transport http_error) ->
    Provider_failure_attribution.of_http_error ~binding http_error
  | Llm_provider.Count_tokens_sync.Input_count_failed
      (Llm_provider.Input_token_count.Unsupported { protocol; model_id }) ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (invalid_request
         (Printf.sprintf
            "provider-native input measurement %s is unsupported for model %s"
            (Llm_provider.Input_token_count.show_protocol protocol)
            model_id))
  | Llm_provider.Count_tokens_sync.Input_count_failed
      (Llm_provider.Input_token_count.Invalid_response { protocol; model_id; detail }) ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (invalid_request
         (Printf.sprintf
            "invalid %s input measurement for model %s: %s"
            (Llm_provider.Input_token_count.show_protocol protocol)
            model_id
            detail))
  | Llm_provider.Count_tokens_sync.Output_token_resolution_failed
      Llm_provider.Types.Required_output_token_ceiling_missing ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (invalid_request "prepared request has no effective output-token ceiling")
  | Llm_provider.Count_tokens_sync.Invalid_completion_request detail ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (invalid_request ("invalid prepared completion request: " ^ detail))
;;

let fit_error ~binding = function
  | Llm_provider.Complete.Context_limit_unknown { model_id } ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (Error.Config
         (InvalidConfig
            { field = "max_context"
            ; detail = Printf.sprintf "model %s has no declared context limit" model_id
            }))
  | Llm_provider.Complete.Invalid_context_limit { model_id; max_context_tokens } ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (Error.Config
         (InvalidConfig
            { field = "max_context"
            ; detail =
                Printf.sprintf
                  "model %s declares invalid context limit %d"
                  model_id
                  max_context_tokens
            }))
  | Llm_provider.Complete.Output_reservation_unknown { model_id } ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (invalid_request
         (Printf.sprintf "model %s has no effective output-token reservation" model_id))
  | Llm_provider.Complete.Context_window_exceeded
      { input_tokens; reserved_output_tokens; max_context_tokens } ->
    Provider_failure_attribution.of_request_validation_error
      ~binding
      (Error.Api
         (Llm_provider.Retry.ContextOverflow
            { message =
                Printf.sprintf
                  "prepared request requires %d input + %d reserved output tokens, limit \
                   %d"
                  input_tokens
                  reserved_output_tokens
                  max_context_tokens
            ; limit = Some max_context_tokens
            }))
;;

let supports_native_request_measurement = function
  | Llm_provider.Provider_config.Anthropic -> true
  | Llm_provider.Provider_config.Kimi
  | Llm_provider.Provider_config.OpenAI_compat
  | Llm_provider.Provider_config.Ollama
  | Llm_provider.Provider_config.Gemini
  | Llm_provider.Provider_config.Glm
  | Llm_provider.Provider_config.DashScope -> false
;;

let enforce_context_fit agent (provider_config : Llm_provider.Provider_config.t) =
  match agent.context_fit_admission with
  | Disabled -> false
  | Enforce_when_supported -> supports_native_request_measurement provider_config.kind
;;

let finish_call ?on_provider_failure = function
  | Ok response ->
    notify_attribution on_provider_failure None;
    Ok response
  | Error (detailed : Provider_failure_attribution.detailed_error) ->
    notify_attribution on_provider_failure detailed.provider_failure;
    Error detailed.error
;;

let provider_config_for_turn ~turn_config agent =
  match agent.provider_config with
  | Some provider_config ->
    Ok (Provider.provider_config_with_agent_config ~config:turn_config provider_config)
  | None ->
    Provider.provider_config_of_agent
      ~state:{ agent.state with config = turn_config }
      ~base_url:agent.options.base_url
      agent.options.provider
;;

let dispatch_sync
      ~sw
      ?clock
      ?(trace_context = [])
      ?on_provider_failure
      ~turn_config
      agent
      (prep : Agent_turn.turn_preparation)
  =
  let ( let* ) = Result.bind in
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  match provider_config_for_turn ~turn_config agent with
  | Error error ->
    let detailed = Provider_failure_attribution.of_provider_configuration_error error in
    notify_attribution on_provider_failure detailed.provider_failure;
    Error detailed.error
  | Ok pc ->
    let* binding =
      binding_identity_for_call agent pc
      |> Result.map_error (binding_identity_error ?on_provider_failure)
    in
    let compatibility_call () =
      Llm_provider.Complete.complete
        ~sw
        ~net:agent.net
        ?clock
        ?transport:agent.options.transport
        ~config:pc
        ~messages:prep.Agent_turn.effective_messages
        ~tools
        ~trace_context
        ?body_timeout_s:agent.options.body_timeout_s
        ()
      |> Result.map_error (Provider_failure_attribution.of_http_error ~binding)
    in
    let admitted_call () =
      let prepared =
        Llm_provider.Complete.prepare_request
          ~config:pc
          ~messages:prep.Agent_turn.effective_messages
          ~tools
          ~trace_context
          ()
      in
      match Llm_provider.Complete.measure_request ~sw ~net:agent.net ?clock prepared with
      | Error error -> Error (measurement_error ~binding error)
      | Ok measured ->
        (match Llm_provider.Complete.admit_request measured with
         | Error error -> Error (fit_error ~binding error)
         | Ok admitted ->
           Llm_provider.Complete.complete_admitted
             ~sw
             ~net:agent.net
             ?clock
             ?transport:agent.options.transport
             admitted
             ?body_timeout_s:agent.options.body_timeout_s
             ()
           |> Result.map_error (Provider_failure_attribution.of_http_error ~binding))
    in
    if enforce_context_fit agent pc
    then finish_call ?on_provider_failure (admitted_call ())
    else finish_call ?on_provider_failure (compatibility_call ())
;;

let dispatch_stream
      ~sw
      ?clock
      ~turn_config
      agent
      (prep : Agent_turn.turn_preparation)
      ~on_event
      ?capture_id
      ?(trace_context = [])
      ?on_telemetry
      ?on_provider_failure
      ()
  =
  let ( let* ) = Result.bind in
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  match provider_config_for_turn ~turn_config agent with
  | Error error ->
    let detailed = Provider_failure_attribution.of_provider_configuration_error error in
    notify_attribution on_provider_failure detailed.provider_failure;
    Error detailed.error
  | Ok pc ->
    let* binding =
      binding_identity_for_call agent pc
      |> Result.map_error (binding_identity_error ?on_provider_failure)
    in
    let compatibility_call () =
      Llm_provider.Complete.complete_stream
        ~sw
        ~net:agent.net
        ?clock
        ?transport:agent.options.transport
        ?capture_id
        ~config:pc
        ~messages:prep.Agent_turn.effective_messages
        ~tools
        ~trace_context
        ~on_event
        ?on_telemetry
        ?stream_idle_timeout_s:agent.options.stream_idle_timeout_s
        ()
      |> Result.map_error (Provider_failure_attribution.of_http_error ~binding)
    in
    let admitted_call () =
      let prepared =
        Llm_provider.Complete.prepare_request
          ~config:pc
          ~messages:prep.Agent_turn.effective_messages
          ~tools
          ~trace_context
          ?capture_id
          ?stream_idle_timeout_s:agent.options.stream_idle_timeout_s
          ()
      in
      match Llm_provider.Complete.measure_request ~sw ~net:agent.net ?clock prepared with
      | Error error -> Error (measurement_error ~binding error)
      | Ok measured ->
        (match Llm_provider.Complete.admit_request measured with
         | Error error -> Error (fit_error ~binding error)
         | Ok admitted ->
           Llm_provider.Complete.complete_stream_admitted
             ~sw
             ~net:agent.net
             ?clock
             ?transport:agent.options.transport
             admitted
             ~on_event
             ?on_telemetry
             ()
           |> Result.map_error (Provider_failure_attribution.of_http_error ~binding))
    in
    if enforce_context_fit agent pc
    then finish_call ?on_provider_failure (admitted_call ())
    else finish_call ?on_provider_failure (compatibility_call ())
;;
