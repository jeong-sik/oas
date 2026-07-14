open Agent_types

let sdk_error_of_http_error = Http_error_sdk.of_http_error

let notify_attribution callback attribution =
  Option.iter (fun notify -> notify attribution) callback
;;

let binding_identity_for_call agent provider_config =
  let transport =
    Binding_identity.transport_for_call ~injected:(Option.is_some agent.options.transport)
  in
  Binding_identity.of_provider_config ~transport provider_config
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
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  match provider_config_for_turn ~turn_config agent with
  | Error error ->
    let detailed = Provider_failure_attribution.of_provider_configuration_error error in
    notify_attribution on_provider_failure detailed.provider_failure;
    Error detailed.error
  | Ok pc ->
    let call () =
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
    in
    (match call () with
     | Ok resp ->
       notify_attribution on_provider_failure None;
       Ok resp
     | Error err ->
       let binding = binding_identity_for_call agent pc in
       let detailed = Provider_failure_attribution.of_http_error ~binding err in
       notify_attribution on_provider_failure detailed.provider_failure;
       Error detailed.error)
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
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  match provider_config_for_turn ~turn_config agent with
  | Error error ->
    let detailed = Provider_failure_attribution.of_provider_configuration_error error in
    notify_attribution on_provider_failure detailed.provider_failure;
    Error detailed.error
  | Ok pc ->
    let call () =
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
    in
    (match call () with
     | Ok resp ->
       notify_attribution on_provider_failure None;
       Ok resp
     | Error err ->
       let binding = binding_identity_for_call agent pc in
       let detailed = Provider_failure_attribution.of_http_error ~binding err in
       notify_attribution on_provider_failure detailed.provider_failure;
       Error detailed.error)
;;
