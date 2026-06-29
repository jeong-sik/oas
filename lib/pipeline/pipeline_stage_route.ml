open Agent_types
open Result_syntax

let sdk_error_of_http_error = Http_error_sdk.of_http_error

let dispatch_sync
      ~sw
      ?clock
      ?(trace_context = [])
      agent
      (prep : Agent_turn.turn_preparation)
  =
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  let* pc =
    Provider.provider_config_of_agent
      ~state:agent.state
      ~base_url:agent.options.base_url
      agent.options.provider
  in
  let call () =
    match clock with
    | Some clock ->
      Llm_provider.Complete.complete_with_retry
        ~sw
        ~net:agent.net
        ?transport:agent.options.transport
        ~clock
        ~config:pc
        ~messages:prep.Agent_turn.effective_messages
        ~tools
        ?runtime_mcp_policy:prep.Agent_turn.runtime_mcp_policy
        ~trace_context
        ?priority:agent.options.priority
        ?body_timeout_s:agent.options.body_timeout_s
        ()
    | None ->
      Llm_provider.Complete.complete
        ~sw
        ~net:agent.net
        ?transport:agent.options.transport
        ~config:pc
        ~messages:prep.Agent_turn.effective_messages
        ~tools
        ?runtime_mcp_policy:prep.Agent_turn.runtime_mcp_policy
        ~trace_context
        ?priority:agent.options.priority
        ?body_timeout_s:agent.options.body_timeout_s
        ()
  in
  match call () with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)
;;

let dispatch_stream
      ~sw
      ?clock
      agent
      (prep : Agent_turn.turn_preparation)
      ~on_event
      ?(trace_context = [])
      ?on_telemetry
      ()
  =
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  let* pc =
    Provider.provider_config_of_agent
      ~state:agent.state
      ~base_url:agent.options.base_url
      agent.options.provider
  in
  let call () =
    match clock with
    | Some clock ->
      Llm_provider.Complete.complete_stream_with_retry
        ~sw
        ~net:agent.net
        ?transport:agent.options.transport
        ~clock
        ~config:pc
        ~messages:prep.Agent_turn.effective_messages
        ~tools
        ?runtime_mcp_policy:prep.Agent_turn.runtime_mcp_policy
        ~trace_context
        ~on_event
        ?on_telemetry
        ?priority:agent.options.priority
        ?stream_idle_timeout_s:agent.options.stream_idle_timeout_s
        ()
    | None ->
      Llm_provider.Complete.complete_stream
        ~sw
        ~net:agent.net
        ?transport:agent.options.transport
        ~config:pc
        ~messages:prep.Agent_turn.effective_messages
        ~tools
        ?runtime_mcp_policy:prep.Agent_turn.runtime_mcp_policy
        ~trace_context
        ~on_event
        ?on_telemetry
        ?priority:agent.options.priority
        ?stream_idle_timeout_s:agent.options.stream_idle_timeout_s
        ()
  in
  match call () with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)
;;
