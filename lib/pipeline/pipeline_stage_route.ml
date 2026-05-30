module Retry = Llm_provider.Retry
open Agent_types
open Result_syntax

let sdk_error_of_http_error : Llm_provider.Http_client.http_error -> Error.sdk_error =
  function
  | Llm_provider.Http_client.HttpError { code; body } ->
    Error.Api (Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError { message; kind; _ } ->
    Error.Api (Retry.NetworkError { message; kind })
  | Llm_provider.Http_client.TimeoutError _ as err ->
    Error.Provider (Llm_provider.Error.of_http_error err)
  | Llm_provider.Http_client.AcceptRejected { reason } ->
    Error.Api (Retry.InvalidRequest { message = reason })
  | Llm_provider.Http_client.ProviderTerminal { kind = Max_turns r; _ } ->
    Error.Agent (MaxTurnsExceeded { turns = r.turns; limit = r.limit })
  | Llm_provider.Http_client.ProviderTerminal { kind = Other _; _ } as err ->
    Error.Provider (Llm_provider.Error.of_http_error err)
  | Llm_provider.Http_client.ProviderFailure _ as err ->
    Error.Provider (Llm_provider.Error.of_http_error err)
;;

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
  match
    Llm_provider.Complete.complete_stream
      ~sw
      ~net:agent.net
      ?clock
      ?stream_idle_timeout_s:agent.options.stream_idle_timeout_s
      ?body_timeout_s:agent.options.body_timeout_s
      ?transport:agent.options.transport
      ~config:pc
      ~messages:prep.Agent_turn.effective_messages
      ~tools
      ?runtime_mcp_policy:prep.Agent_turn.runtime_mcp_policy
      ~trace_context
      ~on_event
      ?on_telemetry
      ?priority:agent.options.priority
      ()
  with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)
;;
