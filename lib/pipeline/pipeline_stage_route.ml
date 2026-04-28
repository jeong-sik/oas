module Retry = Llm_provider.Retry

open Agent_types

let ( let* ) = Result.bind

let sdk_error_of_http_error : Llm_provider.Http_client.http_error -> Error.sdk_error =
  function
  | Llm_provider.Http_client.HttpError { code; body } ->
      Error.Api (Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError { message; kind; _ } ->
      Error.Api (Retry.NetworkError { message; kind })
  | Llm_provider.Http_client.AcceptRejected { reason } ->
      Error.Api (Retry.InvalidRequest { message = reason })
  | Llm_provider.Http_client.CliTransportRequired { kind } ->
      Error.Api
        (Retry.InvalidRequest
           {
             message =
               Printf.sprintf
                 "CLI transport required for %s but none was injected; \
                  pass ~transport via agent.options.transport"
                 kind;
           })
  | Llm_provider.Http_client.ProviderTerminal { kind = Max_turns r; _ } ->
      Error.Agent (MaxTurnsExceeded { turns = r.turns; limit = r.limit })
  | Llm_provider.Http_client.ProviderTerminal
      { kind = Other reason; message } ->
      Error.Api
        (Retry.InvalidRequest
           { message = Printf.sprintf "%s: %s" reason message })

let dispatch_sync ~sw ?clock agent (prep : Agent_turn.turn_preparation) =
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  let* pc =
    Provider.provider_config_of_agent ~state:agent.state
      ~base_url:agent.options.base_url agent.options.provider
  in
  let call () =
    match clock with
    | Some clock ->
        Llm_provider.Complete.complete_with_retry ~sw ~net:agent.net
          ?transport:agent.options.transport ~clock ~config:pc
          ~messages:prep.Agent_turn.effective_messages ~tools
          ?runtime_mcp_policy:agent.options.runtime_mcp_policy
          ?priority:agent.options.priority ()
    | None ->
        Llm_provider.Complete.complete ~sw ~net:agent.net
          ?transport:agent.options.transport ~config:pc
          ~messages:prep.Agent_turn.effective_messages ~tools
          ?runtime_mcp_policy:agent.options.runtime_mcp_policy
          ?priority:agent.options.priority ()
  in
  match call () with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)

let dispatch_stream ~sw ?clock agent (prep : Agent_turn.turn_preparation) ~on_event =
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  let* pc =
    Provider.provider_config_of_agent ~state:agent.state
      ~base_url:agent.options.base_url agent.options.provider
  in
  match
    Llm_provider.Complete.complete_stream ~sw ~net:agent.net
      ?clock
      ?stream_idle_timeout_s:agent.options.stream_idle_timeout_s
      ?body_timeout_s:agent.options.body_timeout_s
      ?transport:agent.options.transport ~config:pc
      ~messages:prep.Agent_turn.effective_messages ~tools
      ?runtime_mcp_policy:agent.options.runtime_mcp_policy
      ~on_event ?priority:agent.options.priority ()
  with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)
