module Retry = Llm_provider.Retry

open Types
open Agent_types
open Agent_trace

open Pipeline_common



(** Convert [Llm_provider.Http_client.http_error] into the [sdk_error]
    shape that legacy [Api.create_message] surfaced.  Keeps downstream
    Pipeline/Retry/ContextOverflow handling source-compatible while the
    Sync dispatch migrates to {!Llm_provider.Complete.complete}.

    HTTP status codes are re-classified via {!Retry.classify_error} so
    ContextOverflow/RateLimited/etc. still map to the same variants. *)
let sdk_error_of_http_error : Llm_provider.Http_client.http_error -> Error.sdk_error =
  function
  | Llm_provider.Http_client.HttpError { code; body } ->
      Error.Api (Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError { message; kind; _ } ->
      Error.Api (Retry.NetworkError { message; kind })
  | Llm_provider.Http_client.AcceptRejected { reason } ->
      Error.Api (Retry.InvalidRequest { message = reason })
  | Llm_provider.Http_client.CliTransportRequired { kind } ->
      Error.Api (Retry.InvalidRequest {
        message = Printf.sprintf
          "CLI transport required for %s but none was injected; \
           pass ~transport via agent.options.transport" kind })
  | Llm_provider.Http_client.ProviderTerminal { kind = Max_turns r; _ } ->
      (* Map provider-internal max_turns to the agent-runtime variant so
         the existing [MaxTurnsExceeded] graceful path (checkpoint +
         [TurnBudgetExhausted] stop_reason) handles it instead of the
         cascade treating it as a transient API failure. *)
      Error.Agent (MaxTurnsExceeded { turns = r.turns; limit = r.limit })
  | Llm_provider.Http_client.ProviderTerminal
      { kind = Other reason; message } ->
      Error.Api (Retry.InvalidRequest {
        message = Printf.sprintf "%s: %s" reason message })

(** Sync dispatch via {!Llm_provider.Complete.complete}.  Routes all
    provider kinds through the consolidated path so [on_request_end]
    metrics fire and [Llm_transport.t] (set via [agent.options.transport])
    handles CLI providers.  Legacy {!Api.create_message} remains for
    Stream fallback pending PR-O2b. *)
let dispatch_sync ~sw ?clock agent prep =
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  let open Result in
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
          ~sw ~net:agent.net ?transport:agent.options.transport ~clock
          ~config:pc ~messages:prep.effective_messages ~tools
          ?runtime_mcp_policy:agent.options.runtime_mcp_policy
          ?priority:agent.options.priority ()
    | None ->
        Llm_provider.Complete.complete
          ~sw ~net:agent.net ?transport:agent.options.transport
          ~config:pc ~messages:prep.effective_messages ~tools
          ?runtime_mcp_policy:agent.options.runtime_mcp_policy
          ?priority:agent.options.priority ()
  in
  match call () with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)

let dispatch_stream ~sw agent prep ~on_event =
  let tools = Option.value prep.Agent_turn.tools_json ~default:[] in
  let open Result in
  let* pc =
    Provider.provider_config_of_agent
      ~state:agent.state
      ~base_url:agent.options.base_url
      agent.options.provider
  in
  match
    Llm_provider.Complete.complete_stream
      ~sw ~net:agent.net ?transport:agent.options.transport
      ~config:pc ~messages:prep.effective_messages ~tools
      ?runtime_mcp_policy:agent.options.runtime_mcp_policy
      ~on_event ?priority:agent.options.priority ()
  with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)

(** Dispatch the API call via the chosen strategy (sync or stream). *)
let stage_route ~sw ?clock ~api_strategy agent prep =
  match api_strategy with
  | Sync ->
    Tracing.with_span agent.options.tracer
      { kind = Api_call; name = "create_message";
        agent_name = agent.state.config.name;
        turn = agent.state.turn_count; extra = [] }
      (fun _tracer -> dispatch_sync ~sw ?clock agent prep)
  | Stream { on_event } ->
    Tracing.with_span agent.options.tracer
      { kind = Api_call; name = "create_message_stream";
        agent_name = agent.state.config.name;
        turn = agent.state.turn_count; extra = [] }
      (fun _tracer -> dispatch_stream ~sw agent prep ~on_event)

