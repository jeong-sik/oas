open Agent_types
open Result_syntax

let _log = Log.create ~module_name:"pipeline_stage_route" ()
let sdk_error_of_http_error = Http_error_sdk.of_http_error

let output_token_receipt_observer agent ~model_id =
  match agent.options.event_bus with
  | None -> None
  | Some bus ->
    Some
      (fun receipt ->
        Pipeline_common.safe_publish
          ~log:_log
          bus
          { Event_bus.meta = Pipeline_common.event_envelope agent
          ; payload =
              Event_bus.Custom
                ( "oas.output_token_receipt"
                , `Assoc
                    [ "agent_name", `String agent.state.config.name
                    ; "turn", `Int agent.state.turn_count
                    ; "model_id", `String model_id
                    ; "receipt", Llm_provider.Types.output_token_receipt_to_yojson receipt
                    ] )
          })
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
  let on_output_token_receipt =
    output_token_receipt_observer agent ~model_id:pc.model_id
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
        ?on_output_token_receipt
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
        ?on_output_token_receipt
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
  let on_output_token_receipt =
    output_token_receipt_observer agent ~model_id:pc.model_id
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
        ?on_output_token_receipt
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
        ?on_output_token_receipt
        ()
  in
  match call () with
  | Ok resp -> Ok resp
  | Error err -> Error (sdk_error_of_http_error err)
;;
