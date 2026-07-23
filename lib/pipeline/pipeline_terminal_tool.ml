open Types
open Result_syntax

type terminal_tool_completion =
  { invocation : Tool.Invocation.t
  ; response : Types.api_response
  ; checkpoint_stage : Agent_types.checkpoint_stage
  }

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted of Agent_types.checkpoint_stage
  | TerminalToolCompleted of terminal_tool_completion

let response agent tool_uses : Types.api_response =
  { id = ""
  ; model = agent.Agent_types.state.config.model
  ; stop_reason = StopToolUse
  ; content = Nonempty.to_list tool_uses
  ; usage = None
  ; telemetry = None
  }
;;

let unpack_execution_result = function
  | Ok ({ Agent_tools.completed_results; completion } : Agent_tools.execution_report) ->
    completed_results, completion, None
  | Error
      ({ Agent_tools.completed_results; completion; cause } :
        Agent_tools.execution_failure) -> completed_results, completion, Some cause
;;

let outcome ~response completion checkpoint_stage =
  match completion with
  | Agent_tools.Continue_after_batch -> Ok (ToolsExecuted checkpoint_stage)
  | Agent_tools.Terminal_completed invocation ->
    Ok (TerminalToolCompleted { invocation; response; checkpoint_stage })
  | Agent_tools.Terminal_failed { invocation; effect_disposition; detail } ->
    let tool_use_id = Tool.Invocation.tool_use_id invocation in
    (match effect_disposition with
     | Tool.Proven_post_effect ->
       Error
         (Error.Agent
            (Error.TerminalToolEffectFailed
               { tool_use_id; effect_disposition = Error.Proven_post_effect; detail }))
     | Tool.Effect_outcome_unknown ->
       Error
         (Error.Agent
            (Error.TerminalToolEffectFailed
               { tool_use_id; effect_disposition = Error.Effect_outcome_unknown; detail }))
     | Tool.Proven_pre_effect ->
       Error
         (Error.Internal
            "pre-effect terminal failure crossed the correction-capable boundary"))
;;

let durability_failure ~invocation ~detail =
  match Tool.Invocation.completion invocation with
  | Tool.Continue_after_success -> Error (Error.Internal detail)
  | Tool.Terminal_after_success disposition ->
    let effect_disposition =
      match disposition with
      | Tool.Proven_post_effect -> Error.Proven_post_effect
      | Tool.Proven_pre_effect | Tool.Effect_outcome_unknown ->
        Error.Effect_outcome_unknown
    in
    Error
      (Error.Agent
         (Error.TerminalToolDurabilityFailed { invocation; effect_disposition; detail }))
;;

let resume_topology_error detail =
  Error.Internal
    (Execution_agent_scope.error_to_string
       (Execution_agent_scope.Resume_topology_mismatch detail))
;;

let recovered_report ~turn ~invocations ~tool_results tool_uses =
  let rec recover_results expected_index acc invocations tool_uses tool_results =
    match invocations, tool_uses, tool_results with
    | ( authority :: invocations
      , ToolUse { id; name; input } :: tool_uses
      , ToolResult { tool_use_id; content; outcome; _ } :: tool_results ) ->
      let* () =
        Execution_agent_scope.validate_invocation_authority
          authority
          ~turn
          ~planned_index:expected_index
          ~tool_use_id:id
          ~tool_name:name
          ~input
        |> Result.map_error (fun error ->
          Error.Internal (Execution_agent_scope.error_to_string error))
      in
      if String.equal tool_use_id id
      then
        recover_results
          (expected_index + 1)
          ({ Agent_tools.invocation = authority.invocation
           ; tool_name = authority.tool_name
           ; input = authority.input
           ; content
           ; outcome
           }
           :: acc)
          invocations
          tool_uses
          tool_results
      else
        Error (resume_topology_error "restored ToolResult identity differs from ToolUse")
    | [], [], [] -> Ok (List.rev acc)
    | _ ->
      Error
        (resume_topology_error
           "persisted invocation count differs from restored topology")
  in
  let tool_uses = Nonempty.to_list tool_uses in
  let* completed_results = recover_results 0 [] invocations tool_uses tool_results in
  let persisted_invocations =
    List.map
      (fun (authority : Execution_agent_scope.invocation_authority) ->
         authority.invocation)
      invocations
  in
  let* completion =
    Agent_tools.recovered_batch_completion ~invocations:persisted_invocations tool_results
  in
  Ok Agent_tools.{ completed_results; completion }
;;

let recovered_outcome agent ~turn ~invocations ~tool_results tool_uses =
  let response = response agent tool_uses in
  let* report = recovered_report ~turn ~invocations ~tool_results tool_uses in
  outcome ~response report.completion Agent_types.After_tool_results_appended
;;
