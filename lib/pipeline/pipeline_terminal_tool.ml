open Types
open Result_syntax

type turn_outcome =
  | Complete of Types.api_response
  | ToolsExecuted of Agent_types.checkpoint_stage
  | TerminalToolCompleted of
      { invocation : Tool.Invocation.t
      ; response : Types.api_response
      ; checkpoint_stage : Agent_types.checkpoint_stage
      }

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
    Error
      (Error.Internal
         (Printf.sprintf
            "terminal tool %S failed after an effect boundary that forbids another \
             provider turn (%s): %s"
            (Tool.Invocation.tool_use_id invocation)
            (Tool.show_failure_effect_disposition effect_disposition)
            detail))
;;

let recovered_outcome agent ~turn:_ ~invocations ~tool_results tool_uses =
  let response = response agent tool_uses in
  let* completion = Agent_tools.recovered_batch_completion ~invocations tool_results in
  outcome ~response completion Agent_types.After_tool_results_appended
;;
