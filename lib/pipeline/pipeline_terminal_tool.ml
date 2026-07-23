open Types

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
  | Ok
      ({ Agent_tools.completed_results; completion } : Agent_tools.execution_report) ->
    completed_results, completion, None
  | Error
      ({ Agent_tools.completed_results; completion; cause } :
        Agent_tools.execution_failure) -> completed_results, completion, Some cause
;;

let outcome ~response completion checkpoint_stage =
  match completion with
  | Agent_tools.Continue_after_batch -> ToolsExecuted checkpoint_stage
  | Agent_tools.Terminal_completed invocation ->
    TerminalToolCompleted { invocation; response; checkpoint_stage }
;;

let recovered agent ~turn tool_uses =
  let response = response agent tool_uses in
  let completion =
    Agent_tools.recovered_batch_completion
      ~tools:(Tool_set.to_list agent.Agent_types.tools)
      ~turn
      ~tool_uses:response.content
      ~tool_results:
        (Pipeline_stage_prepare.last_tool_results_from agent.Agent_types.state.messages)
  in
  response, completion
;;

let recovered_outcome agent ~turn tool_uses =
  let response, completion = recovered agent ~turn tool_uses in
  outcome ~response completion Agent_types.After_tool_results_appended
;;
