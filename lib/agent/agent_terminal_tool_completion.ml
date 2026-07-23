type t =
  { invocation : Tool.Invocation.t
  ; response : Types.api_response
  ; checkpoint_stage : Agent_types.checkpoint_stage
  }

let of_pipeline (completion : Pipeline.terminal_tool_completion) : t =
  { invocation = completion.invocation
  ; response = completion.response
  ; checkpoint_stage = completion.checkpoint_stage
  }
;;
