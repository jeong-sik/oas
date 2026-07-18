type tool_execution_result =
  { invocation : Tool.Invocation.t
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; content : string
  ; outcome : Types.tool_result_outcome
  }

type execution_error =
  | Hook_execution_failed of
      { hook_name : string
      ; stage : Hooks.hook_stage
      ; tool_name : string
      ; invocation : Tool.Invocation.t
      ; detail : string
      }

type execution_failure_cause =
  | Hook_failure of execution_error
  | Durability_failure of
      { invocation : Tool.Invocation.t
      ; detail : string
      }
  | Observer_failure of
      { invocation : Tool.Invocation.t
      ; exception_ : exn
      ; backtrace : Printexc.raw_backtrace
      }

type execution_failure =
  { completed_results : tool_execution_result list
  ; cause : execution_failure_cause
  }
