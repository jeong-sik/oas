type tool_execution_result =
  { invocation : Tool.Invocation.t
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; content : string
  ; outcome : Types.tool_result_outcome
  }

type batch_completion =
  | Continue_after_batch
  | Terminal_completed of Tool.Invocation.t
  | Terminal_failed of
      { invocation : Tool.Invocation.t
      ; effect_disposition : Tool.failure_effect_disposition
      ; detail : string
      }

type execution_report =
  { completed_results : tool_execution_result list
  ; completion : batch_completion
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
  ; completion : batch_completion
  ; cause : execution_failure_cause
  }

type deferred_failure =
  | Deferred_hook_failure of execution_error
  | Deferred_observer_failure of
      { invocation : Tool.Invocation.t
      ; exception_ : exn
      ; backtrace : Printexc.raw_backtrace
      }

type tool_dispatch =
  { result : tool_execution_result
  ; deferred_failure : deferred_failure option
  }

type pending_tool_dispatch =
  { result : tool_execution_result
  ; finish_observers : unit -> tool_dispatch
  }

let failure_priority = function
  | Durability_failure _ -> 3
  | Observer_failure _ -> 2
  | Hook_failure _ -> 1
;;

let prefer_failure current candidate =
  match current with
  | None -> Some candidate
  | Some primary ->
    if failure_priority candidate > failure_priority primary
    then Some candidate
    else current
;;

let%test "durability failure dominates an earlier observer failure" =
  let schedule : Tool.schedule =
    { planned_index = 0; batch_index = 0; batch_size = 1; execution_mode = Tool.Serial }
  in
  let invocation =
    Tool.Invocation.create
      ~tool_use_id:"priority"
      ~turn:0
      ~schedule
      ~completion:Tool.Continue_after_success
  in
  let observer =
    Observer_failure
      { invocation
      ; exception_ = Failure "observer"
      ; backtrace = Printexc.get_callstack 1
      }
  in
  match
    prefer_failure (Some observer) (Durability_failure { invocation; detail = "lost" })
  with
  | Some (Durability_failure _) -> true
  | Some (Hook_failure _ | Observer_failure _) | None -> false
;;
