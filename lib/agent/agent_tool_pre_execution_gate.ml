type settlement =
  | Admit
  | Block of string
  | Reject of
      { stage : Hooks.hook_stage
      ; detail : string
      }

type scheduled_settlement =
  | Run_admitted
  | Return_outcome of Agent_tool_execution_types.scheduled_tool_outcome

let publish_approval
      ?correlation_id
      ?run_id
      ~event_bus
      ~agent_name
      (request : Hooks.tool_approval_request)
      approval
  =
  match event_bus with
  | Some bus ->
    Event_bus.publish
      bus
      (Event_bus.mk_event
         ?correlation_id
         ?run_id
         (Event_bus.ToolApprovalCompleted
            { agent_name
            ; invocation = request.invocation
            ; tool_name = request.tool_name
            ; approval
            }))
  | None -> ()
;;

let settle
      ?tool_approval
      ?correlation_id
      ?run_id
      ~event_bus
      ~agent_name
      ~invocation
      ~tool_name
      ~input
  = function
  | Hooks.Continue -> Admit
  | Hooks.Block reason -> Block reason
  | Hooks.HookFailed { stage; detail } -> Reject { stage; detail }
  | Hooks.ElicitToolApproval prompt ->
    (match tool_approval with
     | None ->
       Reject
         { stage = Hooks.Pre_tool_use
         ; detail =
             "ElicitToolApproval at pre_tool_use requires the configured tool_approval \
              callback; no tool invocation was opened"
         }
     | Some callback ->
       let request = { Hooks.prompt; invocation; tool_name; input } in
       (match callback request with
        | exception exception_ ->
          Llm_provider.Reserved_exn.reraise_if_reserved exception_;
          Reject
            { stage = Hooks.Pre_tool_use
            ; detail =
                "tool_approval callback raised before tool execution: "
                ^ Printexc.to_string exception_
            }
        | approval ->
          publish_approval ?correlation_id ?run_id ~event_bus ~agent_name request approval;
          (match approval with
           | Hooks.Approved -> Admit
           | Hooks.Denied -> Block "Tool execution was denied by the caller"
           | Hooks.Timed_out -> Block "Tool execution approval timed out")))
  | (Hooks.AdjustParams _ | Hooks.ElicitInput _ | Hooks.Nudge _) as decision ->
    Reject
      { stage = Hooks.Pre_tool_use
      ; detail =
          Printf.sprintf
            "illegal decision %s escaped hook validation"
            (Hooks.decision_kind_to_string (Hooks.classify_decision decision))
      }
;;

let settle_existing_rejection
      durable
      (result : Agent_tool_execution_types.tool_execution_result)
  =
  Execution_agent_scope.execute_phased
    durable
    ~invoke:(fun ~start_child:_ ~tool_name:_ ~input:_ ->
      (result.content, result.outcome), (fun () -> ()))
  |> Result.map (function
    | Execution_agent_scope.Executed (settled, _, _)
    | Execution_agent_scope.Replayed settled ->
      { Agent_tool_execution_types.invocation = settled.invocation
      ; tool_name = settled.tool_name
      ; input = settled.input
      ; content = settled.content
      ; outcome = settled.outcome
      })
;;

let scheduled_settlement
      ?settle_rejected
      ~index
      ~invocation
      ~tool_name
      ~blocked_result
  = function
  | Admit -> Run_admitted
  | Block reason ->
    let result = blocked_result reason in
    let result =
      match settle_rejected with
      | None -> result
      | Some settle -> settle result
    in
    Return_outcome
      { index
      ; completed_result = Some result
      ; completion = Agent_tool_execution_types.Continue_after_batch
      ; failure = None
      }
  | Reject { stage; detail } ->
    let result = blocked_result detail in
    (match settle_rejected with
     | None -> ()
     | Some settle ->
       let _ = settle result in
       ());
    Return_outcome
      { index
      ; completed_result = None
      ; completion = Agent_tool_execution_types.Continue_after_batch
      ; failure =
          Some
            (Agent_tool_execution_types.Hook_failure
               (Agent_tool_execution_types.Hook_execution_failed
                  { hook_name = "pre_tool_use"
                  ; stage
                  ; tool_name
                  ; invocation
                  ; detail
                  }))
      }
