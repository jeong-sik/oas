type settlement =
  | Admit
  | Block of string
  | Reject of
      { stage : Hooks.hook_stage
      ; detail : string
      }

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
       let approval = callback request in
       publish_approval ?correlation_id ?run_id ~event_bus ~agent_name request approval;
       (match approval with
        | Hooks.Approved -> Admit
        | Hooks.Denied -> Block "Tool execution was denied by the caller"
        | Hooks.Timed_out -> Block "Tool execution approval timed out"))
  | (Hooks.AdjustParams _ | Hooks.ElicitInput _ | Hooks.Nudge _) as decision ->
    Reject
      { stage = Hooks.Pre_tool_use
      ; detail =
          Printf.sprintf
            "illegal decision %s escaped hook validation"
            (Hooks.decision_kind_to_string (Hooks.classify_decision decision))
      }
;;
