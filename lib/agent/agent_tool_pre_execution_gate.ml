type settlement =
  | Admit
  | Block of string
  | Reject of
      { stage : Hooks.hook_stage
      ; detail : string
      }

let publish_response ?correlation_id ?run_id ~event_bus ~agent_name request response =
  match event_bus with
  | Some bus ->
    Event_bus.publish
      bus
      (Event_bus.mk_event
         ?correlation_id
         ?run_id
         (Event_bus.ElicitationCompleted
            { agent_name; question = request.Hooks.question; response }))
  | None -> ()
;;

let settle ?elicitation ?correlation_id ?run_id ~event_bus ~agent_name = function
  | Hooks.Continue -> Admit
  | Hooks.Block reason -> Block reason
  | Hooks.HookFailed { stage; detail } -> Reject { stage; detail }
  | Hooks.ElicitInput request ->
    (match elicitation with
     | None ->
       Reject
         { stage = Hooks.Pre_tool_use
         ; detail =
             "ElicitInput at pre_tool_use requires the configured elicitation callback; \
              no tool invocation was opened"
         }
     | Some callback ->
       let response = callback request in
       publish_response ?correlation_id ?run_id ~event_bus ~agent_name request response;
       (match response with
        | Hooks.Answer _ -> Admit
        | Hooks.Declined -> Block "Tool execution was declined by the caller"
        | Hooks.Timeout -> Block "Tool execution approval timed out"))
  | (Hooks.AdjustParams _ | Hooks.Nudge _) as decision ->
    Reject
      { stage = Hooks.Pre_tool_use
      ; detail =
          Printf.sprintf
            "illegal decision %s escaped hook validation"
            (Hooks.decision_kind_to_string (Hooks.classify_decision decision))
      }
;;
