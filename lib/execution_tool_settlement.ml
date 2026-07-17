module Event = Execution_event
module Journal = Execution_journal
module Writer = Execution_lane_writer

type t =
  { writer : Writer.t
  ; invocation_node : Event.Node_id.t
  }

type status =
  | Ready_to_execute
  | Outcome_unknown
  | Settled of Llm_provider.Types.content_block

type error =
  | Authority_unavailable of Writer.read_error
  | Invocation_not_found
  | Invocation_identity_mismatch
  | Effect_outcome_unknown
  | Attempt_admission_failed of Writer.submit_error
  | Attempt_commit_failed of Writer.ticket_error
  | Receipt_admission_outcome_unknown of Writer.submit_error
  | Receipt_settlement_outcome_unknown of Writer.ticket_error

type execution =
  | Executed of Llm_provider.Types.content_block * Journal.cursor * int
  | Replayed of Llm_provider.Types.content_block

let read_node writer node =
  match Writer.find_node writer node with
  | Error error -> Error (Authority_unavailable error)
  | Ok None -> Error Invocation_not_found
  | Ok (Some view) -> Ok view
;;

let parent_node view = Event.parent_node_id view.Journal.node

let create ~writer ~invocation_node ~invocation =
  let open Result_syntax in
  let* invocation_view = read_node writer invocation_node in
  let* provider_attempt =
    match
      ( Event.node_kind invocation_view.node
      , parent_node invocation_view
      , invocation_view.materialized )
    with
    | ( Event.Tool_invocation { schedule; _ }
      , Some parent
      , Journal.Tool_invocation_state
          { input = Some (Llm_provider.Types.ToolUse { id; _ }); _ } )
      when String.equal id (Tool.Invocation.tool_use_id invocation)
           && Execution_tool_schedule.equal schedule (Tool.Invocation.schedule invocation)
      -> Ok parent
    | ( ( Event.Agent_run _
        | Event.Agent_turn _
        | Event.Provider_attempt _
        | Event.Output_block _
        | Event.Tool_invocation _
        | Event.Tool_attempt )
      , (None | Some _)
      , _ ) -> Error Invocation_identity_mismatch
  in
  let* provider_view = read_node writer provider_attempt in
  let* turn_node =
    match Event.node_kind provider_view.node, parent_node provider_view with
    | Event.Provider_attempt _, Some parent -> Ok parent
    | Event.Provider_attempt _, None
    | ( ( Event.Agent_run _
        | Event.Agent_turn _
        | Event.Output_block _
        | Event.Tool_invocation _
        | Event.Tool_attempt )
      , _ ) -> Error Invocation_identity_mismatch
  in
  let* turn_view = read_node writer turn_node in
  match Event.node_kind turn_view.node with
  | Event.Agent_turn { ordinal } when ordinal = Tool.Invocation.turn invocation ->
    Ok { writer; invocation_node }
  | Event.Agent_turn _
  | Event.Agent_run _
  | Event.Provider_attempt _
  | Event.Output_block _
  | Event.Tool_invocation _
  | Event.Tool_attempt -> Error Invocation_identity_mismatch
;;

let status authority =
  match read_node authority.writer authority.invocation_node with
  | Error _ as error -> error
  | Ok view ->
    (match view.materialized, view.children, view.status with
     | Journal.Tool_invocation_state { result = Some result; _ }, _, _ ->
       Ok (Settled result)
     | Journal.Tool_invocation_state { result = None; _ }, [], Journal.Open ->
       Ok Ready_to_execute
     | ( Journal.Tool_invocation_state { result = None; _ }
       , _
       , (Journal.Open | Journal.Closed _) ) -> Ok Outcome_unknown
     | ( ( Journal.Agent_run_state
         | Journal.Agent_turn_state
         | Journal.Provider_attempt_state _
         | Journal.Output_block_state _
         | Journal.Tool_attempt_state )
       , _
       , _ ) -> Error Invocation_identity_mismatch)
;;

let begin_attempt authority =
  match
    Writer.submit
      authority.writer
      (Journal.Transaction.begin_tool_attempt ~invocation:authority.invocation_node ())
  with
  | Error error -> Error (Attempt_admission_failed error)
  | Ok ticket ->
    (match Writer.await ticket with
     | Error error -> Error (Attempt_commit_failed error)
     | Ok committed ->
       let node, _event = committed.value in
       Ok node)
;;

let settle authority attempt result =
  let transaction =
    Journal.Transaction.settle_tool_attempt
      ~attempt
      ~invocation:authority.invocation_node
      ~result
      ()
  in
  match Writer.submit authority.writer transaction with
  | Error error -> Error (Receipt_admission_outcome_unknown error)
  | Ok ticket ->
    (match Writer.await ticket with
     | Error error -> Error (Receipt_settlement_outcome_unknown error)
     | Ok committed -> Ok (result, committed.through, committed.group_event_count))
;;

let execute authority ~invoke =
  let open Result_syntax in
  let* current = status authority in
  match current with
  | Settled result -> Ok (Replayed result)
  | Outcome_unknown -> Error Effect_outcome_unknown
  | Ready_to_execute ->
    let* attempt = begin_attempt authority in
    let result = invoke () in
    let+ committed, through, event_count = settle authority attempt result in
    Executed (committed, through, event_count)
;;
