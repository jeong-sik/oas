module Event = Execution_event
module Journal = Execution_journal
module Settlement = Execution_tool_settlement
module Writer = Execution_lane_writer
module Tx = Journal.Transaction

type t =
  { writer : Writer.t
  ; run : Journal.run
  }

type turn =
  { scope : t
  ; node : Event.Node_id.t
  }

type provider_attempt =
  { turn : turn
  ; node : Event.Node_id.t
  }

type invocation = { authority : Settlement.t }

type error =
  | Admission_failed of Writer.submit_error
  | Mutation_failed of Writer.ticket_error
  | Invalid_provider_attempt of string
  | Settlement_failed of Settlement.error

let settlement_error_to_string = function
  | Settlement.Authority_unavailable error -> Writer.read_error_to_string error
  | Settlement.Invocation_not_found -> "execution invocation was not found"
  | Settlement.Invocation_identity_mismatch -> "execution invocation identity mismatch"
  | Settlement.Effect_outcome_unknown -> "tool effect outcome is unknown"
  | Settlement.Attempt_admission_failed error -> Writer.submit_error_to_string error
  | Settlement.Attempt_commit_failed error -> Writer.ticket_error_to_string error
  | Settlement.Receipt_admission_outcome_unknown error ->
    Writer.submit_error_to_string error
  | Settlement.Receipt_settlement_outcome_unknown error ->
    Writer.ticket_error_to_string error
;;

let error_to_string = function
  | Admission_failed error -> Writer.submit_error_to_string error
  | Mutation_failed error -> Writer.ticket_error_to_string error
  | Invalid_provider_attempt detail -> "invalid provider attempt: " ^ detail
  | Settlement_failed error -> settlement_error_to_string error
;;

let transact writer transaction =
  match Writer.submit writer transaction with
  | Error error -> Error (Admission_failed error)
  | Ok ticket ->
    (match Writer.await ticket with
     | Error error -> Error (Mutation_failed error)
     | Ok receipt -> Ok receipt.value)
;;

let start ~writer ~agent_name =
  Result.map
    (fun (run, _event) -> { writer; run })
    (transact writer (Tx.start_run ~agent_name ()))
;;

let open_turn scope ~ordinal =
  transact
    scope.writer
    (Tx.open_node
       ~run:scope.run
       ~parent:(Journal.run_root scope.run)
       ~kind:(Event.Agent_turn { ordinal })
       ())
  |> Result.map (fun (node, _event) -> { scope; node })
;;

let before_provider_attempt turn ~ordinal binding =
  match Event.provider_attempt ~ordinal binding with
  | Error detail -> Error (Invalid_provider_attempt detail)
  | Ok kind ->
    transact
      turn.scope.writer
      (Tx.open_node ~run:turn.scope.run ~parent:turn.node ~kind ())
    |> Result.map (fun (node, _event) -> { turn; node })
;;

let open_invocation provider ~invocation ~tool_name ~input =
  let scope = provider.turn.scope in
  match
    transact
      scope.writer
      (Tx.open_tool_invocation
         ~run:scope.run
         ~provider_attempt:provider.node
         ~invocation
         ~tool_name
         ~input
         ())
  with
  | Error _ as error -> error
  | Ok (node, _events) ->
    Settlement.create
      ~writer:scope.writer
      ~run:scope.run
      ~invocation_node:node
      ~invocation
    |> Result.map (fun authority -> { authority })
    |> Result.map_error (fun error -> Settlement_failed error)
;;

let execute invocation ~invoke =
  Settlement.execute invocation.authority ~invoke
  |> Result.map_error (fun error -> Settlement_failed error)
;;

let close_node writer node terminal =
  transact writer (Tx.close_node ~node terminal) |> Result.map ignore
;;

let close_provider_attempt provider terminal =
  close_node provider.turn.scope.writer provider.node terminal
;;

let close_turn turn terminal = close_node turn.scope.writer turn.node terminal

let finish scope terminal =
  transact scope.writer (Tx.finish_run ~run:scope.run terminal) |> Result.map ignore
;;
