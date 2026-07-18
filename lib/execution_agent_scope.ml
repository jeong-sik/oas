module Event = Execution_event
module Journal = Execution_journal
module Settlement = Execution_tool_settlement
module Writer = Execution_lane_writer
module Tx = Journal.Transaction
module Json = Execution_json

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

type scope_locator = { run_id : Event.Run_id.t }

type invocation_locator =
  { run_id : Event.Run_id.t
  ; node : Event.Node_id.t
  }

type invocation =
  { durable : Settlement.durable_invocation
  ; locator : invocation_locator
  }

type abort_reason =
  | Failed of Event.failure
  | Cancelled of
      { reason : string option
      ; data : Yojson.Safe.t option
      }

type error =
  | Admission_failed of Writer.submit_error
  | Mutation_failed of Writer.ticket_error
  | Invalid_provider_attempt of string
  | Scope_unavailable of Writer.read_error
  | Run_not_found
  | Invocation_locator_mismatch
  | Settlement_failed of Settlement.error

let settlement_error_to_string = function
  | Settlement.Authority_unavailable error -> Writer.read_error_to_string error
  | Settlement.Invocation_not_found -> "execution invocation was not found"
  | Settlement.Invocation_identity_mismatch ->
    "execution invocation identity does not match its durable topology"
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
  | Scope_unavailable error -> Writer.read_error_to_string error
  | Run_not_found -> "execution run was not found"
  | Invocation_locator_mismatch ->
    "execution invocation locator does not match durable topology"
  | Settlement_failed error -> settlement_error_to_string error
;;

let locator_version = 1

let require_locator_version fields =
  match Json.int_field "version" fields with
  | Ok version when version = locator_version -> Ok ()
  | Ok version ->
    Error (Printf.sprintf "unsupported execution locator version %d" version)
  | Error _ as error -> error
;;

let scope_locator scope = { run_id = Journal.run_id scope.run }

let scope_locator_to_yojson (locator : scope_locator) =
  `Assoc
    [ "version", `Int locator_version
    ; "run_id", `String (Event.Run_id.to_string locator.run_id)
    ]
;;

let scope_locator_of_yojson json =
  let open Result_syntax in
  let* fields =
    Json.object_fields
      ~context:"execution scope locator"
      ~required:[ "version"; "run_id" ]
      ~optional:[]
      json
  in
  let* () = require_locator_version fields in
  let* run_id = Json.string_field "run_id" fields in
  let+ run_id = Event.Run_id.of_string run_id in
  { run_id }
;;

let invocation_locator_to_yojson (locator : invocation_locator) =
  `Assoc
    [ "version", `Int locator_version
    ; "run_id", `String (Event.Run_id.to_string locator.run_id)
    ; "node_id", `String (Event.Node_id.to_string locator.node)
    ]
;;

let invocation_locator_of_yojson json =
  let open Result_syntax in
  let* fields =
    Json.object_fields
      ~context:"execution invocation locator"
      ~required:[ "version"; "run_id"; "node_id" ]
      ~optional:[]
      json
  in
  let* () = require_locator_version fields in
  let* run_id_text = Json.string_field "run_id" fields in
  let* run_id = Event.Run_id.of_string run_id_text in
  let* node_text = Json.string_field "node_id" fields in
  let+ node = Event.Node_id.of_string node_text in
  { run_id; node }
;;

let transact writer transaction =
  Eio.Fiber.check ();
  match Writer.submit writer transaction with
  | Error error -> Error (Admission_failed error)
  | Ok ticket ->
    (match Eio.Cancel.protect (fun () -> Writer.await ticket) with
     | Error error -> Error (Mutation_failed error)
     | Ok receipt -> Ok receipt.value)
;;

let transact_cleanup writer transaction =
  Eio.Cancel.protect (fun () ->
    match Writer.submit writer transaction with
    | Error error -> Error (Admission_failed error)
    | Ok ticket ->
      (match Writer.await ticket with
       | Error error -> Error (Mutation_failed error)
       | Ok receipt -> Ok receipt.value))
;;

let start ~writer ~agent_name =
  transact writer (Tx.start_run ~agent_name ())
  |> Result.map (fun (run, _event) -> { writer; run })
;;

let resume ~writer (locator : scope_locator) =
  match Writer.find_run writer locator.run_id with
  | Error error -> Error (Scope_unavailable error)
  | Ok None -> Error Run_not_found
  | Ok (Some view) -> Ok { writer; run = view.Journal.run }
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

let open_provider_attempt turn ~ordinal binding =
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
    let locator = { run_id = Journal.run_id scope.run; node } in
    Settlement.rebind ~writer:scope.writer ~invocation_node:node
    |> Result.map (fun durable -> { durable; locator })
    |> Result.map_error (fun error -> Settlement_failed error)
;;

let invocation_locator invocation = invocation.locator

let rebind_invocation scope locator =
  if not (Event.Run_id.equal locator.run_id (Journal.run_id scope.run))
  then Error Invocation_locator_mismatch
  else (
    match Settlement.rebind ~writer:scope.writer ~invocation_node:locator.node with
    | Error error -> Error (Settlement_failed error)
    | Ok durable ->
      if Event.Run_id.equal durable.run_id locator.run_id
      then Ok { durable; locator }
      else Error Invocation_locator_mismatch)
;;

let execute invocation ~invoke =
  Settlement.execute invocation.durable.authority ~invoke:(fun () ->
    invoke
      ~invocation:invocation.durable.invocation
      ~tool_name:invocation.durable.tool_name
      ~input:invocation.durable.input)
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

let abort scope reason =
  let terminal =
    match reason with
    | Failed failure -> Event.Failed failure
    | Cancelled { reason; data } -> Event.Cancelled { reason; data }
  in
  transact_cleanup scope.writer (Tx.abort_run ~run:scope.run terminal)
  |> Result.map ignore
;;
