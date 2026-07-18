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
  ; scope : t
  }

type tool_result =
  { invocation : Tool.Invocation.t
  ; tool_name : string
  ; input : Yojson.Safe.t
  ; content : string
  ; outcome : Llm_provider.Types.tool_result_outcome
  }

type execution =
  | Executed of tool_result * Journal.cursor * int
  | Replayed of tool_result

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
  | Agent_identity_mismatch of
      { expected : string
      ; actual : string
      }
  | Invocation_locator_mismatch
  | Resume_topology_mismatch of string
  | Invalid_tool_result
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
  | Agent_identity_mismatch { expected; actual } ->
    Printf.sprintf
      "execution run belongs to agent %S but caller supplied %S"
      expected
      actual
  | Invocation_locator_mismatch ->
    "execution invocation locator does not match durable topology"
  | Resume_topology_mismatch detail -> "execution resume topology mismatch: " ^ detail
  | Invalid_tool_result -> "execution settlement returned an invalid ToolResult"
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

let resume ~writer ~agent_name (locator : scope_locator) =
  match Writer.find_run writer locator.run_id with
  | Error error -> Error (Scope_unavailable error)
  | Ok None -> Error Run_not_found
  | Ok (Some view) ->
    (match Event.node_kind view.Journal.opened.value with
     | Event.Agent_run { agent_name = durable_agent_name }
       when String.equal agent_name durable_agent_name ->
       Ok { writer; run = view.Journal.run }
     | Event.Agent_run { agent_name = durable_agent_name } ->
       Error
         (Agent_identity_mismatch { expected = durable_agent_name; actual = agent_name })
     | Event.Agent_turn _
     | Event.Provider_attempt _
     | Event.Output_block _
     | Event.Tool_invocation _
     | Event.Tool_attempt -> Error Run_not_found)
;;

let resume_running ~writer ~agent_name locator =
  match resume ~writer ~agent_name locator with
  | Error _ as error -> error
  | Ok scope ->
    (match Writer.find_run writer locator.run_id with
     | Error error -> Error (Scope_unavailable error)
     | Ok (Some { status = Journal.Running; _ }) -> Ok scope
     | Ok (Some { status = Journal.Finished _; _ }) ->
       Error (Resume_topology_mismatch "execution run is already terminal")
     | Ok None -> Error Run_not_found)
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

let resume_turn scope ~ordinal =
  match Writer.find_node scope.writer (Journal.run_root scope.run) with
  | Error error -> Error (Scope_unavailable error)
  | Ok None -> Error Run_not_found
  | Ok (Some root) ->
    let matching =
      List.filter_map
        (fun (child : Event.node Journal.event_record) ->
           match Event.node_kind child.value with
           | Event.Agent_turn { ordinal = existing } when existing = ordinal ->
             Some (Event.node_id child.value)
           | Event.Agent_run _
           | Event.Agent_turn _
           | Event.Provider_attempt _
           | Event.Output_block _
           | Event.Tool_invocation _
           | Event.Tool_attempt -> None)
        root.children
    in
    (match matching with
     | [] -> Ok None
     | [ node ] ->
       (match Writer.find_node scope.writer node with
        | Error error -> Error (Scope_unavailable error)
        | Ok None -> Error (Resume_topology_mismatch "turn child disappeared")
        | Ok (Some { status = Journal.Open; _ }) -> Ok (Some { scope; node })
        | Ok (Some { status = Journal.Closed _; _ }) ->
          Error (Resume_topology_mismatch "requested turn is already closed"))
     | _ :: _ :: _ -> Error (Resume_topology_mismatch "duplicate turn ordinal"))
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

let resume_provider_attempt (turn : turn) =
  match Writer.find_node turn.scope.writer turn.node with
  | Error error -> Error (Scope_unavailable error)
  | Ok None -> Error (Resume_topology_mismatch "turn node disappeared")
  | Ok (Some view) ->
    let providers =
      List.filter_map
        (fun (child : Event.node Journal.event_record) ->
           match Event.node_kind child.value with
           | Event.Provider_attempt _ -> Some (Event.node_id child.value)
           | Event.Agent_run _
           | Event.Agent_turn _
           | Event.Output_block _
           | Event.Tool_invocation _
           | Event.Tool_attempt -> None)
        view.children
    in
    (match providers with
     | [] -> Ok None
     | [ node ] ->
       (match Writer.find_node turn.scope.writer node with
        | Error error -> Error (Scope_unavailable error)
        | Ok None -> Error (Resume_topology_mismatch "provider child disappeared")
        | Ok (Some { status = Journal.Open; _ }) -> Ok (Some { turn; node })
        | Ok (Some { status = Journal.Closed _; _ }) ->
          Error (Resume_topology_mismatch "requested provider attempt is already closed"))
     | _ :: _ :: _ ->
       Error (Resume_topology_mismatch "multiple provider attempts remain open"))
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
    |> Result.map (fun durable -> { durable; locator; scope })
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
      then Ok { durable; locator; scope }
      else Error Invocation_locator_mismatch)
;;

let invocation_equal left right =
  String.equal (Tool.Invocation.tool_use_id left) (Tool.Invocation.tool_use_id right)
  && Tool.Invocation.turn left = Tool.Invocation.turn right
  && Execution_tool_schedule.equal
       (Tool.Invocation.schedule left)
       (Tool.Invocation.schedule right)
;;

let find_invocation provider ~invocation ~tool_name ~input =
  let scope = provider.turn.scope in
  match Writer.find_node scope.writer provider.node with
  | Error error -> Error (Scope_unavailable error)
  | Ok None -> Error (Resume_topology_mismatch "provider attempt disappeared")
  | Ok (Some view) ->
    let planned_index = Tool.Invocation.planned_index invocation in
    let matching =
      List.filter_map
        (fun (child : Event.node Journal.event_record) ->
           match Event.node_kind child.value with
           | Event.Tool_invocation { schedule; _ }
             when schedule.planned_index = planned_index ->
             Some (Event.node_id child.value)
           | Event.Agent_run _
           | Event.Agent_turn _
           | Event.Provider_attempt _
           | Event.Output_block _
           | Event.Tool_invocation _
           | Event.Tool_attempt -> None)
        view.children
    in
    (match matching with
     | [] -> Ok None
     | [ node ] ->
       let locator = { run_id = Journal.run_id scope.run; node } in
       (match rebind_invocation scope locator with
        | Error _ as error -> error
        | Ok rebound
          when invocation_equal rebound.durable.invocation invocation
               && String.equal rebound.durable.tool_name tool_name
               && Yojson.Safe.equal rebound.durable.input input -> Ok (Some rebound)
        | Ok _ ->
          Error
            (Resume_topology_mismatch
               "tool occurrence identity differs from the restored Agent checkpoint"))
     | _ :: _ :: _ -> Error (Resume_topology_mismatch "duplicate tool occurrence"))
;;

let provider_invocations_settled provider =
  match Writer.find_node provider.turn.scope.writer provider.node with
  | Error error -> Error (Scope_unavailable error)
  | Ok None -> Error (Resume_topology_mismatch "provider attempt disappeared")
  | Ok (Some view) ->
    let invocation_nodes =
      List.filter_map
        (fun (child : Event.node Journal.event_record) ->
           match Event.node_kind child.value with
           | Event.Tool_invocation _ -> Some (Event.node_id child.value)
           | Event.Agent_run _
           | Event.Agent_turn _
           | Event.Provider_attempt _
           | Event.Output_block _
           | Event.Tool_attempt -> None)
        view.children
    in
    let rec all_settled = function
      | [] -> Ok true
      | node :: rest ->
        (match Writer.find_node provider.turn.scope.writer node with
         | Error error -> Error (Scope_unavailable error)
         | Ok None -> Error (Resume_topology_mismatch "tool invocation disappeared")
         | Ok
             (Some
                { materialized = Journal.Tool_invocation_state { result = Some _; _ }; _ })
           -> all_settled rest
         | Ok
             (Some
                { materialized = Journal.Tool_invocation_state { result = None; _ }; _ })
           -> Ok false
         | Ok (Some _) ->
           Error (Resume_topology_mismatch "provider child changed node kind"))
    in
    (match invocation_nodes with
     | [] -> Ok false
     | nodes -> all_settled nodes)
;;

let tool_result_of_block durable = function
  | Llm_provider.Types.ToolResult { tool_use_id; content; outcome; _ }
    when String.equal
           tool_use_id
           (Tool.Invocation.tool_use_id durable.Settlement.invocation) ->
    Ok
      { invocation = durable.invocation
      ; tool_name = durable.tool_name
      ; input = durable.input
      ; content
      ; outcome
      }
  | Llm_provider.Types.Text _
  | Llm_provider.Types.Thinking _
  | Llm_provider.Types.ReasoningDetails _
  | Llm_provider.Types.RedactedThinking _
  | Llm_provider.Types.ToolUse _
  | Llm_provider.Types.ToolResult _
  | Llm_provider.Types.Image _
  | Llm_provider.Types.Document _
  | Llm_provider.Types.Audio _ -> Error Invalid_tool_result
;;

let execute_phased invocation ~invoke =
  let durable = invocation.durable in
  let settled =
    Settlement.execute_with_attempt_phased
      durable.authority
      ~invoke:(fun parent_attempt ->
        let start_child ~agent_name =
          transact invocation.scope.writer (Tx.start_run ~parent_attempt ~agent_name ())
          |> Result.map (fun (run, _event) -> { writer = invocation.scope.writer; run })
        in
        let (content, outcome), after_settle =
          invoke ~start_child ~tool_name:durable.tool_name ~input:durable.input
        in
        Settlement.phased_effect
          ~result:
            (Llm_provider.Types.ToolResult
               { tool_use_id = Tool.Invocation.tool_use_id durable.invocation
               ; content
               ; outcome
               ; json = None
               ; content_blocks = None
               })
          ~after_settle)
    |> Result.map_error (fun error -> Settlement_failed error)
  in
  match settled with
  | Error _ as error -> error
  | Ok (Settlement.Executed (block, through, event_count)) ->
    tool_result_of_block durable block
    |> Result.map (fun result -> Executed (result, through, event_count))
  | Ok (Settlement.Replayed block) ->
    tool_result_of_block durable block |> Result.map (fun result -> Replayed result)
;;

let execute invocation ~invoke =
  execute_phased invocation ~invoke:(fun ~start_child ~tool_name ~input ->
    invoke ~start_child ~tool_name ~input, Fun.id)
;;

let close_node writer node terminal =
  transact writer (Tx.close_node ~node terminal) |> Result.map ignore
;;

let close_provider_attempt provider terminal =
  close_node provider.turn.scope.writer provider.node terminal
;;

let close_turn (turn : turn) terminal = close_node turn.scope.writer turn.node terminal

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
