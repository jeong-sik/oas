open Result_syntax
module Event = Execution_event

module Event_id_set = Set.Make (struct
    type t = Event.Event_id.t

    let compare = Event.Event_id.compare
  end)

module Node_id_map = Map.Make (struct
    type t = Event.Node_id.t

    let compare = Event.Node_id.compare
  end)

module Node_id_set = Set.Make (struct
    type t = Event.Node_id.t

    let compare = Event.Node_id.compare
  end)

module Run_id_map = Map.Make (struct
    type t = Event.Run_id.t

    let compare = Event.Run_id.compare
  end)

type run =
  { id : Event.Run_id.t
  ; root : Event.Node_id.t
  }

let run_id run = run.id
let run_root run = run.root

let equal_run left right =
  Event.Run_id.equal left.id right.id && Event.Node_id.equal left.root right.root
;;

type node_status =
  | Open
  | Closed of Event.terminal
[@@deriving show]

type node_view =
  { node : Event.node
  ; status : node_status
  }

type run_status =
  | Running
  | Finished of Event.terminal
[@@deriving show]

type run_view =
  { run : run
  ; parent_invocation : Event.Node_id.t option
  ; status : run_status
  }

type invariant_violation =
  | Sequence_mismatch of
      { expected : int
      ; actual : int
      }
  | Duplicate_event_id of Event.Event_id.t
  | Unknown_parent_event of Event.Event_id.t
  | Event_run_mismatch of
      { envelope_run_id : Event.Run_id.t
      ; payload_run_id : Event.Run_id.t
      }
  | Duplicate_node_id of Event.Node_id.t
  | Unknown_node of Event.Node_id.t
  | Duplicate_run_id of Event.Run_id.t
  | Unknown_run of Event.Run_id.t
  | Run_already_finished of Event.Run_id.t
  | Node_already_closed of Event.Node_id.t
  | Parent_required of Event.Node_id.t
  | Unknown_parent_node of Event.Node_id.t
  | Parent_node_closed of Event.Node_id.t
  | Cross_run_parent of
      { node_run_id : Event.Run_id.t
      ; parent_run_id : Event.Run_id.t
      }
  | Invalid_parent_kind of
      { parent : Event.Node_id.t
      ; child : Event.Node_id.t
      }
  | Invalid_child_run_parent of Event.Node_id.t
  | Root_parent_event_mismatch
  | Parent_event_mismatch of
      { expected : Event.Event_id.t
      ; actual : Event.Event_id.t option
      }
  | Invalid_update_for_node of Event.Node_id.t
  | Output_snapshot_already_materialized of Event.Node_id.t
  | Output_delta_after_snapshot of Event.Node_id.t
  | Tool_input_already_materialized of Event.Node_id.t
  | Tool_input_delta_after_snapshot of Event.Node_id.t
  | Tool_input_not_materialized of Event.Node_id.t
  | Tool_result_already_materialized of Event.Node_id.t
  | Tool_result_while_children_open of Event.Node_id.t
  | Tool_result_not_materialized of Event.Node_id.t
  | Child_after_tool_result of Event.Node_id.t
  | Node_has_open_children of Event.Node_id.t
  | Run_has_open_nodes of Event.Run_id.t
  | Root_must_use_finish_run of Event.Node_id.t
  | Agent_run_must_use_start_run
  | Top_level_run_already_exists
[@@deriving show]

type error =
  | Invalid_argument of string
  | Invalid_event of string
  | Invariant_violation of invariant_violation

let error_to_string = function
  | Invalid_argument detail -> "invalid execution journal argument: " ^ detail
  | Invalid_event detail -> "invalid execution event: " ^ detail
  | Invariant_violation violation -> show_invariant_violation violation
;;

module Reducer = struct
  type node_record =
    { view : node_view
    ; last_event_id : Event.Event_id.t
    ; open_children : Node_id_set.t
    ; output_snapshot_materialized : bool
    ; tool_input_materialized : bool
    ; tool_result_materialized : bool
    }

  type run_record =
    { view : run_view
    ; open_nodes : Node_id_set.t
    }

  type t =
    { last_seq : int
    ; event_ids : Event_id_set.t
    ; nodes : node_record Node_id_map.t
    ; runs : run_record Run_id_map.t
    }

  let empty =
    { last_seq = 0
    ; event_ids = Event_id_set.empty
    ; nodes = Node_id_map.empty
    ; runs = Run_id_map.empty
    }
  ;;

  let last_seq state = state.last_seq
  let find_node_record state node_id = Node_id_map.find_opt node_id state.nodes

  let find_node state node_id =
    Option.map
      (fun (record : node_record) -> record.view)
      (find_node_record state node_id)
  ;;

  let find_run_record state run_id = Run_id_map.find_opt run_id state.runs

  let find_run state run_id =
    Option.map (fun (record : run_record) -> record.view) (find_run_record state run_id)
  ;;

  let latest_node_event state node_id =
    Option.map (fun record -> record.last_event_id) (find_node_record state node_id)
  ;;

  let event_id_of_string value =
    match Event.Event_id.of_string value with
    | Ok event_id -> event_id
    | Error detail -> invalid_arg ("Execution_journal.Reducer: " ^ detail)
  ;;

  let optional_event_id = function
    | None -> None
    | Some value -> Some (event_id_of_string value)
  ;;

  let referenced_event_ids event =
    let envelope = Event.envelope event in
    [ optional_event_id envelope.parent_event_id; optional_event_id envelope.caused_by ]
    |> List.filter_map Fun.id
  ;;

  let validate_event_references state event =
    match
      List.find_opt
        (fun event_id -> not (Event_id_set.mem event_id state.event_ids))
        (referenced_event_ids event)
    with
    | None -> Ok ()
    | Some event_id -> Error (Unknown_parent_event event_id)
  ;;

  let event_parent event = optional_event_id (Event.envelope event).parent_event_id

  let validate_root_parent event =
    match event_parent event with
    | None -> Ok ()
    | Some _ -> Error Root_parent_event_mismatch
  ;;

  let validate_parent_event event expected =
    let actual = event_parent event in
    match actual with
    | Some actual when Event.Event_id.equal expected actual -> Ok ()
    | None | Some _ -> Error (Parent_event_mismatch { expected; actual })
  ;;

  let validate_event_run event payload_run_id =
    let envelope_run_id = Event.run_id event in
    if Event.Run_id.equal envelope_run_id payload_run_id
    then Ok ()
    else Error (Event_run_mismatch { envelope_run_id; payload_run_id })
  ;;

  let ensure_node_open node_id (record : node_record) =
    match record.view.status with
    | Open -> Ok ()
    | Closed _ -> Error (Node_already_closed node_id)
  ;;

  let ensure_run_running run_id (record : run_record) =
    match record.view.status with
    | Running -> Ok ()
    | Finished _ -> Error (Run_already_finished run_id)
  ;;

  let parent_accepts_child parent_kind child_kind =
    match parent_kind, child_kind with
    | Event.Agent_run _, Event.Provider_turn _ -> true
    | Event.Provider_turn _, (Event.Output_block _ | Event.Tool_invocation _) -> true
    | Event.Tool_invocation _, Event.Tool_attempt -> true
    | ( ( Event.Agent_run _
        | Event.Provider_turn _
        | Event.Output_block _
        | Event.Tool_invocation _
        | Event.Tool_attempt )
      , _ ) -> false
  ;;

  let validate_update (record : node_record) node_id update =
    match Event.node_kind record.view.node, update with
    | Event.Provider_turn _, Event.Provider_event _ -> Ok ()
    | Event.Output_block _, Event.Output_delta _ ->
      if record.output_snapshot_materialized
      then Error (Output_delta_after_snapshot node_id)
      else Ok ()
    | Event.Output_block _, Event.Output_snapshot _ ->
      if record.output_snapshot_materialized
      then Error (Output_snapshot_already_materialized node_id)
      else Ok ()
    | Event.Tool_invocation _, Event.Tool_input_delta _ ->
      if record.tool_input_materialized
      then Error (Tool_input_delta_after_snapshot node_id)
      else Ok ()
    | Event.Tool_invocation _, Event.Tool_input_snapshot _ ->
      if record.tool_input_materialized
      then Error (Tool_input_already_materialized node_id)
      else Ok ()
    | Event.Tool_invocation _, Event.Tool_result _ ->
      if not record.tool_input_materialized
      then Error (Tool_input_not_materialized node_id)
      else if record.tool_result_materialized
      then Error (Tool_result_already_materialized node_id)
      else if not (Node_id_set.is_empty record.open_children)
      then Error (Tool_result_while_children_open node_id)
      else Ok ()
    | Event.Tool_attempt, Event.Tool_progress _ -> Ok ()
    | ( ( Event.Agent_run _
        | Event.Provider_turn _
        | Event.Output_block _
        | Event.Tool_invocation _
        | Event.Tool_attempt )
      , _ ) -> Error (Invalid_update_for_node node_id)
  ;;

  let add_node state node event_id =
    let node_id = Event.node_id node in
    let record =
      { view = { node; status = Open }
      ; last_event_id = event_id
      ; open_children = Node_id_set.empty
      ; output_snapshot_materialized = false
      ; tool_input_materialized =
          (match Event.node_kind node with
           | Event.Tool_invocation { input = Some _; _ } -> true
           | Event.Agent_run _ | Event.Provider_turn _ | Event.Output_block _
           | Event.Tool_invocation { input = None; _ }
           | Event.Tool_attempt -> false)
      ; tool_result_materialized = false
      }
    in
    let nodes = Node_id_map.add node_id record state.nodes in
    let nodes =
      match Event.parent_node_id node with
      | None -> nodes
      | Some parent_id ->
        (match Node_id_map.find_opt parent_id nodes with
         | None ->
           invalid_arg "Execution_journal.Reducer: validated parent node disappeared"
         | Some parent_record ->
           let parent_record =
             { parent_record with
               open_children = Node_id_set.add node_id parent_record.open_children
             }
           in
           Node_id_map.add parent_id parent_record nodes)
    in
    { state with nodes }
  ;;

  let open_agent_run state event node =
    let node_id = Event.node_id node in
    let run_id = Event.node_run_id node in
    if Run_id_map.mem run_id state.runs
    then Error (Duplicate_run_id run_id)
    else
      let* parent_invocation =
        match Event.parent_node_id node with
        | None ->
          let* () = validate_root_parent event in
          let+ () =
            if Run_id_map.is_empty state.runs
            then Ok ()
            else Error Top_level_run_already_exists
          in
          None
        | Some parent_id ->
          (match find_node_record state parent_id with
           | None -> Error (Unknown_parent_node parent_id)
           | Some parent_record ->
             let* () = ensure_node_open parent_id parent_record in
             (match Event.node_kind parent_record.view.node with
              | Event.Tool_invocation _ ->
                if not parent_record.tool_input_materialized
                then Error (Tool_input_not_materialized parent_id)
                else if parent_record.tool_result_materialized
                then Error (Child_after_tool_result parent_id)
                else
                  let+ () = validate_parent_event event parent_record.last_event_id in
                  Some parent_id
              | _ -> Error (Invalid_child_run_parent parent_id)))
      in
      let run = { id = run_id; root = node_id } in
      let run_record =
        { view = { run; parent_invocation; status = Running }
        ; open_nodes = Node_id_set.singleton node_id
        }
      in
      let state = add_node state node (Event.event_id event) in
      Ok { state with runs = Run_id_map.add run_id run_record state.runs }
  ;;

  let open_non_root state event node =
    let node_id = Event.node_id node in
    let run_id = Event.node_run_id node in
    let* run_record =
      match find_run_record state run_id with
      | None -> Error (Unknown_run run_id)
      | Some run_record -> Ok run_record
    in
    let* () = ensure_run_running run_id run_record in
    let* parent_id =
      match Event.parent_node_id node with
      | None -> Error (Parent_required node_id)
      | Some parent_id -> Ok parent_id
    in
    let* parent_record =
      match find_node_record state parent_id with
      | None -> Error (Unknown_parent_node parent_id)
      | Some parent_record -> Ok parent_record
    in
    let* () = ensure_node_open parent_id parent_record in
    let parent_run_id = Event.node_run_id parent_record.view.node in
    let* () =
      if Event.Run_id.equal run_id parent_run_id
      then Ok ()
      else Error (Cross_run_parent { node_run_id = run_id; parent_run_id })
    in
    let* () =
      if
        parent_accepts_child
          (Event.node_kind parent_record.view.node)
          (Event.node_kind node)
      then Ok ()
      else Error (Invalid_parent_kind { parent = parent_id; child = node_id })
    in
    let* () =
      match Event.node_kind parent_record.view.node, Event.node_kind node with
      | Event.Tool_invocation _, Event.Tool_attempt
        when not parent_record.tool_input_materialized ->
        Error (Tool_input_not_materialized parent_id)
      | Event.Tool_invocation _, Event.Tool_attempt
        when parent_record.tool_result_materialized ->
        Error (Child_after_tool_result parent_id)
      | _ -> Ok ()
    in
    let* () = validate_parent_event event parent_record.last_event_id in
    let state = add_node state node (Event.event_id event) in
    let run_record =
      { run_record with open_nodes = Node_id_set.add node_id run_record.open_nodes }
    in
    Ok { state with runs = Run_id_map.add run_id run_record state.runs }
  ;;

  let apply_open state event node =
    let node_id = Event.node_id node in
    let* () = validate_event_run event (Event.node_run_id node) in
    if Node_id_map.mem node_id state.nodes
    then Error (Duplicate_node_id node_id)
    else (
      match Event.node_kind node with
      | Event.Agent_run _ -> open_agent_run state event node
      | Event.Provider_turn _
      | Event.Output_block _
      | Event.Tool_invocation _
      | Event.Tool_attempt -> open_non_root state event node)
  ;;

  let apply_update state event node_id update =
    let* record =
      match find_node_record state node_id with
      | None -> Error (Unknown_node node_id)
      | Some record -> Ok record
    in
    let* () = ensure_node_open node_id record in
    let* () = validate_event_run event (Event.node_run_id record.view.node) in
    let* () = validate_parent_event event record.last_event_id in
    let* () = validate_update record node_id update in
    let updated =
      { record with
        last_event_id = Event.event_id event
      ; output_snapshot_materialized =
          (match update with
           | Event.Output_snapshot _ -> true
           | Event.Provider_event _
           | Event.Output_delta _
           | Event.Tool_input_delta _
           | Event.Tool_input_snapshot _
           | Event.Tool_progress _
           | Event.Tool_result _ -> record.output_snapshot_materialized)
      ; tool_input_materialized =
          (match update with
           | Event.Tool_input_snapshot _ -> true
           | Event.Provider_event _
           | Event.Output_delta _
           | Event.Output_snapshot _
           | Event.Tool_input_delta _
           | Event.Tool_progress _
           | Event.Tool_result _ -> record.tool_input_materialized)
      ; tool_result_materialized =
          (match update with
           | Event.Tool_result _ -> true
           | Event.Provider_event _
           | Event.Output_delta _
           | Event.Output_snapshot _
           | Event.Tool_input_delta _
           | Event.Tool_input_snapshot _
           | Event.Tool_progress _ -> record.tool_result_materialized)
      }
    in
    Ok { state with nodes = Node_id_map.add node_id updated state.nodes }
  ;;

  let detach_from_parent nodes node =
    match Event.parent_node_id node with
    | None -> nodes
    | Some parent_id ->
      (match Node_id_map.find_opt parent_id nodes with
       | None ->
         invalid_arg "Execution_journal.Reducer: validated parent node disappeared"
       | Some parent_record ->
         let parent_record =
           { parent_record with
             open_children =
               Node_id_set.remove (Event.node_id node) parent_record.open_children
           }
         in
         Node_id_map.add parent_id parent_record nodes)
  ;;

  let apply_close state event node_id terminal =
    let* record =
      match find_node_record state node_id with
      | None -> Error (Unknown_node node_id)
      | Some record -> Ok record
    in
    let* () = ensure_node_open node_id record in
    let run_id = Event.node_run_id record.view.node in
    let* () = validate_event_run event run_id in
    let* () = validate_parent_event event record.last_event_id in
    let* () =
      if not (Node_id_set.is_empty record.open_children)
      then Error (Node_has_open_children node_id)
      else Ok ()
    in
    let* () =
      match Event.node_kind record.view.node, terminal with
      | Event.Tool_invocation _, Event.Succeeded when not record.tool_input_materialized
        -> Error (Tool_input_not_materialized node_id)
      | Event.Tool_invocation _, Event.Succeeded when not record.tool_result_materialized
        -> Error (Tool_result_not_materialized node_id)
      | _ -> Ok ()
    in
    let closed_record =
      { view = { record.view with status = Closed terminal }
      ; last_event_id = Event.event_id event
      ; open_children = record.open_children
      ; output_snapshot_materialized = record.output_snapshot_materialized
      ; tool_input_materialized = record.tool_input_materialized
      ; tool_result_materialized = record.tool_result_materialized
      }
    in
    let nodes =
      Node_id_map.add node_id closed_record state.nodes
      |> fun nodes -> detach_from_parent nodes record.view.node
    in
    match Event.node_kind record.view.node with
    | Event.Agent_run _ ->
      let* run_record =
        match find_run_record state run_id with
        | None -> Error (Unknown_run run_id)
        | Some run_record -> Ok run_record
      in
      let* () = ensure_run_running run_id run_record in
      let* () =
        if Event.Node_id.equal run_record.view.run.root node_id
        then Ok ()
        else Error Agent_run_must_use_start_run
      in
      let* () =
        if
          not
            (Node_id_set.equal
               run_record.open_nodes
               (Node_id_set.singleton run_record.view.run.root))
        then Error (Run_has_open_nodes run_id)
        else Ok ()
      in
      let finished_run =
        { view = { run_record.view with status = Finished terminal }
        ; open_nodes = Node_id_set.empty
        }
      in
      Ok { state with nodes; runs = Run_id_map.add run_id finished_run state.runs }
    | Event.Provider_turn _
    | Event.Output_block _
    | Event.Tool_invocation _
    | Event.Tool_attempt ->
      let* run_record =
        match find_run_record state run_id with
        | None -> Error (Unknown_run run_id)
        | Some run_record -> Ok run_record
      in
      let run_record =
        { run_record with open_nodes = Node_id_set.remove node_id run_record.open_nodes }
      in
      Ok { state with nodes; runs = Run_id_map.add run_id run_record state.runs }
  ;;

  let apply_payload state event =
    match Event.payload event with
    | Event.Node_opened node -> apply_open state event node
    | Event.Node_updated { node_id; update } -> apply_update state event node_id update
    | Event.Node_closed { node_id; terminal } -> apply_close state event node_id terminal
  ;;

  let apply state event =
    let expected = state.last_seq + 1 in
    let actual = Event.seq event in
    if actual <> expected
    then Error (Sequence_mismatch { expected; actual })
    else (
      let event_id = Event.event_id event in
      if Event_id_set.mem event_id state.event_ids
      then Error (Duplicate_event_id event_id)
      else
        let* () = validate_event_references state event in
        let* state = apply_payload state event in
        Ok
          { state with
            last_seq = actual
          ; event_ids = Event_id_set.add event_id state.event_ids
          })
  ;;
end

type journal_state =
  { reducer : Reducer.t
  ; events_rev : Event.t list
  }

type t =
  { mu : Eio.Mutex.t
  ; mutable state : journal_state
  }

let with_read journal f = Eio.Mutex.use_ro journal.mu (fun () -> f journal.state)

let with_write journal f =
  Eio.Mutex.use_rw ~protect:true journal.mu (fun () -> f journal.state)
;;

let length journal = with_read journal (fun state -> Reducer.last_seq state.reducer)
let last_seq journal = with_read journal (fun state -> Reducer.last_seq state.reducer)

let events journal =
  let events_rev = with_read journal (fun state -> state.events_rev) in
  List.rev events_rev
;;

let events_after journal ~after_seq =
  if after_seq < 0
  then Error (Invalid_argument "after_seq must be non-negative")
  else (
    let events_rev = with_read journal (fun state -> state.events_rev) in
    let rec collect chronological = function
      | event :: rest when Event.seq event > after_seq ->
        collect (event :: chronological) rest
      | _ -> chronological
    in
    Ok (collect [] events_rev))
;;

let find_node journal node_id =
  with_read journal (fun state -> Reducer.find_node state.reducer node_id)
;;

let find_run journal run_id =
  with_read journal (fun state -> Reducer.find_run state.reducer run_id)
;;

let append_locked journal state ~run_id ~parent_event_id payload =
  let event_id = Event.Event_id.fresh () in
  let parent_event_string = Option.map Event.Event_id.to_string parent_event_id in
  let envelope =
    Event_envelope.make
      ~event_id:(Event.Event_id.to_string event_id)
      ~correlation_id:(Event.Run_id.to_string run_id)
      ~run_id:(Event.Run_id.to_string run_id)
      ~seq:(Reducer.last_seq state.reducer + 1)
      ?parent_event_id:parent_event_string
      ?caused_by:parent_event_string
      ~source_clock:Event_envelope.Wall
      ()
  in
  let* event =
    match Event.make ~envelope ~payload with
    | Ok event -> Ok event
    | Error detail -> Error (Invalid_event detail)
  in
  let* reducer =
    match Reducer.apply state.reducer event with
    | Ok reducer -> Ok reducer
    | Error violation -> Error (Invariant_violation violation)
  in
  journal.state <- { reducer; events_rev = event :: state.events_rev };
  Ok event
;;

let node_record_or_error state node_id =
  match Reducer.find_node state.reducer node_id with
  | None -> Error (Invariant_violation (Unknown_node node_id))
  | Some view -> Ok view
;;

let node_last_event state node_id =
  match Reducer.latest_node_event state.reducer node_id with
  | None -> Error (Invariant_violation (Unknown_node node_id))
  | Some event_id -> Ok event_id
;;

let start_run ?parent_invocation journal ~agent_name =
  with_write journal (fun state ->
    let run_id = Event.Run_id.fresh () in
    let root = Event.Node_id.fresh () in
    let* parent_event_id =
      match parent_invocation with
      | None -> Ok None
      | Some node_id ->
        let+ event_id = node_last_event state node_id in
        Some event_id
    in
    let* node =
      match
        Event.make_node
          ~node_id:root
          ~run_id
          ~parent_node_id:parent_invocation
          ~kind:(Event.Agent_run { agent_name })
      with
      | Ok node -> Ok node
      | Error detail -> Error (Invalid_argument detail)
    in
    let+ _ =
      append_locked journal state ~run_id ~parent_event_id (Event.Node_opened node)
    in
    { id = run_id; root })
;;

let validate_run_handle state run =
  match Reducer.find_run state.reducer run.id with
  | None -> Error (Invariant_violation (Unknown_run run.id))
  | Some view when not (equal_run view.run run) ->
    Error (Invalid_argument "run handle does not match the journal run root")
  | Some { status = Finished _; _ } ->
    Error (Invariant_violation (Run_already_finished run.id))
  | Some { status = Running; _ } -> Ok ()
;;

let open_node journal ~run ~parent ~kind =
  with_write journal (fun state ->
    let* () = validate_run_handle state run in
    match kind with
    | Event.Agent_run _ -> Error (Invariant_violation Agent_run_must_use_start_run)
    | Event.Provider_turn _
    | Event.Output_block _
    | Event.Tool_invocation _
    | Event.Tool_attempt ->
      let node_id = Event.Node_id.fresh () in
      let* parent_event_id = node_last_event state parent in
      let* node =
        match
          Event.make_node ~node_id ~run_id:run.id ~parent_node_id:(Some parent) ~kind
        with
        | Ok node -> Ok node
        | Error detail -> Error (Invalid_argument detail)
      in
      let+ _ =
        append_locked
          journal
          state
          ~run_id:run.id
          ~parent_event_id:(Some parent_event_id)
          (Event.Node_opened node)
      in
      node_id)
;;

let update_node journal ~node update =
  with_write journal (fun state ->
    let* view = node_record_or_error state node in
    let* parent_event_id = node_last_event state node in
    append_locked
      journal
      state
      ~run_id:(Event.node_run_id view.node)
      ~parent_event_id:(Some parent_event_id)
      (Event.Node_updated { node_id = node; update }))
;;

let close_node journal ~node terminal =
  with_write journal (fun state ->
    let* view = node_record_or_error state node in
    match Event.node_kind view.node with
    | Event.Agent_run _ -> Error (Invariant_violation (Root_must_use_finish_run node))
    | Event.Provider_turn _
    | Event.Output_block _
    | Event.Tool_invocation _
    | Event.Tool_attempt ->
      let* parent_event_id = node_last_event state node in
      append_locked
        journal
        state
        ~run_id:(Event.node_run_id view.node)
        ~parent_event_id:(Some parent_event_id)
        (Event.Node_closed { node_id = node; terminal }))
;;

let finish_run journal ~run terminal =
  with_write journal (fun state ->
    let* () = validate_run_handle state run in
    let* parent_event_id = node_last_event state run.root in
    append_locked
      journal
      state
      ~run_id:run.id
      ~parent_event_id:(Some parent_event_id)
      (Event.Node_closed { node_id = run.root; terminal }))
;;

let create () =
  { mu = Eio.Mutex.create (); state = { reducer = Reducer.empty; events_rev = [] } }
;;
