open Result_syntax
module Event = Execution_event
module Store = Execution_event_store

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

type 'a event_record =
  { event : Event.t
  ; value : 'a
  }

type node_status =
  | Open
  | Closed of Event.terminal event_record

type materialized =
  | Agent_run_state
  | Agent_turn_state
  | Provider_attempt_state of { provider_response_id : string option }
  | Output_block_state of { snapshot : Llm_provider.Types.content_block option }
  | Tool_invocation_state of
      { input : Llm_provider.Types.content_block option
      ; result : Llm_provider.Types.content_block option
      }
  | Tool_attempt_state

type node_view =
  { node : Event.node
  ; opened : Event.node event_record
  ; status : node_status
  ; updates : Event.node_update event_record list
  ; children : Event.node event_record list
  ; materialized : materialized
  ; through_seq : int
  }

type run_status =
  | Running
  | Finished of Event.terminal event_record

type run_view =
  { run : run
  ; opened : Event.node event_record
  ; parent_invocation : Event.Node_id.t option
  ; status : run_status
  ; through_seq : int
  }

type cursor = Store.cursor

let cursor_seq = Store.cursor_seq
let cursor_to_yojson = Store.cursor_to_yojson
let cursor_of_yojson = Store.cursor_of_yojson

type invariant_violation =
  | Sequence_mismatch of
      { expected : int
      ; actual : int
      }
  | Duplicate_event_id of Event.Event_id.t
  | Unknown_parent_event of Event.Event_id.t
  | Unknown_cause_event of Event.Event_id.t
  | Correlation_mismatch of
      { expected : Event.Correlation_id.t
      ; actual : Event.Correlation_id.t
      }
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
  | Provider_response_id_already_materialized of Event.Node_id.t
  | Output_snapshot_already_materialized of Event.Node_id.t
  | Output_snapshot_kind_mismatch of
      { node : Event.Node_id.t
      ; expected : Event.output_block_kind
      ; actual : Event.content_block_classification
      }
  | Output_delta_after_snapshot of Event.Node_id.t
  | Output_snapshot_not_materialized of Event.Node_id.t
  | Tool_input_already_materialized of Event.Node_id.t
  | Tool_input_snapshot_not_tool_use of Event.Node_id.t
  | Tool_input_snapshot_identity_mismatch of Event.Node_id.t
  | Tool_input_delta_after_snapshot of Event.Node_id.t
  | Tool_input_not_materialized of Event.Node_id.t
  | Tool_result_already_materialized of Event.Node_id.t
  | Tool_result_snapshot_not_tool_result of Event.Node_id.t
  | Tool_result_snapshot_identity_mismatch of Event.Node_id.t
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
  | Identity_failure of string
  | Cursor_scope_mismatch
  | Cursor_ahead of
      { after_seq : int
      ; last_seq : int
      }
  | Persistence_failure of Store.error
  | Invariant_violation of invariant_violation

let error_to_string = function
  | Invalid_argument detail -> "invalid execution journal argument: " ^ detail
  | Invalid_event detail -> "invalid execution event: " ^ detail
  | Identity_failure detail -> "execution journal identity allocation failed: " ^ detail
  | Cursor_scope_mismatch -> "execution journal cursor belongs to another scope"
  | Cursor_ahead { after_seq; last_seq } ->
    Printf.sprintf
      "execution journal cursor %d is ahead of last sequence %d"
      after_seq
      last_seq
  | Persistence_failure error ->
    "execution journal persistence failed: " ^ Store.error_to_string error
  | Invariant_violation violation -> show_invariant_violation violation
;;

module Reducer = struct
  type node_record =
    { node : Event.node
    ; opened : Event.node event_record
    ; status : node_status
    ; last_event_id : Event.Event_id.t
    ; open_children : Node_id_set.t
    ; children_rev : Event.node event_record list
    ; updates_rev : Event.node_update event_record list
    ; provider_response_id : string option
    ; output_snapshot : Llm_provider.Types.content_block option
    ; tool_input : Llm_provider.Types.content_block option
    ; tool_result : Llm_provider.Types.content_block option
    }

  type run_record =
    { view : run_view
    ; open_nodes : Node_id_set.t
    }

  type t =
    { last_seq : int
    ; correlation_id : Event.Correlation_id.t option
    ; event_ids : Event_id_set.t
    ; nodes : node_record Node_id_map.t
    ; runs : run_record Run_id_map.t
    }

  let empty =
    { last_seq = 0
    ; correlation_id = None
    ; event_ids = Event_id_set.empty
    ; nodes = Node_id_map.empty
    ; runs = Run_id_map.empty
    }
  ;;

  let last_seq state = state.last_seq
  let correlation_id state = state.correlation_id
  let find_node_record state node_id = Node_id_map.find_opt node_id state.nodes

  let materialized (record : node_record) =
    match Event.node_kind record.node with
    | Event.Agent_run _ -> Agent_run_state
    | Event.Agent_turn _ -> Agent_turn_state
    | Event.Provider_attempt _ ->
      Provider_attempt_state { provider_response_id = record.provider_response_id }
    | Event.Output_block _ -> Output_block_state { snapshot = record.output_snapshot }
    | Event.Tool_invocation _ ->
      Tool_invocation_state { input = record.tool_input; result = record.tool_result }
    | Event.Tool_attempt -> Tool_attempt_state
  ;;

  let node_view state (record : node_record) =
    { node = record.node
    ; opened = record.opened
    ; status = record.status
    ; updates = List.rev record.updates_rev
    ; children = List.rev record.children_rev
    ; materialized = materialized record
    ; through_seq = state.last_seq
    }
  ;;

  let find_node state node_id =
    Option.map (node_view state) (find_node_record state node_id)
  ;;

  let find_run_record state run_id = Run_id_map.find_opt run_id state.runs

  let find_run state run_id =
    Option.map
      (fun (record : run_record) -> { record.view with through_seq = state.last_seq })
      (find_run_record state run_id)
  ;;

  let latest_node_event state node_id =
    Option.map (fun record -> record.last_event_id) (find_node_record state node_id)
  ;;

  let open_subtree_postorder state root =
    let open_children_chronological (record : node_record) =
      record.children_rev
      |> List.rev
      |> List.filter_map (fun child ->
        let child_id = Event.node_id child.value in
        if Node_id_set.mem child_id record.open_children then Some child_id else None)
    in
    let rec loop work reverse_postorder =
      match work with
      | [] -> Ok (List.rev reverse_postorder)
      | `Exit node_id :: rest -> loop rest (node_id :: reverse_postorder)
      | `Enter node_id :: rest ->
        (match find_node_record state node_id with
         | None -> Error (Unknown_node node_id)
         | Some record ->
           let children = open_children_chronological record in
           let children_rev = List.rev_map (fun child -> `Enter child) children in
           let work = List.rev_append children_rev (`Exit node_id :: rest) in
           loop work reverse_postorder)
    in
    loop [ `Enter root ] []
  ;;

  let validate_event_references state event =
    match Event.parent_event_id event with
    | None -> Ok ()
    | Some event_id ->
      if Event_id_set.mem event_id state.event_ids
      then Ok ()
      else Error (Unknown_parent_event event_id)
  ;;

  let validate_event_causes state event =
    List.fold_left
      (fun result cause ->
         let* () = result in
         match cause with
         | Event.External_event _ -> Ok ()
         | Event.Internal_event event_id ->
           if Event_id_set.mem event_id state.event_ids
           then Ok ()
           else Error (Unknown_cause_event event_id))
      (Ok ())
      (Event.causes event)
  ;;

  let validate_correlation state event =
    let actual = Event.correlation_id event in
    match state.correlation_id with
    | None -> Ok ()
    | Some expected when Event.Correlation_id.equal expected actual -> Ok ()
    | Some expected -> Error (Correlation_mismatch { expected; actual })
  ;;

  let event_parent = Event.parent_event_id

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
    match record.status with
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
    | Event.Agent_run _, Event.Agent_turn _ -> true
    | Event.Agent_turn _, Event.Provider_attempt _ -> true
    | Event.Provider_attempt _, (Event.Output_block _ | Event.Tool_invocation _) -> true
    | Event.Tool_invocation _, Event.Tool_attempt -> true
    | ( ( Event.Agent_run _
        | Event.Agent_turn _
        | Event.Provider_attempt _
        | Event.Output_block _
        | Event.Tool_invocation _
        | Event.Tool_attempt )
      , _ ) -> false
  ;;

  let validate_update (record : node_record) node_id update =
    match Event.node_kind record.node, update with
    | Event.Provider_attempt _, Event.Provider_event _ -> Ok ()
    | Event.Provider_attempt _, Event.Provider_response_id_snapshot _ ->
      if Option.is_some record.provider_response_id
      then Error (Provider_response_id_already_materialized node_id)
      else Ok ()
    | Event.Output_block _, Event.Output_delta _ ->
      if Option.is_some record.output_snapshot
      then Error (Output_delta_after_snapshot node_id)
      else Ok ()
    | Event.Output_block { block_kind = expected; _ }, Event.Output_snapshot block ->
      if Option.is_some record.output_snapshot
      then Error (Output_snapshot_already_materialized node_id)
      else (
        let actual = Event.classify_content_block block in
        match actual with
        | Event.Output_content actual when actual = expected -> Ok ()
        | Event.Output_content _ | Event.Tool_use_content | Event.Tool_result_content ->
          Error (Output_snapshot_kind_mismatch { node = node_id; expected; actual }))
    | Event.Tool_invocation _, Event.Tool_input_delta _ ->
      if Option.is_some record.tool_input
      then Error (Tool_input_delta_after_snapshot node_id)
      else Ok ()
    | ( Event.Tool_invocation { provider_tool_use_id; tool_name; _ }
      , Event.Tool_input_snapshot block ) ->
      if Option.is_some record.tool_input
      then Error (Tool_input_already_materialized node_id)
      else (
        match block with
        | Llm_provider.Types.ToolUse { id; name; _ }
          when String.equal name tool_name
               && Option.fold ~none:true ~some:(String.equal id) provider_tool_use_id ->
          Ok ()
        | Llm_provider.Types.ToolUse _ ->
          Error (Tool_input_snapshot_identity_mismatch node_id)
        | Llm_provider.Types.Text _
        | Llm_provider.Types.Thinking _
        | Llm_provider.Types.ReasoningDetails _
        | Llm_provider.Types.RedactedThinking _
        | Llm_provider.Types.ToolResult _
        | Llm_provider.Types.Image _
        | Llm_provider.Types.Document _
        | Llm_provider.Types.Audio _ -> Error (Tool_input_snapshot_not_tool_use node_id))
    | Event.Tool_invocation _, Event.Tool_result block ->
      if Option.is_none record.tool_input
      then Error (Tool_input_not_materialized node_id)
      else if Option.is_some record.tool_result
      then Error (Tool_result_already_materialized node_id)
      else if not (Node_id_set.is_empty record.open_children)
      then Error (Tool_result_while_children_open node_id)
      else (
        match block, record.tool_input with
        | ( Llm_provider.Types.ToolResult { tool_use_id; _ }
          , Some (Llm_provider.Types.ToolUse { id; _ }) )
          when String.equal tool_use_id id -> Ok ()
        | Llm_provider.Types.ToolResult _, Some (Llm_provider.Types.ToolUse _) ->
          Error (Tool_result_snapshot_identity_mismatch node_id)
        | Llm_provider.Types.ToolResult _, (None | Some _) ->
          Error (Tool_input_not_materialized node_id)
        | ( ( Llm_provider.Types.Text _
            | Llm_provider.Types.Thinking _
            | Llm_provider.Types.ReasoningDetails _
            | Llm_provider.Types.RedactedThinking _
            | Llm_provider.Types.ToolUse _
            | Llm_provider.Types.Image _
            | Llm_provider.Types.Document _
            | Llm_provider.Types.Audio _ )
          , _ ) -> Error (Tool_result_snapshot_not_tool_result node_id))
    | Event.Tool_attempt, Event.Tool_progress _ -> Ok ()
    | ( ( Event.Agent_run _
        | Event.Agent_turn _
        | Event.Provider_attempt _
        | Event.Output_block _
        | Event.Tool_invocation _
        | Event.Tool_attempt )
      , _ ) -> Error (Invalid_update_for_node node_id)
  ;;

  let add_node state node event =
    let node_id = Event.node_id node in
    let opened = { event; value = node } in
    let record =
      { node
      ; opened
      ; status = Open
      ; last_event_id = Event.event_id event
      ; open_children = Node_id_set.empty
      ; children_rev = []
      ; updates_rev = []
      ; provider_response_id = None
      ; output_snapshot = None
      ; tool_input = None
      ; tool_result = None
      }
    in
    let nodes = Node_id_map.add node_id record state.nodes in
    let* nodes =
      match Event.parent_node_id node with
      | None -> Ok nodes
      | Some parent_id ->
        (match Node_id_map.find_opt parent_id nodes with
         | None -> Error (Unknown_parent_node parent_id)
         | Some parent_record ->
           let parent_record =
             { parent_record with
               open_children = Node_id_set.add node_id parent_record.open_children
             ; children_rev = opened :: parent_record.children_rev
             }
           in
           Ok (Node_id_map.add parent_id parent_record nodes))
    in
    Ok { state with nodes }
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
             (match Event.node_kind parent_record.node with
              | Event.Tool_invocation _ ->
                if Option.is_none parent_record.tool_input
                then Error (Tool_input_not_materialized parent_id)
                else if Option.is_some parent_record.tool_result
                then Error (Child_after_tool_result parent_id)
                else
                  let+ () = validate_parent_event event parent_record.last_event_id in
                  Some parent_id
              | _ -> Error (Invalid_child_run_parent parent_id)))
      in
      let run = { id = run_id; root = node_id } in
      let run_record =
        { view =
            { run
            ; opened = { event; value = node }
            ; parent_invocation
            ; status = Running
            ; through_seq = 0
            }
        ; open_nodes = Node_id_set.singleton node_id
        }
      in
      let* state = add_node state node event in
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
    let parent_run_id = Event.node_run_id parent_record.node in
    let* () =
      if Event.Run_id.equal run_id parent_run_id
      then Ok ()
      else Error (Cross_run_parent { node_run_id = run_id; parent_run_id })
    in
    let* () =
      if parent_accepts_child (Event.node_kind parent_record.node) (Event.node_kind node)
      then Ok ()
      else Error (Invalid_parent_kind { parent = parent_id; child = node_id })
    in
    let* () =
      match Event.node_kind parent_record.node, Event.node_kind node with
      | Event.Tool_invocation _, Event.Tool_attempt
        when Option.is_none parent_record.tool_input ->
        Error (Tool_input_not_materialized parent_id)
      | Event.Tool_invocation _, Event.Tool_attempt
        when Option.is_some parent_record.tool_result ->
        Error (Child_after_tool_result parent_id)
      | _ -> Ok ()
    in
    let* () = validate_parent_event event parent_record.last_event_id in
    let* state = add_node state node event in
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
      | Event.Agent_turn _
      | Event.Provider_attempt _
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
    let* () = validate_event_run event (Event.node_run_id record.node) in
    let* () = validate_parent_event event record.last_event_id in
    let* () = validate_update record node_id update in
    let updated =
      { record with
        last_event_id = Event.event_id event
      ; updates_rev = { event; value = update } :: record.updates_rev
      ; provider_response_id =
          (match update with
           | Event.Provider_response_id_snapshot value -> Some value
           | Event.Provider_event _
           | Event.Output_delta _
           | Event.Output_snapshot _
           | Event.Tool_input_delta _
           | Event.Tool_input_snapshot _
           | Event.Tool_progress _
           | Event.Tool_result _ -> record.provider_response_id)
      ; output_snapshot =
          (match update with
           | Event.Output_snapshot value -> Some value
           | Event.Provider_event _
           | Event.Provider_response_id_snapshot _
           | Event.Output_delta _
           | Event.Tool_input_delta _
           | Event.Tool_input_snapshot _
           | Event.Tool_progress _
           | Event.Tool_result _ -> record.output_snapshot)
      ; tool_input =
          (match update with
           | Event.Tool_input_snapshot value -> Some value
           | Event.Provider_event _
           | Event.Provider_response_id_snapshot _
           | Event.Output_delta _
           | Event.Output_snapshot _
           | Event.Tool_input_delta _
           | Event.Tool_progress _
           | Event.Tool_result _ -> record.tool_input)
      ; tool_result =
          (match update with
           | Event.Tool_result value -> Some value
           | Event.Provider_event _
           | Event.Provider_response_id_snapshot _
           | Event.Output_delta _
           | Event.Output_snapshot _
           | Event.Tool_input_delta _
           | Event.Tool_input_snapshot _
           | Event.Tool_progress _ -> record.tool_result)
      }
    in
    Ok { state with nodes = Node_id_map.add node_id updated state.nodes }
  ;;

  let detach_from_parent nodes node =
    match Event.parent_node_id node with
    | None -> Ok nodes
    | Some parent_id ->
      (match Node_id_map.find_opt parent_id nodes with
       | None -> Error (Unknown_parent_node parent_id)
       | Some parent_record ->
         let parent_record =
           { parent_record with
             open_children =
               Node_id_set.remove (Event.node_id node) parent_record.open_children
           }
         in
         Ok (Node_id_map.add parent_id parent_record nodes))
  ;;

  let apply_close state event node_id terminal =
    let* record =
      match find_node_record state node_id with
      | None -> Error (Unknown_node node_id)
      | Some record -> Ok record
    in
    let* () = ensure_node_open node_id record in
    let run_id = Event.node_run_id record.node in
    let* () = validate_event_run event run_id in
    let* () = validate_parent_event event record.last_event_id in
    let* () =
      if not (Node_id_set.is_empty record.open_children)
      then Error (Node_has_open_children node_id)
      else Ok ()
    in
    let* () =
      match Event.node_kind record.node, terminal with
      | Event.Tool_invocation _, Event.Succeeded when Option.is_none record.tool_input ->
        Error (Tool_input_not_materialized node_id)
      | Event.Tool_invocation _, Event.Succeeded when Option.is_none record.tool_result ->
        Error (Tool_result_not_materialized node_id)
      | Event.Output_block _, Event.Succeeded when Option.is_none record.output_snapshot
        -> Error (Output_snapshot_not_materialized node_id)
      | _ -> Ok ()
    in
    let closed_record =
      { node = record.node
      ; opened = record.opened
      ; status = Closed { event; value = terminal }
      ; last_event_id = Event.event_id event
      ; open_children = record.open_children
      ; children_rev = record.children_rev
      ; updates_rev = record.updates_rev
      ; provider_response_id = record.provider_response_id
      ; output_snapshot = record.output_snapshot
      ; tool_input = record.tool_input
      ; tool_result = record.tool_result
      }
    in
    let* nodes =
      detach_from_parent (Node_id_map.add node_id closed_record state.nodes) record.node
    in
    match Event.node_kind record.node with
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
        { view = { run_record.view with status = Finished { event; value = terminal } }
        ; open_nodes = Node_id_set.empty
        }
      in
      Ok { state with nodes; runs = Run_id_map.add run_id finished_run state.runs }
    | Event.Agent_turn _
    | Event.Provider_attempt _
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
        let* () = validate_correlation state event in
        let* () = validate_event_references state event in
        let* () = validate_event_causes state event in
        let* state = apply_payload state event in
        let correlation_id =
          match state.correlation_id with
          | Some _ as correlation_id -> correlation_id
          | None -> Some (Event.correlation_id event)
        in
        Ok
          { state with
            last_seq = actual
          ; correlation_id
          ; event_ids = Event_id_set.add event_id state.event_ids
          })
  ;;
end

type journal_state =
  { reducer : Reducer.t
  ; events_rev : Event.t list
  }

type snapshot =
  { scope_id : Store.Scope_id.t
  ; reducer : Reducer.t
  }

type t =
  { scope_id : Store.Scope_id.t
  ; correlation_id : Event.Correlation_id.t
  ; store_writer : Store.writer option
  ; writer_gate : Eio.Mutex.t
  ; mu : Eio.Mutex.t
  ; mutable state : journal_state
  }

let with_read journal f = Eio.Mutex.use_ro journal.mu (fun () -> f journal.state)
let with_state_write journal f = Eio.Mutex.use_rw ~protect:true journal.mu f
let with_writer_gate journal f = Eio.Mutex.use_rw ~protect:true journal.writer_gate f
let length journal = with_read journal (fun state -> Reducer.last_seq state.reducer)
let last_seq journal = with_read journal (fun state -> Reducer.last_seq state.reducer)

let events journal =
  let events_rev = with_read journal (fun state -> state.events_rev) in
  List.rev events_rev
;;

let make_cursor scope_id seq =
  match Store.make_cursor ~scope_id ~seq with
  | Ok cursor -> cursor
  | Error detail -> invalid_arg detail
;;

let beginning_cursor journal = make_cursor journal.scope_id 0

let current_cursor journal =
  let seq = last_seq journal in
  make_cursor journal.scope_id seq
;;

let events_after journal ~(after : cursor) =
  if not (Store.Scope_id.equal (Store.cursor_scope_id after) journal.scope_id)
  then Error Cursor_scope_mismatch
  else if Store.cursor_seq after < 0
  then Error (Invalid_argument "cursor sequence must be non-negative")
  else (
    let after_seq = Store.cursor_seq after in
    let last_seq, events_rev =
      with_read journal (fun state -> Reducer.last_seq state.reducer, state.events_rev)
    in
    if after_seq > last_seq
    then Error (Cursor_ahead { after_seq; last_seq })
    else (
      let rec collect chronological = function
        | event :: rest when Event.seq event > after_seq ->
          collect (event :: chronological) rest
        | _ -> chronological
      in
      Ok (collect [] events_rev, make_cursor journal.scope_id last_seq)))
;;

let snapshot journal =
  let reducer = with_read journal (fun state -> state.reducer) in
  { scope_id = journal.scope_id; reducer }
;;

let snapshot_cursor (snapshot : snapshot) =
  make_cursor snapshot.scope_id (Reducer.last_seq snapshot.reducer)
;;

let snapshot_find_node (snapshot : snapshot) node_id =
  Reducer.find_node snapshot.reducer node_id
;;

let snapshot_find_run (snapshot : snapshot) run_id =
  Reducer.find_run snapshot.reducer run_id
;;

let find_node journal node_id = snapshot_find_node (snapshot journal) node_id
let find_run journal run_id = snapshot_find_run (snapshot journal) run_id

let identity_result = function
  | Ok value -> Ok value
  | Error detail -> Error (Identity_failure detail)
;;

let payload_result = function
  | Ok payload -> Ok payload
  | Error detail -> Error (Invalid_event detail)
;;

let append_to_state
      (state : journal_state)
      ~event_id
      ~correlation_id
      ~run_id
      ~parent_event_id
      ?(causes = [])
      payload
  =
  let parent_event_string = Option.map Event.Event_id.to_string parent_event_id in
  let envelope =
    Event_envelope.make
      ~event_id:(Event.Event_id.to_string event_id)
      ~correlation_id:(Event.Correlation_id.to_string correlation_id)
      ~run_id:(Event.Run_id.to_string run_id)
      ~seq:(Reducer.last_seq state.reducer + 1)
      ?parent_event_id:parent_event_string
      ~source_clock:Event_envelope.Wall
      ()
  in
  let* event =
    match Event.make_validated ~causes ~envelope payload with
    | Ok event -> Ok event
    | Error detail -> Error (Invalid_event detail)
  in
  let* reducer =
    match Reducer.apply state.reducer event with
    | Ok reducer -> Ok reducer
    | Error violation -> Error (Invariant_violation violation)
  in
  Ok ({ reducer; events_rev = event :: state.events_rev }, event)
;;

type 'a mutation =
  { next_state : journal_state
  ; events : Event.t list
  ; value : 'a
  }

let append_one
      journal
      (state : journal_state)
      ~event_id
      ~run_id
      ~parent_event_id
      ?causes
      payload
  =
  let+ next_state, event =
    append_to_state
      state
      ~event_id
      ~correlation_id:journal.correlation_id
      ~run_id
      ~parent_event_id
      ?causes
      payload
  in
  { next_state; events = [ event ]; value = event }
;;

let persist_mutation journal (captured_state : journal_state) events =
  match journal.store_writer with
  | None -> Ok ()
  | Some store_writer ->
    let expected_next_seq = Reducer.last_seq captured_state.reducer + 1 in
    (match Store.append_batch store_writer ~expected_next_seq events with
     | Ok (Store.Stored | Store.Already_committed) -> Ok ()
     | Error error -> Error (Persistence_failure error))
;;

let commit_mutation journal (captured_state : journal_state) mutation =
  let* () = persist_mutation journal captured_state mutation.events in
  with_state_write journal (fun () ->
    if not (journal.state == captured_state)
    then Error (Invalid_event "execution writer gate invariant violated")
    else (
      journal.state <- mutation.next_state;
      Ok mutation.value))
;;

let with_write journal f =
  with_writer_gate journal (fun () ->
    let captured_state = with_read journal Fun.id in
    let* mutation = f captured_state in
    Eio.Cancel.protect (fun () -> commit_mutation journal captured_state mutation))
;;

let node_record_or_error (state : journal_state) node_id =
  match Reducer.find_node_record state.reducer node_id with
  | None -> Error (Invariant_violation (Unknown_node node_id))
  | Some record -> Ok record
;;

let node_last_event (state : journal_state) node_id =
  match Reducer.latest_node_event state.reducer node_id with
  | None -> Error (Invariant_violation (Unknown_node node_id))
  | Some event_id -> Ok event_id
;;

let start_run ?parent_invocation ?causes journal ~agent_name =
  let* run_id = identity_result (Event.Run_id.fresh ()) in
  let* root = identity_result (Event.Node_id.fresh ()) in
  let* event_id = identity_result (Event.Event_id.fresh ()) in
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
  let* payload = payload_result (Event.validate_payload (Event.Node_opened node)) in
  with_write journal (fun state ->
    let* parent_event_id =
      match parent_invocation with
      | None -> Ok None
      | Some node_id ->
        let+ event_id = node_last_event state node_id in
        Some event_id
    in
    let+ mutation =
      append_one journal state ~event_id ~run_id ~parent_event_id ?causes payload
    in
    { next_state = mutation.next_state
    ; events = mutation.events
    ; value = { id = run_id; root }, mutation.value
    })
;;

let validate_run_handle (state : journal_state) run =
  match Reducer.find_run state.reducer run.id with
  | None -> Error (Invariant_violation (Unknown_run run.id))
  | Some view when not (equal_run view.run run) ->
    Error (Invalid_argument "run handle does not match the journal run root")
  | Some { status = Finished _; _ } ->
    Error (Invariant_violation (Run_already_finished run.id))
  | Some { status = Running; _ } -> Ok ()
;;

let open_node ?causes journal ~run ~parent ~kind =
  match kind with
  | Event.Agent_run _ -> Error (Invariant_violation Agent_run_must_use_start_run)
  | Event.Agent_turn _
  | Event.Provider_attempt _
  | Event.Output_block _
  | Event.Tool_invocation _
  | Event.Tool_attempt ->
    let* node_id = identity_result (Event.Node_id.fresh ()) in
    let* event_id = identity_result (Event.Event_id.fresh ()) in
    let* node =
      match
        Event.make_node ~node_id ~run_id:run.id ~parent_node_id:(Some parent) ~kind
      with
      | Ok node -> Ok node
      | Error detail -> Error (Invalid_argument detail)
    in
    let* payload = payload_result (Event.validate_payload (Event.Node_opened node)) in
    with_write journal (fun state ->
      let* () = validate_run_handle state run in
      let* parent_event_id = node_last_event state parent in
      let+ mutation =
        append_one
          journal
          state
          ~event_id
          ~run_id:run.id
          ~parent_event_id:(Some parent_event_id)
          ?causes
          payload
      in
      { next_state = mutation.next_state
      ; events = mutation.events
      ; value = node_id, mutation.value
      })
;;

let update_node ?causes journal ~node update =
  let* event_id = identity_result (Event.Event_id.fresh ()) in
  let* payload =
    payload_result
      (Event.validate_payload (Event.Node_updated { node_id = node; update }))
  in
  with_write journal (fun state ->
    let* view = node_record_or_error state node in
    let* parent_event_id = node_last_event state node in
    append_one
      journal
      state
      ~event_id
      ~run_id:(Event.node_run_id view.node)
      ~parent_event_id:(Some parent_event_id)
      ?causes
      payload)
;;

let close_node ?causes journal ~node terminal =
  let* event_id = identity_result (Event.Event_id.fresh ()) in
  let* terminal = payload_result (Event.validate_terminal terminal) in
  let payload = Event.close_payload ~node_id:node terminal in
  with_write journal (fun state ->
    let* view = node_record_or_error state node in
    match Event.node_kind view.node with
    | Event.Agent_run _ -> Error (Invariant_violation (Root_must_use_finish_run node))
    | Event.Agent_turn _
    | Event.Provider_attempt _
    | Event.Output_block _
    | Event.Tool_invocation _
    | Event.Tool_attempt ->
      let* parent_event_id = node_last_event state node in
      append_one
        journal
        state
        ~event_id
        ~run_id:(Event.node_run_id view.node)
        ~parent_event_id:(Some parent_event_id)
        ?causes
        payload)
;;

let finish_run ?causes journal ~run terminal =
  let* event_id = identity_result (Event.Event_id.fresh ()) in
  let* terminal = payload_result (Event.validate_terminal terminal) in
  let payload = Event.close_payload ~node_id:run.root terminal in
  with_write journal (fun state ->
    let* () = validate_run_handle state run in
    let* parent_event_id = node_last_event state run.root in
    append_one
      journal
      state
      ~event_id
      ~run_id:run.id
      ~parent_event_id:(Some parent_event_id)
      ?causes
      payload)
;;

let abort_run ?causes journal ~run terminal =
  match terminal with
  | Event.Succeeded ->
    Error (Invalid_argument "abort_run requires a failed or cancelled terminal")
  | Event.Failed _ | Event.Cancelled _ ->
    let* terminal = payload_result (Event.validate_terminal terminal) in
    with_writer_gate journal (fun () ->
      let captured_state = with_read journal Fun.id in
      let* () = validate_run_handle captured_state run in
      let* order =
        match Reducer.open_subtree_postorder captured_state.reducer run.root with
        | Ok order -> Ok order
        | Error violation -> Error (Invariant_violation violation)
      in
      let rec apply_closes state events_rev closed_by_node = function
        | [] -> Ok (state, events_rev)
        | node_id :: nodes ->
          let* event_id = identity_result (Event.Event_id.fresh ()) in
          let* record = node_record_or_error state node_id in
          let* parent_event_id = node_last_event state node_id in
          let payload = Event.close_payload ~node_id terminal in
          let child_causes =
            record.children_rev
            |> List.rev
            |> List.filter_map (fun (child : Event.node event_record) ->
              let child_id = Event.node_id child.value in
              Option.map
                (fun event -> Event.Internal_event (Event.event_id event))
                (Node_id_map.find_opt child_id closed_by_node))
          in
          let event_causes = Option.value causes ~default:[] @ child_causes in
          let* next_state, event =
            append_to_state
              state
              ~event_id
              ~correlation_id:journal.correlation_id
              ~run_id:(Event.node_run_id record.node)
              ~parent_event_id:(Some parent_event_id)
              ~causes:event_causes
              payload
          in
          Eio.Fiber.yield ();
          apply_closes
            next_state
            (event :: events_rev)
            (Node_id_map.add node_id event closed_by_node)
            nodes
      in
      let* final_state, events_rev =
        apply_closes captured_state [] Node_id_map.empty order
      in
      let events = List.rev events_rev in
      Eio.Cancel.protect (fun () ->
        commit_mutation
          journal
          captured_state
          { next_state = final_state; events; value = events }))
;;

let replay_events events =
  let rec loop (state : journal_state) = function
    | [] -> Ok state
    | event :: rest ->
      let* reducer =
        match Reducer.apply state.reducer event with
        | Ok reducer -> Ok reducer
        | Error violation -> Error (Invariant_violation violation)
      in
      loop { reducer; events_rev = event :: state.events_rev } rest
  in
  loop ({ reducer = Reducer.empty; events_rev = [] } : journal_state) events
;;

let create ?correlation_id ?store () =
  let* scope_id, correlation_id, state, store_writer =
    match store with
    | None ->
      let* scope_id =
        match Store.Scope_id.fresh () with
        | Ok value -> Ok value
        | Error detail -> Error (Identity_failure detail)
      in
      let* correlation_id =
        match correlation_id with
        | Some correlation_id -> Ok correlation_id
        | None -> identity_result (Event.Correlation_id.fresh ())
      in
      Ok (scope_id, correlation_id, { reducer = Reducer.empty; events_rev = [] }, None)
    | Some store ->
      let store_correlation = Store.correlation_id store in
      let* () =
        match correlation_id with
        | None -> Ok ()
        | Some correlation_id
          when Event.Correlation_id.equal correlation_id store_correlation -> Ok ()
        | Some _ -> Error (Invalid_argument "store correlation identity mismatch")
      in
      let* events =
        match Store.load_all store with
        | Ok events -> Ok events
        | Error error -> Error (Persistence_failure error)
      in
      let* state = replay_events events in
      let* store_writer =
        match Store.attach store with
        | Ok writer -> Ok writer
        | Error error -> Error (Persistence_failure error)
      in
      Ok (Store.scope_id store, store_correlation, state, Some store_writer)
  in
  Ok
    { scope_id
    ; correlation_id
    ; store_writer
    ; writer_gate = Eio.Mutex.create ()
    ; mu = Eio.Mutex.create ()
    ; state
    }
;;
