open Alcotest
open Agent_sdk
module Event = Execution_event
module Journal = Execution_journal

let require_ok = function
  | Ok value -> value
  | Error error -> fail (Journal.error_to_string error)
;;

let require_codec_ok = function
  | Ok value -> value
  | Error detail -> fail detail
;;

let serial_schedule : Hooks.tool_schedule =
  { planned_index = 0; batch_index = 0; batch_size = 1; execution_mode = Tool.Serial }
;;

let provider_turn turn =
  Event.Provider_turn
    { turn
    ; model = "test-model"
    ; provider_response_id = Some ("response-" ^ string_of_int turn)
    }
;;

let tool_invocation name =
  Event.Tool_invocation
    { provider_tool_use_id = Some ("provider-" ^ name)
    ; tool_name = name
    ; input = None
    ; schedule = serial_schedule
    }
;;

let check_contiguous events =
  List.iteri
    (fun index event -> check int "global sequence" (index + 1) (Event.seq event))
    events
;;

let build_recursive_run journal =
  let parent_run = require_ok (Journal.start_run journal ~agent_name:"keeper") in
  let parent_turn =
    require_ok
      (Journal.open_node
         journal
         ~run:parent_run
         ~parent:(Journal.run_root parent_run)
         ~kind:(provider_turn 0))
  in
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:parent_turn
          (Event.Provider_event (`Assoc [ "phase", `String "started" ]))));
  let thinking =
    require_ok
      (Journal.open_node
         journal
         ~run:parent_run
         ~parent:parent_turn
         ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Thinking_block }))
  in
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:thinking
          (Event.Output_delta (`Assoc [ "text", `String "inspect" ]))));
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:thinking
          (Event.Output_snapshot (`Assoc [ "text", `String "inspect source" ]))));
  ignore (require_ok (Journal.close_node journal ~node:thinking Event.Succeeded));
  let invocation =
    require_ok
      (Journal.open_node
         journal
         ~run:parent_run
         ~parent:parent_turn
         ~kind:(tool_invocation "read_source"))
  in
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:invocation
          (Event.Tool_input_delta (`Assoc [ "path", `String "lib/agent.ml" ]))));
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:invocation
          (Event.Tool_input_snapshot (`Assoc [ "path", `String "lib/agent.ml" ]))));
  let attempt =
    require_ok
      (Journal.open_node
         journal
         ~run:parent_run
         ~parent:invocation
         ~kind:Event.Tool_attempt)
  in
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:attempt
          (Event.Tool_progress (`Assoc [ "bytes", `Int 512 ]))));
  ignore (require_ok (Journal.close_node journal ~node:attempt Event.Succeeded));
  let child_run =
    require_ok
      (Journal.start_run ~parent_invocation:invocation journal ~agent_name:"reviewer")
  in
  let child_turn =
    require_ok
      (Journal.open_node
         journal
         ~run:child_run
         ~parent:(Journal.run_root child_run)
         ~kind:(provider_turn 0))
  in
  ignore (require_ok (Journal.close_node journal ~node:child_turn Event.Succeeded));
  ignore (require_ok (Journal.finish_run journal ~run:child_run Event.Succeeded));
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:invocation
          (Event.Tool_result (`Assoc [ "review", `String "accepted" ]))));
  ignore (require_ok (Journal.close_node journal ~node:invocation Event.Succeeded));
  ignore (require_ok (Journal.close_node journal ~node:parent_turn Event.Succeeded));
  ignore (require_ok (Journal.finish_run journal ~run:parent_run Event.Succeeded));
  parent_run, child_run
;;

let test_recursive_tree_and_global_sequence () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let parent_run, child_run = build_recursive_run journal in
  let events = Journal.events journal in
  check int "all events retained" 21 (List.length events);
  check_contiguous events;
  let tail = require_ok (Journal.events_after journal ~after_seq:15) in
  check int "exclusive cursor" 6 (List.length tail);
  check bool "cursor order" true (List.for_all (fun event -> Event.seq event > 15) tail);
  (match Journal.find_run journal (Journal.run_id parent_run) with
   | Some { status = Journal.Finished Event.Succeeded; parent_invocation = None; _ } -> ()
   | _ -> fail "parent run was not finished as a root run");
  match Journal.find_run journal (Journal.run_id child_run) with
  | Some { status = Journal.Finished Event.Succeeded; parent_invocation = Some _; _ } ->
    ()
  | _ -> fail "child run did not retain its invocation parent"
;;

let test_codec_roundtrip_and_exactness () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let run = require_ok (Journal.start_run journal ~agent_name:"codec") in
  let event = List.hd (Journal.events journal) in
  let decoded = require_codec_ok (Event.of_json_string (Event.to_json_string event)) in
  check bool "event round-trip" true (Event.equal event decoded);
  (match
     Event.node_update_of_yojson (`Assoc [ "type", `String "unknown"; "value", `Null ])
   with
   | Error _ -> ()
   | Ok _ -> fail "unknown update type was accepted");
  let json = Event.to_yojson event in
  let with_unknown_envelope_field =
    match json with
    | `Assoc fields ->
      let envelope = List.assoc "envelope" fields in
      let envelope =
        match envelope with
        | `Assoc envelope_fields -> `Assoc (("legacy", `Bool true) :: envelope_fields)
        | _ -> fail "event envelope was not an object"
      in
      `Assoc (("envelope", envelope) :: List.remove_assoc "envelope" fields)
    | _ -> fail "execution event was not an object"
  in
  (match Event.of_yojson with_unknown_envelope_field with
   | Error _ -> ()
   | Ok _ -> fail "unknown envelope field was accepted");
  let pending_input =
    require_codec_ok
      (Event.node_kind_of_yojson (Event.node_kind_to_yojson (tool_invocation "pending")))
  in
  (match pending_input with
   | Event.Tool_invocation { input = None; _ } -> ()
   | _ -> fail "absent tool input did not round-trip as None");
  let null_input =
    Event.Tool_invocation
      { provider_tool_use_id = None
      ; tool_name = "null_input"
      ; input = Some `Null
      ; schedule = serial_schedule
      }
  in
  (match
     require_codec_ok (Event.node_kind_of_yojson (Event.node_kind_to_yojson null_input))
   with
   | Event.Tool_invocation { input = Some `Null; _ } -> ()
   | _ -> fail "canonical JSON null was confused with absent input");
  ignore (require_ok (Journal.finish_run journal ~run Event.Succeeded))
;;

let id module_id value =
  match module_id value with
  | Ok value -> value
  | Error detail -> fail detail
;;

let make_manual_event ~event_id ~run_id ~seq ?parent_event_id payload =
  let envelope =
    Event_envelope.make
      ~event_id:(Event.Event_id.to_string event_id)
      ~correlation_id:(Event.Run_id.to_string run_id)
      ~run_id:(Event.Run_id.to_string run_id)
      ~seq
      ?parent_event_id
      ()
  in
  require_codec_ok (Event.make ~envelope ~payload)
;;

let test_reducer_rejects_sequence_and_unknown_parent_event () =
  let run_id = id Event.Run_id.of_string "execution-run-manual" in
  let root_id = id Event.Node_id.of_string "execution-node-manual" in
  let node =
    require_codec_ok
      (Event.make_node
         ~node_id:root_id
         ~run_id
         ~parent_node_id:None
         ~kind:(Event.Agent_run { agent_name = "manual" }))
  in
  let event_id = id Event.Event_id.of_string "execution-event-manual" in
  let seq_two = make_manual_event ~event_id ~run_id ~seq:2 (Event.Node_opened node) in
  (match Journal.Reducer.apply Journal.Reducer.empty seq_two with
   | Error (Journal.Sequence_mismatch { expected = 1; actual = 2 }) -> ()
   | _ -> fail "sequence gap was not rejected");
  let unknown_parent = "execution-event-does-not-exist" in
  let seq_one =
    make_manual_event
      ~event_id
      ~run_id
      ~seq:1
      ~parent_event_id:unknown_parent
      (Event.Node_opened node)
  in
  match Journal.Reducer.apply Journal.Reducer.empty seq_one with
  | Error (Journal.Unknown_parent_event _) -> ()
  | _ -> fail "unknown event reference was not rejected"
;;

let test_hierarchy_and_lifecycle_rejections () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let run = require_ok (Journal.start_run journal ~agent_name:"invalid") in
  let root = Journal.run_root run in
  (match
     Journal.open_node
       journal
       ~run
       ~parent:root
       ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Text_block })
   with
   | Error (Journal.Invariant_violation (Journal.Invalid_parent_kind _)) -> ()
   | result ->
     failf
       "invalid root child result: %s"
       (match result with
        | Ok _ -> "accepted"
        | Error e -> Journal.error_to_string e));
  check int "rejected open is not committed" 1 (Journal.length journal);
  let turn =
    require_ok (Journal.open_node journal ~run ~parent:root ~kind:(provider_turn 0))
  in
  (match
     Journal.update_node journal ~node:turn (Event.Output_delta (`String "wrong"))
   with
   | Error (Journal.Invariant_violation (Journal.Invalid_update_for_node _)) -> ()
   | _ -> fail "invalid update was not rejected");
  let output =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:turn
         ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Text_block }))
  in
  (match Journal.close_node journal ~node:turn Event.Succeeded with
   | Error (Journal.Invariant_violation (Journal.Node_has_open_children _)) -> ()
   | _ -> fail "node with an open child was closed");
  (match Journal.start_run ~parent_invocation:turn journal ~agent_name:"bad-child" with
   | Error (Journal.Invariant_violation (Journal.Invalid_child_run_parent _)) -> ()
   | _ -> fail "child run under a provider turn was accepted");
  (match Journal.close_node journal ~node:root Event.Succeeded with
   | Error (Journal.Invariant_violation (Journal.Root_must_use_finish_run _)) -> ()
   | _ -> fail "root was closed without finish_run");
  ignore (require_ok (Journal.close_node journal ~node:output Event.Succeeded));
  let invocation =
    require_ok
      (Journal.open_node journal ~run ~parent:turn ~kind:(tool_invocation "streamed"))
  in
  (match
     Journal.update_node journal ~node:invocation (Event.Tool_result (`String "early"))
   with
   | Error (Journal.Invariant_violation (Journal.Tool_input_not_materialized _)) -> ()
   | _ -> fail "tool result was accepted before canonical input");
  (match Journal.open_node journal ~run ~parent:invocation ~kind:Event.Tool_attempt with
   | Error (Journal.Invariant_violation (Journal.Tool_input_not_materialized _)) -> ()
   | _ -> fail "tool attempt was opened before canonical input");
  (match Journal.close_node journal ~node:invocation Event.Succeeded with
   | Error (Journal.Invariant_violation (Journal.Tool_input_not_materialized _)) -> ()
   | _ -> fail "tool invocation succeeded without canonical input");
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:invocation
          (Event.Tool_input_snapshot (`Assoc [ "value", `Int 1 ]))));
  (match
     Journal.update_node
       journal
       ~node:invocation
       (Event.Tool_input_snapshot (`Assoc [ "value", `Int 2 ]))
   with
   | Error (Journal.Invariant_violation (Journal.Tool_input_already_materialized _)) -> ()
   | _ -> fail "a second canonical tool input snapshot was accepted");
  (match
     Journal.update_node
       journal
       ~node:invocation
       (Event.Tool_input_delta (`String "late"))
   with
   | Error (Journal.Invariant_violation (Journal.Tool_input_delta_after_snapshot _)) -> ()
   | _ -> fail "tool input delta was accepted after its snapshot");
  (match Journal.close_node journal ~node:invocation Event.Succeeded with
   | Error (Journal.Invariant_violation (Journal.Tool_result_not_materialized _)) -> ()
   | _ -> fail "tool invocation succeeded without a canonical result");
  let attempt =
    require_ok
      (Journal.open_node journal ~run ~parent:invocation ~kind:Event.Tool_attempt)
  in
  (match
     Journal.update_node
       journal
       ~node:invocation
       (Event.Tool_result (`Assoc [ "value", `Int 1 ]))
   with
   | Error (Journal.Invariant_violation (Journal.Tool_result_while_children_open _)) -> ()
   | _ -> fail "tool result was accepted while an attempt was open");
  ignore (require_ok (Journal.close_node journal ~node:attempt Event.Succeeded));
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:invocation
          (Event.Tool_result (`Assoc [ "value", `Int 1 ]))));
  (match
     Journal.update_node
       journal
       ~node:invocation
       (Event.Tool_result (`Assoc [ "value", `Int 2 ]))
   with
   | Error (Journal.Invariant_violation (Journal.Tool_result_already_materialized _)) ->
     ()
   | _ -> fail "a second canonical tool result was accepted");
  (match Journal.open_node journal ~run ~parent:invocation ~kind:Event.Tool_attempt with
   | Error (Journal.Invariant_violation (Journal.Child_after_tool_result _)) -> ()
   | _ -> fail "a tool attempt was opened after the canonical result");
  ignore (require_ok (Journal.close_node journal ~node:invocation Event.Succeeded));
  let failed_invocation =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:turn
         ~kind:(tool_invocation "parse_failure"))
  in
  ignore
    (require_ok
       (Journal.close_node
          journal
          ~node:failed_invocation
          (Event.Failed
             { kind = Event.Protocol_failure
             ; detail = "tool input was not valid JSON"
             ; data = None
             })));
  ignore (require_ok (Journal.close_node journal ~node:turn Event.Succeeded));
  ignore (require_ok (Journal.finish_run journal ~run Event.Succeeded))
;;

let test_output_snapshot_is_terminal_for_output_updates () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let run = require_ok (Journal.start_run journal ~agent_name:"output-snapshot") in
  let turn =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:(Journal.run_root run)
         ~kind:(provider_turn 0))
  in
  let output =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:turn
         ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Text_block }))
  in
  ignore
    (require_ok
       (Journal.update_node journal ~node:output (Event.Output_delta (`String "a"))));
  ignore
    (require_ok
       (Journal.update_node
          journal
          ~node:output
          (Event.Output_snapshot (`String "answer"))));
  (match
     Journal.update_node
       journal
       ~node:output
       (Event.Output_snapshot (`String "replacement"))
   with
   | Error (Journal.Invariant_violation (Journal.Output_snapshot_already_materialized _))
     -> ()
   | _ -> fail "a second output snapshot was accepted");
  (match
     Journal.update_node journal ~node:output (Event.Output_delta (`String "late"))
   with
   | Error (Journal.Invariant_violation (Journal.Output_delta_after_snapshot _)) -> ()
   | _ -> fail "an output delta was accepted after its snapshot");
  ignore (require_ok (Journal.close_node journal ~node:output Event.Succeeded));
  ignore (require_ok (Journal.close_node journal ~node:turn Event.Succeeded));
  ignore (require_ok (Journal.finish_run journal ~run Event.Succeeded))
;;

let test_json_terminal_and_id_boundaries () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let run = require_ok (Journal.start_run journal ~agent_name:"validation") in
  let turn =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:(Journal.run_root run)
         ~kind:(provider_turn 0))
  in
  let before_invalid_updates = Journal.length journal in
  let invalid_json_values = [ `Float nan; `Intlit "not-a-json-integer" ] in
  List.iter
    (fun value ->
       match Journal.update_node journal ~node:turn (Event.Provider_event value) with
       | Error (Journal.Invalid_event _) -> ()
       | _ -> fail "non-serializable provider JSON entered the in-memory journal")
    invalid_json_values;
  check
    int
    "invalid JSON did not advance in-memory journal"
    before_invalid_updates
    (Journal.length journal);
  let invalid_payload = `Assoc [ "nested", `List [ `Float infinity ] ] in
  let invalid_updates =
    [ Event.Provider_event invalid_payload
    ; Event.Output_delta invalid_payload
    ; Event.Output_snapshot invalid_payload
    ; Event.Tool_input_delta invalid_payload
    ; Event.Tool_input_snapshot invalid_payload
    ; Event.Tool_progress invalid_payload
    ; Event.Tool_result invalid_payload
    ]
  in
  List.iter
    (fun update ->
       let envelope =
         Event_envelope.make
           ~event_id:(Event.Event_id.to_string (Event.Event_id.fresh ()))
           ~correlation_id:(Event.Run_id.to_string (Journal.run_id run))
           ~run_id:(Event.Run_id.to_string (Journal.run_id run))
           ~seq:1
           ()
       in
       match
         Event.make ~envelope ~payload:(Event.Node_updated { node_id = turn; update })
       with
       | Error _ -> ()
       | Ok _ -> fail "an opaque non-finite JSON update passed Event.make")
    invalid_updates;
  (match
     Event.make_node
       ~node_id:(Event.Node_id.fresh ())
       ~run_id:(Journal.run_id run)
       ~parent_node_id:(Some turn)
       ~kind:
         (Event.Tool_invocation
            { provider_tool_use_id = None
            ; tool_name = "invalid_input"
            ; input = Some invalid_payload
            ; schedule = serial_schedule
            })
   with
   | Error _ -> ()
   | Ok _ -> fail "non-finite canonical tool input passed make_node");
  (match
     Event.node_update_of_yojson
       (`Assoc [ "type", `String "provider_event"; "value", `Float nan ])
   with
   | Error _ -> ()
   | Ok _ -> fail "non-finite JSON passed the node update decoder");
  (match
     Event.node_kind_of_yojson
       (Event.node_kind_to_yojson
          (Event.Tool_invocation
             { provider_tool_use_id = None
             ; tool_name = "invalid_codec_input"
             ; input = Some invalid_payload
             ; schedule = serial_schedule
             }))
   with
   | Error _ -> ()
   | Ok _ -> fail "non-finite JSON passed the node kind decoder");
  let output =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:turn
         ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Text_block }))
  in
  let expect_invalid_terminal terminal =
    match Journal.close_node journal ~node:output terminal with
    | Error (Journal.Invalid_event _) -> ()
    | _ -> fail "invalid terminal payload was committed"
  in
  expect_invalid_terminal
    (Event.Failed { kind = Event.Protocol_failure; detail = " \t\n"; data = None });
  expect_invalid_terminal
    (Event.Failed
       { kind = Event.Protocol_failure
       ; detail = "provider returned invalid data"
       ; data = Some invalid_payload
       });
  expect_invalid_terminal (Event.Cancelled { reason = None; data = None });
  expect_invalid_terminal (Event.Cancelled { reason = Some "  "; data = None });
  expect_invalid_terminal
    (Event.Cancelled { reason = Some "provider cancelled"; data = Some (`Float nan) });
  (match
     Event.terminal_of_yojson
       (`Assoc
           [ "type", `String "failed"
           ; ( "failure"
             , `Assoc
                 [ "kind", `String "protocol"; "detail", `String "  "; "data", `Null ] )
           ])
   with
   | Error _ -> ()
   | Ok _ -> fail "whitespace-only failure passed the terminal decoder");
  ignore
    (require_ok
       (Journal.close_node
          journal
          ~node:output
          (Event.Cancelled { reason = Some "operator requested stop"; data = None })));
  ignore (require_ok (Journal.close_node journal ~node:turn Event.Succeeded));
  ignore (require_ok (Journal.finish_run journal ~run Event.Succeeded));
  let expect_prefix_only_rejected result =
    match result with
    | Error _ -> ()
    | Ok _ -> fail "prefix-only execution identifier was accepted"
  in
  expect_prefix_only_rejected (Event.Event_id.of_string "execution-event-");
  expect_prefix_only_rejected (Event.Run_id.of_string "execution-run-");
  expect_prefix_only_rejected (Event.Node_id.of_string "execution-node-")
;;

let test_one_top_level_run_per_journal () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let run = require_ok (Journal.start_run journal ~agent_name:"root") in
  let expect_existing_root = function
    | Error (Journal.Invariant_violation Journal.Top_level_run_already_exists) -> ()
    | Error error -> fail (Journal.error_to_string error)
    | Ok _ -> fail "a second top-level run was accepted"
  in
  expect_existing_root (Journal.start_run journal ~agent_name:"second-root");
  check int "rejected root is not committed" 1 (Journal.length journal);
  ignore (require_ok (Journal.finish_run journal ~run Event.Succeeded));
  expect_existing_root (Journal.start_run journal ~agent_name:"root-after-finish");
  check int "finished scope is not reused" 2 (Journal.length journal)
;;

let test_concurrent_updates_keep_one_sequence () =
  Eio_main.run
  @@ fun _env ->
  let journal = Journal.create () in
  let run = require_ok (Journal.start_run journal ~agent_name:"concurrent") in
  let turn =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:(Journal.run_root run)
         ~kind:(provider_turn 0))
  in
  let output =
    require_ok
      (Journal.open_node
         journal
         ~run
         ~parent:turn
         ~kind:(Event.Output_block { ordinal = 0; block_kind = Event.Text_block }))
  in
  Eio.Switch.run (fun sw ->
    List.init 32 Fun.id
    |> List.iter (fun index ->
      Eio.Fiber.fork ~sw (fun () ->
        ignore
          (require_ok
             (Journal.update_node
                journal
                ~node:output
                (Event.Output_delta (`Assoc [ "index", `Int index ])))))));
  let events = Journal.events journal in
  check int "every update retained" 35 (List.length events);
  check_contiguous events
;;

let () =
  run
    "Execution journal"
    [ ( "topology"
      , [ test_case
            "recursive runs retain hierarchy and order"
            `Quick
            test_recursive_tree_and_global_sequence
        ; test_case
            "hierarchy and lifecycle violations"
            `Quick
            test_hierarchy_and_lifecycle_rejections
        ; test_case
            "concurrent updates share one sequence"
            `Quick
            test_concurrent_updates_keep_one_sequence
        ; test_case
            "output snapshot terminates output updates"
            `Quick
            test_output_snapshot_is_terminal_for_output_updates
        ; test_case
            "one top-level run defines the execution scope"
            `Quick
            test_one_top_level_run_per_journal
        ] )
    ; ( "codec"
      , [ test_case
            "round-trip and exact schema"
            `Quick
            test_codec_roundtrip_and_exactness
        ; test_case
            "reducer rejects gaps and dangling causality"
            `Quick
            test_reducer_rejects_sequence_and_unknown_parent_event
        ; test_case
            "JSON terminal and identifier boundaries"
            `Quick
            test_json_terminal_and_id_boundaries
        ] )
    ]
;;
