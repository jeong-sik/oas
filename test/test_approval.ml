(** Tests for approval callback (human-in-the-loop) in execute_tools. *)

open Alcotest
open Agent_sdk
open Types

let descriptor_with execution_mode = { Tool.execution_mode }

let contains_substring ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop idx =
    if needle_len = 0
    then true
    else if idx + needle_len > haystack_len
    then false
    else if String.sub haystack idx needle_len = needle
    then true
    else loop (idx + 1)
  in
  loop 0
;;

(** Helper: create a simple tool that echoes its input as JSON string *)
let make_echo_tool ?descriptor name =
  Tool.create
    ?descriptor
    ~name
    ~description:"echo"
    ~parameters:
      [ { Types.name = "value"
        ; description = "Optional string value"
        ; param_type = String
        ; required = false
        }
      ; { Types.name = "x"
        ; description = "Optional integer value"
        ; param_type = Integer
        ; required = false
        }
      ]
    (fun input -> Ok { Types.content = Yojson.Safe.to_string input; _meta = None })
;;

let execute_with_tools_in_env
      env
      ~tools
      ~hooks
      ?event_bus
      ?approval
      ?on_tool_execution_started
      ?on_tool_execution_finished
      tool_uses
  =
  let net = Eio.Stdenv.net env in
  let options = { Agent.default_options with hooks; approval } in
  let agent =
    Agent.create
      ~config:(Types.default_config ~model:"test-model")
      ~net
      ~tools
      ~options
      ()
  in
  let opts = Agent.options agent in
  let event_bus =
    match event_bus with
    | Some bus -> Some bus
    | None -> opts.event_bus
  in
  Agent_tools.execute_tools
    ~context:(Agent.context agent)
    ~tools:(Tool_set.to_list (Agent.tools agent))
    ~hooks:opts.hooks
    ~event_bus
    ~tracer:opts.tracer
    ~agent_name:(Agent.state agent).config.name
    ~turn_count:(Agent.state agent).turn_count
    ~usage:(Agent.state agent).usage
    ~approval:opts.approval
    ?on_tool_execution_started
    ?on_tool_execution_finished
    tool_uses
;;

(** Helper: create a minimal agent inside Eio with given hooks and approval.
    Returns execute_tools results for the given tool_uses. *)
let run_execute_with_tools
      ~tools
      ~hooks
      ?approval
      ?on_tool_execution_started
      ?on_tool_execution_finished
      tool_uses
  =
  Eio_main.run
  @@ fun env ->
  execute_with_tools_in_env
    env
    ~tools
    ~hooks
    ?approval
    ?on_tool_execution_started
    ?on_tool_execution_finished
    tool_uses
;;

let run_execute ~hooks ?approval tool_uses =
  run_execute_with_tools
    ~tools:[ make_echo_tool "safe"; make_echo_tool "dangerous" ]
    ~hooks
    ?approval
    tool_uses
;;

(* --- Test cases --- *)

let test_approval_required_no_callback () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) }
  in
  let results =
    run_execute
      ~hooks
      [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [ "value", `String "hello" ] }
      ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check
      string
      "content"
      "Tool rejected: approval required but no approval callback is registered"
      result.content;
    check bool "is error" true (tool_result_outcome_is_error result.outcome)
  | _ -> fail "expected exactly one result"
;;

let test_tool_lifecycle_callback_exceptions_propagate () =
  let call ~on_tool_execution_started ~on_tool_execution_finished () =
    ignore
      (run_execute_with_tools
         ~tools:[ make_echo_tool "safe" ]
         ~hooks:Hooks.empty
         ~on_tool_execution_started
         ~on_tool_execution_finished
         [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [ "x", `Int 1 ] } ])
  in
  let no_started_failure ~tool_use_id:_ ~tool_name:_ ~input:_ ~schedule:_ = () in
  let no_finished_failure ~tool_use_id:_ ~tool_name:_ ~content:_ ~is_error:_ = () in
  let started_failure ~tool_use_id:_ ~tool_name:_ ~input:_ ~schedule:_ =
    failwith "started callback boom"
  in
  let finished_failure ~tool_use_id:_ ~tool_name:_ ~content:_ ~is_error:_ =
    failwith "finished callback boom"
  in
  check_raises
    "started callback exception propagates"
    (Failure "started callback boom")
    (call
       ~on_tool_execution_started:started_failure
       ~on_tool_execution_finished:no_finished_failure);
  check_raises
    "finished callback exception propagates"
    (Failure "finished callback boom")
    (call
       ~on_tool_execution_started:no_started_failure
       ~on_tool_execution_finished:finished_failure)
;;

let test_approval_approve () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) }
  in
  let approval ~tool_name:_ ~input:_ = Hooks.Approve in
  let results =
    run_execute
      ~hooks
      ~approval
      [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [ "value", `String "data" ] } ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content" {|{"value":"data"}|} result.content;
    check bool "no error" false (tool_result_outcome_is_error result.outcome)
  | _ -> fail "expected exactly one result"
;;

let test_approval_reject () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) }
  in
  let approval ~tool_name:_ ~input:_ = Hooks.Reject "too dangerous" in
  let results =
    run_execute
      ~hooks
      ~approval
      [ ToolUse
          { id = "t1"; name = "dangerous"; input = `Assoc [ "value", `String "rm -rf" ] }
      ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content" "Tool rejected: too dangerous" result.content;
    check bool "is error" true (tool_result_outcome_is_error result.outcome)
  | _ -> fail "expected exactly one result"
;;

let test_block_is_deterministic_failure () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.Block "policy denied") }
  in
  let results =
    run_execute
      ~hooks
      [ ToolUse
          { id = "t1"
          ; name = "dangerous"
          ; input = `Assoc [ "value", `String "must not run" ]
          }
      ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "reason is verbatim" "policy denied" result.content;
    check bool "is error" true (tool_result_outcome_is_error result.outcome);
    (match result.outcome with
     | Tool_failed
         { failure_kind = Agent_tools.Non_retryable_tool_error
         ; error_class = Some Types.Deterministic
         } -> ()
     | _ -> fail "expected deterministic non-retryable tool error")
  | _ -> fail "expected exactly one result"
;;

let test_approval_edit () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) }
  in
  let safe_input = `Assoc [ "value", `String "sanitized" ] in
  let approval ~tool_name:_ ~input:_ = Hooks.Edit safe_input in
  let results =
    run_execute
      ~hooks
      ~approval
      [ ToolUse
          { id = "t1"
          ; name = "dangerous"
          ; input = `Assoc [ "value", `String "original" ]
          }
      ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content uses edited input" {|{"value":"sanitized"}|} result.content;
    check bool "no error" false (tool_result_outcome_is_error result.outcome)
  | _ -> fail "expected exactly one result"
;;

let test_selective_approval () =
  (* Only "dangerous" requires approval; "safe" is auto-approved *)
  let hooks =
    { Hooks.empty with
      pre_tool_use =
        Some
          (fun event ->
            match event with
            | Hooks.PreToolUse { tool_name; _ } when tool_name = "dangerous" ->
              Hooks.ApprovalRequired
            | _ -> Hooks.Continue)
    }
  in
  let approval ~tool_name ~input:_ =
    if tool_name = "dangerous" then Hooks.Reject "blocked" else Hooks.Approve
  in
  let results =
    run_execute
      ~hooks
      ~approval
      [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [ "value", `String "ok" ] }
      ; ToolUse
          { id = "t2"; name = "dangerous"; input = `Assoc [ "value", `String "bad" ] }
      ]
  in
  (* Results may be in any order due to Eio.Fiber.List.map, so sort by id *)
  let sorted =
    List.sort (fun a b -> String.compare a.Agent_tools.tool_use_id b.tool_use_id) results
  in
  match sorted with
  | [ safe; dangerous ] ->
    check string "safe id" "t1" safe.tool_use_id;
    check string "safe executed" {|{"value":"ok"}|} safe.content;
    check bool "safe no error" false (tool_result_outcome_is_error safe.outcome);
    check string "dangerous id" "t2" dangerous.tool_use_id;
    check string "dangerous rejected" "Tool rejected: blocked" dangerous.content;
    check bool "dangerous is error" true (tool_result_outcome_is_error dangerous.outcome)
  | _ -> fail "expected exactly two results"
;;

let test_non_tool_use_blocks_filtered () =
  (* Non-ToolUse blocks (Text, Thinking) must be filtered out, not produce
     bogus ("", "", false) triples. Regression test for issue #327. *)
  let hooks = Hooks.empty in
  let results =
    run_execute
      ~hooks
      [ Text "some assistant text"
      ; ToolUse { id = "t1"; name = "safe"; input = `Assoc [ "value", `String "data" ] }
      ; Thinking { signature = None; content = "reasoning" }
      ; ToolUse { id = "t2"; name = "safe"; input = `Assoc [ "value", `String "more" ] }
      ]
  in
  (* Only the 2 ToolUse blocks should produce results *)
  check int "result count" 2 (List.length results);
  let sorted =
    List.sort (fun a b -> String.compare a.Agent_tools.tool_use_id b.tool_use_id) results
  in
  match sorted with
  | [ first; second ] ->
    check string "first id" "t1" first.tool_use_id;
    check string "second id" "t2" second.tool_use_id
  | _ -> fail "expected exactly two results"
;;

let test_only_non_tool_use_blocks () =
  (* When all blocks are non-ToolUse, result should be empty *)
  let hooks = Hooks.empty in
  let results =
    run_execute
      ~hooks
      [ Text "just text"; Thinking { signature = None; content = "thoughts" } ]
  in
  check int "empty results" 0 (List.length results)
;;

let test_concurrent_tools_share_batch () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let started_a, resolve_a = Eio.Promise.create () in
  let started_b, resolve_b = Eio.Promise.create () in
  let make_barrier_tool name resolve_self await_other =
    make_echo_tool ~descriptor:(descriptor_with Tool.Concurrent) name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun input ->
            Eio.Promise.resolve resolve_self ();
            Eio.Time.with_timeout_exn clock 0.05 (fun () -> Eio.Promise.await await_other);
            Ok { Types.content = Yojson.Safe.to_string input; _meta = None })
    }
  in
  let tools =
    [ make_barrier_tool "read_a" resolve_a started_b
    ; make_barrier_tool "read_b" resolve_b started_a
    ]
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "read_a"; input = `Assoc [ "value", `String "a" ] }
      ; ToolUse { id = "t2"; name = "read_b"; input = `Assoc [ "value", `String "b" ] }
      ]
  in
  match results with
  | [ first; second ]
    when (not (tool_result_outcome_is_error first.outcome))
         && not (tool_result_outcome_is_error second.outcome) -> ()
  | _ -> fail "concurrent batch should allow both tools to start"
;;

let test_serial_tools_run_sequentially () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let running = ref false in
  let make_guarded_tool name =
    make_echo_tool ~descriptor:(descriptor_with Tool.Serial) name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !running then failwith "serial overlap detected";
            running := true;
            Eio.Time.sleep clock 0.01;
            running := false;
            Ok { Types.content = name; _meta = None })
    }
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools:[ make_guarded_tool "write_a"; make_guarded_tool "write_b" ]
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "write_a"; input = `Assoc [] }
      ; ToolUse { id = "t2"; name = "write_b"; input = `Assoc [] }
      ]
  in
  match results with
  | [ first; second ]
    when first.content = "write_a"
         && (not (tool_result_outcome_is_error first.outcome))
         && second.content = "write_b"
         && not (tool_result_outcome_is_error second.outcome) -> ()
  | _ -> fail "serial tools should execute sequentially"
;;

let test_undeclared_tools_default_to_sequential () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let running = ref false in
  let make_guarded_tool name =
    make_echo_tool name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !running then failwith "undeclared tool overlap detected";
            running := true;
            Eio.Time.sleep clock 0.01;
            running := false;
            Ok { Types.content = name; _meta = None })
    }
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools:[ make_guarded_tool "implicit_a"; make_guarded_tool "implicit_b" ]
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "implicit_a"; input = `Assoc [] }
      ; ToolUse { id = "t2"; name = "implicit_b"; input = `Assoc [] }
      ]
  in
  match results with
  | [ first; second ]
    when first.content = "implicit_a"
         && (not (tool_result_outcome_is_error first.outcome))
         && second.content = "implicit_b"
         && not (tool_result_outcome_is_error second.outcome) -> ()
  | _ -> fail "undeclared tools should stay sequential by default"
;;

let test_serial_barrier_splits_concurrent_batches () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let concurrent_running = ref 0 in
  let serial_running = ref false in
  let make_concurrent_tool name =
    make_echo_tool ~descriptor:(descriptor_with Tool.Concurrent) name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !serial_running then failwith "concurrent call overlapped with serial call";
            incr concurrent_running;
            Eio.Time.sleep clock 0.02;
            decr concurrent_running;
            Ok { Types.content = name; _meta = None })
    }
  in
  let make_serial_tool name =
    make_echo_tool ~descriptor:(descriptor_with Tool.Serial) name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !concurrent_running > 0
            then failwith "serial call overlapped with concurrent call";
            serial_running := true;
            Eio.Time.sleep clock 0.02;
            serial_running := false;
            Ok { Types.content = name; _meta = None })
    }
  in
  let tools =
    [ make_concurrent_tool "concurrent_before"
    ; make_serial_tool "serial_mid"
    ; make_concurrent_tool "concurrent_after"
    ]
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "concurrent_before"; input = `Assoc [] }
      ; ToolUse { id = "t2"; name = "serial_mid"; input = `Assoc [] }
      ; ToolUse { id = "t3"; name = "concurrent_after"; input = `Assoc [] }
      ]
  in
  match results with
  | [ first; second; third ]
    when first.content = "concurrent_before"
         && (not (tool_result_outcome_is_error first.outcome))
         && second.content = "serial_mid"
         && (not (tool_result_outcome_is_error second.outcome))
         && third.content = "concurrent_after"
         && not (tool_result_outcome_is_error third.outcome) -> ()
  | _ -> fail "serial tool should separate concurrent batches"
;;

let test_tool_exception_still_publishes_tool_completed () =
  Eio_main.run
  @@ fun env ->
  let event_bus = Event_bus.create () in
  let subscription = Event_bus.subscribe event_bus in
  let raising_tool =
    Tool.create ~name:"boom" ~description:"raises" ~parameters:[] (fun _ ->
      failwith "kaboom")
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools:[ raising_tool ]
      ~hooks:Hooks.empty
      ~event_bus
      [ ToolUse { id = "t1"; name = "boom"; input = `Assoc [] } ]
  in
  (match results with
   | [ result ] ->
     check bool "tool result is error" true (tool_result_outcome_is_error result.outcome);
     check
       bool
       "tool result reports exception"
       true
       (contains_substring ~needle:"Tool 'boom' raised" result.content)
   | _ -> fail "expected exactly one result");
  match
    List.map
      (fun (event : Event_bus.event) -> event.payload)
      (Event_bus.drain subscription)
  with
  | [ ToolCalled { tool_name = "boom"; tool_use_id = called_id; _ }
    ; ToolCompleted
        { tool_name = "boom"
        ; tool_use_id = completed_id
        ; output = Error { message; recoverable = false; error_class = Some Unknown }
        ; _
        }
    ] ->
    check
      bool
      "completion event reports exception"
      true
      (contains_substring ~needle:"Tool 'boom' raised" message);
    (* Both events carry the provider tool_use id, so subscribers can
       join called/completed pairs (and hook-side records) on it. *)
    check string "ToolCalled carries the provider id" "t1" called_id;
    check string "ToolCompleted carries the provider id" "t1" completed_id
  | _ -> fail "expected ToolCalled followed by ToolCompleted error"
;;

let () =
  run
    "Approval"
    [ ( "approval_required"
      , [ test_case
            "no callback = explicit failure"
            `Quick
            test_approval_required_no_callback
        ; test_case "Approve = normal execution" `Quick test_approval_approve
        ; test_case "Reject with reason" `Quick test_approval_reject
        ; test_case
            "Block is a typed deterministic failure"
            `Quick
            test_block_is_deterministic_failure
        ; test_case "Edit modifies input" `Quick test_approval_edit
        ; test_case "selective by tool name" `Quick test_selective_approval
        ] )
    ; ( "non_tool_use_filtering"
      , [ test_case
            "mixed blocks filtered (#327)"
            `Quick
            test_non_tool_use_blocks_filtered
        ; test_case "only non-ToolUse = empty" `Quick test_only_non_tool_use_blocks
        ] )
    ; ( "scheduling"
      , [ test_case "concurrent batch" `Quick test_concurrent_tools_share_batch
        ; test_case
            "serial tools stay sequential"
            `Quick
            test_serial_tools_run_sequentially
        ; test_case
            "undeclared tools default serial"
            `Quick
            test_undeclared_tools_default_to_sequential
        ; test_case
            "serial barrier splits concurrent batches"
            `Quick
            test_serial_barrier_splits_concurrent_batches
        ] )
    ; ( "event_bus"
      , [ test_case
            "tool exception still publishes ToolCompleted"
            `Quick
            test_tool_exception_still_publishes_tool_completed
        ] )
    ; ( "callbacks"
      , [ test_case
            "lifecycle callback exceptions propagate"
            `Quick
            test_tool_lifecycle_callback_exceptions_propagate
        ] )
    ]
;;
