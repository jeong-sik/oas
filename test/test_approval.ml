open Base
(** Tests for approval callback (human-in-the-loop) in execute_tools. *)

open Alcotest
open Agent_sdk
open Types

let descriptor_with ?mutation_class concurrency_class =
  { Tool.kind = None
  ; mutation_class
  ; concurrency_class = Some concurrency_class
  ; permission = None
  ; shell = None
  ; notes = []
  ; examples = []
  }
;;

(** Helper: create a simple tool that echoes its input as JSON string *)
let make_echo_tool ?descriptor name =
  Tool.create ?descriptor ~name ~description:"echo" ~parameters:[] (fun input ->
    Ok { Types.content = Yojson.Safe.to_string input })
;;

let execute_with_tools_in_env env ~tools ~hooks ?approval tool_uses =
  let net = Eio.Stdenv.net env in
  let options = { Agent.default_options with hooks; approval } in
  let agent = Agent.create ~net ~tools ~options () in
  let opts = Agent.options agent in
  Agent_tools.execute_tools
    ~context:(Agent.context agent)
    ~tools:(Tool_set.to_list (Agent.tools agent))
    ~hooks:opts.hooks
    ~event_bus:opts.event_bus
    ~tracer:opts.tracer
    ~agent_name:(Agent.state agent).config.name
    ~turn_count:(Agent.state agent).turn_count
    ~usage:(Agent.state agent).usage
    ~approval:opts.approval
    tool_uses
;;

(** Helper: create a minimal agent inside Eio with given hooks and approval.
    Returns execute_tools results for the given tool_uses. *)
let run_execute_with_tools ~tools ~hooks ?approval tool_uses =
  Eio_main.run
  @@ fun env -> execute_with_tools_in_env env ~tools ~hooks ?approval tool_uses
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
  (* ApprovalRequired with no callback registered: permissive fallthrough *)
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) }
  in
  let results =
    run_execute ~hooks [ ToolUse { id = "t1"; name = "safe"; input = `String "hello" } ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content" {|"hello"|} result.content;
    check bool "no error" false result.is_error
  | _ -> fail "expected exactly one result"
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
      [ ToolUse { id = "t1"; name = "safe"; input = `String "data" } ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content" {|"data"|} result.content;
    check bool "no error" false result.is_error
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
      [ ToolUse { id = "t1"; name = "dangerous"; input = `String "rm -rf" } ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content" "Tool rejected: too dangerous" result.content;
    check bool "is error" true result.is_error
  | _ -> fail "expected exactly one result"
;;

let test_approval_edit () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.ApprovalRequired) }
  in
  let safe_input = `String "sanitized" in
  let approval ~tool_name:_ ~input:_ = Hooks.Edit safe_input in
  let results =
    run_execute
      ~hooks
      ~approval
      [ ToolUse { id = "t1"; name = "dangerous"; input = `String "original" } ]
  in
  match results with
  | [ result ] ->
    check string "id" "t1" result.tool_use_id;
    check string "content uses edited input" {|"sanitized"|} result.content;
    check bool "no error" false result.is_error
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
      [ ToolUse { id = "t1"; name = "safe"; input = `String "ok" }
      ; ToolUse { id = "t2"; name = "dangerous"; input = `String "bad" }
      ]
  in
  (* Results may be in any order due to Eio.Fiber.List.map, so sort by id *)
  let sorted =
    List.sort (fun a b -> String.compare a.Agent_tools.tool_use_id b.tool_use_id) results
  in
  match sorted with
  | [ safe; dangerous ] ->
    check string "safe id" "t1" safe.tool_use_id;
    check string "safe executed" {|"ok"|} safe.content;
    check bool "safe no error" false safe.is_error;
    check string "dangerous id" "t2" dangerous.tool_use_id;
    check string "dangerous rejected" "Tool rejected: blocked" dangerous.content;
    check bool "dangerous is error" true dangerous.is_error
  | _ -> fail "expected exactly two results"
;;

let test_skip_override_unaffected () =
  (* Skip and Override decisions still work when approval is configured *)
  let hooks =
    { Hooks.empty with
      pre_tool_use =
        Some
          (fun event ->
            match event with
            | Hooks.PreToolUse { tool_name = "safe"; _ } -> Hooks.Skip
            | Hooks.PreToolUse { tool_name = "dangerous"; _ } ->
              Hooks.Override "overridden"
            | _ -> Hooks.Continue)
    }
  in
  let approval_called = ref false in
  let approval ~tool_name:_ ~input:_ =
    approval_called := true;
    Hooks.Approve
  in
  let results =
    run_execute
      ~hooks
      ~approval
      [ ToolUse { id = "t1"; name = "safe"; input = `Null }
      ; ToolUse { id = "t2"; name = "dangerous"; input = `Null }
      ]
  in
  let sorted =
    List.sort (fun a b -> String.compare a.Agent_tools.tool_use_id b.tool_use_id) results
  in
  check bool "approval callback not called" false !approval_called;
  match sorted with
  | [ skip; override ] ->
    check string "skip id" "t1" skip.tool_use_id;
    check string "skipped" "Tool execution skipped by hook" skip.content;
    check bool "skip not error" false skip.is_error;
    check string "override id" "t2" override.tool_use_id;
    check string "overridden" "overridden" override.content;
    check bool "override not error" false override.is_error
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
      ; ToolUse { id = "t1"; name = "safe"; input = `String "data" }
      ; Thinking { thinking_type = "thinking"; content = "reasoning" }
      ; ToolUse { id = "t2"; name = "safe"; input = `String "more" }
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
      [ Text "just text"; Thinking { thinking_type = "thinking"; content = "thoughts" } ]
  in
  check int "empty results" 0 (List.length results)
;;

let test_parallel_read_tools_share_batch () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let started_a, resolve_a = Eio.Promise.create () in
  let started_b, resolve_b = Eio.Promise.create () in
  let make_barrier_tool name resolve_self await_other =
    make_echo_tool
      ~descriptor:(descriptor_with ~mutation_class:"read_only" Tool.Parallel_read)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun input ->
            Eio.Promise.resolve resolve_self ();
            Eio.Time.with_timeout_exn clock 0.05 (fun () -> Eio.Promise.await await_other);
            Ok { Types.content = Yojson.Safe.to_string input })
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
      [ ToolUse { id = "t1"; name = "read_a"; input = `String "a" }
      ; ToolUse { id = "t2"; name = "read_b"; input = `String "b" }
      ]
  in
  match results with
  | [ first; second ] when (not first.is_error) && not second.is_error -> ()
  | _ -> fail "parallel read batch should allow both tools to start"
;;

let test_workspace_tools_run_sequentially () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let running = ref false in
  let make_guarded_tool name =
    make_echo_tool
      ~descriptor:
        (descriptor_with ~mutation_class:"workspace_mutating" Tool.Sequential_workspace)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !running then failwith "workspace overlap detected";
            running := true;
            Eio.Time.sleep clock 0.01;
            running := false;
            Ok { Types.content = name })
    }
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools:[ make_guarded_tool "write_a"; make_guarded_tool "write_b" ]
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "write_a"; input = `Null }
      ; ToolUse { id = "t2"; name = "write_b"; input = `Null }
      ]
  in
  match results with
  | [ first; second ]
    when first.content = "write_a"
         && (not first.is_error)
         && second.content = "write_b"
         && not second.is_error -> ()
  | _ -> fail "workspace tools should execute sequentially"
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
            Ok { Types.content = name })
    }
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools:[ make_guarded_tool "implicit_a"; make_guarded_tool "implicit_b" ]
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "implicit_a"; input = `Null }
      ; ToolUse { id = "t2"; name = "implicit_b"; input = `Null }
      ]
  in
  match results with
  | [ first; second ]
    when first.content = "implicit_a"
         && (not first.is_error)
         && second.content = "implicit_b"
         && not second.is_error -> ()
  | _ -> fail "undeclared tools should stay sequential by default"
;;

let test_workspace_barrier_splits_parallel_read_batches () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let read_running = ref 0 in
  let workspace_running = ref false in
  let make_read_tool name =
    make_echo_tool
      ~descriptor:(descriptor_with ~mutation_class:"read_only" Tool.Parallel_read)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !workspace_running then failwith "read overlapped with workspace";
            incr read_running;
            Eio.Time.sleep clock 0.02;
            decr read_running;
            Ok { Types.content = name })
    }
  in
  let make_workspace_tool name =
    make_echo_tool
      ~descriptor:
        (descriptor_with ~mutation_class:"workspace_mutating" Tool.Sequential_workspace)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !read_running > 0 then failwith "workspace overlapped with read";
            workspace_running := true;
            Eio.Time.sleep clock 0.02;
            workspace_running := false;
            Ok { Types.content = name })
    }
  in
  let tools =
    [ make_read_tool "read_before"
    ; make_workspace_tool "write_mid"
    ; make_read_tool "read_after"
    ]
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "read_before"; input = `Null }
      ; ToolUse { id = "t2"; name = "write_mid"; input = `Null }
      ; ToolUse { id = "t3"; name = "read_after"; input = `Null }
      ]
  in
  match results with
  | [ first; second; third ]
    when first.content = "read_before"
         && (not first.is_error)
         && second.content = "write_mid"
         && (not second.is_error)
         && third.content = "read_after"
         && not third.is_error -> ()
  | _ -> fail "workspace tool should form a sequential barrier between read batches"
;;

let test_exclusive_external_barrier_isolation () =
  (* Exclusive_external tools must not overlap with any other tool.
     Schedule: [read, workspace, exclusive, read_after]
     Expected batches:
       Parallel_batch [read]
       Sequential_batch workspace
       Exclusive_batch exclusive   (barrier: no overlap before or after)
       Parallel_batch [read_after]
     Overlap detection uses mutable flags checked at runtime. *)
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let any_running = ref false in
  let exclusive_running = ref false in
  let make_read_tool name =
    make_echo_tool
      ~descriptor:(descriptor_with ~mutation_class:"read_only" Tool.Parallel_read)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !exclusive_running then failwith "read overlapped with exclusive";
            any_running := true;
            Eio.Time.sleep clock 0.02;
            any_running := false;
            Ok { Types.content = name })
    }
  in
  let make_workspace_tool name =
    make_echo_tool
      ~descriptor:
        (descriptor_with ~mutation_class:"workspace_mutating" Tool.Sequential_workspace)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !exclusive_running then failwith "workspace overlapped with exclusive";
            any_running := true;
            Eio.Time.sleep clock 0.02;
            any_running := false;
            Ok { Types.content = name })
    }
  in
  let make_exclusive_tool name =
    make_echo_tool
      ~descriptor:
        (descriptor_with ~mutation_class:"external_effect" Tool.Exclusive_external)
      name
    |> fun tool ->
    { tool with
      Tool.handler =
        Tool.Simple
          (fun _ ->
            if !any_running then failwith "exclusive overlapped with another tool";
            exclusive_running := true;
            Eio.Time.sleep clock 0.02;
            exclusive_running := false;
            Ok { Types.content = name })
    }
  in
  let tools =
    [ make_read_tool "read_first"
    ; make_workspace_tool "write_mid"
    ; make_exclusive_tool "ext_call"
    ; make_read_tool "read_last"
    ]
  in
  let results =
    execute_with_tools_in_env
      env
      ~tools
      ~hooks:Hooks.empty
      [ ToolUse { id = "t1"; name = "read_first"; input = `Null }
      ; ToolUse { id = "t2"; name = "write_mid"; input = `Null }
      ; ToolUse { id = "t3"; name = "ext_call"; input = `Null }
      ; ToolUse { id = "t4"; name = "read_last"; input = `Null }
      ]
  in
  match results with
  | [ { Agent_tools.tool_name = "read_first"; is_error = false; _ }
    ; { tool_name = "write_mid"; is_error = false; _ }
    ; { tool_name = "ext_call"; is_error = false; _ }
    ; { tool_name = "read_last"; is_error = false; _ }
    ] -> ()
  | _ -> fail "exclusive external tool must run in complete isolation"
;;

let test_exclusive_batch_kind_metadata () =
  (* Verify the schedule.batch_kind field is "exclusive" for Exclusive_external
     tools. Capture it via the PreToolUse hook. *)
  Eio_main.run
  @@ fun env ->
  let captured_kinds = ref [] in
  let hooks =
    { Hooks.empty with
      pre_tool_use =
        Some
          (fun event ->
            (match event with
             | Hooks.PreToolUse { tool_name; schedule; _ } ->
               captured_kinds := (tool_name, schedule.batch_kind) :: !captured_kinds
             | _ -> ());
            Hooks.Continue)
    }
  in
  let read_tool =
    make_echo_tool
      ~descriptor:(descriptor_with ~mutation_class:"read_only" Tool.Parallel_read)
      "reader"
  in
  let seq_tool =
    make_echo_tool
      ~descriptor:
        (descriptor_with ~mutation_class:"workspace_mutating" Tool.Sequential_workspace)
      "writer"
  in
  let excl_tool =
    make_echo_tool
      ~descriptor:
        (descriptor_with ~mutation_class:"external_effect" Tool.Exclusive_external)
      "ext"
  in
  let _results =
    execute_with_tools_in_env
      env
      ~tools:[ read_tool; seq_tool; excl_tool ]
      ~hooks
      [ ToolUse { id = "t1"; name = "reader"; input = `Null }
      ; ToolUse { id = "t2"; name = "writer"; input = `Null }
      ; ToolUse { id = "t3"; name = "ext"; input = `Null }
      ]
  in
  let kinds = List.rev !captured_kinds in
  check
    (list (pair string string))
    "batch_kind metadata"
    [ "reader", "parallel"; "writer", "sequential"; "ext", "exclusive" ]
    kinds
;;

let () =
  run
    "Approval"
    [ ( "approval_required"
      , [ test_case "no callback = fallthrough" `Quick test_approval_required_no_callback
        ; test_case "Approve = normal execution" `Quick test_approval_approve
        ; test_case "Reject with reason" `Quick test_approval_reject
        ; test_case "Edit modifies input" `Quick test_approval_edit
        ; test_case "selective by tool name" `Quick test_selective_approval
        ; test_case "Skip/Override unaffected" `Quick test_skip_override_unaffected
        ] )
    ; ( "non_tool_use_filtering"
      , [ test_case
            "mixed blocks filtered (#327)"
            `Quick
            test_non_tool_use_blocks_filtered
        ; test_case "only non-ToolUse = empty" `Quick test_only_non_tool_use_blocks
        ] )
    ; ( "scheduling"
      , [ test_case "parallel read batch" `Quick test_parallel_read_tools_share_batch
        ; test_case
            "workspace tools stay sequential"
            `Quick
            test_workspace_tools_run_sequentially
        ; test_case
            "undeclared tools default sequential"
            `Quick
            test_undeclared_tools_default_to_sequential
        ; test_case
            "workspace barrier splits read batches"
            `Quick
            test_workspace_barrier_splits_parallel_read_batches
        ; test_case
            "exclusive external barrier isolation (#589)"
            `Quick
            test_exclusive_external_barrier_isolation
        ; test_case
            "exclusive batch_kind metadata (#589)"
            `Quick
            test_exclusive_batch_kind_metadata
        ] )
    ]
;;
