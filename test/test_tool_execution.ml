(** Tests for tool hook execution, scheduling, and lifecycle events. *)

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

let execute_result_with_tools_in_env
      env
      ~tools
      ~hooks
      ?event_bus
      ?journal
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      tool_uses
  =
  let net = Eio.Stdenv.net env in
  let options = { Agent.default_options with hooks } in
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
    ?journal
    ~tracer:opts.tracer
    ~agent_name:(Agent.state agent).config.name
    ~turn_count:(Agent.state agent).turn_count
    ~usage:(Agent.state agent).usage
    ?on_tool_execution_started
    ?on_tool_execution_finished
    ?on_hook_invoked
    tool_uses
;;

let execute_with_tools_in_env
      env
      ~tools
      ~hooks
      ?event_bus
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      tool_uses
  =
  match
    execute_result_with_tools_in_env
      env
      ~tools
      ~hooks
      ?event_bus
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      tool_uses
  with
  | Ok results -> results
  | Error
      { Agent_tools.completed_results
      ; cause = Agent_tools.Hook_failure (Agent_tools.Hook_execution_failed failure)
      } ->
    failf
      "unexpected hook failure %s at %s after %d completed result(s): %s"
      failure.hook_name
      (Hooks.hook_stage_to_string failure.stage)
      (List.length completed_results)
      failure.detail
  | Error
      { Agent_tools.cause = Agent_tools.Observer_failure { exception_; backtrace }; _ } ->
    Printexc.raise_with_backtrace exception_ backtrace
;;

(** Helper: create a minimal agent inside Eio with given hooks.
    Returns execute_tools results for the given tool_uses. *)
let run_execute_with_tools
      ~tools
      ~hooks
      ?on_tool_execution_started
      ?on_tool_execution_finished
      ?on_hook_invoked
      tool_uses
  =
  Eio_main.run
  @@ fun env ->
  execute_with_tools_in_env
    env
    ~tools
    ~hooks
    ?on_tool_execution_started
    ?on_tool_execution_finished
    ?on_hook_invoked
    tool_uses
;;

let run_execute ~hooks tool_uses =
  run_execute_with_tools
    ~tools:[ make_echo_tool "safe"; make_echo_tool "dangerous" ]
    ~hooks
    tool_uses
;;

(* --- Test cases --- *)

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

let test_pre_hook_observer_exception_propagates () =
  let observer ~hook_name:_ ~decision:_ ~detail:_ = failwith "hook observer boom" in
  check_raises
    "hook observer exception propagates"
    (Failure "hook observer boom")
    (fun () ->
       ignore
         (run_execute_with_tools
            ~tools:[ make_echo_tool "safe" ]
            ~hooks:Hooks.empty
            ~on_hook_invoked:observer
            [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [ "x", `Int 1 ] } ]))
;;

let test_reserved_callback_exception_is_not_tagged () =
  let on_tool_execution_started ~tool_use_id:_ ~tool_name:_ ~input:_ ~schedule:_ =
    raise Sys.Break
  in
  match
    run_execute_with_tools
      ~tools:[ make_echo_tool "safe" ]
      ~hooks:Hooks.empty
      ~on_tool_execution_started
      [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [] } ]
  with
  | _ -> fail "reserved callback exception must propagate"
  | exception Sys.Break -> ()
;;

let test_post_hook_observer_exception_propagates_after_completion () =
  let executed = ref 0 in
  let started = ref 0 in
  let finished = ref 0 in
  let tool =
    Tool.create ~name:"safe" ~description:"" ~parameters:[] (fun _ ->
      incr executed;
      Ok { Types.content = "done"; _meta = None })
  in
  let observer ~hook_name ~decision:_ ~detail:_ =
    if String.equal hook_name "post_tool_use" then failwith "post observer boom"
  in
  let on_tool_execution_started ~tool_use_id:_ ~tool_name:_ ~input:_ ~schedule:_ =
    incr started
  in
  let on_tool_execution_finished ~tool_use_id:_ ~tool_name:_ ~content:_ ~is_error:_ =
    incr finished
  in
  check_raises
    "post observer exception propagates"
    (Failure "post observer boom")
    (fun () ->
       ignore
         (run_execute_with_tools
            ~tools:[ tool ]
            ~hooks:Hooks.empty
            ~on_tool_execution_started
            ~on_tool_execution_finished
            ~on_hook_invoked:observer
            [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [] } ]));
  check int "tool executed exactly once" 1 !executed;
  check int "execution start observed" 1 !started;
  check int "execution completion observed before propagation" 1 !finished
;;

let test_post_hook_failure_is_typed_agent_error () =
  Eio_main.run
  @@ fun env ->
  let executed = ref 0 in
  let started = ref 0 in
  let finished = ref 0 in
  let tool =
    Tool.create ~name:"safe" ~description:"" ~parameters:[] (fun _ ->
      incr executed;
      Ok { Types.content = "done"; _meta = None })
  in
  let hooks =
    { Hooks.empty with post_tool_use = Some (fun _ -> failwith "post hook boom") }
  in
  let result =
    execute_result_with_tools_in_env
      env
      ~tools:[ tool ]
      ~hooks
      ~on_tool_execution_started:(fun ~tool_use_id:_ ~tool_name:_ ~input:_ ~schedule:_ ->
        incr started)
      ~on_tool_execution_finished:
        (fun
          ~tool_use_id:_ ~tool_name:_ ~content:_ ~is_error:_ -> incr finished)
      [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [] } ]
  in
  (match result with
   | Error
       { Agent_tools.completed_results = [ completed ]
       ; cause =
           Agent_tools.Hook_failure
             (Agent_tools.Hook_execution_failed
                { hook_name = "post_tool_use"
                ; stage = Hooks.Post_tool_use
                ; tool_name = "safe"
                ; tool_use_id = "t1"
                ; detail
                })
       } ->
     check
       bool
       "hook exception detail retained"
       true
       (contains_substring ~needle:"post hook boom" detail);
     check string "completed result retained" "done" completed.content
   | Ok _ -> fail "post hook failure was returned as successful tool results"
   | Error _ -> fail "unexpected hook execution error payload");
  check int "tool effect happened once" 1 !executed;
  check int "execution start observed" 1 !started;
  check int "execution completion observed" 1 !finished
;;

let test_concurrent_journal_failure_retains_sibling_results () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  let first_started, resolve_first = Eio.Promise.create () in
  let sibling_started, resolve_sibling = Eio.Promise.create () in
  let later_serial_runs = ref 0 in
  let finished_ids = ref [] in
  let journal =
    Durable_event.create
      ~on_append:(function
        | Durable_event.Tool_completed { tool_name = "first"; _ } ->
          failwith "journal observer boom"
        | _ -> ())
      ()
  in
  let concurrent_tool name resolve_self await_other =
    Tool.create
      ~descriptor:(descriptor_with Tool.Concurrent)
      ~name
      ~description:""
      ~parameters:[]
      (fun _ ->
         Eio.Promise.resolve resolve_self ();
         Eio.Time.with_timeout_exn clock 0.05 (fun () -> Eio.Promise.await await_other);
         Eio.Time.sleep clock 0.005;
         Ok { Types.content = name; _meta = None })
  in
  let later_serial =
    Tool.create
      ~descriptor:(descriptor_with Tool.Serial)
      ~name:"later_serial"
      ~description:""
      ~parameters:[]
      (fun _ ->
         incr later_serial_runs;
         Ok { Types.content = "must not run"; _meta = None })
  in
  let result =
    execute_result_with_tools_in_env
      env
      ~tools:
        [ concurrent_tool "first" resolve_first sibling_started
        ; concurrent_tool "sibling" resolve_sibling first_started
        ; later_serial
        ]
      ~hooks:Hooks.empty
      ~journal
      ~on_tool_execution_finished:(fun ~tool_use_id ~tool_name:_ ~content:_ ~is_error:_ ->
        finished_ids := tool_use_id :: !finished_ids)
      [ ToolUse { id = "t1"; name = "first"; input = `Assoc [] }
      ; ToolUse { id = "t2"; name = "sibling"; input = `Assoc [] }
      ; ToolUse { id = "t3"; name = "later_serial"; input = `Assoc [] }
      ]
  in
  (match result with
   | Error
       { Agent_tools.completed_results = [ first; sibling ]
       ; cause = Agent_tools.Observer_failure { exception_; _ }
       } ->
     check string "first result order" "t1" first.tool_use_id;
     check string "sibling result order" "t2" sibling.tool_use_id;
     check string "first result retained" "first" first.content;
     check string "sibling result retained" "sibling" sibling.content;
     check
       string
       "original observer exception retained"
       "Failure(\"journal observer boom\")"
       (Printexc.to_string exception_)
   | Ok _ -> fail "observer failure must remain terminal"
   | Error _ -> fail "unexpected concurrent execution failure");
  check
    (list string)
    "both concurrent completion observers ran"
    [ "t1"; "t2" ]
    (List.sort String.compare !finished_ids);
  check int "later serial batch was not started" 0 !later_serial_runs;
  check
    int
    "journal retained both Tool_completed events"
    2
    (Durable_event.events journal
     |> List.filter (function
       | Durable_event.Tool_completed _ -> true
       | _ -> false)
     |> List.length)
;;

let test_block_emits_no_execution_lifecycle () =
  let executed = ref 0 in
  let started = ref 0 in
  let finished = ref 0 in
  let tool =
    Tool.create ~name:"safe" ~description:"" ~parameters:[] (fun _ ->
      incr executed;
      Ok { Types.content = "must not run"; _meta = None })
  in
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _ -> Hooks.Block "caller denied") }
  in
  let results =
    run_execute_with_tools
      ~tools:[ tool ]
      ~hooks
      ~on_tool_execution_started:(fun ~tool_use_id:_ ~tool_name:_ ~input:_ ~schedule:_ ->
        incr started)
      ~on_tool_execution_finished:
        (fun
          ~tool_use_id:_ ~tool_name:_ ~content:_ ~is_error:_ -> incr finished)
      [ ToolUse { id = "t1"; name = "safe"; input = `Assoc [] } ]
  in
  (match results with
   | [ result ] ->
     check string "block reason" "caller denied" result.content;
     check
       bool
       "block remains model-visible error"
       true
       (tool_result_outcome_is_error result.outcome)
   | _ -> fail "expected one blocked result");
  check int "tool did not execute" 0 !executed;
  check int "execution start was not fabricated" 0 !started;
  check int "execution completion was not fabricated" 0 !finished
;;

let test_block_is_deterministic_failure () =
  let hooks =
    { Hooks.empty with pre_tool_use = Some (fun _event -> Hooks.Block "caller denied") }
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
    check string "reason is verbatim" "caller denied" result.content;
    check bool "is error" true (tool_result_outcome_is_error result.outcome);
    (match result.outcome with
     | Tool_failed
         { failure_kind = Agent_tools.Non_retryable_tool_error
         ; error_class = Some Types.Deterministic
         } -> ()
     | _ -> fail "expected deterministic non-retryable tool error")
  | _ -> fail "expected exactly one result"
;;

let test_unknown_tool_is_uniform_validation_with_full_diagnostics () =
  let names = List.init 14 (fun index -> Printf.sprintf "tool_%02d" index) in
  let run tools =
    match
      run_execute_with_tools
        ~tools
        ~hooks:Hooks.empty
        [ ToolUse { id = "missing"; name = "Missing"; input = `Assoc [] } ]
    with
    | [ result ] -> result
    | _ -> fail "expected one unknown-tool result"
  in
  let check_validation label result =
    match result.Agent_tools.outcome with
    | Tool_failed
        { failure_kind = Agent_tools.Validation_error
        ; error_class = Some Types.Deterministic
        } -> ()
    | _ -> fail (label ^ " must be a deterministic validation failure")
  in
  let without_registered_tools = run [] in
  check_validation "empty catalog" without_registered_tools;
  check
    string
    "empty catalog diagnostic"
    "Tool not found: Missing. Available tools: (none)"
    without_registered_tools.content;
  let with_registered_tools = run (List.map make_echo_tool names) in
  check_validation "populated catalog" with_registered_tools;
  check
    string
    "all available names are rendered without truncation"
    ("Tool not found: Missing. Available tools: " ^ String.concat "," names)
    with_registered_tools.content
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

let test_dispatch_passes_exact_tool_invocation () =
  let tool =
    Tool.create_with_execution_env
      ~descriptor:(descriptor_with Tool.Concurrent)
      ~name:"observe_invocation"
      ~description:"Return exact invocation identity"
      ~parameters:[]
      (fun execution_env _ ->
         match Tool.Execution_env.invocation execution_env with
         | Some invocation ->
           Eio.Fiber.yield ();
           Ok
             { Types.content =
                 Printf.sprintf
                   "%s:%d:%d"
                   (Tool.Invocation.tool_use_id invocation)
                   (Tool.Invocation.turn invocation)
                   (Tool.Invocation.planned_index invocation)
             ; _meta = None
             }
         | None ->
           Error
             { Types.message = "missing exact invocation"
             ; recoverable = false
             ; error_class = Some Types.Deterministic
             })
  in
  match
    run_execute_with_tools
      ~tools:[ tool ]
      ~hooks:Hooks.empty
      [ ToolUse
          { id = "provider-call-duplicate"
          ; name = "observe_invocation"
          ; input = `Assoc []
          }
      ; ToolUse
          { id = "provider-call-duplicate"
          ; name = "observe_invocation"
          ; input = `Assoc []
          }
      ]
  with
  | [ first; second ] ->
    check string "first exact occurrence" "provider-call-duplicate:0:0" first.content;
    check string "second exact occurrence" "provider-call-duplicate:0:1" second.content;
    check bool "first tool succeeded" false (tool_result_outcome_is_error first.outcome);
    check bool "second tool succeeded" false (tool_result_outcome_is_error second.outcome)
  | _ -> fail "expected two invocation-aware results"
;;

let test_tool_exception_still_publishes_tool_completed () =
  Eio_main.run
  @@ fun env ->
  let event_bus = Event_bus.create () in
  let config =
    Event_bus.subscription_config ~capacity:2 ~overflow:Event_bus.Drop_newest
    |> Result.get_ok
  in
  let subscription = Event_bus.subscribe ~config event_bus in
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
    "Tool_execution"
    [ ( "pre_tool_use"
      , [ test_case
            "Block is a typed deterministic failure"
            `Quick
            test_block_is_deterministic_failure
        ] )
    ; ( "routing"
      , [ test_case
            "unknown tools are uniform validation failures with full diagnostics"
            `Quick
            test_unknown_tool_is_uniform_validation_with_full_diagnostics
        ; test_case
            "dispatch passes exact tool invocation"
            `Quick
            test_dispatch_passes_exact_tool_invocation
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
        ; test_case
            "pre-hook observer exception propagates"
            `Quick
            test_pre_hook_observer_exception_propagates
        ; test_case
            "reserved callback exception is not tagged"
            `Quick
            test_reserved_callback_exception_is_not_tagged
        ; test_case
            "post-hook observer propagates after completion"
            `Quick
            test_post_hook_observer_exception_propagates_after_completion
        ; test_case
            "concurrent journal failure retains sibling results"
            `Quick
            test_concurrent_journal_failure_retains_sibling_results
        ] )
    ; ( "hook_failures"
      , [ test_case
            "post-hook failure is a typed agent error"
            `Quick
            test_post_hook_failure_is_typed_agent_error
        ] )
    ; ( "lifecycle_truth"
      , [ test_case
            "Block emits no execution lifecycle"
            `Quick
            test_block_emits_no_execution_lifecycle
        ] )
    ]
;;
