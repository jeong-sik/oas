open Agent_sdk

let provider =
  { Provider.provider = Provider.Local { base_url = "http://mock.local" }
  ; model_id = "mock-model"
  ; api_key_env = ""
  }
;;

let tool_response id input : Types.api_response =
  { id = "response-" ^ id
  ; model = "mock-model"
  ; stop_reason = StopToolUse
  ; content = [ ToolUse { id; name = "Execute"; input } ]
  ; usage = None
  ; telemetry = None
  }
;;

let final_response : Types.api_response =
  { id = "response-final"
  ; model = "mock-model"
  ; stop_reason = EndTurn
  ; content = [ Text "finished" ]
  ; usage = None
  ; telemetry = None
  }
;;

let append events event = events := !events @ [ event ]

let failing_tool events =
  let parameters : Types.tool_param list =
    [ { name = "cmd"; description = "command"; param_type = String; required = true }
    ; { name = "cwd"
      ; description = "working directory"
      ; param_type = String
      ; required = false
      }
    ]
  in
  Tool.create ~name:"Execute" ~description:"execute a command" ~parameters (fun _ ->
    append events "tool";
    Error
      { Types.message = "working directory is required"
      ; recoverable = true
      ; error_class = Some Deterministic
      })
;;

type scenario =
  { result : (Types.api_response, Error.sdk_error) result
  ; events : string list
  ; requests : Llm_provider.Llm_transport.completion_request list
  ; agent : Agent.t
  ; decision_checkpoint : Checkpoint.t option
  }

let run_scenario env ~judge_json ?(fail_recovery_checkpoint = false) () =
  Eio.Switch.run
  @@ fun sw ->
  let events = ref [] in
  let requests = ref [] in
  let decision_checkpoint = ref None in
  let responses =
    ref
      [ tool_response "p1" (`Assoc [ "cmd", `String "gh pr list" ])
      ; tool_response "c1" (`Assoc [ "cmd", `String "gh pr list" ])
      ; final_response
      ]
  in
  let next_response request =
    requests := !requests @ [ request ];
    append events (Printf.sprintf "provider%d" (List.length !requests));
    match !responses with
    | response :: rest ->
      responses := rest;
      response
    | [] -> Alcotest.fail "unexpected additional provider call"
  in
  let transport : Llm_provider.Llm_transport.t =
    { complete_sync =
        (fun request ->
          { Llm_provider.Llm_transport.response = Ok (next_response request)
          ; latency_ms = Some 0
          })
    ; complete_stream =
        (fun ?on_telemetry:_ ~on_event:_ request -> Ok (next_response request))
    }
  in
  let judge =
    Tool_failure_recovery.create ~complete:(fun ~sw:_ request ->
      append events "judge";
      Alcotest.(check bool)
        "judge receives structured schema"
        true
        (request.Tool_failure_recovery.output_schema <> `Null);
      Ok judge_json)
  in
  let checkpoint_sink (snapshot : Agent.checkpoint_snapshot) =
    match snapshot.stage with
    | After_retry_feedback_appended ->
      append events "decision_checkpoint";
      decision_checkpoint := Some snapshot.checkpoint;
      if fail_recovery_checkpoint then Error "recovery checkpoint rejected" else Ok ()
    | After_assistant_collected | After_tool_results_appended -> Ok ()
  in
  let options =
    { Agent.default_options with
      transport = Some transport
    ; provider = Some provider
    ; guardrails = Guardrails.permissive
    }
  in
  let config =
    { Types.default_config with
      name = "recovery-test"
    ; model = "mock-model"
    ; system_prompt = Some "base system"
    ; max_turns = 0
    }
  in
  let agent =
    Agent.create
      ~net:env#net
      ~config
      ~tools:[ failing_tool events ]
      ~options
      ~checkpoint_sink
      ~tool_failure_judge:judge
      ()
  in
  Alcotest.(check bool)
    "judge enables tool yielding"
    true
    (Agent.state agent).config.yield_on_tool;
  let result =
    Agent.run
      ~sw
      ~on_yield:(fun () -> append events "yield")
      ~on_resume:(fun () -> append events "resume")
      agent
      "review the pull request"
  in
  { result
  ; events = !events
  ; requests = !requests
  ; agent
  ; decision_checkpoint = !decision_checkpoint
  }
;;

let retry_modified_json =
  {|{"action":"retry_modified","revised_calls":[{"current_tool_use_id":"c1","tool_name":"Execute","revised_input":{"cmd":"gh pr list","cwd":"/repo"}}]}|}
;;

let test_judge_runs_between_yield_and_resume () =
  Eio_main.run
  @@ fun env ->
  let scenario = run_scenario env ~judge_json:retry_modified_json () in
  (match scenario.result with
   | Ok response ->
     Alcotest.(check string) "final text" "finished" (Types.text_of_response response)
   | Error error -> Alcotest.fail (Error.to_string error));
  Alcotest.(check (list string))
    "lease-safe order"
    [ "provider1"
    ; "tool"
    ; "yield"
    ; "resume"
    ; "provider2"
    ; "tool"
    ; "yield"
    ; "judge"
    ; "decision_checkpoint"
    ; "resume"
    ; "provider3"
    ]
    scenario.events;
  let third = List.nth scenario.requests 2 in
  let system_prompt = Option.value third.config.system_prompt ~default:"" in
  Alcotest.(check bool)
    "recovery is a system suffix"
    true
    (String.starts_with ~prefix:"base system\n\nOAS one-turn typed" system_prompt);
  let user_messages =
    List.filter (fun (message : Types.message) -> message.role = User) third.messages
  in
  Alcotest.(check int) "no synthetic recovery User message" 1 (List.length user_messages)
;;

let test_ask_user_stops_without_resume_or_third_call () =
  Eio_main.run
  @@ fun env ->
  let scenario =
    run_scenario
      env
      ~judge_json:
        {|{"action":"ask_user","question":"Which repository should I inspect?"}|}
      ()
  in
  (match scenario.result with
   | Error (Error.Agent (Error.InputRequired request)) ->
     Alcotest.(check string)
       "question"
       "Which repository should I inspect?"
       request.question
   | Error error -> Alcotest.fail (Error.to_string error)
   | Ok _ -> Alcotest.fail "expected InputRequired");
  Alcotest.(check int) "two main calls" 2 (List.length scenario.requests);
  Alcotest.(check (list string))
    "terminal judge does not reacquire"
    [ "provider1"
    ; "tool"
    ; "yield"
    ; "resume"
    ; "provider2"
    ; "tool"
    ; "yield"
    ; "judge"
    ; "decision_checkpoint"
    ]
    scenario.events
;;

let test_checkpoint_failure_does_not_commit_receipt () =
  Eio_main.run
  @@ fun env ->
  let scenario =
    run_scenario env ~judge_json:retry_modified_json ~fail_recovery_checkpoint:true ()
  in
  (match scenario.result with
   | Error (Error.Internal detail) ->
     Alcotest.(check bool) "checkpoint failure surfaced" true (String.length detail > 0)
   | Error error -> Alcotest.fail (Error.to_string error)
   | Ok _ -> Alcotest.fail "expected checkpoint failure");
  (match Tool_failure_recovery.latest_receipt (Agent.state scenario.agent).messages with
   | Ok None -> ()
   | Ok (Some _) -> Alcotest.fail "failed checkpoint decision leaked into live state"
   | Error error -> Alcotest.fail (Tool_failure_recovery.show_receipt_error error));
  Alcotest.(check int) "no third main call" 2 (List.length scenario.requests)
;;

let test_checkpoint_roundtrip_preserves_canonical_rounds () =
  Eio_main.run
  @@ fun env ->
  let scenario =
    run_scenario
      env
      ~judge_json:{|{"action":"ask_user","question":"Which repository?"}|}
      ()
  in
  let checkpoint = Agent.checkpoint ~session_id:"typed-recovery" scenario.agent in
  let restored = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json checkpoint)) in
  match Tool_failure_episode.latest_completed_rounds ~count:2 restored.messages with
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  | Ok [ current; previous ] ->
    (match Tool_failure_episode.detect ~previous ~current with
     | [ episode ] ->
       Alcotest.(check string) "canonical tool" "Execute" episode.current.tool_name;
       Alcotest.(check bool)
         "failure kind"
         true
         (episode.current.failure_kind = Types.Recoverable_tool_error);
       Alcotest.(check bool)
         "error class"
         true
         (episode.current.error_class = Some Types.Deterministic);
       Alcotest.(check bool)
         "executed input"
         true
         (Yojson.Safe.equal
            episode.current.input
            (`Assoc [ "cmd", `String "gh pr list" ]))
     | episodes ->
       Alcotest.failf "expected one restored episode, got %d" (List.length episodes))
  | Ok rounds ->
    Alcotest.failf "expected two restored rounds, got %d" (List.length rounds)
;;

let test_new_run_boundary_hides_old_receipt () =
  Eio_main.run
  @@ fun env ->
  let scenario =
    run_scenario
      env
      ~judge_json:{|{"action":"ask_user","question":"Which repository?"}|}
      ()
  in
  let boundary =
    Types.make_message
      ~metadata:Types.Conversation_metadata.run_boundary
      ~role:Types.User
      [ Types.Text "new request" ]
  in
  let messages = (Agent.state scenario.agent).messages @ [ boundary ] in
  match Tool_failure_recovery.latest_receipt messages with
  | Ok None -> ()
  | Ok (Some _) -> Alcotest.fail "receipt crossed the new run boundary"
  | Error error -> Alcotest.fail (Tool_failure_recovery.show_receipt_error error)
;;

let test_attach_receipt_does_not_cross_new_run_boundary () =
  Eio_main.run
  @@ fun env ->
  let scenario =
    run_scenario
      env
      ~judge_json:{|{"action":"ask_user","question":"Which repository?"}|}
      ()
  in
  let messages = (Agent.state scenario.agent).messages in
  let episodes =
    match Tool_failure_episode.latest_completed_rounds ~count:2 messages with
    | Ok [ current; previous ] -> Tool_failure_episode.detect ~previous ~current
    | Ok rounds ->
      Alcotest.failf "expected two completed rounds, got %d" (List.length rounds)
    | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  in
  let receipt =
    match Tool_failure_recovery.latest_receipt messages with
    | Ok (Some receipt) -> receipt
    | Ok None -> Alcotest.fail "expected recovery receipt"
    | Error error -> Alcotest.fail (Tool_failure_recovery.show_receipt_error error)
  in
  let boundary =
    Types.make_message
      ~metadata:Types.Conversation_metadata.run_boundary
      ~role:Types.User
      [ Types.Text "new request" ]
  in
  match
    Tool_failure_recovery.attach_receipt
      ~messages:(messages @ [ boundary ])
      ~episodes
      ~receipt
  with
  | Error Tool_failure_recovery.Result_message_not_found -> ()
  | Error error -> Alcotest.fail (Tool_failure_recovery.show_receipt_error error)
  | Ok _ -> Alcotest.fail "receipt crossed the new run boundary"
;;

let mutate_receipt_fields messages mutate =
  let changed = ref false in
  let messages =
    List.map
      (fun (message : Types.message) ->
         let metadata =
           List.map
             (fun (key, json) ->
                match !changed, json with
                | false, `Assoc fields when List.mem_assoc "version" fields ->
                  changed := true;
                  key, `Assoc (mutate fields)
                | _ -> key, json)
             message.metadata
         in
         { message with metadata })
      messages
  in
  if not !changed then Alcotest.fail "recovery receipt metadata not found";
  messages
;;

let test_receipt_record_is_closed () =
  Eio_main.run
  @@ fun env ->
  let scenario =
    run_scenario
      env
      ~judge_json:{|{"action":"ask_user","question":"Which repository?"}|}
      ()
  in
  let messages = (Agent.state scenario.agent).messages in
  let check_invalid label mutate =
    let messages = mutate_receipt_fields messages mutate in
    match Tool_failure_recovery.latest_receipt messages with
    | Error (Tool_failure_recovery.Invalid_receipt_metadata _) -> ()
    | Error error ->
      Alcotest.failf "%s: %s" label (Tool_failure_recovery.show_receipt_error error)
    | Ok _ -> Alcotest.failf "%s: expected invalid receipt metadata" label
  in
  check_invalid "duplicate field" (fun fields -> ("version", `Int 1) :: fields);
  check_invalid "unexpected field" (fun fields -> ("unexpected", `Null) :: fields)
;;

let project calls results =
  let executions =
    List.map
      (function
        | Types.ToolUse { id; name; input } ->
          ({ tool_use_id = id; tool_name = name; input }
           : Tool_failure_episode.executed_call)
        | _ -> Alcotest.fail "expected ToolUse fixture")
      calls
  in
  match Tool_failure_episode.project ~executions ~tool_results:results with
  | Ok round -> round
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let failed_result id : Types.content_block =
  Types.ToolResult
    { tool_use_id = id
    ; content = "failed"
    ; outcome =
        Types.Tool_failed
          { failure_kind = Types.Recoverable_tool_error
          ; error_class = Some Types.Deterministic
          }
    ; json = None
    ; content_blocks = None
    }
;;

let sample_episodes () =
  let previous =
    project
      [ ToolUse { id = "p1"; name = "Execute"; input = `Assoc [ "cmd", `String "x" ] } ]
      [ failed_result "p1" ]
  in
  let current =
    project
      [ ToolUse { id = "c1"; name = "Execute"; input = `Assoc [ "cmd", `String "x" ] } ]
      [ failed_result "c1" ]
  in
  Tool_failure_episode.detect ~previous ~current
;;

let resumed_final_run env ~checkpoint ~judge =
  Eio.Switch.run
  @@ fun sw ->
  let requests = ref [] in
  let record request =
    requests := !requests @ [ request ];
    final_response
  in
  let transport : Llm_provider.Llm_transport.t =
    { complete_sync =
        (fun request ->
          { Llm_provider.Llm_transport.response = Ok (record request)
          ; latency_ms = Some 0
          })
    ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ request -> Ok (record request))
    }
  in
  let options =
    { Agent.default_options with
      transport = Some transport
    ; provider = Some provider
    ; guardrails = Guardrails.permissive
    }
  in
  let events = ref [] in
  let agent =
    Agent.resume
      ~net:env#net
      ~checkpoint
      ~tools:[ failing_tool events ]
      ~options
      ~tool_failure_judge:judge
      ()
  in
  let result = Agent.run_turn_stream ~sw ~on_event:(fun _ -> ()) agent in
  result, !requests
;;

let test_decision_checkpoint_resumes_without_rejudging () =
  Eio_main.run
  @@ fun env ->
  let scenario = run_scenario env ~judge_json:retry_modified_json () in
  let checkpoint =
    match scenario.decision_checkpoint with
    | Some checkpoint -> checkpoint
    | None -> Alcotest.fail "missing recovery decision checkpoint"
  in
  let judge_calls = ref 0 in
  let judge =
    Tool_failure_recovery.create ~complete:(fun ~sw:_ _ ->
      incr judge_calls;
      Ok {|{"action":"replan","instruction":"must not run"}|})
  in
  let result, requests = resumed_final_run env ~checkpoint ~judge in
  (match result with
   | Ok (`Complete response) ->
     Alcotest.(check string)
       "resumed final text"
       "finished"
       (Types.text_of_response response)
   | Ok `ToolsExecuted -> Alcotest.fail "unexpected resumed tool execution"
   | Error error -> Alcotest.fail (Error.to_string error));
  Alcotest.(check int) "persisted receipt skips judge" 0 !judge_calls;
  Alcotest.(check int) "one resumed provider call" 1 (List.length requests);
  let request = List.hd requests in
  let system_prompt = Option.value request.config.system_prompt ~default:"" in
  Alcotest.(check bool)
    "persisted control restored"
    true
    (String.starts_with ~prefix:"base system\n\nOAS one-turn typed" system_prompt)
;;

let message ~role ~content ~metadata : Types.message =
  { role; content; name = None; tool_call_id = None; metadata }
;;

let test_resume_does_not_correlate_across_external_user_runs () =
  Eio_main.run
  @@ fun env ->
  let seed =
    Agent.create
      ~net:env#net
      ~config:
        { Types.default_config with
          name = "boundary-test"
        ; model = "mock-model"
        ; system_prompt = Some "base system"
        ; max_turns = 0
        ; yield_on_tool = true
        }
      ()
  in
  let boundary = Types.Conversation_metadata.run_boundary in
  let input = `Assoc [ "cmd", `String "gh pr list" ] in
  let call id : Types.content_block = Types.ToolUse { id; name = "Execute"; input } in
  let round_metadata id =
    [ Tool_failure_episode.completed_round_metadata
        [ ({ tool_use_id = id; tool_name = "Execute"; input }
           : Tool_failure_episode.executed_call)
        ]
    ]
  in
  let checkpoint =
    { (Agent.checkpoint seed) with
      turn_count = 2
    ; messages =
        [ message ~role:User ~content:[ Text "first request" ] ~metadata:boundary
        ; message ~role:Assistant ~content:[ call "p1" ] ~metadata:[]
        ; message
            ~role:Tool
            ~content:[ failed_result "p1" ]
            ~metadata:(round_metadata "p1")
        ; message ~role:User ~content:[ Text "second request" ] ~metadata:boundary
        ; message ~role:Assistant ~content:[ call "c1" ] ~metadata:[]
        ; message
            ~role:Tool
            ~content:[ failed_result "c1" ]
            ~metadata:(round_metadata "c1")
        ]
    }
  in
  let judge_calls = ref 0 in
  let judge =
    Tool_failure_recovery.create ~complete:(fun ~sw:_ _ ->
      incr judge_calls;
      Ok {|{"action":"replan","instruction":"must not run"}|})
  in
  let result, requests = resumed_final_run env ~checkpoint ~judge in
  (match result with
   | Ok (`Complete _) -> ()
   | Ok `ToolsExecuted -> Alcotest.fail "unexpected resumed tool execution"
   | Error error -> Alcotest.fail (Error.to_string error));
  Alcotest.(check int) "no cross-user judge call" 0 !judge_calls;
  Alcotest.(check int) "main provider still runs" 1 (List.length requests)
;;

let test_resume_rejects_result_without_execution_metadata () =
  Eio_main.run
  @@ fun env ->
  let seed =
    Agent.create
      ~net:env#net
      ~config:
        { Types.default_config with
          name = "missing-execution-metadata-test"
        ; model = "mock-model"
        ; system_prompt = Some "base system"
        ; max_turns = 0
        }
      ()
  in
  let input = `Assoc [ "cmd", `String "gh pr list" ] in
  let checkpoint =
    { (Agent.checkpoint seed) with
      turn_count = 1
    ; messages =
        [ message
            ~role:User
            ~content:[ Text "request" ]
            ~metadata:Types.Conversation_metadata.run_boundary
        ; message
            ~role:Assistant
            ~content:[ ToolUse { id = "c1"; name = "Execute"; input } ]
            ~metadata:[]
        ; message ~role:Tool ~content:[ failed_result "c1" ] ~metadata:[]
        ]
    }
  in
  let judge_calls = ref 0 in
  let judge =
    Tool_failure_recovery.create ~complete:(fun ~sw:_ _ ->
      incr judge_calls;
      Ok {|{"action":"replan","instruction":"must not run"}|})
  in
  let result, requests = resumed_final_run env ~checkpoint ~judge in
  (match result with
   | Error
       (Error.Agent
          (Error.ToolFailureRecoveryFailed { stage = Error.Resume_restore; detail })) ->
     Alcotest.(check bool) "explicit restore detail" true (String.length detail > 0)
   | Error error -> Alcotest.fail (Error.to_string error)
   | Ok _ -> Alcotest.fail "expected explicit recovery restore error");
  Alcotest.(check int) "judge not called" 0 !judge_calls;
  Alcotest.(check int) "provider not called" 0 (List.length requests)
;;

let test_run_boundary_metadata_remains_provider_mergeable () =
  let messages =
    [ message ~role:Tool ~content:[ failed_result "c1" ] ~metadata:[]
    ; message
        ~role:User
        ~content:[ Text "next external request" ]
        ~metadata:Types.Conversation_metadata.run_boundary
    ]
  in
  match Llm_provider.Api_common.merge_tool_result_followup_user_messages messages with
  | [ merged ] ->
    Alcotest.(check int)
      "tool result and external follow-up share one provider user span"
      2
      (List.length merged.Types.content)
  | merged ->
    Alcotest.fail
      (Printf.sprintf "expected one provider message, got %d" (List.length merged))
;;

let test_retry_modified_requires_changed_input () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let episodes = sample_episodes () in
  let judge =
    Tool_failure_recovery.create ~complete:(fun ~sw:_ _ ->
      Ok
        {|{"action":"retry_modified","revised_calls":[{"current_tool_use_id":"c1","tool_name":"Execute","revised_input":{"cmd":"x"}}]}|})
  in
  match Tool_failure_recovery.decide ~sw ~agent_name:"test" ~turn:2 ~episodes judge with
  | Error
      (Tool_failure_recovery.Invalid_response
         (Tool_failure_recovery.Invalid_decision
            (Tool_failure_recovery.Unchanged_revised_input { current_tool_use_id = "c1" })))
    -> ()
  | Error error -> Alcotest.fail (Tool_failure_recovery.judge_error_to_string error)
  | Ok decision ->
    Alcotest.fail
      ("expected unchanged-input rejection, got "
       ^ Tool_failure_recovery.show_decision decision)
;;

let decide_fixture sw text =
  let judge = Tool_failure_recovery.create ~complete:(fun ~sw:_ _ -> Ok text) in
  Tool_failure_recovery.decide
    ~sw
    ~agent_name:"test"
    ~turn:2
    ~episodes:(sample_episodes ())
    judge
;;

let test_response_rejects_unexpected_field () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  match
    decide_fixture
      sw
      {|{"action":"replan","instruction":"use repository cwd","reason":"ignored"}|}
  with
  | Error
      (Tool_failure_recovery.Invalid_response
         (Tool_failure_recovery.Unexpected_field "reason")) -> ()
  | Error error -> Alcotest.fail (Tool_failure_recovery.judge_error_to_string error)
  | Ok decision ->
    Alcotest.fail
      ("expected unexpected-field rejection, got "
       ^ Tool_failure_recovery.show_decision decision)
;;

let test_response_rejects_duplicate_field () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  match
    decide_fixture
      sw
      {|{"action":"replan","action":"defer","instruction":"use cwd","reason":"wait"}|}
  with
  | Error
      (Tool_failure_recovery.Invalid_response
         (Tool_failure_recovery.Duplicate_field "action")) -> ()
  | Error error -> Alcotest.fail (Tool_failure_recovery.judge_error_to_string error)
  | Ok decision ->
    Alcotest.fail
      ("expected duplicate-field rejection, got "
       ^ Tool_failure_recovery.show_decision decision)
;;

let test_decision_observation_omits_revised_input () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  match
    decide_fixture
      sw
      {|{"action":"retry_modified","revised_calls":[{"current_tool_use_id":"c1","tool_name":"Execute","revised_input":{"cmd":"private"}}]}|}
  with
  | Error error -> Alcotest.fail (Tool_failure_recovery.judge_error_to_string error)
  | Ok decision ->
    let observation = Tool_failure_recovery.decision_observation_to_yojson decision in
    let open Yojson.Safe.Util in
    let field_names =
      observation |> to_assoc |> List.map fst |> List.sort String.compare
    in
    Alcotest.(check (list string))
      "only the closed decision observation fields are emitted"
      [ "action"; "call_count"; "tool_names" ]
      field_names;
    Alcotest.(check string)
      "action retained"
      "retry_modified"
      (observation |> member "action" |> to_string);
    Alcotest.(check int)
      "call count retained"
      1
      (observation |> member "call_count" |> to_int)
;;

let () =
  Alcotest.run
    "tool_failure_recovery"
    [ ( "agent_loop"
      , [ Alcotest.test_case
            "judge between yield and resume"
            `Quick
            test_judge_runs_between_yield_and_resume
        ; Alcotest.test_case
            "ask user is terminal"
            `Quick
            test_ask_user_stops_without_resume_or_third_call
        ; Alcotest.test_case
            "checkpoint failure is transactional"
            `Quick
            test_checkpoint_failure_does_not_commit_receipt
        ; Alcotest.test_case
            "decision checkpoint resumes without rejudging"
            `Quick
            test_decision_checkpoint_resumes_without_rejudging
        ; Alcotest.test_case
            "resume respects external user run boundary"
            `Quick
            test_resume_does_not_correlate_across_external_user_runs
        ; Alcotest.test_case
            "resume requires execution metadata"
            `Quick
            test_resume_rejects_result_without_execution_metadata
        ; Alcotest.test_case
            "run boundary metadata is provider-mergeable"
            `Quick
            test_run_boundary_metadata_remains_provider_mergeable
        ; Alcotest.test_case
            "checkpoint preserves canonical rounds"
            `Quick
            test_checkpoint_roundtrip_preserves_canonical_rounds
        ; Alcotest.test_case
            "new run hides old receipt"
            `Quick
            test_new_run_boundary_hides_old_receipt
        ; Alcotest.test_case
            "receipt attachment respects new run"
            `Quick
            test_attach_receipt_does_not_cross_new_run_boundary
        ; Alcotest.test_case
            "receipt record is closed"
            `Quick
            test_receipt_record_is_closed
        ] )
    ; ( "decision_validation"
      , [ Alcotest.test_case
            "retry input must change"
            `Quick
            test_retry_modified_requires_changed_input
        ; Alcotest.test_case
            "unexpected response field is rejected"
            `Quick
            test_response_rejects_unexpected_field
        ; Alcotest.test_case
            "duplicate response field is rejected"
            `Quick
            test_response_rejects_duplicate_field
        ; Alcotest.test_case
            "decision observation omits revised input"
            `Quick
            test_decision_observation_omits_revised_input
        ] )
    ]
;;
