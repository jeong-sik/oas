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
  }

let run_scenario env ~judge_json ?(fail_recovery_checkpoint = false) () =
  Eio.Switch.run
  @@ fun sw ->
  let events = ref [] in
  let requests = ref [] in
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
    ; yield_on_tool = true
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
  let result =
    Agent.run
      ~sw
      ~on_yield:(fun () -> append events "yield")
      ~on_resume:(fun () -> append events "resume")
      agent
      "review the pull request"
  in
  { result; events = !events; requests = !requests; agent }
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

let project calls results =
  match Tool_failure_episode.project ~tool_uses:calls ~tool_results:results with
  | Ok round -> round
  | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
;;

let failed_result id =
  Types.ToolResult
    { tool_use_id = id
    ; content = "failed"
    ; is_error = true
    ; failure_kind = Some Recoverable_tool_error
    ; error_class = Some Deterministic
    ; json = None
    ; content_blocks = None
    }
;;

let test_retry_modified_requires_changed_input () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
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
  let episodes =
    match Tool_failure_episode.detect ~previous ~current with
    | Ok episodes -> episodes
    | Error error -> Alcotest.fail (Tool_failure_episode.show_error error)
  in
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
        ] )
    ; ( "decision_validation"
      , [ Alcotest.test_case
            "retry input must change"
            `Quick
            test_retry_modified_requires_changed_input
        ] )
    ]
;;
