open Agent_sdk
module Internal_agent = Agent_sdk__Agent_types
module Pipeline_common = Agent_sdk__Pipeline_common

let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)
let check_string = Alcotest.(check string)

let provider_d_config : Provider.config =
  { provider = Local { base_url = "http://127.0.0.1:65535" }
  ; model_id = "provider_d_chat"
  ; api_key_env = "DUMMY_KEY"
  }
;;

let text_response ?(content = [ Types.Text "ok" ]) () : Types.api_response =
  { id = "resp-1"
  ; model = "provider_d_chat"
  ; stop_reason = EndTurn
  ; content
  ; usage = None
  ; telemetry = None
  }
;;

let with_agent
      ?(config = Types.default_config)
      ?(options = Internal_agent.default_options)
      f
  =
  Eio_main.run
  @@ fun env ->
  let agent = Internal_agent.create ~net:env#net ~config ~options () in
  f agent
;;

let test_strategy_and_outcome_constructors () =
  let events = ref 0 in
  let strategy =
    Pipeline_common.Stream
      { on_event = (fun _ -> incr events); on_telemetry = Some (fun _ -> incr events) }
  in
  (match strategy with
   | Pipeline_common.Stream { on_event; on_telemetry = Some _ } ->
     on_event MessageStop;
     check_int "event callback" 1 !events
   | _ -> Alcotest.fail "expected stream strategy");
  (match Pipeline_common.Sync with
   | Pipeline_common.Sync -> ()
   | _ -> Alcotest.fail "expected sync strategy");
  let complete = Pipeline_common.Complete (text_response ()) in
  (match complete with
   | Pipeline_common.Complete response -> check_string "response id" "resp-1" response.id
   | _ -> Alcotest.fail "expected complete outcome");
  (match Pipeline_common.ToolsExecuted with
   | Pipeline_common.ToolsExecuted -> ()
   | _ -> Alcotest.fail "expected tools outcome");
  match Pipeline_common.IdleSkipped with
  | Pipeline_common.IdleSkipped -> ()
  | _ -> Alcotest.fail "expected idle outcome"
;;

let test_validate_completion_contract_accepts_default_text () =
  with_agent (fun agent ->
    match Pipeline_common.validate_completion_contract agent (text_response ()) with
    | Ok () -> ()
    | Error err -> Alcotest.failf "unexpected contract error: %s" (Error.to_string err))
;;

let test_validate_completion_contract_rejects_missing_tool () =
  let config = { Types.default_config with tool_choice = Some Types.Any } in
  let options =
    { Internal_agent.default_options with provider = Some provider_d_config }
  in
  with_agent ~config ~options (fun agent ->
    match Pipeline_common.validate_completion_contract agent (text_response ()) with
    | Error (Error.Agent (CompletionContractViolation _)) -> ()
    | Error err -> Alcotest.failf "unexpected error: %s" (Error.to_string err)
    | Ok () -> Alcotest.fail "expected missing tool-use violation")
;;

let test_validate_completion_contract_accepts_tool_use () =
  let config = { Types.default_config with tool_choice = Some Types.Any } in
  let options =
    { Internal_agent.default_options with provider = Some provider_d_config }
  in
  let response =
    text_response
      ~content:[ Types.ToolUse { id = "call-1"; name = "lookup"; input = `Assoc [] } ]
      ()
  in
  with_agent ~config ~options (fun agent ->
    match Pipeline_common.validate_completion_contract agent response with
    | Ok () -> ()
    | Error err -> Alcotest.failf "unexpected contract error: %s" (Error.to_string err))
;;

let test_event_envelope_falls_back_without_trace () =
  with_agent (fun agent ->
    let envelope = Pipeline_common.event_envelope agent in
    check_bool "correlation id" true (String.length envelope.correlation_id > 0);
    check_bool "run id" true (String.length envelope.run_id > 0))
;;

let test_event_envelope_uses_trace_and_lifecycle () =
  let root =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-pipeline-common-%d" (Unix.getpid ()))
  in
  Unix.mkdir root 0o755;
  let trace_path = Filename.concat root "trace.jsonl" in
  let trace =
    match Raw_trace.create ~session_id:"session-123" ~path:trace_path () with
    | Ok trace -> trace
    | Error err -> Alcotest.fail (Error.to_string err)
  in
  let options = { Internal_agent.default_options with raw_trace = Some trace } in
  with_agent ~options (fun agent ->
    Internal_agent.set_lifecycle agent ~current_run_id:"run-123" Agent.Running;
    let envelope = Pipeline_common.event_envelope agent in
    check_string "trace session" "session-123" envelope.correlation_id;
    check_string "lifecycle run" "run-123" envelope.run_id);
  (try Sys.remove trace_path with
   | Sys_error _ -> ());
  try Unix.rmdir root with
  | Unix.Unix_error _ -> ()
;;

let test_total_prompt_tokens_for_agent_includes_tiered_memory () =
  let messages : Types.message list =
    [ { role = User
      ; content = [ Text "short request" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let base_tokens =
    with_agent (fun agent -> Pipeline_common.total_prompt_tokens_for_agent agent messages)
  in
  let tiered_memory : Types.tiered_memory =
    { long_term = Some "long term memory"
    ; mid_term = Some "mid term memory"
    ; short_term = Some "short term memory"
    }
  in
  let options =
    { Internal_agent.default_options with tiered_memory = Some tiered_memory }
  in
  let tiered_tokens =
    with_agent ~options (fun agent ->
      Pipeline_common.total_prompt_tokens_for_agent agent messages)
  in
  check_bool "base tokens positive" true (base_tokens > 0);
  check_bool "tiered memory adds tokens" true (tiered_tokens > base_tokens)
;;

let () =
  Alcotest.run
    "Pipeline_common_coverage"
    [ ( "types"
      , [ Alcotest.test_case
            "strategy and outcome constructors"
            `Quick
            test_strategy_and_outcome_constructors
        ] )
    ; ( "contract"
      , [ Alcotest.test_case
            "default text accepted"
            `Quick
            test_validate_completion_contract_accepts_default_text
        ; Alcotest.test_case
            "missing required tool rejected"
            `Quick
            test_validate_completion_contract_rejects_missing_tool
        ; Alcotest.test_case
            "tool use accepted"
            `Quick
            test_validate_completion_contract_accepts_tool_use
        ] )
    ; ( "envelope"
      , [ Alcotest.test_case
            "fallback ids"
            `Quick
            test_event_envelope_falls_back_without_trace
        ; Alcotest.test_case
            "trace and lifecycle ids"
            `Quick
            test_event_envelope_uses_trace_and_lifecycle
        ] )
    ; ( "tokens"
      , [ Alcotest.test_case
            "tiered memory counted"
            `Quick
            test_total_prompt_tokens_for_agent_includes_tiered_memory
        ] )
    ]
;;
