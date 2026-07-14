(** Typed cooperative tool-boundary execution tests. *)

open Agent_sdk

let mock_provider : Provider.config =
  { provider = Provider.Local { base_url = "http://mock.local" }
  ; model_id = "mock-model"
  ; api_key_env = ""
  }
;;

let text_response text : Types.api_response =
  { id = "advanced-text"
  ; model = "mock-model"
  ; stop_reason = Types.EndTurn
  ; content = [ Types.Text text ]
  ; usage = None
  ; telemetry = None
  }
;;

let tool_use_response : Types.api_response =
  { id = "advanced-tool"
  ; model = "mock-model"
  ; stop_reason = Types.StopToolUse
  ; content =
      [ Types.ToolUse
          { id = "call_1"
          ; name = "get_time"
          ; input = `Assoc [ "timezone", `String "UTC" ]
          }
      ]
  ; usage = None
  ; telemetry = None
  }
;;

let sequence_transport responses =
  let remaining = ref responses in
  let call_count = ref 0 in
  let next () =
    incr call_count;
    match !remaining with
    | response :: rest ->
      remaining := rest;
      response
    | [] -> Alcotest.fail "mock transport exhausted"
  in
  let transport : Llm_provider.Llm_transport.t =
    { complete_sync =
        (fun _request ->
          { Llm_provider.Llm_transport.response = Ok (next ()); latency_ms = Some 0 })
    ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ _request -> Ok (next ()))
    }
  in
  transport, call_count
;;

let make_agent
      ~net
      ~transport
      ~raw_trace
      ~checkpoint_sink
      ~context_injector
      ~on_run_complete
      ~tool
  =
  let options =
    { Agent.default_options with
      provider = Some mock_provider
    ; transport = Some transport
    ; raw_trace = Some raw_trace
    ; context_injector
    ; on_run_complete
    }
  in
  let config =
    { (Types.default_config ~model:"mock-model") with name = "advanced-boundary-test" }
  in
  Agent.create ~net ~config ~tools:[ tool ] ~options ~checkpoint_sink ()
;;

let time_tool on_execute =
  Tool.create
    ~name:"get_time"
    ~description:"Get current time"
    ~parameters:
      [ { Types.name = "timezone"
        ; param_type = Types.String
        ; description = "timezone"
        ; required = true
        }
      ]
    (fun _input ->
       on_execute ();
       Ok { Types.content = "12:00 UTC"; _meta = None })
;;

let messages_contain_text expected messages =
  List.exists
    (fun (message : Types.message) ->
       List.exists
         (function
           | Types.Text text -> String.equal expected text
           | Types.Thinking _
           | Types.ReasoningDetails _
           | Types.RedactedThinking _
           | Types.ToolUse _
           | Types.ToolResult _
           | Types.Image _
           | Types.Document _
           | Types.Audio _ -> false)
         message.content)
    messages
;;

let last_record path =
  match Raw_trace.read_all ~path () with
  | Error error -> Alcotest.fail (Error.to_string error)
  | Ok records ->
    (match List.rev records with
     | record :: _ -> record
     | [] -> Alcotest.fail "raw trace is empty")
;;

let with_temp_trace f =
  let path = Filename.temp_file "oas-agent-advanced" ".jsonl" in
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists path then Sys.remove path)
    (fun () -> f path)
;;

let test_yield_after_context_checkpoint () =
  with_temp_trace
  @@ fun trace_path ->
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let trace = Raw_trace.create ~path:trace_path () |> Result.get_ok in
  let persisted = ref [] in
  let checkpoint_sink snapshot =
    persisted := snapshot :: !persisted;
    Ok ()
  in
  let tool_executed = ref false in
  let context_injected = ref false in
  let context_injector ~tool_name:_ ~input:_ ~output:_ =
    context_injected := true;
    Some
      { Hooks.context_updates = []
      ; extra_messages =
          [ Types.make_message ~role:Types.User [ Types.Text "projected context" ] ]
      }
  in
  let completions = ref [] in
  let transport, call_count = sequence_transport [ tool_use_response ] in
  let agent =
    make_agent
      ~net:env#net
      ~transport
      ~raw_trace:trace
      ~checkpoint_sink
      ~context_injector:(Some context_injector)
      ~on_run_complete:(Some (fun completed -> completions := completed :: !completions))
      ~tool:(time_tool (fun () -> tool_executed := true))
  in
  let callback_count = ref 0 in
  let on_tool_boundary (boundary : Agent.Advanced.tool_boundary) =
    incr callback_count;
    Alcotest.(check bool) "tool completed before callback" true !tool_executed;
    Alcotest.(check bool) "context injected before callback" true !context_injected;
    (match !persisted with
     | latest :: _ ->
       Alcotest.(check bool)
         "context checkpoint persisted before callback"
         true
         (latest.Agent.stage = Agent.After_context_injection)
     | [] -> Alcotest.fail "callback ran without a successful checkpoint");
    Alcotest.(check int) "boundary turn" 1 boundary.turn;
    Alcotest.(check bool)
      "boundary stage"
      true
      (boundary.checkpoint_stage = Agent.After_context_injection);
    Agent.Advanced.Yield
  in
  (match
     Agent.Advanced.run_blocks
       ~sw
       ~api_strategy:Agent.Sync
       ~on_tool_boundary
       agent
       [ Types.Text "what time is it?" ]
   with
   | Error error -> Alcotest.fail (Error.to_string error)
   | Ok (Agent.Advanced.Completed _) -> Alcotest.fail "expected cooperative yield"
   | Ok (Agent.Advanced.Yielded yielded) ->
     Alcotest.(check int) "yielded turn" 1 yielded.turn;
     Alcotest.(check int) "checkpoint turn" 1 yielded.checkpoint.turn_count;
     Alcotest.(check bool)
       "yielded checkpoint stage"
       true
       (yielded.checkpoint_stage = Agent.After_context_injection);
     Alcotest.(check bool)
       "yielded checkpoint includes projected context"
       true
       (messages_contain_text "projected context" yielded.checkpoint.messages));
  Alcotest.(check int) "callback count" 1 !callback_count;
  Alcotest.(check int) "provider call count" 1 !call_count;
  Alcotest.(check (list bool)) "not terminal-complete" [] !completions;
  (match Agent.lifecycle agent with
   | Some snapshot ->
     Alcotest.(check bool) "lifecycle ready" true (snapshot.status = Agent.Ready);
     Alcotest.(check (option string)) "no lifecycle error" None snapshot.last_error
   | None -> Alcotest.fail "missing lifecycle snapshot");
  let record = last_record trace_path in
  Alcotest.(check bool) "trace finished" true (record.record_type = Raw_trace.Run_finished);
  Alcotest.(check (option string)) "trace error" None record.error;
  Alcotest.(check (option string))
    "trace yield outcome"
    (Some "cooperative_tool_boundary_yield")
    record.stop_reason
;;

let test_continue_reaches_terminal_completion () =
  with_temp_trace
  @@ fun trace_path ->
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let trace = Raw_trace.create ~path:trace_path () |> Result.get_ok in
  let checkpoint_sink _snapshot = Ok () in
  let completions = ref [] in
  let transport, call_count =
    sequence_transport [ tool_use_response; text_response "done" ]
  in
  let agent =
    make_agent
      ~net:env#net
      ~transport
      ~raw_trace:trace
      ~checkpoint_sink
      ~context_injector:None
      ~on_run_complete:(Some (fun completed -> completions := completed :: !completions))
      ~tool:(time_tool ignore)
  in
  let callback_count = ref 0 in
  let on_tool_boundary (boundary : Agent.Advanced.tool_boundary) =
    incr callback_count;
    Alcotest.(check bool)
      "base tool-result checkpoint boundary"
      true
      (boundary.checkpoint_stage = Agent.After_tool_results_appended);
    Agent.Advanced.Continue
  in
  (match
     Agent.Advanced.run_blocks
       ~sw
       ~api_strategy:Agent.Sync
       ~on_tool_boundary
       agent
       [ Types.Text "what time is it?" ]
   with
   | Error error -> Alcotest.fail (Error.to_string error)
   | Ok (Agent.Advanced.Yielded _) -> Alcotest.fail "expected terminal completion"
   | Ok (Agent.Advanced.Completed response) ->
     Alcotest.(check string)
       "visible response"
       "done"
       (Types.visible_text_of_response response));
  Alcotest.(check int) "callback count" 1 !callback_count;
  Alcotest.(check int) "provider call count" 2 !call_count;
  Alcotest.(check (list bool)) "terminal callback" [ true ] !completions;
  (match Agent.lifecycle agent with
   | Some snapshot ->
     Alcotest.(check bool) "lifecycle completed" true (snapshot.status = Agent.Completed)
   | None -> Alcotest.fail "missing lifecycle snapshot");
  let record = last_record trace_path in
  Alcotest.(check (option string)) "trace error" None record.error;
  Alcotest.(check (option string))
    "terminal stop reason"
    (Some (Types.show_stop_reason Types.EndTurn))
    record.stop_reason
;;

let test_checkpoint_failure_prevents_callback () =
  with_temp_trace
  @@ fun trace_path ->
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let trace = Raw_trace.create ~path:trace_path () |> Result.get_ok in
  let checkpoint_sink (snapshot : Agent.checkpoint_snapshot) =
    match snapshot.stage with
    | Agent.After_context_injection -> Error "durable sink rejected boundary"
    | Agent.After_assistant_collected | Agent.After_tool_results_appended -> Ok ()
  in
  let context_injector ~tool_name:_ ~input:_ ~output:_ =
    Some { Hooks.context_updates = []; extra_messages = [] }
  in
  let transport, _call_count = sequence_transport [ tool_use_response ] in
  let agent =
    make_agent
      ~net:env#net
      ~transport
      ~raw_trace:trace
      ~checkpoint_sink
      ~context_injector:(Some context_injector)
      ~on_run_complete:None
      ~tool:(time_tool ignore)
  in
  let callback_count = ref 0 in
  let outcome =
    Agent.Advanced.run_blocks
      ~sw
      ~api_strategy:Agent.Sync
      ~on_tool_boundary:(fun _boundary ->
        incr callback_count;
        Agent.Advanced.Yield)
      agent
      [ Types.Text "what time is it?" ]
  in
  (match outcome with
   | Error (Error.Internal _) -> ()
   | Error error -> Alcotest.fail ("unexpected error: " ^ Error.to_string error)
   | Ok _ -> Alcotest.fail "expected checkpoint failure");
  Alcotest.(check int) "callback suppressed" 0 !callback_count;
  (match Agent.lifecycle agent with
   | Some snapshot ->
     Alcotest.(check bool) "lifecycle failed" true (snapshot.status = Agent.Failed)
   | None -> Alcotest.fail "missing lifecycle snapshot");
  let record = last_record trace_path in
  Alcotest.(check bool)
    "trace has typed failure detail"
    true
    (Option.is_some record.error);
  Alcotest.(check (option string)) "no yield stop reason" None record.stop_reason
;;

let () =
  Alcotest.run
    "Agent advanced cooperative execution"
    [ ( "tool boundary"
      , [ Alcotest.test_case
            "yield after context checkpoint"
            `Quick
            test_yield_after_context_checkpoint
        ; Alcotest.test_case
            "continue reaches terminal completion"
            `Quick
            test_continue_reaches_terminal_completion
        ; Alcotest.test_case
            "checkpoint failure prevents callback"
            `Quick
            test_checkpoint_failure_prevents_callback
        ] )
    ]
;;
