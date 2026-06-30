(** Unit tests for Agent module. *)

open Agent_sdk
open Types

(* --- find_handoff_in_messages --- *)

let test_find_handoff_none () =
  let msgs =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ Text "world" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  Alcotest.(check bool)
    "no handoff"
    true
    (Agent_handoff.find_handoff_in_messages msgs = None)
;;

let test_find_handoff_normal_tool () =
  let msgs =
    [ { role = User
      ; content = [ Text "use_tool" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content =
          [ ToolUse { id = "t1"; name = "calculator"; input = `Assoc [ "a", `Int 1 ] } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  Alcotest.(check bool)
    "non-handoff tool"
    true
    (Agent_handoff.find_handoff_in_messages msgs = None)
;;

let test_find_handoff_present () =
  let input = `Assoc [ "prompt", `String "research this" ] in
  let msgs =
    [ { role = User
      ; content = [ Text "delegate" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ ToolUse { id = "h1"; name = "transfer_to_researcher"; input } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match Agent_handoff.find_handoff_in_messages msgs with
  | Some (id, name, prompt) ->
    Alcotest.(check string) "tool id" "h1" id;
    Alcotest.(check string) "target name" "researcher" name;
    Alcotest.(check string) "prompt" "research this" prompt
  | None -> Alcotest.fail "expected Some handoff"
;;

let test_find_handoff_no_prompt_field () =
  let input = `Assoc [ "other", `Int 42 ] in
  let msgs =
    [ { role = Assistant
      ; content = [ ToolUse { id = "h2"; name = "transfer_to_coder"; input } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match Agent_handoff.find_handoff_in_messages msgs with
  | Some (_, _, prompt) ->
    Alcotest.(check string) "default prompt" "Continue the conversation." prompt
  | None -> Alcotest.fail "expected Some handoff"
;;

let test_find_handoff_empty () =
  Alcotest.(check bool)
    "empty messages"
    true
    (Agent_handoff.find_handoff_in_messages [] = None)
;;

let test_find_handoff_mixed_content () =
  let msgs =
    [ { role = Assistant
      ; content =
          [ Text "I'll delegate"
          ; ToolUse
              { id = "h3"
              ; name = "transfer_to_analyst"
              ; input = `Assoc [ "prompt", `String "analyze" ]
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  match Agent_handoff.find_handoff_in_messages msgs with
  | Some (id, name, prompt) ->
    Alcotest.(check string) "id" "h3" id;
    Alcotest.(check string) "name" "analyst" name;
    Alcotest.(check string) "prompt" "analyze" prompt
  | None -> Alcotest.fail "expected handoff in mixed content"
;;

(* --- replace_tool_result --- *)

let test_replace_existing () =
  let msgs =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Assistant
      ; content = [ ToolUse { id = "t1"; name = "calc"; input = `Null } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "old result"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let updated =
    Agent_handoff.replace_tool_result
      msgs
      ~tool_id:"t1"
      ~content:"new result"
      ~is_error:false
  in
  let last = List.nth updated (List.length updated - 1) in
  Alcotest.(check bool) "legacy role preserved" true (last.role = User);
  match last.content with
  | [ ToolResult { tool_use_id; content; is_error; _ } ] ->
    Alcotest.(check string) "id preserved" "t1" tool_use_id;
    Alcotest.(check string) "content replaced" "new result" content;
    Alcotest.(check bool) "not error" false is_error
  | _ -> Alcotest.fail "expected single ToolResult"
;;

let test_replace_missing_appends () =
  let msgs =
    [ { role = User
      ; content = [ Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let updated =
    Agent_handoff.replace_tool_result
      msgs
      ~tool_id:"t99"
      ~content:"injected"
      ~is_error:true
  in
  let last = List.nth updated (List.length updated - 1) in
  Alcotest.(check bool) "fallback role" true (last.role = Tool);
  match last.content with
  | [ ToolResult { tool_use_id; content; is_error; _ } ] ->
    Alcotest.(check string) "id" "t99" tool_use_id;
    Alcotest.(check string) "content" "injected" content;
    Alcotest.(check bool) "is error" true is_error
  | _ -> Alcotest.fail "expected appended ToolResult"
;;

let test_replace_preserves_other_results () =
  let msgs =
    [ { role = User
      ; content =
          [ ToolResult
              { tool_use_id = "t1"
              ; content = "keep"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ; ToolResult
              { tool_use_id = "t2"
              ; content = "replace me"
              ; is_error = false
              ; json = None
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let updated =
    Agent_handoff.replace_tool_result
      msgs
      ~tool_id:"t2"
      ~content:"replaced"
      ~is_error:true
  in
  let last = List.nth updated (List.length updated - 1) in
  match last.content with
  | [ ToolResult { tool_use_id = "t1"; content = c1; is_error = e1; _ }
    ; ToolResult { tool_use_id = "t2"; content = c2; is_error = e2; _ }
    ] ->
    Alcotest.(check string) "t1 unchanged" "keep" c1;
    Alcotest.(check bool) "t1 no error" false e1;
    Alcotest.(check string) "t2 replaced" "replaced" c2;
    Alcotest.(check bool) "t2 error" true e2
  | other ->
    Alcotest.fail (Printf.sprintf "unexpected content: %d blocks" (List.length other))
;;

let test_ensure_final_text_extra_tool_withheld_turn () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let thinking_only : Types.api_response =
    { id = "r0"
    ; model = "mock-model"
    ; stop_reason = EndTurn
    ; content = [ Thinking { signature = None; content = "private reasoning" } ]
    ; usage = None
    ; telemetry = None
    }
  in
  let final_answer : Types.api_response =
    { id = "r1"
    ; model = "mock-model"
    ; stop_reason = EndTurn
    ; content = [ Text "the final answer" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let run_with ~ensure_final_text =
    let call_index = ref 0 in
    let tools_seen = ref [] in
    let next (req : Llm_provider.Llm_transport.completion_request) =
      tools_seen := !tools_seen @ [ List.length req.tools ];
      let resp = if !call_index = 0 then thinking_only else final_answer in
      incr call_index;
      resp
    in
    let transport : Llm_provider.Llm_transport.t =
      { complete_sync =
          (fun req ->
            { Llm_provider.Llm_transport.response = Ok (next req); latency_ms = None })
      ; complete_stream = (fun ?on_telemetry:_ ~on_event:_ req -> Ok (next req))
      }
    in
    let options =
      { Agent.default_options with
        transport = Some transport
      ; provider =
          Some
            { Provider.provider = Provider.Local { base_url = "http://mock:0/v1" }
            ; model_id = "mock-model"
            ; api_key_env = ""
            }
      }
    in
    let tool =
      Agent_tool.create_simple ~name:"noop" ~description:"noop" (fun _ -> Ok final_answer)
    in
    let agent =
      Agent.create
        ~net
        ~config:
          { Types.default_config with
            name = "ensure-final-text-test"
          ; max_turns = 4
          ; ensure_final_text
          }
        ~tools:[ tool ]
        ~options
        ()
    in
    Eio.Switch.run (fun sw ->
      Agent.run_blocks ~sw agent [ Text "hi" ], !call_index, !tools_seen)
  in
  let has_text = function
    | Ok resp ->
      List.exists
        (function
          | Text _ -> true
          | _ -> false)
        resp.content
    | Error _ -> false
  in
  let on_result, on_calls, on_tools = run_with ~ensure_final_text:true in
  let off_result, off_calls, _ = run_with ~ensure_final_text:false in
  Alcotest.(check bool)
    "ensure final text produces visible text"
    true
    (has_text on_result);
  Alcotest.(check int) "extra answer turn" 2 on_calls;
  (match on_tools with
   | [ first; 0 ] ->
     Alcotest.(check bool) "withholds tools on extra turn" true (first >= 1)
   | _ -> Alcotest.fail "expected first turn tools and second turn none");
  Alcotest.(check int) "default keeps one turn" 1 off_calls;
  Alcotest.(check bool) "default leaves text-free result" false (has_text off_result)
;;

let () =
  Alcotest.run
    "Agent"
    [ ( "find_handoff"
      , [ Alcotest.test_case "no handoff" `Quick test_find_handoff_none
        ; Alcotest.test_case "normal tool ignored" `Quick test_find_handoff_normal_tool
        ; Alcotest.test_case "handoff present" `Quick test_find_handoff_present
        ; Alcotest.test_case "no prompt field" `Quick test_find_handoff_no_prompt_field
        ; Alcotest.test_case "empty messages" `Quick test_find_handoff_empty
        ; Alcotest.test_case "mixed content" `Quick test_find_handoff_mixed_content
        ] )
    ; ( "replace_tool_result"
      , [ Alcotest.test_case "replace existing" `Quick test_replace_existing
        ; Alcotest.test_case "missing appends" `Quick test_replace_missing_appends
        ; Alcotest.test_case
            "preserves siblings"
            `Quick
            test_replace_preserves_other_results
        ] )
    ; ( "ensure_final_text"
      , [ Alcotest.test_case
            "extra tool-withheld answer turn"
            `Quick
            test_ensure_final_text_extra_tool_withheld_turn
        ] )
    ; ( "exit_condition"
      , [ Alcotest.test_case "error type round-trip" `Quick (fun () ->
            let err = Error.Agent (Error.ExitConditionMet { turn = 7 }) in
            let msg = Error.to_string err in
            Alcotest.(check bool) "contains turn" true (String.length msg > 0 && msg <> ""))
        ; Alcotest.test_case "error_domain poly variant" `Quick (fun () ->
            let err = Error.Agent (Error.ExitConditionMet { turn = 3 }) in
            let poly = Error_domain.of_sdk_error err in
            match poly with
            | `Exit_condition_met 3 -> ()
            | _ -> Alcotest.fail "expected Exit_condition_met 3")
        ; Alcotest.test_case "error_domain back to sdk_error" `Quick (fun () ->
            let poly : Error_domain.sdk_error_poly = `Exit_condition_met 5 in
            let sdk = Error_domain.to_sdk_error poly in
            match sdk with
            | Error.Agent (Error.ExitConditionMet { turn = 5 }) -> ()
            | _ -> Alcotest.fail "expected ExitConditionMet turn=5")
        ; Alcotest.test_case "config default is None" `Quick (fun () ->
            Alcotest.(check bool)
              "exit_condition is None"
              true
              (Types.default_config.exit_condition = None))
        ; Alcotest.test_case "builder with_exit_condition" `Quick (fun () ->
            let pred turn = turn >= 3 in
            Eio_main.run
            @@ fun env ->
            let net = Eio.Stdenv.net env in
            let agent =
              Builder.create ~net ~model:"test"
              |> Builder.with_exit_condition pred
              |> Builder.build_safe
              |> Result.get_ok
            in
            match (Agent.state agent).config.exit_condition with
            | Some f ->
              Alcotest.(check bool) "pred(2)=false" false (f 2);
              Alcotest.(check bool) "pred(3)=true" true (f 3)
            | None -> Alcotest.fail "expected Some exit_condition")
        ] )
    ]
;;
