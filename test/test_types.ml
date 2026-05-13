(** Test Types parsing, especially Unknown stop_reason variant. *)

open Agent_sdk

let test_known_stop_reasons () =
  Alcotest.(check string)
    "end_turn"
    "Types.EndTurn"
    (Types.show_stop_reason (Types.stop_reason_of_string "end_turn"));
  Alcotest.(check string)
    "tool_use"
    "Types.StopToolUse"
    (Types.show_stop_reason (Types.stop_reason_of_string "tool_use"));
  Alcotest.(check string)
    "max_tokens"
    "Types.MaxTokens"
    (Types.show_stop_reason (Types.stop_reason_of_string "max_tokens"));
  Alcotest.(check string)
    "stop_sequence"
    "Types.StopSequence"
    (Types.show_stop_reason (Types.stop_reason_of_string "stop_sequence"))
;;

let test_unknown_stop_reason () =
  let sr = Types.stop_reason_of_string "some_new_reason" in
  let shown = Types.show_stop_reason sr in
  Alcotest.(check bool)
    "contains Unknown"
    true
    (String.length shown > 0
     &&
     try
       let _ = String.index shown 'U' in
       true
     with
     | Not_found -> false);
  match sr with
  | Types.Unknown s ->
    Alcotest.(check string) "preserves original string" "some_new_reason" s
  | _ -> Alcotest.fail (Printf.sprintf "expected Unknown variant, got %s" shown)
;;

let test_empty_stop_reason () =
  let sr = Types.stop_reason_of_string "" in
  match sr with
  | Types.Unknown s -> Alcotest.(check string) "empty string preserved" "" s
  | _ ->
    Alcotest.fail
      (Printf.sprintf "expected Unknown for empty, got %s" (Types.show_stop_reason sr))
;;

let test_model_to_string () =
  Alcotest.(check string)
    "opus 4.6"
    "claude-opus-4-6-20250514"
    (Types.model_to_string "claude-opus-4-6");
  Alcotest.(check string)
    "sonnet 4.6"
    "claude-sonnet-4-6-20250514"
    (Types.model_to_string "claude-sonnet-4-6");
  Alcotest.(check string)
    "haiku 4.5"
    "claude-haiku-4-5-20251001"
    (Types.model_to_string "claude-haiku-4-5");
  Alcotest.(check string) "custom" "my-model" (Types.model_to_string "my-model")
;;

let test_role_to_string () =
  Alcotest.(check string) "user" "user" (Types.role_to_string Types.User);
  Alcotest.(check string) "assistant" "assistant" (Types.role_to_string Types.Assistant)
;;

let test_param_type_to_string () =
  Alcotest.(check string) "string" "string" (Types.param_type_to_string Types.String);
  Alcotest.(check string) "integer" "integer" (Types.param_type_to_string Types.Integer);
  Alcotest.(check string) "number" "number" (Types.param_type_to_string Types.Number);
  Alcotest.(check string) "boolean" "boolean" (Types.param_type_to_string Types.Boolean);
  Alcotest.(check string) "array" "array" (Types.param_type_to_string Types.Array);
  Alcotest.(check string) "object" "object" (Types.param_type_to_string Types.Object)
;;

let test_tool_choice_auto () =
  let json = Types.tool_choice_to_json Types.Auto in
  match json with
  | `Assoc [ ("type", `String "auto") ] -> ()
  | _ -> Alcotest.fail "expected auto"
;;

let test_tool_choice_any () =
  let json = Types.tool_choice_to_json Types.Any in
  match json with
  | `Assoc [ ("type", `String "any") ] -> ()
  | _ -> Alcotest.fail "expected any"
;;

let test_tool_choice_tool () =
  let json = Types.tool_choice_to_json (Types.Tool "calculator") in
  match json with
  | `Assoc [ ("type", `String "tool"); ("name", `String "calculator") ] -> ()
  | _ -> Alcotest.fail "expected tool with name"
;;

let test_tool_choice_none () =
  let json = Types.tool_choice_to_json Types.None_ in
  (match json with
   | `Assoc [ ("type", `String "none") ] -> ()
   | _ -> Alcotest.fail "expected none type");
  let rt = Types.tool_choice_of_json json in
  match rt with
  | Ok Types.None_ -> ()
  | Ok _ -> Alcotest.fail "expected None_ variant"
  | Error _ -> Alcotest.fail "expected Ok"
;;

let test_add_usage () =
  let stats = Types.empty_usage in
  let u : Types.api_usage =
    { input_tokens = 10
    ; output_tokens = 20
    ; cache_creation_input_tokens = 5
    ; cache_read_input_tokens = 3
    ; cost_usd = None
    }
  in
  let result = Types.add_usage stats u in
  Alcotest.(check int) "input" 10 result.total_input_tokens;
  Alcotest.(check int) "output" 20 result.total_output_tokens;
  Alcotest.(check int) "cache_creation" 5 result.total_cache_creation_input_tokens;
  Alcotest.(check int) "cache_read" 3 result.total_cache_read_input_tokens;
  Alcotest.(check int) "api_calls" 1 result.api_calls
;;

let test_add_usage_accumulates () =
  let u1 : Types.api_usage =
    { input_tokens = 10
    ; output_tokens = 5
    ; cache_creation_input_tokens = 0
    ; cache_read_input_tokens = 0
    ; cost_usd = None
    }
  in
  let u2 : Types.api_usage =
    { input_tokens = 20
    ; output_tokens = 15
    ; cache_creation_input_tokens = 3
    ; cache_read_input_tokens = 7
    ; cost_usd = None
    }
  in
  let stats = Types.add_usage Types.empty_usage u1 in
  let stats = Types.add_usage stats u2 in
  Alcotest.(check int) "total input" 30 stats.total_input_tokens;
  Alcotest.(check int) "total output" 20 stats.total_output_tokens;
  Alcotest.(check int) "api_calls" 2 stats.api_calls
;;

let test_default_config () =
  let c = Types.default_config in
  Alcotest.(check string) "name" "agent" c.name;
  Alcotest.(check (option int)) "max_tokens" None c.max_tokens;
  Alcotest.(check int) "max_turns" 10 c.max_turns;
  Alcotest.(check bool) "no system prompt" true (c.system_prompt = None);
  Alcotest.(check bool) "no top_p" true (c.top_p = None);
  Alcotest.(check bool) "no top_k" true (c.top_k = None);
  Alcotest.(check bool) "no min_p" true (c.min_p = None);
  Alcotest.(check bool) "no enable_thinking" true (c.enable_thinking = None);
  Alcotest.(check bool) "no thinking_budget" true (c.thinking_budget = None);
  Alcotest.(check bool) "cache off" false c.cache_system_prompt;
  Alcotest.(check bool) "no max_input_tokens" true (c.max_input_tokens = None);
  Alcotest.(check bool) "no max_total_tokens" true (c.max_total_tokens = None)
;;

(* ── yojson roundtrips (Phase 3) ──────────────────────────────── *)

let test_model_yojson_roundtrip () =
  let variants =
    [ "claude-opus-4-6"
    ; "claude-sonnet-4-6"
    ; "claude-opus-4-5"
    ; "claude-sonnet-4"
    ; "claude-haiku-4-5"
    ; "claude-3-7-sonnet"
    ; "my-model"
    ]
  in
  List.iter
    (fun m ->
       let json = Types.model_to_yojson m in
       match Types.model_of_yojson json with
       | Ok decoded ->
         Alcotest.(check string)
           "model roundtrip"
           (Types.show_model m)
           (Types.show_model decoded)
       | Error msg -> Alcotest.fail ("model_of_yojson: " ^ msg))
    variants
;;

let test_role_yojson_roundtrip () =
  List.iter
    (fun r ->
       let json = Types.role_to_yojson r in
       match Types.role_of_yojson json with
       | Ok decoded ->
         Alcotest.(check string)
           "role roundtrip"
           (Types.show_role r)
           (Types.show_role decoded)
       | Error msg -> Alcotest.fail ("role_of_yojson: " ^ msg))
    [ Types.System; Types.User; Types.Assistant; Types.Tool ]
;;

let test_param_type_yojson_roundtrip () =
  let variants =
    [ Types.String
    ; Types.Integer
    ; Types.Number
    ; Types.Boolean
    ; Types.Array
    ; Types.Object
    ]
  in
  List.iter
    (fun p ->
       let json = Types.param_type_to_yojson p in
       match Types.param_type_of_yojson json with
       | Ok decoded ->
         Alcotest.(check string)
           "param_type roundtrip"
           (Types.show_param_type p)
           (Types.show_param_type decoded)
       | Error msg -> Alcotest.fail ("param_type_of_yojson: " ^ msg))
    variants
;;

let test_tool_choice_of_json_error_bogus () =
  let json = `Assoc [ "type", `String "bogus" ] in
  match Types.tool_choice_of_json json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for bogus type"
;;

let test_tool_choice_of_json_error_non_object () =
  let json = `String "not an object" in
  match Types.tool_choice_of_json json with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "expected error for non-object"
;;

let test_show_content_block_variants () =
  let blocks =
    [ Types.Text "hello"
    ; Types.Thinking { thinking_type = "sig"; content = "hmm" }
    ; Types.RedactedThinking "redacted"
    ; Types.ToolUse { id = "tu1"; name = "read"; input = `Null }
    ; Types.ToolResult
        { tool_use_id = "tu1"; content = "ok"; is_error = false; json = None }
    ; Types.Image { media_type = "image/png"; data = "abc"; source_type = "base64" }
    ; Types.Document
        { media_type = "application/pdf"; data = "pdf"; source_type = "base64" }
    ]
  in
  List.iter
    (fun b ->
       let s = Types.show_content_block b in
       Alcotest.(check bool) "show non-empty" true (String.length s > 0))
    blocks
;;

let test_show_message () =
  let m : Types.message =
    { role = Types.User
    ; content = [ Types.Text "test" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let s = Types.show_message m in
  Alcotest.(check bool) "show_message non-empty" true (String.length s > 0)
;;

let test_show_agent_config () =
  let s = Types.show_agent_config Types.default_config in
  Alcotest.(check bool) "show_agent_config non-empty" true (String.length s > 0)
;;

let test_show_agent_state () =
  let state : Types.agent_state =
    { config = Types.default_config
    ; messages = []
    ; turn_count = 0
    ; usage = Types.empty_usage
    }
  in
  let s = Types.show_agent_state state in
  Alcotest.(check bool) "show_agent_state non-empty" true (String.length s > 0)
;;

let test_show_api_response () =
  let r : Types.api_response =
    { id = "msg-1"
    ; model = "test"
    ; stop_reason = Types.EndTurn
    ; content = [ Types.Text "hi" ]
    ; usage = None
    ; telemetry = None
    }
  in
  let s = Types.show_api_response r in
  Alcotest.(check bool) "show_api_response non-empty" true (String.length s > 0)
;;

let test_tool_choice_roundtrip_all () =
  let variants = [ Types.Auto; Types.Any; Types.Tool "calc"; Types.None_ ] in
  List.iter
    (fun tc ->
       let json = Types.tool_choice_to_json tc in
       match Types.tool_choice_of_json json with
       | Ok decoded ->
         Alcotest.(check string)
           "tool_choice roundtrip"
           (Types.show_tool_choice tc)
           (Types.show_tool_choice decoded)
       | Error _ -> Alcotest.fail "tool_choice roundtrip failed")
    variants
;;

(* ── role_of_string ──────────────────────────────────────── *)

let test_role_of_string () =
  Alcotest.(check (option string))
    "system"
    (Some "system")
    (Option.map Types.role_to_string (Types.role_of_string "system"));
  Alcotest.(check (option string))
    "user"
    (Some "user")
    (Option.map Types.role_to_string (Types.role_of_string "user"));
  Alcotest.(check (option string))
    "assistant"
    (Some "assistant")
    (Option.map Types.role_to_string (Types.role_of_string "assistant"));
  Alcotest.(check (option string))
    "tool"
    (Some "tool")
    (Option.map Types.role_to_string (Types.role_of_string "tool"));
  Alcotest.(check bool) "unknown" true (Types.role_of_string "unknown" = None)
;;

(* ── Convenience message constructors ──────────────────── *)

let test_text_message () =
  let m = Types.text_message Types.User "hello" in
  Alcotest.(check string) "role" "user" (Types.role_to_string m.role);
  match m.content with
  | [ Types.Text "hello" ] -> ()
  | _ -> Alcotest.fail "expected single Text block"
;;

let test_system_msg () =
  let m = Types.system_msg "system prompt" in
  Alcotest.(check string) "role" "system" (Types.role_to_string m.role);
  match m.content with
  | [ Types.Text "system prompt" ] -> ()
  | _ -> Alcotest.fail "expected Text"
;;

let test_user_msg () =
  let m = Types.user_msg "question" in
  Alcotest.(check string) "role" "user" (Types.role_to_string m.role);
  match m.content with
  | [ Types.Text "question" ] -> ()
  | _ -> Alcotest.fail "expected Text"
;;

let test_assistant_msg () =
  let m = Types.assistant_msg "answer" in
  Alcotest.(check string) "role" "assistant" (Types.role_to_string m.role);
  match m.content with
  | [ Types.Text "answer" ] -> ()
  | _ -> Alcotest.fail "expected Text"
;;

let test_tool_result_msg () =
  let m = Types.tool_result_msg ~tool_use_id:"tu1" ~content:"result" () in
  Alcotest.(check string) "role" "tool" (Types.role_to_string m.role);
  match m.content with
  | [ Types.ToolResult { tool_use_id = "tu1"; content = "result"; is_error = false; _ } ]
    -> ()
  | _ -> Alcotest.fail "expected ToolResult"
;;

let test_tool_result_msg_error () =
  let m = Types.tool_result_msg ~tool_use_id:"tu2" ~content:"err" ~is_error:true () in
  match m.content with
  | [ Types.ToolResult { is_error = true; _ } ] -> ()
  | _ -> Alcotest.fail "expected error ToolResult"
;;

(* ── text_of_content / text_of_message ─────────────────── *)

let test_text_of_content_text_only () =
  let content = [ Types.Text "hello"; Types.Text "world" ] in
  Alcotest.(check string) "joined" "hello\nworld" (Types.text_of_content content)
;;

let test_text_of_content_mixed () =
  let content =
    [ Types.Text "start"
    ; Types.Thinking { thinking_type = "sig"; content = "hmm" }
    ; Types.ToolUse { id = "tu1"; name = "search"; input = `Null }
    ; Types.Text "end"
    ]
  in
  Alcotest.(check string) "skips non-text" "start\nend" (Types.text_of_content content)
;;

let test_text_of_content_tool_result () =
  let content =
    [ Types.ToolResult
        { tool_use_id = "tu1"; content = "result text"; is_error = false; json = None }
    ]
  in
  Alcotest.(check string)
    "includes tool result"
    "result text"
    (Types.text_of_content content)
;;

let test_text_of_content_empty () =
  Alcotest.(check string) "empty" "" (Types.text_of_content [])
;;

let test_text_of_message () =
  let m : Types.message =
    { role = Types.User
    ; content = [ Types.Text "hi" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  Alcotest.(check string) "text" "hi" (Types.text_of_message m)
;;

(* ── Audio content block show ──────────────────────────── *)

let test_show_audio_block () =
  let block =
    Types.Audio { media_type = "audio/wav"; data = "data"; source_type = "base64" }
  in
  let s = Types.show_content_block block in
  Alcotest.(check bool) "show non-empty" true (String.length s > 0)
;;

(* ── role System / Tool ────────────────────────────────── *)

let test_role_system_tool_strings () =
  Alcotest.(check string) "system" "system" (Types.role_to_string Types.System);
  Alcotest.(check string) "tool" "tool" (Types.role_to_string Types.Tool)
;;

let () =
  Alcotest.run
    "Types"
    [ ( "stop_reason"
      , [ Alcotest.test_case "known stop reasons" `Quick test_known_stop_reasons
        ; Alcotest.test_case "unknown stop reason" `Quick test_unknown_stop_reason
        ; Alcotest.test_case "empty stop reason" `Quick test_empty_stop_reason
        ] )
    ; "model", [ Alcotest.test_case "model_to_string" `Quick test_model_to_string ]
    ; ( "role"
      , [ Alcotest.test_case "role_to_string" `Quick test_role_to_string
        ; Alcotest.test_case "role_of_string" `Quick test_role_of_string
        ; Alcotest.test_case "system/tool strings" `Quick test_role_system_tool_strings
        ] )
    ; ( "param_type"
      , [ Alcotest.test_case "param_type_to_string" `Quick test_param_type_to_string ] )
    ; ( "tool_choice"
      , [ Alcotest.test_case "auto" `Quick test_tool_choice_auto
        ; Alcotest.test_case "any" `Quick test_tool_choice_any
        ; Alcotest.test_case "tool" `Quick test_tool_choice_tool
        ; Alcotest.test_case "none roundtrip" `Quick test_tool_choice_none
        ] )
    ; ( "usage"
      , [ Alcotest.test_case "add_usage" `Quick test_add_usage
        ; Alcotest.test_case "accumulates" `Quick test_add_usage_accumulates
        ] )
    ; "config", [ Alcotest.test_case "default_config" `Quick test_default_config ]
    ; ( "yojson_roundtrip"
      , [ Alcotest.test_case "model" `Quick test_model_yojson_roundtrip
        ; Alcotest.test_case "role" `Quick test_role_yojson_roundtrip
        ; Alcotest.test_case "param_type" `Quick test_param_type_yojson_roundtrip
        ; Alcotest.test_case "tool_choice all" `Quick test_tool_choice_roundtrip_all
        ] )
    ; ( "tool_choice_errors"
      , [ Alcotest.test_case "bogus type" `Quick test_tool_choice_of_json_error_bogus
        ; Alcotest.test_case "non-object" `Quick test_tool_choice_of_json_error_non_object
        ] )
    ; ( "show_functions"
      , [ Alcotest.test_case
            "content_block variants"
            `Quick
            test_show_content_block_variants
        ; Alcotest.test_case "message" `Quick test_show_message
        ; Alcotest.test_case "agent_config" `Quick test_show_agent_config
        ; Alcotest.test_case "agent_state" `Quick test_show_agent_state
        ; Alcotest.test_case "api_response" `Quick test_show_api_response
        ; Alcotest.test_case "audio block" `Quick test_show_audio_block
        ] )
    ; ( "message_constructors"
      , [ Alcotest.test_case "text_message" `Quick test_text_message
        ; Alcotest.test_case "system_msg" `Quick test_system_msg
        ; Alcotest.test_case "user_msg" `Quick test_user_msg
        ; Alcotest.test_case "assistant_msg" `Quick test_assistant_msg
        ; Alcotest.test_case "tool_result_msg" `Quick test_tool_result_msg
        ; Alcotest.test_case "tool_result_msg error" `Quick test_tool_result_msg_error
        ] )
    ; ( "tool_schema_yojson"
      , [ Alcotest.test_case "roundtrip" `Quick (fun () ->
            let schema : Types.tool_schema =
              { name = "calc"
              ; description = "Calculate"
              ; parameters =
                  [ { name = "expr"
                    ; description = "Expression"
                    ; param_type = Types.String
                    ; required = true
                    }
                  ; { name = "precision"
                    ; description = "Decimal places"
                    ; param_type = Types.Integer
                    ; required = false
                    }
                  ]
              }
            in
            let json = Types.tool_schema_to_yojson schema in
            match Types.tool_schema_of_yojson json with
            | Ok decoded ->
              Alcotest.(check string) "name" "calc" decoded.name;
              Alcotest.(check int) "params" 2 (List.length decoded.parameters)
            | Error msg -> Alcotest.fail ("tool_schema_of_yojson: " ^ msg))
        ; Alcotest.test_case "tool_param roundtrip" `Quick (fun () ->
            let param : Types.tool_param =
              { name = "x"
              ; description = "Value"
              ; param_type = Types.Number
              ; required = true
              }
            in
            let json = Types.tool_param_to_yojson param in
            match Types.tool_param_of_yojson json with
            | Ok decoded ->
              Alcotest.(check string) "name" "x" decoded.name;
              Alcotest.(check bool) "required" true decoded.required
            | Error msg -> Alcotest.fail ("tool_param_of_yojson: " ^ msg))
        ] )
    ; ( "text_extraction"
      , [ Alcotest.test_case "text only" `Quick test_text_of_content_text_only
        ; Alcotest.test_case "mixed" `Quick test_text_of_content_mixed
        ; Alcotest.test_case "tool result" `Quick test_text_of_content_tool_result
        ; Alcotest.test_case "empty" `Quick test_text_of_content_empty
        ; Alcotest.test_case "text_of_message" `Quick test_text_of_message
        ] )
    ]
;;
