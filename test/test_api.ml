(** Tests for api.ml -- content block JSON, build_body_assoc, parse_response *)

open Alcotest
open Agent_sdk

(* Helper: compare content_block via show string *)
let check_block msg expected actual =
  check string msg
    (Types.show_content_block expected)
    (Types.show_content_block actual)

(* ------------------------------------------------------------------ *)
(* content_block_to_json / content_block_of_json round-trips            *)
(* ------------------------------------------------------------------ *)

let test_text_round_trip () =
  let block = Types.Text "hello world" in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "text" block parsed
  | None -> fail "returned None"

let test_thinking_round_trip () =
  let block = Types.Thinking ("sig123", "I think therefore I am") in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "thinking" block parsed
  | None -> fail "returned None"

let test_redacted_thinking_round_trip () =
  let block = Types.RedactedThinking "redacted_data_blob" in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "redacted_thinking" block parsed
  | None -> fail "returned None"

let test_tool_use_round_trip () =
  let block = Types.ToolUse ("tu_001", "calculator", `Assoc [("expr", `String "2+2")]) in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_use" block parsed
  | None -> fail "returned None"

let test_tool_result_round_trip () =
  let block = Types.ToolResult ("tu_001", "4", false) in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_result" block parsed
  | None -> fail "returned None"

let test_tool_result_error_round_trip () =
  let block = Types.ToolResult ("tu_002", "failed", true) in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "tool_result_error" block parsed
  | None -> fail "returned None"

let test_image_round_trip () =
  let block = Types.Image { media_type = "image/png"; data = "abc"; source_type = "base64" } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "image" block parsed
  | None -> fail "returned None"

let test_document_round_trip () =
  let block = Types.Document { media_type = "application/pdf"; data = "pdf"; source_type = "base64" } in
  let json = Api.content_block_to_json block in
  match Api.content_block_of_json json with
  | Some parsed -> check_block "document" block parsed
  | None -> fail "returned None"

let test_unknown_type_returns_none () =
  let json = `Assoc [("type", `String "future_block"); ("data", `String "x")] in
  match Api.content_block_of_json json with
  | None -> ()
  | Some _ -> fail "expected None for unknown type"

(* ------------------------------------------------------------------ *)
(* build_body_assoc                                                     *)
(* ------------------------------------------------------------------ *)

let make_state ?thinking_budget ?tool_choice () =
  let config = { Types.default_config with
    system_prompt = Some "You are helpful.";
    thinking_budget;
    tool_choice;
  } in
  { Types.config; messages = []; turn_count = 0; usage = Types.empty_usage }

let test_build_body_basic () =
  let config = make_state () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check string "model present" "claude-sonnet-4-6-20250514"
    (json |> member "model" |> to_string);
  check bool "stream false" false
    (json |> member "stream" |> to_bool);
  check string "system prompt" "You are helpful."
    (json |> member "system" |> to_string)

let test_build_body_with_thinking_budget () =
  let config = make_state ~thinking_budget:1024 () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let thinking = json |> member "thinking" in
  check string "thinking type" "enabled"
    (thinking |> member "type" |> to_string);
  check int "budget_tokens" 1024
    (thinking |> member "budget_tokens" |> to_int)

let test_build_body_without_thinking () =
  let config = make_state () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let has_thinking = List.exists (fun (k, _) -> k = "thinking") assoc in
  check bool "no thinking key" false has_thinking

let test_build_body_with_tool_choice () =
  let config = make_state ~tool_choice:(Types.Tool "calculator") () in
  let assoc = Api.build_body_assoc ~config ~messages:[] ~stream:false () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  let tc = json |> member "tool_choice" in
  check string "tool_choice type" "tool"
    (tc |> member "type" |> to_string);
  check string "tool_choice name" "calculator"
    (tc |> member "name" |> to_string)

let test_build_body_with_tools () =
  let tool_json = `Assoc [("name", `String "calc"); ("description", `String "calc")] in
  let assoc = Api.build_body_assoc ~config:(make_state ()) ~messages:[]
    ~tools:[tool_json] ~stream:true () in
  let json = `Assoc assoc in
  let open Yojson.Safe.Util in
  check bool "stream true" true (json |> member "stream" |> to_bool);
  let tools = json |> member "tools" |> to_list in
  check int "1 tool" 1 (List.length tools)

(* ------------------------------------------------------------------ *)
(* parse_response                                                       *)
(* ------------------------------------------------------------------ *)

let test_parse_response_complete () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_test",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "end_turn",
    "content": [
      {"type": "text", "text": "Hello there."},
      {"type": "thinking", "signature": "sig", "thinking": "Let me think..."}
    ],
    "usage": {"input_tokens": 100, "output_tokens": 50}
  }|} in
  let resp = Api.parse_response json in
  check string "id" "msg_test" resp.id;
  check string "model" "claude-sonnet-4-6-20250514" resp.model;
  check int "content count" 2 (List.length resp.content);
  (match resp.stop_reason with
   | Types.EndTurn -> ()
   | sr -> fail (Printf.sprintf "expected EndTurn, got %s" (Types.show_stop_reason sr)));
  (match resp.usage with
   | Some u when u.Types.input_tokens = 100 && u.output_tokens = 50 -> ()
   | _ -> fail "expected usage input=100 output=50")

let test_parse_response_tool_use () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_tu",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "tool_use",
    "content": [
      {"type": "tool_use", "id": "tu_1", "name": "calc", "input": {"x": 1}}
    ],
    "usage": null
  }|} in
  let resp = Api.parse_response json in
  (match resp.stop_reason with
   | Types.StopToolUse -> ()
   | sr -> fail (Printf.sprintf "expected StopToolUse, got %s" (Types.show_stop_reason sr)));
  check bool "usage is None" true (resp.usage = None);
  (match resp.content with
   | [Types.ToolUse ("tu_1", "calc", _)] -> ()
   | _ -> fail "expected single ToolUse")

let test_parse_response_unknown_stop () =
  let json = Yojson.Safe.from_string {|{
    "id": "msg_unk",
    "model": "claude-sonnet-4-6-20250514",
    "stop_reason": "new_future_reason",
    "content": [],
    "usage": null
  }|} in
  let resp = Api.parse_response json in
  (match resp.stop_reason with
   | Types.Unknown "new_future_reason" -> ()
   | sr -> fail (Printf.sprintf "expected Unknown, got %s" (Types.show_stop_reason sr)))

(* ------------------------------------------------------------------ *)
(* message_to_json                                                      *)
(* ------------------------------------------------------------------ *)

let test_message_to_json () =
  let msg = { Types.role = Types.User; content = [Types.Text "hi"] } in
  let json = Api.message_to_json msg in
  let open Yojson.Safe.Util in
  check string "role" "user" (json |> member "role" |> to_string);
  let content = json |> member "content" |> to_list in
  check int "1 block" 1 (List.length content)

(* ------------------------------------------------------------------ *)
(* Test runner                                                          *)
(* ------------------------------------------------------------------ *)

let () =
  run "Api" [
    "content_block_round_trip", [
      test_case "text" `Quick test_text_round_trip;
      test_case "thinking" `Quick test_thinking_round_trip;
      test_case "redacted_thinking" `Quick test_redacted_thinking_round_trip;
      test_case "tool_use" `Quick test_tool_use_round_trip;
      test_case "tool_result" `Quick test_tool_result_round_trip;
      test_case "tool_result_error" `Quick test_tool_result_error_round_trip;
      test_case "image" `Quick test_image_round_trip;
      test_case "document" `Quick test_document_round_trip;
      test_case "unknown type" `Quick test_unknown_type_returns_none;
    ];
    "build_body_assoc", [
      test_case "basic" `Quick test_build_body_basic;
      test_case "with thinking_budget" `Quick test_build_body_with_thinking_budget;
      test_case "without thinking" `Quick test_build_body_without_thinking;
      test_case "with tool_choice" `Quick test_build_body_with_tool_choice;
      test_case "with tools" `Quick test_build_body_with_tools;
    ];
    "parse_response", [
      test_case "complete response" `Quick test_parse_response_complete;
      test_case "tool_use response" `Quick test_parse_response_tool_use;
      test_case "unknown stop_reason" `Quick test_parse_response_unknown_stop;
    ];
    "message_to_json", [
      test_case "user message" `Quick test_message_to_json;
    ];
  ]
