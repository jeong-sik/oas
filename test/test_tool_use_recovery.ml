(** Tests for Tool_use_recovery — lenient extraction of ToolUse blocks
    from Text content. *)

open Alcotest
open Agent_sdk.Types
module TUR = Agent_sdk.Tool_use_recovery

(* ── Fixtures ────────────────────────────────────────────── *)

let make_response ?(model = "test") ~content () : api_response =
  { id = "test_id"
  ; model
  ; stop_reason = EndTurn
  ; content
  ; usage = None
  ; telemetry = None
  }

(* ── find_json_object ────────────────────────────────────── *)

let test_find_object_simple () =
  match TUR.find_json_object "prefix {\"a\":1} suffix" with
  | Some (start, len) ->
    check int "start" 7 start;
    check int "length" 7 len
  | None -> fail "expected object"

let test_find_object_nested () =
  match TUR.find_json_object "{\"outer\": {\"inner\": 42}}" with
  | Some (start, len) ->
    check int "start at 0" 0 start;
    check int "full object length" 24 len
  | None -> fail "expected object"

let test_find_object_string_with_braces () =
  (* Braces inside string literals must not affect depth *)
  match TUR.find_json_object "{\"key\": \"value with } brace\"}" with
  | Some (start, len) ->
    check int "start" 0 start;
    check int "length matches" 29 len
  | None -> fail "expected object"

let test_find_object_none () =
  check (option (pair int int)) "no object"
    None (TUR.find_json_object "no braces here")

(* ── extract_name_and_input ──────────────────────────────── *)

let test_extract_anthropic_style () =
  let json = `Assoc [
    ("name", `String "keeper_board_post");
    ("input", `Assoc [("title", `String "hi")])
  ] in
  match TUR.extract_name_and_input json with
  | Some (name, input) ->
    check string "name" "keeper_board_post" name;
    check (option string) "title"
      (Some "hi")
      (match input with
       | `Assoc fs -> (match List.assoc_opt "title" fs with
                      | Some (`String s) -> Some s | _ -> None)
       | _ -> None)
  | None -> fail "expected extraction"

let test_extract_openai_arguments_object () =
  let json = `Assoc [
    ("name", `String "tool");
    ("arguments", `Assoc [("x", `Int 1)])
  ] in
  match TUR.extract_name_and_input json with
  | Some (name, _) -> check string "name" "tool" name
  | None -> fail "expected extraction"

let test_extract_double_stringified () =
  (* OpenAI-style: arguments value is a JSON-encoded string *)
  let json = `Assoc [
    ("name", `String "tool");
    ("arguments", `String "{\"x\":42}")
  ] in
  match TUR.extract_name_and_input json with
  | Some (_, input) ->
    (match input with
     | `Assoc [("x", `Int 42)] -> ()
     | _ -> fail "expected decoded {x:42}")
  | None -> fail "expected extraction"

let test_extract_tool_calls_wrapper () =
  let json = `Assoc [
    ("tool_calls", `List [
      `Assoc [
        ("function", `Assoc [
          ("name", `String "foo");
          ("arguments", `Assoc [("k", `String "v")])
        ])
      ]
    ])
  ] in
  match TUR.extract_name_and_input json with
  | Some (name, _) -> check string "name" "foo" name
  | None -> fail "expected extraction"

let test_extract_none () =
  check (option (pair string string)) "no shape"
    None
    (Option.map (fun (n, _) -> (n, "")) (TUR.extract_name_and_input (`String "plain")))

(* ── recover_response ────────────────────────────────────── *)

let test_recover_text_to_tool_use () =
  let text = "```json\n{\"name\": \"my_tool\", \"input\": {\"x\": 1}}\n```" in
  let response = make_response ~content:[Text text] () in
  let recovered = TUR.recover_response ~valid_tool_names:["my_tool"] response in
  match recovered.content with
  | [ToolUse { name; _ }] ->
    check string "promoted to ToolUse" "my_tool" name;
    check bool "stop_reason updated" true
      (recovered.stop_reason = StopToolUse)
  | _ -> fail "expected single ToolUse block"

let test_recover_skips_unknown_tool () =
  let text = "{\"name\": \"unknown_tool\", \"input\": {}}" in
  let response = make_response ~content:[Text text] () in
  let recovered = TUR.recover_response ~valid_tool_names:["my_tool"] response in
  (* Unknown tool name: leave as Text *)
  match recovered.content with
  | [Text _] -> ()
  | _ -> fail "expected Text preserved"

let test_recover_noop_when_tool_use_present () =
  let response = make_response
    ~content:[ToolUse { id = "1"; name = "existing"; input = `Null }] ()
  in
  let recovered = TUR.recover_response
    ~valid_tool_names:["existing"; "other"] response
  in
  check bool "response unchanged"
    true (recovered == response)

let test_recover_noop_when_no_tools () =
  let response = make_response ~content:[Text "{\"name\":\"x\",\"input\":{}}"] () in
  let recovered = TUR.recover_response ~valid_tool_names:[] response in
  check bool "no tools configured: passthrough"
    true (recovered == response)

let test_recover_plain_text_preserved () =
  let response = make_response ~content:[Text "just a plain answer"] () in
  let recovered = TUR.recover_response
    ~valid_tool_names:["any_tool"] response
  in
  match recovered.content with
  | [Text s] -> check string "text preserved" "just a plain answer" s
  | _ -> fail "expected Text preserved"

(* ── Suite ───────────────────────────────────────────────── *)

let () =
  run "Tool_use_recovery" [
    "find_json_object", [
      test_case "simple" `Quick test_find_object_simple;
      test_case "nested" `Quick test_find_object_nested;
      test_case "string braces" `Quick test_find_object_string_with_braces;
      test_case "none" `Quick test_find_object_none;
    ];
    "extract_name_and_input", [
      test_case "anthropic style" `Quick test_extract_anthropic_style;
      test_case "openai arguments object" `Quick test_extract_openai_arguments_object;
      test_case "double stringified" `Quick test_extract_double_stringified;
      test_case "tool_calls wrapper" `Quick test_extract_tool_calls_wrapper;
      test_case "no shape" `Quick test_extract_none;
    ];
    "recover_response", [
      test_case "text to tool use" `Quick test_recover_text_to_tool_use;
      test_case "skip unknown tool" `Quick test_recover_skips_unknown_tool;
      test_case "noop when tool use present" `Quick test_recover_noop_when_tool_use_present;
      test_case "noop when no tools" `Quick test_recover_noop_when_no_tools;
      test_case "plain text preserved" `Quick test_recover_plain_text_preserved;
    ];
  ]
