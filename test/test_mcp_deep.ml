(** Deep coverage tests for lib/protocol/mcp.ml.

    Targets the 116 uncovered lines (54.33% coverage).
    Focuses on pure functions that do not require Eio context:
    - output_token_budget: env var parsing
    - truncate_output: string truncation
    - text_of_tool_result: content block extraction
    - mcp_tool_of_json: JSON tool definition parsing
    - merge_env: environment variable merging *)

open Agent_sdk
module Sdk_types = Mcp_protocol.Mcp_types

(* ── Env helper ──────────────────────────────────────────────── *)

let with_env key value f =
  let original = Sys.getenv_opt key in
  (match value with
   | Some v -> Unix.putenv key v
   | None ->
     (try Unix.putenv key "" with
      | _ -> ()));
  Fun.protect
    ~finally:(fun () ->
      match original with
      | Some v -> Unix.putenv key v
      | None ->
        (try Unix.putenv key "" with
         | _ -> ()))
    f
;;

let make_tool_result ?is_error ?structured_content content =
  let fields = [ "content", Sdk_types.tool_content_list_to_yojson content ] in
  let fields =
    match is_error with
    | Some b -> ("isError", `Bool b) :: fields
    | None -> fields
  in
  let fields =
    match structured_content with
    | Some json -> ("structuredContent", json) :: fields
    | None -> fields
  in
  match Sdk_types.tool_result_of_yojson (`Assoc fields) with
  | Ok result -> result
  | Error detail -> failwith ("tool_result_of_yojson failed: " ^ detail)
;;

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop index =
    if index + sub_len > text_len
    then false
    else if String.sub text index sub_len = sub
    then true
    else loop (index + 1)
  in
  if sub_len = 0 then true else loop 0
;;

(* ── output_token_budget tests ───────────────────────────────── *)

let test_budget_default_no_env () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" None (fun () ->
    let budget = Mcp.output_token_budget () in
    Alcotest.(check int) "default 25000" 25_000 budget)
;;

let test_budget_valid_env () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "100") (fun () ->
    let budget = Mcp.output_token_budget () in
    Alcotest.(check int) "100" 100 budget)
;;

let test_budget_zero () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "0") (fun () ->
    let budget = Mcp.output_token_budget () in
    Alcotest.(check int) "zero -> default" 25_000 budget)
;;

let test_budget_garbage () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "not_a_number") (fun () ->
    let budget = Mcp.output_token_budget () in
    Alcotest.(check int) "garbage -> default" 25_000 budget)
;;

let test_budget_whitespace () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "  50  ") (fun () ->
    let budget = Mcp.output_token_budget () in
    Alcotest.(check int) "trimmed" 50 budget)
;;

(* ── truncate_output tests ───────────────────────────────────── *)

let test_truncate_short () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "1000") (fun () ->
    let result = Mcp.truncate_output "hello" in
    Alcotest.(check string) "no truncation" "hello" result)
;;

let test_truncate_long () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "2") (fun () ->
    (* 2 tokens * 4 chars = 8 char limit *)
    let text = String.make 20 'a' in
    let result = Mcp.truncate_output text in
    Alcotest.(check bool)
      "truncated"
      true
      (contains_substring ~sub:"...[oas mcp output truncated]" result);
    (* First 8 chars should be preserved *)
    Alcotest.(check bool)
      "starts with 8 a's"
      true
      (String.length result > 8 && String.sub result 0 8 = String.make 8 'a'))
;;

let test_truncate_exact_boundary () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "3") (fun () ->
    (* 3 tokens * 4 = 12 chars *)
    let text = String.make 12 'b' in
    let result = Mcp.truncate_output text in
    Alcotest.(check string) "exact boundary no truncation" text result)
;;

let test_truncate_one_over () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "3") (fun () ->
    (* 3 tokens * 4 = 12 chars, input is 13 *)
    let text = String.make 13 'c' in
    let result = Mcp.truncate_output text in
    Alcotest.(check bool)
      "truncated at 13"
      true
      (contains_substring ~sub:"...[oas mcp output truncated]" result))
;;

(* ── text_of_tool_result tests ───────────────────────────────── *)

let test_text_single_text () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "25000") (fun () ->
    let result : Sdk_types.tool_result =
      make_tool_result
        [ Sdk_types.TextContent
            { type_ = "text"; text = "hello world"; annotations = None }
        ]
    in
    Alcotest.(check string) "single text" "hello world" (Mcp.text_of_tool_result result))
;;

let test_text_multiple_text_blocks () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "25000") (fun () ->
    let result : Sdk_types.tool_result =
      make_tool_result
        [ Sdk_types.TextContent { type_ = "text"; text = "line1"; annotations = None }
        ; Sdk_types.TextContent { type_ = "text"; text = "line2"; annotations = None }
        ; Sdk_types.TextContent { type_ = "text"; text = "line3"; annotations = None }
        ]
    in
    Alcotest.(check string)
      "multi text"
      "line1\nline2\nline3"
      (Mcp.text_of_tool_result result))
;;

let test_text_empty_content () =
  let result : Sdk_types.tool_result = make_tool_result [] in
  Alcotest.(check string) "empty" "" (Mcp.text_of_tool_result result)
;;

let test_text_image_only () =
  let result : Sdk_types.tool_result =
    make_tool_result
      [ Sdk_types.ImageContent
          { type_ = "image"
          ; data = "base64"
          ; mime_type = "image/png"
          ; annotations = None
          }
      ]
  in
  Alcotest.(check string) "image only" "" (Mcp.text_of_tool_result result)
;;

let test_text_with_truncation () =
  with_env "OAS_MCP_OUTPUT_MAX_TOKENS" (Some "1") (fun () ->
    let result : Sdk_types.tool_result =
      make_tool_result
        [ Sdk_types.TextContent
            { type_ = "text"; text = String.make 100 'x'; annotations = None }
        ]
    in
    let text = Mcp.text_of_tool_result result in
    Alcotest.(check bool)
      "truncated"
      true
      (contains_substring ~sub:"...[oas mcp output truncated]" text))
;;

(* ── mcp_tool_of_json tests ──────────────────────────────────── *)

let test_tool_json_complete () =
  let json =
    `Assoc
      [ "name", `String "search"
      ; "description", `String "Search things"
      ; ( "inputSchema"
        , `Assoc
            [ "type", `String "object"
            ; "properties", `Assoc [ "query", `Assoc [ "type", `String "string" ] ]
            ] )
      ]
  in
  match Mcp.mcp_tool_of_json json with
  | Some t ->
    Alcotest.(check string) "name" "search" t.name;
    Alcotest.(check string) "desc" "Search things" t.description
  | None -> Alcotest.fail "expected Some"
;;

let test_tool_json_no_name () =
  let json = `Assoc [ "description", `String "orphan" ] in
  Alcotest.(check bool) "None" true (Mcp.mcp_tool_of_json json = None)
;;

let test_tool_json_name_is_int () =
  let json = `Assoc [ "name", `Int 42; "description", `String "bad name type" ] in
  Alcotest.(check bool) "None for int name" true (Mcp.mcp_tool_of_json json = None)
;;

let test_tool_json_not_assoc () =
  Alcotest.(check bool) "None for string" true (Mcp.mcp_tool_of_json (`String "hi") = None);
  Alcotest.(check bool) "None for list" true (Mcp.mcp_tool_of_json (`List []) = None);
  Alcotest.(check bool) "None for int" true (Mcp.mcp_tool_of_json (`Int 5) = None)
;;

let test_tool_json_no_description () =
  let json = `Assoc [ "name", `String "bare_tool"; "inputSchema", `Assoc [] ] in
  match Mcp.mcp_tool_of_json json with
  | Some t -> Alcotest.(check string) "empty desc" "" t.description
  | None -> Alcotest.fail "expected Some"
;;

let test_tool_json_input_schema_underscore () =
  let json =
    `Assoc [ "name", `String "t"; "input_schema", `Assoc [ "type", `String "object" ] ]
  in
  match Mcp.mcp_tool_of_json json with
  | Some t -> Alcotest.(check string) "name" "t" t.name
  | None -> Alcotest.fail "expected Some with input_schema"
;;

let test_tool_json_no_schema () =
  let json = `Assoc [ "name", `String "minimal" ] in
  match Mcp.mcp_tool_of_json json with
  | Some t ->
    Alcotest.(check string) "default schema" "minimal" t.name;
    (* Schema should default to empty Assoc *)
    Alcotest.(check bool) "schema is assoc" true (t.input_schema = `Assoc [])
  | None -> Alcotest.fail "expected Some"
;;

(* ── merge_env tests ─────────────────────────────────────────── *)

let test_merge_env_empty_extras () =
  let env = Mcp.merge_env [] in
  let current = Unix.environment () in
  Alcotest.(check int) "same length" (Array.length current) (Array.length env)
;;

let test_merge_env_adds_new () =
  let key = "OAS_TEST_MERGE_ENV_UNIQUE_KEY_12345" in
  let env = Mcp.merge_env [ key, "test_value" ] in
  let found =
    Array.to_list env |> List.exists (fun entry -> entry = key ^ "=test_value")
  in
  Alcotest.(check bool) "new key present" true found
;;

let test_merge_env_overrides () =
  let key = "PATH" in
  let env = Mcp.merge_env [ key, "/custom/path" ] in
  let matches =
    Array.to_list env
    |> List.filter (fun entry ->
      String.length entry >= String.length key + 1
      && String.sub entry 0 (String.length key + 1) = key ^ "=")
  in
  Alcotest.(check int) "exactly one PATH" 1 (List.length matches);
  Alcotest.(check string) "overridden" (key ^ "=/custom/path") (List.hd matches)
;;

(* ── Runner ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "MCP Deep"
    [ ( "output_token_budget"
      , [ Alcotest.test_case "default no env" `Quick test_budget_default_no_env
        ; Alcotest.test_case "valid env" `Quick test_budget_valid_env
        ; Alcotest.test_case "zero" `Quick test_budget_zero
        ; Alcotest.test_case "garbage" `Quick test_budget_garbage
        ; Alcotest.test_case "whitespace" `Quick test_budget_whitespace
        ] )
    ; ( "truncate_output"
      , [ Alcotest.test_case "short" `Quick test_truncate_short
        ; Alcotest.test_case "long" `Quick test_truncate_long
        ; Alcotest.test_case "exact boundary" `Quick test_truncate_exact_boundary
        ; Alcotest.test_case "one over" `Quick test_truncate_one_over
        ] )
    ; ( "text_of_tool_result"
      , [ Alcotest.test_case "single text" `Quick test_text_single_text
        ; Alcotest.test_case "multiple texts" `Quick test_text_multiple_text_blocks
        ; Alcotest.test_case "empty content" `Quick test_text_empty_content
        ; Alcotest.test_case "image only" `Quick test_text_image_only
        ; Alcotest.test_case "with truncation" `Quick test_text_with_truncation
        ] )
    ; ( "mcp_tool_of_json"
      , [ Alcotest.test_case "complete" `Quick test_tool_json_complete
        ; Alcotest.test_case "no name" `Quick test_tool_json_no_name
        ; Alcotest.test_case "name is int" `Quick test_tool_json_name_is_int
        ; Alcotest.test_case "not assoc" `Quick test_tool_json_not_assoc
        ; Alcotest.test_case "no description" `Quick test_tool_json_no_description
        ; Alcotest.test_case
            "input_schema underscore"
            `Quick
            test_tool_json_input_schema_underscore
        ; Alcotest.test_case "no schema" `Quick test_tool_json_no_schema
        ] )
    ; ( "merge_env"
      , [ Alcotest.test_case "empty extras" `Quick test_merge_env_empty_extras
        ; Alcotest.test_case "adds new" `Quick test_merge_env_adds_new
        ; Alcotest.test_case "overrides" `Quick test_merge_env_overrides
        ] )
    ]
;;
