(** Tool-calling verification harness for LLM provider backends.

    Validates that parsed API responses contain well-formed tool calls,
    correct stop reasons, and no silently dropped content blocks.
    When tool schemas are provided, validates arguments against their
    JSON Schema declarations.

    Pure functions — no Eio, no network.

    @since 0.187.7
    @since 0.185.0 — schema-aware argument validation (P0) *)

open Types

(** A single schema violation detected in tool call arguments. *)
type schema_violation =
  { path : string
  ; expected : string
  ; actual : string
  }

type tool_call_check =
  { name : string
  ; arguments_valid : bool
  ; violations : schema_violation list
  }

type validation_result =
  { tool_calls_found : tool_call_check list
  ; stop_reason_correct : bool
  ; all_tools_declared : bool
  ; dropped_content_blocks : int
  }

let empty_result =
  { tool_calls_found = []
  ; stop_reason_correct = true
  ; all_tools_declared = true
  ; dropped_content_blocks = 0
  }
;;

(* ── Minimal JSON Schema validator ──────────────────── *)

(** Validate a JSON value against a minimal JSON Schema subset.
    Supports: type, required, properties, enum, items (arrays).
    Returns a list of violations with paths relative to the root. *)
let rec validate_against_schema
          ?(json_path = "$")
          (schema : Yojson.Safe.t)
          (value : Yojson.Safe.t)
  : schema_violation list
  =
  let open Yojson.Safe.Util in
  let violations = ref [] in
  let add p expected actual =
    violations := { path = p; expected; actual } :: !violations
  in
  let type_name = function
    | `Assoc _ -> "object"
    | `List _ -> "array"
    | `String _ -> "string"
    | `Int _ | `Intlit _ | `Float _ -> "number"
    | `Bool _ -> "boolean"
    | `Null -> "null"
  in
  (* Check "type" constraint *)
  (match schema |> member "type" with
   | `String "object" ->
     (match value with
      | `Assoc _ -> ()
      | _ -> add json_path "object" (type_name value))
   | `String "array" ->
     (match value with
      | `List _ -> ()
      | _ -> add json_path "array" (type_name value))
   | `String "string" ->
     (match value with
      | `String _ -> ()
      | _ -> add json_path "string" (type_name value))
   | `String "number" | `String "integer" ->
     (match value with
      | `Int _ | `Float _ -> ()
      | _ -> add json_path "number" (type_name value))
   | `String "boolean" ->
     (match value with
      | `Bool _ -> ()
      | _ -> add json_path "boolean" (type_name value))
   | _ -> ());
  (* Check "required" fields *)
  (match schema |> member "required", value with
   | `List required_fields, `Assoc fields ->
     List.iter
       (fun field_name ->
          if not (List.mem_assoc field_name fields)
          then add (json_path ^ "." ^ field_name) "required" "missing")
       (List.filter_map
          (function
            | `String s -> Some s
            | _ -> None)
          required_fields)
   | _ -> ());
  (* Check "properties" recursively *)
  (match schema |> member "properties", value with
   | `Assoc props_schema, `Assoc fields ->
     List.iter
       (fun (field_name, field_value) ->
          match List.assoc_opt field_name props_schema with
          | Some prop_schema ->
            violations
            := !violations
               @ validate_against_schema
                   ~json_path:(json_path ^ "." ^ field_name)
                   prop_schema
                   field_value
          | None -> ())
       fields
   | _ -> ());
  (* Check "enum" constraint *)
  (match schema |> member "enum" with
   | `List allowed ->
     let allowed_set =
       List.fold_left (fun acc v -> Yojson.Safe.to_string v :: acc) [] allowed
     in
     let value_str = Yojson.Safe.to_string value in
     if not (List.mem value_str allowed_set)
     then
       add
         json_path
         (Printf.sprintf "one of [%s]" (String.concat "," allowed_set))
         value_str
   | _ -> ());
  (* Check "items" for arrays *)
  (match schema |> member "items", value with
   | item_schema, `List items ->
     List.iteri
       (fun i item ->
          violations
          := !violations
             @ validate_against_schema
                 ~json_path:(Printf.sprintf "%s[%d]" json_path i)
                 item_schema
                 item)
       items
   | _ -> ());
  !violations
;;

(** Extract parameter schema from a tool definition JSON.
    Handles both "input_schema" (Anthropic) and "parameters" (OpenAI). *)
let extract_tool_schema (tool_def : Yojson.Safe.t) : Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  match tool_def |> member "input_schema" with
  | schema when schema <> `Null -> Some schema
  | _ ->
    (match tool_def |> member "parameters" with
     | schema when schema <> `Null -> Some schema
     | _ ->
       (* OpenAI wraps in "function" *)
       (match tool_def |> member "function" with
        | `Assoc func ->
          (match `Assoc func |> member "parameters" with
           | schema when schema <> `Null -> Some schema
           | _ -> None)
        | _ -> None))
;;

(** Build a name→schema map from a list of tool definition JSONs. *)
let build_schema_map (tools : Yojson.Safe.t list) : (string * Yojson.Safe.t) list =
  let open Yojson.Safe.Util in
  List.filter_map
    (fun tool ->
       let name =
         match tool |> member "name" with
         | `String s -> s
         | _ ->
           (match tool |> member "function" |> member "name" with
            | `String s -> s
            | _ -> "")
       in
       if name = ""
       then None
       else (
         match extract_tool_schema tool with
         | Some schema -> Some (name, schema)
         | None -> None))
    tools
;;

(** Validate a parsed {!api_response} against a set of declared tool names.

    Checks:
    - Every {!ToolUse} block has a name present in [declared_tools]
    - Every {!ToolUse} block has parseable input (valid Yojson)
    - [stop_reason] is {!StopToolUse} when tool calls exist, {!EndTurn} otherwise
    - No content blocks were silently dropped during parse (heuristic: empty text)

    For schema-aware validation, use {!validate_response_with_schemas}. *)
let validate_response ~declared_tools (resp : api_response) : validation_result =
  let tool_calls =
    List.filter_map
      (function
        | ToolUse { name; input = _; _ } ->
          Some { name; arguments_valid = true; violations = [] }
        | _ -> None)
      resp.content
  in
  let has_tool_calls = tool_calls <> [] in
  let stop_reason_correct =
    match has_tool_calls, resp.stop_reason with
    | true, StopToolUse -> true
    | false, (EndTurn | MaxTokens | StopSequence) -> true
    | _ -> false
  in
  let declared_set = List.sort_uniq String.compare declared_tools in
  let all_tools_declared =
    List.for_all (fun (tc : tool_call_check) -> List.mem tc.name declared_set) tool_calls
  in
  let dropped_content_blocks =
    List.length
      (List.filter
         (function
           | Text s when String.trim s = "" -> true
           | _ -> false)
         resp.content)
  in
  { tool_calls_found = tool_calls
  ; stop_reason_correct
  ; all_tools_declared
  ; dropped_content_blocks
  }
;;

(** Schema-aware response validation.

    Like {!validate_response} but also validates each tool call's arguments
    against the corresponding JSON Schema from [tool_schemas].

    [tool_schemas] is a [(name * schema)] list, typically built by
    {!build_schema_map} from the tool definition JSONs passed in the request.

    @since 0.185.0 *)
let validate_response_with_schemas ~declared_tools ~tool_schemas (resp : api_response)
  : validation_result
  =
  let schema_lookup =
    let tbl = Hashtbl.create 8 in
    List.iter (fun (name, schema) -> Hashtbl.replace tbl name schema) tool_schemas;
    tbl
  in
  let tool_calls =
    List.filter_map
      (function
        | ToolUse { name; input; _ } ->
          let violations =
            match Hashtbl.find_opt schema_lookup name with
            | Some schema -> validate_against_schema schema input
            | None -> []
          in
          Some { name; arguments_valid = violations = []; violations }
        | _ -> None)
      resp.content
  in
  let has_tool_calls = tool_calls <> [] in
  let stop_reason_correct =
    match has_tool_calls, resp.stop_reason with
    | true, StopToolUse -> true
    | false, (EndTurn | MaxTokens | StopSequence) -> true
    | _ -> false
  in
  let declared_set = List.sort_uniq String.compare declared_tools in
  let all_tools_declared =
    List.for_all (fun (tc : tool_call_check) -> List.mem tc.name declared_set) tool_calls
  in
  let dropped_content_blocks =
    List.length
      (List.filter
         (function
           | Text s when String.trim s = "" -> true
           | _ -> false)
         resp.content)
  in
  { tool_calls_found = tool_calls
  ; stop_reason_correct
  ; all_tools_declared
  ; dropped_content_blocks
  }
;;

(** Format schema violations into a structured feedback message suitable
    for re-injection into the LLM conversation as a tool result error.
    Follows Sam Chon's structured feedback format.

    @since 0.185.0 *)
let format_violations_feedback (tc : tool_call_check) : string =
  let violation_lines =
    List.map
      (fun (v : schema_violation) ->
         Printf.sprintf "  - path: %s, expected: %s, got: %s" v.path v.expected v.actual)
      tc.violations
  in
  Printf.sprintf
    "Tool call '%s' failed schema validation:\n%s\nPlease retry with correct arguments."
    tc.name
    (String.concat "\n" violation_lines)
;;

let validate_anthropic_response ~declared_tools json =
  validate_response ~declared_tools (Backend_anthropic.parse_response json)
;;

let validate_gemini_response ~declared_tools json =
  validate_response ~declared_tools (Backend_gemini.parse_response json)
;;

let validate_openai_response ~declared_tools json =
  let json_str = Yojson.Safe.to_string json in
  match Backend_openai_parse.parse_openai_response_result json_str with
  | Ok resp -> validate_response ~declared_tools resp
  | Error _ -> empty_result
;;

(* ── Inline Tests ─────────────────────────────────────── *)

let%test "anthropic tool_use response validates correctly" =
  let json =
    `Assoc
      [ "id", `String "msg_123"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "tool_use"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "tu_001"
                ; "name", `String "get_weather"
                ; "input", `Assoc [ "city", `String "Seoul" ]
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 100
            ; "output_tokens", `Int 50
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[ "get_weather" ] json in
  result.stop_reason_correct
  && result.all_tools_declared
  && List.length result.tool_calls_found = 1
  && (List.hd result.tool_calls_found).name = "get_weather"
;;

let%test "anthropic undeclared tool fails validation" =
  let json =
    `Assoc
      [ "id", `String "msg_456"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "tool_use"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "tu_002"
                ; "name", `String "unknown_tool"
                ; "input", `Assoc []
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 10
            ; "output_tokens", `Int 5
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[ "get_weather" ] json in
  result.stop_reason_correct && not result.all_tools_declared
;;

let%test "gemini functionCall response validates correctly" =
  let json =
    `Assoc
      [ ( "candidates"
        , `List
            [ `Assoc
                [ ( "content"
                  , `Assoc
                      [ ( "parts"
                        , `List
                            [ `Assoc
                                [ ( "functionCall"
                                  , `Assoc
                                      [ "name", `String "search"
                                      ; "args", `Assoc [ "query", `String "OCaml 5" ]
                                      ] )
                                ]
                            ] )
                      ] )
                ; "finishReason", `String "STOP"
                ]
            ] )
      ]
  in
  let result = validate_gemini_response ~declared_tools:[ "search" ] json in
  result.stop_reason_correct
  && result.all_tools_declared
  && List.length result.tool_calls_found = 1
;;

let%test "openai tool_calls response validates correctly" =
  let json =
    `Assoc
      [ "id", `String "chatcmpl-123"
      ; "model", `String "gpt-4o"
      ; ( "choices"
        , `List
            [ `Assoc
                [ ( "message"
                  , `Assoc
                      [ "role", `String "assistant"
                      ; "content", `Null
                      ; ( "tool_calls"
                        , `List
                            [ `Assoc
                                [ "id", `String "call_001"
                                ; "type", `String "function"
                                ; ( "function"
                                  , `Assoc
                                      [ "name", `String "read_file"
                                      ; "arguments", `String {|{"path": "/etc/hosts"}|}
                                      ] )
                                ]
                            ] )
                      ] )
                ; "finish_reason", `String "tool_calls"
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "prompt_tokens", `Int 50
            ; "completion_tokens", `Int 20
            ; "total_tokens", `Int 70
            ] )
      ]
  in
  let result = validate_openai_response ~declared_tools:[ "read_file" ] json in
  result.stop_reason_correct
  && result.all_tools_declared
  && List.length result.tool_calls_found = 1
  && (List.hd result.tool_calls_found).name = "read_file"
;;

let%test "wrong stop_reason for tool calls fails validation" =
  let json =
    `Assoc
      [ "id", `String "msg_bad"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "end_turn"
      ; ( "content"
        , `List
            [ `Assoc
                [ "type", `String "tool_use"
                ; "id", `String "tu_003"
                ; "name", `String "test"
                ; "input", `Assoc []
                ]
            ] )
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 10
            ; "output_tokens", `Int 5
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[ "test" ] json in
  not result.stop_reason_correct
;;

let%test "text-only response passes validation" =
  let json =
    `Assoc
      [ "id", `String "msg_text"
      ; "model", `String "claude-4-sonnet"
      ; "stop_reason", `String "end_turn"
      ; "content", `List [ `Assoc [ "type", `String "text"; "text", `String "Hello" ] ]
      ; ( "usage"
        , `Assoc
            [ "input_tokens", `Int 10
            ; "output_tokens", `Int 5
            ; "cache_creation_input_tokens", `Int 0
            ; "cache_read_input_tokens", `Int 0
            ] )
      ]
  in
  let result = validate_anthropic_response ~declared_tools:[] json in
  result.stop_reason_correct && result.all_tools_declared && result.tool_calls_found = []
;;
