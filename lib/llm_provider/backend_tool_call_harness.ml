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

type response_parse_error =
  { response_backend : string
  ; response_parse_error : string
  }

(* ── Minimal JSON Schema validator ──────────────────── *)

let json_schema_type_name = function
  | `String s -> Some s
  | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let json_string = function
  | `String s -> Some s
  | `Assoc _ | `List _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

(* The complete JSON Schema primitive [type] vocabulary as a closed set. Parsing
   the [type] string into this variant once (parse, don't validate) makes an
   unknown/non-standard type explicit instead of a permissive default
   (RFC-OAS-029 S8.1). Adding a future JSON Schema type is a single edit here. *)
type json_schema_type =
  | Schema_object
  | Schema_array
  | Schema_string
  | Schema_number
  | Schema_integer
  | Schema_boolean
  | Schema_null

let json_schema_type_of_string = function
  | "object" -> Some Schema_object
  | "array" -> Some Schema_array
  | "string" -> Some Schema_string
  | "number" -> Some Schema_number
  | "integer" -> Some Schema_integer
  | "boolean" -> Some Schema_boolean
  | "null" -> Some Schema_null
  | _ -> None
;;

let json_matches_schema_type expected value =
  let is_object = function
    | `List _
    | `Int _
    | `Intlit _
    | `Float _
    | `String _
    | `Bool _
    | `Null
    | `Tuple _
    | `Variant _ -> false
    | `Assoc _ -> true
  in
  let is_array = function
    | `Assoc _
    | `Int _
    | `Intlit _
    | `Float _
    | `String _
    | `Bool _
    | `Null
    | `Tuple _
    | `Variant _ -> false
    | `List _ -> true
  in
  let is_string = function
    | `Assoc _
    | `List _
    | `Int _
    | `Intlit _
    | `Float _
    | `Bool _
    | `Null
    | `Tuple _
    | `Variant _ -> false
    | `String _ -> true
  in
  let is_number = function
    | `Int _ | `Intlit _ | `Float _ -> true
    | `Assoc _ | `List _ | `String _ | `Bool _ | `Null | `Tuple _ | `Variant _ -> false
  in
  let is_integer = function
    | `Int _ | `Intlit _ -> true
    | `Assoc _ | `List _ | `Float _ | `String _ | `Bool _ | `Null | `Tuple _ | `Variant _
      -> false
  in
  let is_boolean = function
    | `Bool _ -> true
    | `Assoc _
    | `List _
    | `Int _
    | `Intlit _
    | `Float _
    | `String _
    | `Null
    | `Tuple _
    | `Variant _ -> false
  in
  let is_null = function
    | `Null -> true
    | `Assoc _
    | `List _
    | `Int _
    | `Intlit _
    | `Float _
    | `String _
    | `Bool _
    | `Tuple _
    | `Variant _ -> false
  in
  match json_schema_type_of_string expected with
  | None ->
    (* Unknown/non-standard schema [type]: do not silently accept any value.
       A malformed type keyword is surfaced as a violation, not a permissive
       pass (RFC-OAS-029 S8.1; replaces the prior [unsupported_type <> ""]). *)
    false
  | Some Schema_object -> is_object value
  | Some Schema_array -> is_array value
  | Some Schema_string -> is_string value
  | Some Schema_number -> is_number value
  | Some Schema_integer -> is_integer value
  | Some Schema_boolean -> is_boolean value
  | Some Schema_null -> is_null value
;;

let schema_if_present = function
  | `Null -> None
  | schema -> Some schema
;;

let tool_call_check_of_content = function
  | ToolUse { name; input = _; _ } ->
    Some { name; arguments_valid = true; violations = [] }
  | Text _
  | Thinking _
  | ReasoningDetails _
  | RedactedThinking _
  | ToolResult _
  | Image _
  | Document _
  | Audio _ -> None
;;

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
  (match json_schema_type_name (schema |> member "type") with
   | Some expected when not (json_matches_schema_type expected value) ->
     add json_path expected (Json_util.json_type_name value)
   | Some _ | None -> ());
  (* Check "required" fields *)
  (match schema |> member "required", value with
   | `List required_fields, `Assoc fields ->
     List.iter
       (fun field_name ->
          if not (List.mem_assoc field_name fields)
          then add (json_path ^ "." ^ field_name) "required" "missing")
       (List.filter_map json_string required_fields)
   | (`Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null), `Assoc _
   | _, (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) -> ());
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
   | (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null), `Assoc _
   | _, (`List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) -> ());
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
   | `Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> ());
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
   | _, (`Assoc _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null) -> ());
  !violations
;;

let schema_tool_call_check schema_lookup = function
  | ToolUse { name; input; _ } ->
    let violations =
      match Hashtbl.find_opt schema_lookup name with
      | Some schema -> validate_against_schema schema input
      | None -> []
    in
    Some { name; arguments_valid = violations = []; violations }
  | Text _
  | Thinking _
  | ReasoningDetails _
  | RedactedThinking _
  | ToolResult _
  | Image _
  | Document _
  | Audio _ -> None
;;

let stop_reason_matches_tool_calls ~has_tool_calls = function
  | StopToolUse -> has_tool_calls
  | UnmatchedToolCalls -> not has_tool_calls
  | Unknown _ -> false
  | EndTurn
  | MaxTokens
  | StopSequence
  | Refusal
  | ContentFilter
  | RepetitionTruncation
  | PauseTurn
  | Compaction
  | ContextWindowExceeded -> not has_tool_calls
;;

let is_dropped_content_block = function
  | Text s when String.trim s = "" -> true
  | Text _
  | Thinking _
  | ReasoningDetails _
  | RedactedThinking _
  | ToolUse _
  | ToolResult _
  | Image _
  | Document _
  | Audio _ -> false
;;

(** Extract parameter schema from a tool definition JSON.
    Handles both "input_schema" (Anthropic) and "parameters" (Openai). *)
let extract_tool_schema (tool_def : Yojson.Safe.t) : Yojson.Safe.t option =
  let open Yojson.Safe.Util in
  match schema_if_present (tool_def |> member "input_schema") with
  | Some schema -> Some schema
  | None ->
    (match schema_if_present (tool_def |> member "parameters") with
     | Some schema -> Some schema
     | None ->
       (* Openai wraps in "function" *)
       (match tool_def |> member "function" with
        | `Assoc func -> schema_if_present (`Assoc func |> member "parameters")
        | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None))
;;

(** Build a name→schema map from a list of tool definition JSONs. *)
let build_schema_map (tools : Yojson.Safe.t list) : (string * Yojson.Safe.t) list =
  let open Yojson.Safe.Util in
  List.filter_map
    (fun tool ->
       let name =
         match json_string (tool |> member "name") with
         | Some s -> s
         | None ->
           (match tool |> member "function" with
            | `Assoc func ->
              Option.value ~default:"" (json_string (`Assoc func |> member "name"))
            | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> "")
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
    - [stop_reason] is {!StopToolUse} when tool calls exist, a non-tool terminal
      reason otherwise, or the canonical fail-closed unmatched-tool-calls marker
      when the provider claimed tool use without a tool block
    - No content blocks were silently dropped during parse (heuristic: empty text)

    For schema-aware validation, use {!validate_response_with_schemas}. *)
let validate_response ~declared_tools (resp : api_response) : validation_result =
  let tool_calls = List.filter_map tool_call_check_of_content resp.content in
  let has_tool_calls = tool_calls <> [] in
  let stop_reason_correct =
    stop_reason_matches_tool_calls ~has_tool_calls resp.stop_reason
  in
  let declared_set = List.sort_uniq String.compare declared_tools in
  let all_tools_declared =
    List.for_all (fun (tc : tool_call_check) -> List.mem tc.name declared_set) tool_calls
  in
  let dropped_content_blocks =
    List.length (List.filter is_dropped_content_block resp.content)
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
  let tool_calls = List.filter_map (schema_tool_call_check schema_lookup) resp.content in
  let has_tool_calls = tool_calls <> [] in
  let stop_reason_correct =
    stop_reason_matches_tool_calls ~has_tool_calls resp.stop_reason
  in
  let declared_set = List.sort_uniq String.compare declared_tools in
  let all_tools_declared =
    List.for_all (fun (tc : tool_call_check) -> List.mem tc.name declared_set) tool_calls
  in
  let dropped_content_blocks =
    List.length (List.filter is_dropped_content_block resp.content)
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
  let parsed =
    try Backend_openai_parse.parse_openai_response_result json_str with
    | exn -> Error (Backend_openai_parse.Provider_error (Printexc.to_string exn))
  in
  match parsed with
  | Ok resp -> Ok (validate_response ~declared_tools resp)
  | Error parse_error ->
    let response_parse_error =
      match parse_error with
      | Backend_openai_parse.Provider_error msg -> msg
      | Backend_openai_parse.Empty_completion _ ->
        "empty completion (no thinking, text, or tool calls)"
    in
    Error { response_backend = "openai"; response_parse_error }
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
      ; "model", `String "gpt"
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
  match validate_openai_response ~declared_tools:[ "read_file" ] json with
  | Error _ -> false
  | Ok result ->
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
