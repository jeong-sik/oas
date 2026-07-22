(** Structured output helpers.

    This module keeps exact tool-use helpers ([schema_to_tool_json],
    [extract_tool_input]) for callers using forced tool calls,
    but the direct extraction APIs now prefer provider-native JSON schema
    output via {!Llm_provider.Complete}. Unsupported providers fail fast
    instead of silently falling back to prompt-only JSON mode. *)

open Result_syntax
open Types

type 'a schema =
  { name : string
  ; description : string
  ; params : tool_param list
  ; parse : Yojson.Safe.t -> ('a, string) result
  }

(** Build the tool_schema JSON for an extraction schema *)
let schema_to_tool_json (s : _ schema) : Yojson.Safe.t =
  `Assoc
    [ "name", `String s.name
    ; "description", `String s.description
    ; "input_schema", Types.params_to_input_schema s.params
    ]
;;

let schema_to_json_schema (s : _ schema) : Yojson.Safe.t =
  Types.params_to_input_schema s.params
;;

let text_block = function
  | Text s -> Some s
  | Thinking _
  | ReasoningDetails _
  | RedactedThinking _
  | Image _
  | Document _
  | Audio _
  | ToolUse _
  | ToolResult _ -> None
;;

(** Extract a tool_use input JSON from an API response's content blocks.
    Returns the first ToolUse matching the schema name, or an error. *)
let extract_tool_input ~(schema : _ schema) (content : content_block list) =
  let found =
    List.find_map
      (function
        | ToolUse { name; input; _ } when name = schema.name -> Some input
        | Text _
        | Thinking _
        | ReasoningDetails _
        | RedactedThinking _
        | Image _
        | Document _
        | Audio _
        | ToolUse _
        | ToolResult _ -> None)
      content
  in
  let* json =
    found
    |> Option.to_result
         ~none:
           (Error.Internal
              (Printf.sprintf "No tool_use block for '%s' in response" schema.name))
  in
  schema.parse json
  |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e }))
;;

let parse_json_string text =
  try Ok (Yojson.Safe.from_string text) with
  | Yojson.Json_error detail -> Error detail
;;

let parse_schema_text_json schema json =
  try
    schema.parse json
    |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e }))
  with
  | Yojson.Json_error detail -> Error (Error.Serialization (JsonParseError { detail }))
;;

type response_json_shape =
  | Any_json
  | Object_json

let text_json_of_response (response : api_response) =
  response |> Types.text_of_response |> String.trim
;;

let extract_response_json ?(shape = Any_json) (response : api_response)
  : (Yojson.Safe.t, Error.sdk_error) result
  =
  let text = text_json_of_response response in
  if text = ""
  then
    Error
      (Error.Serialization
         (JsonParseError
            { detail = "structured output response did not contain text JSON" }))
  else
    let* json =
      parse_json_string text
      |> Result.map_error (fun detail -> Error.Serialization (JsonParseError { detail }))
    in
    match shape, json with
    | Any_json, _ -> Ok json
    | Object_json, `Assoc _ -> Ok json
    | Object_json, _ ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = "structured output response JSON was not an object" }))
;;

(** Extract structured output from the response text JSON. *)
let extract_text_json ~(schema : _ schema) (response : api_response)
  : ('a, Error.sdk_error) result
  =
  let* json = extract_response_json response in
  parse_schema_text_json schema json
;;

let sdk_error_of_http_error =
  Http_error_sdk.of_http_error
    ~accept_rejected:(Config_invalid_config { field = "output_schema" })
;;

let provider_config_for_schema ~base_url ?provider ~config ~(schema : _ schema) () =
  let state = { config; messages = []; turn_count = 0; usage = empty_usage } in
  let* provider_cfg = Provider.provider_config_of_agent ~state ~base_url provider in
  Ok
    { provider_cfg with
      Llm_provider.Provider_config.tool_choice = None
    ; response_format = Types.JsonSchema (schema_to_json_schema schema)
    ; output_schema = Some (schema_to_json_schema schema)
    }
;;

(** Extract structured output from a prompt using provider-native JSON
    schema output when available. Unsupported providers fail fast. *)
let extract ~sw ~net ?base_url ?provider ~config ~(schema : 'a schema) prompt
  : ('a, Error.sdk_error) result
  =
  let base_url =
    Option.value ~default:Llm_provider.Api_common.default_base_url base_url
  in
  let messages =
    [ { role = User
      ; content = [ Text prompt ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let* provider_cfg = provider_config_for_schema ~base_url ?provider ~config ~schema () in
  let* response =
    Llm_provider.Complete.complete ~sw ~net ~config:provider_cfg ~messages ~tools:[] ()
    |> Result.map_error sdk_error_of_http_error
  in
  extract_text_json ~schema response
;;

(* ── Extractors ────────────────────────────────────────────────── *)

(** An extractor converts an api_response into a typed value.
    Use with {!run_structured} for Agent.t-level structured output. *)
type 'a extractor = api_response -> ('a, string) result

let schema_extractor (schema : 'a schema) : 'a extractor =
  fun response -> extract_text_json ~schema response |> Result.map_error Error.to_string
;;

let response_json_extractor ?shape () : Yojson.Safe.t extractor =
  fun response ->
  extract_response_json ?shape response |> Result.map_error Error.to_string
;;

(* NOTE: keep [json_extractor] / [text_extractor] for callers who parse
   free-form responses themselves. *)
let try_parse f x =
  try Ok (f x) with
  | Yojson.Json_error e -> Error (Printf.sprintf "JSON parse: %s" e)
  | Yojson.Safe.Util.Type_error (msg, _) -> Error (Printf.sprintf "JSON type: %s" msg)
  | Failure msg -> Error (Printf.sprintf "parse failure: %s" msg)
;;

let json_extractor (parse : Yojson.Safe.t -> 'a) : 'a extractor =
  fun resp ->
  let texts = List.filter_map text_block resp.content in
  let* text =
    match texts with
    | [] -> Error "no text content in response"
    | text :: _ -> Ok text
  in
  let* json =
    parse_json_string text
    |> Result.map_error (fun e -> Printf.sprintf "JSON parse: %s" e)
  in
  try_parse parse json
;;

(** Extract a value from the first text block using a string parser. *)
let text_extractor (parse : string -> 'a option) : 'a extractor =
  fun resp ->
  let texts = List.filter_map text_block resp.content in
  let* text =
    match texts with
    | [] -> Error "no text content in response"
    | text :: _ -> Ok text
  in
  parse text |> Option.to_result ~none:"text extractor returned None"
;;

(** Run an agent with a prompt and extract a structured value from the response.
    Uses the full Agent pipeline (hooks, tools, tracing) unlike {!extract}
    which calls the API directly. *)
let run_structured ~sw ?clock agent prompt ~(extract : 'a extractor) =
  let* response = Agent.run ~sw ?clock agent prompt in
  extract response
  |> Result.map_error (fun detail -> Error.Serialization (JsonParseError { detail }))
;;

(** Extract structured output with SSE streaming.
    Like [extract] but streams via {!Llm_provider.Complete.complete_stream}. *)
let extract_stream
      ~sw
      ~net
      ?base_url
      ?provider
      ?clock
      ~config
      ~(schema : 'a schema)
      ~on_event
      prompt
  : ('a * api_response, Error.sdk_error) result
  =
  let base_url =
    Option.value ~default:Llm_provider.Api_common.default_base_url base_url
  in
  let messages =
    [ { role = User
      ; content = [ Text prompt ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let* provider_cfg = provider_config_for_schema ~base_url ?provider ~config ~schema () in
  let* response =
    Llm_provider.Complete.complete_stream
      ~sw
      ~net
      ?clock
      ~config:provider_cfg
      ~messages
      ~tools:[]
      ~on_event
      ()
    |> Result.map_error sdk_error_of_http_error
  in
  let* value = extract_text_json ~schema response in
  Ok (value, response)
;;

[@@@coverage off]
(* === Inline tests === *)

let test_schema : string schema =
  { name = "test_extract"
  ; description = "A test schema"
  ; params =
      [ { name = "value"
        ; description = "The value"
        ; param_type = String
        ; required = true
        }
      ; { name = "count"
        ; description = "A count"
        ; param_type = Integer
        ; required = false
        }
      ]
  ; parse =
      (fun json ->
        let open Yojson.Safe.Util in
        match json |> member "value" |> to_string_option with
        | Some s -> Ok s
        | None -> Error "missing value field")
  }
;;

let%test "schema_to_tool_json produces valid structure" =
  let json = schema_to_tool_json test_schema in
  let open Yojson.Safe.Util in
  json |> member "name" |> to_string = "test_extract"
  && json |> member "description" |> to_string = "A test schema"
  &&
  let input_schema = json |> member "input_schema" in
  input_schema |> member "type" |> to_string = "object"
;;

let%test "schema_to_tool_json required field lists required params" =
  let json = schema_to_tool_json test_schema in
  let open Yojson.Safe.Util in
  let required = json |> member "input_schema" |> member "required" |> to_list in
  List.length required = 1 && List.exists (fun j -> to_string j = "value") required
;;

let%test "schema_to_tool_json properties have correct types" =
  let json = schema_to_tool_json test_schema in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  let value_type = props |> member "value" |> member "type" |> to_string in
  let count_type = props |> member "count" |> member "type" |> to_string in
  value_type = "string" && count_type = "integer"
;;

let%test "extract_tool_input finds matching tool_use block" =
  let content =
    [ Text "some text"
    ; ToolUse
        { id = "tu1"; name = "test_extract"; input = `Assoc [ "value", `String "hello" ] }
    ]
  in
  match extract_tool_input ~schema:test_schema content with
  | Ok "hello" -> true
  | _ -> false
;;

let%test "extract_tool_input returns error when no matching block" =
  let content = [ Text "only text" ] in
  match extract_tool_input ~schema:test_schema content with
  | Error (Error.Internal _) -> true
  | _ -> false
;;

let%test "extract_tool_input skips non-matching tool names" =
  let content = [ ToolUse { id = "tu1"; name = "other_tool"; input = `Assoc [] } ] in
  match extract_tool_input ~schema:test_schema content with
  | Error (Error.Internal _) -> true
  | _ -> false
;;

let%test "extract_tool_input parse error propagates" =
  let content =
    [ ToolUse
        { id = "tu1"; name = "test_extract"; input = `Assoc [ "wrong", `String "x" ] }
    ]
  in
  match extract_tool_input ~schema:test_schema content with
  | Error (Error.Serialization _) -> true
  | _ -> false
;;

let%test "json_extractor parses json from text block" =
  let extractor =
    json_extractor (fun j -> Yojson.Safe.Util.(j |> member "key" |> to_string))
  in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "{\"key\":\"val\"}" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Ok "val" -> true
  | _ -> false
;;

let%test "json_extractor returns error on empty content" =
  let extractor = json_extractor (fun _ -> "x") in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "json_extractor returns error on invalid json" =
  let extractor = json_extractor (fun _ -> "x") in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "not json" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "text_extractor parses text content" =
  let extractor = text_extractor (fun s -> if s = "yes" then Some true else None) in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "yes" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Ok true -> true
  | _ -> false
;;

let%test "text_extractor returns error when parse returns None" =
  let extractor = text_extractor (fun _ -> None) in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "anything" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error _ -> true
  | Ok _ -> false
;;

(* --- Additional coverage tests --- *)

let%test "schema_to_tool_json empty params" =
  let s : string schema =
    { name = "empty"; description = "d"; params = []; parse = (fun _ -> Ok "x") }
  in
  let json = schema_to_tool_json s in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  let required = json |> member "input_schema" |> member "required" |> to_list in
  props = `Assoc [] && required = []
;;

let%test "schema_to_tool_json boolean param type" =
  let s : bool schema =
    { name = "booltest"
    ; description = "d"
    ; params =
        [ { name = "flag"; description = "f"; param_type = Boolean; required = true } ]
    ; parse = (fun _ -> Ok true)
    }
  in
  let json = schema_to_tool_json s in
  let open Yojson.Safe.Util in
  let flag_type =
    json
    |> member "input_schema"
    |> member "properties"
    |> member "flag"
    |> member "type"
    |> to_string
  in
  flag_type = "boolean"
;;

let%test "extract_tool_input multiple tool_use picks matching name" =
  let content =
    [ ToolUse { id = "t1"; name = "other"; input = `Assoc [ "x", `Int 1 ] }
    ; ToolUse
        { id = "t2"; name = "test_extract"; input = `Assoc [ "value", `String "found" ] }
    ]
  in
  match extract_tool_input ~schema:test_schema content with
  | Ok "found" -> true
  | _ -> false
;;

let%test "json_extractor type error produces descriptive message" =
  let extractor =
    json_extractor (fun j -> Yojson.Safe.Util.(j |> member "key" |> to_int))
  in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "{\"key\":\"not_int\"}" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error msg -> String.length msg > 0
  | Ok _ -> false
;;

let%test "json_extractor Failure propagation" =
  let extractor = json_extractor (fun _ -> failwith "custom fail") in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ Text "{}" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error msg -> String.length msg > 0
  | Ok _ -> false
;;

let%test "text_extractor skips non-text blocks" =
  let extractor = text_extractor (fun s -> Some s) in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ ToolUse { id = "tu"; name = "t"; input = `Null }; Text "target" ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Ok "target" -> true
  | _ -> false
;;

let%test "text_extractor empty content" =
  let extractor = text_extractor (fun s -> Some s) in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error "no text content in response" -> true
  | _ -> false
;;

let%test "json_extractor non-text content only" =
  let extractor = json_extractor (fun _ -> "x") in
  let resp =
    { id = ""
    ; model = ""
    ; stop_reason = EndTurn
    ; content = [ ToolUse { id = "tu"; name = "t"; input = `Null } ]
    ; usage = None
    ; telemetry = None
    }
  in
  match extractor resp with
  | Error "no text content in response" -> true
  | _ -> false
;;
