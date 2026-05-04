(** Structured output helpers.

    This module keeps the legacy tool-use helpers ([schema_to_tool_json],
    [extract_tool_input]) for callers that still want forced tool calls,
    but the direct extraction APIs now prefer provider-native JSON schema
    output via {!Llm_provider.Complete}. Unsupported providers fail fast
    instead of silently falling back to prompt-only JSON mode. *)

module Retry = Llm_provider.Retry
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

(** Extract a tool_use input JSON from an API response's content blocks.
    Returns the first ToolUse matching the schema name, or an error. *)
let extract_tool_input ~(schema : _ schema) (content : content_block list) =
  let found =
    List.find_map
      (function
        | ToolUse { name; input; _ } when name = schema.name -> Some input
        | _ -> None)
      content
  in
  match found with
  | Some json ->
    schema.parse json
    |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e }))
  | None ->
    Error
      (Error.Internal
         (Printf.sprintf "No tool_use block for '%s' in response" schema.name))
;;

(** Extract structured output from the response text JSON. *)
let extract_text_json ~(schema : _ schema) (response : api_response)
  : ('a, Error.sdk_error) result
  =
  let text =
    response
    |> Types.text_of_response
    |> Llm_provider.Backend_openai.strip_json_markdown_fences
    |> String.trim
  in
  if text = ""
  then
    Error
      (Error.Serialization
         (JsonParseError
            { detail = "structured output response did not contain text JSON" }))
  else (
    try
      let json = Yojson.Safe.from_string text in
      schema.parse json
      |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e }))
    with
    | Yojson.Json_error detail -> Error (Error.Serialization (JsonParseError { detail })))
;;

let sdk_error_of_http_error = function
  | Llm_provider.Http_client.HttpError { code; body } ->
    Error.Api (Llm_provider.Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError { message; kind; _ } ->
    Error.Api (Llm_provider.Retry.NetworkError { message; kind })
  | Llm_provider.Http_client.AcceptRejected { reason } ->
    Error.Config (InvalidConfig { field = "output_schema"; detail = reason })
  | Llm_provider.Http_client.CliTransportRequired { kind } ->
    Error.Config
      (UnsupportedProvider
         { detail =
             Printf.sprintf
               "CLI transport required for %s, but native structured output is only \
                wired for HTTP providers"
               kind
         })
  | Llm_provider.Http_client.ProviderTerminal { kind = Max_turns r; _ } ->
    Error.Agent (MaxTurnsExceeded { turns = r.turns; limit = r.limit })
  | Llm_provider.Http_client.ProviderTerminal { kind = Other reason; message } ->
    Error.Api
      (Llm_provider.Retry.InvalidRequest
         { message = Printf.sprintf "%s: %s" reason message })
  | Llm_provider.Http_client.ProviderFailure { kind; message } ->
    let message = Llm_provider.Http_client.provider_failure_to_string ~kind ~message in
    (match kind with
     | Llm_provider.Http_client.Capacity_exhausted _ ->
       Error.Api (Llm_provider.Retry.Overloaded { message })
     | Llm_provider.Http_client.Hard_quota { retry_after } ->
       Error.Api (Llm_provider.Retry.RateLimited { retry_after; message })
     | Llm_provider.Http_client.Capability_mismatch _
     | Llm_provider.Http_client.Cli_policy_invalid _
     | Llm_provider.Http_client.Cli_startup_failed _
     | Llm_provider.Http_client.Provider_parse_error _
     | Llm_provider.Http_client.Unknown_provider_failure _ ->
       Error.Api (Llm_provider.Retry.InvalidRequest { message }))
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
  let base_url = Option.value ~default:Api.default_base_url base_url in
  let messages =
    [ { role = User
      ; content = [ Text prompt ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let* provider_cfg =
    provider_config_for_schema ~base_url ?provider ~config ~schema ()
  in
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

let schema_json_extractor (schema : 'a schema) : 'a extractor =
  fun response ->
  extract_text_json ~schema response |> Result.map_error Error.to_string
;;

(* NOTE: keep [json_extractor] / [text_extractor] for callers who parse
   free-form responses themselves. *)
let json_extractor (parse : Yojson.Safe.t -> 'a) : 'a extractor =
  fun resp ->
  let texts =
    List.filter_map
      (function
        | Text s -> Some s
        | _ -> None)
      resp.content
  in
  match texts with
  | [] -> Error "no text content in response"
  | text :: _ ->
    (try Ok (parse (Yojson.Safe.from_string text)) with
     | Yojson.Json_error e -> Error (Printf.sprintf "JSON parse: %s" e)
     | Yojson.Safe.Util.Type_error (msg, _) -> Error (Printf.sprintf "JSON type: %s" msg)
     | Failure msg -> Error (Printf.sprintf "parse failure: %s" msg))
;;

(** Extract a value from the first text block using a string parser. *)
let text_extractor (parse : string -> 'a option) : 'a extractor =
  fun resp ->
  let texts =
    List.filter_map
      (function
        | Text s -> Some s
        | _ -> None)
      resp.content
  in
  match texts with
  | [] -> Error "no text content in response"
  | text :: _ ->
    (match parse text with
     | Some v -> Ok v
     | None -> Error "text extractor returned None")
;;

(** Run an agent with a prompt and extract a structured value from the response.
    Uses the full Agent pipeline (hooks, tools, tracing) unlike {!extract}
    which calls the API directly. *)
let run_structured ~sw ?clock agent prompt ~(extract : 'a extractor) =
  let* response = Agent.run ~sw ?clock agent prompt in
  extract response
  |> Result.map_error (fun detail -> Error.Serialization (JsonParseError { detail }))
;;

let validation_feedback_message ~summary ~error_msg =
  Printf.sprintf
    "The previous response did not satisfy the required JSON schema.\n\
     Validation error: %s\n\
     Please return only valid JSON that matches the schema.\n\
     %s"
    error_msg
    summary
;;

(** Extract structured output with validation retry (Instructor pattern).

    On parse/extraction failure, feeds the error message back to the LLM
    as a tool_result with [is_error=true] and retries. This self-healing
    loop gives the model a chance to correct its output.

    [max_retries] defaults to 2 (so up to 3 total attempts).
    [on_validation_error] is called on each retry for observability. *)

(** Result of [extract_with_retry] — includes accumulated usage across all
    attempts. [api_usage] stays reserved for a single provider response. *)
type 'a retry_result =
  { value : 'a
  ; total_usage : usage_stats
  ; attempts : int
  }

let extract_with_retry
      ~sw
      ~net
      ?base_url
      ?provider
      ?clock
      ~config
      ~(schema : 'a schema)
      ?(max_retries = 2)
      ?(on_validation_error : (int -> string -> unit) option)
      prompt
  : ('a retry_result, Error.sdk_error) result
  =
  let base_url = Option.value ~default:Api.default_base_url base_url in
  let add_retry_usage acc resp_usage =
    match resp_usage with
    | None -> acc
    | Some u ->
      { total_input_tokens = acc.total_input_tokens + u.input_tokens
      ; total_output_tokens = acc.total_output_tokens + u.output_tokens
      ; total_cache_creation_input_tokens =
          acc.total_cache_creation_input_tokens + u.cache_creation_input_tokens
      ; total_cache_read_input_tokens =
          acc.total_cache_read_input_tokens + u.cache_read_input_tokens
      ; api_calls = acc.api_calls + 1
      ; estimated_cost_usd =
          acc.estimated_cost_usd +. Option.value ~default:0.0 u.cost_usd
      }
  in
  let initial_message =
    { role = User
    ; content = [ Text prompt ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let retry_policy =
    { Tool_retry_policy.max_retries
    ; retry_on_validation_error = true
    ; retry_on_recoverable_tool_error = false
    ; feedback_style = Tool_retry_policy.Plain_error_text
    }
  in
  let* provider_cfg = provider_config_for_schema ~base_url ?provider ~config ~schema () in
  let rec attempt n acc_usage messages =
    let* response =
      Llm_provider.Complete.complete
        ~sw
        ~net
        ?clock
        ~config:provider_cfg
        ~messages
        ~tools:[]
        ()
      |> Result.map_error sdk_error_of_http_error
    in
    let total = add_retry_usage acc_usage response.usage in
    (match extract_text_json ~schema response with
     | Ok v -> Ok { value = v; total_usage = total; attempts = n + 1 }
     | Error e ->
       let error_msg = Error.to_string e in
       let decision =
         Tool_retry_policy.decide
           ~policy:retry_policy
           ~prior_retries:n
           [ { Tool_retry_policy.tool_name = schema.name
             ; detail = error_msg
             ; kind = Tool_retry_policy.Validation_error
             ; error_class = Tool_retry_policy.Deterministic
             }
           ]
       in
       (match decision with
        | Tool_retry_policy.Retry { retry_count; summary } ->
          (match on_validation_error with
           | Some cb -> cb retry_count error_msg
           | None -> ());
          let retry_messages =
            messages
            @ [ { role = Assistant
                ; content = response.content
                ; name = None
                ; tool_call_id = None
                ; metadata = []
                }
              ; { role = User
                ; content = [ Text (validation_feedback_message ~summary ~error_msg) ]
                ; name = None
                ; tool_call_id = None
                ; metadata = []
                }
              ]
          in
          attempt retry_count total retry_messages
        | Tool_retry_policy.Exhausted _ | Tool_retry_policy.No_retry -> Error e))
  in
  let initial_messages = [ initial_message ] in
  attempt 0 empty_usage initial_messages
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
  let base_url = Option.value ~default:Api.default_base_url base_url in
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
