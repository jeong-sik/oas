(** Unified LLM provider types.

    Single source of truth for message, response, tool, and streaming types.
    Downstream consumers link against this module directly.

    @since 0.42.0 *)

(** {1 Message Types} *)

(** Role in a conversation.
    4-variant superset: System and Tool are required by multi-agent
    coordinators that inject system prompts and relay tool results. *)
type role =
  | System
  | User
  | Assistant
  | Tool
[@@deriving yojson, show]

let role_to_string = function
  | System -> "system"
  | User -> "user"
  | Assistant -> "assistant"
  | Tool -> "tool"
;;

let role_of_string = function
  | "system" -> Some System
  | "user" -> Some User
  | "assistant" -> Some Assistant
  | "tool" -> Some Tool
  | _ -> None
;;

(** {1 Tool Types} *)

(** Tool parameter schema *)
type param_type =
  | String
  | Integer
  | Number
  | Boolean
  | Array
  | Object
[@@deriving yojson, show]

let param_type_to_string = function
  | String -> "string"
  | Integer -> "integer"
  | Number -> "number"
  | Boolean -> "boolean"
  | Array -> "array"
  | Object -> "object"
;;

(** Tool execution result types.
    Defined before content_block/message/api_response to avoid
    field-name shadowing on the [content] record field. *)
type tool_output = { content : string }

type tool_error_class =
  | Transient
  | Deterministic
  | Unknown
[@@deriving yojson, show]

type tool_error =
  { message : string
  ; recoverable : bool
  ; error_class : tool_error_class option
  }

type tool_result = (tool_output, tool_error) result

type tool_param =
  { name : string
  ; description : string
  ; param_type : param_type
  ; required : bool
  }
[@@deriving yojson, show]

let param_type_of_string = function
  | "string" -> Ok String
  | "integer" -> Ok Integer
  | "number" -> Ok Number
  | "boolean" -> Ok Boolean
  | "array" -> Ok Array
  | "object" -> Ok Object
  | other -> Error other
;;

let tool_param_to_json (p : tool_param) : Yojson.Safe.t =
  `Assoc
    [ "name", `String p.name
    ; "description", `String p.description
    ; "param_type", `String (param_type_to_string p.param_type)
    ; "required", `Bool p.required
    ]
;;

let tool_param_of_json (json : Yojson.Safe.t) : (tool_param, string) result =
  let open Yojson.Safe.Util in
  match param_type_of_string (json |> member "param_type" |> to_string) with
  | Error s -> Error (Printf.sprintf "unknown param_type: %s" s)
  | Ok param_type ->
    Ok
      { name = json |> member "name" |> to_string
      ; description = json |> member "description" |> to_string
      ; param_type
      ; required = json |> member "required" |> to_bool
      }
;;

let params_to_input_schema (params : tool_param list) : Yojson.Safe.t =
  let properties =
    List.map
      (fun (p : tool_param) ->
         ( p.name
         , `Assoc
             [ "type", `String (param_type_to_string p.param_type)
             ; "description", `String p.description
             ] ))
      params
  in
  let required =
    List.filter_map
      (fun (p : tool_param) -> if p.required then Some (`String p.name) else None)
      params
  in
  `Assoc
    [ "type", `String "object"
    ; "properties", `Assoc properties
    ; "required", `List required
    ]
;;

(** Tool definition *)
type tool_schema =
  { name : string
  ; description : string
  ; parameters : tool_param list
  }
[@@deriving yojson, show]

let tool_schema_to_json (s : tool_schema) : Yojson.Safe.t =
  `Assoc
    [ "name", `String s.name
    ; "description", `String s.description
    ; "parameters", `List (List.map tool_param_to_json s.parameters)
    ]
;;

let result_all items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok item :: rest -> loop (item :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] items
;;

let tool_schema_of_json (json : Yojson.Safe.t) : (tool_schema, string) result =
  let open Yojson.Safe.Util in
  match
    json |> member "parameters" |> to_list |> List.map tool_param_of_json |> result_all
  with
  | Error e -> Error e
  | Ok parameters ->
    Ok
      { name = json |> member "name" |> to_string
      ; description = json |> member "description" |> to_string
      ; parameters
      }
;;

(** Tool choice mode *)
type tool_choice =
  | Auto
  | Any
  | Tool of string
  | None_ (** Disables tool use. Anthropic: {type:none}, OpenAI: "none" *)
[@@deriving show]

let tool_choice_to_json = function
  | Auto -> `Assoc [ "type", `String "auto" ]
  | Any -> `Assoc [ "type", `String "any" ]
  | Tool name -> `Assoc [ "type", `String "tool"; "name", `String name ]
  | None_ -> `Assoc [ "type", `String "none" ]
;;

type response_format =
  | Off
  | JsonMode
  | JsonSchema of Yojson.Safe.t
[@@deriving show]

let response_format_of_json_mode enabled = if enabled then JsonMode else Off

let response_format_to_json = function
  | Off -> `Assoc [ "type", `String "off" ]
  | JsonMode -> `Assoc [ "type", `String "json_mode" ]
  | JsonSchema schema -> `Assoc [ "type", `String "json_schema"; "schema", schema ]
;;

(** {1 Content Types} *)

(** Content block types -- inline records for clarity *)
type content_block =
  | Text of string
  | Thinking of
      { thinking_type : string
      ; content : string
      }
  | RedactedThinking of string
  | ToolUse of
      { id : string
      ; name : string
      ; input : Yojson.Safe.t
      }
  | ToolResult of
      { tool_use_id : string
      ; content : string
      ; is_error : bool
      ; json : Yojson.Safe.t option
        (** Parsed JSON payload when available. Consumers
                        should prefer [json] over [content] for structured access.
                        [content] remains the canonical string for API serialization. *)
      }
  | Image of
      { media_type : string
      ; data : string
      ; source_type : string
      }
  | Document of
      { media_type : string
      ; data : string
      ; source_type : string
      }
  | Audio of
      { media_type : string
      ; data : string
      ; source_type : string
      }
[@@deriving show]

(** A single message in the conversation.
    [name] identifies the speaker (e.g. tool result source).
    [tool_call_id] links a tool result back to its tool_use request. *)
type message =
  { role : role
  ; content : content_block list
  ; name : string option [@default None]
  ; tool_call_id : string option [@default None]
  ; metadata : (string * Yojson.Safe.t) list [@default []]
  }
[@@deriving show]

(** {1 Response Types} *)

(** Stop reason from API *)
type stop_reason =
  | EndTurn
  | StopToolUse
  | MaxTokens
  | StopSequence
  | Unknown of string
[@@deriving show]

let stop_reason_of_string = function
  | "end_turn" -> EndTurn
  | "tool_use" -> StopToolUse
  | "max_tokens" -> MaxTokens
  | "stop_sequence" -> StopSequence
  | other -> Unknown other
;;

(** API usage from a single response *)
type api_usage =
  { input_tokens : int
  ; output_tokens : int
  ; cache_creation_input_tokens : int
  ; cache_read_input_tokens : int
  ; cost_usd : float option
  }
[@@deriving show, yojson]

(** Provider-reported inference timing from a single API call.
    llama-server populates all fields; cloud providers return [None]. *)
type inference_timings =
  { prompt_n : int option
  ; prompt_ms : float option
  ; prompt_per_second : float option
  ; predicted_n : int option
  ; predicted_ms : float option
  ; predicted_per_second : float option
  ; cache_n : int option
  }
[@@deriving show, yojson]

(** Per-call inference telemetry.
    Parsed from the raw API response; never computed by downstream. *)
type inference_telemetry =
  { system_fingerprint : string option
  ; timings : inference_timings option
  ; reasoning_tokens : int option
  ; reasoning_tokens_estimated : bool
  ; request_latency_ms : int
  ; peak_memory_gb : float option
  ; provider_kind : Provider_kind.t option
  ; reasoning_effort : string option
  ; canonical_model_id : string option
  ; effective_context_window : int option
  ; provider_internal_action_count : int option
  }
[@@deriving show, yojson]

(** API response *)
type api_response =
  { id : string
  ; model : string
  ; stop_reason : stop_reason
  ; content : content_block list
  ; usage : api_usage option
  ; telemetry : inference_telemetry option
  }
[@@deriving show]

(** {1 SSE Streaming Types} *)

type content_delta =
  | TextDelta of string
  | ThinkingDelta of string
  | InputJsonDelta of string

type sse_event =
  | MessageStart of
      { id : string
      ; model : string
      ; usage : api_usage option
      }
  | ContentBlockStart of
      { index : int
      ; content_type : string
      ; tool_id : string option
      ; tool_name : string option
      }
  | ContentBlockDelta of
      { index : int
      ; delta : content_delta
      }
  | ContentBlockStop of { index : int }
  | MessageDelta of
      { stop_reason : stop_reason option
      ; usage : api_usage option
      }
  | MessageStop
  | Ping
  | SSEError of string

(** {1 Convenience Constructors}

    Convenience constructors for consumers that work with flat [string]
    messages and need to convert to [content_block list]. *)

(** Create a message with default [None] for optional fields. *)
let make_message ?name ?tool_call_id ?(metadata = []) ~role content =
  { role; content; name; tool_call_id; metadata }
;;

(** Create a text-only message. *)
let text_message role text = make_message ~role [ Text text ]

(** Create a system message. *)
let system_msg text = text_message System text

(** Create a user message. *)
let user_msg text = text_message User text

(** Create an assistant message. *)
let assistant_msg text = text_message Assistant text

(** Try to parse content as JSON, returning None on failure. *)
let try_parse_json (s : string) : Yojson.Safe.t option =
  if String.length s = 0
  then None
  else (
    match Yojson.Safe.from_string s with
    | json -> Some json
    | exception Yojson.Json_error _ -> None)
;;

(** Create a tool result message.
    When [json] is not provided, attempts to parse [content] as JSON
    so downstream consumers can access structured data without re-parsing. *)
let tool_result_msg ~tool_use_id ~content ?(is_error = false) ?json () =
  let json =
    match json with
    | Some _ -> json
    | None -> try_parse_json content
  in
  make_message
    ~tool_call_id:tool_use_id
    ~role:Tool
    [ ToolResult { tool_use_id; content; is_error; json } ]
;;

(** {1 Tool Result Validation}

    Minimal structural validation for tool result payloads.
    P0 Verification Loop will extend this with full JSON Schema checking. *)

type tool_result_validation_error =
  | Expected_object of string (** Expected JSON object, got other type *)
  | Expected_array of string  (** Expected JSON array, got other type *)
  | Empty_content of string   (** Tool returned empty content *)
  | Json_parse_failed of string (** Content is not valid JSON *)
[@@deriving show]

(** Validate that a ToolResult's payload matches a minimal expected shape.
    Returns [Ok ()] when the result passes, or a descriptive error.
    This is the foundation for P0's full JSON Schema validation loop. *)
let validate_tool_result_shape
      ~expect_object:(expect_obj : bool)
      ~expect_array:(expect_arr : bool)
      (block : content_block)
  : (unit, tool_result_validation_error) result
  =
  match block with
  | ToolResult { content; json; _ } ->
    if String.length (String.trim content) = 0
    then Error (Empty_content "ToolResult content is empty")
    else if expect_obj || expect_arr then
      (match json with
       | None ->
         (* content was not parseable as JSON *)
         Error (Json_parse_failed "ToolResult content is not valid JSON")
       | Some json_value ->
         if expect_obj && not expect_arr then
           (match json_value with
            | `Assoc _ -> Ok ()
            | _ -> Error (Expected_object "ToolResult JSON is not an object"))
         else if expect_arr && not expect_obj then
           (match json_value with
            | `List _ -> Ok ()
            | _ -> Error (Expected_array "ToolResult JSON is not an array"))
         else
           (* Both allowed — any JSON is fine *)
           Ok ())
    else Ok ()
  | _ -> Ok ()
;;

(** Extract text from content blocks, concatenating with newlines.
    Drops Thinking, Image, ToolUse, etc. *)
let text_of_content content =
  content
  |> List.filter_map (function
    | Text s -> Some s
    | ToolResult { content; _ } -> Some content
    | _ -> None)
  |> String.concat "\n"
;;

(** Extract text from a message. *)
let text_of_message (msg : message) = text_of_content msg.content

(** Extract text from an api_response. *)
let text_of_response (resp : api_response) = text_of_content resp.content

(** {1 Usage Helpers} *)

let zero_api_usage =
  { input_tokens = 0
  ; output_tokens = 0
  ; cache_creation_input_tokens = 0
  ; cache_read_input_tokens = 0
  ; cost_usd = None
  }
;;

let usage_of_response (resp : api_response) =
  match resp.usage with
  | Some u -> u
  | None -> zero_api_usage
;;
