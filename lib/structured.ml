(** Structured output helpers.

    The direct extraction APIs pick their wire from the resolved model's
    declared capabilities via
    {!Llm_provider.Structured_output_strategy.select}: provider-native JSON
    schema output where it exists, otherwise the schema carried as a single
    tool's [input_schema], otherwise JSON mode with the schema in the prompt.
    The choice is made once, before the request; OAS does not send one wire,
    observe a rejection, and retry with another. A model that declares none of
    the three fails before any bytes are sent.

    The tool path is not a consolation prize. A tool's [input_schema] is a
    JSON Schema the provider constrains the model against, and it is the only
    structured-output wire that exists at all on Z.AI GLM, DeepSeek, Cohere,
    MiniMax and Ollama Cloud. *)

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
  response
  |> Types.text_of_response
  |> Llm_provider.Backend_openai.strip_json_markdown_fences
  |> String.trim
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

let text_message role text =
  { role; content = [ Text text ]; name = None; tool_call_id = None; metadata = [] }
;;

(* Only emitted on the [Model_choice] tool path, where nothing in the request
   compels the call. A provider that honors [tool_choice] needs no prose. *)
let single_tool_instruction (schema : _ schema) =
  Printf.sprintf
    "Call the %s tool exactly once with the answer. Do not reply in prose."
    schema.name
;;

(* The [<schema>...</schema>] delimiter is Nous Research's published JSON-mode
   convention, carried verbatim in the Hermes model cards and their reference
   jsonmode script, which makes it the most widely trained-on framing for this
   request. Naming JSON explicitly also satisfies providers whose JSON mode
   documents a prompt requirement — DeepSeek's json_object mode rejects a
   request that never mentions it. *)
let prompt_schema_instruction (schema : _ schema) =
  Printf.sprintf
    "Respond with a single JSON object and nothing else. It must validate against this \
     JSON Schema:\n\
     <schema>\n\
     %s\n\
     </schema>"
    (Yojson.Safe.pretty_to_string (schema_to_json_schema schema))
;;

(** A structured-output request as the selected strategy shapes it: the
    provider config, the tools the request carries, and the messages. All
    three differ per strategy, so they are decided together rather than
    patched independently at three call sites. *)
type prepared_request =
  { provider_cfg : Llm_provider.Provider_config.t
  ; tools : Yojson.Safe.t list
  ; messages : message list
  ; strategy : Llm_provider.Structured_output_strategy.t
  }

let strategy_for_config provider_cfg =
  (* The override-aware capability view, so a config that declares its endpoint
     has no forced tool_choice (supports_tool_choice_override = Some false) does
     not get routed to a forced-choice tool strategy the request gate would then
     reject. Same view the gate reads (Provider_config.validate_tool_choice_request). *)
  let capabilities =
    Llm_provider.Provider_config.tool_choice_capabilities_for_config provider_cfg
  in
  Llm_provider.Structured_output_strategy.select ~capabilities
;;

(* Shape an already-resolved provider config for one structured-output turn.
   Separated from provider resolution so the one-shot path and the agent's
   terminal turn share exactly one definition of what each strategy puts on
   the wire; [history] is empty for the former and the agent's accumulated
   conversation for the latter. *)
let shape_request
      ~(base_cfg : Llm_provider.Provider_config.t)
      ~(schema : _ schema)
      ~history
      ~prompt
  =
  let* strategy =
    strategy_for_config base_cfg
    |> Result.map_error (fun reason ->
      Error.Config
        (InvalidConfig
           { field = "output_schema"
           ; detail =
               Printf.sprintf
                 "model %s: %s"
                 base_cfg.model_id
                 (Llm_provider.Structured_output_strategy.unsupported_to_string reason)
           }))
  in
  let open Llm_provider.Structured_output_strategy in
  let user = text_message User prompt in
  let turn extra = history @ extra in
  match strategy with
  | Native_json_schema ->
    Ok
      { provider_cfg =
          { base_cfg with
            Llm_provider.Provider_config.tool_choice = None
          ; response_format = Types.JsonSchema (schema_to_json_schema schema)
          ; output_schema = Some (schema_to_json_schema schema)
          }
      ; tools = []
      ; messages = turn [ user ]
      ; strategy
      }
  | Tool_call selection ->
    (* No [output_schema] and no [response_format]: this request never asks
       for native schema output, so the provider gates that reject it (Glm's
       hard reject, the per-model capability check) are not reached. The
       schema travels as the single tool's input_schema instead. *)
    let tool_choice =
      match selection with
      | Forced_named -> Some (Types.Tool schema.name)
      | Forced_any | Model_choice -> tool_choice_of_selection selection
    in
    let messages =
      match selection with
      | Model_choice ->
        turn [ text_message System (single_tool_instruction schema); user ]
      | Forced_named | Forced_any -> turn [ user ]
    in
    Ok
      { provider_cfg =
          { base_cfg with
            Llm_provider.Provider_config.tool_choice
          ; response_format = Types.Off
          ; output_schema = None
          }
      ; tools = [ schema_to_tool_json schema ]
      ; messages
      ; strategy
      }
  | Json_mode_with_prompt_schema ->
    Ok
      { provider_cfg =
          { base_cfg with
            Llm_provider.Provider_config.tool_choice = None
          ; response_format = Types.JsonMode
          ; output_schema = None
          }
      ; tools = []
      ; messages = turn [ text_message System (prompt_schema_instruction schema); user ]
      ; strategy
      }
;;

let prepare_request ~base_url ?provider ~config ~(schema : _ schema) ~prompt () =
  let state = { config; messages = []; turn_count = 0; usage = empty_usage } in
  let* base_cfg = Provider.provider_config_of_agent ~state ~base_url provider in
  shape_request ~base_cfg ~schema ~history:[] ~prompt
;;

(* Where the structured value lives depends on the strategy, because the
   strategies put it in different channels. Reading the tool arguments on a
   tool-call request is the whole point of that strategy; reading response
   text there instead would silently accept a turn in which the constraint
   never applied. *)
type extraction_failure =
  | No_answer_text_but_tool_calls of { stop_reason : Types.stop_reason }
  | No_answer_text of { stop_reason : Types.stop_reason }
  | Tool_not_called of
      { expected : string
      ; stop_reason : Types.stop_reason
      }
  | Malformed_json of string
  | Schema_mismatch of string

let extraction_failure_to_string = function
  | No_answer_text_but_tool_calls { stop_reason } ->
    Printf.sprintf
      "the turn emitted tool calls and no answer text (stop_reason=%s); the structured \
       answer is produced on a later turn"
      (Types.stop_reason_to_string stop_reason)
  | No_answer_text { stop_reason } ->
    Printf.sprintf
      "the response carried no answer text (stop_reason=%s)"
      (Types.stop_reason_to_string stop_reason)
  | Tool_not_called { expected; stop_reason } ->
    Printf.sprintf
      "the response did not call the %s tool that carries the schema (stop_reason=%s); \
       the model answered outside the constrained channel"
      expected
      (Types.stop_reason_to_string stop_reason)
  | Malformed_json detail -> Printf.sprintf "answer text was not valid JSON: %s" detail
  | Schema_mismatch detail ->
    Printf.sprintf "answer JSON did not match the requested shape: %s" detail
;;

let sdk_error_of_extraction_failure failure =
  Error.Serialization (JsonParseError { detail = extraction_failure_to_string failure })
;;

let has_tool_use content =
  List.exists
    (function
      | ToolUse _ -> true
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | Image _
      | Document _
      | Audio _
      | ToolResult _ -> false)
    content
;;

(* Reads the answer-text channel with [visible_text_of_response] rather than
   [text_of_response]: the latter also concatenates ToolResult payloads, so a
   response carrying both a tool result and an answer would reach the JSON
   parser as two documents joined by a newline. *)
let extract_text_json_typed ~(schema : _ schema) (response : api_response) =
  let text =
    response
    |> Types.visible_text_of_response
    |> Llm_provider.Backend_openai.strip_json_markdown_fences
    |> String.trim
  in
  if text = ""
  then
    Error
      (if has_tool_use response.content
       then No_answer_text_but_tool_calls { stop_reason = response.stop_reason }
       else No_answer_text { stop_reason = response.stop_reason })
  else (
    match parse_json_string text with
    | Error detail -> Error (Malformed_json detail)
    | Ok json ->
      (try schema.parse json |> Result.map_error (fun e -> Schema_mismatch e) with
       | Yojson.Json_error detail -> Error (Malformed_json detail)
       | Yojson.Safe.Util.Type_error (detail, _) -> Error (Schema_mismatch detail)))
;;

let extract_tool_input_typed ~(schema : _ schema) (response : api_response) =
  match
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
      response.content
  with
  | None ->
    Error (Tool_not_called { expected = schema.name; stop_reason = response.stop_reason })
  | Some input ->
    (try schema.parse input |> Result.map_error (fun e -> Schema_mismatch e) with
     | Yojson.Json_error detail -> Error (Malformed_json detail)
     | Yojson.Safe.Util.Type_error (detail, _) -> Error (Schema_mismatch detail))
;;

(* Where the structured value lives depends on the strategy, because the
   strategies put it in different channels. Reading the tool arguments on a
   tool-call request is the whole point of that strategy; reading response
   text there instead would silently accept a turn in which the constraint
   never applied. *)
let extract_by_strategy_typed ~strategy ~(schema : 'a schema) (response : api_response) =
  match (strategy : Llm_provider.Structured_output_strategy.t) with
  | Native_json_schema | Json_mode_with_prompt_schema ->
    extract_text_json_typed ~schema response
  | Tool_call _ -> extract_tool_input_typed ~schema response
;;

let extract_by_strategy ~strategy ~schema response =
  extract_by_strategy_typed ~strategy ~schema response
  |> Result.map_error sdk_error_of_extraction_failure
;;

(** Extract structured output from a prompt.

    The wire is chosen from the resolved model's declared capabilities by
    {!Llm_provider.Structured_output_strategy.select}: native schema output
    where the provider has it, otherwise the schema carried as a single tool,
    otherwise JSON mode with the schema in the prompt. A model that declares
    none of the three fails before the request is sent. *)
let extract ~sw ~net ?base_url ?provider ~config ~(schema : 'a schema) prompt
  : ('a, Error.sdk_error) result
  =
  let base_url =
    Option.value ~default:Llm_provider.Api_common.default_base_url base_url
  in
  let* prepared = prepare_request ~base_url ?provider ~config ~schema ~prompt () in
  let* response =
    Llm_provider.Complete.complete
      ~sw
      ~net
      ~config:prepared.provider_cfg
      ~messages:prepared.messages
      ~tools:prepared.tools
      ()
    |> Result.map_error sdk_error_of_http_error
  in
  extract_by_strategy ~strategy:prepared.strategy ~schema response
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

(* ── Agent loop: structured output as a terminal step ────────────

   The constraint cannot simply be carried by every turn of a tool loop the
   way it can on a one-shot request. On a native-schema request that is safe —
   a turn that calls a tool produces no answer text, and the constraint lands
   on the turn after the tool result (measured 2026-07-22 against gpt-5.5 and
   a local glm-4.7-flash; Anthropic documents the same: "Grammar state resets
   between sections"). The tool strategy has no such property: a schema tool
   exposed on every turn is a tool the model can call at any point, ending the
   loop early, and it collides with the agent's own toolset, breaking the
   "exactly one tool" premise that lets [Model_choice] work at all.

   So the loop runs normally to its terminal response, and the structured
   answer is requested in one additional turn that carries the schema and no
   agent tools. This is what Vercel AI SDK 6 converged on when it retired
   generateObject in favour of folding structured output into the tool loop as
   a final step. The extra turn is a real cost and is deliberately explicit
   rather than hidden inside the loop. *)

let terminal_prompt =
  "Using the conversation above, produce the final answer now. Do not call any further \
   tools."
;;

(* The provider config the terminal turn should run on — the same one each
   loop turn runs on, not the raw stored carrier. The agent's stored
   provider_config is a transport carrier (endpoint, credential, headers,
   capabilities) whose model_id / system_prompt / sampling are placeholders;
   the loop merges the live agent_config over it every turn via
   provider_config_with_agent_config (see pipeline_stage_route). Skipping that
   merge here would run the terminal structured turn on the carrier's model
   with no system prompt — a different model and a persona-stripped final
   answer than the loop the caller just ran. *)
let agent_provider_config ~agent =
  let config = (Agent.state agent).config in
  match Agent.provider_config agent with
  | Some cfg -> Ok (Provider.provider_config_with_agent_config ~config cfg)
  | None ->
    let options = Agent.options agent in
    Provider.provider_config_of_agent
      ~state:(Agent.state agent)
      ~base_url:options.base_url
      options.provider
;;

(** Run the agent's normal tool loop, then take one further turn that carries
    the schema on whichever wire the resolved model declares.

    Unlike {!run_structured}, the caller does not have to put a
    [response_format] on the agent config and does not have to hope the
    provider has a native schema field: a model with no native field gets the
    schema as the terminal turn's single tool, and one with neither gets JSON
    mode plus the schema in the prompt.

    The agent's own tools are not carried on the terminal turn. That is the
    point of running it separately — see the comment above.

    @since 0.220.0 *)
let run_structured_schema ~sw ?clock agent prompt ~(schema : 'a schema)
  : ('a, Error.sdk_error) result
  =
  let* _loop_response = Agent.run ~sw ?clock agent prompt in
  let* base_cfg = agent_provider_config ~agent in
  let* prepared =
    shape_request
      ~base_cfg
      ~schema
      ~history:(Agent.base_messages agent)
      ~prompt:terminal_prompt
  in
  let* response =
    Llm_provider.Complete.complete
      ~sw
      ~net:(Agent.net agent)
      ~config:prepared.provider_cfg
      ~messages:prepared.messages
      ~tools:prepared.tools
      ()
    |> Result.map_error sdk_error_of_http_error
  in
  (* The terminal turn is a real turn and its tokens are real cost. The loop
     folds each turn's usage into agent state; this out-of-loop turn must too,
     or [Agent.state agent |> usage] under-reports the run by exactly this
     turn. Mirrors the loop's fold in pipeline stage_collect. *)
  Agent.update_state agent (fun st ->
    { st with
      usage =
        Agent_turn.accumulate_usage
          ~current_usage:st.usage
          ~provider_config:(Some prepared.provider_cfg)
          ~provider:(Agent.options agent).provider
          ~response_model:(Some response.model)
          ~response_usage:response.usage
    });
  extract_by_strategy ~strategy:prepared.strategy ~schema response
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
  let* prepared = prepare_request ~base_url ?provider ~config ~schema ~prompt () in
  let* response =
    Llm_provider.Complete.complete_stream
      ~sw
      ~net
      ?clock
      ~config:prepared.provider_cfg
      ~messages:prepared.messages
      ~tools:prepared.tools
      ~on_event
      ()
    |> Result.map_error sdk_error_of_http_error
  in
  let* value = extract_by_strategy ~strategy:prepared.strategy ~schema response in
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
