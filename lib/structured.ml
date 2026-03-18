(** Structured output via tool_use pattern.

    Anthropic does not have a native json_schema response_format.
    Instead, we use tool_choice=Tool(name) to force the model to call a
    specific tool, then extract the input JSON as structured output.

    This module provides a typed schema + extraction function that
    abstracts this pattern. *)

open Types

type 'a schema = {
  name: string;
  description: string;
  params: tool_param list;
  parse: Yojson.Safe.t -> ('a, string) result;
}

(** Build the tool_schema JSON for an extraction schema *)
let schema_to_tool_json (s : _ schema) : Yojson.Safe.t =
  let properties = List.fold_left (fun acc param ->
    let prop = `Assoc [
      ("type", `String (param_type_to_string param.param_type));
      ("description", `String param.description);
    ] in
    (param.name, prop) :: acc
  ) [] s.params in
  let required = List.filter_map (fun p ->
    if p.required then Some (`String p.name) else None
  ) s.params in
  `Assoc [
    ("name", `String s.name);
    ("description", `String s.description);
    ("input_schema", `Assoc [
      ("type", `String "object");
      ("properties", `Assoc (List.rev properties));
      ("required", `List required);
    ]);
  ]

(** Extract a tool_use input JSON from an API response's content blocks.
    Returns the first ToolUse matching the schema name, or an error. *)
let extract_tool_input ~(schema : _ schema) (content : content_block list) =
  let found = List.find_map (function
    | ToolUse { name; input; _ } when name = schema.name -> Some input
    | _ -> None
  ) content in
  match found with
  | Some json -> schema.parse json |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e }))
  | None -> Error (Error.Internal (Printf.sprintf "No tool_use block for '%s' in response" schema.name))

(** Extract structured output from a prompt using the Anthropic API.
    Forces tool_choice=Tool(schema.name), sends the prompt, and parses
    the resulting tool_use input as the structured value.

    Requires Eio context (sw, net) and an agent_state for config. *)
let extract ~sw ~net ?base_url ?provider ~config ~(schema : 'a schema) prompt
    : ('a, Error.sdk_error) result =
  let config_with_tool = { config with
    tool_choice = Some (Tool schema.name);
  } in
  let state = { config = config_with_tool; messages = []; turn_count = 0; usage = empty_usage } in
  let messages = [{ role = User; content = [Text prompt] }] in
  let tools = [schema_to_tool_json schema] in
  match Api.create_message ~sw ~net ?base_url ?provider ~config:state ~messages ~tools () with
  | Error e -> Error e
  | Ok response ->
    (match extract_tool_input ~schema response.content with
     | Ok v -> Ok v
     | Error e -> Error e)

(* ── Extractors ────────────────────────────────────────────────── *)

(** An extractor converts an api_response into a typed value.
    Use with {!run_structured} for Agent.t-level structured output. *)
type 'a extractor = api_response -> ('a, string) result

(** Extract a JSON value from the first text block and parse it. *)
let json_extractor (parse : Yojson.Safe.t -> 'a) : 'a extractor =
  fun resp ->
    let texts =
      List.filter_map (function Text s -> Some s | _ -> None) resp.content
    in
    match texts with
    | [] -> Error "no text content in response"
    | text :: _ ->
      (try Ok (parse (Yojson.Safe.from_string text))
       with
       | Yojson.Json_error e -> Error (Printf.sprintf "JSON parse: %s" e)
       | exn -> Error (Printexc.to_string exn))

(** Extract a value from the first text block using a string parser. *)
let text_extractor (parse : string -> 'a option) : 'a extractor =
  fun resp ->
    let texts =
      List.filter_map (function Text s -> Some s | _ -> None) resp.content
    in
    match texts with
    | [] -> Error "no text content in response"
    | text :: _ ->
      (match parse text with
       | Some v -> Ok v
       | None -> Error "text extractor returned None")

(** Run an agent with a prompt and extract a structured value from the response.
    Uses the full Agent pipeline (hooks, tools, tracing) unlike {!extract}
    which calls the API directly. *)
let run_structured ~sw ?clock agent prompt ~(extract : 'a extractor) =
  match Agent.run ~sw ?clock agent prompt with
  | Error e -> Error e
  | Ok response ->
    (match extract response with
     | Ok v -> Ok v
     | Error detail ->
       Error (Error.Serialization (JsonParseError { detail })))

(** Extract structured output with validation retry (Instructor pattern).

    On parse/extraction failure, feeds the error message back to the LLM
    as a tool_result with [is_error=true] and retries. This self-healing
    loop gives the model a chance to correct its output.

    [max_retries] defaults to 2 (so up to 3 total attempts).
    [on_validation_error] is called on each retry for observability. *)
(** Result of [extract_with_retry] — includes total usage across all attempts. *)
type 'a retry_result = {
  value: 'a;
  total_usage: api_usage option;
  attempts: int;
}

let extract_with_retry ~sw ~net ?base_url ?provider ?clock
    ~config ~(schema : 'a schema) ?(max_retries=2)
    ?(on_validation_error : (int -> string -> unit) option)
    prompt : ('a retry_result, Error.sdk_error) result =
  let config_with_tool = { config with
    tool_choice = Some (Tool schema.name);
  } in
  let tools = [schema_to_tool_json schema] in
  let add_usage acc resp_usage =
    match acc, resp_usage with
    | None, u -> u
    | Some _, None -> acc
    | Some a, Some u ->
      Some { input_tokens = a.input_tokens + u.input_tokens;
             output_tokens = a.output_tokens + u.output_tokens;
             cache_creation_input_tokens =
               a.cache_creation_input_tokens + u.cache_creation_input_tokens;
             cache_read_input_tokens =
               a.cache_read_input_tokens + u.cache_read_input_tokens }
  in
  let rec attempt n acc_usage messages =
    let state = { config = config_with_tool; messages = []; turn_count = 0;
                  usage = empty_usage } in
    match Api.create_message ~sw ~net ?base_url ?provider ?clock
            ~config:state ~messages ~tools () with
    | Error e -> Error e
    | Ok response ->
        let total = add_usage acc_usage response.usage in
        match extract_tool_input ~schema response.content with
        | Ok v -> Ok { value = v; total_usage = total; attempts = n + 1 }
        | Error e when n < max_retries ->
            let error_msg = Error.to_string e in
            (match on_validation_error with
             | Some cb -> cb (n + 1) error_msg
             | None -> ());
            let tool_use_id = List.find_map (function
              | ToolUse { id; name; _ } when name = schema.name -> Some id
              | _ -> None) response.content
              |> Option.value ~default:"structured_retry" in
            (* Keep only the original user message + latest error feedback.
               Previous failed attempts are dropped to avoid unbounded
               token growth across retries. *)
            let retry_messages = [
              List.hd messages;  (* original user prompt *)
              { role = Assistant; content = response.content };
              { role = User; content = [
                  ToolResult {
                    tool_use_id;
                    content = Printf.sprintf
                      "Validation error (attempt %d/%d): %s. Please fix the output and try again."
                      (n + 1) (max_retries + 1) error_msg;
                    is_error = true;
                  }
                ] };
            ] in
            attempt (n + 1) total retry_messages
        | Error e -> Error e
  in
  let initial_messages = [{ role = User; content = [Text prompt] }] in
  attempt 0 None initial_messages

(** Extract structured output with SSE streaming.
    Like [extract] but uses [Streaming.create_message_stream] to receive
    incremental SSE events.  Calls [on_event] for each event.
    Falls back to sync API + synthetic events for non-Anthropic providers. *)
let extract_stream ~sw ~net ?base_url ?provider ?clock ~config ~(schema : 'a schema)
    ~on_event prompt : ('a * api_response, Error.sdk_error) result =
  let config_with_tool = { config with
    tool_choice = Some (Tool schema.name);
  } in
  let state = { config = config_with_tool; messages = []; turn_count = 0; usage = empty_usage } in
  let messages = [{ role = User; content = [Text prompt] }] in
  let tools = [schema_to_tool_json schema] in
  let api_result =
    let stream_result =
      Streaming.create_message_stream ~sw ~net ?base_url ?provider
        ~config:state ~messages ~tools ~on_event ()
    in
    match stream_result with
    | Ok _ -> stream_result
    | Error (Error.Config (UnsupportedProvider _)) ->
        (* Non-Anthropic: fallback to sync + synthetic events *)
        let sync_result = Api.create_message ~sw ~net ?base_url ?provider
          ?clock ~config:state ~messages ~tools () in
        (match sync_result with
         | Ok response ->
           Streaming.emit_synthetic_events response on_event;
           Ok response
         | Error _ -> sync_result)
    | Error _ -> stream_result
  in
  match api_result with
  | Error e -> Error e
  | Ok response ->
    (match extract_tool_input ~schema response.content with
     | Ok value -> Ok (value, response)
     | Error e -> Error e)
