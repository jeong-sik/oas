(** Agent state checkpoint — versioned JSON serialization.

    Captures the full conversation state (messages, usage, config) as a
    pure value for callers that want to persist and later restore agent
    state.  This module only handles serialization; file I/O and resume
    orchestration are left to the caller. *)

open Types

let checkpoint_version = 2

type t = {
  version: int;
  session_id: string;
  agent_name: string;
  model: model;
  system_prompt: string option;
  messages: message list;
  usage: usage_stats;
  turn_count: int;
  created_at: float;
  tools: tool_schema list;
  tool_choice: tool_choice option;
  mcp_sessions: Mcp_session.info list;
}

(* ── Serialization helpers ──────────────────────────────────────── *)

let usage_to_json u =
  `Assoc [
    ("total_input_tokens", `Int u.total_input_tokens);
    ("total_output_tokens", `Int u.total_output_tokens);
    ("total_cache_creation_input_tokens", `Int u.total_cache_creation_input_tokens);
    ("total_cache_read_input_tokens", `Int u.total_cache_read_input_tokens);
    ("api_calls", `Int u.api_calls);
  ]

let usage_of_json json =
  let open Yojson.Safe.Util in
  {
    total_input_tokens = json |> member "total_input_tokens" |> to_int;
    total_output_tokens = json |> member "total_output_tokens" |> to_int;
    total_cache_creation_input_tokens =
      json |> member "total_cache_creation_input_tokens" |> to_int_option
      |> Option.value ~default:0;
    total_cache_read_input_tokens =
      json |> member "total_cache_read_input_tokens" |> to_int_option
      |> Option.value ~default:0;
    api_calls =
      json |> member "api_calls" |> to_int_option
      |> Option.value ~default:0;
  }

let result_all items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok item :: rest -> loop (item :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] items

let tool_param_to_json (p : tool_param) =
  `Assoc [
    ("name", `String p.name);
    ("description", `String p.description);
    ("param_type", `String (param_type_to_string p.param_type));
    ("required", `Bool p.required);
  ]

let tool_param_of_json json =
  let open Yojson.Safe.Util in
  let type_str = json |> member "param_type" |> to_string in
  let param_type =
    match type_str with
    | "string" -> Ok String
    | "integer" -> Ok Integer
    | "number" -> Ok Number
    | "boolean" -> Ok Boolean
    | "array" -> Ok Array
    | "object" -> Ok Object
    | other -> Error (Printf.sprintf "Unknown param_type: %s" other)
  in
  Result.map
    (fun param_type ->
      {
        name = json |> member "name" |> to_string;
        description = json |> member "description" |> to_string;
        param_type;
        required = json |> member "required" |> to_bool;
      })
    param_type

let tool_schema_to_json (s : tool_schema) =
  `Assoc [
    ("name", `String s.name);
    ("description", `String s.description);
    ("parameters", `List (List.map tool_param_to_json s.parameters));
  ]

let tool_schema_of_json json =
  let open Yojson.Safe.Util in
  let parameters =
    json |> member "parameters" |> to_list |> List.map tool_param_of_json |> result_all
  in
  Result.map
    (fun parameters ->
      {
        name = json |> member "name" |> to_string;
        description = json |> member "description" |> to_string;
        parameters;
      })
    parameters

let content_block_of_json_strict json =
  try
    match Api.content_block_of_json json with
    | Some block -> Ok block
    | None ->
        let open Yojson.Safe.Util in
        let block_type =
          json |> member "type" |> to_string_option |> Option.value ~default:"<missing>"
        in
        Error (Printf.sprintf "Unknown content block type: %s" block_type)
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Printf.sprintf "Invalid content block: %s" msg)
  | exn ->
      Error (Printf.sprintf "Invalid content block: %s" (Printexc.to_string exn))

let message_of_json json =
  let open Yojson.Safe.Util in
  let role_str = json |> member "role" |> to_string in
  let role =
    match role_str with
    | "assistant" -> Ok Assistant
    | "user" -> Ok User
    | other -> Error (Printf.sprintf "Unknown role: %s" other)
  in
  let content =
    json |> member "content" |> to_list
    |> List.map content_block_of_json_strict
    |> result_all
  in
  match role, content with
  | Ok role, Ok content -> Ok { role; content }
  | Error e, _ -> Error e
  | _, Error e -> Error e

(* ── Public API ─────────────────────────────────────────────────── *)

let to_json cp =
  `Assoc [
    ("version", `Int cp.version);
    ("session_id", `String cp.session_id);
    ("agent_name", `String cp.agent_name);
    ("model", model_to_yojson cp.model);
    ("system_prompt",
      (match cp.system_prompt with Some s -> `String s | None -> `Null));
    ("messages", `List (List.map Api.message_to_json cp.messages));
    ("usage", usage_to_json cp.usage);
    ("turn_count", `Int cp.turn_count);
    ("created_at", `Float cp.created_at);
    ("tools", `List (List.map tool_schema_to_json cp.tools));
    ("tool_choice",
      (match cp.tool_choice with
       | Some tc -> tool_choice_to_json tc
       | None -> `Null));
    ("mcp_sessions", Mcp_session.info_list_to_json cp.mcp_sessions);
  ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let version = json |> member "version" |> to_int in
    if version <> checkpoint_version && version <> 1 then
      Error (Printf.sprintf "Unsupported checkpoint version: %d (expected %d or 1)"
        version checkpoint_version)
    else
      let tool_choice =
        match json |> member "tool_choice" with
        | `Null -> Ok None
        | tc -> Result.map Option.some (tool_choice_of_json tc)
      in
      match tool_choice with
      | Error e -> Error e
      | Ok tool_choice ->
        let model = model_of_yojson (json |> member "model") in
        let messages =
          json |> member "messages" |> to_list |> List.map message_of_json |> result_all
        in
        let tools =
          json |> member "tools" |> to_list |> List.map tool_schema_of_json |> result_all
        in
        let mcp_sessions =
          match json |> member "mcp_sessions" with
          | `Null -> Ok []  (* v1 backward compatibility *)
          | `List _ as lst -> Mcp_session.info_list_of_json lst
          | _ -> Error "Checkpoint.of_json: mcp_sessions must be a JSON array or null"
        in
        (match model, messages, tools, mcp_sessions with
         | Ok model, Ok messages, Ok tools, Ok mcp_sessions ->
             Ok {
               version = checkpoint_version;
               session_id = json |> member "session_id" |> to_string;
               agent_name = json |> member "agent_name" |> to_string;
               model;
               system_prompt = json |> member "system_prompt" |> to_string_option;
               messages;
               usage = json |> member "usage" |> usage_of_json;
               turn_count = json |> member "turn_count" |> to_int;
               created_at = json |> member "created_at" |> to_float;
               tools;
               tool_choice;
               mcp_sessions;
             }
         | Error e, _, _, _ -> Error e
         | _, Error e, _, _ -> Error e
         | _, _, Error e, _ -> Error e
         | _, _, _, Error e -> Error e)
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Printf.sprintf "Checkpoint.of_json: %s" msg)
  | exn ->
    Error (Printf.sprintf "Checkpoint.of_json: %s" (Printexc.to_string exn))

let to_string cp =
  to_json cp |> Yojson.Safe.to_string

let of_string s =
  try
    let json = Yojson.Safe.from_string s in
    of_json json
  with Yojson.Json_error msg ->
    Error (Printf.sprintf "Invalid JSON: %s" msg)

let message_count cp = List.length cp.messages

let token_usage cp = cp.usage
