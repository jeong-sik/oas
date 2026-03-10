(** Agent state checkpoint — versioned JSON serialization.

    Captures the full conversation state (messages, usage, config) so an
    agent can be suspended and later resumed.  All functions are pure
    (string / Yojson.Safe.t in, string / Yojson.Safe.t out); file I/O is
    left to the caller. *)

open Types

let checkpoint_version = 1

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
  let param_type = match type_str with
    | "string" -> String
    | "integer" -> Integer
    | "number" -> Number
    | "boolean" -> Boolean
    | "array" -> Array
    | "object" -> Object
    | _ -> String
  in
  {
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    param_type;
    required = json |> member "required" |> to_bool;
  }

let tool_schema_to_json (s : tool_schema) =
  `Assoc [
    ("name", `String s.name);
    ("description", `String s.description);
    ("parameters", `List (List.map tool_param_to_json s.parameters));
  ]

let tool_schema_of_json json =
  let open Yojson.Safe.Util in
  {
    name = json |> member "name" |> to_string;
    description = json |> member "description" |> to_string;
    parameters = json |> member "parameters" |> to_list |> List.map tool_param_of_json;
  }

let message_of_json json =
  let open Yojson.Safe.Util in
  let role_str = json |> member "role" |> to_string in
  let role = match role_str with
    | "assistant" -> Assistant
    | _ -> User
  in
  let content =
    json |> member "content" |> to_list
    |> List.filter_map Api.content_block_of_json
  in
  { role; content }

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
  ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let version = json |> member "version" |> to_int in
    if version <> checkpoint_version then
      Error (Printf.sprintf "Unsupported checkpoint version: %d (expected %d)"
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
        Ok {
          version;
          session_id = json |> member "session_id" |> to_string;
          agent_name = json |> member "agent_name" |> to_string;
          model =
            (match model_of_yojson (json |> member "model") with
             | Ok m -> m
             | Error _ -> Claude_sonnet_4_6);
          system_prompt = json |> member "system_prompt" |> to_string_option;
          messages =
            json |> member "messages" |> to_list |> List.map message_of_json;
          usage = json |> member "usage" |> usage_of_json;
          turn_count = json |> member "turn_count" |> to_int;
          created_at = json |> member "created_at" |> to_float;
          tools =
            json |> member "tools" |> to_list |> List.map tool_schema_of_json;
          tool_choice;
        }
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
