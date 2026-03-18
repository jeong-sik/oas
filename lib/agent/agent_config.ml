(** Agent configuration file loader.

    Loads agent configuration from a JSON file (oas.json) and converts
    it to a Builder.t for agent construction.

    Format:
    {[
      {
        "name": "my-agent",
        "model": "claude-sonnet-4-6",
        "system_prompt": "You are helpful.",
        "max_tokens": 4096,
        "max_turns": 10,
        "provider": "local",
        "base_url": "http://127.0.0.1:8085",
        "enable_thinking": true,
        "thinking_budget": 2048,
        "tools": [
          { "name": "get_weather", "description": "Get weather",
            "parameters": [...] }
        ],
        "mcp_servers": [
          { "command": "npx", "args": ["-y", "@modelcontextprotocol/server-everything"],
            "name": "everything" }
        ]
      }
    ]}

    Provider values: "local" (llama-server), "anthropic", "openai",
    or any string (treated as OpenAI-compatible with api_key_env = that string).
*)

let ( let* ) = Result.bind

(* ── Tool config ─────────────────────────────────────────── *)

type tool_file_config = {
  name: string;
  description: string;
  parameters: Types.tool_param list;
}

(* ── MCP server config ───────────────────────────────────── *)

type mcp_file_config = {
  command: string;
  args: string list;
  name: string;
  env: string list;
}

(* ── Agent config ────────────────────────────────────────── *)

type agent_file_config = {
  name: string;
  model: string;
  system_prompt: string option;
  max_tokens: int option;
  max_turns: int option;
  enable_thinking: bool option;
  thinking_budget: int option;
  provider: string option;
  base_url: string option;
  tools: tool_file_config list;
  mcp_servers: mcp_file_config list;
}

(* ── JSON parsing ────────────────────────────────────────── *)

let parse_param json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description = json |> member "description" |> to_string_option
      |> Option.value ~default:"" in
    let param_type = json |> member "type" |> to_string_option
      |> Option.value ~default:"string"
      |> Mcp.json_schema_type_to_param_type in
    let required = json |> member "required" |> to_bool_option
      |> Option.value ~default:false in
    Ok { Types.name; description; param_type; required }
  with Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "parameter"; detail = msg }))

let parse_tool json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description = json |> member "description" |> to_string_option
      |> Option.value ~default:"" in
    let params_json = match json |> member "parameters" with
      | `List ps -> ps
      | _ -> []
    in
    let params_result = List.fold_left (fun acc j ->
      match acc with
      | Error _ as e -> e
      | Ok ps ->
        match parse_param j with
        | Ok p -> Ok (p :: ps)
        | Error e -> Error e
    ) (Ok []) params_json in
    let* parameters = params_result in
    Ok { name; description; parameters = List.rev parameters }
  with Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "tool"; detail = msg }))

let parse_mcp json =
  let open Yojson.Safe.Util in
  try
    let command = json |> member "command" |> to_string in
    let args = match json |> member "args" with
      | `List items -> List.filter_map to_string_option items
      | _ -> []
    in
    let name = json |> member "name" |> to_string_option
      |> Option.value ~default:command in
    let env = match json |> member "env" with
      | `List items -> List.filter_map to_string_option items
      | _ -> []
    in
    Ok { command; args; name; env }
  with Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "mcp_server"; detail = msg }))

let of_json json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string_option
      |> Option.value ~default:"agent" in
    let model = json |> member "model" |> to_string_option
      |> Option.value ~default:"claude-sonnet-4-6" in
    let system_prompt = json |> member "system_prompt" |> to_string_option in
    let max_tokens = json |> member "max_tokens" |> to_int_option in
    let max_turns = json |> member "max_turns" |> to_int_option in
    let enable_thinking = json |> member "enable_thinking" |> to_bool_option in
    let thinking_budget = json |> member "thinking_budget" |> to_int_option in
    let provider = json |> member "provider" |> to_string_option in
    let base_url = json |> member "base_url" |> to_string_option in
    let tools_json = match json |> member "tools" with
      | `List ts -> ts
      | _ -> []
    in
    let tools_result = List.fold_left (fun acc j ->
      match acc with
      | Error _ as e -> e
      | Ok ts ->
        match parse_tool j with
        | Ok t -> Ok (t :: ts)
        | Error e -> Error e
    ) (Ok []) tools_json in
    let* tools = tools_result in
    let mcp_json = match json |> member "mcp_servers" with
      | `List ms -> ms
      | _ -> []
    in
    let mcp_result = List.fold_left (fun acc j ->
      match acc with
      | Error _ as e -> e
      | Ok ms ->
        match parse_mcp j with
        | Ok m -> Ok (m :: ms)
        | Error e -> Error e
    ) (Ok []) mcp_json in
    let* mcp_servers = mcp_result in
    Ok {
      name;
      model;
      system_prompt;
      max_tokens;
      max_turns;
      enable_thinking;
      thinking_budget;
      provider;
      base_url;
      tools = List.rev tools;
      mcp_servers = List.rev mcp_servers;
    }
  with Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "agent_config"; detail = msg }))

let load path =
  try
    let data = In_channel.with_open_text path In_channel.input_all in
    let json = Yojson.Safe.from_string data in
    of_json json
  with
  | Sys_error msg ->
    Error (Error.Io (FileOpFailed { op = "load"; path; detail = msg }))
  | Yojson.Json_error msg ->
    Error (Error.Io (FileOpFailed { op = "load"; path; detail = "JSON error: " ^ msg }))

(** Resolve provider string + optional base_url to a Provider.config. *)
let resolve_provider ~model_id provider_str base_url =
  match provider_str with
  | "local" ->
      let url = match base_url with
        | Some u -> u
        | None -> Defaults.local_llm_url
      in
      { Provider.provider = Local { base_url = url };
        model_id; api_key_env = "" }
  | "anthropic" ->
      { Provider.provider = Anthropic;
        model_id; api_key_env = "ANTHROPIC_API_KEY" }
  | "openai" ->
      let url = match base_url with
        | Some u -> u
        | None -> "https://api.openai.com"
      in
      { Provider.provider = OpenAICompat {
          base_url = url; auth_header = None;
          path = "/v1/chat/completions"; static_token = None };
        model_id; api_key_env = "OPENAI_API_KEY" }
  | other ->
      let url = match base_url with
        | Some u -> u
        | None -> Defaults.local_llm_url
      in
      { Provider.provider = OpenAICompat {
          base_url = url; auth_header = None;
          path = "/v1/chat/completions"; static_token = None };
        model_id; api_key_env = other }

(** Convert a loaded config to a Builder.t. *)
let to_builder ~net (cfg : agent_file_config) =
  let model = Model_registry.resolve_model_id cfg.model in
  let b = Builder.create ~net ~model in
  let b = Builder.with_name cfg.name b in
  let b = match cfg.system_prompt with
    | Some p -> Builder.with_system_prompt p b
    | None -> b
  in
  let b = match cfg.max_tokens with
    | Some n -> Builder.with_max_tokens n b
    | None -> b
  in
  let b = match cfg.max_turns with
    | Some n -> Builder.with_max_turns n b
    | None -> b
  in
  let b = match cfg.enable_thinking with
    | Some v -> Builder.with_enable_thinking v b
    | None -> b
  in
  let b = match cfg.thinking_budget with
    | Some n -> Builder.with_thinking_budget n b
    | None -> b
  in
  let b = match cfg.provider with
    | Some p -> Builder.with_provider (resolve_provider ~model_id:model p cfg.base_url) b
    | None -> b
  in
  let tools = List.map (fun (tc : tool_file_config) ->
    Tool.create ~name:tc.name ~description:tc.description
      ~parameters:tc.parameters
      (fun input ->
        Ok { Types.content = Printf.sprintf "[%s] called with %s"
          tc.name (Yojson.Safe.to_string input) })
  ) cfg.tools in
  let b = if tools <> [] then Builder.with_tools tools b else b in
  b
