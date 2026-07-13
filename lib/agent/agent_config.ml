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
        "provider": "llama-server",
        "enable_thinking": true,
        "thinking_budget": 2048,
        "reasoning_effort": "high",
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

    Provider values are exact runtime provider ids or catalog-declared aliases
    from {!Provider_runtime_binding}. Endpoint, transport, and authentication
    facts belong to the provider catalog and are never inferred here.
*)

open Result_syntax

(* ── Tool config ─────────────────────────────────────────── *)

type tool_file_config =
  { name : string
  ; description : string
  ; parameters : Types.tool_param list
  }

(* ── MCP server config ───────────────────────────────────── *)

type mcp_file_config =
  | Stdio_mcp of
      { command : string
      ; args : string list
      ; name : string
      ; env : string list
      }
  | Http_mcp of
      { url : string
      ; headers : (string * string) list
      ; name : string
      }

(* ── Agent config ────────────────────────────────────────── *)

type agent_file_config =
  { name : string
  ; model : string
  ; system_prompt : string option
  ; max_tokens : int option
  ; enable_thinking : bool option
  ; preserve_thinking : bool option
  ; thinking_budget : int option
  ; reasoning_effort : Llm_provider.Reasoning_effort.t option
  ; provider : string option
  ; tools : tool_file_config list
  ; mcp_servers : mcp_file_config list
  }

(* ── JSON parsing ────────────────────────────────────────── *)

let root_config_field = "<root>"

let agent_config_fields =
  [ "name"
  ; "model"
  ; "system_prompt"
  ; "max_tokens"
  ; "enable_thinking"
  ; "preserve_thinking"
  ; "thinking_budget"
  ; "reasoning_effort"
  ; "provider"
  ; "tools"
  ; "mcp_servers"
  ]
;;

let json_pointer_escape s =
  s
  |> String.split_on_char '~'
  |> String.concat "~0"
  |> String.split_on_char '/'
  |> String.concat "~1"
;;

let field_path_to_string = function
  | [] -> root_config_field
  | [ field ] -> field
  | fields -> "/" ^ String.concat "/" (List.map json_pointer_escape fields)
;;

let invalid_type ~field ~expected json =
  Error
    (Error.Config
       (InvalidConfig
          { field
          ; detail =
              Printf.sprintf
                "expected %s, got %s"
                expected
                (Llm_provider.Json_util.json_type_name json)
          }))
;;

let field_opt field = function
  | `Assoc pairs -> List.assoc_opt field pairs
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ -> None
;;

let require_object ~field = function
  | `Assoc _ -> Ok ()
  | other -> invalid_type ~field ~expected:"object" other
;;

let reject_unknown_fields ~allowed = function
  | `Assoc fields ->
    (match List.find_opt (fun (name, _) -> not (List.mem name allowed)) fields with
     | None -> Ok ()
     | Some (field, _) ->
       Error
         (Error.Config (InvalidConfig { field; detail = "unknown configuration field" })))
  | (`Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _) as other ->
    invalid_type ~field:root_config_field ~expected:"object" other
;;

let invalid_type_at ~field_path ~expected json =
  invalid_type ~field:(field_path_to_string field_path) ~expected json
;;

let parse_optional_list_field ~field json =
  match field_opt field json with
  | None -> Ok []
  | Some (`List values) -> Ok values
  | Some other -> invalid_type ~field ~expected:"list" other
;;

let parse_optional_object_field ~field json =
  match field_opt field json with
  | None -> Ok []
  | Some (`Assoc pairs) -> Ok pairs
  | Some other -> invalid_type ~field ~expected:"object" other
;;

let parse_optional_string_list_field ~field json =
  let* values = parse_optional_list_field ~field json in
  List.fold_left
    (fun acc value ->
       match acc, value with
       | (Error _ as e), _ -> e
       | Ok values, `String value -> Ok (value :: values)
       | Ok _, other -> invalid_type ~field ~expected:"list of strings" other)
    (Ok [])
    values
  |> Result.map List.rev
;;

let parse_param json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description = Util.json_member_str "description" json in
    let* param_type =
      match json |> member "type" with
      | `String type_name ->
        (match Mcp.json_schema_type_to_param_type_result type_name with
         | Ok param_type -> Ok param_type
         | Error detail ->
           Error (Error.Config (InvalidConfig { field = "parameter.type"; detail })))
      | `Null ->
        Error
          (Error.Config
             (InvalidConfig
                { field = "parameter.type"; detail = "missing required field" }))
      | other -> invalid_type ~field:"parameter.type" ~expected:"string" other
    in
    let required = Util.json_member_bool "required" json in
    Ok { Types.name; description; param_type; required }
  with
  | Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "parameter"; detail = msg }))
;;

let parse_tool json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description = Util.json_member_str "description" json in
    let* params_json = parse_optional_list_field ~field:"parameters" json in
    let params_result =
      List.fold_left
        (fun acc j ->
           match acc with
           | Error _ as e -> e
           | Ok ps ->
             (match parse_param j with
              | Ok p -> Ok (p :: ps)
              | Error e -> Error e))
        (Ok [])
        params_json
    in
    let* parameters = params_result in
    Ok { name; description; parameters = List.rev parameters }
  with
  | Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "tool"; detail = msg }))
;;

let parse_mcp json =
  let open Yojson.Safe.Util in
  try
    match json |> member "url" |> to_string_option with
    | Some url ->
      (* HTTP MCP: { "url": "...", "name": "...", "headers": {...} } *)
      let name = json |> member "name" |> to_string_option |> Option.value ~default:url in
      let* header_fields = parse_optional_object_field ~field:"headers" json in
      let* headers =
        List.fold_left
          (fun acc (k, v) ->
             match acc, v with
             | (Error _ as e), _ -> e
             | Ok headers, `String value -> Ok ((k, value) :: headers)
             | Ok _, other ->
               invalid_type_at ~field_path:[ "headers"; k ] ~expected:"string" other)
          (Ok [])
          header_fields
        |> Result.map List.rev
      in
      Ok (Http_mcp { url; headers; name })
    | None ->
      (* Stdio MCP: { "command": "...", "args": [...], ... } *)
      let command = json |> member "command" |> to_string in
      let* args = parse_optional_string_list_field ~field:"args" json in
      let name =
        json |> member "name" |> to_string_option |> Option.value ~default:command
      in
      let* env = parse_optional_string_list_field ~field:"env" json in
      Ok (Stdio_mcp { command; args; name; env })
  with
  | Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "mcp_server"; detail = msg }))
;;

let of_json json =
  let open Yojson.Safe.Util in
  try
    let* () = require_object ~field:root_config_field json in
    let* () = reject_unknown_fields ~allowed:agent_config_fields json in
    let name =
      json |> member "name" |> to_string_option |> Option.value ~default:"agent"
    in
    let system_prompt = json |> member "system_prompt" |> to_string_option in
    let max_tokens = json |> member "max_tokens" |> to_int_option in
    let enable_thinking = json |> member "enable_thinking" |> to_bool_option in
    let preserve_thinking = json |> member "preserve_thinking" |> to_bool_option in
    let thinking_budget = json |> member "thinking_budget" |> to_int_option in
    let* reasoning_effort =
      match json |> member "reasoning_effort" with
      | `Null -> Ok None
      | `String value ->
        (match Llm_provider.Reasoning_effort.of_string value with
         | Some effort -> Ok (Some effort)
         | None ->
           Error
             (Error.Config
                (InvalidConfig
                   { field = "reasoning_effort"
                   ; detail =
                       Printf.sprintf
                         "unsupported value %S; expected one of %s"
                         value
                         Llm_provider.Reasoning_effort.values_for_log
                   })))
      | _ ->
        Error
          (Error.Config
             (InvalidConfig
                { field = "reasoning_effort"; detail = "must be a string or null" }))
    in
    let provider = json |> member "provider" |> to_string_option in
    let* tools_json = parse_optional_list_field ~field:"tools" json in
    let* tools =
      match tools_json with
      | [] -> Ok []
      | _ :: _ ->
        Error
          (Error.Config
             (InvalidConfig
                { field = "tools"
                ; detail =
                    "inline config tools have no executable runner; use mcp_servers or \
                     register typed tools in code"
                }))
    in
    let* mcp_json = parse_optional_list_field ~field:"mcp_servers" json in
    let mcp_result =
      List.fold_left
        (fun acc j ->
           match acc with
           | Error _ as e -> e
           | Ok ms ->
             (match parse_mcp j with
              | Ok m -> Ok (m :: ms)
              | Error e -> Error e))
        (Ok [])
        mcp_json
    in
    let* mcp_servers = mcp_result in
    let* model =
      match json |> member "model" with
      | `String value when String.trim value <> "" -> Ok value
      | `String _ | `Null ->
        Error
          (Error.Config
             (InvalidConfig
                { field = "model"; detail = "exact non-empty model id is required" }))
      | other -> invalid_type ~field:"model" ~expected:"string" other
    in
    Ok
      { name
      ; model
      ; system_prompt
      ; max_tokens
      ; enable_thinking
      ; preserve_thinking
      ; thinking_budget
      ; reasoning_effort
      ; provider
      ; tools = List.rev tools
      ; mcp_servers = List.rev mcp_servers
      }
  with
  | Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = root_config_field; detail = msg }))
;;

let load path =
  try
    let data = In_channel.with_open_text path In_channel.input_all in
    let json = Yojson.Safe.from_string data in
    of_json json
  with
  | Sys_error msg -> Error (Error.Io (FileOpFailed { op = "load"; path; detail = msg }))
  | Yojson.Json_error msg ->
    Error (Error.Io (FileOpFailed { op = "load"; path; detail = "JSON error: " ^ msg }))
;;

let resolve_provider ~model_id provider_id =
  match Provider_runtime_binding.resolve ~model:model_id provider_id with
  | Some result -> Result.map snd result
  | None ->
    Error
      (Error.Config
         (InvalidConfig
            { field = "provider"
            ; detail = Printf.sprintf "unknown provider id %S" provider_id
            }))
;;

(** Convert mcp_file_config to a server spec for stdio, or connect HTTP directly. *)
let connect_mcp_server ~sw ~mgr ~net mcp_cfg =
  match mcp_cfg with
  | Stdio_mcp { command; args; name; env } ->
    let env_pairs =
      List.filter_map
        (fun entry ->
           match String.split_on_char '=' entry with
           | k :: rest -> Some (k, String.concat "=" rest)
           | [] -> None)
        env
    in
    let spec : Mcp.server_spec = { command; args; env = env_pairs; name } in
    Mcp.connect_and_load ~sw ~mgr spec
  | Http_mcp { url; headers; name } ->
    let spec : Mcp_http.http_spec = { base_url = url; headers; name } in
    Mcp_http.connect_and_load_managed ~sw ~net spec
;;

(** Connect all MCP servers from config.  Config-declared servers are required:
    dropping a failed server silently removes tools from the agent surface. *)
let connect_mcp_servers_required ~sw ~mgr ~net mcp_cfgs =
  List.fold_left
    (fun acc cfg ->
       match connect_mcp_server ~sw ~mgr ~net cfg with
       | Ok managed -> Result.map (fun manageds -> managed :: manageds) acc
       | Error e ->
         let name =
           match cfg with
           | Stdio_mcp { name; _ } -> name
           | Http_mcp { name; _ } -> name
         in
         Error
           (Error.Config
              (InvalidConfig
                 { field = "mcp_servers"
                 ; detail =
                     Printf.sprintf
                       "required MCP server %S failed to connect: %s"
                       name
                       (Error.to_string e)
                 })))
    (Ok [])
    mcp_cfgs
  |> Result.map List.rev
;;

(** Convert a loaded config to a Builder.t.
    When [~sw] and [~mgr] are provided, MCP servers from config are connected
    and their tools are registered.  Without them, MCP servers are skipped. *)
let to_builder ?sw ?mgr ~net (cfg : agent_file_config) =
  let* () =
    match cfg.tools with
    | [] -> Ok ()
    | _ :: _ ->
      Error
        (Error.Config
           (InvalidConfig
              { field = "tools"
              ; detail =
                  "inline config tools have no executable runner; use mcp_servers or \
                   register typed tools in code"
              }))
  in
  let model = cfg.model in
  let b = Builder.create ~net ~model in
  let b = Builder.with_name cfg.name b in
  let b =
    match cfg.system_prompt with
    | Some p -> Builder.with_system_prompt p b
    | None -> b
  in
  let b =
    match cfg.max_tokens with
    | Some n -> Builder.with_max_tokens n b
    | None -> b
  in
  let b =
    match cfg.enable_thinking with
    | Some v -> Builder.with_enable_thinking v b
    | None -> b
  in
  let b =
    match cfg.preserve_thinking with
    | Some v -> Builder.with_preserve_thinking v b
    | None -> b
  in
  let b =
    match cfg.thinking_budget with
    | Some n -> Builder.with_thinking_budget n b
    | None -> b
  in
  let b =
    match cfg.reasoning_effort with
    | Some effort -> Builder.with_reasoning_effort effort b
    | None -> b
  in
  let* b =
    match cfg.provider with
    | Some provider_id ->
      let* provider = resolve_provider ~model_id:model provider_id in
      Ok (Builder.with_provider provider b)
    | None -> Ok b
  in
  (* Connect MCP servers if sw+mgr provided *)
  let* b =
    match sw, mgr with
    | Some sw, Some mgr when cfg.mcp_servers <> [] ->
      let* managed = connect_mcp_servers_required ~sw ~mgr ~net cfg.mcp_servers in
      Ok (if managed <> [] then Builder.with_mcp_clients managed b else b)
    | _ -> Ok b
  in
  Ok b
;;
