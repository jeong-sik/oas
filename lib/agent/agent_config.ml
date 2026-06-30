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

    Provider values are runtime provider ids or aliases from
    {!Provider_runtime_binding}; ["local"] remains the built-in llama-server
    shorthand. Unknown strings are custom provider names unless paired with an
    explicit [base_url], in which case they are treated as an explicit
    OpenAI-compatible endpoint using the string as [api_key_env].
*)

open Result_syntax

let bearer_auth_header_for_env api_key_env =
  if String.trim api_key_env = "" then None else Some "Authorization"
;;

let string_has_suffix s suffix =
  let len = String.length s in
  let suffix_len = String.length suffix in
  len >= suffix_len && String.sub s (len - suffix_len) suffix_len = suffix
;;

let string_has_prefix s prefix =
  let len = String.length s in
  let prefix_len = String.length prefix in
  len >= prefix_len && String.sub s 0 prefix_len = prefix
;;

let trim_trailing_slashes s =
  let rec loop i =
    if
      i > 0
      && s.[i - 1] = '/'
      && not (i >= 3 && s.[i - 3] = ':' && s.[i - 2] = '/' && s.[i - 1] = '/')
    then loop (i - 1)
    else String.sub s 0 i
  in
  loop (String.length s)
;;

let normalize_request_path path =
  let path = String.trim path in
  if path = "" || path.[0] = '/' then path else "/" ^ path
;;

let normalize_openai_compat_endpoint ~base_url ~path =
  let base_url = base_url |> String.trim |> trim_trailing_slashes in
  let path = normalize_request_path path in
  let path =
    if string_has_suffix base_url "/v1" && string_has_prefix path "/v1/"
    then String.sub path 3 (String.length path - 3)
    else path
  in
  base_url, path
;;

let openai_compat_config ~base_url ~api_key_env ?(path = "/v1/chat/completions") () =
  let base_url, path = normalize_openai_compat_endpoint ~base_url ~path in
  Provider.OpenAICompat
    { base_url
    ; auth_header = bearer_auth_header_for_env api_key_env
    ; path
    ; static_token = None
    }
;;

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
  ; max_turns : int option
  ; enable_thinking : bool option
  ; preserve_thinking : bool option
  ; thinking_budget : int option
  ; provider : string option
  ; base_url : string option
  ; tools : tool_file_config list
  ; mcp_servers : mcp_file_config list
  }

(* ── JSON parsing ────────────────────────────────────────── *)

let root_config_field = "<root>"

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
    let name =
      json |> member "name" |> to_string_option |> Option.value ~default:"agent"
    in
    let model =
      json
      |> member "model"
      |> to_string_option
      |> Option.value ~default:"claude-sonnet-4-6"
    in
    let system_prompt = json |> member "system_prompt" |> to_string_option in
    let max_tokens = json |> member "max_tokens" |> to_int_option in
    let max_turns = json |> member "max_turns" |> to_int_option in
    let enable_thinking = json |> member "enable_thinking" |> to_bool_option in
    let preserve_thinking = json |> member "preserve_thinking" |> to_bool_option in
    let thinking_budget = json |> member "thinking_budget" |> to_int_option in
    let provider = json |> member "provider" |> to_string_option in
    let base_url = json |> member "base_url" |> to_string_option in
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
    Ok
      { name
      ; model
      ; system_prompt
      ; max_tokens
      ; max_turns
      ; enable_thinking
      ; preserve_thinking
      ; thinking_budget
      ; provider
      ; base_url
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

(** Resolve provider string + optional base_url to a Provider.config. *)
let provider_config_of_binding ~model_id ?base_url (binding : Provider_runtime_binding.t) =
  match base_url with
  | Some url ->
    { Provider.provider =
        openai_compat_config
          ~base_url:url
          ~api_key_env:binding.api_key_env
          ~path:binding.request_path
          ()
    ; model_id
    ; api_key_env = binding.api_key_env
    }
  | None ->
    { Provider.provider = Custom_registered { name = binding.id }
    ; model_id
    ; api_key_env = binding.api_key_env
    }
;;

let resolve_provider ~model_id provider_str base_url =
  let normalized = String.lowercase_ascii (String.trim provider_str) in
  if normalized = "local"
  then (
    let url =
      match base_url with
      | Some u -> u
      | None -> Defaults.resolve_local_llm_url ()
    in
    { Provider.provider = Local { base_url = url }; model_id; api_key_env = "" })
  else (
    match Provider_runtime_binding.find normalized with
    | Some binding -> provider_config_of_binding ~model_id ?base_url binding
    | None ->
      (match base_url with
       | Some url ->
         let api_key_env = String.trim provider_str in
         { Provider.provider = openai_compat_config ~base_url:url ~api_key_env ()
         ; model_id
         ; api_key_env
         }
       | None ->
         { Provider.provider = Custom_registered { name = normalized }
         ; model_id
         ; api_key_env = String.trim provider_str
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
    let spec : Mcp.server_spec =
      { command; args; env = env_pairs; env_policy = Mcp.Minimal; name }
    in
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
  let model = Model_registry.resolve_model_id cfg.model in
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
    match cfg.max_turns with
    | Some n -> Builder.with_max_turns n b
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
    match cfg.provider with
    | Some p -> Builder.with_provider (resolve_provider ~model_id:model p cfg.base_url) b
    | None -> b
  in
  if cfg.tools <> []
  then
    invalid_arg
      "Agent_config.to_builder: inline config tools have no executable runner; use \
       mcp_servers or register typed tools in code";
  (* Connect MCP servers if sw+mgr provided *)
  let b =
    match sw, mgr with
    | Some sw, Some mgr when cfg.mcp_servers <> [] ->
      (match connect_mcp_servers_required ~sw ~mgr ~net cfg.mcp_servers with
       | Ok managed -> if managed <> [] then Builder.with_mcp_clients managed b else b
       | Error err -> invalid_arg (Error.to_string err))
    | _ -> b
  in
  b
;;
