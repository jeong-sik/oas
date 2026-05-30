(** Agent configuration file loader.

    Loads agent configuration from a JSON file (oas.json) and converts
    it to a Builder.t for agent construction.

    Format:
    {[
      {
        "name": "my-agent",
        "model": "agent_llm_a-sonnet-4-6",
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

    Provider values: "local" (llama-server), "provider_a", "provider_d",
    or any string (treated as Provider_d-compatible with api_key_env = that string).
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
  ; thinking_budget : int option
  ; provider : string option
  ; base_url : string option
  ; tools : tool_file_config list
  ; mcp_servers : mcp_file_config list
  }

(* ── JSON parsing ────────────────────────────────────────── *)

let list_or_empty = function
  | `List values -> values
  | `Assoc _ | `Bool _ | `Float _ | `Int _ | `Intlit _ | `Null | `String _ -> []
;;

let assoc_or_empty = function
  | `Assoc pairs -> pairs
  | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ -> []
;;

let string_option = function
  | `String value -> Some value
  | `Assoc _ | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null -> None
;;

let parse_param json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let description = Util.json_member_str "description" json in
    let param_type =
      json
      |> member "type"
      |> to_string_option
      |> Option.value ~default:"string"
      |> Mcp.json_schema_type_to_param_type
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
    let params_json = json |> member "parameters" |> list_or_empty in
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
      let headers =
        json
        |> member "headers"
        |> assoc_or_empty
        |> List.filter_map (fun (k, v) ->
          match string_option v with
          | Some s -> Some (k, s)
          | None -> None)
      in
      Ok (Http_mcp { url; headers; name })
    | None ->
      (* Stdio MCP: { "command": "...", "args": [...], ... } *)
      let command = json |> member "command" |> to_string in
      let args =
        json |> member "args" |> list_or_empty |> List.filter_map string_option
      in
      let name =
        json |> member "name" |> to_string_option |> Option.value ~default:command
      in
      let env = json |> member "env" |> list_or_empty |> List.filter_map string_option in
      Ok (Stdio_mcp { command; args; name; env })
  with
  | Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "mcp_server"; detail = msg }))
;;

let of_json json =
  let open Yojson.Safe.Util in
  try
    let name =
      json |> member "name" |> to_string_option |> Option.value ~default:"agent"
    in
    let model =
      json
      |> member "model"
      |> to_string_option
      |> Option.value ~default:"agent_llm_a-sonnet-4-6"
    in
    let system_prompt = json |> member "system_prompt" |> to_string_option in
    let max_tokens = json |> member "max_tokens" |> to_int_option in
    let max_turns = json |> member "max_turns" |> to_int_option in
    let enable_thinking = json |> member "enable_thinking" |> to_bool_option in
    let thinking_budget = json |> member "thinking_budget" |> to_int_option in
    let provider = json |> member "provider" |> to_string_option in
    let base_url = json |> member "base_url" |> to_string_option in
    let tools_json = json |> member "tools" |> list_or_empty in
    let tools_result =
      List.fold_left
        (fun acc j ->
           match acc with
           | Error _ as e -> e
           | Ok ts ->
             (match parse_tool j with
              | Ok t -> Ok (t :: ts)
              | Error e -> Error e))
        (Ok [])
        tools_json
    in
    let* tools = tools_result in
    let mcp_json = json |> member "mcp_servers" |> list_or_empty in
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
      ; thinking_budget
      ; provider
      ; base_url
      ; tools = List.rev tools
      ; mcp_servers = List.rev mcp_servers
      }
  with
  | Type_error (msg, _) ->
    Error (Error.Config (InvalidConfig { field = "agent_config"; detail = msg }))
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

(** Resolve provider string + optional base_url to a Provider.config.

    Canonical provider kinds ({!Llm_provider.Provider_kind.t}) are dispatched
    through {!Llm_provider.Provider_kind.of_string}, which accepts the
    canonical wire forms (["provider_a"], ["provider_d_compat"], …) plus the
    documented aliases (["agent_llm_a"] -> Anthropic,
    ["provider_d"] -> OpenAI_compat, ["provider_n"] -> Ollama), case-insensitively
    with leading/trailing whitespace trimmed.

    ["local"] remains a first-class routing shorthand (not a Provider_kind
    variant) and is matched explicitly before the parser so that
    ["LOCAL"] / [" local "] also reach the Local branch.

    Any kind the parser recognises beyond Anthropic / OpenAI_compat — or
    anything it does not recognise at all — falls through to the registry
    lookup path, preserving the legacy [Custom_registered] behaviour that
    downstream tests and configs depend on. *)
let resolve_provider ~model_id provider_str base_url =
  let normalized = String.lowercase_ascii (String.trim provider_str) in
  if normalized = "local"
  then (
    let url =
      match base_url with
      | Some u -> u
      | None -> Defaults.local_llm_url
    in
    { Provider.provider = Local { base_url = url }; model_id; api_key_env = "" })
  else (
    match Llm_provider.Provider_kind.of_string provider_str with
    | Some Anthropic ->
      { Provider.provider = Anthropic; model_id; api_key_env = "PROVIDER_A_API_KEY" }
    | Some OpenAI_compat ->
      let api_key_env = "PROVIDER_D_API_KEY" in
      let url =
        match base_url with
        | Some u -> u
        | None -> "https://api.provider_d.com"
      in
      { Provider.provider = openai_compat_config ~base_url:url ~api_key_env ()
      ; model_id
      ; api_key_env
      }
    | Some (Kimi | Ollama | Gemini | Glm | DashScope) | None ->
      let registry = Llm_provider.Provider_registry.default () in
      (match Llm_provider.Provider_registry.find registry provider_str with
       | Some entry ->
         (match base_url with
          | None ->
            (* Preserve registry-declared kind (Gemini/Glm/Ollama/etc.)
                    via Custom_registered. Downstream (provider_config_of_agent,
                    request_path, Capabilities, resolve) dispatches through
                    Provider_registry by name, retaining entry.defaults.kind. *)
            { Provider.provider = Custom_registered { name = provider_str }
            ; model_id
            ; api_key_env = entry.defaults.api_key_env
            }
          | Some url ->
            (* Explicit base_url override: legacy Provider.config variant
                    cannot carry kind + arbitrary URL simultaneously, so kind
                    flattens to OpenAI_compat. For kind-preserving override,
                    construct Llm_provider.Provider_config.t directly via
                    Provider_config.make. *)
            let api_key_env = entry.defaults.api_key_env in
            { Provider.provider =
                openai_compat_config
                  ~base_url:url
                  ~api_key_env
                  ~path:entry.defaults.request_path
                  ()
            ; model_id
            ; api_key_env
            })
       | None ->
         let api_key_env = provider_str in
         let url =
           match base_url with
           | Some u -> u
           | None -> Defaults.local_llm_url
         in
         { Provider.provider = openai_compat_config ~base_url:url ~api_key_env ()
         ; model_id
         ; api_key_env
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

(** Connect all MCP servers from config (best-effort: failures are logged,
    not fatal). *)
let connect_mcp_servers_best_effort ~sw ~mgr ~net mcp_cfgs =
  List.fold_left
    (fun acc cfg ->
       match connect_mcp_server ~sw ~mgr ~net cfg with
       | Ok managed -> managed :: acc
       | Error e ->
         let name =
           match cfg with
           | Stdio_mcp { name; _ } -> name
           | Http_mcp { name; _ } -> name
         in
         let _log = Log.create ~module_name:"agent_config" () in
         Log.warn
           _log
           "MCP server failed"
           [ S ("server", name); S ("error", Error.to_string e) ];
         acc)
    []
    mcp_cfgs
  |> List.rev
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
    match cfg.thinking_budget with
    | Some n -> Builder.with_thinking_budget n b
    | None -> b
  in
  let b =
    match cfg.provider with
    | Some p -> Builder.with_provider (resolve_provider ~model_id:model p cfg.base_url) b
    | None -> b
  in
  let tools =
    List.map
      (fun (tc : tool_file_config) ->
         Tool.create
           ~name:tc.name
           ~description:tc.description
           ~parameters:tc.parameters
           (fun input ->
              Ok
                { Types.content =
                    Printf.sprintf
                      "[%s] called with %s"
                      tc.name
                      (Yojson.Safe.to_string input)
                }))
      cfg.tools
  in
  let b = if tools <> [] then Builder.with_tools tools b else b in
  (* Connect MCP servers if sw+mgr provided *)
  let b =
    match sw, mgr with
    | Some sw, Some mgr when cfg.mcp_servers <> [] ->
      let managed = connect_mcp_servers_best_effort ~sw ~mgr ~net cfg.mcp_servers in
      if managed <> [] then Builder.with_mcp_clients managed b else b
    | _ -> b
  in
  b
;;
