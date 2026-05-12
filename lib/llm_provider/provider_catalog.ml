(** External provider catalog overlay. *)

type transport =
  | Http
  | Cli
  | Managed
  | Custom_openai_compat
[@@deriving show]

type auth_mode =
  | No_auth
  | Api_key_env of string
  | Cli_cached_login
  | Oauth_cached_login
  | Setup_token_env of string
  | File of string
  | Exec of string
[@@deriving show]

type entry =
  { id : string
  ; aliases : string list
  ; kind : Provider_config.provider_kind
  ; transport : transport
  ; command : string option
  ; base_url : string
  ; request_path : string
  ; api_key_env : string
  ; auth : auth_mode
  ; default_model : string option
  ; max_context : int option
  ; capabilities : Capabilities.capabilities
  ; non_interactive : bool
  ; interactive_required : bool
  ; daemon_safe : bool
  ; credential_scope : string option
  }

type t = entry list

let json_kind = function
  | `Null -> "null"
  | `Bool _ -> "bool"
  | `Int _ -> "int"
  | `Intlit _ -> "intlit"
  | `Float _ -> "float"
  | `String _ -> "string"
  | `Assoc _ -> "object"
  | `List _ -> "array"
  | `Tuple _ -> "tuple"
  | `Variant _ -> "variant"
;;

let warn_type_mismatch key ~expected actual =
  match actual with
  | `Null -> ()
  | _ ->
    Diag.warn
      "provider_catalog"
      "ignoring field %S: expected %s, got %s"
      key
      expected
      (json_kind actual)
;;

let member key json = Yojson.Safe.Util.member key json

let member_string key json =
  match member key json with
  | `String s -> Some s
  | actual ->
    warn_type_mismatch key ~expected:"string" actual;
    None
;;

let member_string_default key ~default json =
  match member_string key json with
  | Some s -> s
  | None -> default
;;

let member_bool key json =
  match member key json with
  | `Bool b -> Some b
  | actual ->
    warn_type_mismatch key ~expected:"bool" actual;
    None
;;

let member_bool_default key ~default json =
  match member_bool key json with
  | Some b -> b
  | None -> default
;;

let member_int key json =
  match member key json with
  | `Int n -> Some n
  | `Intlit s ->
    (match int_of_string_opt s with
     | Some n -> Some n
     | None ->
       Diag.warn
         "provider_catalog"
         "ignoring field %S: integer literal %S out of native int range"
         key
         s;
       None)
  | actual ->
    warn_type_mismatch key ~expected:"int" actual;
    None
;;

let member_string_list key json =
  match member key json with
  | `List items ->
    Some
      (List.filter_map
         (function
           | `String s when String.trim s <> "" -> Some (String.trim s)
           | actual ->
             warn_type_mismatch key ~expected:"string list" actual;
             None)
         items)
  | `Null -> None
  | actual ->
    warn_type_mismatch key ~expected:"array" actual;
    None
;;

let parse_transport = function
  | None -> None
  | Some raw ->
    (match String.lowercase_ascii (String.trim raw) with
     | "http" -> Some Http
     | "cli" -> Some Cli
     | "managed" -> Some Managed
     | "custom_openai_compat" | "custom-openai-compat" | "openai_compat" ->
       Some Custom_openai_compat
     | other ->
       Diag.warn "provider_catalog" "unknown transport %S; treating as http" other;
       Some Http)
;;

let default_transport_for_kind kind =
  if Provider_kind.is_subprocess_cli kind then Cli else Http
;;

let auth_env = function
  | Api_key_env env | Setup_token_env env -> env
  | No_auth | Cli_cached_login | Oauth_cached_login | File _ | Exec _ -> ""
;;

let parse_auth json =
  match member "auth" json with
  | `Assoc _ as auth_json ->
    let auth_type =
      member_string_default "type" ~default:"none" auth_json
      |> String.trim
      |> String.lowercase_ascii
    in
    let env =
      match member_string "env" auth_json with
      | Some v -> v
      | None -> member_string_default "key" ~default:"" auth_json
    in
    (match auth_type with
     | "none" -> No_auth
     | "api_key_env" | "api-key-env" | "env" -> Api_key_env env
     | "setup_token_env" | "setup-token-env" -> Setup_token_env env
     | "cli_cached_login" | "cli-cached-login" -> Cli_cached_login
     | "oauth_cached_login" | "oauth-cached-login" -> Oauth_cached_login
     | "file" -> File (member_string_default "path" ~default:"" auth_json)
     | "exec" -> Exec (member_string_default "command" ~default:"" auth_json)
     | other ->
       Diag.warn "provider_catalog" "unknown auth type %S; treating as none" other;
       No_auth)
  | _ ->
    (match member_string "api_key_env" json with
     | Some env when String.trim env <> "" -> Api_key_env env
     | _ -> No_auth)
;;

let parse_thinking_control_format = function
  | None -> None
  | Some raw ->
    (match String.lowercase_ascii (String.trim raw) with
     | "none" | "no_thinking_control" | "no-thinking-control" ->
       Some Capabilities.No_thinking_control
     | "thinking_object" | "thinking-object" -> Some Capabilities.Thinking_object
     | "chat_template_kwargs" | "chat-template-kwargs" ->
       Some Capabilities.Chat_template_kwargs
     | other ->
       Diag.warn
         "provider_catalog"
         "unknown thinking_control_format %S; inheriting base"
         other;
       None)
;;

let member_supported_models json = member_string_list "supported_models" json

let capability_base json =
  let label =
    match member_string "capabilities_base" json with
    | Some v -> Some v
    | None -> member_string "base" json
  in
  match label with
  | Some raw ->
    (match Capabilities.capabilities_for_provider_label raw with
     | Some caps -> caps
     | None ->
       Diag.warn
         "provider_catalog"
         "unknown capabilities_base %S; using default capabilities"
         raw;
       Capabilities.default_capabilities)
  | None -> Capabilities.default_capabilities
;;

let override_bool key caps f json =
  match member_bool key json with
  | Some v -> f caps v
  | None -> caps
;;

let override_int_opt key caps f json =
  match member_int key json with
  | Some v -> f caps (Some v)
  | None -> caps
;;

let parse_capabilities provider_json =
  let cap_json =
    match member "capabilities" provider_json with
    | `Assoc _ as v -> v
    | _ -> provider_json
  in
  let base = capability_base provider_json in
  let caps =
    base
    |> fun caps ->
    override_int_opt
      "max_context_tokens"
      caps
      (fun caps v -> { caps with Capabilities.max_context_tokens = v })
      cap_json
    |> fun caps ->
    override_int_opt
      "max_output_tokens"
      caps
      (fun caps v -> { caps with Capabilities.max_output_tokens = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_tools"
      caps
      (fun caps v -> { caps with Capabilities.supports_tools = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_tool_choice"
      caps
      (fun caps v -> { caps with Capabilities.supports_tool_choice = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_parallel_tool_calls"
      caps
      (fun caps v -> { caps with Capabilities.supports_parallel_tool_calls = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_runtime_mcp_tools"
      caps
      (fun caps v -> { caps with Capabilities.supports_runtime_mcp_tools = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_runtime_tool_events"
      caps
      (fun caps v -> { caps with Capabilities.supports_runtime_tool_events = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_reasoning"
      caps
      (fun caps v -> { caps with Capabilities.supports_reasoning = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_extended_thinking"
      caps
      (fun caps v -> { caps with Capabilities.supports_extended_thinking = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_reasoning_budget"
      caps
      (fun caps v -> { caps with Capabilities.supports_reasoning_budget = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_response_format_json"
      caps
      (fun caps v -> { caps with Capabilities.supports_response_format_json = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_structured_output"
      caps
      (fun caps v -> { caps with Capabilities.supports_structured_output = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_multimodal_inputs"
      caps
      (fun caps v -> { caps with Capabilities.supports_multimodal_inputs = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_image_input"
      caps
      (fun caps v -> { caps with Capabilities.supports_image_input = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_audio_input"
      caps
      (fun caps v -> { caps with Capabilities.supports_audio_input = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_video_input"
      caps
      (fun caps v -> { caps with Capabilities.supports_video_input = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_native_streaming"
      caps
      (fun caps v -> { caps with Capabilities.supports_native_streaming = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_system_prompt"
      caps
      (fun caps v -> { caps with Capabilities.supports_system_prompt = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_caching"
      caps
      (fun caps v -> { caps with Capabilities.supports_caching = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_prompt_caching"
      caps
      (fun caps v -> { caps with Capabilities.supports_prompt_caching = v })
      cap_json
    |> fun caps ->
    override_int_opt
      "prompt_cache_alignment"
      caps
      (fun caps v -> { caps with Capabilities.prompt_cache_alignment = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_top_k"
      caps
      (fun caps v -> { caps with Capabilities.supports_top_k = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_min_p"
      caps
      (fun caps v -> { caps with Capabilities.supports_min_p = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_seed"
      caps
      (fun caps v -> { caps with Capabilities.supports_seed = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_seed_with_images"
      caps
      (fun caps v -> { caps with Capabilities.supports_seed_with_images = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_computer_use"
      caps
      (fun caps v -> { caps with Capabilities.supports_computer_use = v })
      cap_json
    |> fun caps ->
    override_bool
      "supports_code_execution"
      caps
      (fun caps v -> { caps with Capabilities.supports_code_execution = v })
      cap_json
    |> fun caps ->
    override_bool
      "is_ollama"
      caps
      (fun caps v -> { caps with Capabilities.is_ollama = v })
      cap_json
    |> fun caps ->
    override_bool
      "emits_usage_tokens"
      caps
      (fun caps v -> { caps with Capabilities.emits_usage_tokens = v })
      cap_json
  in
  let caps =
    match
      parse_thinking_control_format (member_string "thinking_control_format" cap_json)
    with
    | Some thinking_control_format -> { caps with Capabilities.thinking_control_format }
    | None -> caps
  in
  match member_supported_models cap_json with
  | Some models -> { caps with Capabilities.supported_models = Some models }
  | None -> caps
;;

let parse_entry json =
  match member_string "id" json with
  | None -> Error "provider entry missing required \"id\" field"
  | Some id ->
    let id = String.trim id in
    if id = ""
    then Error "provider entry has empty \"id\" field"
    else (
      let kind_raw = member_string_default "kind" ~default:"openai_compat" json in
      match Provider_kind.of_string kind_raw with
      | None -> Error (Printf.sprintf "provider %S has unknown kind %S" id kind_raw)
      | Some kind ->
        let auth = parse_auth json in
        let api_key_env =
          match member_string "api_key_env" json with
          | Some env -> env
          | None -> auth_env auth
        in
        let transport =
          match parse_transport (member_string "transport" json) with
          | Some transport -> transport
          | None -> default_transport_for_kind kind
        in
        let interactive_required =
          member_bool_default "interactive_required" ~default:false json
        in
        let non_interactive =
          member_bool_default "non_interactive" ~default:(not interactive_required) json
        in
        let daemon_safe =
          member_bool_default
            "daemon_safe"
            ~default:(non_interactive && not interactive_required)
            json
        in
        let capabilities = parse_capabilities json in
        let max_context =
          match member_int "max_context" json with
          | Some _ as v -> v
          | None -> capabilities.Capabilities.max_context_tokens
        in
        Ok
          { id
          ; aliases = Option.value (member_string_list "aliases" json) ~default:[]
          ; kind
          ; transport
          ; command = member_string "command" json
          ; base_url = member_string_default "base_url" ~default:"" json
          ; request_path =
              member_string_default
                "request_path"
                ~default:(Provider_config.request_path_default_for_kind kind)
                json
          ; api_key_env
          ; auth
          ; default_model = member_string "default_model" json
          ; max_context
          ; capabilities
          ; non_interactive
          ; interactive_required
          ; daemon_safe
          ; credential_scope = member_string "credential_scope" json
          })
;;

let of_json json =
  let schema_version =
    match member "schema_version" json with
    | `Int n -> n
    | _ -> 0
  in
  if schema_version <> 1
  then
    Error
      (Printf.sprintf
         "unsupported provider catalog schema_version: %d (expected 1)"
         schema_version)
  else (
    let items =
      match member "providers" json with
      | `List xs -> xs
      | _ -> []
    in
    let results = List.map parse_entry items in
    let errors =
      List.filter_map
        (function
          | Error e -> Some e
          | Ok _ -> None)
        results
    in
    if errors <> []
    then Error (String.concat "; " errors)
    else
      Ok
        (List.filter_map
           (function
             | Ok e -> Some e
             | Error _ -> None)
           results))
;;

let load_file path =
  let read_result =
    try Ok (Yojson.Safe.from_file path) with
    | Sys_error msg ->
      Error (Printf.sprintf "cannot read provider catalog %s: %s" path msg)
    | Yojson.Json_error msg ->
      Error (Printf.sprintf "provider catalog JSON parse error in %s: %s" path msg)
  in
  Result.bind read_result of_json
;;

let load_runtime_file path =
  match load_file path with
  | Ok catalog ->
    Diag.info
      "provider_catalog"
      "loaded %d provider entries from %s"
      (List.length catalog)
      path;
    Some catalog
  | Error msg ->
    Diag.warn "provider_catalog" "failed to load %s: %s" path msg;
    None
;;

let normalize_id s = String.lowercase_ascii (String.trim s)

let lookup t provider_id =
  let needle = normalize_id provider_id in
  List.find_opt
    (fun entry ->
       normalize_id entry.id = needle
       || List.exists (fun alias -> normalize_id alias = needle) entry.aliases)
    t
;;

let default_model_for_provider t provider_id =
  match lookup t provider_id with
  | Some entry -> entry.default_model
  | None -> None
;;

let env_loaded_catalog : t option Lazy.t =
  lazy
    (match Cli_common_env.get "OAS_PROVIDER_CATALOG" with
     | None -> None
     | Some path -> load_runtime_file path)
;;

let runtime_override : t option Atomic.t = Atomic.make None
let set_global t = Atomic.set runtime_override (Some t)
let clear_global () = Atomic.set runtime_override None

let global () =
  match Atomic.get runtime_override with
  | Some _ as v -> v
  | None ->
    let env_value = Lazy.force env_loaded_catalog in
    (match Atomic.get runtime_override with
     | Some _ as v -> v
     | None -> env_value)
;;
