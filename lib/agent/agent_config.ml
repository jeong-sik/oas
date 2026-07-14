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

let _log = Log.create ~module_name:"agent_config" ()

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

let parse_optional_string_field ~field json =
  match field_opt field json with
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some other -> invalid_type ~field ~expected:"string or null" other
;;

let parse_optional_int_field ~field json =
  match field_opt field json with
  | None | Some `Null -> Ok None
  | Some (`Int value) -> Ok (Some value)
  | Some other -> invalid_type ~field ~expected:"integer or null" other
;;

let parse_optional_bool_field ~field json =
  match field_opt field json with
  | None | Some `Null -> Ok None
  | Some (`Bool value) -> Ok (Some value)
  | Some other -> invalid_type ~field ~expected:"boolean or null" other
;;

let parse_required_string_field ~field json =
  match field_opt field json with
  | None ->
    Error
      (Error.Config (InvalidConfig { field; detail = "required string field is missing" }))
  | Some (`String value) -> Ok value
  | Some other -> invalid_type ~field ~expected:"string" other
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

let parse_mcp json =
  let* () = require_object ~field:"mcp_server" json in
  match field_opt "url" json, field_opt "command" json with
  | Some _, Some _ ->
    Error
      (Error.Config
         (InvalidConfig
            { field = "mcp_server"
            ; detail = "exactly one transport field is allowed: url or command"
            }))
  | None, None ->
    Error
      (Error.Config
         (InvalidConfig
            { field = "mcp_server"
            ; detail = "exactly one transport field is required: url or command"
            }))
  | Some _, None ->
    let* () = reject_unknown_fields ~allowed:[ "url"; "name"; "headers" ] json in
    let* url = parse_required_string_field ~field:"url" json in
    let* configured_name = parse_optional_string_field ~field:"name" json in
    let name = Option.value ~default:url configured_name in
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
  | None, Some _ ->
    let* () = reject_unknown_fields ~allowed:[ "command"; "args"; "name"; "env" ] json in
    let* command = parse_required_string_field ~field:"command" json in
    let* args = parse_optional_string_list_field ~field:"args" json in
    let* configured_name = parse_optional_string_field ~field:"name" json in
    let name = Option.value ~default:command configured_name in
    let* env = parse_optional_string_list_field ~field:"env" json in
    Ok (Stdio_mcp { command; args; name; env })
;;

let of_json json =
  let open Yojson.Safe.Util in
  try
    let* () = require_object ~field:root_config_field json in
    let* () = reject_unknown_fields ~allowed:agent_config_fields json in
    let* name = parse_optional_string_field ~field:"name" json in
    let name = Option.value ~default:"agent" name in
    let* system_prompt = parse_optional_string_field ~field:"system_prompt" json in
    let* max_tokens = parse_optional_int_field ~field:"max_tokens" json in
    let* enable_thinking = parse_optional_bool_field ~field:"enable_thinking" json in
    let* preserve_thinking = parse_optional_bool_field ~field:"preserve_thinking" json in
    let* thinking_budget = parse_optional_int_field ~field:"thinking_budget" json in
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
    let* provider = parse_optional_string_field ~field:"provider" json in
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
let connect_mcp_server ~sw ?mgr ~net mcp_cfg =
  match mcp_cfg with
  | Stdio_mcp { command; args; name; env } ->
    (match mgr with
     | None ->
       Error
         (Error.Config
            (InvalidConfig
               { field = "mcp_servers"
               ; detail =
                   Printf.sprintf
                     "stdio MCP server %S requires ~mgr; process manager is missing"
                     name
               }))
     | Some mgr ->
       let env_pairs =
         List.filter_map
           (fun entry ->
              match String.split_on_char '=' entry with
              | k :: rest -> Some (k, String.concat "=" rest)
              | [] -> None)
           env
       in
       let spec : Mcp.server_spec = { command; args; env = env_pairs; name } in
       Mcp.connect_and_load ~sw ~mgr spec)
  | Http_mcp { url; headers; name } ->
    let spec : Mcp_http.http_spec = { base_url = url; headers; name } in
    Mcp_http.connect_and_load_managed ~sw ~net spec
;;

(** Connect all MCP servers from config.  Config-declared servers are required:
    dropping a failed server silently removes tools from the agent surface. *)
let mcp_server_name = function
  | Stdio_mcp { name; _ } -> name
  | Http_mcp { name; _ } -> name
;;

let required_mcp_connection_error cfg error =
  Error.Config
    (InvalidConfig
       { field = "mcp_servers"
       ; detail =
           Printf.sprintf
             "required MCP server %S failed to connect: %s"
             (mcp_server_name cfg)
             (Error.to_string error)
       })
;;

type 'a cleanup_failure =
  { resource : 'a
  ; exception_ : exn
  ; backtrace : Printexc.raw_backtrace
  }

let rollback_connected ~close connected =
  List.fold_left
    (fun failures resource ->
       match close resource with
       | () -> failures
       | exception exception_ ->
         let backtrace = Printexc.get_raw_backtrace () in
         { resource; exception_; backtrace } :: failures)
    []
    connected
  |> List.rev
;;

let connect_mcp_servers_transactionally ~connect ~close ~report_cleanup_failures mcp_cfgs =
  let rollback connected =
    Eio.Cancel.protect (fun () ->
      match rollback_connected ~close connected with
      | [] -> ()
      | failures ->
        (match report_cleanup_failures failures with
         | () -> ()
         | exception reporter_exception ->
           let reporter_backtrace = Printexc.get_raw_backtrace () in
           let cleanup_diagnostics =
             List.mapi
               (fun index { exception_; backtrace; _ } ->
                  Printf.sprintf
                    "cleanup[%d]: %s\n%s"
                    index
                    (Log.redact (Printexc.to_string exception_))
                    (Printexc.raw_backtrace_to_string backtrace))
               failures
             |> String.concat "\n"
           in
           Eio.traceln
             "agent_config: MCP cleanup failure reporter raised: %s\n%s\n%s"
             (Log.redact (Printexc.to_string reporter_exception))
             (Printexc.raw_backtrace_to_string reporter_backtrace)
             cleanup_diagnostics))
  in
  let rec loop connected = function
    | [] -> Ok (List.rev connected)
    | cfg :: remaining ->
      let connection =
        try connect cfg with
        | exn ->
          let raw_backtrace = Printexc.get_raw_backtrace () in
          rollback connected;
          Printexc.raise_with_backtrace exn raw_backtrace
      in
      (match connection with
       | Ok managed -> loop (managed :: connected) remaining
       | Error error ->
         rollback connected;
         Error (required_mcp_connection_error cfg error))
  in
  loop [] mcp_cfgs
;;

let close_mcp_managed_for_rollback (managed : Mcp.managed) =
  match managed.transport with
  | Stdio { client; _ } -> Mcp.close client
  | Http { close_fn; _ } -> close_fn ()
;;

let report_mcp_cleanup_failures failures =
  let failures_json =
    List.map
      (fun { resource = (managed : Mcp.managed); exception_; backtrace } ->
         `Assoc
           [ "server", `String managed.name
           ; "error", `String (Printexc.to_string exception_)
           ; "backtrace", `String (Printexc.raw_backtrace_to_string backtrace)
           ])
      failures
  in
  Log.error
    _log
    "MCP transactional rollback could not close every connected server"
    [ Log.J ("failures", `List failures_json) ]
;;

let connect_mcp_servers_required ~sw ?mgr ~net mcp_cfgs =
  connect_mcp_servers_transactionally
    ~connect:(connect_mcp_server ~sw ?mgr ~net)
    ~close:close_mcp_managed_for_rollback
    ~report_cleanup_failures:report_mcp_cleanup_failures
    mcp_cfgs
;;

let%test "required MCP connection stops at the first error and closes prior successes" =
  let first = Stdio_mcp { command = "first"; args = []; name = "first"; env = [] } in
  let second = Stdio_mcp { command = "second"; args = []; name = "second"; env = [] } in
  let third = Stdio_mcp { command = "third"; args = []; name = "third"; env = [] } in
  let fourth = Stdio_mcp { command = "fourth"; args = []; name = "fourth"; env = [] } in
  let connection_error =
    Error.Mcp (ServerStartFailed { command = "third"; detail = "boom" })
  in
  let outcomes =
    ref
      [ Ok "connected-first"
      ; Ok "connected-second"
      ; Error connection_error
      ; Ok "not-attempted"
      ]
  in
  let connect _cfg =
    match !outcomes with
    | outcome :: remaining ->
      outcomes := remaining;
      outcome
    | [] -> failwith "connector called more times than configured outcomes"
  in
  let closed = ref [] in
  let cleanup_failures = ref [] in
  let result =
    Eio_main.run
    @@ fun _env ->
    connect_mcp_servers_transactionally
      ~connect
      ~close:(fun connected -> closed := connected :: !closed)
      ~report_cleanup_failures:(fun failures -> cleanup_failures := failures)
      [ first; second; third; fourth ]
  in
  let exact_error =
    match result with
    | Error (Error.Config (InvalidConfig { field; detail })) ->
      String.equal field "mcp_servers"
      && String.equal
           detail
           "required MCP server \"third\" failed to connect: Failed to start MCP server \
            'third': boom"
    | Error error ->
      failwith ("unexpected MCP transaction error: " ^ Error.to_string error)
    | Ok _ -> false
  in
  exact_error
  && List.rev !closed = [ "connected-second"; "connected-first" ]
  && !cleanup_failures = []
  && !outcomes = [ Ok "not-attempted" ]
;;

let%test "required MCP connection does not attempt a server after an initial error" =
  let first = Http_mcp { url = "first"; headers = []; name = "first" } in
  let second = Http_mcp { url = "second"; headers = []; name = "second" } in
  let connection_error = Error.Mcp (InitializeFailed { detail = "first failed" }) in
  let outcomes = ref [ Error connection_error; Ok "not-attempted" ] in
  let connect _cfg =
    match !outcomes with
    | outcome :: remaining ->
      outcomes := remaining;
      outcome
    | [] -> failwith "connector called more times than configured outcomes"
  in
  let close_calls = ref 0 in
  let result =
    Eio_main.run
    @@ fun _env ->
    connect_mcp_servers_transactionally
      ~connect
      ~close:(fun _connected -> incr close_calls)
      ~report_cleanup_failures:(fun _ -> failwith "unexpected cleanup failure")
      [ first; second ]
  in
  Result.is_error result && !close_calls = 0 && !outcomes = [ Ok "not-attempted" ]
;;

let%test "required MCP rollback survives cancellation and preserves every failure" =
  let exception Connector_raised of int ref in
  let exception Cleanup_raised of string in
  let exception Reporter_raised in
  let first = Http_mcp { url = "first"; headers = []; name = "first" } in
  let second = Http_mcp { url = "second"; headers = []; name = "second" } in
  let third = Http_mcp { url = "third"; headers = []; name = "third" } in
  let payload = ref 42 in
  let outcomes = ref [ Ok "connected-first"; Ok "connected-second" ] in
  let previous_backtrace_status = Printexc.backtrace_status () in
  Printexc.record_backtrace true;
  Fun.protect
    ~finally:(fun () -> Printexc.record_backtrace previous_backtrace_status)
    (fun () ->
       Eio_main.run
       @@ fun _env ->
       Eio.Cancel.sub
       @@ fun cancel_context ->
       let expected_backtrace = ref None in
       let connect _cfg =
         match !outcomes with
         | outcome :: remaining ->
           outcomes := remaining;
           outcome
         | [] ->
           Eio.Cancel.cancel cancel_context Exit;
           (try raise (Connector_raised payload) with
            | exn ->
              let backtrace = Printexc.get_raw_backtrace () in
              expected_backtrace := Some (Printexc.raw_backtrace_to_string backtrace);
              Printexc.raise_with_backtrace exn backtrace)
       in
       let closed = ref [] in
       let reported = ref [] in
       let observed_backtrace =
         match
           connect_mcp_servers_transactionally
             ~connect
             ~close:(fun connected ->
               closed := connected :: !closed;
               Eio.Fiber.yield ();
               raise (Cleanup_raised connected))
             ~report_cleanup_failures:(fun failures ->
               reported := failures;
               raise Reporter_raised)
             [ first; second; third ]
         with
         | Ok _ | Error _ -> None
         | exception Connector_raised actual_payload when actual_payload == payload ->
           Some (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
         | exception _ -> None
       in
       let reported_resources = List.map (fun { resource; _ } -> resource) !reported in
       let reported_exceptions =
         List.map
           (fun { exception_; backtrace; _ } ->
              match exception_ with
              | Cleanup_raised resource ->
                resource, Printexc.raw_backtrace_to_string backtrace
              | _ -> "unexpected", "")
           !reported
       in
       let original_backtrace_preserved =
         match !expected_backtrace, observed_backtrace with
         | Some expected, Some observed ->
           String.length expected > 0 && String.starts_with ~prefix:expected observed
         | None, _ | _, None -> false
       in
       original_backtrace_preserved
       && List.rev !closed = [ "connected-second"; "connected-first" ]
       && reported_resources = [ "connected-second"; "connected-first" ]
       && List.for_all
            (fun (resource, backtrace) ->
               List.mem resource [ "connected-second"; "connected-first" ]
               && String.length backtrace > 0)
            reported_exceptions)
;;

(** Convert a loaded config to a Builder.t.
    Every configured MCP server requires [~sw].  Stdio servers additionally
    require [~mgr].  Missing runtime resources are rejected instead of silently
    dropping the configured tool surface. *)
let to_builder ?sw ?mgr ~net (cfg : agent_file_config) =
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
  let* b =
    match cfg.mcp_servers, sw with
    | [], _ -> Ok b
    | _ :: _, None ->
      Error
        (Error.Config
           (InvalidConfig
              { field = "mcp_servers"
              ; detail = "configured MCP servers require ~sw; runtime switch is missing"
              }))
    | mcp_servers, Some sw
      when Option.is_none mgr
           && List.exists
                (function
                  | Stdio_mcp _ -> true
                  | Http_mcp _ -> false)
                mcp_servers ->
      Error
        (Error.Config
           (InvalidConfig
              { field = "mcp_servers"
              ; detail =
                  "configured stdio MCP servers require ~mgr; process manager is missing"
              }))
    | mcp_servers, Some sw ->
      let* managed = connect_mcp_servers_required ~sw ?mgr ~net mcp_servers in
      Ok (Builder.with_mcp_clients managed b)
  in
  Ok b
;;
