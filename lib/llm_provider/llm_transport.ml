(** Abstract transport for LLM completions.

    @since 0.78.0 *)

type runtime_mcp_server =
  | Stdio_server of
      { name : string
      ; command : string
      ; args : string list
      ; env : (string * string) list
      }
  | Http_server of
      { name : string
      ; url : string
      ; headers : (string * string) list
      }

type runtime_mcp_policy =
  { servers : runtime_mcp_server list
  ; allowed_server_names : string list
  ; allowed_tool_names : string list
  ; permission_mode : string option
  ; approval_mode : string option
  ; strict : bool
  ; disable_builtin_tools : bool
  }

type completion_request =
  { config : Provider_config.t
  ; messages : Types.message list
  ; tools : Yojson.Safe.t list
  ; runtime_mcp_policy : runtime_mcp_policy option
  }

let empty_runtime_mcp_policy =
  { servers = []
  ; allowed_server_names = []
  ; allowed_tool_names = []
  ; permission_mode = None
  ; approval_mode = None
  ; strict = true
  ; disable_builtin_tools = false
  }
;;

let runtime_mcp_server_name = function
  | Stdio_server { name; _ } | Http_server { name; _ } -> name
;;

let runtime_mcp_policy_to_yojson (policy : runtime_mcp_policy) =
  let server_to_yojson = function
    | Stdio_server { name; command; args; env } ->
      `Assoc
        [ "kind", `String "stdio"
        ; "name", `String name
        ; "command", `String command
        ; "args", Cli_common_json.json_of_string_list args
        ; "env", `Assoc (List.map (fun (k, v) -> k, `String v) env)
        ]
    | Http_server { name; url; headers } ->
      `Assoc
        [ "kind", `String "http"
        ; "name", `String name
        ; "url", `String url
        ; "headers", `Assoc (List.map (fun (k, v) -> k, `String v) headers)
        ]
  in
  `Assoc
    [ "servers", `List (List.map server_to_yojson policy.servers)
    ; "allowed_server_names", Cli_common_json.json_of_string_list policy.allowed_server_names
    ; "allowed_tool_names", Cli_common_json.json_of_string_list policy.allowed_tool_names
    ; ( "permission_mode"
      , match policy.permission_mode with
        | Some mode -> `String mode
        | None -> `Null )
    ; ( "approval_mode"
      , match policy.approval_mode with
        | Some mode -> `String mode
        | None -> `Null )
    ; "strict", `Bool policy.strict
    ; "disable_builtin_tools", `Bool policy.disable_builtin_tools
    ]
;;

type sync_result =
  { response : (Types.api_response, Http_client.http_error) result
  ; latency_ms : int
  }

type stream_result = (Types.api_response, Http_client.http_error) result

type t =
  { complete_sync : completion_request -> sync_result
  ; complete_stream :
      on_event:(Types.sse_event -> unit) -> completion_request -> stream_result
  }
