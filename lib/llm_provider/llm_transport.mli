open Base
(** Abstract transport for LLM completions.

    Decouples the completion logic (cache, retry, request execution) from
    the underlying I/O mechanism (HTTP, subprocess, etc.).

    @since 0.78.0

    @stability Internal
    @since 0.93.1 *)

(** Request-scoped MCP server declaration for CLI transports. *)
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

val empty_runtime_mcp_policy : runtime_mcp_policy
val runtime_mcp_server_name : runtime_mcp_server -> string
val runtime_mcp_policy_to_yojson : runtime_mcp_policy -> Yojson.Safe.t

(** A completion request: everything needed to produce a response. *)
type completion_request =
  { config : Provider_config.t
  ; messages : Types.message list
  ; tools : Yojson.Safe.t list
  ; runtime_mcp_policy : runtime_mcp_policy option
  }

(** Result of a sync completion. *)
type sync_result =
  { response : (Types.api_response, Http_client.http_error) result
  ; latency_ms : int
  }

(** Result of a streaming completion. *)
type stream_result = (Types.api_response, Http_client.http_error) result

(** Transport interface.

    Both [complete_sync] and [complete_stream] handle the full
    request → I/O → response pipeline for their transport kind.

    - HTTP transport: build request body, POST, parse response
    - Subprocess transport: write stdin, read stdout, parse output *)
type t =
  { complete_sync : completion_request -> sync_result
  ; complete_stream :
      on_event:(Types.sse_event -> unit) -> completion_request -> stream_result
  }
