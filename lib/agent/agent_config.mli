(** Agent configuration file parsing and MCP server connection.

    Loads agent configuration from JSON files, resolves providers,
    and connects MCP servers.

    @stability Internal
    @since 0.93.1 *)

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

(** {1 Parsing} *)

val parse_mcp : Yojson.Safe.t -> (mcp_file_config, Error.sdk_error) result
val of_json : Yojson.Safe.t -> (agent_file_config, Error.sdk_error) result
val load : string -> (agent_file_config, Error.sdk_error) result

(** {1 Provider resolution} *)

val resolve_provider
  :  model_id:Types.model
  -> string
  -> (Provider.config, Error.sdk_error) result

(** {1 MCP connection} *)

val connect_mcp_server
  :  sw:Eio.Switch.t
  -> ?mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> mcp_file_config
  -> (Mcp.managed, Error.sdk_error) result

val connect_mcp_servers_required
  :  sw:Eio.Switch.t
  -> ?mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> mcp_file_config list
  -> (Mcp.managed list, Error.sdk_error) result

(** {1 Builder conversion} *)

(** Convert a loaded configuration to a builder.  Every configured MCP server
    requires [sw]; stdio servers additionally require [mgr].  Missing resources
    produce an explicit [Error.Config] error and never drop configured tools
    silently. *)
val to_builder
  :  ?sw:Eio.Switch.t
  -> ?mgr:[ `Generic | `Unix ] Eio.Process.mgr_ty Eio.Resource.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> agent_file_config
  -> (Builder.t, Error.sdk_error) result
