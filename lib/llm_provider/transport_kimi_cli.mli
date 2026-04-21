(** Kimi CLI non-interactive transport.

    Implements {!Llm_transport.t} by spawning [kimi --print]
    subprocesses. Uses [--output-format stream-json] so tool calls and
    tool results survive as structured content blocks.

    @since 0.169.0

    @stability Internal *)

(** Configuration for the Kimi CLI subprocess. *)
type config = {
  kimi_path: string;
    (** Path to the [kimi] executable. Default ["kimi"]. *)
  model: string option;
    (** [--model] override. [None] uses the CLI-configured default.
        Default is [Some "kimi-for-coding"] so the coding plan model is
        selected unless the request provides a concrete model id. *)
  cwd: string option;
    (** Working directory for the subprocess. *)
  mcp_config_files: string list;
    (** Additional [--mcp-config-file] paths. Empty preserves the CLI's
        default MCP discovery (including [~/.kimi/mcp.json] when it
        exists). *)
  mcp_config_json: string list;
    (** Additional [--mcp-config] JSON strings. Empty means no extra
        inline MCP config is injected. *)
  forward_tool_results: bool;
    (** When [true] (default), prior [ToolUse]/[ToolResult] content
        blocks in the conversation history are flattened back into the
        prompt so later turns keep the provider-native tool trace. *)
  cancel: unit Eio.Promise.t option;
    (** When [Some p] and [p] resolves mid-run, the [kimi] subprocess
        receives [SIGINT] via [Eio.Process.signal].
        Default [None]. *)
}

(** Sensible defaults: [kimi] in PATH, [kimi-for-coding], no explicit
    MCP overrides. *)
val default_config : config

(** Create a Kimi CLI transport.

    The returned {!Llm_transport.t} spawns a fresh [kimi --print]
    process for each completion request. System prompt and messages from
    the {!Llm_transport.completion_request} are flattened into a single
    prompt string; Kimi's stream-JSON output is then re-expanded into
    {!Types.Text}, {!Types.ToolUse}, and {!Types.ToolResult} blocks.

    External OAS tool schemas in [req.tools] are not bridged through
    print mode; provider-native built-in tools and configured MCP
    servers remain available. Full external-tool callback support would
    require a future Wire-mode transport.

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  config:config ->
  Llm_transport.t
