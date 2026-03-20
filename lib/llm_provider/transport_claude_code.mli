(** Claude Code non-interactive transport.

    Implements {!Llm_transport.t} by spawning [claude -p] subprocesses.
    No API key required -- uses the user's OAuth authentication.

    Reuses the {!Eio.Process} pattern from {!Mcp.connect}.

    @since 0.78.0 *)

(** Configuration for the Claude Code subprocess. *)
type config = {
  claude_path: string;
    (** Path to the [claude] executable. Default ["claude"]. *)
  model: string option;
    (** [--model] override. [None] uses the user's default. *)
  max_turns: int option;
    (** [--max-turns] limit. [None] uses the default (single turn). *)
  allowed_tools: string list;
    (** [--allowedTools] whitelist. Empty = no tools. *)
  permission_mode: string option;
    (** [--permission-mode] (e.g. "bypassPermissions"). *)
  mcp_config: string option;
    (** [--mcp-config] path. *)
  cwd: string option;
    (** Working directory for the subprocess. *)
}

(** Sensible defaults: [claude] in PATH, no overrides. *)
val default_config : config

(** Create a Claude Code transport.

    The returned {!Llm_transport.t} spawns a fresh [claude -p] process
    for each completion request. System prompt and messages from the
    {!Llm_transport.completion_request} are mapped to CLI flags.

    Sync mode uses [--output-format json].
    Stream mode uses [--output-format stream-json --verbose].

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  config:config ->
  Llm_transport.t
