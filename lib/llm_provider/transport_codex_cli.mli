(** Codex CLI non-interactive transport.

    Implements {!Llm_transport.t} by spawning [codex exec] subprocesses.
    Codex outputs raw text (not JSON). Token usage is extracted from
    the trailing output lines when available.

    @since 0.133.0

    @stability Internal *)

(** Configuration for the Codex CLI subprocess. *)
type config = {
  codex_path: string;
    (** Path to the [codex] executable. Default ["codex"]. *)
  cwd: string option;
    (** Working directory for the subprocess. *)
  mcp_config: string option;
    (** Accepted for parity with Claude Code; the current [codex]
        binary has no MCP flag so a set value triggers a one-shot
        [Eio.traceln] warning and is otherwise ignored.

        @since 0.140.0 *)
  allowed_tools: string list;
    (** Accepted for parity with Claude Code; [codex] has no
        equivalent whitelist flag.  A non-empty list triggers a
        one-shot warning and is otherwise ignored.

        @since 0.140.0 *)
  max_turns: int option;
    (** Accepted for parity; no equivalent flag on [codex].
        @since 0.140.0 *)
  permission_mode: string option;
    (** Accepted for parity; no equivalent flag on [codex].
        @since 0.140.0 *)
}

(** Sensible defaults: [codex] in PATH, no overrides. *)
val default_config : config

(** Create a Codex CLI transport.

    The returned {!Llm_transport.t} spawns a fresh [codex exec] process
    for each completion request. Messages are flattened into a single
    prompt string passed as the exec argument.

    Stream mode runs sync then emits synthetic SSE events.

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create :
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  config:config ->
  Llm_transport.t
