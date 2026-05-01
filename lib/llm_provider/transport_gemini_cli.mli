(** Gemini CLI non-interactive transport.

    Implements {!Llm_transport.t} by spawning [gemini -p] subprocesses.
    Uses [--output-format json] for structured output.

    @since 0.133.0

    @stability Internal *)

(** Configuration for the Gemini CLI subprocess. *)
type config =
  { gemini_path : string (** Path to the [gemini] executable. Default ["gemini"]. *)
  ; model : string option (** [--model] override. [None] uses the user's default. *)
  ; yolo : bool (** [--yolo] flag disables confirmation prompts. Default [true]. *)
  ; cwd : string option (** Working directory for the subprocess. *)
  ; mcp_config : string option
    (** Accepted for parity with Claude Code; the current [gemini]
        binary has no MCP flag so a set value triggers a one-shot
        [Eio.traceln] warning and is otherwise ignored.

        @since 0.140.0 *)
  ; allowed_tools : string list
    (** Accepted for parity with Claude Code; [gemini] has no
        equivalent whitelist flag.  A non-empty list triggers a
        one-shot warning and is otherwise ignored.

        @since 0.140.0 *)
  ; max_turns : int option
    (** Accepted for parity; no equivalent flag on [gemini].
        @since 0.140.0 *)
  ; permission_mode : string option
    (** Accepted for parity; no equivalent flag on [gemini].
        @since 0.140.0 *)
  ; cancel : unit Eio.Promise.t option
    (** When [Some p] and [p] resolves mid-run, the [gemini]
        subprocess receives [SIGINT] via [Eio.Process.signal].
        Default [None].

        @since 0.148.0 *)
  }

(** Sensible defaults: [gemini] in PATH, yolo enabled, no overrides. *)
val default_config : config

(** Create a Gemini CLI transport.

    The returned {!Llm_transport.t} spawns a fresh [gemini -p] process
    for each completion request. System prompt and messages from the
    {!Llm_transport.completion_request} are mapped to CLI flags.

    Sync mode uses [--output-format json].
    Stream mode runs sync then emits synthetic SSE events.

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create : sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr -> config:config -> Llm_transport.t
