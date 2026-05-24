(** Provider_f CLI non-interactive transport.

    Implements {!Llm_transport.t} by spawning [provider_f -p] subprocesses.
    Uses [--output-format json] for structured output.

    @since 0.133.0

    @stability Internal *)

(** Configuration for the Provider_f CLI subprocess. *)
type config =
  { provider_f_path : string
    (** Path to the [provider_f] executable. Default ["provider_f"]. *)
  ; model : string option (** [--model] override. [None] uses the user's default. *)
  ; yolo : bool (** [--yolo] flag disables confirmation prompts. Default [true]. *)
  ; cwd : string option (** Working directory for the subprocess. *)
  ; mcp_config : string option
    (** Accepted for parity with Agent_llm_a Code; the current [provider_f]
        binary has no MCP flag so a set value triggers a one-shot
        [Eio.traceln] warning and is otherwise ignored.

        @since 0.140.0 *)
  ; allowed_tools : string list
    (** Accepted for parity with Agent_llm_a Code; [provider_f] has no
        equivalent whitelist flag.  A non-empty list triggers a
        one-shot warning and is otherwise ignored.

        @since 0.140.0 *)
  ; max_turns : int option
    (** Accepted for parity; no equivalent flag on [provider_f].
        @since 0.140.0 *)
  ; permission_mode : string option
    (** Accepted for parity; no equivalent flag on [provider_f].
        @since 0.140.0 *)
  ; cancel : unit Eio.Promise.t option
    (** When [Some p] and [p] resolves mid-run, the [provider_f]
        subprocess receives [SIGINT] via [Eio.Process.signal].
        Default [None].

        @since 0.148.0 *)
  ; clock : float Eio.Time.clock_ty Eio.Resource.t option
    (** Optional Eio clock used together with
        [stdout_idle_timeout_s] to bound subprocess silence.
        Both must be [Some _] for the idle bound to engage —
        see {!Cli_common_subprocess.run_collect}.

        Default [None].

        @since 0.191.0 *)
  ; stdout_idle_timeout_s : float option
    (** When [Some s] and [clock] is [Some _], the [provider_f]
        subprocess is aborted via [SIGINT] if no stdout line
        arrives within [s] seconds.  Mirrors the provider_c-cli idle
        bound (see downstream coordinator #13894,
        RFC-0022 attempt liveness) and oas PRs #1458
        (cli_tool_a) / #1459 (cli_tool_d) / #1460 (cli_tool_c).

        Default [None].

        @since 0.191.0 *)
  }

(** Sensible defaults: [provider_f] in PATH, yolo enabled, no overrides. *)
val default_config : config

(** Create a Provider_f CLI transport.

    The returned {!Llm_transport.t} spawns a fresh [provider_f -p] process
    for each completion request. System prompt and messages from the
    {!Llm_transport.completion_request} are mapped to CLI flags.

    Sync mode uses [--output-format json].
    Stream mode runs sync then emits synthetic SSE events.

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create : sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr -> config:config -> Llm_transport.t
