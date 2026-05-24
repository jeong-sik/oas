(** Agent_llm_a Code non-interactive transport.

    Implements {!Llm_transport.t} by spawning [agent_llm_a -p] subprocesses.
    No API key required -- uses the user's OAuth authentication.

    Reuses the {!Eio.Process} pattern from {!Mcp.connect}.

    @since 0.78.0

    @stability Internal
    @since 0.93.1 *)

(** Configuration for the Agent_llm_a Code subprocess. *)
type config =
  { agent_llm_a_path : string
    (** Path to the [agent_llm_a] executable. Default ["agent_llm_a"]. *)
  ; model : string option (** [--model] override. [None] uses the user's default. *)
  ; max_turns : int option
    (** [--max-turns] limit. [None] uses the default (single turn). *)
  ; allowed_tools : string list (** [--allowedTools] whitelist. Empty = no tools. *)
  ; permission_mode : string option (** [--permission-mode] (e.g. "bypassPermissions"). *)
  ; mcp_config : string option (** [--mcp-config] path. *)
  ; cwd : string option (** Working directory for the subprocess. *)
  ; tool_use_via_stream_json : bool
    (** When [true] (default), [complete_sync] internally uses
        [--output-format stream-json] and aggregates the assistant
        content blocks so [tool_use] / [thinking] survive in the
        returned {!Types.api_response.content}.  The plain
        [--output-format json] flattens content into a single
        [result] string and drops structured blocks.

        @since 0.140.0 *)
  ; forward_tool_results : bool
    (** When [true], prior [ToolUse]/[ToolResult] content blocks in
        the conversation history are flattened into the CLI prompt so
        the next turn sees the tool exchange.  Default [false] — the
        OAS agent loop typically resolves tools itself and feeds only
        fresh text to the CLI.

        @since 0.146.0 *)
  ; cancel : unit Eio.Promise.t option
    (** When [Some p] and [p] resolves mid-run, the [agent_llm_a]
        subprocess receives [SIGINT] via [Eio.Process.signal].
        Applied to every call served by this transport instance.
        Default [None].

        @since 0.148.0 *)
  ; clock : float Eio.Time.clock_ty Eio.Resource.t option
    (** Optional Eio clock used together with
        [stdout_idle_timeout_s] to bound subprocess silence.
        Both must be [Some _] for the idle bound to engage —
        see {!Cli_common_subprocess.run_collect} and
        {!Cli_common_subprocess.run_stream_lines}.

        Default [None].

        @since 0.191.0 *)
  ; stdout_idle_timeout_s : float option
    (** When [Some s] and [clock] is [Some _], the [agent_llm_a]
        subprocess is aborted via [SIGINT] if no stdout line
        arrives within [s] seconds.  Mirrors the provider_c-cli idle
        bound introduced for long-running coordinator turns
        (see downstream coordinator #13894 for original RFC-0022
        attempt-liveness context); the same field is wired here so
        OAS callers can opt-in.

        Default [None].

        @since 0.191.0 *)
  }

(** Sensible defaults: [agent_llm_a] in PATH, no overrides. *)
val default_config : config

(** Create a Agent_llm_a Code transport.

    The returned {!Llm_transport.t} spawns a fresh [agent_llm_a -p] process
    for each completion request. System prompt and messages from the
    {!Llm_transport.completion_request} are mapped to CLI flags.

    Sync mode uses [--output-format json].
    Stream mode uses [--output-format stream-json --verbose].

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create : sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr -> config:config -> Llm_transport.t
