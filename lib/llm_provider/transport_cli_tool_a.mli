(** Agent_code CLI non-interactive transport.

    Implements {!Llm_transport.t} by spawning [agent_code exec] subprocesses.
    Agent_code outputs JSONL envelopes. Text and tool events are projected into
    OAS response/content events; raw CLI usage counters are intentionally
    not surfaced because this provider is declared [emits_usage_tokens=false]
    and the counters can be cumulative rather than per-response.

    @since 0.133.0

    @stability Internal *)

(** Configuration for the Agent_code CLI subprocess. *)
type config =
  { agent_code_path : string
    (** Path to the [agent_code] executable. Default ["agent_code"]. *)
  ; model : string option
    (** [--model] override. [None] uses the user's Agent_code CLI default.

        @since 0.164.0 *)
  ; cwd : string option (** Working directory for the subprocess. *)
  ; mcp_config : string option
    (** Accepted for parity with Agent_llm_a Code; the current [agent_code]
        binary has no MCP flag so a set value triggers a one-shot
        [Eio.traceln] warning and is otherwise ignored.

        @since 0.140.0 *)
  ; allowed_tools : string list
    (** Accepted for parity with Agent_llm_a Code; [agent_code] has no
        equivalent whitelist flag.  A non-empty list triggers a
        one-shot warning and is otherwise ignored.

        @since 0.140.0 *)
  ; max_turns : int option
    (** Accepted for parity; no equivalent flag on [agent_code].
        @since 0.140.0 *)
  ; permission_mode : string option
    (** Accepted for parity; no equivalent flag on [agent_code].
        @since 0.140.0 *)
  ; cancel : unit Eio.Promise.t option
    (** When [Some p] and [p] resolves mid-run, the [agent_code]
        subprocess receives [SIGINT] via [Eio.Process.signal].
        Default [None].

        @since 0.148.0 *)
  ; clock : float Eio.Time.clock_ty Eio.Resource.t option
    (** Optional Eio clock used together with
        [stdout_idle_timeout_s] to bound subprocess silence.
        Both must be [Some _] for the idle bound to engage —
        see {!Cli_common_subprocess.run_stream_lines}.

        Default [None].

        @since 0.191.0 *)
  ; stdout_idle_timeout_s : float option
    (** When [Some s] and [clock] is [Some _], the [agent_code]
        subprocess is aborted via [SIGINT] if no stdout line
        arrives within [s] seconds.  Mirrors the provider_c-cli idle
        bound introduced for long-running coordinator turns
        (see downstream coordinator #13894 for original RFC-0022
        attempt-liveness context); the same field is wired here so
        OAS callers can opt-in.

        Default [None].

        @since 0.191.0 *)
  }

(** Sensible defaults: [agent_code] in PATH, no overrides. *)
val default_config : config

(** Create a Agent_code CLI transport.

    The returned {!Llm_transport.t} spawns a fresh [agent_code exec] process
    for each completion request. Messages are flattened into a single
    prompt string passed as the exec argument.

    Stream mode runs sync then emits synthetic SSE events.

    @param sw Eio switch controlling subprocess lifetime.
    @param mgr Eio process manager for spawning. *)
val create : sw:Eio.Switch.t -> mgr:_ Eio.Process.mgr -> config:config -> Llm_transport.t
