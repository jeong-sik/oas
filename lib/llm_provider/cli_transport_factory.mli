(** Unified CLI transport factory — RFC-0058 Phase B.

    @stability Internal

    Protocol-string dispatch over the 4 CLI transport modules:
    - ["anthropic-cli"] → [Transport_claude_code]
    - ["codex-cli"]     → [Transport_codex_cli]
    - ["google-cli"]    → [Transport_gemini_cli]
    - ["kimi-cli"]      → [Transport_kimi_cli]

    Each protocol maps to an existing transport module's [create] function.
    Fields in [cli_config] not consumed by a given protocol are ignored;
    some underlying transports (e.g. codex-cli, gemini-cli) emit one-shot
    runtime warnings when they observe parity fields they cannot honor,
    so "ignored" here means "no effect on the request" rather than silent.

    HTTP-only transports ([transport_openai_compat]) are NOT part of this
    factory — they are constructed directly from HTTP parameters. *)

(** Superset config for all CLI transports.

    Fields not consumed by a given protocol are ignored during construction.
    Protocol-specific boolean flags are [bool option]: [None] means
    "inherit the underlying transport's native default" (so the factory
    cannot silently flip a default to [false]); [Some b] is an explicit
    override. *)
type cli_config =
  { command : string
  ; model : string option
  ; cwd : string option
  ; (* MCP config — shared by claude, codex, gemini (single file path) *)
    mcp_config : string option
  ; (* MCP config — kimi-specific (lists of file paths / JSON strings) *)
    mcp_config_files : string list
  ; mcp_config_json : string list
  ; (* Tool config *)
    allowed_tools : string list
  ; max_turns : int option
  ; permission_mode : string option
  ; (* Protocol-specific flags. [None] = use the transport's native default. *)
    tool_use_via_stream_json : bool option (** [anthropic-cli] only *)
  ; forward_tool_results : bool option (** [anthropic-cli], [kimi-cli] *)
  ; yolo : bool option (** [google-cli] only *)
  ; config_file : string option (** [kimi-cli] only *)
  ; extra_env : (string * string) list (** [kimi-cli] only *)
  ; session_id : string option (** [kimi-cli] only *)
  ; (* Infrastructure *)
    cancel : unit Eio.Promise.t option
  ; clock : float Eio.Time.clock_ty Eio.Resource.t option
  ; stdout_idle_timeout_s : float option
  }

(** All fields at safe defaults: empty lists, [None] options, [None] for
    protocol-specific bool flags. [command] defaults to [""] — callers
    must override; [create] rejects an empty [command] up-front. *)
val default_config : cli_config

(** Construct a CLI [Llm_transport.t] by dispatching on [protocol].

    Internally starts from the target transport's [default_config] and
    overrides only the fields the caller set, so a [cli_config] with
    [tool_use_via_stream_json = None] preserves the underlying
    transport's native default rather than forcing [false].

    @raise Failure if [protocol] is unknown or [config.command] is empty. *)
val create
  :  protocol:string
  -> config:cli_config
  -> sw:Eio.Switch.t
  -> mgr:_ Eio.Process.mgr
  -> Llm_transport.t

(** [true] when [protocol] maps to a registered CLI transport module. *)
val is_known_protocol : string -> bool

(** All registered protocol strings, sorted alphabetically. *)
val registered_protocols : unit -> string list
