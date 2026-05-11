(** Unified CLI transport factory — RFC-0058 Phase B.

    Protocol-string dispatch over the 4 CLI transport modules:
    - ["anthropic-cli"] → [Transport_claude_code]
    - ["codex-cli"]     → [Transport_codex_cli]
    - ["google-cli"]    → [Transport_gemini_cli]
    - ["kimi-cli"]      → [Transport_kimi_cli]

    Each protocol maps to an existing transport module's [create] function.
    Fields in [cli_config] not used by a given protocol are silently ignored.

    HTTP-only transports ([transport_openai_compat]) are NOT part of this
    factory — they are constructed directly from HTTP parameters. *)

(** Superset config for all CLI transports.

    Fields not used by a given protocol are ignored during construction.
    This avoids per-protocol config types while keeping the mapping explicit
    in the factory implementation. *)
type cli_config = {
  command : string;
  model : string option;
  cwd : string option;
  (* MCP config — shared by claude, codex, gemini (single file path) *)
  mcp_config : string option;
  (* MCP config — kimi-specific (lists of file paths / JSON strings) *)
  mcp_config_files : string list;
  mcp_config_json : string list;
  (* Tool config *)
  allowed_tools : string list;
  max_turns : int option;
  permission_mode : string option;
  (* Protocol-specific flags *)
  tool_use_via_stream_json : bool;   (** [anthropic-cli] only *)
  forward_tool_results : bool;       (** [anthropic-cli], [kimi-cli] *)
  yolo : bool;                       (** [google-cli] only *)
  config_file : string option;       (** [kimi-cli] only *)
  extra_env : (string * string) list; (** [kimi-cli] only *)
  session_id : string option;        (** [kimi-cli] only *)
  (* Infrastructure *)
  cancel : unit Eio.Promise.t option;
  clock : float Eio.Time.clock_ty Eio.Resource.t option;
  stdout_idle_timeout_s : float option;
}

val default_config : cli_config
(** All fields at safe defaults: empty lists, [None] options, [false] bools.
    [command] defaults to [""] — callers must override. *)

val create :
  protocol:string ->
  config:cli_config ->
  sw:Eio.Switch.t ->
  mgr:_ Eio.Process.mgr ->
  Llm_transport.t
(** Construct a CLI [Llm_transport.t] by dispatching on [protocol].

    @raise Failure if [protocol] has no registered CLI transport. *)

val is_known_protocol : string -> bool
(** [true] when [protocol] maps to a registered CLI transport module. *)

val registered_protocols : unit -> string list
(** All registered protocol strings, sorted alphabetically. *)
