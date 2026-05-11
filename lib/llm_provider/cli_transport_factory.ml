(** Unified CLI transport factory — RFC-0058 Phase B.

    Protocol-string dispatch over the 4 CLI transport modules:
    - ["anthropic-cli"] → [Transport_claude_code]
    - ["codex-cli"]     → [Transport_codex_cli]
    - ["google-cli"]    → [Transport_gemini_cli]
    - ["kimi-cli"]      → [Transport_kimi_cli]

    Each protocol maps to an existing transport module's [create] function.
    Fields in [cli_config] not used by a given protocol are silently ignored. *)

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

let default_config =
  { command = ""
  ; model = None
  ; cwd = None
  ; mcp_config = None
  ; mcp_config_files = []
  ; mcp_config_json = []
  ; allowed_tools = []
  ; max_turns = None
  ; permission_mode = None
  ; tool_use_via_stream_json = false
  ; forward_tool_results = false
  ; yolo = false
  ; config_file = None
  ; extra_env = []
  ; session_id = None
  ; cancel = None
  ; clock = None
  ; stdout_idle_timeout_s = None
  }
;;

let registered_protocols () =
  List.sort String.compare [ "anthropic-cli"; "codex-cli"; "google-cli"; "kimi-cli" ]
;;

let is_known_protocol protocol =
  match protocol with
  | "anthropic-cli" | "codex-cli" | "google-cli" | "kimi-cli" -> true
  | _ -> false
;;

let create ~protocol ~config ~sw ~mgr =
  match protocol with
  | "anthropic-cli" ->
    let tc =
      { Transport_claude_code.claude_path = config.command
      ; model = config.model
      ; max_turns = config.max_turns
      ; allowed_tools = config.allowed_tools
      ; permission_mode = config.permission_mode
      ; mcp_config = config.mcp_config
      ; cwd = config.cwd
      ; tool_use_via_stream_json = config.tool_use_via_stream_json
      ; forward_tool_results = config.forward_tool_results
      ; cancel = config.cancel
      ; clock = config.clock
      ; stdout_idle_timeout_s = config.stdout_idle_timeout_s
      }
    in
    Transport_claude_code.create ~sw ~mgr ~config:tc
  | "codex-cli" ->
    let tc =
      { Transport_codex_cli.codex_path = config.command
      ; model = config.model
      ; cwd = config.cwd
      ; mcp_config = config.mcp_config
      ; allowed_tools = config.allowed_tools
      ; max_turns = config.max_turns
      ; permission_mode = config.permission_mode
      ; cancel = config.cancel
      ; clock = config.clock
      ; stdout_idle_timeout_s = config.stdout_idle_timeout_s
      }
    in
    Transport_codex_cli.create ~sw ~mgr ~config:tc
  | "google-cli" ->
    let tc =
      { Transport_gemini_cli.gemini_path = config.command
      ; model = config.model
      ; yolo = config.yolo
      ; cwd = config.cwd
      ; mcp_config = config.mcp_config
      ; allowed_tools = config.allowed_tools
      ; max_turns = config.max_turns
      ; permission_mode = config.permission_mode
      ; cancel = config.cancel
      ; clock = config.clock
      ; stdout_idle_timeout_s = config.stdout_idle_timeout_s
      }
    in
    Transport_gemini_cli.create ~sw ~mgr ~config:tc
  | "kimi-cli" ->
    let tc =
      { Transport_kimi_cli.kimi_path = config.command
      ; model = config.model
      ; cwd = config.cwd
      ; config_file = config.config_file
      ; mcp_config_files = config.mcp_config_files
      ; mcp_config_json = config.mcp_config_json
      ; forward_tool_results = config.forward_tool_results
      ; extra_env = config.extra_env
      ; cancel = config.cancel
      ; session_id = config.session_id
      ; clock = config.clock
      ; stdout_idle_timeout_s = config.stdout_idle_timeout_s
      }
    in
    Transport_kimi_cli.create ~sw ~mgr ~config:tc
  | unknown ->
    failwith
      (Printf.sprintf
         "Cli_transport_factory: unknown CLI protocol %S (expected one of: %s)"
         unknown
         (String.concat ", " (registered_protocols ())))
;;
