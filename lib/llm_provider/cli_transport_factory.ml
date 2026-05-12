(** Unified CLI transport factory — RFC-0058 Phase B.

    Protocol-string dispatch over the 4 CLI transport modules:
    - ["anthropic-cli"] → [Transport_claude_code]
    - ["codex-cli"]     → [Transport_codex_cli]
    - ["google-cli"]    → [Transport_gemini_cli]
    - ["kimi-cli"]      → [Transport_kimi_cli]

    Each protocol maps to an existing transport module's [create] function.
    Fields in [cli_config] not consumed by a given protocol are ignored. *)

type cli_config =
  { command : string
  ; model : string option
  ; cwd : string option
  ; mcp_config : string option
  ; mcp_config_files : string list
  ; mcp_config_json : string list
  ; allowed_tools : string list
  ; max_turns : int option
  ; permission_mode : string option
  ; tool_use_via_stream_json : bool option
  ; forward_tool_results : bool option
  ; yolo : bool option
  ; config_file : string option
  ; extra_env : (string * string) list
  ; session_id : string option
  ; cancel : unit Eio.Promise.t option
  ; clock : float Eio.Time.clock_ty Eio.Resource.t option
  ; stdout_idle_timeout_s : float option
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
  ; tool_use_via_stream_json = None
  ; forward_tool_results = None
  ; yolo = None
  ; config_file = None
  ; extra_env = []
  ; session_id = None
  ; cancel = None
  ; clock = None
  ; stdout_idle_timeout_s = None
  }
;;

(* Single source of truth — [registered_protocols], [is_known_protocol],
   and [create]'s match all flow from this list. Adding a new CLI
   protocol means adding here AND wiring a [create] arm; the build
   surfaces the second because [unknown] catch-all is fatal. *)
let supported_protocols = [ "anthropic-cli"; "codex-cli"; "google-cli"; "kimi-cli" ]
let registered_protocols () = List.sort String.compare supported_protocols
let is_known_protocol protocol = List.mem protocol supported_protocols

(* Coalesce caller-supplied option with transport's native default.
   [None] from the caller means "inherit the transport's default" —
   used for fields like [model] where each transport ships a sensible
   default (e.g. Kimi's "kimi-for-coding") that must not be silently
   overwritten by [Cli_transport_factory.default_config]'s [None]. *)
let inherit_or_override caller_value ~transport_default =
  match caller_value with
  | Some _ -> caller_value
  | None -> transport_default
;;

let create ~protocol ~config ~sw ~mgr =
  if not (is_known_protocol protocol)
  then
    failwith
      (Printf.sprintf
         "Cli_transport_factory.create: unknown CLI protocol %S (expected one of: %s)"
         protocol
         (String.concat ", " (registered_protocols ())));
  if String.trim config.command = ""
  then
    failwith
      (Printf.sprintf
         "Cli_transport_factory.create: protocol %S requires a non-empty command"
         protocol);
  match protocol with
  | "anthropic-cli" ->
    let tc =
      { Transport_claude_code.default_config with
        claude_path = config.command
      ; model =
          inherit_or_override
            config.model
            ~transport_default:Transport_claude_code.default_config.model
      ; max_turns = config.max_turns
      ; allowed_tools = config.allowed_tools
      ; permission_mode = config.permission_mode
      ; mcp_config = config.mcp_config
      ; cwd = config.cwd
      ; cancel = config.cancel
      ; clock = config.clock
      ; stdout_idle_timeout_s = config.stdout_idle_timeout_s
      }
    in
    let tc =
      match config.tool_use_via_stream_json with
      | Some v -> { tc with Transport_claude_code.tool_use_via_stream_json = v }
      | None -> tc
    in
    let tc =
      match config.forward_tool_results with
      | Some v -> { tc with Transport_claude_code.forward_tool_results = v }
      | None -> tc
    in
    Transport_claude_code.create ~sw ~mgr ~config:tc
  | "codex-cli" ->
    (* Codex CLI has no protocol-specific bool flags, so the cli_config
       layer can fully specify the record. Listing all fields explicitly
       is the warning-23-clean alternative to a useless [with]. *)
    let tc : Transport_codex_cli.config =
      { codex_path = config.command
      ; model =
          inherit_or_override
            config.model
            ~transport_default:Transport_codex_cli.default_config.model
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
      { Transport_gemini_cli.default_config with
        gemini_path = config.command
      ; model =
          inherit_or_override
            config.model
            ~transport_default:Transport_gemini_cli.default_config.model
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
    let tc =
      match config.yolo with
      | Some v -> { tc with Transport_gemini_cli.yolo = v }
      | None -> tc
    in
    Transport_gemini_cli.create ~sw ~mgr ~config:tc
  | "kimi-cli" ->
    let tc =
      { Transport_kimi_cli.default_config with
        kimi_path = config.command
      ; model =
          inherit_or_override
            config.model
            ~transport_default:Transport_kimi_cli.default_config.model
      ; cwd = config.cwd
      ; config_file = config.config_file
      ; mcp_config_files = config.mcp_config_files
      ; mcp_config_json = config.mcp_config_json
      ; extra_env = config.extra_env
      ; cancel = config.cancel
      ; session_id = config.session_id
      ; clock = config.clock
      ; stdout_idle_timeout_s = config.stdout_idle_timeout_s
      }
    in
    let tc =
      match config.forward_tool_results with
      | Some v -> { tc with Transport_kimi_cli.forward_tool_results = v }
      | None -> tc
    in
    Transport_kimi_cli.create ~sw ~mgr ~config:tc
  | unknown ->
    (* Unreachable: [is_known_protocol] gate above rejects unknown values.
       Kept as defensive arm so [supported_protocols] drift cannot reach
       a partial match. *)
    failwith
      (Printf.sprintf
         "Cli_transport_factory.create: unhandled CLI protocol %S (supported_protocols \
          drift — add a dispatch arm)"
         unknown)
;;
