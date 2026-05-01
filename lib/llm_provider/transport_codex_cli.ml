open Base
(** Codex CLI non-interactive transport.

    Uses [codex exec --json] which emits JSONL envelopes on stdout:

    {v
    {"type":"thread.started","thread_id":"..."}
    {"type":"turn.started"}
    {"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"..."}}
    {"type":"item.{started,completed}","item":{"id":"item_1","type":"command_execution","command":"...","aggregated_output":"...","exit_code":0}}
    {"type":"turn.completed","usage":{"input_tokens":N,"cached_input_tokens":N,"output_tokens":N}}
    v}

    [agent_message] items become {!Types.Text} blocks in the aggregate
    response.  [command_execution] items are Codex's internal shell tool
    — transparent to the outer agent, tracked via [Eio.traceln] only.
    The CLI may print raw usage counters, but this transport does not
    surface them as {!Types.api_usage}: Codex CLI is declared
    [emits_usage_tokens = false], and observed counters can be cumulative
    across the CLI thread instead of a single OAS response.

    @since 0.133.0 *)

type config =
  { codex_path : string
  ; model : string option
  ; cwd : string option
  ; mcp_config : string option
  ; allowed_tools : string list
  ; max_turns : int option
  ; permission_mode : string option
  ; cancel : unit Eio.Promise.t option
    (* When [Some p] and [p] resolves mid-run, the [codex]
       subprocess receives [SIGINT].  Default [None]. *)
  }

let default_config =
  { codex_path = "codex"
  ; model = None
  ; cwd = None
  ; mcp_config = None
  ; allowed_tools = []
  ; max_turns = None
  ; permission_mode = None
  ; cancel = None
  }
;;

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules. *)

type stream_state =
  { next_index : int ref
  ; item_indices : (string, int) Hashtbl.t
  ; item_result_indices : (string, int) Hashtbl.t
  }

let create_stream_state () =
  { next_index = ref 0
  ; item_indices = Hashtbl.create 8
  ; item_result_indices = Hashtbl.create 8
  }
;;

let index_for_item (state : stream_state) item_id =
  match Hashtbl.find_opt state.item_indices item_id with
  | Some idx -> idx
  | None ->
    let idx = !(state.next_index) in
    state.next_index := idx + 1;
    Hashtbl.replace state.item_indices item_id idx;
    idx
;;

let result_index_for_item (state : stream_state) item_id =
  match Hashtbl.find_opt state.item_result_indices item_id with
  | Some idx -> idx
  | None ->
    let idx = !(state.next_index) in
    state.next_index := idx + 1;
    Hashtbl.replace state.item_result_indices item_id idx;
    idx
;;

let toml_string value = Yojson.Safe.to_string (`String value)
let toml_string_list values = "[" ^ String.concat "," (List.map toml_string values) ^ "]"

let toml_string_assoc entries =
  let body =
    entries
    |> List.map (fun (key, value) -> Printf.sprintf "%s=%s" key (toml_string value))
    |> String.concat ","
  in
  "{" ^ body ^ "}"
;;

(* Codex CLI 0.125.0+ accepts [mcp_servers.<name>.bearer_token_env_var = "VAR"]
   for HTTP MCP servers, reading the actual Bearer token from the named env
   variable in its own process environment.  This avoids the secret reaching
   argv (visible via [ps eww]).  The transport mints a deterministic env var
   name per server so the override and the env injection stay in sync. *)
let bearer_env_var_name server_name =
  let buf = Buffer.create 32 in
  Buffer.add_string buf "OAS_CODEX_MCP_";
  String.iter
    (fun c ->
       let c = Char.uppercase_ascii c in
       if (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
       then Buffer.add_char buf c
       else Buffer.add_char buf '_')
    server_name;
  Buffer.add_string buf "_BEARER";
  Buffer.contents buf
;;

let is_authorization_header (key, _) =
  String.lowercase_ascii (String.trim key) = "authorization"
;;

let extract_bearer_token value =
  let trimmed = String.trim value in
  let prefix = "bearer " in
  if
    String.length trimmed >= String.length prefix
    && String.lowercase_ascii (String.sub trimmed 0 (String.length prefix)) = prefix
  then (
    let token =
      String.sub
        trimmed
        (String.length prefix)
        (String.length trimmed - String.length prefix)
      |> String.trim
    in
    if token = "" then None else Some token)
  else None
;;

let server_tool_name ~server ~tool = Printf.sprintf "mcp__%s__%s" server tool

let text_of_mcp_result_json json =
  let open Yojson.Safe.Util in
  match json |> member "content" with
  | `List blocks ->
    let texts =
      blocks
      |> List.filter_map (fun block ->
        match block |> member "type" |> to_string_option with
        | Some "text" -> block |> member "text" |> to_string_option
        | _ -> None)
    in
    if texts <> [] then String.concat "\n" texts else Yojson.Safe.to_string json
  | _ -> Yojson.Safe.to_string json
;;

let is_error_mcp_result_json json =
  let open Yojson.Safe.Util in
  json |> member "is_error" |> to_bool_option |> Option.value ~default:false
  || json |> member "isError" |> to_bool_option |> Option.value ~default:false
;;

let content_blocks_of_jsonl lines =
  let blocks = ref [] in
  List.iter
    (fun line ->
       try
         let json = Yojson.Safe.from_string line in
         match Cli_common_json.member_str "type" json with
         | "item.completed" ->
           let item = Yojson.Safe.Util.member "item" json in
           let item_type = Cli_common_json.member_str "type" item in
           if item_type = "agent_message"
           then blocks := Types.Text (Cli_common_json.member_str "text" item) :: !blocks
           else if item_type = "mcp_tool_call"
           then (
             let id = Cli_common_json.member_str "id" item in
             let server = Cli_common_json.member_str "server" item in
             let tool = Cli_common_json.member_str "tool" item in
             let input =
               match Yojson.Safe.Util.member "arguments" item with
               | `Null -> `Assoc []
               | json -> json
             in
             let name = server_tool_name ~server ~tool in
             let result_json =
               match Yojson.Safe.Util.member "result" item with
               | `Null -> None
               | json -> Some json
             in
             let tool_blocks =
               match result_json with
               | Some result_json ->
                 [ Types.ToolUse { id; name; input }
                 ; Types.ToolResult
                     { tool_use_id = id
                     ; content = text_of_mcp_result_json result_json
                     ; is_error = is_error_mcp_result_json result_json
                     ; json = Some result_json
                     }
                 ]
               | None -> [ Types.ToolUse { id; name; input } ]
             in
             blocks := List.rev_append tool_blocks !blocks)
         | _ -> ()
       with
       | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> ())
    lines;
  List.rev !blocks
;;

(* ── stdout recovery on nonzero exit ──────────────────── *)

(** Lightweight structural check used as the [stdout_recovery] hook
    for [Cli_common_subprocess.run_stream_lines].  A successful codex
    run always emits a [turn.completed] envelope on its own line; if
    the captured stdout contains that envelope we treat the run as
    structurally complete even when the process exited nonzero (e.g.
    the codex-cli 0.125.0+ [record_rollout_items] race that surfaces
    [thread <UUID> not found] on stderr after the LLM response is
    already on stdout).  We scan for the marker substring rather than
    parse every line — this hook is consulted only on nonzero exits
    (already a slow path) and the marker does not appear outside the
    JSONL [type] field in codex output. *)
let stdout_contains_turn_completed stdout_str =
  let needle = {|"type":"turn.completed"|} in
  let m = String.length needle
  and h = String.length stdout_str in
  if m > h
  then false
  else (
    let rec scan i =
      if i + m > h
      then false
      else (
        let eq = ref true
        and j = ref 0 in
        while !eq && !j < m do
          if String.unsafe_get stdout_str (i + !j) <> String.unsafe_get needle !j
          then eq := false;
          incr j
        done;
        !eq || scan (i + 1))
    in
    scan 0)
;;

(* ── CLI argument building ───────────────────────────── *)

(** Threshold at which [build_args] stops passing the prompt as a
    positional argv entry and expects the caller to feed it via
    stdin instead.  macOS [ARG_MAX] is ~1 MiB for the combined argv
    + envp block; 512 KiB leaves headroom for env vars and the other
    argv entries.  Env override [OAS_CODEX_PROMPT_ARGV_THRESHOLD]
    accepts an integer byte count for per-host tuning. *)
let default_prompt_argv_threshold = 512 * 1024

let prompt_argv_threshold () =
  match Sys.getenv_opt "OAS_CODEX_PROMPT_ARGV_THRESHOLD" with
  | Some raw ->
    (match int_of_string_opt (String.trim raw) with
     | Some v when v >= 0 -> v
     | _ -> default_prompt_argv_threshold)
  | None -> default_prompt_argv_threshold
;;

let prompt_exceeds_argv_budget prompt = String.length prompt >= prompt_argv_threshold ()

let stdin_for_prompt prompt =
  if prompt_exceeds_argv_budget prompt then Some prompt else None
;;

(* Codex has no dedicated --no-mcp / --no-hooks flags; every runtime
   toggle goes through [-c key=value] TOML overrides (see
   `~/.codex/config.toml` schema).  Non-interactive OAS runs default to
   MCP OFF by injecting [-c mcp_servers={}] before any caller-provided
   overrides.  Additional env vars let callers refine sandbox/profile
   without changing the config record.

   OAS_CODEX_CONFIG   "k=v,k2=v2"                → -c k=v -c k2=v2
   OAS_CODEX_SANDBOX  read-only|workspace-write  → -s <value>
                      |danger-full-access
   OAS_CODEX_PROFILE  <name>                     → -p <name>
   OAS_CODEX_SKIP_GIT 1                          → --skip-git-repo-check

   Unset → no flag added, preserving the pre-0.159 argv.

   NB: the [mcp_config]/[allowed_tools]/[max_turns]/[permission_mode]
   config fields have never had Codex equivalents and are still unused
   here.  Callers wanting those knobs should emit [-c ...] overrides
   via OAS_CODEX_CONFIG. *)
let legacy_env_extra_args () =
  let extras = ref [] in
  let add a = extras := !extras @ a in
  add [ "-c"; "mcp_servers={}" ];
  (match Cli_common_env.kv_pairs "OAS_CODEX_CONFIG" with
   | None | Some [] -> ()
   | Some pairs ->
     List.iter (fun (k, v) -> add [ "-c"; Printf.sprintf "%s=%s" k v ]) pairs);
  (match Cli_common_env.get "OAS_CODEX_SANDBOX" with
   | Some v -> add [ "-s"; v ]
   | None -> ());
  (match Cli_common_env.get "OAS_CODEX_PROFILE" with
   | Some v -> add [ "-p"; v ]
   | None -> ());
  if Cli_common_env.bool "OAS_CODEX_SKIP_GIT" then add [ "--skip-git-repo-check" ];
  !extras
;;

let cli_model_override ~(config : config) ~(req_config : Provider_config.t) =
  match String.trim req_config.model_id |> String.lowercase_ascii with
  | "" | "auto" -> config.model
  | _ -> Some (String.trim req_config.model_id)
;;

(* Returns [Ok (overrides, env_pairs)].  [env_pairs] are name=value pairs that
   the caller must inject into the spawned [codex] process's environment so
   that any [bearer_token_env_var=...] overrides resolve.  When no HTTP MCP
   server carries an [Authorization: Bearer ...] header, [env_pairs] is
   empty and behaviour matches the pre-fix [http_headers=...] path. *)
let runtime_mcp_overrides (policy : Llm_transport.runtime_mcp_policy)
  : (string list * (string * string) list, string) result
  =
  let add_server acc server =
    let name = Llm_transport.runtime_mcp_server_name server in
    match acc, server with
    | (Error _ as e), _ -> e
    | Ok (overrides, env_pairs), Llm_transport.Http_server { url; headers; _ } ->
      let auth_headers, other_headers = List.partition is_authorization_header headers in
      let bearer_extra =
        match auth_headers with
        | [ (_, value) ] ->
          (match extract_bearer_token value with
           | Some token -> Some (bearer_env_var_name name, token)
           | None -> None)
        | _ -> None
      in
      let url_override = Printf.sprintf "mcp_servers.%s.url=%s" name (toml_string url) in
      let bearer_overrides, headers_to_emit, env_pairs' =
        match bearer_extra with
        | Some (env_var, token) ->
          ( [ Printf.sprintf
                "mcp_servers.%s.bearer_token_env_var=%s"
                name
                (toml_string env_var)
            ]
          , other_headers
          , (env_var, token) :: env_pairs )
        | None -> [], headers, env_pairs
      in
      let header_overrides =
        if headers_to_emit = []
        then []
        else
          [ Printf.sprintf
              "mcp_servers.%s.http_headers=%s"
              name
              (toml_string_assoc headers_to_emit)
          ]
      in
      Ok (overrides @ [ url_override ] @ bearer_overrides @ header_overrides, env_pairs')
    | Ok (overrides, env_pairs), Llm_transport.Stdio_server { command; args; env; _ } ->
      Ok
        ( (overrides
           @ [ Printf.sprintf "mcp_servers.%s.command=%s" name (toml_string command)
             ; Printf.sprintf "mcp_servers.%s.args=%s" name (toml_string_list args)
             ]
           @
           if env = []
           then []
           else [ Printf.sprintf "mcp_servers.%s.env=%s" name (toml_string_assoc env) ])
        , env_pairs )
  in
  let server_overrides =
    List.fold_left add_server (Ok ([ "mcp_servers={}" ], [])) policy.servers
  in
  match server_overrides with
  | Error _ as e -> e
  | Ok (overrides, env_pairs) ->
    let server_names =
      match policy.allowed_server_names with
      | [] -> List.map Llm_transport.runtime_mcp_server_name policy.servers
      | names -> names
    in
    let tool_overrides =
      match policy.allowed_tool_names with
      | [] -> []
      | tool_names ->
        List.concat_map
          (fun server_name ->
             List.map
               (fun tool_name ->
                  Printf.sprintf
                    "mcp_servers.%s.tools.%s.approval_mode=%s"
                    server_name
                    tool_name
                    (toml_string "approve"))
               tool_names)
          server_names
    in
    Ok (overrides @ tool_overrides, env_pairs)
;;

let non_mcp_env_extra_args () =
  let extras = ref [] in
  let add a = extras := !extras @ a in
  (match Cli_common_env.get "OAS_CODEX_SANDBOX" with
   | Some v -> add [ "-s"; v ]
   | None -> ());
  (match Cli_common_env.get "OAS_CODEX_PROFILE" with
   | Some v -> add [ "-p"; v ]
   | None -> ());
  if Cli_common_env.bool "OAS_CODEX_SKIP_GIT" then add [ "--skip-git-repo-check" ];
  !extras
;;

let build_args
      ~(config : config)
      ~(req_config : Provider_config.t)
      ?runtime_mcp_policy
      ~prompt
      ()
  =
  let prompt_via_stdin = prompt_exceeds_argv_budget prompt in
  (* Order: exec-level flags come before the positional prompt.  When the
     prompt is too large for argv, pass "-" so Codex reads it from stdin. *)
  let model_args =
    match cli_model_override ~config ~req_config with
    | None -> []
    | Some model -> [ "--model"; model ]
  in
  (* [--ephemeral] suppresses Codex CLI session/rollout persistence.  Without
     it, codex-cli 0.125.0+ records each invocation under
     [$CODEX_HOME/sessions/], and an asynchronous record_rollout_items step
     can race process exit, surfacing
       [ERROR codex_core::session: failed to record rollout items:
        thread <UUID> not found]
     on stderr together with [exit code 1].  OAS' [cli_common_subprocess]
     classifies any nonzero exit as [NetworkError], discarding the otherwise
     valid stdout JSONL.  We never call [codex resume]/[fork] from this
     transport, so persisted sessions are dead weight; opting into ephemeral
     mode removes the race entirely. *)
  let mcp_argv, mcp_extra_env =
    match runtime_mcp_policy with
    | Some policy ->
      (match runtime_mcp_overrides policy with
       | Ok (overrides, env_pairs) ->
         List.concat_map (fun override -> [ "-c"; override ]) overrides, env_pairs
       | Error _ -> [], [])
    | None -> legacy_env_extra_args (), []
  in
  let argv =
    [ config.codex_path; "exec"; "--json"; "--ephemeral" ]
    @ mcp_argv
    @ model_args
    @ (match runtime_mcp_policy with
       | Some (policy : Llm_transport.runtime_mcp_policy) ->
         (match Cli_common_env.get "OAS_CODEX_SANDBOX" with
          | Some _ -> non_mcp_env_extra_args ()
          | None when policy.disable_builtin_tools ->
            [ "-s"; "read-only" ] @ non_mcp_env_extra_args ()
          | None -> non_mcp_env_extra_args ())
       | None -> [])
    @ [ (if prompt_via_stdin then "-" else prompt) ]
  in
  argv, mcp_extra_env
;;

(* ── JSONL envelope parsing ──────────────────────────── *)

(** Extract raw usage from a [turn.completed] envelope. The transport keeps
    this private and does not put it on [api_response.usage] because Codex
    CLI counters are not a stable per-response contract. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    Some
      { Types.input_tokens = Cli_common_json.member_int "input_tokens" u
      ; output_tokens = Cli_common_json.member_int "output_tokens" u
      ; cache_creation_input_tokens = 0
      ; cache_read_input_tokens = Cli_common_json.member_int "cached_input_tokens" u
      ; cost_usd = None
      }
  | _ -> None
;;

(** Parse a single envelope into zero or more OAS sse_events.
    [command_execution] items do not map to an OAS concept — Codex runs
    them transparently — so we emit no event for them. *)
let events_of_line_with_state (state : stream_state) line =
  try
    let json = Yojson.Safe.from_string line in
    let typ = Cli_common_json.member_str "type" json in
    match typ with
    | "thread.started" ->
      let id = Cli_common_json.member_str "thread_id" json in
      [ Types.MessageStart { id; model = "codex"; usage = None } ]
    | "item.started" ->
      let item = Yojson.Safe.Util.member "item" json in
      let item_type = Cli_common_json.member_str "type" item in
      if item_type = "mcp_tool_call"
      then (
        let item_id = Cli_common_json.member_str "id" item in
        let index = index_for_item state item_id in
        let server = Cli_common_json.member_str "server" item in
        let tool = Cli_common_json.member_str "tool" item in
        let tool_name = server_tool_name ~server ~tool in
        let arguments =
          match Yojson.Safe.Util.member "arguments" item with
          | `Null -> "{}"
          | json -> Yojson.Safe.to_string json
        in
        [ Types.ContentBlockStart
            { index
            ; content_type = "tool_use"
            ; tool_id = Some item_id
            ; tool_name = Some tool_name
            }
        ; Types.ContentBlockDelta { index; delta = Types.InputJsonDelta arguments }
        ])
      else []
    | "item.completed" ->
      let item = Yojson.Safe.Util.member "item" json in
      let item_type = Cli_common_json.member_str "type" item in
      if item_type = "agent_message"
      then (
        let item_id = Cli_common_json.member_str "id" item in
        let index = index_for_item state item_id in
        let text = Cli_common_json.member_str "text" item in
        [ Types.ContentBlockStart
            { index; content_type = "text"; tool_id = None; tool_name = None }
        ; Types.ContentBlockDelta { index; delta = Types.TextDelta text }
        ; Types.ContentBlockStop { index }
        ])
      else if item_type = "mcp_tool_call"
      then (
        let item_id = Cli_common_json.member_str "id" item in
        let tool_use_index = index_for_item state item_id in
        let tool_result_events =
          match Yojson.Safe.Util.member "result" item with
          | `Null -> []
          | result_json ->
            let index = result_index_for_item state item_id in
            let content_type =
              if is_error_mcp_result_json result_json
              then "tool_result_error"
              else "tool_result"
            in
            [ Types.ContentBlockStart
                { index; content_type; tool_id = Some item_id; tool_name = None }
            ; Types.ContentBlockDelta
                { index; delta = Types.TextDelta (text_of_mcp_result_json result_json) }
            ; Types.ContentBlockStop { index }
            ]
        in
        Types.ContentBlockStop { index = tool_use_index } :: tool_result_events)
      else [] (* command_execution, etc. — no OAS mapping. *)
    | "turn.completed" ->
      let _raw_usage = parse_usage json in
      [ Types.MessageDelta { stop_reason = Some Types.EndTurn; usage = None }
      ; Types.MessageStop
      ]
    | _ -> []
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []
;;

(** Aggregate JSONL envelopes into an [api_response].  Only
    [agent_message] text contributes to [content]; [thread.started]
    supplies [id]. Raw [turn.completed.usage] is intentionally ignored
    because Codex CLI usage is not a stable per-response contract.
    [command_execution] items are counted as provider-native internal
    actions rather than surfaced as OAS tool calls. *)
let parse_jsonl_result ?(model_id = "codex") lines =
  let thread_id = ref "" in
  let provider_internal_action_count = ref 0 in
  List.iter
    (fun line ->
       try
         let json = Yojson.Safe.from_string line in
         let typ = Cli_common_json.member_str "type" json in
         match typ with
         | "thread.started" -> thread_id := Cli_common_json.member_str "thread_id" json
         | "item.completed" ->
           let item = Yojson.Safe.Util.member "item" json in
           (match Cli_common_json.member_str "type" item with
            | "command_execution" -> incr provider_internal_action_count
            | _ -> ())
         | "turn.completed" ->
           let _raw_usage = parse_usage json in
           ()
         | _ -> ()
       with
       | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> ())
    lines;
  let content = content_blocks_of_jsonl lines in
  let telemetry =
    if !provider_internal_action_count > 0
    then
      Some
        { Types.system_fingerprint = None
        ; timings = None
        ; reasoning_tokens = None
        ; request_latency_ms = 0
        ; peak_memory_gb = None
        ; provider_kind = None
        ; reasoning_effort = None
        ; canonical_model_id = None
        ; effective_context_window = None
        ; provider_internal_action_count = Some !provider_internal_action_count
        }
    else None
  in
  if content = [] && !thread_id = ""
  then
    Error
      (Http_client.NetworkError
         { message = "no events parsed from codex output"; kind = Unknown })
  else
    Ok
      { Types.id = !thread_id
      ; model = model_id
      ; stop_reason = Types.EndTurn
      ; content
      ; usage = None
      ; telemetry
      }
;;

(* ── Transport constructor ───────────────────────────── *)

(* Fires once per transport instance when any Claude-only config field
   is set.  Codex CLI has no flag for these yet, so we warn and drop. *)
let warn_unsupported_once (config : config) warned =
  if !warned
  then ()
  else (
    warned := true;
    let warn field =
      Eio.traceln "[warn] %s is not supported by codex_cli, ignoring" field
    in
    if Option.is_some config.mcp_config then warn "mcp_config";
    if config.allowed_tools <> [] then warn "allowed_tools";
    if Option.is_some config.max_turns then warn "max_turns";
    if Option.is_some config.permission_mode then warn "permission_mode")
;;

(** Strip API-key env so [codex] uses its own session/auth rather than
    inherited [OPENAI_API_KEY].  Mirrors the rationale in
    {!Transport_claude_code.claude_cli_scrub_env}. *)
let codex_cli_scrub_env = [ "OPENAI_API_KEY" ]

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config) : Llm_transport.t =
  let warned = ref false in
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        warn_unsupported_once config warned;
        let messages = Cli_common_prompt.non_system_messages req.messages in
        let system_prompt =
          Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages
        in
        let prompt =
          Cli_common_prompt.prompt_of_messages messages
          |> fun prompt ->
          Cli_common_prompt.prompt_with_system_prompt ~prompt ~system_prompt
        in
        let model_id =
          Option.value
            ~default:"codex"
            (cli_model_override ~config ~req_config:req.config)
        in
        let runtime_mcp_policy_error =
          match req.runtime_mcp_policy with
          | Some policy ->
            (match runtime_mcp_overrides policy with
             | Ok _ -> None
             | Error msg -> Some msg)
          | None -> None
        in
        match runtime_mcp_policy_error with
        | Some msg ->
          { Llm_transport.response =
              Error (Http_client.NetworkError { message = msg; kind = Unknown })
          ; latency_ms = 0
          }
        | None ->
          let argv, mcp_extra_env =
            build_args
              ~config
              ~req_config:req.config
              ?runtime_mcp_policy:req.runtime_mcp_policy
              ~prompt
              ()
          in
          let seen_lines = ref [] in
          let on_line line =
            if String.trim line <> "" then seen_lines := line :: !seen_lines
          in
          (match
             Cli_common_subprocess.run_stream_lines
               ~sw
               ~mgr
               ~name:"codex"
               ~cwd:config.cwd
               ~extra_env:mcp_extra_env
               ~scrub_env:codex_cli_scrub_env
               ?stdin_content:(stdin_for_prompt prompt)
               ~stdout_recovery:stdout_contains_turn_completed
               ~on_line
               ?cancel:config.cancel
               argv
           with
           | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
           | Ok { stdout = _; stderr = _; latency_ms } ->
             let response = parse_jsonl_result ~model_id (List.rev !seen_lines) in
             { Llm_transport.response; latency_ms }))
  ; complete_stream =
      (fun ~on_event (req : Llm_transport.completion_request) ->
        warn_unsupported_once config warned;
        let messages = Cli_common_prompt.non_system_messages req.messages in
        let system_prompt =
          Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages
        in
        let prompt =
          Cli_common_prompt.prompt_of_messages messages
          |> fun prompt ->
          Cli_common_prompt.prompt_with_system_prompt ~prompt ~system_prompt
        in
        let model_id =
          Option.value
            ~default:"codex"
            (cli_model_override ~config ~req_config:req.config)
        in
        let runtime_mcp_policy_error =
          match req.runtime_mcp_policy with
          | Some policy ->
            (match runtime_mcp_overrides policy with
             | Ok _ -> None
             | Error msg -> Some msg)
          | None -> None
        in
        match runtime_mcp_policy_error with
        | Some msg -> Error (Http_client.NetworkError { message = msg; kind = Unknown })
        | None ->
          let argv, mcp_extra_env =
            build_args
              ~config
              ~req_config:req.config
              ?runtime_mcp_policy:req.runtime_mcp_policy
              ~prompt
              ()
          in
          let state = create_stream_state () in
          let seen_lines = ref [] in
          let on_line line =
            if String.trim line <> ""
            then (
              seen_lines := line :: !seen_lines;
              List.iter on_event (events_of_line_with_state state line))
          in
          (match
             Cli_common_subprocess.run_stream_lines
               ~sw
               ~mgr
               ~name:"codex"
               ~cwd:config.cwd
               ~extra_env:mcp_extra_env
               ~scrub_env:codex_cli_scrub_env
               ?stdin_content:(stdin_for_prompt prompt)
               ~stdout_recovery:stdout_contains_turn_completed
               ~on_line
               ?cancel:config.cancel
               argv
           with
           | Error _ as e -> e
           | Ok _ -> parse_jsonl_result ~model_id (List.rev !seen_lines)))
  }
;;

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let codex_req ?(model_id = "auto") () =
  Provider_config.make ~kind:Provider_config.Codex_cli ~model_id ~base_url:"" ()
;;

let%test "default_config codex_path" = default_config.codex_path = "codex"

let%test "default_config parity fields absent" =
  default_config.model = None
  && default_config.mcp_config = None
  && default_config.allowed_tools = []
  && default_config.max_turns = None
  && default_config.permission_mode = None
;;

let%test "build_args includes --json flag" =
  let args, env =
    build_args ~config:default_config ~req_config:(codex_req ()) ~prompt:"hello" ()
  in
  args = [ "codex"; "exec"; "--json"; "--ephemeral"; "-c"; "mcp_servers={}"; "hello" ]
  && env = []
;;

let%test "build_args includes --ephemeral right after --json" =
  (* Regression guard: --ephemeral must precede config overrides and the
     prompt so codex-cli 0.125.0+ skips session/rollout persistence even
     when callers add custom -c flags. *)
  let args, _ =
    build_args ~config:default_config ~req_config:(codex_req ()) ~prompt:"hello" ()
  in
  let rec idx_of x = function
    | [] -> -1
    | hd :: _ when hd = x -> 0
    | _ :: tl ->
      let r = idx_of x tl in
      if r < 0 then r else r + 1
  in
  let json_idx = idx_of "--json" args in
  let ephemeral_idx = idx_of "--ephemeral" args in
  json_idx >= 0 && ephemeral_idx = json_idx + 1
;;

let%test "build_args ignores extra parity fields" =
  let config =
    { default_config with
      mcp_config = Some "/tmp/mcp.json"
    ; allowed_tools = [ "Read" ]
    ; max_turns = Some 5
    ; permission_mode = Some "bypassPermissions"
    }
  in
  let args, _ = build_args ~config ~req_config:(codex_req ()) ~prompt:"hi" () in
  args = [ "codex"; "exec"; "--json"; "--ephemeral"; "-c"; "mcp_servers={}"; "hi" ]
;;

let%test "build_args with requested model" =
  let args, _ =
    build_args
      ~config:default_config
      ~req_config:(codex_req ~model_id:"gpt-5.4" ())
      ~prompt:"hi"
      ()
  in
  args
  = [ "codex"
    ; "exec"
    ; "--json"
    ; "--ephemeral"
    ; "-c"
    ; "mcp_servers={}"
    ; "--model"
    ; "gpt-5.4"
    ; "hi"
    ]
;;

let%test "build_args with config default model for auto request" =
  let config = { default_config with model = Some "gpt-5.2-codex" } in
  let args, _ = build_args ~config ~req_config:(codex_req ()) ~prompt:"hi" () in
  args
  = [ "codex"
    ; "exec"
    ; "--json"
    ; "--ephemeral"
    ; "-c"
    ; "mcp_servers={}"
    ; "--model"
    ; "gpt-5.2-codex"
    ; "hi"
    ]
;;

let%test "prompt_exceeds_argv_budget: small prompt stays in argv" =
  not (prompt_exceeds_argv_budget "hello")
;;

let%test "prompt_exceeds_argv_budget: 1 MiB prompt routes to stdin" =
  prompt_exceeds_argv_budget (String.make (1 * 1024 * 1024) 'x')
;;

let%test "stdin_for_prompt: Some when over budget, None under" =
  let over = String.make (1 * 1024 * 1024) 'x' in
  stdin_for_prompt "hi" = None && stdin_for_prompt over = Some over
;;

let%test "build_args uses stdin sentinel when prompt is too large" =
  let big = String.make (1 * 1024 * 1024) 'x' in
  let args, _ =
    build_args ~config:default_config ~req_config:(codex_req ()) ~prompt:big ()
  in
  (not (List.mem big args)) && List.nth args (List.length args - 1) = "-"
;;

(* ── Bearer-token env-var indirection ────────────────── *)

let%test "bearer_env_var_name uppercases and sanitises" =
  bearer_env_var_name "context7" = "OAS_CODEX_MCP_CONTEXT7_BEARER"
  && bearer_env_var_name "my-server" = "OAS_CODEX_MCP_MY_SERVER_BEARER"
  && bearer_env_var_name "x.y.z" = "OAS_CODEX_MCP_X_Y_Z_BEARER"
;;

let%test "extract_bearer_token strips prefix case-insensitively" =
  extract_bearer_token "Bearer abc123" = Some "abc123"
  && extract_bearer_token "bearer xyz" = Some "xyz"
  && extract_bearer_token "BEARER  spaced  " = Some "spaced"
  && extract_bearer_token "" = None
  && extract_bearer_token "Basic dXNlcg==" = None
  && extract_bearer_token "Bearer " = None
  && extract_bearer_token "Bearer   " = None
;;

let%test "is_authorization_header is case-insensitive" =
  is_authorization_header ("Authorization", "x")
  && is_authorization_header ("authorization", "x")
  && is_authorization_header ("AUTHORIZATION", "x")
  && not (is_authorization_header ("X-API-Key", "x"))
;;

let bearer_policy_with_header header_value =
  let server =
    Llm_transport.Http_server
      { name = "context7"
      ; url = "https://api.example.com/mcp"
      ; headers = [ "Authorization", header_value; "X-Trace", "id123" ]
      }
  in
  { Llm_transport.servers = [ server ]
  ; allowed_server_names = []
  ; allowed_tool_names = []
  ; permission_mode = None
  ; approval_mode = None
  ; strict = false
  ; disable_builtin_tools = true
  }
;;

let%test "runtime_mcp_overrides redirects Bearer to env var indirection" =
  let policy = bearer_policy_with_header "Bearer secret-token" in
  match runtime_mcp_overrides policy with
  | Error _ -> false
  | Ok (overrides, env_pairs) ->
    let env_var = "OAS_CODEX_MCP_CONTEXT7_BEARER" in
    (* Token is in env_pairs, never in overrides string. *)
    env_pairs = [ env_var, "secret-token" ]
    && List.exists
         (fun s ->
            s = Printf.sprintf "mcp_servers.context7.bearer_token_env_var=\"%s\"" env_var)
         overrides
    (* Other headers stay in http_headers (no Authorization). *)
    && List.exists
         (fun s -> s = "mcp_servers.context7.http_headers={X-Trace=\"id123\"}")
         overrides
    (* Argv must NOT leak the literal token. *)
    && not
         (List.exists
            (fun s ->
               try
                 ignore (Str.search_forward (Str.regexp_string "secret-token") s 0);
                 true
               with
               | Not_found -> false)
            overrides)
;;

let%test "runtime_mcp_overrides keeps non-Bearer auth header verbatim" =
  let policy = bearer_policy_with_header "Basic dXNlcjpwYXNz" in
  match runtime_mcp_overrides policy with
  | Error _ -> false
  | Ok (overrides, env_pairs) ->
    env_pairs = []
    && (not
          (List.exists
             (fun s ->
                try
                  ignore
                    (Str.search_forward (Str.regexp_string "bearer_token_env_var") s 0);
                  true
                with
                | Not_found -> false)
             overrides))
    && List.exists
         (fun s ->
            try
              ignore (Str.search_forward (Str.regexp_string "Authorization=\"Basic") s 0);
              true
            with
            | Not_found -> false)
         overrides
;;

let%test "runtime_mcp_overrides without auth header is unchanged" =
  let server =
    Llm_transport.Http_server
      { name = "context7"
      ; url = "https://api.example.com/mcp"
      ; headers = [ "X-API-Key", "k" ]
      }
  in
  let policy =
    { Llm_transport.servers = [ server ]
    ; allowed_server_names = []
    ; allowed_tool_names = []
    ; permission_mode = None
    ; approval_mode = None
    ; strict = false
    ; disable_builtin_tools = true
    }
  in
  match runtime_mcp_overrides policy with
  | Error _ -> false
  | Ok (overrides, env_pairs) ->
    env_pairs = []
    && List.exists
         (fun s -> s = "mcp_servers.context7.http_headers={X-API-Key=\"k\"}")
         overrides
;;

let%test "parse_jsonl_result extracts text + thread_id and suppresses usage" =
  let lines =
    [ {|{"type":"thread.started","thread_id":"abc-123"}|}
    ; {|{"type":"turn.started"}|}
    ; {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hi"}}|}
    ; {|{"type":"turn.completed","usage":{"input_tokens":100,"cached_input_tokens":5,"output_tokens":3}}|}
    ]
  in
  match parse_jsonl_result lines with
  | Ok resp ->
    resp.id = "abc-123"
    && resp.content = [ Types.Text "hi" ]
    && resp.stop_reason = Types.EndTurn
    && resp.usage = None
  | Error _ -> false
;;

let%test "parse_jsonl_result aggregates multiple agent_messages" =
  let lines =
    [ {|{"type":"thread.started","thread_id":"t1"}|}
    ; {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"first"}}|}
    ; {|{"type":"item.completed","item":{"id":"item_2","type":"agent_message","text":"second"}}|}
    ; {|{"type":"turn.completed","usage":{"input_tokens":1,"cached_input_tokens":0,"output_tokens":2}}|}
    ]
  in
  match parse_jsonl_result lines with
  | Ok resp -> resp.content = [ Types.Text "first"; Types.Text "second" ]
  | Error _ -> false
;;

let%test "parse_jsonl_result preserves requested model_id" =
  let lines =
    [ {|{"type":"thread.started","thread_id":"t1"}|}
    ; {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"ok"}}|}
    ]
  in
  match parse_jsonl_result ~model_id:"gpt-5.4" lines with
  | Ok resp -> resp.model = "gpt-5.4"
  | Error _ -> false
;;

let%test "parse_jsonl_result skips command_execution items" =
  let lines =
    [ {|{"type":"thread.started","thread_id":"t"}|}
    ; {|{"type":"item.completed","item":{"id":"item_1","type":"command_execution","command":"ls","exit_code":0}}|}
    ; {|{"type":"item.completed","item":{"id":"item_2","type":"agent_message","text":"done"}}|}
    ]
  in
  match parse_jsonl_result lines with
  | Ok resp ->
    (* command_execution contributes nothing; only agent_message text. *)
    resp.content = [ Types.Text "done" ]
  | Error _ -> false
;;

let%test "parse_jsonl_result empty lines → Error" =
  match parse_jsonl_result [] with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "events_of_line thread.started → MessageStart" =
  let state = create_stream_state () in
  let line = {|{"type":"thread.started","thread_id":"abc"}|} in
  match events_of_line_with_state state line with
  | [ Types.MessageStart { id = "abc"; model = "codex"; _ } ] -> true
  | _ -> false
;;

let%test "events_of_line agent_message → 3 block events" =
  let state = create_stream_state () in
  let line =
    {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hi"}}|}
  in
  match events_of_line_with_state state line with
  | [ Types.ContentBlockStart _; Types.ContentBlockDelta _; Types.ContentBlockStop _ ] ->
    true
  | _ -> false
;;

let%test "events_of_line command_execution → no events" =
  let state = create_stream_state () in
  let line =
    {|{"type":"item.completed","item":{"id":"item_1","type":"command_execution","command":"ls"}}|}
  in
  events_of_line_with_state state line = []
;;

let%test "events_of_line mcp_tool_call completion emits tool_result block" =
  let state = create_stream_state () in
  let started =
    {|{"type":"item.started","item":{"id":"call_1","type":"mcp_tool_call","server":"example","tool":"example_status","arguments":{"verbose":true}}}|}
  in
  let completed =
    {|{"type":"item.completed","item":{"id":"call_1","type":"mcp_tool_call","server":"example","tool":"example_status","result":{"content":[{"type":"text","text":"ok"}],"isError":false}}}|}
  in
  ignore (events_of_line_with_state state started);
  match events_of_line_with_state state completed with
  | [ Types.ContentBlockStop _
    ; Types.ContentBlockStart { content_type = "tool_result"; _ }
    ; Types.ContentBlockDelta { delta = Types.TextDelta "ok"; _ }
    ; Types.ContentBlockStop _
    ] -> true
  | _ -> false
;;

let%test "events_of_line turn.completed → MessageDelta+Stop" =
  let state = create_stream_state () in
  let line =
    {|{"type":"turn.completed","usage":{"input_tokens":10,"cached_input_tokens":0,"output_tokens":5}}|}
  in
  match events_of_line_with_state state line with
  | [ Types.MessageDelta { stop_reason = Some Types.EndTurn; usage = None }
    ; Types.MessageStop
    ] -> true
  | _ -> false
;;

let%test "events_of_line invalid json → []" =
  let state = create_stream_state () in
  events_of_line_with_state state "not json" = []
;;

(* ── env-driven extra args ──────────────────────────── *)

let with_env k v f =
  let prev = Sys.getenv_opt k in
  Unix.putenv k v;
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | None ->
        (try Unix.putenv k "" with
         | _ -> ())
      | Some old -> Unix.putenv k old)
    f
;;

let with_unset k f =
  let prev = Sys.getenv_opt k in
  (try Unix.putenv k "" with
   | _ -> ());
  Fun.protect
    ~finally:(fun () ->
      match prev with
      | None -> ()
      | Some old -> Unix.putenv k old)
    f
;;

let%test "default: argv disables MCP even with no env" =
  with_unset "OAS_CODEX_CONFIG" (fun () ->
    with_unset "OAS_CODEX_SANDBOX" (fun () ->
      with_unset "OAS_CODEX_PROFILE" (fun () ->
        with_unset "OAS_CODEX_SKIP_GIT" (fun () ->
          let args, _ =
            build_args ~config:default_config ~req_config:(codex_req ()) ~prompt:"hi" ()
          in
          args
          = [ "codex"; "exec"; "--json"; "--ephemeral"; "-c"; "mcp_servers={}"; "hi" ]))))
;;

let%test "env: OAS_CODEX_CONFIG emits -c pairs before prompt" =
  with_env "OAS_CODEX_CONFIG" "mcp_servers={},model=o3" (fun () ->
    let args, _ =
      build_args ~config:default_config ~req_config:(codex_req ()) ~prompt:"hi" ()
    in
    (* must emit -c mcp_servers={} and -c model=o3, and prompt stays last *)
    List.mem "-c" args
    && List.mem "mcp_servers={}" args
    && List.mem "model=o3" args
    && List.nth args (List.length args - 1) = "hi")
;;

let%test "env: OAS_CODEX_SANDBOX and OAS_CODEX_SKIP_GIT" =
  with_env "OAS_CODEX_SANDBOX" "read-only" (fun () ->
    with_env "OAS_CODEX_SKIP_GIT" "true" (fun () ->
      let args, _ =
        build_args ~config:default_config ~req_config:(codex_req ()) ~prompt:"hi" ()
      in
      List.mem "-s" args
      && List.mem "read-only" args
      && List.mem "--skip-git-repo-check" args))
;;

let%test "prompt_exceeds_argv_budget: OAS_CODEX_PROMPT_ARGV_THRESHOLD override" =
  with_env "OAS_CODEX_PROMPT_ARGV_THRESHOLD" "100" (fun () ->
    prompt_exceeds_argv_budget (String.make 200 'x')
    && not (prompt_exceeds_argv_budget (String.make 50 'x')))
;;

let%test "build_args runtime MCP wires request-scoped server" =
  let policy =
    { Llm_transport.empty_runtime_mcp_policy with
      servers =
        [ Llm_transport.Http_server
            { name = "example"; url = "http://127.0.0.1:9999/mcp"; headers = [] }
        ]
    ; allowed_server_names = [ "example" ]
    ; allowed_tool_names = [ "example_status" ]
    ; disable_builtin_tools = true
    }
  in
  let args, env =
    build_args
      ~config:default_config
      ~req_config:(codex_req ())
      ~prompt:"hi"
      ~runtime_mcp_policy:policy
      ()
  in
  List.mem "mcp_servers.example.url=\"http://127.0.0.1:9999/mcp\"" args
  && List.mem "mcp_servers.example.tools.example_status.approval_mode=\"approve\"" args
  && List.mem "-s" args
  && List.mem "read-only" args
  && env = []
;;

(* Regression guard: HTTP MCP servers carrying [Authorization: Bearer X] must
   redirect the token through Codex CLI's [bearer_token_env_var] so it never
   appears on the command line ([ps eww] visibility).  Other headers stay in
   [http_headers={...}]. Pre-fix behaviour leaked the token via argv and
   triggered downstream omission policies. *)
let%test "build_args runtime MCP routes Bearer through env var indirection" =
  (* Use a token whose substring does not collide with any keyword in the
     emitted overrides (e.g. ["tok"] would also match [bearer_token_env_var]). *)
  let token = "s3kr9t-VALUE-Q" in
  let policy =
    { Llm_transport.empty_runtime_mcp_policy with
      servers =
        [ Llm_transport.Http_server
            { name = "agent_tools"
            ; url = "http://127.0.0.1:9999/mcp"
            ; headers =
                [ "Authorization", "Bearer " ^ token
                ; "Accept", "application/json, text/event-stream"
                ]
            }
        ]
    ; allowed_server_names = [ "agent_tools" ]
    }
  in
  let args, env =
    build_args
      ~config:default_config
      ~req_config:(codex_req ())
      ~prompt:"hi"
      ~runtime_mcp_policy:policy
      ()
  in
  let token_in s =
    let nl = String.length token in
    let hl = String.length s in
    if nl > hl
    then false
    else (
      let rec aux i =
        if i + nl > hl
        then false
        else if String.sub s i nl = token
        then true
        else aux (i + 1)
      in
      aux 0)
  in
  List.mem "mcp_servers.agent_tools.url=\"http://127.0.0.1:9999/mcp\"" args
  && List.mem
       "mcp_servers.agent_tools.bearer_token_env_var=\"OAS_CODEX_MCP_AGENT_TOOLS_BEARER\""
       args
  (* Authorization stripped from http_headers; only Accept remains. *)
  && List.mem
       "mcp_servers.agent_tools.http_headers={Accept=\"application/json, \
        text/event-stream\"}"
       args
  (* Token never appears as substring of any argv element. *)
  && (not (List.exists token_in args))
  && env = [ "OAS_CODEX_MCP_AGENT_TOOLS_BEARER", token ]
;;

let%test "parse_jsonl_result includes mcp tool call blocks" =
  let lines =
    [ {|{"type":"thread.started","thread_id":"abc-123"}|}
    ; {|{"type":"item.completed","item":{"id":"call_1","type":"mcp_tool_call","server":"example","tool":"example_status","arguments":{"verbose":true},"result":{"content":[{"type":"text","text":"ok"}],"isError":false}}}|}
    ; {|{"type":"item.completed","item":{"id":"item_2","type":"agent_message","text":"done"}}|}
    ]
  in
  match parse_jsonl_result lines with
  | Ok resp ->
    (match resp.content with
     | [ Types.ToolUse { name = "mcp__example__example_status"; _ }
       ; Types.ToolResult { tool_use_id = "call_1"; content = "ok"; _ }
       ; Types.Text "done"
       ] -> true
     | _ -> false)
  | Error _ -> false
;;

(* ── stdout_recovery validator tests ─────────────────── *)

let%test "stdout_contains_turn_completed: full JSONL stream is recovered" =
  let stdout =
    {|{"type":"thread.started","thread_id":"019dc578-02d3"}
{"type":"turn.started"}
{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hi"}}
{"type":"turn.completed","usage":{"input_tokens":1,"cached_input_tokens":0,"output_tokens":1}}
|}
  in
  stdout_contains_turn_completed stdout
;;

let%test "stdout_contains_turn_completed: incomplete stream is rejected" =
  let stdout =
    {|{"type":"thread.started","thread_id":"019dc578-02d3"}
{"type":"turn.started"}
|}
  in
  not (stdout_contains_turn_completed stdout)
;;

let%test "stdout_contains_turn_completed: empty stdout is rejected" =
  not (stdout_contains_turn_completed "")
;;

let%test "stdout_contains_turn_completed: short stdout below needle length" =
  not (stdout_contains_turn_completed "x")
;;
