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

    @since 0.133.0 *)

type config = {
  codex_path: string;
  cwd: string option;
  mcp_config: string option;
  allowed_tools: string list;
  max_turns: int option;
  permission_mode: string option;
  cancel: unit Eio.Promise.t option;
    (* When [Some p] and [p] resolves mid-run, the [codex]
       subprocess receives [SIGINT].  Default [None]. *)
}

let default_config = {
  codex_path = "codex";
  cwd = None;
  mcp_config = None;
  allowed_tools = [];
  max_turns = None;
  permission_mode = None;
  cancel = None;
}

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules. *)

(* ── CLI argument building ───────────────────────────── *)

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
let env_extra_args () =
  let extras = ref [] in
  let add a = extras := !extras @ a in
  add ["-c"; "mcp_servers={}"];
  (match Cli_common_env.kv_pairs "OAS_CODEX_CONFIG" with
   | None | Some [] -> ()
   | Some pairs ->
     List.iter (fun (k, v) -> add ["-c"; Printf.sprintf "%s=%s" k v]) pairs);
  (match Cli_common_env.get "OAS_CODEX_SANDBOX" with
   | Some v -> add ["-s"; v]
   | None -> ());
  (match Cli_common_env.get "OAS_CODEX_PROFILE" with
   | Some v -> add ["-p"; v]
   | None -> ());
  if Cli_common_env.bool "OAS_CODEX_SKIP_GIT" then
    add ["--skip-git-repo-check"];
  !extras

let build_args ~(config : config) ~prompt =
  (* Order: exec-level flags come before the positional prompt. *)
  [config.codex_path; "exec"; "--json"]
  @ env_extra_args ()
  @ [prompt]

(* ── JSONL envelope parsing ──────────────────────────── *)

(** Extract usage from a [turn.completed] envelope. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    Some { Types.input_tokens = Cli_common_json.member_int "input_tokens" u;
           output_tokens = Cli_common_json.member_int "output_tokens" u;
           cache_creation_input_tokens = 0;
           cache_read_input_tokens =
             Cli_common_json.member_int "cached_input_tokens" u;
           cost_usd = None }
  | _ -> None

(** Parse a single envelope into zero or more OAS sse_events.
    [command_execution] items do not map to an OAS concept — Codex runs
    them transparently — so we emit no event for them. *)
let events_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let typ = Cli_common_json.member_str "type" json in
    match typ with
    | "thread.started" ->
      let id = Cli_common_json.member_str "thread_id" json in
      [Types.MessageStart { id; model = "codex"; usage = None }]
    | "item.completed" ->
      let item = Yojson.Safe.Util.member "item" json in
      let item_type = Cli_common_json.member_str "type" item in
      if item_type = "agent_message" then
        let text = Cli_common_json.member_str "text" item in
        [Types.ContentBlockStart {
           index = 0; content_type = "text";
           tool_id = None; tool_name = None };
         Types.ContentBlockDelta { index = 0; delta = Types.TextDelta text };
         Types.ContentBlockStop { index = 0 }]
      else []  (* command_execution, etc. — no OAS mapping. *)
    | "turn.completed" ->
      let usage = parse_usage json in
      [Types.MessageDelta { stop_reason = Some Types.EndTurn; usage };
       Types.MessageStop]
    | _ -> []
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []

(** Aggregate JSONL envelopes into an [api_response].  Only
    [agent_message] text contributes to [content]; the terminal
    [turn.completed] supplies [usage]; [thread.started] supplies [id]. *)
let parse_jsonl_result lines =
  let thread_id = ref "" in
  let texts = ref [] in
  let usage = ref None in
  List.iter (fun line ->
    try
      let json = Yojson.Safe.from_string line in
      let typ = Cli_common_json.member_str "type" json in
      match typ with
      | "thread.started" ->
        thread_id := Cli_common_json.member_str "thread_id" json
      | "item.completed" ->
        let item = Yojson.Safe.Util.member "item" json in
        if Cli_common_json.member_str "type" item = "agent_message" then
          texts := Cli_common_json.member_str "text" item :: !texts
      | "turn.completed" ->
        usage := parse_usage json
      | _ -> ()
    with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> ()
  ) lines;
  let content = List.rev_map (fun t -> Types.Text t) !texts in
  if content = [] && !thread_id = "" then
    Error (Http_client.NetworkError {
      message = "no events parsed from codex output" })
  else
    Ok { Types.id = !thread_id;
         model = "codex";
         stop_reason = Types.EndTurn;
         content;
         usage = !usage;
         telemetry = None }

(* ── Transport constructor ───────────────────────────── *)

(* Fires once per transport instance when any Claude-only config field
   is set.  Codex CLI has no flag for these yet, so we warn and drop. *)
let warn_unsupported_once (config : config) warned =
  if !warned then ()
  else begin
    warned := true;
    let warn field =
      Eio.traceln "[warn] %s is not supported by codex_cli, ignoring" field
    in
    if Option.is_some config.mcp_config then warn "mcp_config";
    if config.allowed_tools <> [] then warn "allowed_tools";
    if Option.is_some config.max_turns then warn "max_turns";
    if Option.is_some config.permission_mode then warn "permission_mode"
  end

(** Strip API-key env so [codex] uses its own session/auth rather than
    inherited [OPENAI_API_KEY].  Mirrors the rationale in
    {!Transport_claude_code.claude_cli_scrub_env}. *)
let codex_cli_scrub_env =
  ["OPENAI_API_KEY"]

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  let warned = ref false in
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
      warn_unsupported_once config warned;
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let argv = build_args ~config ~prompt in
      let seen_lines = ref [] in
      let on_line line =
        if String.trim line <> "" then
          seen_lines := line :: !seen_lines
      in
      match Cli_common_subprocess.run_stream_lines ~sw ~mgr
              ~name:"codex" ~cwd:config.cwd ~extra_env:[]
              ~scrub_env:codex_cli_scrub_env
              ~on_line ?cancel:config.cancel
              argv with
      | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
      | Ok { stdout = _; stderr = _; latency_ms } ->
        let response = parse_jsonl_result (List.rev !seen_lines) in
        { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      warn_unsupported_once config warned;
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let argv = build_args ~config ~prompt in
      let seen_lines = ref [] in
      let on_line line =
        if String.trim line <> "" then begin
          seen_lines := line :: !seen_lines;
          List.iter on_event (events_of_line line)
        end
      in
      match Cli_common_subprocess.run_stream_lines ~sw ~mgr
              ~name:"codex" ~cwd:config.cwd ~extra_env:[]
              ~scrub_env:codex_cli_scrub_env
              ~on_line ?cancel:config.cancel
              argv with
      | Error _ as e -> e
      | Ok _ ->
        parse_jsonl_result (List.rev !seen_lines));
  }

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let%test "default_config codex_path" =
  default_config.codex_path = "codex"

let%test "default_config parity fields absent" =
  default_config.mcp_config = None
  && default_config.allowed_tools = []
  && default_config.max_turns = None
  && default_config.permission_mode = None

let%test "build_args includes --json flag" =
  let args = build_args ~config:default_config ~prompt:"hello" in
  args = ["codex"; "exec"; "--json"; "-c"; "mcp_servers={}"; "hello"]

let%test "build_args ignores extra parity fields" =
  let config = { default_config with
    mcp_config = Some "/tmp/mcp.json";
    allowed_tools = ["Read"];
    max_turns = Some 5;
    permission_mode = Some "bypassPermissions";
  } in
  let args = build_args ~config ~prompt:"hi" in
  args = ["codex"; "exec"; "--json"; "-c"; "mcp_servers={}"; "hi"]

let%test "parse_jsonl_result extracts text + usage + thread_id" =
  let lines = [
    {|{"type":"thread.started","thread_id":"abc-123"}|};
    {|{"type":"turn.started"}|};
    {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hi"}}|};
    {|{"type":"turn.completed","usage":{"input_tokens":100,"cached_input_tokens":5,"output_tokens":3}}|};
  ] in
  match parse_jsonl_result lines with
  | Ok resp ->
    resp.id = "abc-123"
    && resp.content = [Types.Text "hi"]
    && resp.stop_reason = Types.EndTurn
    && (match resp.usage with
        | Some u -> u.input_tokens = 100
                 && u.output_tokens = 3
                 && u.cache_read_input_tokens = 5
        | None -> false)
  | Error _ -> false

let%test "parse_jsonl_result aggregates multiple agent_messages" =
  let lines = [
    {|{"type":"thread.started","thread_id":"t1"}|};
    {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"first"}}|};
    {|{"type":"item.completed","item":{"id":"item_2","type":"agent_message","text":"second"}}|};
    {|{"type":"turn.completed","usage":{"input_tokens":1,"cached_input_tokens":0,"output_tokens":2}}|};
  ] in
  match parse_jsonl_result lines with
  | Ok resp ->
    resp.content = [Types.Text "first"; Types.Text "second"]
  | Error _ -> false

let%test "parse_jsonl_result skips command_execution items" =
  let lines = [
    {|{"type":"thread.started","thread_id":"t"}|};
    {|{"type":"item.completed","item":{"id":"item_1","type":"command_execution","command":"ls","exit_code":0}}|};
    {|{"type":"item.completed","item":{"id":"item_2","type":"agent_message","text":"done"}}|};
  ] in
  match parse_jsonl_result lines with
  | Ok resp ->
    (* command_execution contributes nothing; only agent_message text. *)
    resp.content = [Types.Text "done"]
  | Error _ -> false

let%test "parse_jsonl_result empty lines → Error" =
  match parse_jsonl_result [] with
  | Error _ -> true
  | Ok _ -> false

let%test "events_of_line thread.started → MessageStart" =
  let line = {|{"type":"thread.started","thread_id":"abc"}|} in
  match events_of_line line with
  | [Types.MessageStart { id = "abc"; model = "codex"; _ }] -> true
  | _ -> false

let%test "events_of_line agent_message → 3 block events" =
  let line = {|{"type":"item.completed","item":{"id":"item_0","type":"agent_message","text":"hi"}}|} in
  (match events_of_line line with
   | [Types.ContentBlockStart _; Types.ContentBlockDelta _; Types.ContentBlockStop _] -> true
   | _ -> false)

let%test "events_of_line command_execution → no events" =
  let line = {|{"type":"item.completed","item":{"id":"item_1","type":"command_execution","command":"ls"}}|} in
  events_of_line line = []

let%test "events_of_line turn.completed → MessageDelta+Stop" =
  let line = {|{"type":"turn.completed","usage":{"input_tokens":10,"cached_input_tokens":0,"output_tokens":5}}|} in
  (match events_of_line line with
   | [Types.MessageDelta { stop_reason = Some Types.EndTurn; _ };
      Types.MessageStop] -> true
   | _ -> false)

let%test "events_of_line invalid json → []" =
  events_of_line "not json" = []

(* ── env-driven extra args ──────────────────────────── *)

let with_env k v f =
  let prev = Sys.getenv_opt k in
  Unix.putenv k v;
  Fun.protect ~finally:(fun () ->
    match prev with
    | None -> (try Unix.putenv k "" with _ -> ())
    | Some old -> Unix.putenv k old)
    f

let with_unset k f =
  let prev = Sys.getenv_opt k in
  (try Unix.putenv k "" with _ -> ());
  Fun.protect ~finally:(fun () ->
    match prev with
    | None -> ()
    | Some old -> Unix.putenv k old)
    f

let%test "default: argv disables MCP even with no env" =
  with_unset "OAS_CODEX_CONFIG" (fun () ->
  with_unset "OAS_CODEX_SANDBOX" (fun () ->
  with_unset "OAS_CODEX_PROFILE" (fun () ->
  with_unset "OAS_CODEX_SKIP_GIT" (fun () ->
    build_args ~config:default_config ~prompt:"hi"
    = ["codex"; "exec"; "--json"; "-c"; "mcp_servers={}"; "hi"]))))

let%test "env: OAS_CODEX_CONFIG emits -c pairs before prompt" =
  with_env "OAS_CODEX_CONFIG" "mcp_servers={},model=o3" (fun () ->
    let args = build_args ~config:default_config ~prompt:"hi" in
    (* must emit -c mcp_servers={} and -c model=o3, and prompt stays last *)
    List.mem "-c" args
    && List.mem "mcp_servers={}" args
    && List.mem "model=o3" args
    && List.nth args (List.length args - 1) = "hi")

let%test "env: OAS_CODEX_SANDBOX and OAS_CODEX_SKIP_GIT" =
  with_env "OAS_CODEX_SANDBOX" "read-only" (fun () ->
  with_env "OAS_CODEX_SKIP_GIT" "true" (fun () ->
    let args = build_args ~config:default_config ~prompt:"hi" in
    List.mem "-s" args
    && List.mem "read-only" args
    && List.mem "--skip-git-repo-check" args))
