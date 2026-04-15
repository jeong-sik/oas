(** Codex CLI non-interactive transport.

    @since 0.133.0 *)

type config = {
  codex_path: string;
  cwd: string option;
  (* Fields below are accepted for parity with the Claude Code config
     so callers can target multiple CLI backends with the same
     structure.  Codex CLI does not yet expose flags for any of
     them; setting a non-default value produces a one-shot
     [Eio.traceln] warning and the value is otherwise ignored. *)
  mcp_config: string option;
  allowed_tools: string list;
  max_turns: int option;
  permission_mode: string option;
}

let default_config = {
  codex_path = "codex";
  cwd = None;
  mcp_config = None;
  allowed_tools = [];
  max_turns = None;
  permission_mode = None;
}

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules. *)

(* ── CLI argument building ───────────────────────────── *)

let build_args ~(config : config) ~prompt =
  [config.codex_path; "exec"; prompt]

(* ── Output parsing ──────────────────────────────────── *)

(** Try to extract a token count from the last lines of codex output.
    Codex may append "tokens used\nN" at the end. *)
let try_extract_tokens (stdout : string) =
  let lines = String.split_on_char '\n' stdout
    |> List.filter (fun s -> String.trim s <> "")
    |> List.rev in
  match lines with
  | count_str :: label :: _ when String.lowercase_ascii (String.trim label) = "tokens used" ->
    (try Some (int_of_string (String.trim count_str))
     with Failure _ -> None)
  | _ -> None

(** Parse raw text output from codex exec into api_response.
    Content is the full stdout text. Usage is extracted from trailing
    "tokens used" line if present. *)
let parse_text_result (stdout : string) =
  let usage = match try_extract_tokens stdout with
    | Some total ->
      Some { Types.input_tokens = 0;
             output_tokens = total;
             cache_creation_input_tokens = 0;
             cache_read_input_tokens = 0;
             cost_usd = None }
    | None -> None
  in
  Ok { Types.id = "";
       model = "codex";
       stop_reason = Types.EndTurn;
       content = [Text (String.trim stdout)];
       usage; telemetry = None }

(* ── Transport constructor ───────────────────────────── *)

let run ~sw ~mgr ~(config : config) argv =
  Cli_common_subprocess.run_collect ~sw ~mgr
    ~name:"codex"
    ~cwd:config.cwd
    ~extra_env:[]
    argv

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

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  let warned = ref false in
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
      warn_unsupported_once config warned;
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let argv = build_args ~config ~prompt in
      match run ~sw ~mgr ~config argv with
      | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
      | Ok { stdout; stderr = _; latency_ms } ->
        let response = parse_text_result stdout in
        { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      warn_unsupported_once config warned;
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let argv = build_args ~config ~prompt in
      (* Codex CLI does not support native streaming; replay synthetic events
         after the sync call completes. *)
      match run ~sw ~mgr ~config argv with
      | Error _ as e -> e
      | Ok { stdout; stderr = _; latency_ms = _ } ->
        let result = parse_text_result stdout in
        (match result with
         | Ok resp ->
           Cli_common_synthetic_events.replay ~on_event resp;
           Ok resp
         | Error _ as e -> e));
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

let%test "build_args ignores extra parity fields" =
  let config = { default_config with
    mcp_config = Some "/tmp/mcp.json";
    allowed_tools = ["Read"];
    max_turns = Some 5;
    permission_mode = Some "bypassPermissions";
  } in
  let args = build_args ~config ~prompt:"hi" in
  (* Codex argv is [codex; exec; prompt] — no flag surface. *)
  args = ["codex"; "exec"; "hi"]

let%test "build_args basic" =
  let args = build_args ~config:default_config ~prompt:"hello" in
  args = ["codex"; "exec"; "hello"]

let%test "parse_text_result plain text" =
  match parse_text_result "hello world\n" with
  | Ok resp ->
    resp.content = [Types.Text "hello world"]
    && resp.stop_reason = Types.EndTurn
    && resp.usage = None
  | Error _ -> false

let%test "parse_text_result with tokens" =
  let stdout = "some output\ntokens used\n42\n" in
  match parse_text_result stdout with
  | Ok resp ->
    resp.content = [Types.Text (String.trim stdout)]
    && (match resp.usage with
        | Some u -> u.output_tokens = 42
        | None -> false)
  | Error _ -> false

let%test "try_extract_tokens with valid count" =
  try_extract_tokens "output\ntokens used\n100\n" = Some 100

let%test "try_extract_tokens no pattern" =
  try_extract_tokens "just some output\n" = None

let%test "try_extract_tokens invalid number" =
  try_extract_tokens "output\ntokens used\nnot_a_number\n" = None
