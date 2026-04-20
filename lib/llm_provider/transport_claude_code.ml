(** Claude Code non-interactive transport.

    @since 0.78.0 *)

type config = {
  claude_path: string;
  model: string option;
  max_turns: int option;
  allowed_tools: string list;
  permission_mode: string option;
  mcp_config: string option;
  cwd: string option;
  tool_use_via_stream_json: bool;
    (* When true, [complete_sync] internally uses [--output-format
       stream-json] and aggregates the assistant blocks so tool_use
       (and thinking) survives in the returned content.  The plain
       [--output-format json] flattens content into a single
       [result] string and drops structured blocks. *)
  forward_tool_results: bool;
    (* When true, prior [ToolUse]/[ToolResult] content blocks in the
       conversation history are flattened into the CLI prompt so the
       next turn sees the tool exchange.  Default [false] — the agent
       loop typically resolves tools itself and feeds only fresh text
       to the CLI. *)
  cancel: unit Eio.Promise.t option;
    (* When [Some p] and [p] is resolved mid-run, the [claude]
       subprocess receives [SIGINT].  Applied to every call served
       by the transport instance.  Default [None]. *)
}

let default_config = {
  claude_path = "claude";
  model = None;
  max_turns = None;
  allowed_tools = [];
  permission_mode = None;
  mcp_config = None;
  cwd = None;
  tool_use_via_stream_json = true;
  forward_tool_results = false;
  cancel = None;
}

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules to deduplicate logic across CLI transports. *)

(* ── CLI argument building ───────────────────────────── *)

(* Non-interactive Claude runs default to MCP OFF by forcing strict MCP
   config resolution.  When [config.mcp_config] or
   [OAS_CLAUDE_MCP_CONFIG] is provided, strict mode narrows the runtime
   to exactly that config; when neither is present, strict mode yields an
   empty MCP surface.  LSP / hooks / auto-memory stay ON — no --bare. *)
let env_extra_args ~(config : config) =
  let extras = ref [] in
  let add a = extras := !extras @ a in
  (* Determine whether an MCP config path is actually available (either
     explicit [config.mcp_config] or OAS_CLAUDE_MCP_CONFIG env fallback).
     [--strict-mcp-config] must only be emitted when we can also point
     Claude at a real config file — otherwise the CLI accepts the flag,
     finds no config, and exits 1 with no stderr, producing the
     "claude exited with code 1: exit code 1" signature that was
     dominating fleet cascade_exhausted failures (690/2.5h, 2026-04-20). *)
  let env_mcp = Cli_common_env.get "OAS_CLAUDE_MCP_CONFIG" in
  let has_mcp_config =
    match config.mcp_config with
    | Some _ -> true
    | None ->
      (match env_mcp with
       | Some v when String.trim v <> "" -> true
       | _ -> false)
  in
  if has_mcp_config then add ["--strict-mcp-config"];
  (* --mcp-config: only used as fallback when config.mcp_config is None.
     Explicit config wins over env, matching the convention that
     programmatic wiring overrides ambient environment. *)
  (match config.mcp_config with
   | Some _ -> ()
   | None ->
     match env_mcp with
     | Some v when String.trim v <> "" -> add ["--mcp-config"; v]
     | _ -> ());
  if Cli_common_env.bool "OAS_CLAUDE_STRICT_MCP" then
    ();
  (match Cli_common_env.list "OAS_CLAUDE_DISALLOWED_TOOLS" with
   | None | Some [] -> ()
   | Some tools -> List.iter (fun t -> add ["--disallowedTools"; t]) tools);
  !extras

let build_args ~(config : config) ~(req_config : Provider_config.t)
    ~prompt ~stream ~system_prompt =
  let args = ref ["-p"; prompt] in
  let add a = args := !args @ a in
  add ["--output-format"; if stream then "stream-json" else "json"];
  if stream then add ["--verbose"];
  (* "auto" means "use the CLI's configured default", so omit [--model]. *)
  let model = match String.trim req_config.model_id |> String.lowercase_ascii with
    | "" | "auto" -> config.model
    | _ -> Some req_config.model_id
  in
  (match model with Some m -> add ["--model"; m] | None -> ());
  (match system_prompt with Some s -> add ["--system-prompt"; s] | None -> ());
  (match config.max_turns with Some n -> add ["--max-turns"; string_of_int n] | None -> ());
  List.iter (fun t -> add ["--allowedTools"; t]) config.allowed_tools;
  (match config.permission_mode with Some m -> add ["--permission-mode"; m] | None -> ());
  (match config.mcp_config with Some c -> add ["--mcp-config"; c] | None -> ());
  add (env_extra_args ~config);
  !args

(* ── JSON parsing ────────────────────────────────────── *)

(** Parse the [usage] object from a result or assistant message. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    Some { Types.input_tokens = Cli_common_json.member_int "input_tokens" u;
           output_tokens = Cli_common_json.member_int "output_tokens" u;
           cache_creation_input_tokens =
             Cli_common_json.member_int "cache_creation_input_tokens" u;
           cache_read_input_tokens =
             Cli_common_json.member_int "cache_read_input_tokens" u;
           cost_usd = None }
  | _ -> None

let parse_stop_reason s = Types.stop_reason_of_string s

(** Parse a sync JSON result into api_response. *)
let parse_json_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    if Cli_common_json.member_bool "is_error" json then
      let msg = Cli_common_json.member_str "result" json in
      Error (Http_client.NetworkError {
        message = Printf.sprintf "Claude Code error: %s" msg })
    else
      let result_text = Cli_common_json.member_str "result" json in
      let model = Cli_common_json.member_str "model" json in
      let session_id = Cli_common_json.member_str "session_id" json in
      let stop_reason =
        parse_stop_reason (Cli_common_json.member_str "stop_reason" json) in
      let usage = parse_usage json in
      Ok { Types.id = session_id;
           model;
           stop_reason;
           content = [Text result_text];
           usage; telemetry = None }
  with
  | Yojson.Json_error msg ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "JSON parse error: %s" msg })

(* ── Stream event parsing ────────────────────────────── *)

(** Parse a single JSONL line from stream-json output.
    Returns a list of OAS sse_events to emit. *)
let events_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    let typ = Cli_common_json.member_str "type" json in
    match typ with
    | "system" ->
      let subtype = Cli_common_json.member_str "subtype" json in
      if subtype = "init" then
        let model = Cli_common_json.member_str "model" json in
        let session_id = Cli_common_json.member_str "session_id" json in
        [Types.MessageStart { id = session_id; model; usage = None }]
      else []
    | "assistant" ->
      let open Yojson.Safe.Util in
      let msg = json |> member "message" in
      let content = msg |> member "content" |> to_list in
      let block_events = List.mapi (fun idx block ->
        let content_type = Cli_common_json.member_str "type" block in
        let text = Cli_common_json.member_str "text" block in
        let tool_id = block |> member "id" |> to_string_option in
        let tool_name = block |> member "name" |> to_string_option in
        let delta = match content_type with
          | "thinking" -> Types.ThinkingDelta text
          | "tool_use" ->
            let input_str = block |> member "input" |> Yojson.Safe.to_string in
            Types.InputJsonDelta input_str
          | _ -> Types.TextDelta text
        in
        [Types.ContentBlockStart { index = idx; content_type; tool_id; tool_name };
         Types.ContentBlockDelta { index = idx; delta };
         Types.ContentBlockStop { index = idx }]
      ) content |> List.flatten in
      let usage = parse_usage msg in
      let stop_reason = msg |> member "stop_reason" |> to_string_option
        |> Option.map parse_stop_reason in
      block_events @ [Types.MessageDelta { stop_reason; usage }]
    | "result" ->
      [Types.MessageStop]
    | _ -> []  (* skip rate_limit_event etc. *)
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []

(** Extract all content blocks (text / thinking / tool_use) from the
    [message.content] array of an "assistant" line.  Unknown types are
    dropped.  The Claude stream-json payload carries structured blocks
    that [--output-format json] flattens into a single [result] string,
    so aggregating across assistant lines is how we recover them. *)
let blocks_of_assistant_message msg =
  let open Yojson.Safe.Util in
  try
    msg |> member "content" |> to_list
    |> List.filter_map (fun block ->
      let typ = Cli_common_json.member_str "type" block in
      match typ with
      | "text" ->
        Some (Types.Text (Cli_common_json.member_str "text" block))
      | "thinking" ->
        Some (Types.Thinking {
          thinking_type = "thinking";
          content = Cli_common_json.member_str "thinking" block;
        })
      | "tool_use" ->
        let id = Cli_common_json.member_str "id" block in
        let name = Cli_common_json.member_str "name" block in
        let input = match block |> member "input" with
          | `Null -> `Assoc []
          | j -> j
        in
        Some (Types.ToolUse { id; name; input })
      | _ -> None)
  with Type_error _ -> []

(** Parse a single line as assistant message blocks.  Errors / wrong
    [type] produce the empty list. *)
let assistant_blocks_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    if Cli_common_json.member_str "type" json = "assistant" then
      blocks_of_assistant_message (Yojson.Safe.Util.member "message" json)
    else []
  with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []

let last_assistant_msg lines =
  let rec loop = function
    | [] -> None
    | line :: rest ->
      (try
        let json = Yojson.Safe.from_string line in
        if Cli_common_json.member_str "type" json = "assistant" then
          Some (Yojson.Safe.Util.member "message" json)
        else loop rest
      with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> loop rest)
  in
  loop (List.rev lines)

(** Parse stream output into an [api_response] that preserves structured
    blocks (text, thinking, tool_use) across all assistant lines.  When
    a terminal "result" line is present, its [stop_reason]/[model]/
    [session_id]/[usage] are adopted; otherwise metadata is derived
    from the final assistant line. *)
let parse_stream_result lines =
  let assistant_blocks =
    List.concat_map assistant_blocks_of_line lines in
  let result_line = List.find_opt (fun line ->
    try
      let json = Yojson.Safe.from_string line in
      Cli_common_json.member_str "type" json = "result"
    with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> false
  ) lines in
  match result_line with
  | Some rline ->
    (try
      let rjson = Yojson.Safe.from_string rline in
      if Cli_common_json.member_bool "is_error" rjson then
        let msg = Cli_common_json.member_str "result" rjson in
        Error (Http_client.NetworkError {
          message = Printf.sprintf "Claude Code error: %s" msg })
      else
        let model = Cli_common_json.member_str "model" rjson in
        let session_id = Cli_common_json.member_str "session_id" rjson in
        let stop_reason =
          parse_stop_reason (Cli_common_json.member_str "stop_reason" rjson) in
        let usage = parse_usage rjson in
        let content =
          if assistant_blocks <> [] then assistant_blocks
          else
            (* No assistant blocks were streamed — fall back to the flat
               [result] string.  Keeps behaviour backward-compatible with
               the old [parse_json_result] path. *)
            [Types.Text (Cli_common_json.member_str "result" rjson)]
        in
        Ok { Types.id = session_id; model; stop_reason; content;
             usage; telemetry = None }
    with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
      Error (Http_client.NetworkError {
        message = "Failed to parse result line" }))
  | None ->
    if assistant_blocks = [] then
      Error (Http_client.NetworkError {
        message = "No result or assistant message in stream output" })
    else
      let id, model, usage = match last_assistant_msg lines with
        | Some msg ->
          (Cli_common_json.member_str "id" msg,
           Cli_common_json.member_str "model" msg,
           parse_usage msg)
        | None -> "", "", None
      in
      Ok { Types.id; model; stop_reason = EndTurn;
           content = assistant_blocks; usage; telemetry = None }

(* ── Transport constructor ───────────────────────────── *)

(** Env vars stripped from every [claude] subprocess.

    When [ANTHROPIC_API_KEY] is set in the parent process, [claude -p]
    authenticates as a metered API client (subject to the org's API
    spend limits) rather than using the user's OAuth/subscription
    session.  MASC / agent integrations that rely on the subscription
    tier must not leak API_KEY env to the CLI.

    The three names below cover the canonical variable plus the
    conventional [_MAIN] / [_WORK] split some callers adopt. *)
let claude_cli_scrub_env =
  ["ANTHROPIC_API_KEY"; "ANTHROPIC_API_KEY_MAIN"; "ANTHROPIC_API_KEY_WORK"]

let run ~sw ~mgr ~(config : config) args =
  Cli_common_subprocess.run_collect ~sw ~mgr
    ~name:"claude"
    ~cwd:config.cwd
    ~extra_env:[]
    ~scrub_env:claude_cli_scrub_env
    ?cancel:config.cancel
    (config.claude_path :: args)

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages
        ~include_tool_blocks:config.forward_tool_results messages in
      let system_prompt =
        Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages in
      if config.tool_use_via_stream_json then
        (* Use stream-json internally so we can aggregate tool_use /
           thinking blocks.  [--output-format json] flattens these into
           the [result] string and we'd lose them. *)
        let args = build_args ~config ~req_config:req.config
          ~prompt ~stream:true ~system_prompt in
        let argv = config.claude_path :: args in
        let seen_lines = ref [] in
        let on_line line =
          if String.trim line <> "" then
            seen_lines := line :: !seen_lines
        in
        match Cli_common_subprocess.run_stream_lines ~sw ~mgr
                ~name:"claude" ~cwd:config.cwd ~extra_env:[]
                ~scrub_env:claude_cli_scrub_env
                ~on_line ?cancel:config.cancel
                argv with
        | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
        | Ok { stdout = _; stderr = _; latency_ms } ->
          let response = parse_stream_result (List.rev !seen_lines) in
          { Llm_transport.response; latency_ms }
      else
        let args = build_args ~config ~req_config:req.config
          ~prompt ~stream:false ~system_prompt in
        match run ~sw ~mgr ~config args with
        | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
        | Ok { stdout; stderr = _; latency_ms } ->
          let response = parse_json_result (String.trim stdout) in
          { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages
        ~include_tool_blocks:config.forward_tool_results messages in
      let system_prompt =
        Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages in
      let args = build_args ~config ~req_config:req.config
        ~prompt ~stream:true ~system_prompt in
      let argv = config.claude_path :: args in
      let seen_lines = ref [] in
      let on_line line =
        if String.trim line <> "" then begin
          seen_lines := line :: !seen_lines;
          List.iter on_event (events_of_line line)
        end
      in
      match Cli_common_subprocess.run_stream_lines ~sw ~mgr
              ~name:"claude"
              ~cwd:config.cwd
              ~extra_env:[]
              ~scrub_env:claude_cli_scrub_env
              ~on_line
              ?cancel:config.cancel
              argv with
      | Error _ as e -> e
      | Ok _ ->
        (* Final response is built from whatever lines we saw. *)
        parse_stream_result (List.rev !seen_lines));
  }

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let%test "default_config claude_path" =
  default_config.claude_path = "claude"

let%test "parse_json_result success" =
  let json = {|{"type":"result","subtype":"success","is_error":false,"result":"hello world","model":"claude-sonnet-4","stop_reason":"end_turn","session_id":"s1","duration_api_ms":100}|} in
  match parse_json_result json with
  | Ok resp ->
    resp.model = "claude-sonnet-4"
    && resp.content = [Types.Text "hello world"]
    && resp.stop_reason = Types.EndTurn
  | Error _ -> false

let%test "parse_json_result error" =
  let json = {|{"type":"result","subtype":"error","is_error":true,"result":"rate limited","model":"m","stop_reason":"","session_id":"s1"}|} in
  match parse_json_result json with
  | Error (Http_client.NetworkError { message }) ->
    String.length message > 0
  | _ -> false

let%test "parse_json_result invalid json" =
  match parse_json_result "not json" with
  | Error _ -> true
  | Ok _ -> false

let%test "events_of_line system init" =
  let line = {|{"type":"system","subtype":"init","model":"claude-sonnet-4","session_id":"s1"}|} in
  match events_of_line line with
  | [Types.MessageStart { model = "claude-sonnet-4"; _ }] -> true
  | _ -> false

let%test "events_of_line assistant with text" =
  let line = {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"hello"}],"stop_reason":null,"usage":{"input_tokens":10,"output_tokens":5}}}|} in
  let events = events_of_line line in
  List.length events >= 3  (* ContentBlockStart + Delta + Stop + MessageDelta *)

let%test "events_of_line result" =
  let line = {|{"type":"result","subtype":"success","is_error":false,"result":"ok","model":"m","stop_reason":"end_turn","session_id":"s1"}|} in
  match events_of_line line with
  | [Types.MessageStop] -> true
  | _ -> false

let%test "events_of_line unknown type" =
  let line = {|{"type":"rate_limit_event","rate_limit_info":{}}|} in
  events_of_line line = []

let%test "events_of_line invalid json" =
  events_of_line "not json" = []

let%test "build_args basic" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Anthropic ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~stream:false ~system_prompt:None in
  List.mem "-p" args && List.mem "json" args

let%test "build_args with model" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Anthropic ~model_id:"claude-sonnet-4" ~base_url:"" ())
    ~prompt:"hello" ~stream:true ~system_prompt:(Some "be helpful") in
  List.mem "--model" args
  && List.mem "claude-sonnet-4" args
  && List.mem "--system-prompt" args
  && List.mem "stream-json" args
  && List.mem "--verbose" args

let%test "build_args omits auto model override" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"auto" ~base_url:"" ())
    ~prompt:"hello" ~stream:false ~system_prompt:None in
  not (List.mem "--model" args)

(* [--strict-mcp-config] must only be emitted when an MCP config path is
   actually available; otherwise Claude CLI accepts the flag, finds no
   config, and exits 1 without stderr (root cause of the
   "claude exited with code 1: exit code 1" fleet failure signature). *)
let%test "build_args omits --strict-mcp-config when no config present" =
  (* No config.mcp_config and no OAS_CLAUDE_MCP_CONFIG env. *)
  Unix.putenv "OAS_CLAUDE_MCP_CONFIG" "";
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~stream:false ~system_prompt:None in
  not (List.mem "--strict-mcp-config" args)
  && not (List.mem "--mcp-config" args)

let%test "build_args includes --strict-mcp-config when config.mcp_config set" =
  let cfg = { default_config with mcp_config = Some "/tmp/mcp.json" } in
  let args = build_args ~config:cfg
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~stream:false ~system_prompt:None in
  List.mem "--strict-mcp-config" args
  && List.mem "--mcp-config" args
  && List.mem "/tmp/mcp.json" args

let%test "build_args includes --strict-mcp-config when env fallback set" =
  Unix.putenv "OAS_CLAUDE_MCP_CONFIG" "/tmp/env-mcp.json";
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~stream:false ~system_prompt:None in
  let ok =
    List.mem "--strict-mcp-config" args
    && List.mem "--mcp-config" args
    && List.mem "/tmp/env-mcp.json" args
  in
  Unix.putenv "OAS_CLAUDE_MCP_CONFIG" "";
  ok

let%test "parse_stop_reason variants" =
  parse_stop_reason "end_turn" = Types.EndTurn
  && parse_stop_reason "tool_use" = Types.StopToolUse
  && parse_stop_reason "max_tokens" = Types.MaxTokens
  && parse_stop_reason "stop_sequence" = Types.StopSequence
  && parse_stop_reason "unknown" = Types.Unknown "unknown"

let%test "parse_stream_result from result line" =
  let lines = [
    {|{"type":"system","subtype":"init","model":"m","session_id":"s1"}|};
    {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"hello"}],"stop_reason":null,"usage":{}}}|};
    {|{"type":"result","subtype":"success","is_error":false,"result":"hello","model":"m","stop_reason":"end_turn","session_id":"s1"}|};
  ] in
  match parse_stream_result lines with
  | Ok resp -> resp.content = [Types.Text "hello"]
  | Error _ -> false

let%test "parse_stream_result fallback to assistant" =
  let lines = [
    {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"fallback"}],"stop_reason":null,"usage":{}}}|};
  ] in
  match parse_stream_result lines with
  | Ok resp ->
    (match resp.content with
     | [Types.Text "fallback"] -> true
     | _ -> false)
  | Error _ -> false

let%test "parse_stream_result no messages" =
  match parse_stream_result [] with
  | Error _ -> true
  | Ok _ -> false

let%test "parse_stream_result restores tool_use blocks" =
  let lines = [
    {|{"type":"system","subtype":"init","model":"m","session_id":"s1"}|};
    {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"using a tool"},{"type":"tool_use","id":"tu_1","name":"calc","input":{"x":1}}],"stop_reason":null,"usage":{}}}|};
    {|{"type":"result","subtype":"success","is_error":false,"result":"unused","model":"m","stop_reason":"tool_use","session_id":"s1"}|};
  ] in
  match parse_stream_result lines with
  | Ok resp ->
    resp.stop_reason = Types.StopToolUse
    && (match resp.content with
        | [Types.Text "using a tool";
           Types.ToolUse { id = "tu_1"; name = "calc"; _ }] -> true
        | _ -> false)
  | Error _ -> false

let%test "parse_stream_result aggregates across multiple assistant lines" =
  let lines = [
    {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"first"}],"stop_reason":null,"usage":{}}}|};
    {|{"type":"assistant","message":{"model":"m","id":"msg2","content":[{"type":"tool_use","id":"tu_2","name":"search","input":{"q":"hi"}}],"stop_reason":null,"usage":{}}}|};
  ] in
  match parse_stream_result lines with
  | Ok resp ->
    (match resp.content with
     | [Types.Text "first"; Types.ToolUse { name = "search"; _ }] -> true
     | _ -> false)
  | Error _ -> false

let%test "parse_stream_result preserves thinking blocks" =
  let lines = [
    {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"thinking","thinking":"let me think"},{"type":"text","text":"done"}],"stop_reason":null,"usage":{}}}|};
    {|{"type":"result","subtype":"success","is_error":false,"result":"unused","model":"m","stop_reason":"end_turn","session_id":"s1"}|};
  ] in
  match parse_stream_result lines with
  | Ok resp ->
    (match resp.content with
     | [Types.Thinking { content = "let me think"; _ };
        Types.Text "done"] -> true
     | _ -> false)
  | Error _ -> false

let%test "default_config has tool_use_via_stream_json=true" =
  default_config.tool_use_via_stream_json = true

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

let sample_req =
  Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ()

let%test "default: strict MCP is always enabled" =
  with_unset "OAS_CLAUDE_STRICT_MCP" (fun () ->
  with_unset "OAS_CLAUDE_MCP_CONFIG" (fun () ->
  with_unset "OAS_CLAUDE_DISALLOWED_TOOLS" (fun () ->
    let args = build_args ~config:default_config ~req_config:sample_req
      ~prompt:"hi" ~stream:false ~system_prompt:None in
    List.mem "--strict-mcp-config" args
    && not (List.mem "--disallowedTools" args))))

let%test "env: OAS_CLAUDE_STRICT_MCP=1 appends --strict-mcp-config" =
  with_env "OAS_CLAUDE_STRICT_MCP" "1" (fun () ->
    let args = build_args ~config:default_config ~req_config:sample_req
      ~prompt:"hi" ~stream:false ~system_prompt:None in
    List.mem "--strict-mcp-config" args)

let%test "env: MCP_CONFIG fallback only when config.mcp_config is None" =
  with_env "OAS_CLAUDE_MCP_CONFIG" "/tmp/mcp.json" (fun () ->
    let args_default = build_args ~config:default_config ~req_config:sample_req
      ~prompt:"hi" ~stream:false ~system_prompt:None in
    let explicit_cfg = { default_config with mcp_config = Some "/tmp/explicit.json" } in
    let args_explicit = build_args ~config:explicit_cfg ~req_config:sample_req
      ~prompt:"hi" ~stream:false ~system_prompt:None in
    List.mem "/tmp/mcp.json" args_default
    && List.mem "/tmp/explicit.json" args_explicit
    && not (List.mem "/tmp/mcp.json" args_explicit))

let%test "env: DISALLOWED_TOOLS splits on comma" =
  with_env "OAS_CLAUDE_DISALLOWED_TOOLS" "Bash,Write" (fun () ->
    let args = build_args ~config:default_config ~req_config:sample_req
      ~prompt:"hi" ~stream:false ~system_prompt:None in
    (* Each token emits its own --disallowedTools flag. *)
    List.mem "--disallowedTools" args
    && List.mem "Bash" args
    && List.mem "Write" args)
