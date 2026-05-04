(** Claude Code non-interactive transport.

    @since 0.78.0 *)

type config =
  { claude_path : string
  ; model : string option
  ; max_turns : int option
  ; allowed_tools : string list
  ; permission_mode : string option
  ; mcp_config : string option
  ; cwd : string option
  ; tool_use_via_stream_json : bool
    (* When true, [complete_sync] internally uses [--output-format
       stream-json] and aggregates the assistant blocks so tool_use
       (and thinking) survives in the returned content.  The plain
       [--output-format json] flattens content into a single
       [result] string and drops structured blocks. *)
  ; forward_tool_results : bool
    (* When true, prior [ToolUse]/[ToolResult] content blocks in the
       conversation history are flattened into the CLI prompt so the
       next turn sees the tool exchange.  Default [false] — the agent
       loop typically resolves tools itself and feeds only fresh text
       to the CLI. *)
  ; cancel : unit Eio.Promise.t option
    (* When [Some p] and [p] is resolved mid-run, the [claude]
       subprocess receives [SIGINT].  Applied to every call served
       by the transport instance.  Default [None]. *)
  }

let default_config =
  { claude_path = "claude"
  ; model = None
  ; max_turns = None
  ; allowed_tools = []
  ; permission_mode = None
  ; mcp_config = None
  ; cwd = None
  ; tool_use_via_stream_json = true
  ; forward_tool_results = false
  ; cancel = None
  }
;;

let effective_max_turns config =
  Option.map
    (Provider_config.clamp_max_turns Provider_config.Claude_code)
    config.max_turns
;;

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules to deduplicate logic across CLI transports. *)

let json_of_string_pairs = Cli_common_json.json_of_string_pairs

let json_of_runtime_mcp_server = function
  | Llm_transport.Stdio_server { name = _; command; args; env } ->
    `Assoc
      [ "command", `String command
      ; "args", Cli_common_json.json_of_string_list args
      ; "env", json_of_string_pairs env
      ]
  | Llm_transport.Http_server { name = _; url; headers } ->
    `Assoc
      [ "type", `String "http"
      ; "url", `String url
      ; "headers", json_of_string_pairs headers
      ]
;;

let mcp_config_json_of_policy (policy : Llm_transport.runtime_mcp_policy)
  : Yojson.Safe.t option
  =
  match policy.servers with
  | [] -> None
  | servers ->
    let entries =
      List.map
        (fun server ->
           Llm_transport.runtime_mcp_server_name server, json_of_runtime_mcp_server server)
        servers
    in
    Some (`Assoc [ "mcpServers", `Assoc entries ])
;;

let claude_allowed_tools_of_policy (policy : Llm_transport.runtime_mcp_policy)
  : string list
  =
  let server_names =
    match policy.allowed_server_names with
    | [] -> List.map Llm_transport.runtime_mcp_server_name policy.servers
    | names -> names
  in
  match policy.allowed_tool_names with
  | [] -> List.map (fun server_name -> "mcp__" ^ server_name) server_names
  | tool_names ->
    List.concat_map
      (fun server_name ->
         List.map
           (fun tool_name -> Printf.sprintf "mcp__%s__%s" server_name tool_name)
           tool_names)
      server_names
;;

(* ── CLI argument building ───────────────────────────── *)

(* Non-interactive Claude runs default to MCP OFF by forcing strict MCP
   config resolution.  When [config.mcp_config] or
   [OAS_CLAUDE_MCP_CONFIG] is provided, strict mode narrows the runtime
   to exactly that config; when neither is present, strict mode yields an
   empty MCP surface.  LSP / hooks / auto-memory stay ON — no --bare. *)
let legacy_env_extra_args ~(config : config) =
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
       | Some _ -> true
       | None -> false)
  in
  if has_mcp_config then add [ "--strict-mcp-config" ];
  (* --mcp-config: only used as fallback when config.mcp_config is None.
     Explicit config wins over env, matching the convention that
     programmatic wiring overrides ambient environment. *)
  (match config.mcp_config with
   | Some _ -> ()
   | None ->
     (match env_mcp with
      | Some v -> add [ "--mcp-config"; v ]
      | None -> ()));
  if Cli_common_env.bool "OAS_CLAUDE_STRICT_MCP" then ();
  (match Cli_common_env.list "OAS_CLAUDE_DISALLOWED_TOOLS" with
   | None | Some [] -> ()
   | Some tools -> List.iter (fun t -> add [ "--disallowedTools"; t ]) tools);
  !extras
;;

(** Threshold at which [build_args] stops passing the prompt as a
    positional argv entry and expects the caller to feed it via
    stdin instead.  macOS [ARG_MAX] is ~1 MiB for the combined argv
    + envp block; 512 KiB leaves headroom for env vars (conversation
    context, OAS_* flags) and the other argv entries added after the
    prompt.  Env override [OAS_CLAUDE_PROMPT_ARGV_THRESHOLD] accepts
    an integer byte count for per-host tuning. *)
let default_prompt_argv_threshold = 512 * 1024

let prompt_argv_threshold () =
  Cli_common_env.int
    ~default:default_prompt_argv_threshold
    "OAS_CLAUDE_PROMPT_ARGV_THRESHOLD"
;;

(** Decide whether the prompt must be routed via stdin.  Callers that
    observe a [true] result should:
      1. call [build_args] which returns [-p <empty>] (or omits the
         prompt entirely, see below);
      2. pass the prompt as [~stdin_content] to the subprocess.
    Returning the threshold here keeps the decision co-located with
    argv construction, so any future argv-budget change lives in one
    place. *)
let prompt_exceeds_argv_budget prompt = String.length prompt >= prompt_argv_threshold ()

(** Caller-side helper: wrap [prompt] in [Some] when it must go via
    stdin, [None] when argv is fine.  Saves the three call sites
    from duplicating the budget check alongside [build_args]. *)
let stdin_for_prompt prompt =
  if prompt_exceeds_argv_budget prompt then Some prompt else None
;;

let build_args
      ~(config : config)
      ~(req_config : Provider_config.t)
      ?runtime_mcp_policy
      ~prompt
      ~stream
      ~system_prompt
      ()
  =
  (* When the prompt is too big for argv, omit it from the positional
     slot. Claude CLI (`--input-format text`, the default) then reads
     the prompt from stdin via [--print] / [-p]. We still pass [-p]
     to select non-interactive mode; the CLI concatenates stdin onto
     the empty positional. *)
  let prompt_via_stdin = prompt_exceeds_argv_budget prompt in
  let args = ref (if prompt_via_stdin then [ "-p" ] else [ "-p"; prompt ]) in
  let add a = args := !args @ a in
  add [ "--output-format"; (if stream then "stream-json" else "json") ];
  if stream then add [ "--verbose" ];
  (* "auto" means "use the CLI's configured default", so omit [--model]. *)
  let model =
    match String.trim req_config.model_id |> String.lowercase_ascii with
    | "" | "auto" -> config.model
    | _ -> Some req_config.model_id
  in
  (match model with
   | Some m -> add [ "--model"; m ]
   | None -> ());
  (match system_prompt with
   | Some s -> add [ "--system-prompt"; s ]
   | None -> ());
  (match effective_max_turns config with
   | Some n -> add [ "--max-turns"; string_of_int n ]
   | None -> ());
  (match runtime_mcp_policy with
   | Some (policy : Llm_transport.runtime_mcp_policy) ->
     let emitted_mcp_config = ref false in
     if policy.disable_builtin_tools then add [ "--tools"; "" ];
     List.iter
       (fun tool_name -> add [ "--allowedTools"; tool_name ])
       (claude_allowed_tools_of_policy policy);
     (match policy.permission_mode with
      | Some mode -> add [ "--permission-mode"; mode ]
      | None ->
        (match config.permission_mode with
         | Some mode -> add [ "--permission-mode"; mode ]
         | None -> ()));
     (match mcp_config_json_of_policy policy with
      | Some json ->
        emitted_mcp_config := true;
        add [ "--mcp-config"; Yojson.Safe.to_string json ]
      | None ->
        (match config.mcp_config with
         | Some c ->
           emitted_mcp_config := true;
           add [ "--mcp-config"; c ]
         | None -> ()));
     if policy.strict && !emitted_mcp_config then add [ "--strict-mcp-config" ];
     (match Cli_common_env.list "OAS_CLAUDE_DISALLOWED_TOOLS" with
      | None | Some [] -> ()
      | Some tools -> List.iter (fun t -> add [ "--disallowedTools"; t ]) tools)
   | None ->
     List.iter (fun t -> add [ "--allowedTools"; t ]) config.allowed_tools;
     (match config.permission_mode with
      | Some m -> add [ "--permission-mode"; m ]
      | None -> ());
     (match config.mcp_config with
      | Some c -> add [ "--mcp-config"; c ]
      | None -> ());
     add (legacy_env_extra_args ~config));
  !args
;;

(* ── JSON parsing ────────────────────────────────────── *)

(** Claude Code reports per-response usage for normal turns, but older
    fleet data has shown impossible session-scale counters leaking through
    this field. Keep [api_usage] as single-response telemetry by dropping
    values above Claude Code's declared 1M input window. *)
let claude_code_max_single_response_input_tokens = 1_000_000

let plausible_single_response_usage (u : Types.api_usage) =
  u.input_tokens <= claude_code_max_single_response_input_tokens
;;

(** Parse the [usage] object from a result or assistant message. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    let usage =
      { Types.input_tokens = Cli_common_json.member_int "input_tokens" u
      ; output_tokens = Cli_common_json.member_int "output_tokens" u
      ; cache_creation_input_tokens =
          Cli_common_json.member_int "cache_creation_input_tokens" u
      ; cache_read_input_tokens = Cli_common_json.member_int "cache_read_input_tokens" u
      ; cost_usd = None
      }
    in
    if plausible_single_response_usage usage
    then Some usage
    else (
      Diag.warn
        "transport_claude_code"
        "usage dropped: input_tokens=%d exceeds single-response ceiling=%d"
        usage.input_tokens
        claude_code_max_single_response_input_tokens;
      None)
  | _ -> None
;;

let parse_stop_reason s = Types.stop_reason_of_string s

(** Parse a sync JSON result into api_response.

    Mirrors the [is_error=true] dispatch in {!parse_stream_result} so
    sync and stream paths agree on classification: structured terminal
    subtypes (e.g. [error_max_turns]) become {!Http_client.ProviderTerminal},
    not {!Http_client.NetworkError}.  See {!parse_stream_result} for the
    reasoning. *)
let parse_json_result ~prompt json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    if Cli_common_json.member_bool "is_error" json
    then (
      let subtype = Cli_common_json.member_str "subtype" json in
      let msg = Cli_common_json.member_str "result" json in
      (* Whitelist only structured terminal subtypes for [ProviderTerminal].
         Unknown/legacy subtypes (e.g. [subtype="error"] for generic API
         failures, rate limits, network) keep the {!NetworkError} path so
         they remain retryable through the cascade.  Adding new structured
         subtypes (e.g. [error_max_thinking_tokens]) is intentionally an
         explicit code change so we never accidentally promote a transient
         API failure to "graceful checkpoint". *)
      match subtype with
      | "error_max_turns" ->
        let turns = Cli_common_json.member_int "num_turns" json in
        Error
          (Http_client.ProviderTerminal
             { kind = Max_turns { turns; limit = turns }
             ; message =
                 Printf.sprintf "claude_code internal max_turns reached at %d turns" turns
             })
      | _ ->
        Error
          (Http_client.NetworkError
             { message = Printf.sprintf "Claude Code error: %s" msg; kind = Unknown }))
    else (
      let result_text = Cli_common_json.member_str "result" json in
      let model = Cli_common_json.member_str "model" json in
      let session_id = Cli_common_json.member_str "session_id" json in
      let stop_reason =
        parse_stop_reason (Cli_common_json.member_str "stop_reason" json)
      in
      let usage =
        match parse_usage json with
        | Some u -> Some (Pricing.annotate_usage_cost ~model_id:model u)
        | None ->
          Some
            (Cli_common_prompt.estimate_usage
               ~prompt
               ~response_text:result_text
               ~model_id:model)
      in
      Ok
        { Types.id = session_id
        ; model
        ; stop_reason
        ; content = [ Text result_text ]
        ; usage
        ; telemetry = None
        })
  with
  | Yojson.Json_error msg ->
    Error
      (Http_client.NetworkError
         { message = Printf.sprintf "JSON parse error: %s" msg; kind = Unknown })
;;

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
      if subtype = "init"
      then (
        let model = Cli_common_json.member_str "model" json in
        let session_id = Cli_common_json.member_str "session_id" json in
        [ Types.MessageStart { id = session_id; model; usage = None } ])
      else []
    | "assistant" ->
      let open Yojson.Safe.Util in
      let msg = json |> member "message" in
      let content = msg |> member "content" |> to_list in
      let block_events =
        List.mapi
          (fun idx block ->
             let content_type = Cli_common_json.member_str "type" block in
             let text = Cli_common_json.member_str "text" block in
             let tool_id = block |> member "id" |> to_string_option in
             let tool_name = block |> member "name" |> to_string_option in
             let delta =
               match content_type with
               | "thinking" -> Types.ThinkingDelta text
               | "tool_use" ->
                 let input_str = block |> member "input" |> Yojson.Safe.to_string in
                 Types.InputJsonDelta input_str
               | _ -> Types.TextDelta text
             in
             [ Types.ContentBlockStart { index = idx; content_type; tool_id; tool_name }
             ; Types.ContentBlockDelta { index = idx; delta }
             ; Types.ContentBlockStop { index = idx }
             ])
          content
        |> List.flatten
      in
      let usage = parse_usage msg in
      let stop_reason =
        msg |> member "stop_reason" |> to_string_option |> Option.map parse_stop_reason
      in
      block_events @ [ Types.MessageDelta { stop_reason; usage } ]
    | "result" -> [ Types.MessageStop ]
    | _ -> [] (* skip rate_limit_event etc. *)
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []
;;

(** Extract all content blocks (text / thinking / tool_use) from the
    [message.content] array of an "assistant" line.  Unknown types are
    dropped.  The Claude stream-json payload carries structured blocks
    that [--output-format json] flattens into a single [result] string,
    so aggregating across assistant lines is how we recover them. *)
let blocks_of_assistant_message msg =
  let open Yojson.Safe.Util in
  try
    msg
    |> member "content"
    |> to_list
    |> List.filter_map (fun block ->
      let typ = Cli_common_json.member_str "type" block in
      match typ with
      | "text" -> Some (Types.Text (Cli_common_json.member_str "text" block))
      | "thinking" ->
        Some
          (Types.Thinking
             { thinking_type = "thinking"
             ; content = Cli_common_json.member_str "thinking" block
             })
      | "tool_use" ->
        let id = Cli_common_json.member_str "id" block in
        let name = Cli_common_json.member_str "name" block in
        let input =
          match block |> member "input" with
          | `Null -> `Assoc []
          | j -> j
        in
        Some (Types.ToolUse { id; name; input })
      | _ -> None)
  with
  | Type_error _ -> []
;;

(** Parse a single line as assistant message blocks.  Errors / wrong
    [type] produce the empty list. *)
let assistant_blocks_of_line line =
  try
    let json = Yojson.Safe.from_string line in
    if Cli_common_json.member_str "type" json = "assistant"
    then blocks_of_assistant_message (Yojson.Safe.Util.member "message" json)
    else []
  with
  | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> []
;;

let last_assistant_msg lines =
  let rec loop = function
    | [] -> None
    | line :: rest ->
      (try
         let json = Yojson.Safe.from_string line in
         if Cli_common_json.member_str "type" json = "assistant"
         then Some (Yojson.Safe.Util.member "message" json)
         else loop rest
       with
       | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> loop rest)
  in
  loop (List.rev lines)
;;

(** Parse stream output into an [api_response] that preserves structured
    blocks (text, thinking, tool_use) across all assistant lines.  When
    a terminal "result" line is present, its [stop_reason]/[model]/
    [session_id]/[usage] are adopted; otherwise metadata is derived
    from the final assistant line. *)
let parse_stream_result lines =
  let assistant_blocks = List.concat_map assistant_blocks_of_line lines in
  let result_line =
    List.find_opt
      (fun line ->
         try
           let json = Yojson.Safe.from_string line in
           Cli_common_json.member_str "type" json = "result"
         with
         | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> false)
      lines
  in
  match result_line with
  | Some rline ->
    (try
       let rjson = Yojson.Safe.from_string rline in
       if Cli_common_json.member_bool "is_error" rjson
       then (
         (* claude_code emits structured [is_error=true] result lines for
           provider-internal terminal conditions (max_turns, …) alongside
           generic API errors.  Burying these as [NetworkError] loses the
           [subtype] field that downstream cascades and the agent runtime
           need to distinguish "subprocess hit its own turn budget —
           checkpoint and resume next cycle" from "transient network
           failure — fall back to next provider".

           Whitelist only structured terminal subtypes for
           [ProviderTerminal]; unknown/legacy subtypes
           ([subtype="error"], generic API errors) keep the
           {!NetworkError} path so they remain retryable.  Adding new
           subtypes (e.g. [error_max_thinking_tokens]) is intentionally
           an explicit code change so we never accidentally promote a
           transient API failure to "graceful checkpoint". *)
         let subtype = Cli_common_json.member_str "subtype" rjson in
         let msg = Cli_common_json.member_str "result" rjson in
         match subtype with
         | "error_max_turns" ->
           let turns = Cli_common_json.member_int "num_turns" rjson in
           Error
             (Http_client.ProviderTerminal
                { kind = Max_turns { turns; limit = turns }
                ; message =
                    Printf.sprintf
                      "claude_code internal max_turns reached at %d turns"
                      turns
                })
         | _ ->
           Error
             (Http_client.NetworkError
                { message = Printf.sprintf "Claude Code error: %s" msg; kind = Unknown }))
       else (
         let model = Cli_common_json.member_str "model" rjson in
         let session_id = Cli_common_json.member_str "session_id" rjson in
         let stop_reason =
           parse_stop_reason (Cli_common_json.member_str "stop_reason" rjson)
         in
         let usage = parse_usage rjson in
         let content =
           if assistant_blocks <> []
           then assistant_blocks
           else
             (* No assistant blocks were streamed — fall back to the flat
               [result] string.  Keeps behaviour backward-compatible with
               the old [parse_json_result] path. *)
             [ Types.Text (Cli_common_json.member_str "result" rjson) ]
         in
         Ok
           { Types.id = session_id; model; stop_reason; content; usage; telemetry = None })
     with
     | Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
       Error
         (Http_client.NetworkError
            { message = "Failed to parse result line"; kind = Unknown }))
  | None ->
    if assistant_blocks = []
    then
      Error
        (Http_client.NetworkError
           { message = "No result or assistant message in stream output"; kind = Unknown })
    else (
      let id, model, usage =
        match last_assistant_msg lines with
        | Some msg ->
          ( Cli_common_json.member_str "id" msg
          , Cli_common_json.member_str "model" msg
          , parse_usage msg )
        | None -> "", "", None
      in
      Ok
        { Types.id
        ; model
        ; stop_reason = EndTurn
        ; content = assistant_blocks
        ; usage
        ; telemetry = None
        })
;;

(* ── Transport constructor ───────────────────────────── *)

(** Env vars stripped from every [claude] subprocess.

    [ANTHROPIC_API_KEY*]: when set in the parent process, [claude -p]
    authenticates as a metered API client (subject to the org's API
    spend limits) rather than using the user's OAuth/subscription
    session.  Agent integrations that rely on the subscription
    tier must not leak API_KEY env to the CLI.  The three names cover
    the canonical variable plus the conventional [_MAIN] / [_WORK]
    split some callers adopt.

    [CODEX_COMPANION_SESSION_ID]: scrubbed so our fresh value injected
    by {!subprocess_session_isolation_env} wins over whatever the parent shell
    inherited.  See that function's doc for the plugin-hook rationale.
    Key order in [Cli_common_subprocess.build_env] is [extras @ base];
    [execve] preserves duplicates and libuv's env parser takes the
    last match, so an un-scrubbed parent value would shadow our
    injection and defeat the isolation.  *)
let claude_cli_scrub_env =
  [ "ANTHROPIC_API_KEY"
  ; "ANTHROPIC_API_KEY_MAIN"
  ; "ANTHROPIC_API_KEY_WORK"
  ; "CODEX_COMPANION_SESSION_ID"
  ]
;;

(** Per-subprocess isolation env for Claude Code plugin hooks.

    The openai-codex plugin installs [session-lifecycle-hook.mjs]
    which runs on every SessionEnd event the [claude] binary fires.
    Its [cleanupSessionJobs] path loads the workspace state file and
    [terminateProcessTree]s any job row whose [sessionId] matches the
    current process's [CODEX_COMPANION_SESSION_ID] env var.

    When an OAS transport spawns a short-lived [claude -p] subprocess
    inside a long-lived parent Claude Code session, they share
    [CODEX_COMPANION_SESSION_ID] via env inheritance.  Each transient
    subprocess's SessionEnd therefore tears down the parent session's
    broker state and jobs — a silent outage vector for any automated
    runtime colocated with an interactive Claude Code session.

    Injecting a subprocess-unique id short-circuits the match in the
    hook: [cleanupSessionJobs] iterates the state file, finds no row
    with the freshly-minted sessionId, and returns without side
    effects.  The hook still runs (we don't disable it); it just
    becomes a no-op for our subprocess. *)
let subprocess_session_isolation_counter = Atomic.make 0

let subprocess_session_isolation_env () =
  let n = Atomic.fetch_and_add subprocess_session_isolation_counter 1 in
  [ ( "CODEX_COMPANION_SESSION_ID"
    , Printf.sprintf "oas-claude-%d-%d-%f" (Unix.getpid ()) n (Unix.gettimeofday ()) )
  ]
;;

let run ~sw ~mgr ~(config : config) ?stdin_content args =
  Cli_common_subprocess.run_collect
    ~sw
    ~mgr
    ~name:"claude"
    ~cwd:config.cwd
    ~extra_env:(subprocess_session_isolation_env ())
    ~scrub_env:claude_cli_scrub_env
    ?stdin_content
    ?cancel:config.cancel
    (config.claude_path :: args)
;;

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config) : Llm_transport.t =
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        let messages = Cli_common_prompt.non_system_messages req.messages in
        let prompt =
          Cli_common_prompt.prompt_of_messages
            ~include_tool_blocks:config.forward_tool_results
            messages
        in
        let system_prompt =
          Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages
        in
        if config.tool_use_via_stream_json
        then (
          (* Use stream-json internally so we can aggregate tool_use /
           thinking blocks.  [--output-format json] flattens these into
           the [result] string and we'd lose them. *)
          let args =
            build_args
              ~config
              ~req_config:req.config
              ?runtime_mcp_policy:req.runtime_mcp_policy
              ~prompt
              ~stream:true
              ~system_prompt
              ()
          in
          let argv = config.claude_path :: args in
          let seen_lines = ref [] in
          let on_line line =
            if String.trim line <> "" then seen_lines := line :: !seen_lines
          in
          match
            Cli_common_subprocess.run_stream_lines
              ~sw
              ~mgr
              ~name:"claude"
              ~cwd:config.cwd
              ~extra_env:(subprocess_session_isolation_env ())
              ~scrub_env:claude_cli_scrub_env
              ?stdin_content:(stdin_for_prompt prompt)
              ~on_line
              ?cancel:config.cancel
              argv
          with
          | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
          | Ok { stdout = _; stderr = _; latency_ms } ->
            let response = parse_stream_result (List.rev !seen_lines) in
            { Llm_transport.response; latency_ms })
        else (
          let args =
            build_args
              ~config
              ~req_config:req.config
              ?runtime_mcp_policy:req.runtime_mcp_policy
              ~prompt
              ~stream:false
              ~system_prompt
              ()
          in
          match run ~sw ~mgr ~config ?stdin_content:(stdin_for_prompt prompt) args with
          | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
          | Ok { stdout; stderr = _; latency_ms } ->
            let response = parse_json_result ~prompt (String.trim stdout) in
            { Llm_transport.response; latency_ms }))
  ; complete_stream =
      (fun ~on_event (req : Llm_transport.completion_request) ->
        let messages = Cli_common_prompt.non_system_messages req.messages in
        let prompt =
          Cli_common_prompt.prompt_of_messages
            ~include_tool_blocks:config.forward_tool_results
            messages
        in
        let system_prompt =
          Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages
        in
        let args =
          build_args
            ~config
            ~req_config:req.config
            ?runtime_mcp_policy:req.runtime_mcp_policy
            ~prompt
            ~stream:true
            ~system_prompt
            ()
        in
        let argv = config.claude_path :: args in
        let seen_lines = ref [] in
        let on_line line =
          if String.trim line <> ""
          then (
            seen_lines := line :: !seen_lines;
            List.iter on_event (events_of_line line))
        in
        match
          Cli_common_subprocess.run_stream_lines
            ~sw
            ~mgr
            ~name:"claude"
            ~cwd:config.cwd
            ~extra_env:(subprocess_session_isolation_env ())
            ~scrub_env:claude_cli_scrub_env
            ?stdin_content:(stdin_for_prompt prompt)
            ~on_line
            ?cancel:config.cancel
            argv
        with
        | Error _ as e -> e
        | Ok _ ->
          (* Final response is built from whatever lines we saw. *)
          parse_stream_result (List.rev !seen_lines))
  }
;;

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let%test "default_config claude_path" = default_config.claude_path = "claude"

let%test "parse_json_result success" =
  let json =
    {|{"type":"result","subtype":"success","is_error":false,"result":"hello world","model":"claude-sonnet-4","stop_reason":"end_turn","session_id":"s1","duration_api_ms":100}|}
  in
  match parse_json_result ~prompt:"" json with
  | Ok resp ->
    resp.model = "claude-sonnet-4"
    && resp.content = [ Types.Text "hello world" ]
    && resp.stop_reason = Types.EndTurn
  | Error _ -> false
;;

let%test "parse_json_result drops impossible cumulative usage" =
  let json =
    {|{"type":"result","subtype":"success","is_error":false,"result":"hello","model":"claude-sonnet-4","stop_reason":"end_turn","session_id":"s1","usage":{"input_tokens":3690186,"output_tokens":42}}|}
  in
  match parse_json_result ~prompt:"fake" json with
  | Ok resp ->
    (match resp.usage with
     | Some u -> u.input_tokens < 3690186
     | None -> false)
  | Error _ -> false
;;

let%test "parse_json_result error" =
  let json =
    {|{"type":"result","subtype":"error","is_error":true,"result":"rate limited","model":"m","stop_reason":"","session_id":"s1"}|}
  in
  match parse_json_result ~prompt:"" json with
  | Error (Http_client.NetworkError { message; _ }) -> String.length message > 0
  | _ -> false
;;

let%test "parse_json_result error_max_turns becomes ProviderTerminal Max_turns" =
  (* Pins the structural fix for downstream #10629: claude_code internal
     [error_max_turns] must surface as a structured terminal so the
     agent runtime can graceful-checkpoint instead of the cascade
     treating it as a transient network failure. *)
  let json =
    {|{"type":"result","subtype":"error_max_turns","is_error":true,"result":"","model":"m","stop_reason":"tool_use","session_id":"s1","num_turns":31}|}
  in
  match parse_json_result ~prompt:"" json with
  | Error (Http_client.ProviderTerminal { kind = Max_turns r; _ }) ->
    r.turns = 31 && r.limit = 31
  | _ -> false
;;

let%test "parse_json_result unknown error subtype stays NetworkError" =
  (* Whitelist guard: only known structured terminals promote to
     ProviderTerminal.  Unknown/legacy subtypes keep the retryable
     NetworkError path so a transient API error never gets silently
     promoted to "graceful checkpoint". *)
  let json =
    {|{"type":"result","subtype":"error_during_execution","is_error":true,"result":"unexpected","model":"m","stop_reason":"","session_id":"s1"}|}
  in
  match parse_json_result ~prompt:"" json with
  | Error (Http_client.NetworkError _) -> true
  | _ -> false
;;

let%test "parse_json_result invalid json" =
  match parse_json_result ~prompt:"" "not json" with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "events_of_line system init" =
  let line =
    {|{"type":"system","subtype":"init","model":"claude-sonnet-4","session_id":"s1"}|}
  in
  match events_of_line line with
  | [ Types.MessageStart { model = "claude-sonnet-4"; _ } ] -> true
  | _ -> false
;;

let%test "events_of_line assistant with text" =
  let line =
    {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"hello"}],"stop_reason":null,"usage":{"input_tokens":10,"output_tokens":5}}}|}
  in
  let events = events_of_line line in
  List.length events >= 3 (* ContentBlockStart + Delta + Stop + MessageDelta *)
;;

let%test "events_of_line result" =
  let line =
    {|{"type":"result","subtype":"success","is_error":false,"result":"ok","model":"m","stop_reason":"end_turn","session_id":"s1"}|}
  in
  match events_of_line line with
  | [ Types.MessageStop ] -> true
  | _ -> false
;;

let%test "events_of_line unknown type" =
  let line = {|{"type":"rate_limit_event","rate_limit_info":{}}|} in
  events_of_line line = []
;;

let%test "events_of_line invalid json" = events_of_line "not json" = []

let%test "build_args basic" =
  let args =
    build_args
      ~config:default_config
      ~req_config:(Provider_config.make ~kind:Anthropic ~model_id:"" ~base_url:"" ())
      ~prompt:"hello"
      ~stream:false
      ~system_prompt:None
      ()
  in
  List.mem "-p" args && List.mem "json" args
;;

let%test "build_args with model" =
  let args =
    build_args
      ~config:default_config
      ~req_config:
        (Provider_config.make ~kind:Anthropic ~model_id:"claude-sonnet-4" ~base_url:"" ())
      ~prompt:"hello"
      ~stream:true
      ~system_prompt:(Some "be helpful")
      ()
  in
  List.mem "--model" args
  && List.mem "claude-sonnet-4" args
  && List.mem "--system-prompt" args
  && List.mem "stream-json" args
  && List.mem "--verbose" args
;;

let%test "build_args omits auto model override" =
  let args =
    build_args
      ~config:default_config
      ~req_config:
        (Provider_config.make ~kind:Claude_code ~model_id:"auto" ~base_url:"" ())
      ~prompt:"hello"
      ~stream:false
      ~system_prompt:None
      ()
  in
  not (List.mem "--model" args)
;;

let%test "build_args clamps claude max_turns to provider hard cap" =
  let config = { default_config with max_turns = Some 39 } in
  let args =
    build_args
      ~config
      ~req_config:
        (Provider_config.make ~kind:Claude_code ~model_id:"auto" ~base_url:"" ())
      ~prompt:"hello"
      ~stream:false
      ~system_prompt:None
      ()
  in
  let rec has_clamped_flag = function
    | "--max-turns" :: "30" :: _ -> true
    | _ :: rest -> has_clamped_flag rest
    | [] -> false
  in
  has_clamped_flag args
;;

(* Strict-MCP/mcp-config pairing invariants are exercised by the
   env-driven tests further down (grep "strict MCP", "MCP_CONFIG
   fallback"), which use the shared with_env/with_unset helpers for
   env isolation. *)

let%test "parse_stop_reason variants" =
  parse_stop_reason "end_turn" = Types.EndTurn
  && parse_stop_reason "tool_use" = Types.StopToolUse
  && parse_stop_reason "max_tokens" = Types.MaxTokens
  && parse_stop_reason "stop_sequence" = Types.StopSequence
  && parse_stop_reason "unknown" = Types.Unknown "unknown"
;;

let%test "parse_stream_result from result line" =
  let lines =
    [ {|{"type":"system","subtype":"init","model":"m","session_id":"s1"}|}
    ; {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"hello"}],"stop_reason":null,"usage":{}}}|}
    ; {|{"type":"result","subtype":"success","is_error":false,"result":"hello","model":"m","stop_reason":"end_turn","session_id":"s1"}|}
    ]
  in
  match parse_stream_result lines with
  | Ok resp -> resp.content = [ Types.Text "hello" ]
  | Error _ -> false
;;

let%test "parse_stream_result fallback to assistant" =
  let lines =
    [ {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"fallback"}],"stop_reason":null,"usage":{}}}|}
    ]
  in
  match parse_stream_result lines with
  | Ok resp ->
    (match resp.content with
     | [ Types.Text "fallback" ] -> true
     | _ -> false)
  | Error _ -> false
;;

let%test "parse_stream_result no messages" =
  match parse_stream_result [] with
  | Error _ -> true
  | Ok _ -> false
;;

let%test "parse_stream_result error_max_turns becomes ProviderTerminal Max_turns" =
  (* Sync/stream parity for downstream #10629: the production hits show
     this exact JSON shape on stdout when claude_code subprocess exits
     1 due to its internal max_turns CLI default (currently 31). *)
  let lines =
    [ {|{"type":"system","subtype":"init","model":"m","session_id":"33e45115"}|}
    ; {|{"type":"result","subtype":"error_max_turns","duration_ms":273722,"duration_api_ms":180417,"is_error":true,"num_turns":31,"stop_reason":"tool_use","session_id":"33e45115"}|}
    ]
  in
  match parse_stream_result lines with
  | Error (Http_client.ProviderTerminal { kind = Max_turns r; _ }) ->
    r.turns = 31 && r.limit = 31
  | _ -> false
;;

let%test "parse_stream_result unknown error subtype stays NetworkError" =
  let lines =
    [ {|{"type":"result","subtype":"error_unknown_future","is_error":true,"result":"who knows","model":"m","stop_reason":"","session_id":"s1"}|}
    ]
  in
  match parse_stream_result lines with
  | Error (Http_client.NetworkError _) -> true
  | _ -> false
;;

let%test "parse_stream_result restores tool_use blocks" =
  let lines =
    [ {|{"type":"system","subtype":"init","model":"m","session_id":"s1"}|}
    ; {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"using a tool"},{"type":"tool_use","id":"tu_1","name":"calc","input":{"x":1}}],"stop_reason":null,"usage":{}}}|}
    ; {|{"type":"result","subtype":"success","is_error":false,"result":"unused","model":"m","stop_reason":"tool_use","session_id":"s1"}|}
    ]
  in
  match parse_stream_result lines with
  | Ok resp ->
    resp.stop_reason = Types.StopToolUse
    &&
      (match resp.content with
      | [ Types.Text "using a tool"; Types.ToolUse { id = "tu_1"; name = "calc"; _ } ] ->
        true
      | _ -> false)
  | Error _ -> false
;;

let%test "parse_stream_result aggregates across multiple assistant lines" =
  let lines =
    [ {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"text","text":"first"}],"stop_reason":null,"usage":{}}}|}
    ; {|{"type":"assistant","message":{"model":"m","id":"msg2","content":[{"type":"tool_use","id":"tu_2","name":"search","input":{"q":"hi"}}],"stop_reason":null,"usage":{}}}|}
    ]
  in
  match parse_stream_result lines with
  | Ok resp ->
    (match resp.content with
     | [ Types.Text "first"; Types.ToolUse { name = "search"; _ } ] -> true
     | _ -> false)
  | Error _ -> false
;;

let%test "parse_stream_result preserves thinking blocks" =
  let lines =
    [ {|{"type":"assistant","message":{"model":"m","id":"msg1","content":[{"type":"thinking","thinking":"let me think"},{"type":"text","text":"done"}],"stop_reason":null,"usage":{}}}|}
    ; {|{"type":"result","subtype":"success","is_error":false,"result":"unused","model":"m","stop_reason":"end_turn","session_id":"s1"}|}
    ]
  in
  match parse_stream_result lines with
  | Ok resp ->
    (match resp.content with
     | [ Types.Thinking { content = "let me think"; _ }; Types.Text "done" ] -> true
     | _ -> false)
  | Error _ -> false
;;

let%test "default_config has tool_use_via_stream_json=true" =
  default_config.tool_use_via_stream_json = true
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

let sample_req = Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ()

let%test "default: --strict-mcp-config omitted when no MCP config is available" =
  (* Previously this test asserted that --strict-mcp-config was emitted
     unconditionally.  That was the buggy behaviour — Claude CLI rejects
     --strict-mcp-config alone and exits 1 with no stderr.  The invariant
     is now: --strict-mcp-config is paired with a real config path, or
     neither flag is emitted. *)
  with_unset "OAS_CLAUDE_STRICT_MCP" (fun () ->
    with_unset "OAS_CLAUDE_MCP_CONFIG" (fun () ->
      with_unset "OAS_CLAUDE_DISALLOWED_TOOLS" (fun () ->
        let args =
          build_args
            ~config:default_config
            ~req_config:sample_req
            ~prompt:"hi"
            ~stream:false
            ~system_prompt:None
            ()
        in
        (not (List.mem "--strict-mcp-config" args))
        && (not (List.mem "--mcp-config" args))
        && not (List.mem "--disallowedTools" args))))
;;

let%test "env: OAS_CLAUDE_STRICT_MCP=1 alone no longer implies --strict-mcp-config" =
  (* OAS_CLAUDE_STRICT_MCP is a legacy downstream intent marker, not a
     trigger for Claude CLI flags.  Without
     a real config path, --strict-mcp-config must NOT be emitted. *)
  with_unset "OAS_CLAUDE_MCP_CONFIG" (fun () ->
    with_env "OAS_CLAUDE_STRICT_MCP" "1" (fun () ->
      let args =
        build_args
          ~config:default_config
          ~req_config:sample_req
          ~prompt:"hi"
          ~stream:false
          ~system_prompt:None
          ()
      in
      not (List.mem "--strict-mcp-config" args)))
;;

let%test "env: MCP_CONFIG fallback only when config.mcp_config is None" =
  with_env "OAS_CLAUDE_MCP_CONFIG" "/tmp/mcp.json" (fun () ->
    let args_default =
      build_args
        ~config:default_config
        ~req_config:sample_req
        ~prompt:"hi"
        ~stream:false
        ~system_prompt:None
        ()
    in
    let explicit_cfg = { default_config with mcp_config = Some "/tmp/explicit.json" } in
    let args_explicit =
      build_args
        ~config:explicit_cfg
        ~req_config:sample_req
        ~prompt:"hi"
        ~stream:false
        ~system_prompt:None
        ()
    in
    List.mem "/tmp/mcp.json" args_default
    && List.mem "/tmp/explicit.json" args_explicit
    && not (List.mem "/tmp/mcp.json" args_explicit))
;;

let%test "env: DISALLOWED_TOOLS splits on comma" =
  with_env "OAS_CLAUDE_DISALLOWED_TOOLS" "Bash,Write" (fun () ->
    let args =
      build_args
        ~config:default_config
        ~req_config:sample_req
        ~prompt:"hi"
        ~stream:false
        ~system_prompt:None
        ()
    in
    (* Each token emits its own --disallowedTools flag. *)
    List.mem "--disallowedTools" args && List.mem "Bash" args && List.mem "Write" args)
;;

let%test "subprocess_session_isolation_env injects fresh CODEX_COMPANION_SESSION_ID" =
  match subprocess_session_isolation_env () with
  | [ (k, v) ] ->
    k = "CODEX_COMPANION_SESSION_ID"
    && String.length v > 0
    &&
    let prefix = "oas-claude-" in
    String.length v >= String.length prefix
    && String.sub v 0 (String.length prefix) = prefix
  | _ -> false
;;

let%test "subprocess_session_isolation_env yields a new id per call" =
  let a = subprocess_session_isolation_env () in
  let b = subprocess_session_isolation_env () in
  let v_of = function
    | [ (_, v) ] -> v
    | _ -> ""
  in
  v_of a <> "" && v_of a <> v_of b
;;

let%test "claude_cli_scrub_env strips CODEX_COMPANION_SESSION_ID" =
  List.mem "CODEX_COMPANION_SESSION_ID" claude_cli_scrub_env
;;

let%test "claude_cli_scrub_env keeps ANTHROPIC_API_KEY entries" =
  List.mem "ANTHROPIC_API_KEY" claude_cli_scrub_env
  && List.mem "ANTHROPIC_API_KEY_MAIN" claude_cli_scrub_env
  && List.mem "ANTHROPIC_API_KEY_WORK" claude_cli_scrub_env
;;

let%test "prompt_exceeds_argv_budget: small prompt stays in argv" =
  not (prompt_exceeds_argv_budget "hello")
;;

let%test "prompt_exceeds_argv_budget: 1 MiB prompt routes to stdin" =
  prompt_exceeds_argv_budget (String.make (1 * 1024 * 1024) 'x')
;;

let%test "prompt_exceeds_argv_budget: OAS_CLAUDE_PROMPT_ARGV_THRESHOLD override" =
  with_env "OAS_CLAUDE_PROMPT_ARGV_THRESHOLD" "100" (fun () ->
    prompt_exceeds_argv_budget (String.make 200 'x')
    && not (prompt_exceeds_argv_budget (String.make 50 'x')))
;;

let%test "stdin_for_prompt: Some when over budget, None under" =
  let over = String.make (1 * 1024 * 1024) 'x' in
  stdin_for_prompt "hi" = None && stdin_for_prompt over = Some over
;;

let%test "build_args omits positional prompt when routing via stdin" =
  let big = String.make (1 * 1024 * 1024) 'x' in
  let args =
    build_args
      ~config:default_config
      ~req_config:sample_req
      ~prompt:big
      ~stream:false
      ~system_prompt:None
      ()
  in
  (* First two argv entries should be ["-p"; "--output-format"], NOT
     ["-p"; <big>] — the prompt must not appear anywhere in argv. *)
  (not (List.mem big args))
  &&
  match args with
  | "-p" :: "--output-format" :: _ -> true
  | _ -> false
;;

let%test "build_args keeps positional prompt when small" =
  let args =
    build_args
      ~config:default_config
      ~req_config:sample_req
      ~prompt:"hello"
      ~stream:false
      ~system_prompt:None
      ()
  in
  List.mem "hello" args
  &&
  match args with
  | "-p" :: "hello" :: _ -> true
  | _ -> false
;;

let%test "runtime MCP policy overrides legacy tool and MCP config wiring" =
  let config =
    { default_config with
      allowed_tools = [ "Bash" ]
    ; permission_mode = Some "bypassPermissions"
    ; mcp_config = Some "/tmp/legacy-mcp.json"
    }
  in
  let policy =
    { Llm_transport.empty_runtime_mcp_policy with
      servers =
        [ Llm_transport.Http_server
            { name = "example"; url = "http://127.0.0.1:9999/mcp"; headers = [] }
        ]
    ; allowed_server_names = [ "example" ]
    ; allowed_tool_names = [ "example_status" ]
    ; permission_mode = Some "plan"
    ; strict = true
    ; disable_builtin_tools = true
    }
  in
  let args =
    build_args
      ~config
      ~req_config:sample_req
      ~runtime_mcp_policy:policy
      ~prompt:"hi"
      ~stream:false
      ~system_prompt:None
      ()
  in
  List.mem "--tools" args
  && List.mem "" args
  && List.mem "--allowedTools" args
  && List.mem "mcp__example__example_status" args
  && List.mem "--permission-mode" args
  && List.mem "plan" args
  && List.mem "--strict-mcp-config" args
  && List.mem
       {|{"mcpServers":{"example":{"type":"http","url":"http://127.0.0.1:9999/mcp","headers":{}}}}|}
       args
  && (not (List.mem "Bash" args))
  && (not (List.mem "/tmp/legacy-mcp.json" args))
  && not (List.mem "bypassPermissions" args)
;;
