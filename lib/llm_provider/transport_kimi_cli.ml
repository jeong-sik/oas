(** Kimi CLI non-interactive transport.

    @since 0.169.0 *)

type config =
  { kimi_path : string
  ; model : string option
  ; cwd : string option
  ; config_file : string option
  ; mcp_config_files : string list
  ; mcp_config_json : string list
  ; forward_tool_results : bool
  ; extra_env : (string * string) list
  ; cancel : unit Eio.Promise.t option
  ; session_id : string option
  }

let default_config =
  { kimi_path = "kimi"
  ; model = Some "kimi-for-coding"
  ; cwd = None
  ; config_file = None
  ; mcp_config_files = []
  ; mcp_config_json = []
  ; forward_tool_results = true
  ; extra_env = []
  ; cancel = None
  ; session_id = None
  }
;;

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules. *)

(* ── CLI argument building ───────────────────────────── *)

(* Kimi CLI imports setproctitle before it starts the request. On macOS,
   long/non-ASCII argv payloads can make setproctitle.getproctitle raise a
   UnicodeDecodeError before the provider sees the prompt. Keep argv small
   and send large prompts through stdin instead. *)
let default_prompt_argv_threshold = 32 * 1024

let prompt_argv_threshold () =
  Cli_common_env.int
    ~default:default_prompt_argv_threshold
    "OAS_KIMI_PROMPT_ARGV_THRESHOLD"
;;

let prompt_exceeds_argv_budget prompt = String.length prompt >= prompt_argv_threshold ()
let sanitize_for_kimi prompt = Utf8_sanitize.sanitize prompt

let prompt_contains_non_ascii prompt =
  let rec loop idx =
    idx < String.length prompt && (Char.code prompt.[idx] > 0x7f || loop (idx + 1))
  in
  loop 0
;;

let prompt_needs_stdin prompt =
  prompt_exceeds_argv_budget prompt || prompt_contains_non_ascii prompt
;;

let stdin_for_prompt prompt =
  let prompt = sanitize_for_kimi prompt in
  if prompt_needs_stdin prompt then Some prompt else None
;;

let prompt_for_cli prompt = Utf8_sanitize.sanitize prompt

let cli_model_override ~(config : config) ~(req_config : Provider_config.t) =
  let override =
    match String.trim req_config.model_id |> String.lowercase_ascii with
    | "" | "auto" -> config.model
    | _ -> Some (String.trim req_config.model_id)
  in
  (match override, Capabilities.kimi_cli_capabilities.supported_models with
   | Some m, Some supported ->
     if not (List.mem (String.lowercase_ascii m) supported)
     then
       Eio.traceln
         "[warn] [kimi_cli] Unsupported model %s requested. Kimi CLI officially supports \
          %s"
         m
         (String.concat ", " supported)
   | _ -> ());
  override
;;

let build_args ~(config : config) ~(req_config : Provider_config.t) ~prompt =
  let prompt = sanitize_for_kimi prompt in
  let prompt_via_stdin = prompt_needs_stdin prompt in
  let args = ref [ config.kimi_path; "--print"; "--output-format"; "stream-json" ] in
  let add a = args := !args @ a in
  if not prompt_via_stdin then add [ "-p"; prompt ];
  (match cli_model_override ~config ~req_config with
   | Some m -> add [ "--model"; m ]
   | None -> ());
  (match config.cwd with
   | Some dir when String.trim dir <> "" -> add [ "--work-dir"; dir ]
   | _ -> ());
  (match config.config_file with
   | Some path when String.trim path <> "" -> add [ "--config-file"; path ]
   | _ -> ());
  List.iter (fun path -> add [ "--mcp-config-file"; path ]) config.mcp_config_files;
  List.iter (fun json -> add [ "--mcp-config"; json ]) config.mcp_config_json;
  (match req_config.enable_thinking with
   | Some true -> add [ "--thinking" ]
   | Some false -> add [ "--no-thinking" ]
   | None -> ());
  (match config.session_id with
   | Some id when String.trim id <> "" -> add [ "--session"; id ]
   | _ -> ());
  !args
;;

(* ── JSON parsing ────────────────────────────────────── *)

let json_of_argument_string = function
  | None | Some "" -> `Assoc []
  | Some s ->
    (try Yojson.Safe.from_string s with
     | Yojson.Json_error _ -> `Assoc [])
;;

let blocks_of_message_content json =
  match json with
  | `String s when String.trim s = "" -> []
  | `String s -> [ Types.Text s ]
  | `List items -> List.filter_map Api_common.content_block_of_json items
  | `Null -> []
  | other -> [ Types.Text (Yojson.Safe.to_string other) ]
;;

let tool_use_of_json json =
  let open Yojson.Safe.Util in
  try
    let fn = json |> member "function" in
    let id = Cli_common_json.member_str "id" json in
    let name = Cli_common_json.member_str "name" fn in
    let args = fn |> member "arguments" |> to_string_option |> json_of_argument_string in
    Some (Types.ToolUse { id; name; input = args })
  with
  | Type_error _ -> None
;;

let tool_result_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "tool_call_id" |> to_string_option with
  | Some tool_use_id ->
    let content_json = json |> member "content" in
    let content, parsed_json =
      match content_json with
      | `String s -> s, Types.try_parse_json s
      | `Null -> "", None
      | other -> Yojson.Safe.to_string other, Some other
    in
    Some (Types.ToolResult { tool_use_id; content; is_error = false; json = parsed_json })
  | None -> None
;;

let parse_json_line line = Yojson.Safe.from_string (Utf8_sanitize.sanitize line)

let blocks_of_output_line line =
  let open Yojson.Safe.Util in
  try
    let json = parse_json_line line in
    match json |> member "role" |> to_string_option with
    | Some "assistant" ->
      let content = blocks_of_message_content (json |> member "content") in
      let tool_uses =
        match json |> member "tool_calls" with
        | `List calls -> List.filter_map tool_use_of_json calls
        | _ -> []
      in
      content @ tool_uses
    | Some "tool" ->
      (match tool_result_of_json json with
       | Some block -> [ block ]
       | None -> [])
    | _ -> []
  with
  | Yojson.Json_error _ | Type_error _ -> []
;;

let response_id_of_lines lines =
  let open Yojson.Safe.Util in
  let find_id line =
    try
      let json = parse_json_line line in
      match json |> member "id" |> to_string_option with
      | Some id when String.trim id <> "" -> Some id
      | _ ->
        (match json |> member "session_id" |> to_string_option with
         | Some id when String.trim id <> "" -> Some id
         | _ -> None)
    with
    | Yojson.Json_error _ | Type_error _ -> None
  in
  List.find_map find_id lines |> Option.value ~default:"kimi-print"
;;

let response_model_of_lines ~model_id lines =
  let open Yojson.Safe.Util in
  let find_model line =
    try
      let json = parse_json_line line in
      match json |> member "model" |> to_string_option with
      | Some m when String.trim m <> "" -> Some m
      | _ -> None
    with
    | Yojson.Json_error _ | Type_error _ -> None
  in
  List.find_map find_model lines |> Option.value ~default:model_id
;;

let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    Some
      { Types.input_tokens = Cli_common_json.member_int "input_tokens" u
      ; output_tokens = Cli_common_json.member_int "output_tokens" u
      ; cache_creation_input_tokens =
          Cli_common_json.member_int "cache_creation_input_tokens" u
      ; cache_read_input_tokens = Cli_common_json.member_int "cache_read_input_tokens" u
      ; cost_usd = None
      }
  | _ -> None
;;

let usage_of_lines lines =
  let find_usage line =
    let open Yojson.Safe.Util in
    try
      let json = parse_json_line line in
      parse_usage json
    with
    | Yojson.Json_error _ | Type_error _ -> None
  in
  List.find_map find_usage lines
;;

let parse_jsonl_result ~model_id ~prompt lines =
  let content = List.concat_map blocks_of_output_line lines in
  if content = []
  then
    Error
      (Http_client.NetworkError
         { message = "no messages parsed from kimi output"; kind = Unknown })
  else (
    let response_text =
      List.filter_map
        (function
          | Types.Text t -> Some t
          | _ -> None)
        content
      |> String.concat ""
    in
    let usage =
      Some (Cli_common_prompt.estimate_usage ~prompt ~response_text ~model_id)
    in
    Ok
      { Types.id = response_id_of_lines lines
      ; model = response_model_of_lines ~model_id lines
      ; stop_reason = Types.EndTurn
      ; content
      ; usage
      ; telemetry = None
      })
;;

(* ── Stream events ───────────────────────────────────── *)

let events_of_block ~index = function
  | Types.Text text ->
    [ Types.ContentBlockStart
        { index; content_type = "text"; tool_id = None; tool_name = None }
    ; Types.ContentBlockDelta { index; delta = Types.TextDelta text }
    ; Types.ContentBlockStop { index }
    ]
  | Types.Thinking { content; _ } ->
    [ Types.ContentBlockStart
        { index; content_type = "thinking"; tool_id = None; tool_name = None }
    ; Types.ContentBlockDelta { index; delta = Types.ThinkingDelta content }
    ; Types.ContentBlockStop { index }
    ]
  | Types.ToolUse { id; name; input } ->
    [ Types.ContentBlockStart
        { index; content_type = "tool_use"; tool_id = Some id; tool_name = Some name }
    ; Types.ContentBlockDelta
        { index; delta = Types.InputJsonDelta (Yojson.Safe.to_string input) }
    ; Types.ContentBlockStop { index }
    ]
  | Types.ToolResult { tool_use_id; content; _ } ->
    [ Types.ContentBlockStart
        { index
        ; content_type = "tool_result"
        ; tool_id = Some tool_use_id
        ; tool_name = None
        }
    ; Types.ContentBlockDelta { index; delta = Types.TextDelta content }
    ; Types.ContentBlockStop { index }
    ]
  | Types.RedactedThinking _ | Types.Image _ | Types.Document _ | Types.Audio _ -> []
;;

let emit_blocks ~on_event ~start_index blocks =
  List.fold_left
    (fun index block ->
       match events_of_block ~index block with
       | [] -> index
       | events ->
         List.iter on_event events;
         index + 1)
    start_index
    blocks
;;

(* ── Error classification ────────────────────────────── *)

let starts_with s prefix =
  let lp = String.length prefix in
  String.length s >= lp && String.sub s 0 lp = prefix
;;

let exit_code_of_message message =
  let prefix = "kimi exited with code " in
  if not (starts_with message prefix)
  then None
  else (
    match String.index_from_opt message (String.length prefix) ':' with
    | None -> None
    | Some colon ->
      let raw =
        String.sub message (String.length prefix) (colon - String.length prefix)
        |> String.trim
      in
      int_of_string_opt raw)
;;

let classify_cli_error = function
  | Error (Http_client.NetworkError { message; _ }) as err ->
    (match exit_code_of_message message with
     | Some 1 ->
       Error
         (Http_client.AcceptRejected
            { reason =
                "kimi_cli rejected the request (exit 1). "
                ^ "This is usually a permanent auth/config/model error rather "
                ^ "than a transient transport failure. "
                ^ message
            })
     | _ -> err)
  | other -> other
;;

(* ── Transport constructor ───────────────────────────── *)

let warn_external_tools_once warned tools =
  if !warned || tools = []
  then ()
  else (
    warned := true;
    Diag.warn
      "transport_kimi_cli"
      "kimi_cli print mode ignores OAS req.tools. Provider-native built-in tools and \
       configured MCP servers remain available; external OAS tool callbacks require a \
       future wire-mode transport.")
;;

(* Drop the first [n] elements of a list.  O(n) but n is the delta between
   successive turns, which is small. *)
let rec drop n = function
  | [] -> []
  | _ :: t when n > 0 -> drop (n - 1) t
  | l -> l
;;

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config) : Llm_transport.t =
  let warned = ref false in
  (* When [session_id] is set we track how many non-system messages have
     already been sent so that subsequent turns can transmit only the delta.
     This avoids re-transmitting the entire conversation history on every
     turn, which is the primary source of token waste in session-reuse mode. *)
  let previous_msg_count = ref 0 in
  let prepare_prompt_and_messages (req : Llm_transport.completion_request) =
    let all_messages = Cli_common_prompt.non_system_messages req.messages in
    let system_prompt =
      Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages
    in
    let resume_existing_session =
      match config.session_id with
      | Some _
        when !previous_msg_count > 0 && List.length all_messages > !previous_msg_count ->
        true
      | _ -> false
    in
    let messages_to_send =
      if resume_existing_session
      then drop !previous_msg_count all_messages
      else all_messages
    in
    let prompt =
      Cli_common_prompt.prompt_of_messages
        ~include_tool_blocks:config.forward_tool_results
        messages_to_send
      |> fun prompt ->
      let prompt =
        if resume_existing_session
        then
          (* When resuming a session the CLI already has the system prompt
             and prior turns in its session file; repeating it would bloat
             the prompt and confuse the context. *)
          prompt
        else Cli_common_prompt.prompt_with_system_prompt ~prompt ~system_prompt
      in
      prompt_for_cli prompt
    in
    previous_msg_count := List.length all_messages;
    prompt, resume_existing_session
  in
  { complete_sync =
      (fun (req : Llm_transport.completion_request) ->
        warn_external_tools_once warned req.tools;
        let prompt, _resume_existing_session = prepare_prompt_and_messages req in
        let prompt = sanitize_for_kimi prompt in
        let model_id =
          Option.value
            ~default:"kimi-for-coding"
            (cli_model_override ~config ~req_config:req.config)
        in
        let argv = build_args ~config ~req_config:req.config ~prompt in
        let seen_lines = ref [] in
        let on_line line =
          if String.trim line <> "" then seen_lines := line :: !seen_lines
        in
        match
          Cli_common_subprocess.run_stream_lines
            ~sw
            ~mgr
            ~name:"kimi"
            ~cwd:config.cwd
            ~extra_env:config.extra_env
            ?stdin_content:(stdin_for_prompt prompt)
            ~on_line
            ?cancel:config.cancel
            argv
        with
        | Error _ as e ->
          { Llm_transport.response = classify_cli_error e; latency_ms = 0 }
        | Ok { latency_ms; _ } ->
          let response = parse_jsonl_result ~model_id ~prompt (List.rev !seen_lines) in
          { Llm_transport.response; latency_ms })
  ; complete_stream =
      (fun ~on_event (req : Llm_transport.completion_request) ->
        warn_external_tools_once warned req.tools;
        let prompt, _resume_existing_session = prepare_prompt_and_messages req in
        let prompt = sanitize_for_kimi prompt in
        let model_id =
          Option.value
            ~default:"kimi-for-coding"
            (cli_model_override ~config ~req_config:req.config)
        in
        let argv = build_args ~config ~req_config:req.config ~prompt in
        let seen_lines = ref [] in
        let next_index = ref 0 in
        let started = ref false in
        let ensure_started () =
          if not !started
          then (
            started := true;
            on_event
              (Types.MessageStart { id = "kimi-print"; model = model_id; usage = None }))
        in
        let on_line line =
          if String.trim line <> ""
          then (
            seen_lines := line :: !seen_lines;
            let blocks = blocks_of_output_line line in
            if blocks <> []
            then (
              ensure_started ();
              next_index := emit_blocks ~on_event ~start_index:!next_index blocks))
        in
        match
          classify_cli_error
            (Cli_common_subprocess.run_stream_lines
               ~sw
               ~mgr
               ~name:"kimi"
               ~cwd:config.cwd
               ~extra_env:config.extra_env
               ?stdin_content:(stdin_for_prompt prompt)
               ~on_line
               ?cancel:config.cancel
               argv)
        with
        | Error _ as e -> e
        | Ok _ ->
          (match parse_jsonl_result ~model_id ~prompt (List.rev !seen_lines) with
           | Error _ as e -> e
           | Ok resp as ok ->
             if !started
             then (
               on_event
                 (Types.MessageDelta
                    { stop_reason = Some resp.stop_reason; usage = resp.usage });
               on_event Types.MessageStop)
             else Cli_common_synthetic_events.replay ~on_event resp;
             ok))
  }
;;

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let kimi_req ?(model_id = "auto") ?enable_thinking () =
  Provider_config.make
    ~kind:Provider_config.Kimi_cli
    ~model_id
    ~base_url:""
    ?enable_thinking
    ()
;;

let%test "default_config uses kimi-for-coding" =
  default_config.model = Some "kimi-for-coding"
;;

let%test "build_args basic" =
  let args = build_args ~config:default_config ~req_config:(kimi_req ()) ~prompt:"hi" in
  args
  = [ "kimi"
    ; "--print"
    ; "--output-format"
    ; "stream-json"
    ; "-p"
    ; "hi"
    ; "--model"
    ; "kimi-for-coding"
    ]
;;

let%test "prompt_for_cli sanitizes invalid utf8 before argv or stdin" =
  prompt_for_cli "abc\xEF\x00d" = "abc\xEF\xBF\xBD d"
;;

let%test "build_args with work dir, mcp, and thinking" =
  let config =
    { default_config with
      cwd = Some "/tmp/work"
    ; mcp_config_files = [ "/tmp/mcp.json" ]
    ; mcp_config_json = [ "{\"mcpServers\":{}}" ]
    }
  in
  let args =
    build_args ~config ~req_config:(kimi_req ~enable_thinking:true ()) ~prompt:"hi"
  in
  List.mem "--work-dir" args
  && List.mem "/tmp/work" args
  && List.mem "--mcp-config-file" args
  && List.mem "/tmp/mcp.json" args
  && List.mem "--mcp-config" args
  && List.mem "{\"mcpServers\":{}}" args
  && List.mem "--thinking" args
;;

let%test "build_args uses request model over default" =
  let args =
    build_args
      ~config:default_config
      ~req_config:(kimi_req ~model_id:"kimi-k2.6" ())
      ~prompt:"hi"
  in
  List.mem "--model" args
  && List.mem "kimi-k2.6" args
  && not (List.mem "kimi-for-coding" args)
;;

let%test "build_args routes threshold prompt via stdin" =
  let big = String.make (1 * 1024 * 1024) 'x' in
  let args = build_args ~config:default_config ~req_config:(kimi_req ()) ~prompt:big in
  (not (List.mem big args)) && not (List.mem "-p" args)
;;

let%test "build_args routes large prompt via stdin" =
  let big = String.make (70 * 1024) 'x' in
  let args = build_args ~config:default_config ~req_config:(kimi_req ()) ~prompt:big in
  (not (List.mem big args)) && not (List.mem "-p" args)
;;

let%test "build_args sanitizes broken utf8 prompt before argv" =
  let bad = "prefix\x80suffix" in
  let args = build_args ~config:default_config ~req_config:(kimi_req ()) ~prompt:bad in
  (not (List.mem bad args)) && not (List.mem "-p" args)
;;

let%test "build_args adds session id when configured" =
  let config = { default_config with session_id = Some "sess-abc" } in
  let args = build_args ~config ~req_config:(kimi_req ()) ~prompt:"next" in
  List.mem "--session" args && List.mem "sess-abc" args
;;

let%test "build_args uses config-file flag for config_file" =
  let config = { default_config with config_file = Some "/tmp/kimi.toml" } in
  let args = build_args ~config ~req_config:(kimi_req ()) ~prompt:"first" in
  List.mem "--config-file" args
  && List.mem "/tmp/kimi.toml" args
  && not (List.mem "--config" args)
;;

let%test "parse_jsonl_result restores tool trace" =
  let lines =
    [ {|{"role":"assistant","content":"Checking files","tool_calls":[{"type":"function","id":"tc_1","function":{"name":"Shell","arguments":"{\"command\":\"ls\"}"}}]}|}
    ; {|{"role":"tool","tool_call_id":"tc_1","content":"README.md"}|}
    ; {|{"role":"assistant","content":"Done"}|}
    ]
  in
  match parse_jsonl_result ~model_id:"kimi-for-coding" ~prompt:"" lines with
  | Ok resp ->
    (match resp.content with
     | [ Types.Text "Checking files"
       ; Types.ToolUse { id = "tc_1"; name = "Shell"; input }
       ; Types.ToolResult { tool_use_id = "tc_1"; content = "README.md"; _ }
       ; Types.Text "Done"
       ] -> input = `Assoc [ "command", `String "ls" ]
     | _ -> false)
  | Error _ -> false
;;

let%test "parse_jsonl_result accepts array-form content" =
  let lines = [ {|{"role":"assistant","content":[{"type":"text","text":"hello"}]}|} ] in
  match parse_jsonl_result ~model_id:"kimi-for-coding" ~prompt:"" lines with
  | Ok resp -> resp.content = [ Types.Text "hello" ]
  | Error _ -> false
;;

let%test "parse_jsonl_result sanitizes broken utf8 output lines" =
  let lines = [ "{\"role\":\"assistant\",\"content\":\"hello\x80\"}" ] in
  match parse_jsonl_result ~model_id:"kimi-for-coding" ~prompt:"" lines with
  | Ok { content = [ Types.Text text ]; _ } -> text = "hello\xEF\xBF\xBD"
  | _ -> false
;;

let%test "classify_cli_error exit 1 becomes AcceptRejected" =
  match
    classify_cli_error
      (Error
         (Http_client.NetworkError
            { message = "kimi exited with code 1: auth failed"; kind = Unknown }))
  with
  | Error (Http_client.AcceptRejected { reason }) -> String.length reason > 0
  | _ -> false
;;

let%test "parse_usage extracts input and output tokens" =
  let json =
    Yojson.Safe.from_string {|{"usage":{"input_tokens":10,"output_tokens":20}}|}
  in
  match parse_usage json with
  | Some u ->
    u.input_tokens = 10
    && u.output_tokens = 20
    && u.cache_creation_input_tokens = 0
    && u.cache_read_input_tokens = 0
    && u.cost_usd = None
  | None -> false
;;

let%test "parse_usage missing keys default to zero" =
  let json = Yojson.Safe.from_string {|{"usage":{"input_tokens":5}}|} in
  match parse_usage json with
  | Some u ->
    u.input_tokens = 5
    && u.output_tokens = 0
    && u.cache_creation_input_tokens = 0
    && u.cache_read_input_tokens = 0
  | None -> false
;;

let%test "parse_usage no usage field returns None" =
  let json = Yojson.Safe.from_string {|{"role":"assistant"}|} in
  parse_usage json = None
;;

let%test "usage_of_lines finds usage across lines" =
  let lines =
    [ {|{"role":"assistant","content":"hi"}|}
    ; {|{"usage":{"input_tokens":1,"output_tokens":2}}|}
    ]
  in
  match usage_of_lines lines with
  | Some u -> u.input_tokens = 1 && u.output_tokens = 2
  | None -> false
;;

let%test "parse_jsonl_result suppresses CLI usage when present" =
  let lines =
    [ {|{"role":"assistant","content":"hello"}|}
    ; {|{"usage":{"input_tokens":10,"output_tokens":20}}|}
    ]
  in
  match parse_jsonl_result ~model_id:"kimi-for-coding" ~prompt:"" lines with
  | Ok resp -> Option.is_some resp.usage
  | Error _ -> false
;;

let%test "parse_jsonl_result keeps None when usage absent" =
  let lines = [ {|{"role":"assistant","content":"hello"}|} ] in
  match parse_jsonl_result ~model_id:"kimi-for-coding" ~prompt:"" lines with
  | Ok resp -> Option.is_some resp.usage
  | Error _ -> false
;;
