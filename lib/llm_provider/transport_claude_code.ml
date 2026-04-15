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
}

let default_config = {
  claude_path = "claude";
  model = None;
  max_turns = None;
  allowed_tools = [];
  permission_mode = None;
  mcp_config = None;
  cwd = None;
}

(* Prompt shaping, JSON helpers, and subprocess orchestration live in the
   shared [Cli_common_*] modules to deduplicate logic across CLI transports. *)

(* ── CLI argument building ───────────────────────────── *)

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

(** Parse stream output: extract final api_response from "result" line. *)
let parse_stream_result lines =
  let result_line = List.find_opt (fun line ->
    try
      let json = Yojson.Safe.from_string line in
      Cli_common_json.member_str "type" json = "result"
    with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> false
  ) lines in
  match result_line with
  | Some line -> parse_json_result line
  | None ->
    (* Fallback: try to assemble from assistant message *)
    let assistant_line = List.find_opt (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        Cli_common_json.member_str "type" json = "assistant"
      with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> false
    ) lines in
    (match assistant_line with
     | Some line ->
       (try
         let json = Yojson.Safe.from_string line in
         let open Yojson.Safe.Util in
         let msg = json |> member "message" in
         let content = msg |> member "content" |> to_list
           |> List.filter_map (fun block ->
             let t = Cli_common_json.member_str "type" block in
             let text = Cli_common_json.member_str "text" block in
             match t with
             | "text" -> Some (Types.Text text)
             | _ -> None)
         in
         let model = Cli_common_json.member_str "model" msg in
         let id = Cli_common_json.member_str "id" msg in
         Ok { Types.id; model; stop_reason = EndTurn; content;
              usage = parse_usage msg; telemetry = None }
       with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
         Error (Http_client.NetworkError {
           message = "Failed to parse assistant message" }))
     | None ->
       Error (Http_client.NetworkError {
         message = "No result or assistant message in stream output" }))

(* ── Transport constructor ───────────────────────────── *)

let run ~sw ~mgr ~(config : config) args =
  Cli_common_subprocess.run_collect ~sw ~mgr
    ~name:"claude"
    ~cwd:config.cwd
    ~extra_env:[]
    (config.claude_path :: args)

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let system_prompt =
        Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages in
      let args = build_args ~config ~req_config:req.config
        ~prompt ~stream:false ~system_prompt in
      match run ~sw ~mgr ~config args with
      | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
      | Ok { stdout; stderr = _; latency_ms } ->
        let response = parse_json_result (String.trim stdout) in
        { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let system_prompt =
        Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages in
      let args = build_args ~config ~req_config:req.config
        ~prompt ~stream:true ~system_prompt in
      match run ~sw ~mgr ~config args with
      | Error _ as e -> e
      | Ok { stdout; stderr = _; latency_ms = _ } ->
        let lines = String.split_on_char '\n' stdout
          |> List.filter (fun s -> String.trim s <> "") in
        (* Emit events for each line *)
        List.iter (fun line ->
          let events = events_of_line line in
          List.iter on_event events
        ) lines;
        (* Parse final response from result line *)
        parse_stream_result lines);
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
