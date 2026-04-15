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

(* ── Prompt construction ─────────────────────────────── *)

(** Extract text from a content block. *)
let text_of_block = function
  | Types.Text t -> Some t
  | _ -> None

(** String label for a role variant. *)
let string_of_role = function
  | Types.System -> "System"
  | Types.User -> "User"
  | Types.Assistant -> "Assistant"
  | Types.Tool -> "Tool"

(** Format OAS messages into a single prompt string for [claude -p].
    Uses the last user message as the prompt. If multi-turn, prepends
    earlier exchanges as context. *)
let prompt_of_messages (messages : Types.message list) =
  let text_of_msg (m : Types.message) =
    List.filter_map text_of_block m.content |> String.concat "\n"
  in
  match List.rev messages with
  | [] -> ""
  | [m] -> text_of_msg m
  | last :: earlier ->
    let context = List.rev earlier |> List.map (fun (m : Types.message) ->
      Printf.sprintf "%s: %s" (string_of_role m.role) (text_of_msg m)
    ) |> String.concat "\n\n" in
    Printf.sprintf "%s\n\n%s" context (text_of_msg last)

(** Extract system prompt from config or first system message. *)
let system_prompt_of ~(config : config) ~(req_config : Provider_config.t)
    (messages : Types.message list) =
  ignore config;
  match req_config.system_prompt with
  | Some sp -> Some sp
  | None ->
    match messages with
    | { Types.role = System; content; _ } :: _ ->
      Some (List.filter_map text_of_block content |> String.concat "\n")
    | _ -> None

(** Filter out system messages (handled via --system-prompt). *)
let non_system_messages (messages : Types.message list) =
  List.filter (fun (m : Types.message) -> m.role <> Types.System) messages

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

(* ── Subprocess execution ────────────────────────────── *)

(** Run [claude -p] and collect all stdout. Returns (stdout, latency_ms). *)
let run_subprocess ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config) args =
  let t0 = Unix.gettimeofday () in
  try
    let r_stdout, w_stdout = Eio_unix.pipe sw in
    let r_stderr, w_stderr = Eio_unix.pipe sw in
    let env = match config.cwd with
      | Some dir ->
        let base = Unix.environment () |> Array.to_list in
        Array.of_list (Printf.sprintf "PWD=%s" dir :: base)
      | None -> Unix.environment ()
    in
    let proc = Eio.Process.spawn ~sw mgr
      ~stdout:(w_stdout :> Eio.Flow.sink_ty Eio.Resource.t)
      ~stderr:(w_stderr :> Eio.Flow.sink_ty Eio.Resource.t)
      ~env
      (config.claude_path :: args)
    in
    Eio.Flow.close w_stdout;
    Eio.Flow.close w_stderr;
    let stdout_buf = Buffer.create 4096 in
    let stderr_buf = Buffer.create 256 in
    (* Read stdout and stderr concurrently *)
    Eio.Fiber.both
      (fun () ->
        let reader = Eio.Buf_read.of_flow
          (r_stdout :> _ Eio.Flow.source) ~max_size:(16 * 1024 * 1024) in
        (try while true do
          Buffer.add_string stdout_buf (Eio.Buf_read.line reader);
          Buffer.add_char stdout_buf '\n'
        done with End_of_file -> ()))
      (fun () ->
        let reader = Eio.Buf_read.of_flow
          (r_stderr :> _ Eio.Flow.source) ~max_size:(1024 * 1024) in
        (try while true do
          Buffer.add_string stderr_buf (Eio.Buf_read.line reader);
          Buffer.add_char stderr_buf '\n'
        done with End_of_file -> ()));
    let status = Eio.Process.await proc in
    let latency_ms = int_of_float ((Unix.gettimeofday () -. t0) *. 1000.0) in
    let stdout_str = Buffer.contents stdout_buf in
    let stderr_str = Buffer.contents stderr_buf in
    (match status with
     | `Exited 0 -> Ok (stdout_str, latency_ms)
     | `Exited code ->
       let detail = if stderr_str <> "" then stderr_str
         else Printf.sprintf "exit code %d" code in
       Error (Http_client.NetworkError {
         message = Printf.sprintf "claude exited with code %d: %s" code detail })
     | `Signaled sig_num ->
       Error (Http_client.NetworkError {
         message = Printf.sprintf "claude killed by signal %d" sig_num }))
  with
  | Eio.Io _ as exn ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "subprocess I/O error: %s" (Printexc.to_string exn) })
  | Unix.Unix_error (err, fn, arg) ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err) })

(* ── JSON parsing ────────────────────────────────────── *)

let member_str key json =
  Yojson.Safe.Util.(json |> member key |> to_string_option)
  |> Option.value ~default:""

let member_int key json =
  Yojson.Safe.Util.(json |> member key |> to_int_option)
  |> Option.value ~default:0

let member_bool key json =
  Yojson.Safe.Util.(json |> member key |> to_bool_option)
  |> Option.value ~default:false

(** Parse the [usage] object from a result or assistant message. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usage" with
  | `Assoc _ as u ->
    Some { Types.input_tokens = member_int "input_tokens" u;
           output_tokens = member_int "output_tokens" u;
           cache_creation_input_tokens = member_int "cache_creation_input_tokens" u;
           cache_read_input_tokens = member_int "cache_read_input_tokens" u ; cost_usd = None }
  | _ -> None

let parse_stop_reason s = Types.stop_reason_of_string s

(** Parse a sync JSON result into api_response. *)
let parse_json_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    if member_bool "is_error" json then
      let msg = member_str "result" json in
      Error (Http_client.NetworkError {
        message = Printf.sprintf "Claude Code error: %s" msg })
    else
      let result_text = member_str "result" json in
      let model = member_str "model" json in
      let session_id = member_str "session_id" json in
      let stop_reason = parse_stop_reason (member_str "stop_reason" json) in
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
    let typ = member_str "type" json in
    match typ with
    | "system" ->
      let subtype = member_str "subtype" json in
      if subtype = "init" then
        let model = member_str "model" json in
        let session_id = member_str "session_id" json in
        [Types.MessageStart { id = session_id; model; usage = None }]
      else []
    | "assistant" ->
      let open Yojson.Safe.Util in
      let msg = json |> member "message" in
      let content = msg |> member "content" |> to_list in
      let block_events = List.mapi (fun idx block ->
        let content_type = member_str "type" block in
        let text = member_str "text" block in
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
      member_str "type" json = "result"
    with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ -> false
  ) lines in
  match result_line with
  | Some line -> parse_json_result line
  | None ->
    (* Fallback: try to assemble from assistant message *)
    let assistant_line = List.find_opt (fun line ->
      try
        let json = Yojson.Safe.from_string line in
        member_str "type" json = "assistant"
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
             let t = member_str "type" block in
             let text = member_str "text" block in
             match t with
             | "text" -> Some (Types.Text text)
             | _ -> None)
         in
         let model = member_str "model" msg in
         let id = member_str "id" msg in
         Ok { Types.id; model; stop_reason = EndTurn; content; usage = parse_usage msg; telemetry = None }
       with Yojson.Json_error _ | Yojson.Safe.Util.Type_error _ ->
         Error (Http_client.NetworkError {
           message = "Failed to parse assistant message" }))
     | None ->
       Error (Http_client.NetworkError {
         message = "No result or assistant message in stream output" }))

(* ── Transport constructor ───────────────────────────── *)

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
      let messages = non_system_messages req.messages in
      let prompt = prompt_of_messages messages in
      let system_prompt = system_prompt_of ~config ~req_config:req.config req.messages in
      let args = build_args ~config ~req_config:req.config
        ~prompt ~stream:false ~system_prompt in
      match run_subprocess ~sw ~mgr ~config args with
      | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
      | Ok (stdout, latency_ms) ->
        let response = parse_json_result (String.trim stdout) in
        { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      let messages = non_system_messages req.messages in
      let prompt = prompt_of_messages messages in
      let system_prompt = system_prompt_of ~config ~req_config:req.config req.messages in
      let args = build_args ~config ~req_config:req.config
        ~prompt ~stream:true ~system_prompt in
      match run_subprocess ~sw ~mgr ~config args with
      | Error _ as e -> e
      | Ok (stdout, _latency_ms) ->
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

let msg role content : Types.message =
  { role; content; name = None; tool_call_id = None }

let%test "default_config claude_path" =
  default_config.claude_path = "claude"

let%test "prompt_of_messages single user" =
  let msgs = [msg User [Text "hello"]] in
  prompt_of_messages msgs = "hello"

let%test "prompt_of_messages empty" =
  prompt_of_messages [] = ""

let%test "prompt_of_messages multi-turn" =
  let msgs = [
    msg User [Text "hi"];
    msg Assistant [Text "hello"];
    msg User [Text "how are you?"];
  ] in
  let result = prompt_of_messages msgs in
  (* Should have context + last message *)
  String.length result > 0
  && result |> String.split_on_char '\n' |> List.length > 1

let%test "non_system_messages filters system" =
  let msgs = [
    msg System [Text "be helpful"];
    msg User [Text "hi"];
  ] in
  let filtered = non_system_messages msgs in
  List.length filtered = 1

let%test "build_args omits auto model override" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"auto" ~base_url:"" ())
    ~prompt:"hello" ~stream:false ~system_prompt:None in
  not (List.mem "--model" args)

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
