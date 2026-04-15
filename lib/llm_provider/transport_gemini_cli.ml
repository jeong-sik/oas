(** Gemini CLI non-interactive transport.

    @since 0.133.0 *)

type config = {
  gemini_path: string;
  model: string option;
  yolo: bool;
  cwd: string option;
}

let default_config = {
  gemini_path = "gemini";
  model = None;
  yolo = true;
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

(** Format OAS messages into a single prompt string.
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
    ~prompt ~system_prompt =
  let args = ref [config.gemini_path; "--output-format"; "json"; "-p"; prompt] in
  let add a = args := !args @ a in
  if config.yolo then add ["--yolo"];
  (* "auto" means "use the CLI's configured default", so omit [--model]. *)
  let model = match String.trim req_config.model_id |> String.lowercase_ascii with
    | "" | "auto" -> config.model
    | _ -> Some req_config.model_id
  in
  (match model with Some m -> add ["--model"; m] | None -> ());
  (match system_prompt with Some s -> add ["--system-prompt"; s] | None -> ());
  !args

(* ── Subprocess execution ────────────────────────────── *)

(** Run gemini CLI and collect all stdout. Returns (stdout, latency_ms). *)
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
      args
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
         message = Printf.sprintf "gemini exited with code %d: %s" code detail })
     | `Signaled sig_num ->
       Error (Http_client.NetworkError {
         message = Printf.sprintf "gemini killed by signal %d" sig_num }))
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

(** Parse usage metadata from Gemini JSON response.
    Gemini returns: {usageMetadata: {promptTokenCount, candidatesTokenCount, cachedContentTokenCount}} *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usageMetadata" with
  | `Assoc _ as u ->
    Some { Types.input_tokens = member_int "promptTokenCount" u;
           output_tokens = member_int "candidatesTokenCount" u;
           cache_creation_input_tokens = 0;
           cache_read_input_tokens = member_int "cachedContentTokenCount" u;
           cost_usd = None }
  | _ -> None

(** Parse the Gemini CLI --output-format json result into api_response.
    Expected format: {"response": "text", "usageMetadata": {...}} *)
let parse_json_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let response_text = member_str "response" json in
    let usage = parse_usage json in
    Ok { Types.id = "";
         model = "gemini";
         stop_reason = Types.EndTurn;
         content = [Text response_text];
         usage; telemetry = None }
  with
  | Yojson.Json_error msg ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "JSON parse error: %s" msg })

(* ── Synthetic stream events ─────────────────────────── *)

(** Emit synthetic SSE events from a sync api_response.
    Gemini CLI does not support streaming, so we replay the
    full response as a sequence of events. *)
let emit_synthetic_events ~on_event (resp : Types.api_response) =
  on_event (Types.MessageStart { id = resp.id; model = resp.model; usage = resp.usage });
  List.iteri (fun idx block ->
    let content_type = match block with
      | Types.Text _ -> "text"
      | Types.Thinking _ -> "thinking"
      | _ -> "text"
    in
    let delta = match block with
      | Types.Text t -> Types.TextDelta t
      | Types.Thinking { content; _ } -> Types.ThinkingDelta content
      | _ -> Types.TextDelta ""
    in
    on_event (Types.ContentBlockStart {
      index = idx; content_type; tool_id = None; tool_name = None });
    on_event (Types.ContentBlockDelta { index = idx; delta });
    on_event (Types.ContentBlockStop { index = idx })
  ) resp.content;
  on_event (Types.MessageDelta {
    stop_reason = Some resp.stop_reason; usage = resp.usage });
  on_event Types.MessageStop

(* ── Transport constructor ───────────────────────────── *)

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
      let messages = non_system_messages req.messages in
      let prompt = prompt_of_messages messages in
      let system_prompt = system_prompt_of ~config ~req_config:req.config req.messages in
      let args = build_args ~config ~req_config:req.config
        ~prompt ~system_prompt in
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
        ~prompt ~system_prompt in
      match run_subprocess ~sw ~mgr ~config args with
      | Error _ as e -> e
      | Ok (stdout, _latency_ms) ->
        let result = parse_json_result (String.trim stdout) in
        (match result with
         | Ok resp ->
           emit_synthetic_events ~on_event resp;
           Ok resp
         | Error _ as e -> e));
  }

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let msg role content : Types.message =
  { role; content; name = None; tool_call_id = None }

let%test "default_config gemini_path" =
  default_config.gemini_path = "gemini"

let%test "default_config yolo true" =
  default_config.yolo = true

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
  String.length result > 0
  && result |> String.split_on_char '\n' |> List.length > 1

let%test "non_system_messages filters system" =
  let msgs = [
    msg System [Text "be helpful"];
    msg User [Text "hi"];
  ] in
  let filtered = non_system_messages msgs in
  List.length filtered = 1

let%test "build_args basic with yolo" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~system_prompt:None in
  List.mem "-p" args
  && List.mem "json" args
  && List.mem "--yolo" args

let%test "build_args without yolo" =
  let config = { default_config with yolo = false } in
  let args = build_args ~config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~system_prompt:None in
  List.mem "-p" args
  && not (List.mem "--yolo" args)

let%test "build_args with model" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"gemini-2.5-pro" ~base_url:"" ())
    ~prompt:"hello" ~system_prompt:(Some "be helpful") in
  List.mem "--model" args
  && List.mem "gemini-2.5-pro" args
  && List.mem "--system-prompt" args

let%test "build_args omits auto model override" =
  let args = build_args ~config:default_config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"auto" ~base_url:"" ())
    ~prompt:"hello" ~system_prompt:None in
  not (List.mem "--model" args)

let%test "parse_json_result success" =
  let json = {|{"response":"hello world","usageMetadata":{"promptTokenCount":10,"candidatesTokenCount":5,"cachedContentTokenCount":2}}|} in
  match parse_json_result json with
  | Ok resp ->
    resp.content = [Types.Text "hello world"]
    && resp.stop_reason = Types.EndTurn
    && (match resp.usage with
        | Some u -> u.input_tokens = 10 && u.output_tokens = 5 && u.cache_read_input_tokens = 2
        | None -> false)
  | Error _ -> false

let%test "parse_json_result no usage" =
  let json = {|{"response":"hello"}|} in
  match parse_json_result json with
  | Ok resp ->
    resp.content = [Types.Text "hello"]
    && resp.usage = None
  | Error _ -> false

let%test "parse_json_result invalid json" =
  match parse_json_result "not json" with
  | Error _ -> true
  | Ok _ -> false
