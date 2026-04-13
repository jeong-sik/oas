(** Codex CLI non-interactive transport.

    @since 0.133.0 *)

type config = {
  codex_path: string;
  cwd: string option;
}

let default_config = {
  codex_path = "codex";
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

(** Filter out system messages (prepended to prompt as context). *)
let non_system_messages (messages : Types.message list) =
  List.filter (fun (m : Types.message) -> m.role <> Types.System) messages

(* ── CLI argument building ───────────────────────────── *)

let build_args ~(config : config) ~prompt =
  [config.codex_path; "exec"; prompt]

(* ── Subprocess execution ────────────────────────────── *)

(** Run codex exec and collect all stdout. Returns (stdout, latency_ms). *)
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
         message = Printf.sprintf "codex exited with code %d: %s" code detail })
     | `Signaled sig_num ->
       Error (Http_client.NetworkError {
         message = Printf.sprintf "codex killed by signal %d" sig_num }))
  with
  | Eio.Io _ as exn ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "subprocess I/O error: %s" (Printexc.to_string exn) })
  | Unix.Unix_error (err, fn, arg) ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err) })

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

(* ── Synthetic stream events ─────────────────────────── *)

(** Emit synthetic SSE events from a sync api_response.
    Codex CLI does not support streaming, so we replay the
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
      let args = build_args ~config ~prompt in
      match run_subprocess ~sw ~mgr ~config args with
      | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
      | Ok (stdout, latency_ms) ->
        let response = parse_text_result stdout in
        { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      let messages = non_system_messages req.messages in
      let prompt = prompt_of_messages messages in
      let args = build_args ~config ~prompt in
      match run_subprocess ~sw ~mgr ~config args with
      | Error _ as e -> e
      | Ok (stdout, _latency_ms) ->
        let result = parse_text_result stdout in
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

let%test "default_config codex_path" =
  default_config.codex_path = "codex"

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
