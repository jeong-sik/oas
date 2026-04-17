(** Gemini CLI non-interactive transport.

    @since 0.133.0 *)

type config = {
  gemini_path: string;
  model: string option;
  yolo: bool;
  cwd: string option;
  (* Fields below are accepted for parity with the Claude Code config
     so callers can target multiple CLI backends with the same
     structure.  Gemini CLI does not yet expose flags for any of
     them; setting a non-default value produces a one-shot
     [Eio.traceln] warning and the value is otherwise ignored. *)
  mcp_config: string option;
  allowed_tools: string list;
  max_turns: int option;
  permission_mode: string option;
  cancel: unit Eio.Promise.t option;
    (* When [Some p] and [p] resolves mid-run, the [gemini]
       subprocess receives [SIGINT].  Default [None]. *)
}

let default_config = {
  gemini_path = "gemini";
  model = None;
  yolo = true;
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

(* ── JSON parsing ────────────────────────────────────── *)

(** Parse and aggregate usage across [stats.models] entries in a Gemini
    CLI [--output-format json] response.  The CLI may invoke multiple
    models for a single prompt (e.g. a utility router + a main model),
    each with its own per-model token counts:

    {v
    "stats": {
      "models": {
        "gemini-2.5-flash-lite": {
          "tokens": { "input": N, "candidates": N, "cached": N }
        },
        "gemini-3-flash-preview": {
          "tokens": { ... }
        }
      }
    }
    v}

    We sum [tokens.input], [tokens.candidates], and [tokens.cached]
    across every model.  Returns [None] when no [stats.models] object
    is present. *)
let parse_usage json =
  let open Yojson.Safe.Util in
  let models_field =
    try json |> member "stats" |> member "models"
    with Type_error _ -> `Null
  in
  match models_field with
  | `Assoc entries when entries <> [] ->
    let input = ref 0 in
    let output = ref 0 in
    let cached = ref 0 in
    List.iter (fun (_model_name, model_json) ->
      match model_json |> member "tokens" with
      | `Assoc _ as t ->
        input  := !input  + Cli_common_json.member_int "input" t;
        output := !output + Cli_common_json.member_int "candidates" t;
        cached := !cached + Cli_common_json.member_int "cached" t
      | _ -> ()
    ) entries;
    Some { Types.input_tokens = !input;
           output_tokens = !output;
           cache_creation_input_tokens = 0;
           cache_read_input_tokens = !cached;
           cost_usd = None }
  | _ -> None

(** Parse the Gemini CLI [--output-format json] result into an
    [api_response].  Expected shape:

    {v
    { "session_id": "...",
      "response": "text",
      "stats": { "models": { ... } } }
    v} *)
let parse_json_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let response_text = Cli_common_json.member_str "response" json in
    let session_id = Cli_common_json.member_str "session_id" json in
    let usage = parse_usage json in
    Ok { Types.id = session_id;
         model = "gemini";
         stop_reason = Types.EndTurn;
         content = [Text response_text];
         usage; telemetry = None }
  with
  | Yojson.Json_error msg ->
    Error (Http_client.NetworkError {
      message = Printf.sprintf "JSON parse error: %s" msg })

(* ── Transport constructor ───────────────────────────── *)

(** Strip API-key env so [gemini] uses the user's Google OAuth session
    rather than a metered API key.  Mirrors the rationale in
    {!Transport_claude_code.claude_cli_scrub_env}. *)
let gemini_cli_scrub_env =
  ["GEMINI_API_KEY"; "GOOGLE_API_KEY"; "GOOGLE_APPLICATION_CREDENTIALS"]

let run ~sw ~mgr ~(config : config) argv =
  Cli_common_subprocess.run_collect ~sw ~mgr
    ~name:"gemini"
    ~cwd:config.cwd
    ~extra_env:[]
    ~scrub_env:gemini_cli_scrub_env
    ?cancel:config.cancel
    argv

(* Fires once per transport instance when any Claude-only config field
   is set.  Gemini CLI has no flag for these yet, so we warn and drop. *)
let warn_unsupported_once (config : config) warned =
  if !warned then ()
  else begin
    warned := true;
    let warn field =
      Eio.traceln "[warn] %s is not supported by gemini_cli, ignoring" field
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
      let system_prompt =
        Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages in
      let argv = build_args ~config ~req_config:req.config
        ~prompt ~system_prompt in
      match run ~sw ~mgr ~config argv with
      | Error _ as e -> { Llm_transport.response = e; latency_ms = 0 }
      | Ok { stdout; stderr = _; latency_ms } ->
        let response = parse_json_result (String.trim stdout) in
        { Llm_transport.response; latency_ms });

    complete_stream = (fun ~on_event (req : Llm_transport.completion_request) ->
      warn_unsupported_once config warned;
      let messages = Cli_common_prompt.non_system_messages req.messages in
      let prompt = Cli_common_prompt.prompt_of_messages messages in
      let system_prompt =
        Cli_common_prompt.system_prompt_of ~req_config:req.config req.messages in
      let argv = build_args ~config ~req_config:req.config
        ~prompt ~system_prompt in
      (* Gemini CLI does not support native streaming; replay synthetic events
         after the sync call completes. *)
      match run ~sw ~mgr ~config argv with
      | Error _ as e -> e
      | Ok { stdout; stderr = _; latency_ms = _ } ->
        let result = parse_json_result (String.trim stdout) in
        (match result with
         | Ok resp ->
           Cli_common_synthetic_events.replay ~on_event resp;
           Ok resp
         | Error _ as e -> e));
  }

(* ── Inline tests ────────────────────────────────────── *)

[@@@coverage off]

let%test "default_config gemini_path" =
  default_config.gemini_path = "gemini"

let%test "default_config yolo true" =
  default_config.yolo = true

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

let%test "default_config parity fields absent" =
  default_config.mcp_config = None
  && default_config.allowed_tools = []
  && default_config.max_turns = None
  && default_config.permission_mode = None

let%test "build_args ignores mcp_config and allowed_tools" =
  let config = { default_config with
    mcp_config = Some "/tmp/mcp.json";
    allowed_tools = ["Read"; "Write"];
    max_turns = Some 3;
    permission_mode = Some "bypassPermissions";
  } in
  let args = build_args ~config
    ~req_config:(Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" ())
    ~prompt:"hello" ~system_prompt:None in
  not (List.mem "--mcp-config" args)
  && not (List.mem "--allowedTools" args)
  && not (List.mem "--max-turns" args)
  && not (List.mem "--permission-mode" args)
  && not (List.mem "/tmp/mcp.json" args)

let%test "parse_json_result success (single model stats)" =
  let json = {|{"session_id":"sid","response":"hello world","stats":{"models":{"gemini-2.5-flash":{"tokens":{"input":10,"candidates":5,"cached":2}}}}}|} in
  match parse_json_result json with
  | Ok resp ->
    resp.content = [Types.Text "hello world"]
    && resp.stop_reason = Types.EndTurn
    && resp.id = "sid"
    && (match resp.usage with
        | Some u -> u.input_tokens = 10 && u.output_tokens = 5 && u.cache_read_input_tokens = 2
        | None -> false)
  | Error _ -> false

let%test "parse_json_result aggregates across multiple models" =
  let json = {|{"session_id":"sid","response":"hi","stats":{"models":{"utility":{"tokens":{"input":100,"candidates":7,"cached":0}},"main":{"tokens":{"input":200,"candidates":3,"cached":5}}}}}|} in
  match parse_json_result json with
  | Ok resp ->
    (match resp.usage with
     | Some u ->
       u.input_tokens = 300
       && u.output_tokens = 10
       && u.cache_read_input_tokens = 5
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
