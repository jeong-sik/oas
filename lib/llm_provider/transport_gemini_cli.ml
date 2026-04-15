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

(** Parse usage metadata from Gemini JSON response.
    Gemini returns: {usageMetadata: {promptTokenCount, candidatesTokenCount, cachedContentTokenCount}} *)
let parse_usage json =
  let open Yojson.Safe.Util in
  match json |> member "usageMetadata" with
  | `Assoc _ as u ->
    Some { Types.input_tokens = Cli_common_json.member_int "promptTokenCount" u;
           output_tokens = Cli_common_json.member_int "candidatesTokenCount" u;
           cache_creation_input_tokens = 0;
           cache_read_input_tokens =
             Cli_common_json.member_int "cachedContentTokenCount" u;
           cost_usd = None }
  | _ -> None

(** Parse the Gemini CLI --output-format json result into api_response.
    Expected format: {"response": "text", "usageMetadata": {...}} *)
let parse_json_result json_str =
  try
    let json = Yojson.Safe.from_string json_str in
    let response_text = Cli_common_json.member_str "response" json in
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

(* ── Transport constructor ───────────────────────────── *)

let run ~sw ~mgr ~(config : config) argv =
  Cli_common_subprocess.run_collect ~sw ~mgr
    ~name:"gemini"
    ~cwd:config.cwd
    ~extra_env:[]
    argv

let create ~sw ~(mgr : _ Eio.Process.mgr) ~(config : config)
  : Llm_transport.t =
  {
    complete_sync = (fun (req : Llm_transport.completion_request) ->
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
