(** Cascade configuration: named provider profiles with JSON hot-reload
    and discovery-aware health filtering.

    @since 0.59.0 *)

(* ── Provider registry ─────────────────────────────────── *)

type provider_defaults = {
  kind: Provider_config.provider_kind;
  base_url: string;
  api_key_env: string;
  request_path: string;
}

let llama_defaults = {
  kind = OpenAI_compat;
  base_url =
    (match Sys.getenv_opt "LLM_ENDPOINTS" with
     | Some s ->
       (match String.split_on_char ',' s with
        | url :: _ -> String.trim url
        | [] -> "http://127.0.0.1:8085")
     | None -> "http://127.0.0.1:8085");
  api_key_env = "";
  request_path = "/v1/chat/completions";
}

let claude_defaults = {
  kind = Anthropic;
  base_url = "https://api.anthropic.com";
  api_key_env = "ANTHROPIC_API_KEY";
  request_path = "/v1/messages";
}

let gemini_defaults = {
  kind = OpenAI_compat;
  base_url =
    (match Sys.getenv_opt "GEMINI_BASE_URL" with
     | Some url -> url
     | None -> "https://generativelanguage.googleapis.com/v1beta/openai");
  api_key_env = "GEMINI_API_KEY";
  request_path = "/chat/completions";
}

let glm_defaults = {
  kind = OpenAI_compat;
  base_url =
    (match Sys.getenv_opt "ZAI_BASE_URL" with
     | Some url -> url
     | None -> "https://open.bigmodel.cn/api/paas/v4");
  api_key_env = "ZAI_API_KEY";
  request_path = "/chat/completions";
}

let openrouter_defaults = {
  kind = OpenAI_compat;
  base_url = "https://openrouter.ai/api/v1";
  api_key_env = "OPENROUTER_API_KEY";
  request_path = "/chat/completions";
}

let known_providers : (string * provider_defaults) list = [
  ("llama", llama_defaults);
  ("claude", claude_defaults);
  ("gemini", gemini_defaults);
  ("glm", glm_defaults);
  ("openrouter", openrouter_defaults);
]

(* ── Model string parsing ──────────────────────────────── *)

let has_api_key env_name =
  env_name = "" ||
  (match Sys.getenv_opt env_name with
   | Some s -> String.trim s <> ""
   | None -> false)

let parse_custom_model model_id =
  match String.index_opt model_id '@' with
  | Some at_idx ->
    let model = String.sub model_id 0 at_idx in
    let url = String.sub model_id (at_idx + 1) (String.length model_id - at_idx - 1) in
    (model, url)
  | None ->
    let url =
      match Sys.getenv_opt "CUSTOM_LLM_BASE_URL" with
      | Some u -> u
      | None -> "http://127.0.0.1:8080"
    in
    (model_id, url)

let parse_model_string ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt (s : string) : Provider_config.t option =
  let s = String.trim s in
  match String.index_opt s ':' with
  | None -> None
  | Some idx ->
    if idx = 0 || idx >= String.length s - 1 then None
    else
      let provider_name = String.sub s 0 idx |> String.lowercase_ascii in
      let model_id =
        String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim
      in
      if model_id = "" then None
      else
        match provider_name with
        | "custom" ->
          let actual_model, base_url = parse_custom_model model_id in
          if actual_model = "" then None
          else Some (Provider_config.make
                  ~kind:OpenAI_compat
                  ~model_id:actual_model
                  ~base_url
                  ~request_path:"/v1/chat/completions"
                  ~temperature
                  ~max_tokens
                  ?system_prompt
                  ())
        | _ ->
          match List.assoc_opt provider_name known_providers with
          | None -> None
          | Some defaults ->
            if not (has_api_key defaults.api_key_env) then None
            else
              let api_key =
                if defaults.api_key_env = "" then ""
                else
                  Sys.getenv_opt defaults.api_key_env
                  |> Option.value ~default:""
              in
              Some (Provider_config.make
                      ~kind:defaults.kind
                      ~model_id
                      ~base_url:defaults.base_url
                      ~api_key
                      ~request_path:defaults.request_path
                      ~temperature
                      ~max_tokens
                      ?system_prompt
                      ())

let parse_model_strings ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt (strs : string list) : Provider_config.t list =
  List.filter_map
    (parse_model_string ~temperature ~max_tokens ?system_prompt)
    strs

(* ── JSON config loading with mtime hot-reload ─────────── *)

let config_cache : (string, float * Yojson.Safe.t) Hashtbl.t =
  Hashtbl.create 4
let config_cache_mu = Mutex.create ()

let load_json path =
  Mutex.lock config_cache_mu;
  Fun.protect ~finally:(fun () -> Mutex.unlock config_cache_mu) (fun () ->
    try
      let st = Unix.stat path in
      let mtime = st.Unix.st_mtime in
      match Hashtbl.find_opt config_cache path with
      | Some (cached_mtime, json) when Float.equal cached_mtime mtime ->
        Ok json
      | _ ->
        let ic = open_in path in
        let content = Fun.protect
            ~finally:(fun () -> close_in_noerr ic)
            (fun () ->
               let len = in_channel_length ic in
               let buf = Bytes.create len in
               really_input ic buf 0 len;
               Bytes.to_string buf)
        in
        let json = Yojson.Safe.from_string content in
        Hashtbl.replace config_cache path (mtime, json);
        Ok json
    with
    | Sys_error msg -> Error msg
    | Unix.Unix_error (err, fn, arg) ->
      Error (Printf.sprintf "%s(%s): %s" fn arg (Unix.error_message err))
    | Yojson.Json_error msg -> Error (Printf.sprintf "JSON error: %s" msg)
    | End_of_file -> Error "unexpected end of file")

let load_profile ~config_path ~name =
  let key = name ^ "_models" in
  match load_json config_path with
  | Error _ -> []
  | Ok json ->
    let open Yojson.Safe.Util in
    match json |> member key with
    | `List items ->
      List.filter_map
        (function
          | `String s -> Some (String.trim s)
          | _ -> None)
        items
    | _ -> []

(* ── Discovery-aware health filtering ──────────────────── *)

let is_local_provider (cfg : Provider_config.t) =
  let url = String.lowercase_ascii cfg.base_url in
  let len = String.length url in
  let starts_with prefix =
    let plen = String.length prefix in
    len >= plen && String.sub url 0 plen = prefix
  in
  starts_with "http://127.0.0.1"
  || starts_with "http://localhost:"
  || starts_with "http://localhost/"
  || url = "http://localhost"

let filter_healthy ~sw ~net (providers : Provider_config.t list) =
  let local_providers =
    List.filter is_local_provider providers
  in
  let cloud_providers =
    List.filter (fun cfg -> not (is_local_provider cfg)) providers
  in
  if local_providers = [] then
    (* No local providers, nothing to filter *)
    providers
  else if cloud_providers = [] then
    (* Only local providers — pass through unchanged.
       Let the provider return connection error rather than empty list. *)
    providers
  else
    (* Mixed: probe local health, remove unhealthy locals *)
    let endpoints =
      local_providers
      |> List.map (fun (cfg : Provider_config.t) -> cfg.base_url)
      |> List.sort_uniq String.compare
    in
    let statuses = Discovery.discover ~sw ~net ~endpoints in
    let any_healthy =
      List.exists (fun (s : Discovery.endpoint_status) -> s.healthy) statuses
    in
    if any_healthy then
      providers  (* At least one local is healthy — keep all *)
    else
      cloud_providers  (* All locals unhealthy — cloud only *)

(* ── Helpers ────────────────────────────────────────────── *)

let text_of_response (resp : Types.api_response) : string =
  resp.content
  |> List.filter_map (function
    | Types.Text t -> Some t
    | _ -> None)
  |> String.concat ""

(* ── Named cascade execution ───────────────────────────── *)

(** Accept-aware cascade: try each provider, skip on failure or rejection. *)
let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one cfg =
    match clock with
    | Some clock ->
      Complete.complete_with_retry ~sw ~net ~clock ~config:cfg
        ~messages ~tools ?cache ?metrics ()
    | None ->
      Complete.complete ~sw ~net ~config:cfg
        ~messages ~tools ?cache ?metrics ()
  in
  let rec try_next last_err = function
    | [] ->
      let msg = match last_err with
        | Some (Http_client.HttpError { code; body }) ->
          Printf.sprintf "HTTP %d: %s" code
            (if String.length body > 200
             then String.sub body 0 200 ^ "..."
             else body)
        | Some (Http_client.NetworkError { message }) -> message
        | None -> "No providers available"
      in
      Error (Http_client.NetworkError {
          message = Printf.sprintf "All models failed: %s" msg
        })
    | (cfg : Provider_config.t) :: rest ->
      match try_one cfg with
      | Ok resp ->
        if accept resp then Ok resp
        else begin
          (match last_err with
           | Some (Http_client.HttpError { code; _ }) ->
             m.on_cascade_fallback
               ~from_model:cfg.model_id ~to_model:"next"
               ~reason:(Printf.sprintf "rejected (prev HTTP %d)" code)
           | _ ->
             m.on_cascade_fallback
               ~from_model:cfg.model_id ~to_model:"next"
               ~reason:"rejected by accept validator");
          try_next
            (Some (Http_client.NetworkError {
                 message = "response rejected by accept validator"
               }))
            rest
        end
      | Error err ->
        let err_str = match err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.NetworkError { message } -> message
        in
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if Complete.is_retryable err then
          try_next (Some err) rest
        else
          Error err
  in
  try_next None providers

let complete_named ~sw ~net ?clock ?config_path
    ~name ~defaults ~messages
    ?(tools = []) ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt ?(accept = fun _ -> true)
    ?timeout_sec ?cache ?metrics () =
  (* 1. Load from config file, fall back to defaults *)
  let model_strings =
    match config_path with
    | Some path ->
      let from_file = load_profile ~config_path:path ~name in
      if from_file <> [] then from_file else defaults
    | None -> defaults
  in
  (* 2. Parse model strings → Provider_config.t list *)
  let providers =
    parse_model_strings ~temperature ~max_tokens ?system_prompt model_strings
  in
  if providers = [] then
    Error (Http_client.NetworkError {
        message =
          Printf.sprintf
            "No callable models for cascade '%s'. Tried: [%s]"
            name (String.concat "; " model_strings)
      })
  else
    (* 3. Filter by local endpoint health *)
    let healthy_providers = filter_healthy ~sw ~net providers in
    if healthy_providers = [] then
      Error (Http_client.NetworkError {
          message =
            Printf.sprintf
              "All providers unhealthy for cascade '%s'" name
        })
    else
      (* 4. Execute cascade with accept validation, enforcing timeout *)
      let run () =
        complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
          ~accept healthy_providers ~messages ~tools
      in
      match clock, timeout_sec with
      | Some clk, Some secs when secs > 0 ->
        (try Eio.Time.with_timeout_exn clk (float_of_int secs) run
         with Eio.Time.Timeout ->
           Error (Http_client.NetworkError {
               message =
                 Printf.sprintf
                   "Cascade '%s' timed out after %ds" name secs
             }))
      | _ -> run ()

(* ── Streaming cascade (no accept, no cache) ──────── *)

(** Streaming cascade: try each provider in order with streaming.
    Failover on connection/HTTP errors only (before stream begins).
    No [accept] validator — once streaming starts, events are already
    emitted to [on_event] and the provider is committed.

    @since 0.61.0 *)
let complete_cascade_stream ~sw ~net ?(metrics : Metrics.t option)
    (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list)
    ~(on_event : Types.sse_event -> unit) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one cfg =
    Complete.complete_stream ~sw ~net ~config:cfg
      ~messages ~tools ~on_event ()
  in
  let rec try_next last_err = function
    | [] ->
      let msg = match last_err with
        | Some (Http_client.HttpError { code; body }) ->
          Printf.sprintf "HTTP %d: %s" code
            (if String.length body > 200
             then String.sub body 0 200 ^ "..."
             else body)
        | Some (Http_client.NetworkError { message }) -> message
        | None -> "No providers available"
      in
      Error (Http_client.NetworkError {
        message = Printf.sprintf "All models failed (stream): %s" msg })
    | (cfg : Provider_config.t) :: rest ->
      match try_one cfg with
      | Ok _ as success -> success
      | Error err ->
        let err_str = match err with
          | Http_client.HttpError { code; _ } ->
            Printf.sprintf "HTTP %d" code
          | Http_client.NetworkError { message } -> message
        in
        (match rest with
         | (next_cfg : Provider_config.t) :: _ ->
           m.on_cascade_fallback
             ~from_model:cfg.model_id ~to_model:next_cfg.model_id
             ~reason:err_str
         | [] -> ());
        if Complete.is_retryable err then
          try_next (Some err) rest
        else
          Error err
  in
  try_next None providers

(** Execute a streaming cascade using a named profile.

    Same resolution steps as {!complete_named}:
    1. Load profile from [config_path], fall back to [defaults]
    2. Filter by local endpoint health
    3. Execute streaming cascade with failover

    Unlike {!complete_named}, no [accept] validator or [cache].

    @since 0.61.0 *)
let complete_named_stream ~sw ~net ?clock ?config_path
    ~name ~defaults ~messages
    ?(tools = []) ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt ?timeout_sec ?metrics ~on_event () =
  let model_strings = match config_path with
    | Some path ->
      let from_file = load_profile ~config_path:path ~name in
      if from_file <> [] then from_file else defaults
    | None -> defaults
  in
  let providers =
    parse_model_strings ~temperature ~max_tokens ?system_prompt model_strings
  in
  if providers = [] then
    Error (Http_client.NetworkError {
      message = Printf.sprintf
        "No callable models for streaming cascade '%s'. Tried: [%s]"
        name (String.concat "; " model_strings) })
  else
    let healthy_providers = filter_healthy ~sw ~net providers in
    if healthy_providers = [] then
      Error (Http_client.NetworkError {
        message = Printf.sprintf
          "All providers unhealthy for streaming cascade '%s'" name })
    else
      let run () =
        complete_cascade_stream ~sw ~net ?metrics
          healthy_providers ~messages ~tools ~on_event
      in
      match clock, timeout_sec with
      | Some clk, Some secs when secs > 0 ->
        (try Eio.Time.with_timeout_exn clk (float_of_int secs) run
         with Eio.Time.Timeout ->
           Error (Http_client.NetworkError {
             message = Printf.sprintf
               "Streaming cascade '%s' timed out after %ds" name secs }))
      | _ -> run ()
