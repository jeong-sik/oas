(** Cascade configuration: named provider profiles with JSON hot-reload
    and discovery-aware health filtering.

    Provider defaults are sourced from {!Provider_registry} (SSOT).

    @since 0.59.0 *)

(* ── Provider registry (SSOT: Provider_registry) ─────── *)

let default_registry = Provider_registry.default ()

(* Build headers list with Authorization when api_key is present.
   Anthropic uses x-api-key; OpenAI-compat (including GLM) uses Bearer. *)
let headers_with_auth ~(kind : Provider_config.provider_kind) ~api_key =
  let base = [("Content-Type", "application/json")] in
  if api_key = "" then base
  else match kind with
    | Anthropic ->
        ("x-api-key", api_key)
        :: ("anthropic-version", "2023-06-01")
        :: base
    | OpenAI_compat | Gemini | Glm | Claude_code ->
        ("Authorization", "Bearer " ^ api_key) :: base

(* ── Model string parsing ──────────────────────────────── *)

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
      let provider_name = String.sub s 0 idx |> String.trim |> String.lowercase_ascii in
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
          match Provider_registry.find default_registry provider_name with
          | None -> None
          | Some entry ->
            if not (entry.is_available ()) then None
            else
              let defaults = entry.defaults in
              let api_key =
                if defaults.api_key_env = "" then ""
                else
                  Sys.getenv_opt defaults.api_key_env
                  |> Option.value ~default:""
              in
              let headers = headers_with_auth ~kind:defaults.kind ~api_key in
              Some (Provider_config.make
                      ~kind:defaults.kind
                      ~model_id
                      ~base_url:defaults.base_url
                      ~api_key ~headers
                      ~request_path:defaults.request_path
                      ~temperature
                      ~max_tokens
                      ?system_prompt
                      ())

let parse_model_string_exn ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt (s : string) : (Provider_config.t, string) result =
  let s = String.trim s in
  match String.index_opt s ':' with
  | None ->
    Error (Printf.sprintf "invalid model spec %S: expected \"provider:model_id\"" s)
  | Some idx ->
    if idx = 0 || idx >= String.length s - 1 then
      Error (Printf.sprintf "invalid model spec %S: empty provider or model_id" s)
    else
      let provider_name = String.sub s 0 idx |> String.trim |> String.lowercase_ascii in
      let model_id =
        String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim
      in
      if model_id = "" then
        Error (Printf.sprintf "invalid model spec %S: empty model_id" s)
      else
        match provider_name with
        | "custom" ->
          let actual_model, base_url = parse_custom_model model_id in
          if actual_model = "" then
            Error (Printf.sprintf "invalid custom model spec %S: empty model after @" s)
          else
            Ok (Provider_config.make
                  ~kind:OpenAI_compat ~model_id:actual_model ~base_url
                  ~request_path:"/v1/chat/completions"
                  ~temperature ~max_tokens ?system_prompt ())
        | _ ->
          (match Provider_registry.find default_registry provider_name with
          | None ->
            Error (Printf.sprintf "unknown provider %S in model spec %S" provider_name s)
          | Some entry ->
            if not (entry.is_available ()) then
              Error (Printf.sprintf "provider %S unavailable (missing env var %S)"
                       provider_name entry.defaults.api_key_env)
            else
              let defaults = entry.defaults in
              let api_key =
                if defaults.api_key_env = "" then ""
                else Sys.getenv_opt defaults.api_key_env |> Option.value ~default:""
              in
              let headers = headers_with_auth ~kind:defaults.kind ~api_key in
              Ok (Provider_config.make
                    ~kind:defaults.kind ~model_id ~base_url:defaults.base_url
                    ~api_key ~headers ~request_path:defaults.request_path
                    ~temperature ~max_tokens ?system_prompt ()))

let parse_model_strings ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt (strs : string list) : Provider_config.t list =
  List.filter_map
    (parse_model_string ~temperature ~max_tokens ?system_prompt)
    strs

(* ── JSON config loading with mtime hot-reload ─────────── *)

let config_cache : (string, float * Yojson.Safe.t) Hashtbl.t =
  Hashtbl.create 4
let config_cache_mu = Eio.Mutex.create ()

let load_json path =
  Eio.Mutex.use_rw ~protect:true config_cache_mu (fun () ->
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

(* ── Cascade-level error classification ────────────────── *)

(** Decide whether an error should cascade to the next provider.
    Different from {!Complete.is_retryable} which governs same-provider retry:
    - Auth errors (401/403) are NOT retryable on the same provider but SHOULD
      cascade to the next one (different provider may have valid credentials).
    - Rate limits and server errors cascade as before.
    - Network errors (connection refused, DNS) always cascade. *)
let should_cascade_to_next = function
  | Http_client.HttpError { code; _ } ->
    code = 401 || code = 403
    || code = 429 || code = 500 || code = 502 || code = 503 || code = 529
  | Http_client.NetworkError _ -> true

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

(** Check whether a provider has credentials when required.
    Local providers (llama on localhost) never need an API key.
    Cloud providers with an empty [api_key] are filtered out. *)
let has_required_api_key (cfg : Provider_config.t) =
  cfg.api_key <> "" || is_local_provider cfg

let filter_healthy ~sw ~net (providers : Provider_config.t list) =
  (* Step 0: Remove cloud providers missing required API keys *)
  let providers =
    let with_keys = List.filter has_required_api_key providers in
    if with_keys = [] then providers  (* keep all rather than empty *)
    else with_keys
  in
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

(* ── Context window resolution ──────────────────────────── *)

let effective_max_context (entry : Provider_registry.entry)
    (caps : Capabilities.capabilities) =
  match caps.max_context_tokens with
  | Some n -> n
  | None -> entry.max_context

(* ── Capability-aware filtering ─────────────────────────── *)

(** Filter providers by a capability predicate.
    Uses {!Capabilities.for_model_id} to resolve model-specific capabilities,
    falling back to the provider-level defaults from the registry.

    Providers that do not satisfy [pred] are removed.
    If all providers are filtered out, returns the original list
    (let the provider return an API error rather than an empty cascade). *)
let filter_by_capabilities ~(pred : Capabilities.capabilities -> bool)
    (providers : Provider_config.t list) =
  let satisfies (cfg : Provider_config.t) =
    let caps = match Capabilities.for_model_id cfg.model_id with
      | Some c -> c
      | None ->
        match Provider_registry.find default_registry cfg.model_id with
        | Some entry -> entry.capabilities
        | None -> Capabilities.default_capabilities
    in
    pred caps
  in
  let filtered = List.filter satisfies providers in
  if filtered = [] then providers  (* fallback: pass all through *)
  else filtered

(* ── Helpers ────────────────────────────────────────────── *)

let text_of_response (resp : Types.api_response) : string =
  resp.content
  |> List.filter_map (function
    | Types.Text t -> Some t
    | _ -> None)
  |> String.concat ""

(* ── Model resolution: named → "default" → hardcoded ─── *)

(* TODO: per-profile temperature / max_tokens overrides from JSON *)

type cascade_source = Named | Default_fallback | Hardcoded_defaults

let resolve_model_strings_traced ?config_path ~name ~defaults () =
  match config_path with
  | Some path ->
    let from_file = load_profile ~config_path:path ~name in
    if from_file <> [] then (from_file, Named)
    else
      let fallback = load_profile ~config_path:path ~name:"default" in
      if fallback <> [] then (fallback, Default_fallback)
      else (defaults, Hardcoded_defaults)
  | None -> (defaults, Hardcoded_defaults)

let resolve_model_strings ?config_path ~name ~defaults () =
  fst (resolve_model_strings_traced ?config_path ~name ~defaults ())

(* ── Named cascade execution ───────────────────────────── *)

(** Accept-aware cascade: try each provider, skip on failure or rejection.
    When [throttle] is provided, each provider call acquires a permit first,
    blocking if the backend has no available capacity. *)
let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ?throttle ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one cfg =
    let call () =
      match clock with
      | Some clock ->
        Complete.complete_with_retry ~sw ~net ~clock ~config:cfg
          ~messages ~tools ?cache ?metrics ()
      | None ->
        Complete.complete ~sw ~net ~config:cfg
          ~messages ~tools ?cache ?metrics ()
    in
    match throttle with
    | Some t when is_local_provider cfg ->
      Provider_throttle.with_permit t call
    | _ -> call ()
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
        if should_cascade_to_next err then
          try_next (Some err) rest
        else
          Error err
  in
  try_next None providers

let complete_named ~sw ~net ?clock ?config_path
    ~name ~defaults ~messages
    ?(tools = []) ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt ?(accept = fun _ -> true) ?(strict_name = false)
    ?timeout_sec ?cache ?metrics ?throttle () =
  (* 1. Resolve: named profile → "default" profile → hardcoded defaults *)
  let model_strings, source =
    resolve_model_strings_traced ?config_path ~name ~defaults ()
  in
  if strict_name && source <> Named then
    Error (Http_client.NetworkError {
        message =
          Printf.sprintf
            "Cascade '%s' not found in config (resolved via %s)"
            name (match source with
                  | Named -> "named"
                  | Default_fallback -> "default_fallback"
                  | Hardcoded_defaults -> "hardcoded_defaults")
      })
  else
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
          ?throttle ~accept healthy_providers ~messages ~tools
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
        if should_cascade_to_next err then
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
    ?system_prompt ?(strict_name = false)
    ?timeout_sec ?metrics ~on_event () =
  let model_strings, source =
    resolve_model_strings_traced ?config_path ~name ~defaults ()
  in
  if strict_name && source <> Named then
    Error (Http_client.NetworkError {
      message = Printf.sprintf
        "Streaming cascade '%s' not found in config (resolved via %s)"
        name (match source with
              | Named -> "named"
              | Default_fallback -> "default_fallback"
              | Hardcoded_defaults -> "hardcoded_defaults") })
  else
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

[@@@coverage off]
(* === Inline tests === *)

(* has_api_key tests moved to test_provider_registry.ml — SSOT *)

let%test "parse_custom_model with @ sign" =
  let (model, url) = parse_custom_model "mymodel@http://host:1234" in
  model = "mymodel" && url = "http://host:1234"

let%test "parse_custom_model no @ sign uses env or default" =
  let (model, _url) = parse_custom_model "some-model" in
  model = "some-model"

let%test "parse_model_string None on empty string" =
  parse_model_string "" = None

let%test "parse_model_string None on no colon" =
  parse_model_string "justmodel" = None

let%test "parse_model_string None on colon at start" =
  parse_model_string ":model" = None

let%test "parse_model_string None on colon at end" =
  parse_model_string "llama:" = None

let%test "parse_model_string None on unknown provider" =
  parse_model_string "unknownprov:model" = None

let%test "parse_model_string llama provider" =
  match parse_model_string "llama:qwen3.5" with
  | Some cfg -> cfg.model_id = "qwen3.5" && cfg.kind = OpenAI_compat
  | None -> false

let%test "parse_model_string custom with url" =
  match parse_model_string "custom:mymodel@http://localhost:9090" with
  | Some cfg -> cfg.model_id = "mymodel" && cfg.base_url = "http://localhost:9090"
  | None -> false

let%test "parse_model_string custom with empty model after @" =
  parse_model_string "custom:@http://foo" = None

let%test "parse_model_string temperature and max_tokens forwarded" =
  match parse_model_string ~temperature:0.7 ~max_tokens:100 "llama:m1" with
  | Some cfg -> cfg.temperature = Some 0.7 && cfg.max_tokens = 100
  | None -> false

let%test "parse_model_string system_prompt forwarded" =
  match parse_model_string ~system_prompt:"test prompt" "llama:m1" with
  | Some cfg -> cfg.system_prompt = Some "test prompt"
  | None -> false

let%test "parse_model_strings empty list" =
  parse_model_strings [] = []

let%test "parse_model_strings filters unavailable" =
  let results = parse_model_strings ["llama:qwen"; "badprovider:x"] in
  List.length results = 1

let%test "is_local_provider 127.0.0.1" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://127.0.0.1:8085" () in
  is_local_provider cfg = true

let%test "is_local_provider localhost with port" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://localhost:8085" () in
  is_local_provider cfg = true

let%test "is_local_provider localhost bare" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://localhost" () in
  is_local_provider cfg = true

let%test "is_local_provider localhost with path" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://localhost/v1" () in
  is_local_provider cfg = true

let%test "is_local_provider remote is false" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"https://api.example.com" () in
  is_local_provider cfg = false

let%test "text_of_response empty content" =
  let resp : Types.api_response = {
    id = ""; model = ""; stop_reason = EndTurn;
    content = []; usage = None } in
  text_of_response resp = ""

let%test "text_of_response text blocks concatenated" =
  let resp : Types.api_response = {
    id = ""; model = ""; stop_reason = EndTurn;
    content = [Types.Text "hello"; Types.Text " world"]; usage = None } in
  text_of_response resp = "hello world"

let%test "text_of_response non-text blocks ignored" =
  let resp : Types.api_response = {
    id = ""; model = ""; stop_reason = EndTurn;
    content = [
      Types.ToolUse { id = "t1"; name = "tool"; input = `Null };
      Types.Text "only this";
    ]; usage = None } in
  text_of_response resp = "only this"

let%test "load_profile nonexistent file returns empty" =
  Eio_main.run (fun _env ->
    load_profile ~config_path:"/nonexistent/file.json" ~name:"test" = [])

let%test "resolve_model_strings no config_path returns defaults" =
  resolve_model_strings ~name:"test" ~defaults:["llama:auto"] () = ["llama:auto"]

let%test "resolve_model_strings nonexistent file returns defaults" =
  Eio_main.run (fun _env ->
    resolve_model_strings ~config_path:"/nonexistent.json"
      ~name:"test" ~defaults:["llama:auto"] () = ["llama:auto"])

let%test "resolve_model_strings named profile found" =
  Eio_main.run (fun _env ->
    let tmp = Filename.temp_file "cascade" ".json" in
    let oc = open_out tmp in
    output_string oc {|{"foo_models": ["glm:flash"]}|};
    close_out oc;
    let result = resolve_model_strings ~config_path:tmp
      ~name:"foo" ~defaults:["llama:auto"] () in
    Sys.remove tmp;
    result = ["glm:flash"])

let%test "resolve_model_strings falls back to default profile" =
  Eio_main.run (fun _env ->
    let tmp = Filename.temp_file "cascade" ".json" in
    let oc = open_out tmp in
    output_string oc {|{"default_models": ["glm:auto", "llama:auto"]}|};
    close_out oc;
    let result = resolve_model_strings ~config_path:tmp
      ~name:"nonexistent" ~defaults:["fallback:x"] () in
    Sys.remove tmp;
    result = ["glm:auto"; "llama:auto"])

let%test "resolve_model_strings named takes priority over default" =
  Eio_main.run (fun _env ->
    let tmp = Filename.temp_file "cascade" ".json" in
    let oc = open_out tmp in
    output_string oc {|{"named_models": ["glm:flash"], "default_models": ["llama:auto"]}|};
    close_out oc;
    let result = resolve_model_strings ~config_path:tmp
      ~name:"named" ~defaults:["fallback:x"] () in
    Sys.remove tmp;
    result = ["glm:flash"])

let%test "default_registry has 6 providers" =
  List.length (Provider_registry.all default_registry) = 6

let%test "default_registry llama is OpenAI_compat" =
  match Provider_registry.find default_registry "llama" with
  | Some e -> e.defaults.kind = OpenAI_compat | None -> false

let%test "default_registry claude is Anthropic" =
  match Provider_registry.find default_registry "claude" with
  | Some e -> e.defaults.kind = Anthropic | None -> false

let%test "default_registry gemini is Gemini" =
  match Provider_registry.find default_registry "gemini" with
  | Some e -> e.defaults.kind = Gemini | None -> false

let%test "parse_model_string trims whitespace" =
  match parse_model_string "  llama : qwen3.5  " with
  | Some cfg -> cfg.model_id = "qwen3.5"
  | None -> false

let%test "parse_model_string case-insensitive provider" =
  match parse_model_string "LLAMA:model1" with
  | Some cfg -> cfg.model_id = "model1"
  | None -> false

let%test "filter_by_capabilities keeps tool-supporting providers" =
  let tool_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"qwen3.5"
    ~base_url:"http://localhost:8085" () in
  let no_tool_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-no-tools"
    ~base_url:"http://localhost:8085" () in
  let result = filter_by_capabilities
    ~pred:(fun c -> c.Capabilities.supports_tools)
    [tool_cfg; no_tool_cfg] in
  (* qwen3.5 has tools via for_model_id, unknown falls back to default (no tools) *)
  List.length result = 1

let%test "filter_by_capabilities returns all if none match" =
  let cfg1 = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-a"
    ~base_url:"http://localhost:8085" () in
  let cfg2 = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-b"
    ~base_url:"http://localhost:8085" () in
  let result = filter_by_capabilities
    ~pred:(fun c -> c.Capabilities.supports_computer_use)
    [cfg1; cfg2] in
  (* Neither supports computer_use, fallback to all *)
  List.length result = 2

let%test "effective_max_context uses capability when present" =
  let entry : Provider_registry.entry = {
    name = "test"; max_context = 128_000;
    defaults = { kind = OpenAI_compat; base_url = ""; api_key_env = "";
                 request_path = "" };
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> true);
  } in
  let caps = { Capabilities.default_capabilities with
               max_context_tokens = Some 200_000 } in
  effective_max_context entry caps = 200_000

let%test "effective_max_context falls back to registry entry" =
  let entry : Provider_registry.entry = {
    name = "test"; max_context = 128_000;
    defaults = { kind = OpenAI_compat; base_url = ""; api_key_env = "";
                 request_path = "" };
    capabilities = Capabilities.default_capabilities;
    is_available = (fun () -> true);
  } in
  let caps = { Capabilities.default_capabilities with
               max_context_tokens = None } in
  effective_max_context entry caps = 128_000

let%test "resolve_model_strings_traced returns Named for existing profile" =
  Eio_main.run (fun _env ->
    let tmp = Filename.temp_file "cascade" ".json" in
    let oc = open_out tmp in
    output_string oc {|{"myname_models": ["llama:auto"]}|};
    close_out oc;
    let (_models, source) = resolve_model_strings_traced ~config_path:tmp
      ~name:"myname" ~defaults:["fallback:x"] () in
    Sys.remove tmp;
    source = Named)

let%test "resolve_model_strings_traced returns Default_fallback on missing name" =
  Eio_main.run (fun _env ->
    let tmp = Filename.temp_file "cascade" ".json" in
    let oc = open_out tmp in
    output_string oc {|{"default_models": ["glm:auto"]}|};
    close_out oc;
    let (models, source) = resolve_model_strings_traced ~config_path:tmp
      ~name:"typo_name" ~defaults:["fallback:x"] () in
    Sys.remove tmp;
    source = Default_fallback && models = ["glm:auto"])

let%test "resolve_model_strings_traced returns Hardcoded_defaults when no config" =
  let (_models, source) = resolve_model_strings_traced
    ~name:"any" ~defaults:["llama:auto"] () in
  source = Hardcoded_defaults

let%test "resolve_model_strings_traced returns Hardcoded_defaults on empty config" =
  Eio_main.run (fun _env ->
    let tmp = Filename.temp_file "cascade" ".json" in
    let oc = open_out tmp in
    output_string oc {|{"other_models": ["glm:auto"]}|};
    close_out oc;
    let (_models, source) = resolve_model_strings_traced ~config_path:tmp
      ~name:"missing" ~defaults:["fallback:x"] () in
    Sys.remove tmp;
    source = Hardcoded_defaults)

(* ── should_cascade_to_next tests ─────────────────────── *)

let%test "should_cascade_to_next 401 auth error" =
  should_cascade_to_next (Http_client.HttpError { code = 401; body = "" }) = true

let%test "should_cascade_to_next 403 forbidden" =
  should_cascade_to_next (Http_client.HttpError { code = 403; body = "" }) = true

let%test "should_cascade_to_next 429 rate limit" =
  should_cascade_to_next (Http_client.HttpError { code = 429; body = "" }) = true

let%test "should_cascade_to_next 500 server error" =
  should_cascade_to_next (Http_client.HttpError { code = 500; body = "" }) = true

let%test "should_cascade_to_next 502 bad gateway" =
  should_cascade_to_next (Http_client.HttpError { code = 502; body = "" }) = true

let%test "should_cascade_to_next 503 service unavailable" =
  should_cascade_to_next (Http_client.HttpError { code = 503; body = "" }) = true

let%test "should_cascade_to_next 529 overloaded" =
  should_cascade_to_next (Http_client.HttpError { code = 529; body = "" }) = true

let%test "should_cascade_to_next network error" =
  should_cascade_to_next (Http_client.NetworkError { message = "refused" }) = true

let%test "should_cascade_to_next 400 bad request stops" =
  should_cascade_to_next (Http_client.HttpError { code = 400; body = "" }) = false

let%test "should_cascade_to_next 404 not found stops" =
  should_cascade_to_next (Http_client.HttpError { code = 404; body = "" }) = false

(* ── has_required_api_key tests ───────────────────────── *)

let%test "has_required_api_key with key present" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"https://api.example.com" ~api_key:"sk-123" () in
  has_required_api_key cfg = true

let%test "has_required_api_key local no key is ok" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://127.0.0.1:8085" () in
  has_required_api_key cfg = true

let%test "has_required_api_key localhost no key is ok" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:"http://localhost:8085" () in
  has_required_api_key cfg = true

let%test "has_required_api_key cloud no key is rejected" =
  let cfg = Provider_config.make ~kind:Anthropic ~model_id:"m"
    ~base_url:"https://api.anthropic.com" () in
  has_required_api_key cfg = false
