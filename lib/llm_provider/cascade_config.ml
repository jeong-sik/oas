(** Cascade configuration: named provider profiles with JSON hot-reload
    and discovery-aware health filtering.

    Provider defaults are sourced from {!Provider_registry} (SSOT).

    @since 0.59.0
    @since 0.92.0 decomposed into Cascade_model_resolve, Cascade_throttle,
    Cascade_config_loader *)

(* ── Re-exports from extracted modules ─────────────── *)

(* Model resolution *)
let resolve_glm_model_id = Cascade_model_resolve.resolve_glm_model_id
let resolve_auto_model_id = Cascade_model_resolve.resolve_auto_model_id
let parse_custom_model = Cascade_model_resolve.parse_custom_model

(* Throttle *)
let populate_throttle_table = Cascade_throttle.populate
let lookup_throttle = Cascade_throttle.lookup

(* Config loader *)
let load_json = Cascade_config_loader.load_json
let load_profile = Cascade_config_loader.load_profile

type inference_params = Cascade_config_loader.inference_params = {
  temperature: float option;
  max_tokens: int option;
}

let resolve_inference_params = Cascade_config_loader.resolve_inference_params

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
              (* For llama provider: round-robin across LLM_ENDPOINTS
                 so multiple local servers share the load transparently. *)
              let base_url =
                if provider_name = "llama" then
                  Provider_registry.next_llama_endpoint ()
                else defaults.base_url
              in
              let resolved_model_id =
                resolve_auto_model_id provider_name model_id
              in
              Some (Provider_config.make
                      ~kind:defaults.kind
                      ~model_id:resolved_model_id
                      ~base_url
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

(* ── Cascade-level error classification ────────────────── *)

(** Decide whether an error should cascade to the next provider. *)
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

(** Check whether a provider has credentials when required. *)
let has_required_api_key (cfg : Provider_config.t) =
  cfg.api_key <> "" || is_local_provider cfg

(** Internal: filter healthy + return discovery statuses for throttle. *)
let filter_healthy_internal ~sw ~net (providers : Provider_config.t list) =
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
    (providers, [])
  else
    let endpoints =
      local_providers
      |> List.map (fun (cfg : Provider_config.t) -> cfg.base_url)
      |> List.sort_uniq String.compare
    in
    let statuses = Discovery.discover ~sw ~net ~endpoints in
    if cloud_providers = [] then
      (providers, statuses)
    else
      let any_healthy =
        List.exists (fun (s : Discovery.endpoint_status) -> s.healthy) statuses
      in
      if any_healthy then
        (providers, statuses)
      else
        (cloud_providers, [])

let filter_healthy ~sw ~net providers =
  fst (filter_healthy_internal ~sw ~net providers)

(* ── Context window resolution ──────────────────────────── *)

let effective_max_context (entry : Provider_registry.entry)
    (caps : Capabilities.capabilities) =
  match caps.max_context_tokens with
  | Some n -> n
  | None -> entry.max_context

(* ── Capability-aware filtering ─────────────────────────── *)

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
  if filtered = [] then providers
  else filtered

(* ── Helpers ────────────────────────────────────────────── *)

let text_of_response (resp : Types.api_response) : string =
  resp.content
  |> List.filter_map (function
    | Types.Text t -> Some t
    | _ -> None)
  |> String.concat ""

(* ── Model resolution: named -> "default" -> hardcoded ─── *)

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

let complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
    ?throttle ?priority ~accept (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one (cfg : Provider_config.t) =
    let call () =
      match clock with
      | Some clock ->
        Complete.complete_with_retry ~sw ~net ~clock ~config:cfg
          ~messages ~tools ?cache ?metrics ?priority ()
      | None ->
        Complete.complete ~sw ~net ~config:cfg
          ~messages ~tools ?cache ?metrics ?priority ()
    in
    let effective_throttle = match throttle with
      | Some _ -> throttle
      | None ->
        if is_local_provider cfg then lookup_throttle cfg.base_url
        else None
    in
    match effective_throttle with
    | Some t -> Provider_throttle.with_permit t call
    | None -> call ()
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
    ?timeout_sec ?cache ?metrics ?throttle ?priority () =
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
    let healthy_providers, local_statuses =
      filter_healthy_internal ~sw ~net providers
    in
    populate_throttle_table local_statuses;
    if healthy_providers = [] then
      Error (Http_client.NetworkError {
          message =
            Printf.sprintf
              "All providers unhealthy for cascade '%s'" name
        })
    else
      let run () =
        complete_cascade_with_accept ~sw ~net ?clock ?cache ?metrics
          ?throttle ?priority ~accept healthy_providers ~messages ~tools
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

let complete_cascade_stream ~sw ~net ?(metrics : Metrics.t option)
    ?(priority : Request_priority.t option)
    (providers : Provider_config.t list)
    ~(messages : Types.message list) ~(tools : Yojson.Safe.t list)
    ~(on_event : Types.sse_event -> unit) =
  let m = match metrics with Some m -> m | None -> Metrics.noop in
  let try_one (cfg : Provider_config.t) =
    let call () =
      Complete.complete_stream ~sw ~net ~config:cfg
        ~messages ~tools ~on_event ?priority ()
    in
    let throttle =
      if is_local_provider cfg then lookup_throttle cfg.base_url
      else None
    in
    match throttle with
    | Some t -> Provider_throttle.with_permit t call
    | None -> call ()
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

let complete_named_stream ~sw ~net ?clock ?config_path
    ~name ~defaults ~messages
    ?(tools = []) ?(temperature = 0.3) ?(max_tokens = 500)
    ?system_prompt ?(strict_name = false)
    ?timeout_sec ?metrics ?priority ~on_event () =
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
    let healthy_providers, local_statuses =
      filter_healthy_internal ~sw ~net providers
    in
    populate_throttle_table local_statuses;
    if healthy_providers = [] then
      Error (Http_client.NetworkError {
        message = Printf.sprintf
          "All providers unhealthy for streaming cascade '%s'" name })
    else
      let run () =
        complete_cascade_stream ~sw ~net ?metrics ?priority
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

(* ── Admission throttle table tests ──────────────────── *)

let%test "populate_throttle_table creates entry from slots" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    let status : Discovery.endpoint_status = {
      url = "http://127.0.0.1:9999"; healthy = true;
      models = []; props = None;
      slots = Some { total = 4; busy = 0; idle = 4 };
      capabilities = Capabilities.default_capabilities;
    } in
    populate_throttle_table [status];
    match lookup_throttle "http://127.0.0.1:9999" with
    | Some t -> Provider_throttle.available t = 4
    | None -> false)

let%test "populate_throttle_table reuses existing entry" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    let status : Discovery.endpoint_status = {
      url = "http://127.0.0.1:9998"; healthy = true;
      models = []; props = None;
      slots = Some { total = 4; busy = 0; idle = 4 };
      capabilities = Capabilities.default_capabilities;
    } in
    populate_throttle_table [status];
    let t1 = lookup_throttle "http://127.0.0.1:9998" in
    (* second call with different slot count should not replace *)
    let status2 = { status with
      slots = Some { total = 8; busy = 0; idle = 8 } } in
    populate_throttle_table [status2];
    let t2 = lookup_throttle "http://127.0.0.1:9998" in
    match t1, t2 with
    | Some a, Some b -> Provider_throttle.available a = Provider_throttle.available b
    | _ -> false)

let%test "populate_throttle_table skips unhealthy endpoints" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    let status : Discovery.endpoint_status = {
      url = "http://127.0.0.1:9997"; healthy = false;
      models = []; props = None;
      slots = Some { total = 4; busy = 0; idle = 4 };
      capabilities = Capabilities.default_capabilities;
    } in
    populate_throttle_table [status];
    lookup_throttle "http://127.0.0.1:9997" = None)

let%test "populate_throttle_table evicts stale entry on unhealthy" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    let healthy : Discovery.endpoint_status = {
      url = "http://127.0.0.1:9995"; healthy = true;
      models = []; props = None;
      slots = Some { total = 4; busy = 0; idle = 4 };
      capabilities = Capabilities.default_capabilities;
    } in
    populate_throttle_table [healthy];
    let has_entry = lookup_throttle "http://127.0.0.1:9995" <> None in
    let unhealthy = { healthy with healthy = false } in
    populate_throttle_table [unhealthy];
    let evicted = lookup_throttle "http://127.0.0.1:9995" = None in
    has_entry && evicted)

let%test "populate_throttle_table fallback when no slot data" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    let status : Discovery.endpoint_status = {
      url = "http://127.0.0.1:9996"; healthy = true;
      models = []; props = None; slots = None;
      capabilities = Capabilities.default_capabilities;
    } in
    populate_throttle_table [status];
    match lookup_throttle "http://127.0.0.1:9996" with
    | Some t -> Provider_throttle.available t = 4  (* default for OpenAI_compat *)
    | None -> false)

let%test "lookup_throttle returns None for unknown URL" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    lookup_throttle "http://unknown:1234" = None)

let%test "populate_throttle_table empty statuses is no-op" =
  Eio_main.run (fun _env ->
    Cascade_throttle.clear ();
    populate_throttle_table [];
    Cascade_throttle.length () = 0)

(* has_api_key tests moved to test_provider_registry.ml -- SSOT *)

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

let with_temp_cascade_json content f =
  let tmp = Filename.temp_file "cascade" ".json" in
  let oc = open_out tmp in
  output_string oc content;
  close_out oc;
  Fun.protect ~finally:(fun () -> try Sys.remove tmp with _ -> ())
    (fun () -> f tmp)

let%test "resolve_model_strings named profile found" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"foo_models": ["glm:flash"]}|} (fun tmp ->
      resolve_model_strings ~config_path:tmp
        ~name:"foo" ~defaults:["llama:auto"] () = ["glm:flash"]))

let%test "resolve_model_strings falls back to default profile" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"default_models": ["glm:auto", "llama:auto"]}|} (fun tmp ->
      resolve_model_strings ~config_path:tmp
        ~name:"nonexistent" ~defaults:["fallback:x"] () = ["glm:auto"; "llama:auto"]))

let%test "resolve_model_strings named takes priority over default" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"named_models": ["glm:flash"], "default_models": ["llama:auto"]}|} (fun tmp ->
      resolve_model_strings ~config_path:tmp
        ~name:"named" ~defaults:["fallback:x"] () = ["glm:flash"]))

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
  List.length result = 1

let%test "filter_by_capabilities returns all if none match" =
  let cfg1 = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-a"
    ~base_url:"http://localhost:8085" () in
  let cfg2 = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-b"
    ~base_url:"http://localhost:8085" () in
  let result = filter_by_capabilities
    ~pred:(fun c -> c.Capabilities.supports_computer_use)
    [cfg1; cfg2] in
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
    with_temp_cascade_json {|{"myname_models": ["llama:auto"]}|} (fun tmp ->
      let (_models, source) = resolve_model_strings_traced ~config_path:tmp
        ~name:"myname" ~defaults:["fallback:x"] () in
      source = Named))

let%test "resolve_model_strings_traced returns Default_fallback on missing name" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"default_models": ["glm:auto"]}|} (fun tmp ->
      let (models, source) = resolve_model_strings_traced ~config_path:tmp
        ~name:"typo_name" ~defaults:["fallback:x"] () in
      source = Default_fallback && models = ["glm:auto"]))

let%test "resolve_model_strings_traced returns Hardcoded_defaults when no config" =
  let (_models, source) = resolve_model_strings_traced
    ~name:"any" ~defaults:["llama:auto"] () in
  source = Hardcoded_defaults

let%test "resolve_model_strings_traced returns Hardcoded_defaults on empty config" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"other_models": ["glm:auto"]}|} (fun tmp ->
      let (_models, source) = resolve_model_strings_traced ~config_path:tmp
        ~name:"missing" ~defaults:["fallback:x"] () in
      source = Hardcoded_defaults))

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
