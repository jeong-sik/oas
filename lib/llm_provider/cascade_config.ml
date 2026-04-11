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
let resolve_api_key_env = Cascade_config_loader.resolve_api_key_env

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
    | OpenAI_compat | Ollama | Gemini | Glm | Claude_code ->
        ("Authorization", "Bearer " ^ api_key) :: base

(* ── String splitting helper ──────────────────────────────── *)

(** Split a "provider:model_id" string at the first colon.
    Returns [None] if the colon is missing, at position 0, or at the end. *)
let split_provider_model (s : string) : (string * string) option =
  match String.index_opt s ':' with
  | None -> None
  | Some idx ->
    if idx = 0 || idx >= String.length s - 1 then None
    else
      let provider_name =
        String.sub s 0 idx |> String.trim |> String.lowercase_ascii
      in
      let model_id =
        String.sub s (idx + 1) (String.length s - idx - 1) |> String.trim
      in
      if model_id = "" then None
      else Some (provider_name, model_id)

(* ── Shared config construction helpers ──────────────────── *)

(** Build a {!Provider_config.t} for "custom:model@url" specs. *)
let make_custom_config ~temperature ~max_tokens ?system_prompt model_id =
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

(** Resolve the effective API key env var name for a provider.

    Checks [api_key_env_overrides] first (exact provider name, then
    wildcard ["*"]), then falls back to the provider registry default.

    Empty-string entries are treated as absent so a user-provided
    [{"glm": ""}] falls through to the wildcard and registry default
    instead of silently disabling auth. *)
let resolve_effective_api_key_env
    ~(api_key_env_overrides : (string * string) list)
    ~(provider_name : string)
    ~(registry_default : string) =
  let find_non_empty key =
    match List.assoc_opt key api_key_env_overrides with
    | Some v when v <> "" -> Some v
    | _ -> None
  in
  match find_non_empty provider_name with
  | Some env -> env
  | None ->
    match find_non_empty "*" with
    | Some env -> env
    | None -> registry_default

(** Build a {!Provider_config.t} from a registry entry. *)
let make_registry_config ~temperature ~max_tokens ?system_prompt
    ?(api_key_env_overrides=[])
    ~provider_name ~model_id (entry : Provider_registry.entry) =
  let defaults = entry.defaults in
  let effective_api_key_env =
    resolve_effective_api_key_env
      ~api_key_env_overrides ~provider_name
      ~registry_default:defaults.api_key_env
  in
  let api_key =
    if effective_api_key_env = "" then ""
    else Sys.getenv_opt effective_api_key_env |> Option.value ~default:""
  in
  let headers = headers_with_auth ~kind:defaults.kind ~api_key in
  (* For local providers, resolve "auto" before endpoint selection so that
     routing uses the concrete model name discovered from the server.
     Filter by provider's own endpoint to prevent cross-provider
     contamination (e.g. ollama:auto must not pick up llama-server models). *)
  let is_local_auto =
    (provider_name = "llama" || provider_name = "ollama")
    && model_id = "auto"
  in
  let effective_model_id =
    if is_local_auto then
      (if provider_name = "ollama" then
         Discovery.first_discovered_model_id_for_url defaults.base_url
       else
         Discovery.first_discovered_model_id ())
      |> Option.value ~default:model_id
    else model_id
  in
  let base_url =
    if provider_name = "llama" then
      (* Route to the endpoint that has this model; round-robin fallback *)
      match Discovery.endpoint_for_model effective_model_id with
      | Some url -> url
      | None -> Provider_registry.next_llama_endpoint ()
    else defaults.base_url
  in
  let resolved_model_id = resolve_auto_model_id provider_name effective_model_id in
  (* Resolve max_context: per-model capabilities override registry default *)
  let max_context =
    let caps =
      Option.value ~default:entry.capabilities
        (Capabilities.for_model_id resolved_model_id)
    in
    match caps.max_context_tokens with
    | Some n -> n
    | None -> entry.max_context
  in
  Provider_config.make
    ~kind:defaults.kind
    ~model_id:resolved_model_id
    ~base_url
    ~api_key ~headers
    ~request_path:defaults.request_path
    ~temperature
    ~max_tokens
    ~max_context
    ?system_prompt
    ()

(* ── Model string parsing ──────────────────────────────── *)

let parse_model_string
    ?(temperature = Constants.Inference.default_temperature)
    ?(max_tokens = Constants.Inference.default_max_tokens)
    ?system_prompt ?(api_key_env_overrides = [])
    (s : string) : Provider_config.t option =
  match split_provider_model (String.trim s) with
  | None -> None
  | Some ("custom", model_id) ->
    make_custom_config ~temperature ~max_tokens ?system_prompt model_id
  | Some (provider_name, model_id) ->
    match Provider_registry.find default_registry provider_name with
    | None -> None
    | Some entry when not (entry.is_available ()) -> None
    | Some entry ->
      Some (make_registry_config ~temperature ~max_tokens ?system_prompt
              ~api_key_env_overrides ~provider_name ~model_id entry)

let parse_model_string_exn
    ?(temperature = Constants.Inference.default_temperature)
    ?(max_tokens = Constants.Inference.default_max_tokens)
    ?system_prompt (s : string) : (Provider_config.t, string) result =
  let s = String.trim s in
  match split_provider_model s with
  | None ->
    Error (Printf.sprintf "invalid model spec %S: expected \"provider:model_id\"" s)
  | Some ("custom", model_id) ->
    (match make_custom_config ~temperature ~max_tokens ?system_prompt model_id with
     | Some cfg -> Ok cfg
     | None ->
       Error (Printf.sprintf "invalid custom model spec %S: empty model after @" s))
  | Some (provider_name, model_id) ->
    match Provider_registry.find default_registry provider_name with
    | None ->
      Error (Printf.sprintf "unknown provider %S in model spec %S" provider_name s)
    | Some entry when not (entry.is_available ()) ->
      Error (Printf.sprintf "provider %S unavailable (missing env var %S)"
               provider_name entry.defaults.api_key_env)
    | Some entry ->
      Ok (make_registry_config ~temperature ~max_tokens ?system_prompt
            ~provider_name ~model_id entry)

(** Expand provider:auto specs that map to multiple models.
    "glm:auto" expands to ["glm:glm-5.1"; "glm:glm-5-turbo"; ...].
    Other specs pass through as-is. *)
let expand_auto_models (strs : string list) : string list =
  List.concat_map (fun s ->
    let trimmed = String.trim s in
    match split_provider_model trimmed with
    | Some ("glm", model_id)
      when String.lowercase_ascii model_id = "auto" ->
      Cascade_model_resolve.glm_auto_models ()
      |> List.map (fun m -> "glm:" ^ m)
    | Some ("glm-coding", model_id)
      when String.lowercase_ascii model_id = "auto" ->
      Cascade_model_resolve.glm_coding_auto_models ()
      |> List.map (fun m -> "glm-coding:" ^ m)
    | _ -> [ trimmed ]
  ) strs

let parse_model_strings
    ?(temperature = Constants.Inference.default_temperature)
    ?(max_tokens = Constants.Inference.default_max_tokens)
    ?system_prompt ?(api_key_env_overrides = [])
    (strs : string list) : Provider_config.t list =
  let expanded = expand_auto_models strs in
  List.filter_map
    (parse_model_string ~temperature ~max_tokens ?system_prompt
       ~api_key_env_overrides)
    expanded

(* Health filtering (extracted to Cascade_health_filter) *)
let is_local_provider = Cascade_health_filter.is_local_provider
let filter_healthy_internal = Cascade_health_filter.filter_healthy_internal
let filter_healthy = Cascade_health_filter.filter_healthy

(* ── Context window resolution ──────────────────────────── *)

let effective_max_context (entry : Provider_registry.entry)
    (caps : Capabilities.capabilities) =
  match caps.max_context_tokens with
  | Some n -> n
  | None -> entry.max_context

(** Resolve a model label to the per-slot context of the endpoint
    that would serve it.  Uses the same resolution logic as
    [make_registry_config]: [current_llama_endpoint] for "llama:*",
    parsed URL for "custom:*".  Cloud providers return [None].

    Does NOT advance the round-robin counter — safe to call for
    prompt sizing before the actual cascade request.

    @since 0.100.8 *)
let resolve_label_context (label : string) : int option =
  match split_provider_model (String.trim label) with
  | None -> None
  | Some ("custom", model_id) ->
    let _, url = parse_custom_model model_id in
    Discovery.discovered_context_for_url url
  | Some ("llama", model_id) ->
    (* Model-aware: find the endpoint that has this model loaded *)
    (match Discovery.context_for_model model_id with
     | Some (_url, ctx) -> Some ctx
     | None ->
       (* Fallback: round-robin endpoint (backward compat for "auto" etc.) *)
       let url = Provider_registry.current_llama_endpoint () in
       if url = "" then None
       else Discovery.discovered_context_for_url url)
  | Some (_, _) ->
    (* Cloud providers: no discovery-based per-slot context *)
    None

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

let dedupe_stable (items : string list) =
  let rec loop seen acc = function
    | [] -> List.rev acc
    | item :: rest ->
      if List.mem item seen then loop seen acc rest
      else loop (item :: seen) (item :: acc) rest
  in
  loop [] [] items

let expand_model_strings_for_execution (items : string list) =
  let expand_one raw =
    let item = String.trim raw in
    if item = "" then []
    else
      match split_provider_model item with
      | Some ("glm", model_id)
        when String.lowercase_ascii model_id = "auto" ->
        [item; "glm:turbo"; "glm:flash"]
      | Some ("glm-coding", model_id)
        when String.lowercase_ascii model_id = "auto" ->
        [item; "glm-coding:glm-5-turbo"; "glm-coding:glm-5.1";
         "glm-coding:glm-4.5-air"]
      | _ -> [item]
  in
  items
  |> List.concat_map expand_one
  |> dedupe_stable

(* Cascade execution (extracted to Cascade_executor) *)
let complete_cascade_with_accept = Cascade_executor.complete_cascade_with_accept

(* Filter providers by kind name (exact, case-insensitive).
   Valid filter values: "ollama", "glm", "anthropic", "gemini", "openai_compat", "claude_code".
   Empty/None filter passes through unchanged. No-match falls back to unfiltered. *)
let apply_provider_filter ~provider_filter ~label providers =
  match provider_filter with
  | None | Some [] -> providers
  | Some filters ->
    let lc_filters = List.map String.lowercase_ascii filters in
    let matches (p : Provider_config.t) =
      List.mem (Provider_config.string_of_provider_kind p.kind) lc_filters
    in
    let filtered = List.filter matches providers in
    if filtered = [] then (
      Eio.traceln "[CascadeConfig] provider_filter matched no providers (%s); \
        falling back to unfiltered (filter=[%s] providers=[%s])"
        label (String.concat "," filters)
        (String.concat "," (List.map (fun (p : Provider_config.t) ->
          Provider_config.string_of_provider_kind p.kind) providers));
      providers)
    else filtered

let complete_named ~sw ~net ?clock ?config_path
    ~name ~defaults ~messages
    ?(tools = [])
    ?(temperature = Constants.Inference.default_temperature)
    ?(max_tokens = Constants.Inference.default_max_tokens)
    ?system_prompt ?tool_choice ?(accept = fun _ -> true) ?accept_reason
    ?(strict_name = false)
    ?(accept_on_exhaustion = false)
    ?timeout_sec ?cache ?metrics ?throttle ?priority ?provider_filter () =
  let accept_result =
    match accept_reason with
    | Some validator -> validator
    | None ->
      fun response ->
        if accept response
        then Ok ()
        else Error "response rejected by accept validator"
  in
  let model_strings, source =
    resolve_model_strings_traced ?config_path ~name ~defaults ()
  in
  let model_strings = expand_model_strings_for_execution model_strings in
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
  (* Sync discovery state before parsing so that endpoint_for_model can
     route llama:model_id to the correct endpoint. Without this, the
     model_endpoints atomic is empty and all llama models fall back to
     round-robin, which may route to a server without that model. #677 *)
  let _discovery_statuses =
    let endpoints = Discovery.endpoints_from_env () in
    if endpoints <> [] then
      Discovery.refresh_and_sync ~sw ~net ~endpoints
    else []
  in
  (* Resolve per-cascade api_key_env overrides from config *)
  let api_key_env_overrides =
    match config_path with
    | Some path -> resolve_api_key_env ~config_path:path ~name
    | None -> []
  in
  let providers =
    let parsed = parse_model_strings ~temperature ~max_tokens ?system_prompt
        ~api_key_env_overrides model_strings in
    (* Propagate tool_choice to each provider config so it reaches the
       HTTP body (e.g. "tool_choice": "required" for OpenAI-compatible).
       Without this, tool_choice from Agent hooks is lost in cascade. *)
    let with_tc = match tool_choice with
      | Some tc -> List.map (fun (p : Provider_config.t) -> { p with tool_choice = Some tc }) parsed
      | None -> parsed
    in
    apply_provider_filter ~provider_filter ~label:"named" with_tc
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
          ?throttle ?priority ~accept:accept_result ~accept_on_exhaustion
          healthy_providers ~messages ~tools
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

let complete_cascade_stream = Cascade_executor.complete_cascade_stream

let complete_named_stream ~sw ~net ?clock ?config_path
    ~name ~defaults ~messages
    ?(tools = [])
    ?(temperature = Constants.Inference.default_temperature)
    ?(max_tokens = Constants.Inference.default_max_tokens)
    ?system_prompt ?tool_choice ?(strict_name = false)
    ?timeout_sec ?metrics ?priority ?provider_filter ~on_event () =
  let model_strings, source =
    resolve_model_strings_traced ?config_path ~name ~defaults ()
  in
  let model_strings = expand_model_strings_for_execution model_strings in
  if strict_name && source <> Named then
    Error (Http_client.NetworkError {
      message = Printf.sprintf
        "Streaming cascade '%s' not found in config (resolved via %s)"
        name (match source with
              | Named -> "named"
              | Default_fallback -> "default_fallback"
              | Hardcoded_defaults -> "hardcoded_defaults") })
  else
  (* Resolve per-cascade api_key_env overrides from config *)
  let api_key_env_overrides =
    match config_path with
    | Some path -> resolve_api_key_env ~config_path:path ~name
    | None -> []
  in
  let providers =
    let parsed = parse_model_strings ~temperature ~max_tokens ?system_prompt
        ~api_key_env_overrides model_strings in
    let with_tc = match tool_choice with
      | Some tc -> List.map (fun (p : Provider_config.t) -> { p with tool_choice = Some tc }) parsed
      | None -> parsed
    in
    apply_provider_filter ~provider_filter ~label:"stream" with_tc
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

(* ── Local Capacity Query ──────────────────────────────── *)

type local_capacity = {
  total : int;
  process_active : int;
  process_available : int;
  process_queue_length : int;
  all_discovered : bool;
  endpoints_found : int;
}

let empty_capacity = {
  total = 0; process_active = 0; process_available = 0;
  process_queue_length = 0; all_discovered = true; endpoints_found = 0;
}

let local_capacity_for_selections ~sw ~net ?config_path selections =
  (* 1. Resolve each selection through the same path as complete_named *)
  let model_strings =
    selections
    |> List.concat_map (fun s ->
         resolve_model_strings ?config_path ~name:s ~defaults:[s] ())
    |> expand_model_strings_for_execution
    |> List.sort_uniq String.compare
  in
  (* 2. Parse to provider configs *)
  let providers = parse_model_strings model_strings in
  (* 3. Filter to local providers *)
  let local_urls =
    providers
    |> List.filter is_local_provider
    |> List.map (fun (cfg : Provider_config.t) -> cfg.base_url)
    |> List.sort_uniq String.compare
  in
  if local_urls = [] then
    empty_capacity
  else begin
    (* 4. Probe endpoints not yet in throttle table (cold start) *)
    let need_probe =
      List.filter (fun url -> Cascade_throttle.lookup url = None) local_urls
    in
    if need_probe <> [] then begin
      let statuses = Discovery.discover ~sw ~net ~endpoints:need_probe in
      Cascade_throttle.populate statuses
    end;
    (* 5. Aggregate capacity from throttle table *)
    let infos =
      List.filter_map (fun url -> Cascade_throttle.capacity url) local_urls
    in
    match infos with
    | [] -> empty_capacity
    | _ ->
      List.fold_left (fun acc (info : Cascade_throttle.capacity_info) ->
        { total = acc.total + info.total;
          process_active = acc.process_active + info.process_active;
          process_available = acc.process_available + info.process_available;
          process_queue_length = acc.process_queue_length + info.process_queue_length;
          all_discovered = acc.all_discovered
            && info.source = Provider_throttle.Discovered;
          endpoints_found = acc.endpoints_found + 1;
        })
        { empty_capacity with all_discovered = true }
        infos
  end

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

let%test "expand_auto_models glm:auto expands" =
  let expanded = expand_auto_models ["glm:auto"] in
  List.length expanded >= 2
  && List.hd expanded = "glm:glm-5.1"
  && List.exists (fun s -> s = "glm:glm-5-turbo") expanded

let%test "expand_auto_models non-auto passes through" =
  expand_auto_models ["glm:glm-5.1"; "ollama:auto"] =
    ["glm:glm-5.1"; "ollama:auto"]

let%test "expand_auto_models mixed" =
  let expanded = expand_auto_models ["glm:auto"; "ollama:auto"] in
  List.length expanded >= 3
  && List.hd expanded = "glm:glm-5.1"
  && List.nth expanded (List.length expanded - 1) = "ollama:auto"

let%test "parse_model_strings empty list" =
  parse_model_strings [] = []

let%test "parse_model_strings filters unavailable" =
  let results = parse_model_strings ["llama:qwen"; "badprovider:x"] in
  List.length results = 1

let%test "is_local_provider 127.0.0.1" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:Constants.Endpoints.default_url () in
  is_local_provider cfg = true

let%test "is_local_provider localhost with port" =
  let cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"m"
    ~base_url:Constants.Endpoints.default_url_localhost () in
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
    content = []; usage = None; telemetry = None } in
  text_of_response resp = ""

let%test "text_of_response text blocks concatenated" =
  let resp : Types.api_response = {
    id = ""; model = ""; stop_reason = EndTurn;
    content = [Types.Text "hello"; Types.Text " world"]; usage = None; telemetry = None } in
  text_of_response resp = "hello world"

let%test "text_of_response non-text blocks ignored" =
  let resp : Types.api_response = {
    id = ""; model = ""; stop_reason = EndTurn;
    content = [
      Types.ToolUse { id = "t1"; name = "tool"; input = `Null };
      Types.Text "only this";
    ]; usage = None; telemetry = None } in
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

let%test "default_registry has 12 providers" =
  List.length (Provider_registry.all default_registry) = 12

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
    ~base_url:Constants.Endpoints.default_url_localhost () in
  let no_tool_cfg = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-no-tools"
    ~base_url:Constants.Endpoints.default_url_localhost () in
  let result = filter_by_capabilities
    ~pred:(fun c -> c.Capabilities.supports_tools)
    [tool_cfg; no_tool_cfg] in
  List.length result = 1

let%test "filter_by_capabilities returns all if none match" =
  let cfg1 = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-a"
    ~base_url:Constants.Endpoints.default_url_localhost () in
  let cfg2 = Provider_config.make ~kind:OpenAI_compat ~model_id:"unknown-b"
    ~base_url:Constants.Endpoints.default_url_localhost () in
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

(* ── resolve_effective_api_key_env inline tests ──────── *)

let%test "resolve_effective_api_key_env provider match" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("glm", "CUSTOM_KEY")]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "CUSTOM_KEY"

let%test "resolve_effective_api_key_env wildcard match" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("*", "WILDCARD_KEY")]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "WILDCARD_KEY"

let%test "resolve_effective_api_key_env provider over wildcard" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("glm", "SPECIFIC"); ("*", "WILDCARD")]
    ~provider_name:"glm"
    ~registry_default:"DEFAULT"
  = "SPECIFIC"

let%test "resolve_effective_api_key_env no match uses registry default" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("claude", "CLAUDE_KEY")]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "ZAI_API_KEY"

let%test "resolve_effective_api_key_env empty overrides uses registry default" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "ZAI_API_KEY"

let%test "resolve_effective_api_key_env empty-string provider override falls through to wildcard" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("glm", ""); ("*", "WILDCARD_KEY")]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "WILDCARD_KEY"

let%test "resolve_effective_api_key_env empty-string wildcard falls through to registry default" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("*", "")]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "ZAI_API_KEY"

let%test "resolve_effective_api_key_env both empty falls through to registry default" =
  resolve_effective_api_key_env
    ~api_key_env_overrides:[("glm", ""); ("*", "")]
    ~provider_name:"glm"
    ~registry_default:"ZAI_API_KEY"
  = "ZAI_API_KEY"

let%test "parse_model_strings with empty overrides same as without" =
  let with_ov = parse_model_strings ~api_key_env_overrides:[] ["llama:qwen"] in
  let without = parse_model_strings ["llama:qwen"] in
  List.length with_ov = List.length without

let%test "resolve_api_key_env string format" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"test_api_key_env": "MY_KEY"}|} (fun tmp ->
      let overrides = resolve_api_key_env ~config_path:tmp ~name:"test" in
      overrides = [("*", "MY_KEY")]))

let%test "resolve_api_key_env object format" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json
      {|{"test_api_key_env": {"glm": "GLM_KEY", "claude": "CLAUDE_KEY"}}|}
      (fun tmp ->
        let overrides = resolve_api_key_env ~config_path:tmp ~name:"test" in
        List.length overrides = 2
        && List.assoc_opt "glm" overrides = Some "GLM_KEY"
        && List.assoc_opt "claude" overrides = Some "CLAUDE_KEY"))

let%test "resolve_api_key_env falls back to default" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"default_api_key_env": "FALLBACK"}|} (fun tmp ->
      let overrides = resolve_api_key_env ~config_path:tmp ~name:"missing" in
      overrides = [("*", "FALLBACK")]))

let%test "resolve_api_key_env empty on no match" =
  Eio_main.run (fun _env ->
    with_temp_cascade_json {|{"other_field": "x"}|} (fun tmp ->
      let overrides = resolve_api_key_env ~config_path:tmp ~name:"test" in
      overrides = []))

(* should_cascade_to_next + has_required_api_key tests
   moved to Cascade_health_filter *)
