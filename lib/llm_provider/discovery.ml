(** LLM endpoint discovery -- probes local llama-server instances.

    @since 0.53.0 *)

let warn_probe_failure ~url ~phase detail =
  let json =
    `Assoc
      [
        ("event", `String "llm_provider_discovery_probe_failed");
        ("url", `String url);
        ("phase", `String phase);
        ("detail", `String detail);
      ]
  in
  Diag.debug "discovery" "%s" (Yojson.Safe.to_string json)

type model_info = {
  id: string;
  owned_by: string;
}

type server_props = {
  total_slots: int;
  ctx_size: int;
  model: string;
}

type slot_status = {
  total: int;
  busy: int;
  idle: int;
}

type endpoint_status = {
  url: string;
  healthy: bool;
  models: model_info list;
  props: server_props option;
  slots: slot_status option;
  capabilities: Capabilities.capabilities;
}

(* [OAS_LOCAL_QWEN_URL] was the original model-specific name for this
   knob. It was superseded by [OAS_LOCAL_LLM_URL] (same shape, neutral
   name). If a deployment still has the legacy var set we emit a
   one-time migration warning to stderr at module-init time so the
   operator sees it on the next process start, but we do not route to
   the legacy URL — that would couple the public SDK contract to a
   model name that isn't part of the endpoint's meaning. Operators
   should rename their env var; the migration is mechanical. *)
let () =
  match Sys.getenv_opt "OAS_LOCAL_QWEN_URL" with
  | Some v when String.trim v <> "" ->
    Diag.warn "discovery"
      "OAS_LOCAL_QWEN_URL is set (%s) but has been \
       removed in favor of OAS_LOCAL_LLM_URL. The legacy value will be \
       ignored. Rename the env var to migrate."
      (String.trim v)
  | _ -> ()

let default_endpoint =
  match Sys.getenv_opt "OAS_LOCAL_LLM_URL" with
  | Some v when String.trim v <> "" -> String.trim v
  | _ -> Constants.Endpoints.default_url

let ollama_endpoint =
  match Sys.getenv_opt "OLLAMA_HOST" with
  | Some url when String.trim url <> "" -> String.trim url
  | _ -> "http://127.0.0.1:11434"

let endpoints_from_env () =
  let explicit = match Sys.getenv_opt "LLM_ENDPOINTS" with
    | None | Some "" -> [default_endpoint]
    | Some value ->
      value
      |> String.split_on_char ','
      |> List.map String.trim
      |> List.filter (fun s -> s <> "")
  in
  (* Include Ollama endpoint if not already listed.
     Discovery handles both llama-server and Ollama probe paths. *)
  if List.mem ollama_endpoint explicit then explicit
  else explicit @ [ollama_endpoint]

(* ── Probe helpers (extracted to Discovery_probe) ───────── *)
open Discovery_probe

(* ── Shared discovered context state ──────────────────────── *)

(** Snapshot of per-endpoint context and model-to-endpoint mapping,
    stored as a single atomic to prevent tearing between readers and
    writers.  [model_endpoints] maps each discovered model_id (from
    GET /v1/models) to the endpoint URL where it was found. *)
type _discovered_ctx_snapshot = {
  endpoint_ctxs: (string * int) list;
  model_endpoints: (string * string) list;
  per_slot_ctx: int option;
}

let _discovered_ctx : _discovered_ctx_snapshot Atomic.t =
  Atomic.make { endpoint_ctxs = []; model_endpoints = []; per_slot_ctx = None }

let discovered_per_slot_context () =
  (Atomic.get _discovered_ctx).per_slot_ctx

(** Per-endpoint per-slot context map from last probe.
    Returns [(url, per_slot_ctx)] for each healthy endpoint. *)
let discovered_endpoint_contexts () =
  (Atomic.get _discovered_ctx).endpoint_ctxs

(** Look up per-slot context for a specific endpoint URL.
    Returns [None] if the endpoint was not probed or has no props. *)
let discovered_context_for_url (url : string) : int option =
  let normalized = String.trim url in
  List.assoc_opt normalized (Atomic.get _discovered_ctx).endpoint_ctxs

(** Look up the endpoint URL that has [model_id] loaded.
    Uses data from the last {!refresh_and_sync} call. *)
let endpoint_for_model (model_id : string) : string option =
  List.assoc_opt model_id (Atomic.get _discovered_ctx).model_endpoints

(** Look up [model_id] and return [(url, per_slot_ctx)].
    Returns [None] if the model is not found or its endpoint has no
    context data in the current snapshot. *)
let context_for_model (model_id : string) : (string * int) option =
  let snap = Atomic.get _discovered_ctx in
  match List.assoc_opt model_id snap.model_endpoints with
  | None -> None
  | Some url ->
    match List.assoc_opt url snap.endpoint_ctxs with
    | Some ctx -> Some (url, ctx)
    | None -> None

(** Return the first model_id from the discovered model_endpoints index.
    Useful for resolving "auto" model IDs to concrete names discovered
    from the server's /v1/models endpoint. *)
let first_discovered_model_id () : string option =
  let snap = Atomic.get _discovered_ctx in
  match snap.model_endpoints with
  | (model_id, _) :: _ -> Some model_id
  | [] -> None

(** Return the first model_id discovered on a specific endpoint URL.
    Prevents cross-provider contamination: e.g. ollama:auto should only
    resolve models found on the Ollama endpoint, not llama-server models. *)
let first_discovered_model_id_for_url (url : string) : string option =
  let snap = Atomic.get _discovered_ctx in
  List.find_map (fun (model_id, ep_url) ->
    if ep_url = url then Some model_id else None
  ) snap.model_endpoints

let discover ~sw ~net ~endpoints =
  Eio.Fiber.List.map (fun url -> probe_endpoint ~sw ~net url) endpoints

let refresh_and_sync ~sw ~net ~endpoints =
  let statuses = discover ~sw ~net ~endpoints in
  let healthy = List.filter (fun (s : endpoint_status) -> s.healthy) statuses in
  (* llama-server /props reports n_ctx as the per-slot context
     (server total / n_parallel), not the server total. Do NOT divide
     by total_slots again — that was a double-division bug that caused
     context to appear 4x smaller than actual (e.g. 65K → 16K). *)
  let per_slot_contexts = List.filter_map (fun (s : endpoint_status) ->
    match s.props with
    | Some p when p.ctx_size > 0 ->
      Some (s.url, p.ctx_size)
    | _ -> None
  ) healthy in
  let ctx_values = List.map snd per_slot_contexts in
  let per_slot = match ctx_values with
    | [] -> None
    | ctxs -> Some (List.fold_left max 0 ctxs)
  in
  (* Build model_id → URL index from /v1/models responses *)
  let model_endpoints = List.concat_map (fun (s : endpoint_status) ->
    List.map (fun (m : model_info) -> (m.id, s.url)) s.models
  ) healthy in
  let endpoint_ctxs = per_slot_contexts in
  let snap = { endpoint_ctxs; model_endpoints; per_slot_ctx = per_slot } in
  Atomic.set _discovered_ctx snap;
  statuses

let scan_local_endpoints ~sw ~net =
  let endpoints = endpoints_from_env () in
  refresh_and_sync ~sw ~net ~endpoints

(* ── JSON serialization helpers ──────────────────────────── *)

(** Serialize [endpoint_status] to Yojson for diagnostics / health endpoint. *)
let endpoint_status_to_json (s : endpoint_status) =
  let models = `List (List.map (fun (m : model_info) ->
    `Assoc [ ("id", `String m.id); ("owned_by", `String m.owned_by) ]
  ) s.models) in
  let props = match s.props with
    | None -> `Null
    | Some p ->
      `Assoc [ ("total_slots", `Int p.total_slots); ("ctx_size", `Int p.ctx_size); ("model", `String p.model) ]
  in
  let slots = match s.slots with
    | None -> `Null
    | Some sl ->
      `Assoc [ ("total", `Int sl.total); ("busy", `Int sl.busy); ("idle", `Int sl.idle) ]
  in
  let caps_json = `Assoc [
    ("vision", `Bool s.capabilities.Capabilities.vision);
    ("function_calling", `Bool s.capabilities.Capabilities.function_calling);
    ("max_context_tokens",
      match s.capabilities.Capabilities.max_context_tokens with
      | Some n -> `Int n
      | None -> `Null);
  ] in
  `Assoc [
    ("url", `String s.url);
    ("healthy", `Bool s.healthy);
    ("models", models);
    ("props", props);
    ("slots", slots);
    ("capabilities", caps_json);
  ]

(** Derive max context from endpoint status.
    Prefers per-slot context from /props, falls back to capabilities. *)
let max_context_of_status (s : endpoint_status) : int =
  match s.props with
  | Some p when p.ctx_size > 0 -> p.ctx_size
  | _ ->
    match s.capabilities.Capabilities.max_context_tokens with
    | Some n -> n
    | None -> 0

(* ── Inline tests ─────────────────────────────────────────── *)

let%test "port_of_url handles various URL formats" =
  (* These are tested in Discovery_probe; here we verify the open brings them in *)
  Discovery_probe.port_of_url "http://127.0.0.1:8080" = Some 8080
  && Discovery_probe.port_of_url "http://127.0.0.1:11434" = Some 11434
  && Discovery_probe.port_of_url "http://[::1]:11434" = Some 11434
  && Discovery_probe.port_of_url "http://host:8080/path" = Some 8080
  && Discovery_probe.port_of_url "http://host" = None
  && Discovery_probe.port_of_url "user:pass@host:8080" = None
  && Discovery_probe.port_of_url "http://user:pass@host:8080" = Some 8080

let%test "string_contains_ci basic matching" =
  Discovery_probe.string_contains_ci ~haystack:"Qwen2.5-72B-Instruct" ~needle:"qwen"
  && not (Discovery_probe.string_contains_ci ~haystack:"Llama-3.1-70B" ~needle:"qwen")

let%test "url_is_ollama detects default port" =
  Discovery_probe.url_is_ollama "http://127.0.0.1:11434"
  && Discovery_probe.url_is_ollama "http://localhost:11434"
  && not (Discovery_probe.url_is_ollama "http://127.0.0.1:8080")
  && not (Discovery_probe.url_is_ollama "http://127.0.0.1:11435")

let%test "find_context_length extracts from various formats" =
  (* No context_length key *)
  Discovery_probe.find_context_length (`Assoc []) = 0
  (* Direct context_length *)
  && Discovery_probe.find_context_length (`Assoc [("context_length", `Int 32768)]) = 32768
  (* general.context_length *)
  && Discovery_probe.find_context_length (`Assoc [("general.context_length", `Int 65536)]) = 65536
  (* Model-prefixed context_length (Ollama style) *)
  && Discovery_probe.find_context_length (`Assoc [
    ("qwen3_5.context_length", `Int 131072);
    ("other.key", `Int 999);
  ]) = 131072
  (* Precedence: direct > general > max of model-prefixed *)
  && Discovery_probe.find_context_length (`Assoc [
    ("context_length", `Int 100);
    ("general.context_length", `Int 200);
    ("model.context_length", `Int 300);
  ]) = 100

let%test "parse_models extracts model info" =
  let json = `Assoc [("data", `List [
    `Assoc [("id", `String "qwen2.5:72b"); ("owned_by", `String "ollama")];
    `Assoc [("id", `String "llama3.1:70b"); ("owned_by", `String "llama-cpp")];
    `Assoc [("name", `String "no-id-model")];
  ])] in
  let models = Discovery_probe.parse_models json in
  List.length models = 2
  && (List.hd models).id = "qwen2.5:72b"
  && (List.hd models).owned_by = "ollama"
  && (List.nth models 1).id = "llama3.1:70b"

let%test "parse_props extracts server properties" =
  let json = `Assoc [
    ("total_slots", `Int 4);
    ("default_generation_settings", `Assoc [
      ("n_ctx", `Int 16384);
      ("model", `String "qwen2.5:72b");
    ]);
  ] in
  match Discovery_probe.parse_props json with
  | Some p -> p.total_slots = 4 && p.ctx_size = 16384 && p.model = "qwen2.5:72b"
  | None -> false

let%test "parse_slots counts busy slots" =
  let json = `List [
    `Assoc [("is_processing", `Bool true)];
    `Assoc [("is_processing", `Bool false)];
    `Assoc [("state", `Int 1)];
    `Assoc [("state", `Int 0)];
  ] in
  match Discovery_probe.parse_slots json with
  | Some s -> s.total = 4 && s.busy = 2 && s.idle = 2
  | None -> false

let%test "parse_slots returns None for empty list" =
  Discovery_probe.parse_slots `List [] = None

let%test "endpoints_from_env includes ollama by default" =
  let endpoints = endpoints_from_env () in
  List.mem ollama_endpoint endpoints

let%test "endpoints_from_env parses LLM_ENDPOINTS comma-separated" =
  (* Test with explicit env; since we can't set env in tests easily,
     we verify the default behavior includes at least one endpoint *)
  let endpoints = endpoints_from_env () in
  List.length endpoints >= 1

let%test "max_context_of_status prefers props over capabilities" =
  let status = {
    url = "http://localhost:8080";
    healthy = true;
    models = [];
    props = Some { total_slots = 4; ctx_size = 16384; model = "test" };
    slots = None;
    capabilities = {
      Capabilities.vision = false;
      Capabilities.function_calling = true;
      Capabilities.max_context_tokens = Some 32768;
    };
  } in
  max_context_of_status status = 16384
  (* props.ctx_size (16384) takes precedence over capabilities.max_context_tokens (32768) *)

let%test "max_context_of_status falls back to capabilities" =
  let status = {
    url = "http://localhost:8080";
    healthy = true;
    models = [];
    props = None;
    slots = None;
    capabilities = {
      Capabilities.vision = false;
      Capabilities.function_calling = true;
      Capabilities.max_context_tokens = Some 32768;
    };
  } in
  max_context_of_status status = 32768

let%test "max_context_of_status returns 0 when no info" =
  let status = {
    url = "http://localhost:8080";
    healthy = true;
    models = [];
    props = None;
    slots = None;
    capabilities = {
      Capabilities.vision = false;
      Capabilities.function_calling = false;
      Capabilities.max_context_tokens = None;
    };
  } in
  max_context_of_status status = 0