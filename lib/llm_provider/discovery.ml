(** LLM endpoint discovery -- probes local llama-server instances.

    @since 0.53.0 *)

let warn_probe_failure ~url ~phase detail =
  let json =
    `Assoc
      [ "event", `String "llm_provider_discovery_probe_failed"
      ; "url", `String url
      ; "phase", `String phase
      ; "detail", `String detail
      ]
  in
  Diag.debug "discovery" "%s" (Yojson.Safe.to_string json)
;;

(* Discovery types + JSON parsers moved to {!Discovery_parse}.  The
   transparent record-alias re-exports below preserve every existing
   [Discovery.model_info] / [Discovery.server_props] /
   [Discovery.slot_status] caller without forcing them to switch
   imports. *)
type model_info = Discovery_parse.model_info =
  { id : string
  ; owned_by : string
  }

type server_props = Discovery_parse.server_props =
  { total_slots : int
  ; ctx_size : int
  ; model : string
  ; supports_tools : bool option
  }

type slot_status = Discovery_parse.slot_status =
  { total : int
  ; busy : int
  ; idle : int
  }

type endpoint_status =
  { url : string
  ; healthy : bool
  ; models : model_info list
  ; props : server_props option
  ; slots : slot_status option
  ; capabilities : Capabilities.capabilities
  }

let local_llm_url_env_var = "OAS_LOCAL_LLM_URL"
let llm_endpoints_env_var = "LLM_ENDPOINTS"
let ollama_host_env_var = "OLLAMA_HOST"
let default_ollama_endpoint = "http://127.0.0.1:11434"
let env_get_non_empty ~getenv name = getenv name |> Cli_common_env.trim_non_empty_opt
let default_endpoint = Constants.Endpoints.default_url

let resolve_default_endpoint ?(getenv = fun name -> Cli_common_env.get name) () =
  match env_get_non_empty ~getenv local_llm_url_env_var with
  | Some v -> v
  | None -> Constants.Endpoints.default_url
;;

let ollama_endpoint = default_ollama_endpoint

let resolve_ollama_endpoint ?(getenv = fun name -> Cli_common_env.get name) () =
  match env_get_non_empty ~getenv ollama_host_env_var with
  | Some url -> url
  | None -> default_ollama_endpoint
;;

let parse_llm_endpoints_env ?(getenv = fun name -> Cli_common_env.get name) () =
  match env_get_non_empty ~getenv llm_endpoints_env_var with
  | Some urls -> Cli_common_env.split_on_char_trim ',' urls
  | None -> []
;;

let endpoints_from_env ?(getenv = fun name -> Cli_common_env.get name) () =
  let explicit =
    match parse_llm_endpoints_env ~getenv () with
    | [] -> [ resolve_default_endpoint ~getenv () ]
    | urls -> urls
  in
  (* Include Ollama endpoint if not already listed.
     Discovery handles both llama-server and Ollama probe paths. *)
  let ollama_endpoint = resolve_ollama_endpoint ~getenv () in
  if List.mem ollama_endpoint explicit then explicit else explicit @ [ ollama_endpoint ]
;;

(* HTTP helpers moved to {!Discovery_http} so the exhaustive
   [Http_client.http_error] pattern match lives in one place.
   Re-export keeps the in-file probe call sites
   (lines ~250, 480, 487, 510) unchanged. *)
let get_json = Discovery_http.get_json
let get_ok = Discovery_http.get_ok

(* Parsers moved to {!Discovery_parse}; re-export so the in-file
   [probe_endpoint] call sites still type-check unchanged. *)
let parse_models = Discovery_parse.parse_models
let parse_props = Discovery_parse.parse_props
let parse_slots = Discovery_parse.parse_slots

(* ── Ollama fallback ────────────────────────────────────── *)

(** Search for context_length in an Ollama model_info JSON object.
    Ollama uses model-specific key prefixes (e.g. "qwen3_5.context_length")
    rather than the generic "general.context_length".  Apply deterministic
    precedence when multiple keys are present: prefer "context_length",
    then "general.context_length", then take the maximum value across any
    remaining keys ending with ".context_length".  Returns 0 if none. *)
let find_context_length (model_info : Yojson.Safe.t) : int =
  let int_value = function
    | `Int n -> Some n
    | `Float f -> Some (int_of_float f)
    | _ -> None
  in
  match model_info with
  | `Assoc pairs ->
    let preferred key =
      List.find_map (fun (k, value) -> if k = key then int_value value else None) pairs
    in
    (match preferred "context_length" with
     | Some n -> n
     | None ->
       (match preferred "general.context_length" with
        | Some n -> n
        | None ->
          pairs
          |> List.filter_map (fun (key, value) ->
            if String.ends_with ~suffix:".context_length" key
            then int_value value
            else None)
          |> List.fold_left max 0))
  | _ -> 0
;;

(** Detect tool-calling support from a chat template string.
    Checks for tool-related keywords and special tokens used by
    various model families (DashScope, Llama, Mistral, etc.). *)
let template_has_tool_support (template : string) : bool =
  let has_tool_keyword =
    Retry.contains_case_insensitive ~haystack:template ~needle:"tools"
    || Retry.contains_case_insensitive ~haystack:template ~needle:"Tool"
  in
  let has_tool_call_token =
    List.exists
      (fun needle -> Retry.contains_case_insensitive ~haystack:template ~needle)
      [ "<|tool_call|>"
      ; "<|tool_calls|>"
      ; ".tool_call"
      ; "<|im_tool|>"
      ; "<|function_call"
      ]
  in
  has_tool_call_token || (String.index_opt template '{' <> None && has_tool_keyword)
;;

(** Try to detect Ollama via /api/tags and retrieve actual context size
    via /api/show. Returns synthetic server_props on success. *)
let probe_ollama_context ~sw ~net base_url =
  match get_json ~sw ~net (base_url ^ "/api/tags") with
  | Error detail ->
    warn_probe_failure ~url:base_url ~phase:"ollama_tags" detail;
    None
  | Ok tags_json ->
    let open Yojson.Safe.Util in
    let models =
      match member "models" tags_json with
      | `List items -> items
      | _ -> []
    in
    let first_name =
      List.find_map
        (fun item ->
           match member "name" item |> to_string_option with
           | Some name when name <> "" -> Some name
           | _ -> None)
        models
    in
    (match first_name with
     | None -> None
     | Some model_name ->
       let body = Yojson.Safe.to_string (`Assoc [ "name", `String model_name ]) in
       let headers = [ "content-type", "application/json" ] in
       (match
          Http_client.post_sync ~sw ~net ~url:(base_url ^ "/api/show") ~headers ~body ()
        with
        | Ok (code, resp_body) when code >= 200 && code < 300 ->
          (try
             let json = Yojson.Safe.from_string resp_body in
             let model_info = member "model_info" json in
             let ctx = find_context_length model_info in
             if ctx > 0
             then (
               let template =
                 match member "template" json with
                 | `String s -> s
                 | _ -> ""
               in
               let has_tools = template_has_tool_support template in
               Some
                 { total_slots = 1
                 ; ctx_size = ctx
                 ; model = model_name
                 ; supports_tools = Some has_tools
                 })
             else (
               warn_probe_failure
                 ~url:base_url
                 ~phase:"ollama_show"
                 "model_info contained no usable context length";
               None)
           with
           | Yojson.Json_error msg ->
             warn_probe_failure ~url:base_url ~phase:"ollama_show_json" msg;
             None
           | Yojson.Safe.Util.Type_error (msg, _) ->
             warn_probe_failure ~url:base_url ~phase:"ollama_show_parse" msg;
             None)
        | Ok (code, _) ->
          warn_probe_failure
            ~url:base_url
            ~phase:"ollama_show_http"
            (Printf.sprintf "HTTP %d" code);
          None
        | Error err ->
          let detail =
            match err with
            | Http_client.HttpError { code; body } ->
              Printf.sprintf "HTTP %d: %s" code body
            | Http_client.NetworkError { message; _ } -> message
            | Http_client.TimeoutError { message; _ } -> message
            | Http_client.AcceptRejected { reason } -> reason
            | Http_client.ProviderTerminal { message; _ } -> message
            | Http_client.ProviderFailure { kind; message } ->
              Http_client.provider_failure_to_string ~kind ~message
          in
          warn_probe_failure ~url:base_url ~phase:"ollama_show_http" detail;
          None))
;;

(* ── Capability inference ────────────────────────────────── *)

(** Overlay Ollama native [/api/chat] [think] behavior onto a capability record
    returned by a model-specific lookup ([for_model_id]). Vendor identity
    (which endpoint, which model) is decided by the caller; this function only
    encodes the endpoint wire-format consequence. *)
let apply_ollama_think_overlay (caps : Capabilities.capabilities)
  : Capabilities.capabilities
  =
  { caps with
    supports_seed = true
  ; supports_seed_with_images = true
  ; supports_tool_choice = false
  ; thinking_control_format = Capabilities.Ollama_think
  }
;;

(** Infer capabilities from model info and server props.
    Priority: model-specific lookup > generic inference > default.
    When [uses_ollama_think] is [true] the endpoint speaks Ollama native
    [/api/chat] [think] format, so {!apply_ollama_think_overlay} is layered on
    top of the model-specific lookup result, or [ollama_capabilities] is used as
    the fallback base when no lookup match exists. *)
let infer_capabilities ~uses_ollama_think models props =
  (* 1. Try model-specific lookup *)
  let from_lookup =
    List.find_map (fun (m : model_info) -> Capabilities.for_model_id m.id) models
  in
  let base =
    match from_lookup with
    | Some caps ->
      (* Overlay Ollama endpoint flags while keeping model-specific
         context-window and reasoning metadata. *)
      if uses_ollama_think then apply_ollama_think_overlay caps else caps
    | None ->
      if uses_ollama_think
      then
        (* Ollama native base: inherits extended reasoning, top_k/min_p, and
           the seed/tool_choice/[think] flags that travel with this wire
           format. Dynamic tool support from /api/show template analysis is
           merged below via props handling in step 3. *)
        Capabilities.ollama_capabilities
      else (
        (* 2. Generic inference by model name for non-Ollama endpoints *)
        let needs_extended =
          List.exists
            (fun (m : model_info) ->
               Retry.contains_case_insensitive ~haystack:m.id ~needle:"dashscope")
            models
        in
        if needs_extended
        then Capabilities.openai_compat_chat_extended_capabilities
        else Capabilities.openai_compat_chat_capabilities)
  in
  (* 3. Merge ctx_size from /props into capabilities *)
  match props with
  | Some (p : server_props) ->
    let c = Capabilities.with_context_size base ~ctx_size:p.ctx_size in
    (match p.supports_tools with
     | Some t -> Capabilities.with_tool_support c ~supports_tools:t
     | None -> c)
  | None -> base
;;

(* ── Probe ───────────────────────────────────────────────── *)

(** Extract the [host:port] authority from a URL string, ignoring
    userinfo, path, query, and fragment.  Returns [Some port] only when
    a numeric port is present in the authority section.  Used by
    {!url_is_ollama}; see that function for the matching contract. *)
let port_of_url (url : string) : int option =
  let s = String.trim url in
  let len = String.length s in
  (* Skip scheme:// if present *)
  let start =
    match String.index_opt s ':' with
    | Some i when i + 2 < len && s.[i + 1] = '/' && s.[i + 2] = '/' -> i + 3
    | _ -> 0
  in
  (* Authority ends at first '/', '?' or '#'. *)
  let stop =
    let rec scan i =
      if i >= len
      then len
      else (
        match s.[i] with
        | '/' | '?' | '#' -> i
        | _ -> scan (i + 1))
    in
    scan start
  in
  let authority = String.sub s start (stop - start) in
  (* Strip userinfo (everything before the LAST '@' inside authority). *)
  let host_port =
    match String.rindex_opt authority '@' with
    | Some i -> String.sub authority (i + 1) (String.length authority - i - 1)
    | None -> authority
  in
  (* IPv6 literal: [::1]:11434 — host is in brackets, port follows ']'. *)
  let port_str =
    if String.length host_port > 0 && host_port.[0] = '['
    then (
      match String.index_opt host_port ']' with
      | Some i when i + 1 < String.length host_port && host_port.[i + 1] = ':' ->
        Some (String.sub host_port (i + 2) (String.length host_port - i - 2))
      | _ -> None)
    else (
      match String.rindex_opt host_port ':' with
      | Some i -> Some (String.sub host_port (i + 1) (String.length host_port - i - 1))
      | None -> None)
  in
  Option.bind port_str int_of_string_opt
;;

(** Heuristic: does this URL look like an Ollama endpoint?
    Used to skip llama.cpp-only probe paths (/props, /slots) that always
    return 404 on Ollama and pollute logs. Matches when:
    - the URL's authority port is exactly 11434 (Ollama default), OR
    - the trimmed URL equals the configured [ollama_endpoint]
      (env [OLLAMA_HOST] or default)
    The port match is intentionally strict — substring matching on
    ":11434" gives false positives on userinfo (user:11434@host),
    paths (/api/:11434), and query strings (?p=:11434). *)
let url_is_ollama ?(getenv = fun name -> Cli_common_env.get name) (url : string) : bool =
  match port_of_url url with
  | Some 11434 -> true
  | _ -> String.equal (String.trim url) (String.trim (resolve_ollama_endpoint ~getenv ()))
;;

let probe_endpoint ~sw ~net url =
  let base = String.trim url in
  let warn phase detail = warn_probe_failure ~url:base ~phase detail in
  let capture_json phase parse target =
    match get_json ~sw ~net target with
    | Ok json -> parse json
    | Error detail ->
      warn phase detail;
      None
  in
  (* Try /health first (llama.cpp), fall back to / (Ollama returns 200) *)
  let healthy = get_ok ~sw ~net (base ^ "/health") || get_ok ~sw ~net base in
  if not healthy
  then
    { url = base
    ; healthy = false
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  else (
    let is_ollama = url_is_ollama base in
    (* Fetch models, props, and slots concurrently via Eio fibers.
       Skip /props and /slots probes for Ollama endpoints — those are
       llama.cpp-only paths and always 404 on Ollama, polluting logs.
       Ollama context is recovered via probe_ollama_context below. *)
    let models_ref = ref [] in
    let props_ref = ref None in
    let slots_ref = ref None in
    let fibers =
      [ (fun () ->
          models_ref
          := match get_json ~sw ~net (base ^ "/v1/models") with
             | Ok json -> parse_models json
             | Error detail ->
               warn "models" detail;
               [])
      ]
      @
      if is_ollama
      then []
      else
        [ (fun () -> props_ref := capture_json "props" parse_props (base ^ "/props"))
        ; (fun () -> slots_ref := capture_json "slots" parse_slots (base ^ "/slots"))
        ]
    in
    Eio.Fiber.all fibers;
    let models = !models_ref in
    let slots = !slots_ref in
    (* Ollama fallback: if /props failed (or was skipped), try /api/tags + /api/show *)
    let props =
      match !props_ref with
      | Some _ as p -> p
      | None -> probe_ollama_context ~sw ~net base
    in
    (* Vendor-to-behavior mapping: Ollama endpoints currently speak the
       reasoning_effort wire format. [is_ollama] stays the local probe
       result (used above for /props /slots skipping — a *protocol*
       routing concern), but it is mapped to the behavior axis at the
       capability-inference boundary. *)
    let capabilities = infer_capabilities ~uses_ollama_think:is_ollama models props in
    { url = base; healthy; models; props; slots; capabilities })
;;

(* ── Shared discovered context state ──────────────────────── *)

(** Snapshot of per-endpoint context and model-to-endpoint mapping,
    stored as a single atomic to prevent tearing between readers and
    writers.  [model_endpoints] maps each discovered model_id (from
    GET /v1/models) to the endpoint URL where it was found. *)
type _discovered_ctx_snapshot =
  { endpoint_ctxs : (string * int) list
  ; model_endpoints : (string * string) list
  ; per_slot_ctx : int option
  }

let _discovered_ctx : _discovered_ctx_snapshot Atomic.t =
  Atomic.make { endpoint_ctxs = []; model_endpoints = []; per_slot_ctx = None }
;;

let discovered_per_slot_context () = (Atomic.get _discovered_ctx).per_slot_ctx

(** Per-endpoint per-slot context map from last probe.
    Returns [(url, per_slot_ctx)] for each healthy endpoint. *)
let discovered_endpoint_contexts () = (Atomic.get _discovered_ctx).endpoint_ctxs

(** Look up per-slot context for a specific endpoint URL.
    Returns [None] if the endpoint was not probed or has no props. *)
let discovered_context_for_url (url : string) : int option =
  let normalized = String.trim url in
  List.assoc_opt normalized (Atomic.get _discovered_ctx).endpoint_ctxs
;;

(** Look up the endpoint URL that has [model_id] loaded.
    Uses data from the last {!refresh_and_sync} call. *)
let endpoint_for_model (model_id : string) : string option =
  List.assoc_opt model_id (Atomic.get _discovered_ctx).model_endpoints
;;

(** Look up [model_id] and return [(url, per_slot_ctx)].
    Returns [None] if the model is not found or its endpoint has no
    context data in the current snapshot. *)
let context_for_model (model_id : string) : (string * int) option =
  let snap = Atomic.get _discovered_ctx in
  match List.assoc_opt model_id snap.model_endpoints with
  | None -> None
  | Some url ->
    (match List.assoc_opt url snap.endpoint_ctxs with
     | Some ctx -> Some (url, ctx)
     | None -> None)
;;

(** Return the first model_id from the discovered model_endpoints index.
    Useful for resolving "auto" model IDs to concrete names discovered
    from the server's /v1/models endpoint. *)
let first_discovered_model_id () : string option =
  let snap = Atomic.get _discovered_ctx in
  match snap.model_endpoints with
  | (model_id, _) :: _ -> Some model_id
  | [] -> None
;;

(** Return the first model_id discovered on a specific endpoint URL.
    Prevents cross-provider contamination: e.g. ollama:auto should only
    resolve models found on the Ollama endpoint, not llama-server models. *)
let first_discovered_model_id_for_url (url : string) : string option =
  let snap = Atomic.get _discovered_ctx in
  List.find_map
    (fun (model_id, ep_url) -> if ep_url = url then Some model_id else None)
    snap.model_endpoints
;;

let discover ~sw ~net ~endpoints =
  Eio.Fiber.List.map (fun url -> probe_endpoint ~sw ~net url) endpoints
;;

let refresh_and_sync ~sw ~net ~endpoints =
  let statuses = discover ~sw ~net ~endpoints in
  let healthy = List.filter (fun (s : endpoint_status) -> s.healthy) statuses in
  (* llama-server /props reports n_ctx as the per-slot context
     (server total / n_parallel), not the server total. Do NOT divide
     by total_slots again — that was a double-division bug that caused
     context to appear 4x smaller than actual (e.g. 65K → 16K). *)
  let per_slot_contexts =
    List.filter_map
      (fun (s : endpoint_status) ->
         match s.props with
         | Some p when p.ctx_size > 0 -> Some (s.url, p.ctx_size)
         | _ -> None)
      healthy
  in
  let ctx_values = List.map snd per_slot_contexts in
  let per_slot =
    match ctx_values with
    | [] -> None
    | ctxs -> Some (List.fold_left max 0 ctxs)
  in
  (* Build model_id → URL index from /v1/models responses *)
  let model_endpoints =
    List.concat_map
      (fun (s : endpoint_status) ->
         List.map (fun (m : model_info) -> m.id, s.url) s.models)
      healthy
  in
  Atomic.set
    _discovered_ctx
    { endpoint_ctxs = per_slot_contexts; model_endpoints; per_slot_ctx = per_slot };
  statuses
;;

let builtin_scan_ports = [ 8085; 8086; 8087; 8088; 8089; 8090; 11434 ]
let valid_tcp_port p = p >= 1 && p <= 65535
let discovery_ports_env = "OAS_DISCOVERY_PORTS"
let discovery_diag_context = "discovery"

type invalid_scan_port_reason =
  | Not_an_integer
  | Outside_tcp_port_range

type invalid_scan_port =
  { token : string
  ; reason : invalid_scan_port_reason
  }

let invalid_scan_port_reason_to_string = function
  | Not_an_integer -> "not an integer"
  | Outside_tcp_port_range -> "outside TCP port range 1-65535"
;;

let warn_invalid_discovery_port ~env_var ~diag_context ~token ~reason =
  Diag.warn
    diag_context
    "%s token %S ignored: %s"
    env_var
    token
    (invalid_scan_port_reason_to_string reason)
;;

let missing_invalid_port_handler ~token ~reason =
  invalid_arg
    (Printf.sprintf
       "parse_ports_env invalid token %S: %s"
       token
       (invalid_scan_port_reason_to_string reason))
;;

let parse_ports_env_result s =
  String.split_on_char ',' s
  |> List.fold_left
       (fun (ports, invalids) raw ->
          let trimmed = String.trim raw in
          match int_of_string_opt trimmed with
          | None when trimmed = "" ->
            (* Empty tokens make trailing/repeated separators harmless. *)
            ports, invalids
          | Some p when valid_tcp_port p && not (List.mem p ports) -> p :: ports, invalids
          | Some p when valid_tcp_port p ->
            (* Keep the first occurrence so user-provided probe order is stable. *)
            ports, invalids
          | Some _ ->
            ports, { token = trimmed; reason = Outside_tcp_port_range } :: invalids
          | None -> ports, { token = trimmed; reason = Not_an_integer } :: invalids)
       ([], [])
  |> fun (ports, invalids) -> List.rev ports, List.rev invalids
;;

let parse_ports_env ?(on_invalid = missing_invalid_port_handler) s =
  let ports, invalids = parse_ports_env_result s in
  List.iter (fun { token; reason } -> on_invalid ~token ~reason) invalids;
  ports
;;

let default_scan_ports () =
  match Cli_common_env.get discovery_ports_env with
  | Some s ->
    (match
       parse_ports_env
         ~on_invalid:
           (warn_invalid_discovery_port
              ~env_var:discovery_ports_env
              ~diag_context:discovery_diag_context)
         s
     with
     | [] -> builtin_scan_ports
     | ps -> ps)
  | None -> builtin_scan_ports
;;

let scan_local_endpoints ?(ports = default_scan_ports ()) ~sw ~net () =
  let candidates = List.map (fun p -> Printf.sprintf "http://127.0.0.1:%d" p) ports in
  let statuses = discover ~sw ~net ~endpoints:candidates in
  List.filter_map
    (fun (s : endpoint_status) -> if s.healthy then Some s.url else None)
    statuses
;;

(* ── JSON serialization ──────────────────────────────────── *)

let model_info_to_json (m : model_info) =
  `Assoc [ "id", `String m.id; "owned_by", `String m.owned_by ]
;;

let server_props_to_json (p : server_props) =
  `Assoc
    [ "total_slots", `Int p.total_slots
    ; "ctx_size", `Int p.ctx_size
    ; "model", `String p.model
    ]
;;

let slot_status_to_json (s : slot_status) =
  `Assoc [ "total", `Int s.total; "busy", `Int s.busy; "idle", `Int s.idle ]
;;

let capabilities_to_json (c : Capabilities.capabilities) =
  `Assoc
    [ "reasoning", `Bool c.supports_reasoning
    ; "tools", `Bool c.supports_tools
    ; "streaming", `Bool c.supports_native_streaming
    ; "multimodal", `Bool c.supports_multimodal_inputs
    ; "json_output", `Bool c.supports_response_format_json
    ]
;;

let endpoint_status_to_json (e : endpoint_status) =
  let fields =
    [ "url", `String e.url
    ; "healthy", `Bool e.healthy
    ; "models", `List (List.map model_info_to_json e.models)
    ; "capabilities", capabilities_to_json e.capabilities
    ]
  in
  let fields =
    match e.props with
    | Some p -> fields @ [ "props", server_props_to_json p ]
    | None -> fields
  in
  let fields =
    match e.slots with
    | Some s -> fields @ [ "slots", slot_status_to_json s ]
    | None -> fields
  in
  `Assoc fields
;;

let summary_to_json endpoints =
  let total_capacity =
    List.fold_left
      (fun acc (e : endpoint_status) ->
         match e.slots with
         | Some s -> acc + s.total
         | None -> acc)
      0
      endpoints
  in
  let available_capacity =
    List.fold_left
      (fun acc (e : endpoint_status) ->
         match e.slots with
         | Some s -> acc + s.idle
         | None -> acc)
      0
      endpoints
  in
  let active_requests =
    List.fold_left
      (fun acc (e : endpoint_status) ->
         match e.slots with
         | Some s -> acc + s.busy
         | None -> acc)
      0
      endpoints
  in
  `Assoc
    [ "total_capacity", `Int total_capacity
    ; "available_capacity", `Int available_capacity
    ; "active_requests", `Int active_requests
    ]
;;

let max_context_of_status (status : endpoint_status) =
  match status.props with
  | Some p when p.ctx_size > 0 -> Some p.ctx_size
  | _ -> status.capabilities.max_context_tokens
;;

[@@@coverage off]
(* === Inline tests === *)

(* --- default_endpoint --- *)

let%test "default_endpoint is localhost:8085" =
  default_endpoint = Constants.Endpoints.default_url
;;

(* --- parse_llm_endpoints_env (SSOT helper, #1002) --- *)

let%test "parse_llm_endpoints_env empty when unset" =
  (match Sys.getenv_opt "LLM_ENDPOINTS" with
   | Some _ -> Unix.putenv "LLM_ENDPOINTS" ""
   | None -> ());
  parse_llm_endpoints_env () = []
;;

let%test "parse_llm_endpoints_env empty when env is blank" =
  Unix.putenv "LLM_ENDPOINTS" "";
  let res = parse_llm_endpoints_env () in
  res = []
;;

let%test "parse_llm_endpoints_env empty when env has only separators" =
  Unix.putenv "LLM_ENDPOINTS" " , , ";
  let res = parse_llm_endpoints_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  res = []
;;

let%test "parse_llm_endpoints_env preserves order and trims" =
  Unix.putenv "LLM_ENDPOINTS" "  http://a:8080 ,http://b:8081";
  let res = parse_llm_endpoints_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  res = [ "http://a:8080"; "http://b:8081" ]
;;

(* --- endpoints_from_env --- *)

let%test "endpoints_from_env default when unset" =
  (match Sys.getenv_opt "LLM_ENDPOINTS" with
   | Some _ -> Unix.putenv "LLM_ENDPOINTS" ""
   | None -> ());
  let eps = endpoints_from_env () in
  List.hd eps = default_endpoint && List.mem ollama_endpoint eps
;;

let%test "endpoints_from_env parses comma-separated" =
  Unix.putenv "LLM_ENDPOINTS" "http://a:8080,http://b:8081";
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  List.mem "http://a:8080" eps
  && List.mem "http://b:8081" eps
  && List.mem ollama_endpoint eps
;;

let%test "endpoints_from_env trims whitespace" =
  Unix.putenv "LLM_ENDPOINTS" "  http://a:8080 , http://b:8081  ";
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  List.mem "http://a:8080" eps && List.mem "http://b:8081" eps
;;

let%test "endpoints_from_env filters empty parts" =
  Unix.putenv "LLM_ENDPOINTS" "http://a,,http://b,";
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  List.mem "http://a" eps && List.mem "http://b" eps
;;

let%test "endpoints_from_env empty string returns default" =
  Unix.putenv "LLM_ENDPOINTS" "";
  let eps = endpoints_from_env () in
  List.hd eps = default_endpoint
;;

(* --- url_is_ollama --- *)

let%test "url_is_ollama matches default ollama port" =
  url_is_ollama "http://127.0.0.1:11434"
;;

let%test "url_is_ollama matches localhost variant" =
  url_is_ollama "http://localhost:11434"
;;

let%test "url_is_ollama trims whitespace" = url_is_ollama "  http://127.0.0.1:11434  "
let%test "url_is_ollama matches IPv6 literal" = url_is_ollama "http://[::1]:11434"

let%test "url_is_ollama matches with trailing path" =
  url_is_ollama "http://127.0.0.1:11434/api/tags"
;;

let%test "url_is_ollama rejects llama-server port" =
  not (url_is_ollama "http://127.0.0.1:8085")
;;

let%test "url_is_ollama rejects unrelated port" =
  not (url_is_ollama "http://127.0.0.1:8086")
;;

(* Codex review #793: confirm false-positive substring matches no longer fire. *)
let%test "url_is_ollama rejects :11434 in userinfo" =
  not (url_is_ollama "http://user:11434@example.com/v1")
;;

let%test "url_is_ollama rejects :11434 in path" =
  not (url_is_ollama "http://example.com/api/:11434")
;;

let%test "url_is_ollama rejects :11434 in query" =
  not (url_is_ollama "http://example.com:8080/?next=:11434")
;;

(* Codex review: cover the bare ":11434 in query" case with no other port. *)
let%test "url_is_ollama rejects :11434 in query (no other port)" =
  not (url_is_ollama "http://example.com/?next=:11434")
;;

let%test "url_is_ollama rejects :11434 in fragment" =
  not (url_is_ollama "http://example.com:8080/path#:11434")
;;

let%test "url_is_ollama rejects :11434 in fragment (no other port)" =
  not (url_is_ollama "http://example.com/path#frag:11434")
;;

let%test "url_is_ollama rejects port suffix collision (:111434)" =
  not (url_is_ollama "http://example.com:111434/")
;;

let%test "url_is_ollama rejects hostname starting with 11434" =
  not (url_is_ollama "http://11434.example.com/api")
;;

(* --- port_of_url --- *)

let%test "port_of_url default scheme://host:port" =
  port_of_url "http://127.0.0.1:8085" = Some 8085
;;

let%test "port_of_url no port returns None" = port_of_url "http://example.com/path" = None

let%test "port_of_url skips userinfo" =
  port_of_url "http://user:pass@example.com:9000/api" = Some 9000
;;

let%test "port_of_url ipv6 literal" = port_of_url "http://[::1]:8086" = Some 8086
let%test "port_of_url stops at path" = port_of_url "http://h:8085/x:9999" = Some 8085

let%test "endpoints_from_env does not duplicate ollama" =
  Unix.putenv "LLM_ENDPOINTS" (ollama_endpoint ^ ",http://a:8085");
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  List.length (List.filter (( = ) ollama_endpoint) eps) = 1
;;

(* --- parse_models --- *)

let%test "parse_models valid" =
  let json =
    `Assoc
      [ ( "data"
        , `List
            [ `Assoc [ "id", `String "dashscope-3.5-35b"; "owned_by", `String "local" ]
            ; `Assoc [ "id", `String "llama-4-scout"; "owned_by", `String "nous" ]
            ] )
      ]
  in
  let models = parse_models json in
  List.length models = 2 && (List.hd models).id = "dashscope-3.5-35b"
;;

let%test "parse_models empty data" =
  let json = `Assoc [ "data", `List [] ] in
  parse_models json = []
;;

let%test "parse_models missing data" =
  let json = `Assoc [] in
  parse_models json = []
;;

let%test "parse_models non-list data" =
  let json = `Assoc [ "data", `String "bad" ] in
  parse_models json = []
;;

let%test "parse_models missing id skipped" =
  let json = `Assoc [ "data", `List [ `Assoc [ "owned_by", `String "local" ] ] ] in
  parse_models json = []
;;

let%test "parse_models owned_by defaults to unknown" =
  let json = `Assoc [ "data", `List [ `Assoc [ "id", `String "model1" ] ] ] in
  let models = parse_models json in
  List.length models = 1 && (List.hd models).owned_by = "unknown"
;;

(* --- parse_props --- *)

let%test "parse_props valid" =
  let json =
    `Assoc
      [ "total_slots", `Int 4
      ; ( "default_generation_settings"
        , `Assoc [ "n_ctx", `Int 8192; "model", `String "dashscope-3.5" ] )
      ]
  in
  match parse_props json with
  | Some p -> p.total_slots = 4 && p.ctx_size = 8192 && p.model = "dashscope-3.5"
  | None -> false
;;

let%test "parse_props missing total_slots" =
  let json = `Assoc [] in
  parse_props json = None
;;

let%test "parse_props total_slots not int" =
  let json = `Assoc [ "total_slots", `String "4" ] in
  parse_props json = None
;;

let%test "parse_props missing dgs" =
  let json = `Assoc [ "total_slots", `Int 2 ] in
  match parse_props json with
  | Some p -> p.total_slots = 2 && p.ctx_size = 0 && p.model = ""
  | None -> false
;;

let%test "parse_props dgs missing n_ctx" =
  let json =
    `Assoc
      [ "total_slots", `Int 1
      ; "default_generation_settings", `Assoc [ "model", `String "m" ]
      ]
  in
  match parse_props json with
  | Some p -> p.ctx_size = 0 && p.model = "m"
  | None -> false
;;

let%test "parse_props dgs missing model" =
  let json =
    `Assoc
      [ "total_slots", `Int 1
      ; "default_generation_settings", `Assoc [ "n_ctx", `Int 4096 ]
      ]
  in
  match parse_props json with
  | Some p -> p.model = "" && p.ctx_size = 4096
  | None -> false
;;

(* --- parse_slots --- *)

let%test "parse_slots valid" =
  let json =
    `List
      [ `Assoc [ "is_processing", `Bool true ]
      ; `Assoc [ "is_processing", `Bool false ]
      ; `Assoc [ "is_processing", `Bool false ]
      ]
  in
  match parse_slots json with
  | Some s -> s.total = 3 && s.busy = 1 && s.idle = 2
  | None -> false
;;

let%test "parse_slots empty list" = parse_slots (`List []) = None
let%test "parse_slots non-list" = parse_slots (`String "bad") = None

let%test "parse_slots state-based busy detection" =
  let json = `List [ `Assoc [ "state", `Int 1 ]; `Assoc [ "state", `Int 0 ] ] in
  match parse_slots json with
  | Some s -> s.busy = 1 && s.idle = 1
  | None -> false
;;

let%test "parse_slots neither is_processing nor state defaults to idle" =
  let json = `List [ `Assoc [] ] in
  match parse_slots json with
  | Some s -> s.busy = 0 && s.idle = 1
  | None -> false
;;

(* --- contains_case_insensitive (via Retry SSOT) --- *)

let%test "contains_case_insensitive case insensitive match" =
  Retry.contains_case_insensitive ~haystack:"DashScope_3.5-35B" ~needle:"dashscope" = true
;;

let%test "contains_case_insensitive no match" =
  Retry.contains_case_insensitive ~haystack:"nous" ~needle:"dashscope" = false
;;

let%test "contains_case_insensitive needle longer than haystack" =
  Retry.contains_case_insensitive ~haystack:"ab" ~needle:"abcdef" = false
;;

let%test "contains_case_insensitive empty needle" =
  Retry.contains_case_insensitive ~haystack:"anything" ~needle:"" = true
;;

let%test "contains_case_insensitive exact match" =
  Retry.contains_case_insensitive ~haystack:"DASHSCOPE" ~needle:"dashscope" = true
;;

(* --- infer_capabilities --- *)

let%test "infer_capabilities dashscope model gets extended" =
  let models = [ { id = "DashScope_3.5-35B-A3B"; owned_by = "local" } ] in
  let caps = infer_capabilities ~uses_ollama_think:false models None in
  caps.supports_reasoning = true
  && caps.supports_top_k = true
  && caps.supports_min_p = true
;;

let%test "infer_capabilities unknown model gets basic openai" =
  let models = [ { id = "my-custom-model"; owned_by = "local" } ] in
  let caps = infer_capabilities ~uses_ollama_think:false models None in
  caps.supports_tools = true && caps.supports_reasoning = false
;;

let%test "infer_capabilities known model lookup has priority" =
  let models = [ { id = "claude-opus-4-20260320"; owned_by = "anthropic" } ] in
  let caps = infer_capabilities ~uses_ollama_think:false models None in
  caps.supports_caching = true && caps.supports_computer_use = true
;;

let%test "infer_capabilities merges ctx_size from props" =
  let models = [ { id = "my-model"; owned_by = "local" } ] in
  let props =
    Some { total_slots = 4; ctx_size = 32768; model = "my-model"; supports_tools = None }
  in
  let caps = infer_capabilities ~uses_ollama_think:false models props in
  caps.max_context_tokens = Some 32768
;;

let%test "infer_capabilities no models defaults" =
  let caps = infer_capabilities ~uses_ollama_think:false [] None in
  caps.supports_tools = true
;;

(* --- JSON serialization --- *)

let%test "model_info_to_json" =
  let json = model_info_to_json { id = "m1"; owned_by = "local" } in
  let open Yojson.Safe.Util in
  json |> member "id" |> to_string = "m1"
  && json |> member "owned_by" |> to_string = "local"
;;

let%test "server_props_to_json" =
  let json =
    server_props_to_json
      { total_slots = 4; ctx_size = 8192; model = "dashscope"; supports_tools = None }
  in
  let open Yojson.Safe.Util in
  json |> member "total_slots" |> to_int = 4 && json |> member "ctx_size" |> to_int = 8192
;;

let%test "slot_status_to_json" =
  let json = slot_status_to_json { total = 4; busy = 1; idle = 3 } in
  let open Yojson.Safe.Util in
  json |> member "total" |> to_int = 4 && json |> member "busy" |> to_int = 1
;;

let%test "capabilities_to_json" =
  let json = capabilities_to_json Capabilities.default_capabilities in
  let open Yojson.Safe.Util in
  json |> member "tools" |> to_bool = false
;;

let%test "endpoint_status_to_json without props and slots" =
  let es =
    { url = Constants.Endpoints.default_url_localhost
    ; healthy = true
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  let json = endpoint_status_to_json es in
  let open Yojson.Safe.Util in
  json |> member "url" |> to_string = Constants.Endpoints.default_url_localhost
  && json |> member "healthy" |> to_bool = true
;;

let%test "endpoint_status_to_json with props and slots" =
  let es =
    { url = Constants.Endpoints.default_url_localhost
    ; healthy = true
    ; models = [ { id = "m1"; owned_by = "local" } ]
    ; props =
        Some { total_slots = 4; ctx_size = 8192; model = "m1"; supports_tools = None }
    ; slots = Some { total = 4; busy = 1; idle = 3 }
    ; capabilities = Capabilities.default_capabilities
    }
  in
  let json = endpoint_status_to_json es in
  let open Yojson.Safe.Util in
  json |> member "props" |> member "total_slots" |> to_int = 4
  && json |> member "slots" |> member "busy" |> to_int = 1
;;

let%test "summary_to_json empty endpoints" =
  let json = summary_to_json [] in
  let open Yojson.Safe.Util in
  json |> member "total_capacity" |> to_int = 0
  && json |> member "available_capacity" |> to_int = 0
  && json |> member "active_requests" |> to_int = 0
;;

let%test "summary_to_json with slots" =
  let eps =
    [ { url = "a"
      ; healthy = true
      ; models = []
      ; props = None
      ; slots = Some { total = 4; busy = 1; idle = 3 }
      ; capabilities = Capabilities.default_capabilities
      }
    ; { url = "b"
      ; healthy = true
      ; models = []
      ; props = None
      ; slots = Some { total = 2; busy = 2; idle = 0 }
      ; capabilities = Capabilities.default_capabilities
      }
    ]
  in
  let json = summary_to_json eps in
  let open Yojson.Safe.Util in
  json |> member "total_capacity" |> to_int = 6
  && json |> member "available_capacity" |> to_int = 3
  && json |> member "active_requests" |> to_int = 3
;;

let%test "summary_to_json endpoint without slots ignored" =
  let eps =
    [ { url = "a"
      ; healthy = false
      ; models = []
      ; props = None
      ; slots = None
      ; capabilities = Capabilities.default_capabilities
      }
    ]
  in
  let json = summary_to_json eps in
  let open Yojson.Safe.Util in
  json |> member "total_capacity" |> to_int = 0
;;

(* --- max_context_of_status --- *)

let%test "max_context_of_status from props" =
  let status =
    { url = "http://localhost"
    ; healthy = true
    ; models = []
    ; props =
        Some { total_slots = 4; ctx_size = 32768; model = "m"; supports_tools = None }
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  max_context_of_status status = Some 32768
;;

let%test "max_context_of_status from capabilities when no props" =
  let caps = { Capabilities.default_capabilities with max_context_tokens = Some 16384 } in
  let status =
    { url = "http://localhost"
    ; healthy = true
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = caps
    }
  in
  max_context_of_status status = Some 16384
;;

let%test "max_context_of_status None when no info" =
  let status =
    { url = "http://localhost"
    ; healthy = true
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  max_context_of_status status = None
;;

let%test "max_context_of_status prefers props over capabilities" =
  let caps = { Capabilities.default_capabilities with max_context_tokens = Some 8192 } in
  let status =
    { url = "http://localhost"
    ; healthy = true
    ; models = []
    ; props =
        Some { total_slots = 2; ctx_size = 65536; model = "m"; supports_tools = None }
    ; slots = None
    ; capabilities = caps
    }
  in
  max_context_of_status status = Some 65536
;;

(* --- scan port parsing --- *)

let%test "builtin_scan_ports includes Ollama 11434" = List.mem 11434 builtin_scan_ports

let%test "parse_ports_env handles comma-separated list" =
  parse_ports_env "9000,9001,9002" = [ 9000; 9001; 9002 ]
;;

let%test "parse_ports_env trims whitespace" =
  parse_ports_env " 9000 , 9001 ,9002 " = [ 9000; 9001; 9002 ]
;;

let%test "parse_ports_env skips empty tokens" =
  parse_ports_env "9000,,9001," = [ 9000; 9001 ]
;;

let%test "parse_ports_env reports non-numeric tokens" =
  let ports, invalids = parse_ports_env_result "9000,abc,9001" in
  ports = [ 9000; 9001 ] && invalids = [ { token = "abc"; reason = Not_an_integer } ]
;;

let%test "parse_ports_env reports invalid TCP ports" =
  let ports, invalids = parse_ports_env_result "-1,0,1,65535,65536" in
  ports = [ 1; 65535 ]
  && invalids
     = [ { token = "-1"; reason = Outside_tcp_port_range }
       ; { token = "0"; reason = Outside_tcp_port_range }
       ; { token = "65536"; reason = Outside_tcp_port_range }
       ]
;;

let%test "parse_ports_env deduplicates while preserving order" =
  parse_ports_env "9000,9001,9000,9002,9001" = [ 9000; 9001; 9002 ]
;;

(* --- find_context_length --- *)

let%test "find_context_length with general.context_length" =
  let mi = `Assoc [ "general.context_length", `Int 131072 ] in
  find_context_length mi = 131072
;;

let%test "find_context_length with model-specific prefix" =
  let mi =
    `Assoc
      [ "qwen3_5.embedding_length", `Int 3584; "qwen3_5.context_length", `Int 262144 ]
  in
  find_context_length mi = 262144
;;

let%test "find_context_length prefers context_length over general" =
  let mi = `Assoc [ "general.context_length", `Int 8192; "context_length", `Int 4096 ] in
  find_context_length mi = 4096
;;

let%test "find_context_length prefers general.context_length over model-specific" =
  let mi =
    `Assoc [ "qwen3_5.context_length", `Int 262144; "general.context_length", `Int 8192 ]
  in
  find_context_length mi = 8192
;;

let%test "find_context_length takes max of model-specific keys" =
  let mi =
    `Assoc [ "nous.context_length", `Int 8192; "qwen3_5.context_length", `Int 262144 ]
  in
  find_context_length mi = 262144
;;

let%test "find_context_length with float value" =
  let mi = `Assoc [ "nous.context_length", `Float 131072.0 ] in
  find_context_length mi = 131072
;;

let%test "find_context_length returns 0 when no matching key" =
  let mi = `Assoc [ "qwen3_5.embedding_length", `Int 3584 ] in
  find_context_length mi = 0
;;

let%test "find_context_length returns 0 for non-assoc" = find_context_length `Null = 0

let%test "find_context_length bare key without prefix" =
  let mi = `Assoc [ "context_length", `Int 4096 ] in
  find_context_length mi = 4096
;;

(* --- probe_ollama_context parser (unit, no network) --- *)

let%test "infer_capabilities uses ollama context when props present" =
  let models = [ { id = "dashscope-3.5-35b"; owned_by = "ollama" } ] in
  let props =
    Some
      { total_slots = 1
      ; ctx_size = 8192
      ; model = "dashscope-3.5:latest"
      ; supports_tools = None
      }
  in
  let caps = infer_capabilities ~uses_ollama_think:true models props in
  caps.max_context_tokens = Some 8192
;;

let%test "infer_capabilities defaults to 262K when no props for dashscope" =
  let models = [ { id = "dashscope-3.5-35b"; owned_by = "ollama" } ] in
  let caps = infer_capabilities ~uses_ollama_think:true models None in
  caps.max_context_tokens = Some 262_144
;;

(* --- infer_capabilities Ollama native thinking behavior class --- *)

let%test "infer_capabilities ollama unknown model gets ollama_think format" =
  let models = [ { id = "llama3.3"; owned_by = "ollama" } ] in
  let caps = infer_capabilities ~uses_ollama_think:true models None in
  caps.thinking_control_format = Capabilities.Ollama_think
;;

let%test "infer_capabilities ollama unknown model gets seed and conservative tool_choice" =
  let models = [ { id = "phi4"; owned_by = "ollama" } ] in
  let caps = infer_capabilities ~uses_ollama_think:true models None in
  caps.thinking_control_format = Capabilities.Ollama_think
  && caps.supports_seed = true
  && caps.supports_tool_choice = false
;;

let%test "infer_capabilities ollama tool support propagated from api_show template" =
  let models = [ { id = "phi4"; owned_by = "ollama" } ] in
  let props =
    Some { total_slots = 1; ctx_size = 65536; model = "phi4"; supports_tools = Some true }
  in
  let caps = infer_capabilities ~uses_ollama_think:true models props in
  caps.thinking_control_format = Capabilities.Ollama_think
  && caps.supports_tools = true
  && caps.max_context_tokens = Some 65536
;;

let%test
    "infer_capabilities ollama known model lookup preserves context and overlays ollama \
     flags"
  =
  (* dashscope-3.5-35b is in for_model_id with 262K context and supports_reasoning.
     On an Ollama native endpoint, the overlay should add seed support +
     thinking_control_format = Ollama_think while preserving the model-specific
     context window and reasoning fields. *)
  let models = [ { id = "dashscope-3.5-35b"; owned_by = "ollama" } ] in
  let caps = infer_capabilities ~uses_ollama_think:true models None in
  caps.thinking_control_format = Capabilities.Ollama_think
  && caps.supports_seed = true
  && caps.max_context_tokens = Some 262_144
  && caps.supports_reasoning = true
  && caps.supports_tool_choice = false
;;

(* --- discovered context state (atomic snapshot) --- *)

let%test "discovered_ctx snapshot: set and read both fields atomically" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       let snap =
         { endpoint_ctxs = [ "http://a:8085", 4096 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 4096
         }
       in
       Atomic.set _discovered_ctx snap;
       discovered_per_slot_context () = Some 4096
       && discovered_endpoint_contexts () = [ "http://a:8085", 4096 ])
;;

let%test "discovered_ctx snapshot: empty endpoints clears per_slot_ctx" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       (* Simulate a probe that previously found endpoints *)
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 8192 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 8192
         };
       (* Simulate a probe with no valid results *)
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = []; model_endpoints = []; per_slot_ctx = None };
       discovered_per_slot_context () = None && discovered_endpoint_contexts () = [])
;;

let%test "discovered_ctx snapshot: max across multiple endpoints" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 4096; "http://b:8086", 8192 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 8192
         };
       discovered_per_slot_context () = Some 8192
       && List.length (discovered_endpoint_contexts ()) = 2)
;;

let%test "discovered_context_for_url returns per-endpoint value" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 4096; "http://b:8086", 8192 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 8192
         };
       discovered_context_for_url "http://a:8085" = Some 4096
       && discovered_context_for_url "http://b:8086" = Some 8192)
;;

let%test "discovered_context_for_url returns None for unknown endpoint" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 4096 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 4096
         };
       discovered_context_for_url "http://unknown:9999" = None)
;;

let%test "discovered_context_for_url trims whitespace" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 4096 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 4096
         };
       discovered_context_for_url "  http://a:8085  " = Some 4096)
;;

let%test "discovered_ctx initial state is empty" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = []; model_endpoints = []; per_slot_ctx = None };
       discovered_per_slot_context () = None
       && discovered_endpoint_contexts () = []
       && discovered_context_for_url "http://any:8085" = None)
;;

(* --- model-to-endpoint index --- *)

let%test "endpoint_for_model returns url when model is indexed" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 32768 ]
         ; model_endpoints = [ "dashscope-3.5-9b", "http://a:8085" ]
         ; per_slot_ctx = Some 32768
         };
       endpoint_for_model "dashscope-3.5-9b" = Some "http://a:8085")
;;

let%test "endpoint_for_model returns None for unknown model" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 32768 ]
         ; model_endpoints = [ "dashscope-3.5-9b", "http://a:8085" ]
         ; per_slot_ctx = Some 32768
         };
       endpoint_for_model "nonexistent" = None)
;;

let%test "context_for_model returns url and per-slot ctx" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 32768; "http://b:8086", 8192 ]
         ; model_endpoints = [ "model-a", "http://a:8085"; "model-b", "http://b:8086" ]
         ; per_slot_ctx = Some 32768
         };
       context_for_model "model-a" = Some ("http://a:8085", 32768)
       && context_for_model "model-b" = Some ("http://b:8086", 8192))
;;

let%test "context_for_model returns None when model not indexed" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://a:8085", 32768 ]
         ; model_endpoints = []
         ; per_slot_ctx = Some 32768
         };
       context_for_model "any-model" = None)
;;

let%test "first_discovered_model_id returns None for empty snapshot" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = []; model_endpoints = []; per_slot_ctx = None };
       first_discovered_model_id () = None)
;;

let%test "first_discovered_model_id returns first model_id from snapshot" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = [ "http://localhost:8085", 8192 ]
         ; model_endpoints =
             [ "model-a", "http://localhost:8085"; "model-b", "http://localhost:8086" ]
         ; per_slot_ctx = Some 8192
         };
       first_discovered_model_id () = Some "model-a")
;;

let%test "first_discovered_model_id_for_url filters by endpoint" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = []
         ; model_endpoints =
             [ "llama-model", "http://127.0.0.1:8085"
             ; "ollama-model", "http://127.0.0.1:11434"
             ]
         ; per_slot_ctx = None
         };
       first_discovered_model_id_for_url "http://127.0.0.1:11434" = Some "ollama-model")
;;

let%test "first_discovered_model_id_for_url returns None for unknown url" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = []
         ; model_endpoints = [ "llama-model", "http://127.0.0.1:8085" ]
         ; per_slot_ctx = None
         };
       first_discovered_model_id_for_url "http://127.0.0.1:11434" = None)
;;

let%test "first_discovered_model_id_for_url prevents cross-provider" =
  let old = Atomic.get _discovered_ctx in
  Fun.protect
    ~finally:(fun () -> Atomic.set _discovered_ctx old)
    (fun () ->
       Atomic.set
         _discovered_ctx
         { endpoint_ctxs = []
         ; model_endpoints =
             [ "dashscope-3.5-9b-local", "http://127.0.0.1:8085"
             ; "dashscope-3.5:9b-nvfp4", "http://127.0.0.1:11434"
             ]
         ; per_slot_ctx = None
         };
       (* ollama endpoint must NOT return the llama-server model *)
       first_discovered_model_id_for_url "http://127.0.0.1:8085"
       = Some "dashscope-3.5-9b-local"
       && first_discovered_model_id_for_url "http://127.0.0.1:11434"
          = Some "dashscope-3.5:9b-nvfp4")
;;

(* --- template_has_tool_support tests --- *)

let%test "template_has_tool_support detects tools keyword" =
  template_has_tool_support "{{ .Tools }}"
;;

let%test "template_has_tool_support detects Tool keyword" =
  template_has_tool_support "{% for tool in Tools %}{{ tool }}{% endfor %}"
;;

let%test "template_has_tool_support detects <|tool_call|> token" =
  template_has_tool_support "<|im_start|>assistant\n<|tool_call|>\n"
;;

let%test "template_has_tool_support detects <|tool_calls|> token" =
  template_has_tool_support "<|im_start|>assistant<|tool_calls|>["
;;

let%test "template_has_tool_support detects .tool_call token" =
  template_has_tool_support "{% if .tool_call %}{{ .tool_call }}{% endif %}"
;;

let%test "template_has_tool_support detects <|im_tool|> token" =
  template_has_tool_support "<|im_start|><|im_tool|>result"
;;

let%test "template_has_tool_support detects <|function_call token" =
  template_has_tool_support "<|im_start|>assistant\n<|function_call|>{"
;;

let%test "template_has_tool_support rejects template without braces" =
  not (template_has_tool_support "no template markers here")
;;

let%test "template_has_tool_support rejects empty string" =
  not (template_has_tool_support "")
;;

let%test "template_has_tool_support rejects braces without tool signals" =
  not (template_has_tool_support "{{ .Content }}")
;;
