(** LLM endpoint discovery -- probes local llama-server instances.

    @since 0.53.0 *)

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

let default_endpoint =
  let primary = Sys.getenv_opt "OAS_LOCAL_LLM_URL" in
  let legacy = Sys.getenv_opt "OAS_LOCAL_QWEN_URL" in
  match primary, legacy with
  | Some v, _ when String.trim v <> "" -> String.trim v
  | _, Some v when String.trim v <> "" -> String.trim v
  | _ -> "http://127.0.0.1:8085"

let endpoints_from_env () =
  match Sys.getenv_opt "LLM_ENDPOINTS" with
  | None | Some "" -> [default_endpoint]
  | Some value ->
    value
    |> String.split_on_char ','
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")

(* ── HTTP helpers ────────────────────────────────────────── *)

let get_json ~sw ~net url =
  match Http_client.get_sync ~sw ~net ~url ~headers:[] with
  | Ok (code, body) when code >= 200 && code < 300 ->
    (try Ok (Yojson.Safe.from_string body)
     with Yojson.Json_error msg -> Error msg)
  | Ok (code, _) -> Error (Printf.sprintf "HTTP %d" code)
  | Error (Http_client.HttpError { code; _ }) ->
    Error (Printf.sprintf "HTTP %d" code)
  | Error (Http_client.NetworkError { message }) ->
    Error message

let get_ok ~sw ~net url =
  match Http_client.get_sync ~sw ~net ~url ~headers:[] with
  | Ok (code, _) when code >= 200 && code < 300 -> true
  | _ -> false

(* ── Parsers ─────────────────────────────────────────────── *)

let parse_models json =
  let open Yojson.Safe.Util in
  match member "data" json with
  | `List items ->
    items |> List.filter_map (fun item ->
      match item |> member "id" |> to_string_option with
      | Some id ->
        let owned_by =
          item |> member "owned_by" |> to_string_option
          |> Option.value ~default:"unknown"
        in
        Some { id; owned_by }
      | None -> None)
  | _ -> []

let parse_props json =
  let open Yojson.Safe.Util in
  match member "total_slots" json with
  | `Int total_slots ->
    let dgs = member "default_generation_settings" json in
    let ctx_size = match dgs with
      | `Assoc _ ->
        (match member "n_ctx" dgs with
         | `Int n -> n
         | _ -> 0)
      | _ -> 0
    in
    let model = match dgs with
      | `Assoc _ ->
        (match member "model" dgs with
         | `String s -> s
         | _ -> "")
      | _ -> ""
    in
    Some { total_slots; ctx_size; model }
  | _ -> None

let parse_slots json =
  let open Yojson.Safe.Util in
  let items = match json with
    | `List items -> items
    | _ -> []
  in
  if items = [] then None
  else
    let total = List.length items in
    let busy = items |> List.fold_left (fun acc slot ->
      let is_busy =
        (slot |> member "is_processing" |> to_bool_option
         |> Option.value ~default:false)
        || (match slot |> member "state" with
            | `Int n -> n <> 0
            | _ -> false)
      in
      if is_busy then acc + 1 else acc
    ) 0 in
    Some { total; busy; idle = total - busy }

(* ── Ollama fallback ────────────────────────────────────── *)

(** Try to detect Ollama via /api/tags and retrieve actual context size
    via /api/show. Returns synthetic server_props on success. *)
let probe_ollama_context ~sw ~net base_url =
  match get_json ~sw ~net (base_url ^ "/api/tags") with
  | Error _ -> None
  | Ok tags_json ->
    let open Yojson.Safe.Util in
    let models = match member "models" tags_json with
      | `List items -> items
      | _ -> []
    in
    let first_name = List.find_map (fun item ->
      match member "name" item |> to_string_option with
      | Some name when name <> "" -> Some name
      | _ -> None
    ) models in
    match first_name with
    | None -> None
    | Some model_name ->
      let body = Yojson.Safe.to_string (`Assoc [("name", `String model_name)]) in
      let headers = [("content-type", "application/json")] in
      match Http_client.post_sync ~sw ~net
              ~url:(base_url ^ "/api/show") ~headers ~body with
      | Ok (code, resp_body) when code >= 200 && code < 300 ->
        (try
           let json = Yojson.Safe.from_string resp_body in
           let model_info = member "model_info" json in
           let ctx = match member "general.context_length" model_info with
             | `Int n -> n
             | `Float f -> int_of_float f
             | _ -> 0
           in
           if ctx > 0 then
             Some { total_slots = 1; ctx_size = ctx; model = model_name }
           else None
         with _ -> None)
      | _ -> None

(* ── Capability inference ────────────────────────────────── *)

let string_contains_ci ~haystack ~needle =
  let h = String.lowercase_ascii haystack in
  let n = String.lowercase_ascii needle in
  let nlen = String.length n in
  let hlen = String.length h in
  if nlen > hlen then false
  else
    let found = ref false in
    let i = ref 0 in
    while !i <= hlen - nlen && not !found do
      if String.sub h !i nlen = n then found := true;
      incr i
    done;
    !found

(** Infer capabilities from model info and server props.
    Priority: model-specific lookup > generic inference > default. *)
let infer_capabilities models props =
  (* 1. Try model-specific lookup *)
  let from_lookup = List.find_map (fun (m : model_info) ->
    Capabilities.for_model_id m.id
  ) models in
  let base = match from_lookup with
    | Some caps -> caps
    | None ->
      (* 2. Generic inference by model name *)
      let needs_extended =
        List.exists (fun (m : model_info) ->
          string_contains_ci ~haystack:m.id ~needle:"qwen") models
      in
      if needs_extended then Capabilities.openai_chat_extended_capabilities
      else Capabilities.openai_chat_capabilities
  in
  (* 3. Merge ctx_size from /props into capabilities *)
  match props with
  | Some (p : server_props) ->
    Capabilities.with_context_size base ~ctx_size:p.ctx_size
  | None -> base

(* ── Probe ───────────────────────────────────────────────── *)

let probe_endpoint ~sw ~net url =
  let base = String.trim url in
  (* Try /health first (llama.cpp), fall back to / (Ollama returns 200) *)
  let healthy =
    get_ok ~sw ~net (base ^ "/health")
    || get_ok ~sw ~net base
  in
  if not healthy then
    { url = base; healthy = false;
      models = []; props = None; slots = None;
      capabilities = Capabilities.default_capabilities }
  else
    (* Fetch models, props, and slots concurrently via Eio fibers *)
    let models_ref = ref [] in
    let props_ref = ref None in
    let slots_ref = ref None in
    Eio.Fiber.all [
      (fun () ->
        models_ref := match get_json ~sw ~net (base ^ "/v1/models") with
          | Ok json -> parse_models json | Error _ -> []);
      (fun () ->
        props_ref := match get_json ~sw ~net (base ^ "/props") with
          | Ok json -> parse_props json | Error _ -> None);
      (fun () ->
        slots_ref := match get_json ~sw ~net (base ^ "/slots") with
          | Ok json -> parse_slots json | Error _ -> None);
    ];
    let models = !models_ref in
    let slots = !slots_ref in
    (* Ollama fallback: if /props failed, try /api/tags + /api/show *)
    let props = match !props_ref with
      | Some _ as p -> p
      | None -> probe_ollama_context ~sw ~net base
    in
    let capabilities = infer_capabilities models props in
    { url = base; healthy; models; props; slots; capabilities }

let discover ~sw ~net ~endpoints =
  Eio.Fiber.List.map (fun url -> probe_endpoint ~sw ~net url) endpoints

let default_scan_ports = [ 8085; 8086; 8087; 8088; 8089; 8090; 11434 ]

let scan_local_endpoints ?(ports = default_scan_ports) ~sw ~net () =
  let candidates =
    List.map (fun p -> Printf.sprintf "http://127.0.0.1:%d" p) ports
  in
  let statuses = discover ~sw ~net ~endpoints:candidates in
  List.filter_map
    (fun (s : endpoint_status) -> if s.healthy then Some s.url else None)
    statuses

(* ── JSON serialization ──────────────────────────────────── *)

let model_info_to_json (m : model_info) =
  `Assoc [("id", `String m.id); ("owned_by", `String m.owned_by)]

let server_props_to_json (p : server_props) =
  `Assoc [
    ("total_slots", `Int p.total_slots);
    ("ctx_size", `Int p.ctx_size);
    ("model", `String p.model);
  ]

let slot_status_to_json (s : slot_status) =
  `Assoc [
    ("total", `Int s.total);
    ("busy", `Int s.busy);
    ("idle", `Int s.idle);
  ]

let capabilities_to_json (c : Capabilities.capabilities) =
  `Assoc [
    ("reasoning", `Bool c.supports_reasoning);
    ("tools", `Bool c.supports_tools);
    ("streaming", `Bool c.supports_native_streaming);
    ("multimodal", `Bool c.supports_multimodal_inputs);
    ("json_output", `Bool c.supports_response_format_json);
  ]

let endpoint_status_to_json (e : endpoint_status) =
  let fields = [
    ("url", `String e.url);
    ("healthy", `Bool e.healthy);
    ("models", `List (List.map model_info_to_json e.models));
    ("capabilities", capabilities_to_json e.capabilities);
  ] in
  let fields = match e.props with
    | Some p -> fields @ [("props", server_props_to_json p)]
    | None -> fields
  in
  let fields = match e.slots with
    | Some s -> fields @ [("slots", slot_status_to_json s)]
    | None -> fields
  in
  `Assoc fields

let summary_to_json endpoints =
  let total_capacity = List.fold_left (fun acc (e : endpoint_status) ->
    match e.slots with
    | Some s -> acc + s.total
    | None -> acc
  ) 0 endpoints in
  let available_capacity = List.fold_left (fun acc (e : endpoint_status) ->
    match e.slots with
    | Some s -> acc + s.idle
    | None -> acc
  ) 0 endpoints in
  let active_requests = List.fold_left (fun acc (e : endpoint_status) ->
    match e.slots with
    | Some s -> acc + s.busy
    | None -> acc
  ) 0 endpoints in
  `Assoc [
    ("total_capacity", `Int total_capacity);
    ("available_capacity", `Int available_capacity);
    ("active_requests", `Int active_requests);
  ]

let max_context_of_status (status : endpoint_status) =
  match status.props with
  | Some p when p.ctx_size > 0 -> Some p.ctx_size
  | _ -> status.capabilities.max_context_tokens

[@@@coverage off]
(* === Inline tests === *)

(* --- default_endpoint --- *)

let%test "default_endpoint is localhost:8085" =
  default_endpoint = "http://127.0.0.1:8085"

(* --- endpoints_from_env --- *)

let%test "endpoints_from_env default when unset" =
  (match Sys.getenv_opt "LLM_ENDPOINTS" with
   | Some _ -> Unix.putenv "LLM_ENDPOINTS" ""
   | None -> ());
  endpoints_from_env () = [default_endpoint]

let%test "endpoints_from_env parses comma-separated" =
  Unix.putenv "LLM_ENDPOINTS" "http://a:8080,http://b:8081";
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  eps = ["http://a:8080"; "http://b:8081"]

let%test "endpoints_from_env trims whitespace" =
  Unix.putenv "LLM_ENDPOINTS" "  http://a:8080 , http://b:8081  ";
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  eps = ["http://a:8080"; "http://b:8081"]

let%test "endpoints_from_env filters empty parts" =
  Unix.putenv "LLM_ENDPOINTS" "http://a,,http://b,";
  let eps = endpoints_from_env () in
  Unix.putenv "LLM_ENDPOINTS" "";
  eps = ["http://a"; "http://b"]

let%test "endpoints_from_env empty string returns default" =
  Unix.putenv "LLM_ENDPOINTS" "";
  let eps = endpoints_from_env () in
  eps = [default_endpoint]

(* --- parse_models --- *)

let%test "parse_models valid" =
  let json = `Assoc [("data", `List [
    `Assoc [("id", `String "qwen3.5-35b"); ("owned_by", `String "local")];
    `Assoc [("id", `String "llama-4-scout"); ("owned_by", `String "meta")];
  ])] in
  let models = parse_models json in
  List.length models = 2
  && (List.hd models).id = "qwen3.5-35b"

let%test "parse_models empty data" =
  let json = `Assoc [("data", `List [])] in
  parse_models json = []

let%test "parse_models missing data" =
  let json = `Assoc [] in
  parse_models json = []

let%test "parse_models non-list data" =
  let json = `Assoc [("data", `String "bad")] in
  parse_models json = []

let%test "parse_models missing id skipped" =
  let json = `Assoc [("data", `List [
    `Assoc [("owned_by", `String "local")];
  ])] in
  parse_models json = []

let%test "parse_models owned_by defaults to unknown" =
  let json = `Assoc [("data", `List [
    `Assoc [("id", `String "model1")];
  ])] in
  let models = parse_models json in
  List.length models = 1
  && (List.hd models).owned_by = "unknown"

(* --- parse_props --- *)

let%test "parse_props valid" =
  let json = `Assoc [
    ("total_slots", `Int 4);
    ("default_generation_settings", `Assoc [
      ("n_ctx", `Int 8192);
      ("model", `String "qwen3.5");
    ]);
  ] in
  match parse_props json with
  | Some p -> p.total_slots = 4 && p.ctx_size = 8192 && p.model = "qwen3.5"
  | None -> false

let%test "parse_props missing total_slots" =
  let json = `Assoc [] in
  parse_props json = None

let%test "parse_props total_slots not int" =
  let json = `Assoc [("total_slots", `String "4")] in
  parse_props json = None

let%test "parse_props missing dgs" =
  let json = `Assoc [("total_slots", `Int 2)] in
  match parse_props json with
  | Some p -> p.total_slots = 2 && p.ctx_size = 0 && p.model = ""
  | None -> false

let%test "parse_props dgs missing n_ctx" =
  let json = `Assoc [
    ("total_slots", `Int 1);
    ("default_generation_settings", `Assoc [("model", `String "m")]);
  ] in
  match parse_props json with
  | Some p -> p.ctx_size = 0 && p.model = "m"
  | None -> false

let%test "parse_props dgs missing model" =
  let json = `Assoc [
    ("total_slots", `Int 1);
    ("default_generation_settings", `Assoc [("n_ctx", `Int 4096)]);
  ] in
  match parse_props json with
  | Some p -> p.model = "" && p.ctx_size = 4096
  | None -> false

(* --- parse_slots --- *)

let%test "parse_slots valid" =
  let json = `List [
    `Assoc [("is_processing", `Bool true)];
    `Assoc [("is_processing", `Bool false)];
    `Assoc [("is_processing", `Bool false)];
  ] in
  match parse_slots json with
  | Some s -> s.total = 3 && s.busy = 1 && s.idle = 2
  | None -> false

let%test "parse_slots empty list" =
  parse_slots (`List []) = None

let%test "parse_slots non-list" =
  parse_slots (`String "bad") = None

let%test "parse_slots state-based busy detection" =
  let json = `List [
    `Assoc [("state", `Int 1)];
    `Assoc [("state", `Int 0)];
  ] in
  match parse_slots json with
  | Some s -> s.busy = 1 && s.idle = 1
  | None -> false

let%test "parse_slots neither is_processing nor state defaults to idle" =
  let json = `List [`Assoc []] in
  match parse_slots json with
  | Some s -> s.busy = 0 && s.idle = 1
  | None -> false

(* --- string_contains_ci --- *)

let%test "string_contains_ci case insensitive match" =
  string_contains_ci ~haystack:"Qwen3.5-35B" ~needle:"qwen" = true

let%test "string_contains_ci no match" =
  string_contains_ci ~haystack:"llama" ~needle:"qwen" = false

let%test "string_contains_ci needle longer than haystack" =
  string_contains_ci ~haystack:"ab" ~needle:"abcdef" = false

let%test "string_contains_ci empty needle" =
  string_contains_ci ~haystack:"anything" ~needle:"" = true

let%test "string_contains_ci exact match" =
  string_contains_ci ~haystack:"QWEN" ~needle:"qwen" = true

(* --- infer_capabilities --- *)

let%test "infer_capabilities qwen model gets extended" =
  let models = [{ id = "Qwen3.5-35B-A3B"; owned_by = "local" }] in
  let caps = infer_capabilities models None in
  caps.supports_reasoning = true
  && caps.supports_top_k = true
  && caps.supports_min_p = true

let%test "infer_capabilities unknown model gets basic openai" =
  let models = [{ id = "my-custom-model"; owned_by = "local" }] in
  let caps = infer_capabilities models None in
  caps.supports_tools = true
  && caps.supports_reasoning = false

let%test "infer_capabilities known model lookup has priority" =
  let models = [{ id = "claude-opus-4-20260320"; owned_by = "anthropic" }] in
  let caps = infer_capabilities models None in
  caps.supports_caching = true
  && caps.supports_computer_use = true

let%test "infer_capabilities merges ctx_size from props" =
  let models = [{ id = "my-model"; owned_by = "local" }] in
  let props = Some { total_slots = 4; ctx_size = 32768; model = "my-model" } in
  let caps = infer_capabilities models props in
  caps.max_context_tokens = Some 32768

let%test "infer_capabilities no models defaults" =
  let caps = infer_capabilities [] None in
  caps.supports_tools = true

(* --- JSON serialization --- *)

let%test "model_info_to_json" =
  let json = model_info_to_json { id = "m1"; owned_by = "local" } in
  let open Yojson.Safe.Util in
  json |> member "id" |> to_string = "m1"
  && json |> member "owned_by" |> to_string = "local"

let%test "server_props_to_json" =
  let json = server_props_to_json { total_slots = 4; ctx_size = 8192; model = "qwen" } in
  let open Yojson.Safe.Util in
  json |> member "total_slots" |> to_int = 4
  && json |> member "ctx_size" |> to_int = 8192

let%test "slot_status_to_json" =
  let json = slot_status_to_json { total = 4; busy = 1; idle = 3 } in
  let open Yojson.Safe.Util in
  json |> member "total" |> to_int = 4
  && json |> member "busy" |> to_int = 1

let%test "capabilities_to_json" =
  let json = capabilities_to_json Capabilities.default_capabilities in
  let open Yojson.Safe.Util in
  json |> member "tools" |> to_bool = false

let%test "endpoint_status_to_json without props and slots" =
  let es = {
    url = "http://localhost:8085"; healthy = true;
    models = []; props = None; slots = None;
    capabilities = Capabilities.default_capabilities;
  } in
  let json = endpoint_status_to_json es in
  let open Yojson.Safe.Util in
  json |> member "url" |> to_string = "http://localhost:8085"
  && json |> member "healthy" |> to_bool = true

let%test "endpoint_status_to_json with props and slots" =
  let es = {
    url = "http://localhost:8085"; healthy = true;
    models = [{ id = "m1"; owned_by = "local" }];
    props = Some { total_slots = 4; ctx_size = 8192; model = "m1" };
    slots = Some { total = 4; busy = 1; idle = 3 };
    capabilities = Capabilities.default_capabilities;
  } in
  let json = endpoint_status_to_json es in
  let open Yojson.Safe.Util in
  (json |> member "props" |> member "total_slots" |> to_int) = 4
  && (json |> member "slots" |> member "busy" |> to_int) = 1

let%test "summary_to_json empty endpoints" =
  let json = summary_to_json [] in
  let open Yojson.Safe.Util in
  json |> member "total_capacity" |> to_int = 0
  && json |> member "available_capacity" |> to_int = 0
  && json |> member "active_requests" |> to_int = 0

let%test "summary_to_json with slots" =
  let eps = [{
    url = "a"; healthy = true; models = [];
    props = None;
    slots = Some { total = 4; busy = 1; idle = 3 };
    capabilities = Capabilities.default_capabilities;
  }; {
    url = "b"; healthy = true; models = [];
    props = None;
    slots = Some { total = 2; busy = 2; idle = 0 };
    capabilities = Capabilities.default_capabilities;
  }] in
  let json = summary_to_json eps in
  let open Yojson.Safe.Util in
  json |> member "total_capacity" |> to_int = 6
  && json |> member "available_capacity" |> to_int = 3
  && json |> member "active_requests" |> to_int = 3

let%test "summary_to_json endpoint without slots ignored" =
  let eps = [{
    url = "a"; healthy = false; models = [];
    props = None; slots = None;
    capabilities = Capabilities.default_capabilities;
  }] in
  let json = summary_to_json eps in
  let open Yojson.Safe.Util in
  json |> member "total_capacity" |> to_int = 0

(* --- max_context_of_status --- *)

let%test "max_context_of_status from props" =
  let status = {
    url = "http://localhost"; healthy = true; models = [];
    props = Some { total_slots = 4; ctx_size = 32768; model = "m" };
    slots = None; capabilities = Capabilities.default_capabilities;
  } in
  max_context_of_status status = Some 32768

let%test "max_context_of_status from capabilities when no props" =
  let caps = { Capabilities.default_capabilities with max_context_tokens = Some 16384 } in
  let status = {
    url = "http://localhost"; healthy = true; models = [];
    props = None; slots = None; capabilities = caps;
  } in
  max_context_of_status status = Some 16384

let%test "max_context_of_status None when no info" =
  let status = {
    url = "http://localhost"; healthy = true; models = [];
    props = None; slots = None;
    capabilities = Capabilities.default_capabilities;
  } in
  max_context_of_status status = None

let%test "max_context_of_status prefers props over capabilities" =
  let caps = { Capabilities.default_capabilities with max_context_tokens = Some 8192 } in
  let status = {
    url = "http://localhost"; healthy = true; models = [];
    props = Some { total_slots = 2; ctx_size = 65536; model = "m" };
    slots = None; capabilities = caps;
  } in
  max_context_of_status status = Some 65536

(* --- default_scan_ports --- *)

let%test "default_scan_ports includes Ollama 11434" =
  List.mem 11434 default_scan_ports

(* --- probe_ollama_context parser (unit, no network) --- *)

let%test "infer_capabilities uses ollama context when props present" =
  let models = [{ id = "qwen3.5-35b"; owned_by = "ollama" }] in
  let props = Some { total_slots = 1; ctx_size = 8192; model = "qwen3.5:latest" } in
  let caps = infer_capabilities models props in
  caps.max_context_tokens = Some 8192

let%test "infer_capabilities defaults to 262K when no props for qwen" =
  let models = [{ id = "qwen3.5-35b"; owned_by = "ollama" }] in
  let caps = infer_capabilities models None in
  caps.max_context_tokens = Some 262_144
