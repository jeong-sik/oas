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

let default_endpoint = "http://127.0.0.1:8085"

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
  let healthy = get_ok ~sw ~net (base ^ "/health") in
  if not healthy then
    { url = base; healthy = false;
      models = []; props = None; slots = None;
      capabilities = Capabilities.default_capabilities }
  else
    let models =
      match get_json ~sw ~net (base ^ "/v1/models") with
      | Ok json -> parse_models json
      | Error _ -> []
    in
    let props =
      match get_json ~sw ~net (base ^ "/props") with
      | Ok json -> parse_props json
      | Error _ -> None
    in
    let slots =
      match get_json ~sw ~net (base ^ "/slots") with
      | Ok json -> parse_slots json
      | Error _ -> None
    in
    let capabilities = infer_capabilities models props in
    { url = base; healthy; models; props; slots; capabilities }

let discover ~sw ~net ~endpoints =
  List.map (fun url -> probe_endpoint ~sw ~net url) endpoints

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
