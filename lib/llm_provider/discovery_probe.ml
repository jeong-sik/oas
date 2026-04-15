(** LLM endpoint probing -- HTTP helpers, parsers, Ollama fallback,
    capability inference, URL utilities, and probe_endpoint.

    Extracted from {!Discovery} to reduce god-file size.
    @since 0.53.0 *)

(* ── HTTP helpers ────────────────────────────────────────── *)

let get_json ~sw ~net url =
  match Http_client.get_sync ~sw ~net ~url ~headers:[] with
  | Ok (code, body) when code >= 200 && code < 300 ->
    (try Ok (Yojson.Safe.from_string body)
     with Yojson.Json_error msg -> Error msg)
  | Ok (code, _) -> Error (Printf.sprintf "HTTP %d" code)
  | Error (Http_client.HttpError { code; _ }) ->
    Error (Printf.sprintf "HTTP %d" code)
  | Error (Http_client.AcceptRejected { reason }) ->
    Error reason
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
        Some { Discovery.model_info.id; owned_by }
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
    Some { Discovery.server_props.total_slots; ctx_size; model }
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
    Some { Discovery.slot_status.total; busy; idle = total - busy }

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
      List.find_map (fun (k, value) ->
        if k = key then int_value value else None
      ) pairs
    in
    (match preferred "context_length" with
     | Some n -> n
     | None ->
       match preferred "general.context_length" with
       | Some n -> n
       | None ->
         pairs
         |> List.filter_map (fun (key, value) ->
              if String.ends_with ~suffix:".context_length" key
              then int_value value
              else None)
         |> List.fold_left max 0)
  | _ -> 0

(** Try to detect Ollama via /api/tags and retrieve actual context size
    via /api/show. Returns synthetic server_props on success. *)
let probe_ollama_context ~sw ~net base_url =
  match get_json ~sw ~net (base_url ^ "/api/tags") with
  | Error detail ->
    Discovery.warn_probe_failure ~url:base_url ~phase:"ollama_tags" detail;
    None
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
           let ctx = find_context_length model_info in
           if ctx > 0 then
             Some { Discovery.server_props.total_slots = 1; ctx_size = ctx; model = model_name }
           else (
             Discovery.warn_probe_failure ~url:base_url ~phase:"ollama_show"
               "model_info contained no usable context length";
             None)
         with
         | Yojson.Json_error msg ->
             Discovery.warn_probe_failure ~url:base_url ~phase:"ollama_show_json" msg;
             None
         | Yojson.Safe.Util.Type_error (msg, _) ->
             Discovery.warn_probe_failure ~url:base_url ~phase:"ollama_show_parse" msg;
             None)
      | Ok (code, _) ->
          Discovery.warn_probe_failure ~url:base_url ~phase:"ollama_show_http"
            (Printf.sprintf "HTTP %d" code);
          None
      | Error err ->
          let detail =
            match err with
            | Http_client.HttpError { code; body } ->
                Printf.sprintf "HTTP %d: %s" code body
            | Http_client.NetworkError { message } -> message
            | Http_client.AcceptRejected { reason } -> reason
          in
          Discovery.warn_probe_failure ~url:base_url ~phase:"ollama_show_http" detail;
          None

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
  let from_lookup = List.find_map (fun (m : Discovery.model_info) ->
    Capabilities.for_model_id m.id
  ) models in
  let base = match from_lookup with
    | Some caps -> caps
    | None ->
      (* 2. Generic inference by model name *)
      let needs_extended =
        List.exists (fun (m : Discovery.model_info) ->
          string_contains_ci ~haystack:m.id ~needle:"qwen") models
      in
      if needs_extended then Capabilities.openai_chat_extended_capabilities
      else Capabilities.openai_chat_capabilities
  in
  (* 3. Merge ctx_size from /props into capabilities *)
  match props with
  | None -> base
  | Some p ->
    if p.ctx_size > 0 then
      { base with Capabilities.max_context_tokens = Some p.ctx_size }
    else base

(* ── URL utilities ───────────────────────────────────────── *)

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
    | Some i when i + 2 < len && s.[i+1] = '/' && s.[i+2] = '/' -> i + 3
    | _ -> 0
  in
  (* Authority ends at first '/', '?' or '#'. *)
  let stop =
    let rec scan i =
      if i >= len then len
      else match s.[i] with
        | '/' | '?' | '#' -> i
        | _ -> scan (i + 1)
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
    if String.length host_port > 0 && host_port.[0] = '[' then
      match String.index_opt host_port ']' with
      | Some i when i + 1 < String.length host_port && host_port.[i+1] = ':' ->
          Some (String.sub host_port (i + 2)
                  (String.length host_port - i - 2))
      | _ -> None
    else
      match String.rindex_opt host_port ':' with
      | Some i -> Some (String.sub host_port (i + 1)
                          (String.length host_port - i - 1))
      | None -> None
  in
  Option.bind port_str int_of_string_opt

(** Heuristic: does this URL look like an Ollama endpoint?
    Used to skip llama.cpp-only probe paths (/props, /slots) that always
    return 404 on Ollama and pollute logs. Matches when:
    - the URL's authority port is exactly 11434 (Ollama default), OR
    - the trimmed URL equals the configured [Discovery.ollama_endpoint]
      (env [OLLAMA_HOST] or default)
    The port match is intentionally strict — substring matching on
    ":11434" gives false positives on userinfo (user:11434@host),
    paths (/api/:11434), and query strings (?p=:11434). *)
let url_is_ollama (url : string) : bool =
  match port_of_url url with
  | Some 11434 -> true
  | _ -> String.equal (String.trim url) (String.trim Discovery.ollama_endpoint)

(* ── Endpoint probing ────────────────────────────────────── *)

let probe_endpoint ~sw ~net url =
  let base = String.trim url in
  let warn phase detail =
    Discovery.warn_probe_failure ~url:base ~phase detail
  in
  let capture_json phase parse target =
    match get_json ~sw ~net target with
    | Ok json -> parse json
    | Error detail ->
        warn phase detail;
        None
  in
  (* Try /health first (llama.cpp), fall back to / (Ollama returns 200) *)
  let healthy =
    get_ok ~sw ~net (base ^ "/health")
    || get_ok ~sw ~net base
  in
  if not healthy then
    { Discovery.endpoint_status.url = base; healthy = false;
      models = []; props = None; slots = None;
      capabilities = Capabilities.default_capabilities }
  else
    let is_ollama = url_is_ollama base in
    (* Fetch models, props, and slots concurrently via Eio fibers.
       Skip /props and /slots probes for Ollama endpoints — those are
       llama.cpp-only paths and always 404 on Ollama, polluting logs.
       Ollama context is recovered via probe_ollama_context below. *)
    let models_ref = ref [] in
    let props_ref = ref None in
    let slots_ref = ref None in
    let fibers =
      [
        (fun () ->
          models_ref :=
            (match get_json ~sw ~net (base ^ "/v1/models") with
             | Ok json -> parse_models json
             | Error detail ->
                 warn "models" detail;
                 []));
        (fun () ->
          props_ref :=
            capture_json "props" parse_props (base ^ "/props"));
        (fun () ->
          slots_ref :=
            capture_json "slots" parse_slots (base ^ "/slots"));
      ]
      @ (if is_ollama then []
         else [
           (fun () ->
             props_ref := capture_json "props" parse_props (base ^ "/props"));
           (fun () ->
             slots_ref := capture_json "slots" parse_slots (base ^ "/slots"));
         ])
    in
    Eio.Fiber.all fibers;
    let models = !models_ref in
    let slots = !slots_ref in
    (* Ollama fallback: if /props failed (or was skipped), try /api/tags + /api/show *)
    let props = match !props_ref with
      | Some _ as p -> p
      | None -> probe_ollama_context ~sw ~net base
    in
    let capabilities = infer_capabilities models props in
    { Discovery.endpoint_status.url = base; healthy;
      models; props; slots; capabilities }