(** Extensible provider registry with capability-aware queries.

    @since 0.69.0 *)

type provider_defaults = {
  kind: Provider_config.provider_kind;
  base_url: string;
  api_key_env: string;
  request_path: string;
}

type entry = {
  name: string;
  defaults: provider_defaults;
  max_context: int;
  capabilities: Capabilities.capabilities;
  is_available: unit -> bool;
}

type t = {
  entries: (string, entry) Hashtbl.t;
}

let create () = { entries = Hashtbl.create 8 }

let register t entry =
  Hashtbl.replace t.entries entry.name entry

let unregister t name =
  Hashtbl.remove t.entries name

let find t name =
  Hashtbl.find_opt t.entries name

let all t =
  Hashtbl.fold (fun _k v acc -> v :: acc) t.entries []

let available t =
  all t |> List.filter (fun e -> e.is_available ())

let find_capable t pred =
  all t |> List.filter (fun e -> pred e.capabilities)

(* ── Default registry ─────────────────────────────────── *)

let has_api_key env_name =
  env_name = "" ||
  (match Sys.getenv_opt env_name with
   | Some s -> String.trim s <> ""
   | None -> false)

let path_entries ?path () =
  match path with
  | Some value -> String.split_on_char ':' value
  | None ->
      (match Sys.getenv_opt "PATH" with
       | Some value -> String.split_on_char ':' value
       | None -> [])
  |> List.filter (fun entry -> String.trim entry <> "")

let command_candidates ~name =
  if Filename.check_suffix name ".exe" then
    [ name ]
  else
    [ name; name ^ ".exe" ]

let is_runnable_path path =
  Sys.file_exists path && not (Sys.is_directory path)

let command_in_path ?path name =
  path_entries ?path ()
  |> List.exists (fun dir ->
         command_candidates ~name
         |> List.exists (fun candidate ->
                is_runnable_path (Filename.concat dir candidate)))

(** Initial endpoints from LLM_ENDPOINTS env var. *)
let initial_llama_endpoints =
  match Sys.getenv_opt "LLM_ENDPOINTS" with
  | Some s ->
    let urls = s |> String.split_on_char ',' |> List.map String.trim
               |> List.filter (fun s -> s <> "") in
    if urls = [] then [Discovery.default_endpoint] else urls
  | None -> [Discovery.default_endpoint]

(** Mutable endpoint list, protected by atomic snapshot swap.
    Updated by [refresh_llama_endpoints]. *)
let llama_endpoints_ref = Atomic.make (Array.of_list initial_llama_endpoints)

let llama_all_endpoints = initial_llama_endpoints

(** Round-robin counter for distributing calls across endpoints. *)
let llama_rr_counter = Atomic.make 0

(** Pick the next llama endpoint via round-robin.
    Reads the current endpoint snapshot atomically.
    Called by cascade_config when resolving "llama:*" provider. *)
let next_llama_endpoint () =
  let endpoints = Atomic.get llama_endpoints_ref in
  let n = Array.length endpoints in
  let idx = Atomic.fetch_and_add llama_rr_counter 1 mod n in
  endpoints.(idx)

(** Peek at the current llama endpoint without advancing the round-robin.
    Used by context resolution to match the endpoint that will serve
    the next request, without side effects. *)
let current_llama_endpoint () =
  let endpoints = Atomic.get llama_endpoints_ref in
  let n = Array.length endpoints in
  if n = 0 then ""
  else
    let idx = Atomic.get llama_rr_counter mod n in
    endpoints.(idx)

(** Refresh the llama endpoint list by scanning local ports.
    If [LLM_ENDPOINTS] is set, uses that as the source (no scan).
    Otherwise probes ports 8085-8090 and keeps only healthy endpoints.
    Falls back to default 8085 if no healthy endpoints found.
    Call this after Eio scheduler is available (e.g. at server startup). *)
let refresh_llama_endpoints ~sw ~net () =
  let endpoint_urls =
    match Sys.getenv_opt "LLM_ENDPOINTS" with
    | Some s when String.trim s <> "" ->
        let urls = s |> String.split_on_char ',' |> List.map String.trim
                   |> List.filter (fun s -> s <> "") in
        if urls = [] then [Discovery.default_endpoint] else urls
    | _ ->
        (* scan_local_endpoints only returns healthy URLs; we need full statuses
           for context sync, so probe the default + scanned ports. *)
        let candidates =
          List.map (fun p -> Printf.sprintf "http://127.0.0.1:%d" p)
            Discovery.default_scan_ports
        in
        let statuses = Discovery.refresh_and_sync ~sw ~net ~endpoints:candidates in
        let found = List.filter_map (fun (s : Discovery.endpoint_status) ->
          if s.healthy then Some s.url else None
        ) statuses in
        if found = [] then [Discovery.default_endpoint] else found
  in
  (* When LLM_ENDPOINTS is explicit, still probe for context sync *)
  (match Sys.getenv_opt "LLM_ENDPOINTS" with
   | Some s when String.trim s <> "" ->
     ignore (Discovery.refresh_and_sync ~sw ~net ~endpoints:endpoint_urls)
   | _ -> () (* already synced above *));
  Atomic.set llama_endpoints_ref (Array.of_list endpoint_urls);
  endpoint_urls

(** Current active endpoint list (snapshot). *)
let active_llama_endpoints () =
  Array.to_list (Atomic.get llama_endpoints_ref)

let discovered_max_context () =
  Discovery.discovered_per_slot_context ()

let discovered_endpoint_max_context (url : string) =
  Discovery.discovered_context_for_url url

let llama_defaults = {
  kind = OpenAI_compat;
  base_url = List.hd llama_all_endpoints;
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
  kind = Gemini;
  base_url =
    (match Sys.getenv_opt "GEMINI_BASE_URL" with
     | Some url -> url
     | None -> "https://generativelanguage.googleapis.com/v1beta");
  api_key_env = "GEMINI_API_KEY";
  request_path = "";
}

let glm_defaults = {
  kind = Glm;
  base_url =
    (match Sys.getenv_opt "ZAI_BASE_URL" with
     | Some url -> url
     | None -> "https://api.z.ai/api/coding/paas/v4");
  api_key_env = "ZAI_API_KEY";
  request_path = "/chat/completions";
}

let ollama_defaults = {
  kind = Ollama;
  base_url =
    (match Sys.getenv_opt "OLLAMA_HOST" with
     | Some url when String.trim url <> "" -> String.trim url
     | _ -> "http://127.0.0.1:11434");
  api_key_env = "";
  request_path = "/v1/chat/completions";
}

let openrouter_defaults = {
  kind = OpenAI_compat;
  base_url = "https://openrouter.ai/api/v1";
  api_key_env = "OPENROUTER_API_KEY";
  request_path = "/chat/completions";
}

let default () =
  let t = create () in
  let reg name defaults ~max_context caps =
    register t { name; defaults; max_context; capabilities = caps;
                 is_available = (fun () -> has_api_key defaults.api_key_env) }
  in
  reg "llama" llama_defaults ~max_context:128_000
    Capabilities.openai_chat_extended_capabilities;
  reg "claude" claude_defaults ~max_context:200_000
    Capabilities.anthropic_capabilities;
  reg "gemini" gemini_defaults ~max_context:1_000_000
    Capabilities.gemini_capabilities;
  reg "glm" glm_defaults ~max_context:200_000
    Capabilities.glm_capabilities;
  reg "openrouter" openrouter_defaults ~max_context:128_000
    Capabilities.openai_chat_extended_capabilities;
  register t { name = "ollama"; defaults = ollama_defaults;
               max_context = 262_144;
               capabilities = Capabilities.ollama_capabilities;
               is_available = (fun () -> true) };
  (* Claude Code subprocess — always available if claude is in PATH *)
  let cc_defaults = {
    kind = Claude_code;
    base_url = "";
    api_key_env = "";
    request_path = "";
  } in
  let cc_available =
    let cached = command_in_path "claude" in
    fun () -> cached
  in
  register t { name = "cc"; defaults = cc_defaults; max_context = 200_000;
               capabilities = Capabilities.claude_code_capabilities;
               is_available = cc_available };
  t
