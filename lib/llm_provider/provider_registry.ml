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

(** All LLM_ENDPOINTS URLs, parsed once at module init. *)
let llama_all_endpoints =
  match Sys.getenv_opt "LLM_ENDPOINTS" with
  | Some s ->
    let urls = s |> String.split_on_char ',' |> List.map String.trim
               |> List.filter (fun s -> s <> "") in
    if urls = [] then ["http://127.0.0.1:8085"] else urls
  | None -> ["http://127.0.0.1:8085"]

(** Round-robin counter for distributing calls across LLM_ENDPOINTS. *)
let llama_rr_counter = Atomic.make 0

(** Pick the next llama endpoint via round-robin.
    Called by cascade_config when resolving "llama:*" provider. *)
let next_llama_endpoint () =
  let endpoints = Array.of_list llama_all_endpoints in
  let n = Array.length endpoints in
  let idx = Atomic.fetch_and_add llama_rr_counter 1 mod n in
  endpoints.(idx)

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
  (* Claude Code subprocess — always available if claude is in PATH *)
  let cc_defaults = {
    kind = Claude_code;
    base_url = "";
    api_key_env = "";
    request_path = "";
  } in
  let cc_available =
    let cached = lazy (
      try
        let ic = Unix.open_process_in "which claude 2>/dev/null" in
        let result = try input_line ic with End_of_file -> "" in
        ignore (Unix.close_process_in ic);
        String.length (String.trim result) > 0
      with Unix.Unix_error _ | End_of_file -> false
    ) in
    fun () -> Lazy.force cached
  in
  register t { name = "cc"; defaults = cc_defaults; max_context = 200_000;
               capabilities = Capabilities.claude_code_capabilities;
               is_available = cc_available };
  t
