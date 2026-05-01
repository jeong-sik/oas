(** Extensible provider registry with capability-aware queries.

    @since 0.69.0 *)

type provider_defaults =
  { kind : Provider_config.provider_kind
  ; base_url : string
  ; api_key_env : string
  ; request_path : string
  }

type entry =
  { name : string
  ; defaults : provider_defaults
  ; max_context : int
  ; capabilities : Capabilities.capabilities
  ; is_available : unit -> bool
  }

type t = { entries : (string, entry) Hashtbl.t }

let create () = { entries = Hashtbl.create 8 }
let register t entry = Hashtbl.replace t.entries entry.name entry
let unregister t name = Hashtbl.remove t.entries name
let find t name = Hashtbl.find_opt t.entries name
let all t = Hashtbl.fold (fun _k v acc -> v :: acc) t.entries []
let available t = all t |> List.filter (fun e -> e.is_available ())
let find_capable t pred = all t |> List.filter (fun e -> pred e.capabilities)

(* ── Default registry ─────────────────────────────────── *)

let has_api_key env_name = env_name = "" || Cli_common_env.get env_name <> None
let has_any_api_key env_names = List.exists has_api_key env_names

let path_entries ?path () =
  match path with
  | Some value -> Cli_common_env.split_on_char_trim ':' value
  | None ->
    (match Cli_common_env.get "PATH" with
     | Some value -> Cli_common_env.split_on_char_trim ':' value
     | None -> [])
;;

let command_candidates ~name =
  if Filename.check_suffix name ".exe" then [ name ] else [ name; name ^ ".exe" ]
;;

let is_runnable_path path = Sys.file_exists path && not (Sys.is_directory path)

let command_in_path ?path name =
  path_entries ?path ()
  |> List.exists (fun dir ->
    command_candidates ~name
    |> List.exists (fun candidate -> is_runnable_path (Filename.concat dir candidate)))
;;

(** Initial endpoints from LLM_ENDPOINTS env var.
    Falls back to [[Discovery.default_endpoint]] when the variable is
    unset or has no non-empty entries (SSOT: see
    {!Discovery.parse_llm_endpoints_env}). *)
let initial_llama_endpoints =
  match Discovery.parse_llm_endpoints_env () with
  | [] -> [ Discovery.default_endpoint ]
  | urls -> urls
;;

(** Mutable endpoint list, protected by atomic snapshot swap.
    Updated by [refresh_llama_endpoints]. *)
let llama_endpoints_ref = Atomic.make (Array.of_list initial_llama_endpoints)

let llama_all_endpoints = initial_llama_endpoints

(** Round-robin counter for distributing calls across endpoints. *)
let llama_rr_counter = Atomic.make 0

(** Pick the next llama endpoint via round-robin.
    Reads the current endpoint snapshot atomically.
    Called by downstream callers when resolving "llama:*" provider. *)
let next_llama_endpoint () =
  let endpoints = Atomic.get llama_endpoints_ref in
  let n = Array.length endpoints in
  let idx = Atomic.fetch_and_add llama_rr_counter 1 mod n in
  endpoints.(idx)
;;

(** Peek at the current llama endpoint without advancing the round-robin.
    Used by context resolution to match the endpoint that will serve
    the next request, without side effects. *)
let current_llama_endpoint () =
  let endpoints = Atomic.get llama_endpoints_ref in
  let n = Array.length endpoints in
  if n = 0
  then ""
  else (
    let idx = Atomic.get llama_rr_counter mod n in
    endpoints.(idx))
;;

(** Refresh the llama endpoint list by scanning local ports.
    If [LLM_ENDPOINTS] is set, uses that as the source (no scan).
    Otherwise probes ports 8085-8090 and keeps only healthy endpoints.
    Falls back to default 8085 if no healthy endpoints found.
    Call this after Eio scheduler is available (e.g. at server startup). *)
let refresh_llama_endpoints ~sw ~net () =
  (* SSOT: {!Discovery.parse_llm_endpoints_env} returns [[]] for unset,
     empty, or all-empty env values — collapsing the three historical
     guard patterns into a single list-match. *)
  let explicit = Discovery.parse_llm_endpoints_env () in
  let endpoint_urls =
    match explicit with
    | _ :: _ -> explicit
    | [] ->
      (* scan_local_endpoints only returns healthy URLs; we need full statuses
           for context sync, so probe the default + scanned ports. *)
      let candidates =
        List.map
          (fun p -> Printf.sprintf "http://127.0.0.1:%d" p)
          Discovery.default_scan_ports
      in
      let statuses = Discovery.refresh_and_sync ~sw ~net ~endpoints:candidates in
      let found =
        List.filter_map
          (fun (s : Discovery.endpoint_status) -> if s.healthy then Some s.url else None)
          statuses
      in
      if found = [] then [ Discovery.default_endpoint ] else found
  in
  (* When LLM_ENDPOINTS is explicit, still probe for context sync *)
  (match explicit with
   | _ :: _ -> ignore (Discovery.refresh_and_sync ~sw ~net ~endpoints:endpoint_urls)
   | [] -> () (* already synced above *));
  Atomic.set llama_endpoints_ref (Array.of_list endpoint_urls);
  endpoint_urls
;;

(** Current active endpoint list (snapshot). *)
let active_llama_endpoints () = Array.to_list (Atomic.get llama_endpoints_ref)

let discovered_max_context () = Discovery.discovered_per_slot_context ()

let discovered_endpoint_max_context (url : string) =
  Discovery.discovered_context_for_url url
;;

let llama_defaults =
  { kind = OpenAI_compat
  ; base_url = List.hd llama_all_endpoints
  ; api_key_env = ""
  ; request_path = "/v1/chat/completions"
  }
;;

let claude_defaults =
  { kind = Anthropic
  ; base_url = "https://api.anthropic.com"
  ; api_key_env = "ANTHROPIC_API_KEY"
  ; request_path = "/v1/messages"
  }
;;

let env_or_default env_name default_url =
  match Cli_common_env.get env_name with
  | Some url -> url
  | None -> default_url
;;

let gemini_defaults =
  { kind = Gemini
  ; base_url =
      env_or_default "GEMINI_BASE_URL" "https://generativelanguage.googleapis.com/v1beta"
  ; api_key_env = "GEMINI_API_KEY"
  ; request_path = ""
  }
;;

let glm_defaults =
  { kind = Glm
  ; base_url = env_or_default "ZAI_BASE_URL" Zai_catalog.general_base_url
  ; api_key_env = "ZAI_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let glm_coding_defaults =
  { kind = Glm
  ; base_url = env_or_default "ZAI_CODING_BASE_URL" Zai_catalog.coding_base_url
  ; api_key_env = "ZAI_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let kimi_defaults =
  { kind = Kimi
  ; base_url = env_or_default "KIMI_BASE_URL" "https://api.kimi.com/coding"
  ; api_key_env = "KIMI_API_KEY"
  ; request_path = "/v1/messages"
  }
;;

let ollama_defaults =
  { kind = Ollama
  ; base_url = env_or_default "OLLAMA_HOST" "http://127.0.0.1:11434"
  ; api_key_env = ""
  ; request_path = "/api/chat"
  }
;;

let openrouter_defaults =
  { kind = OpenAI_compat
  ; base_url = "https://openrouter.ai/api/v1"
  ; api_key_env = "OPENROUTER_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let groq_defaults =
  { kind = OpenAI_compat
  ; base_url = env_or_default "GROQ_BASE_URL" "https://api.groq.com/openai/v1"
  ; api_key_env = "GROQ_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let deepseek_defaults =
  { kind = OpenAI_compat
  ; base_url = env_or_default "DEEPSEEK_BASE_URL" "https://api.deepseek.com"
  ; api_key_env = "DEEPSEEK_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let dashscope_defaults =
  { kind = DashScope
  ; base_url =
      env_or_default
        "DASHSCOPE_BASE_URL"
        "https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
  ; api_key_env = "DASHSCOPE_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let siliconflow_defaults =
  { kind = OpenAI_compat
  ; base_url = env_or_default "SILICONFLOW_BASE_URL" "https://api.siliconflow.cn/v1"
  ; api_key_env = "SILICONFLOW_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let normalize_url value =
  let trimmed = String.trim value in
  if trimmed = ""
  then trimmed
  else (
    let rec strip_trailing_slash s =
      let len = String.length s in
      if len > 1 && s.[len - 1] = '/'
      then strip_trailing_slash (String.sub s 0 (len - 1))
      else s
    in
    strip_trailing_slash trimmed)
;;

let default () =
  let t = create () in
  let max_context_from_capabilities ~default caps =
    match caps.Capabilities.max_context_tokens with
    | Some ctx when ctx > default -> ctx
    | _ -> default
  in
  let reg name defaults ~max_context caps =
    let max_context = max_context_from_capabilities ~default:max_context caps in
    register
      t
      { name
      ; defaults
      ; max_context
      ; capabilities = caps
      ; is_available = (fun () -> has_api_key defaults.api_key_env)
      }
  in
  reg
    "llama"
    llama_defaults
    ~max_context:128_000
    Capabilities.openai_chat_extended_capabilities;
  reg "claude" claude_defaults ~max_context:200_000 Capabilities.anthropic_capabilities;
  reg "gemini" gemini_defaults ~max_context:1_000_000 Capabilities.gemini_capabilities;
  reg "glm" glm_defaults ~max_context:200_000 Capabilities.glm_capabilities;
  reg "glm-coding" glm_coding_defaults ~max_context:200_000 Capabilities.glm_capabilities;
  register
    t
    { name = "kimi"
    ; defaults = kimi_defaults
    ; max_context =
        max_context_from_capabilities ~default:262_144 Capabilities.kimi_capabilities
    ; capabilities = Capabilities.kimi_capabilities
    ; is_available = (fun () -> has_any_api_key [ "KIMI_API_KEY" ])
    };
  reg
    "openrouter"
    openrouter_defaults
    ~max_context:128_000
    Capabilities.openai_chat_extended_capabilities;
  reg "groq" groq_defaults ~max_context:131_072 Capabilities.openai_chat_capabilities;
  (* DeepSeek v4 series (flash / pro). 1M context, reasoning, tools. *)
  reg
    "deepseek"
    deepseek_defaults
    ~max_context:1_000_000
    Capabilities.openai_chat_capabilities;
  reg
    "dashscope"
    dashscope_defaults
    ~max_context:131_072
    Capabilities.dashscope_capabilities;
  reg
    "alibaba"
    dashscope_defaults
    ~max_context:131_072
    Capabilities.dashscope_capabilities;
  reg
    "siliconflow"
    siliconflow_defaults
    ~max_context:128_000
    Capabilities.openai_chat_capabilities;
  register
    t
    { name = "ollama"
    ; defaults = ollama_defaults
    ; max_context = 262_144
    ; capabilities = Capabilities.ollama_capabilities
    ; is_available = (fun () -> true)
    };
  (* CLI subprocess providers. Exposed under explicit provider labels so
     caller-managed provider/model specs can opt into the non-interactive transports
     without reusing the direct API names. *)
  let claude_code_defaults =
    { kind = Claude_code; base_url = ""; api_key_env = ""; request_path = "" }
  in
  let claude_code_available =
    let cached = command_in_path "claude" in
    fun () -> cached
  in
  let gemini_cli_defaults =
    { kind = Gemini_cli; base_url = ""; api_key_env = ""; request_path = "" }
  in
  let gemini_cli_available =
    let cached = command_in_path "gemini" in
    fun () -> cached
  in
  let kimi_cli_defaults =
    { kind = Kimi_cli; base_url = ""; api_key_env = ""; request_path = "" }
  in
  let kimi_cli_available =
    let cached = command_in_path "kimi" in
    fun () -> cached
  in
  let codex_cli_defaults =
    { kind = Codex_cli; base_url = ""; api_key_env = ""; request_path = "" }
  in
  let codex_cli_available =
    let cached = command_in_path "codex" in
    fun () -> cached
  in
  register
    t
    { name = "claude_code"
    ; defaults = claude_code_defaults
    ; max_context =
        max_context_from_capabilities
          ~default:200_000
          Capabilities.claude_code_capabilities
    ; capabilities = Capabilities.claude_code_capabilities
    ; is_available = claude_code_available
    };
  register
    t
    { name = "cc"
    ; defaults = claude_code_defaults
    ; max_context =
        max_context_from_capabilities
          ~default:200_000
          Capabilities.claude_code_capabilities
    ; capabilities = Capabilities.claude_code_capabilities
    ; is_available = claude_code_available
    };
  register
    t
    { name = "gemini_cli"
    ; defaults = gemini_cli_defaults
    ; max_context =
        max_context_from_capabilities
          ~default:1_000_000
          Capabilities.gemini_cli_capabilities
    ; capabilities = Capabilities.gemini_cli_capabilities
    ; is_available = gemini_cli_available
    };
  register
    t
    { name = "kimi_cli"
    ; defaults = kimi_cli_defaults
    ; max_context =
        max_context_from_capabilities ~default:262_144 Capabilities.kimi_cli_capabilities
    ; capabilities = Capabilities.kimi_cli_capabilities
    ; is_available = kimi_cli_available
    };
  register
    t
    { name = "codex_cli"
    ; defaults = codex_cli_defaults
    ; max_context =
        max_context_from_capabilities ~default:128_000 Capabilities.codex_cli_capabilities
    ; capabilities = Capabilities.codex_cli_capabilities
    ; is_available = codex_cli_available
    };
  t
;;

let provider_name_of_config (config : Provider_config.t) =
  match config.kind with
  | Anthropic -> "claude"
  | Kimi -> "kimi"
  | Gemini -> "gemini"
  | Glm -> if Zai_catalog.is_coding_base_url config.base_url then "glm-coding" else "glm"
  | Claude_code -> "claude_code"
  | Gemini_cli -> "gemini_cli"
  | Kimi_cli -> "kimi_cli"
  | Codex_cli -> "codex_cli"
  | Ollama -> "ollama"
  | DashScope -> "dashscope"
  | OpenAI_compat ->
    if Provider_config.is_local config
    then "llama"
    else (
      let request_path = String.trim config.request_path in
      let base_url = normalize_url config.base_url in
      let registry = default () in
      match
        all registry
        |> List.find_opt (fun (entry : entry) ->
          entry.defaults.kind = config.kind
          && String.equal (normalize_url entry.defaults.base_url) base_url
          && String.equal (String.trim entry.defaults.request_path) request_path)
      with
      | Some entry -> entry.name
      | None -> "openai")
;;
