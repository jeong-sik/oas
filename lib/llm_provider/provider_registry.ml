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

type mutex =
  | Stdlib_mu of Mutex.t
  | Eio_mu of Eio.Mutex.t

type t =
  { mu : mutex
  ; entries : (string, entry) Hashtbl.t
  }

let create () = { mu = Eio_mu (Eio.Mutex.create ()); entries = Hashtbl.create 8 }
let create_sync () = { mu = Stdlib_mu (Mutex.create ()); entries = Hashtbl.create 8 }

let with_lock t f =
  match t.mu with
  | Stdlib_mu mu ->
    Mutex.lock mu;
    Fun.protect f ~finally:(fun () -> Mutex.unlock mu)
  | Eio_mu mu -> Eio.Mutex.use_rw ~protect:true mu f
;;

let register' t entry = Hashtbl.replace t.entries entry.name entry
let register t entry = with_lock t (fun () -> register' t entry)
let unregister t name = with_lock t (fun () -> Hashtbl.remove t.entries name)
let find t name = with_lock t (fun () -> Hashtbl.find_opt t.entries name)
let all t = with_lock t (fun () -> Hashtbl.fold (fun _k v acc -> v :: acc) t.entries [])
let available t = all t |> List.filter (fun e -> e.is_available ())
let find_capable t pred = all t |> List.filter (fun e -> pred e.capabilities)

(* ── Default registry ─────────────────────────────────── *)

let api_key_env_candidates = function
  | "OLLAMA_CLOUD_API_KEY" -> [ "OLLAMA_CLOUD_API_KEY"; "OLLAMA_API_KEY" ]
  | "GEMINI_API_KEY" -> [ "GEMINI_API_KEY"; "GEMINI_API_KEY" ]
  | env_name -> [ env_name ]
;;

let has_direct_api_key env_name =
  env_name = ""
  ||
  match Cli_common_env.get env_name with
  | Some value -> String.trim value <> ""
  | None -> false
;;

let has_api_key env_name =
  api_key_env_candidates env_name |> List.exists has_direct_api_key
;;

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

let safe_file_exists path =
  try Sys.file_exists path with
  | Sys_error _ | Unix.Unix_error _ -> false
;;

let safe_is_directory path =
  try Sys.is_directory path with
  | Sys_error _ | Unix.Unix_error _ -> false
;;

let is_runnable_path path = safe_file_exists path && not (safe_is_directory path)

let command_in_path ?path name =
  path_entries ?path ()
  |> List.exists (fun dir ->
    command_candidates ~name
    |> List.exists (fun candidate -> is_runnable_path (Filename.concat dir candidate)))
;;

let catalog_command_available (entry : Provider_catalog.entry) =
  match entry.transport with
  | Provider_catalog.Http | Provider_catalog.Managed -> true
;;

let catalog_auth_available (entry : Provider_catalog.entry) =
  match entry.auth with
  | Provider_catalog.Api_key_env env | Provider_catalog.Setup_token_env env ->
    has_api_key env
  | Provider_catalog.No_auth -> true
  | Provider_catalog.Oauth_cached_login -> true
;;

let catalog_entry_available entry =
  catalog_command_available entry && catalog_auth_available entry
;;

let register_catalog_entry t (entry : Provider_catalog.entry) =
  with_lock t (fun () ->
    let max_context =
      match entry.max_context, entry.capabilities.Capabilities.max_context_tokens with
      | Some n, _ when n > 0 -> n
      | _, Some n when n > 0 -> n
      | _ -> 128_000
    in
    let defaults : provider_defaults =
      { kind = entry.kind
      ; base_url = entry.base_url
      ; api_key_env = entry.api_key_env
      ; request_path = entry.request_path
      }
    in
    let register_name ~origin name =
      let normalized = String.lowercase_ascii (String.trim name) in
      if normalized = ""
      then
        Diag.warn
          "provider_registry"
          "ignoring empty %s for provider %S in catalog overlay"
          origin
          entry.id
      else
        Hashtbl.replace
          t.entries
          normalized
          { name = normalized
          ; defaults
          ; max_context
          ; capabilities = entry.capabilities
          ; is_available = (fun () -> catalog_entry_available entry)
          }
    in
    register_name ~origin:"id" entry.id;
    List.iter (register_name ~origin:"alias") entry.aliases)
;;

let overlay_provider_catalog t =
  match Provider_catalog.global () with
  | None -> ()
  | Some entries -> List.iter (register_catalog_entry t) entries
;;

(** Initial endpoint snapshot.
    Keep module initialization free of environment reads. Runtime callers that
    need env-aware values use [default_llama_endpoint] or
    [refresh_llama_endpoints]. *)
let initial_llama_endpoints = [ Discovery.default_endpoint ]

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
  (* Guard n = 0 like the sibling [current_llama_endpoint]: without it [_ mod n]
     raises Division_by_zero. The array is non-empty for every reachable call
     today (initial_llama_endpoints falls back to a singleton), so this only
     makes the degenerate case total and consistent with the peer. *)
  if n = 0
  then ""
  else (
    let idx = Atomic.fetch_and_add llama_rr_counter 1 mod n in
    endpoints.(idx))
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
          (Discovery.default_scan_ports ())
      in
      let statuses = Discovery.refresh_and_sync ~sw ~net ~endpoints:candidates in
      let found =
        List.filter_map
          (fun (s : Discovery.endpoint_status) -> if s.healthy then Some s.url else None)
          statuses
      in
      if found = [] then [ Discovery.resolve_default_endpoint () ] else found
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

let default_llama_endpoint ?getenv () =
  match Discovery.parse_llm_endpoints_env ?getenv () with
  | first :: _ -> first
  | [] -> Discovery.resolve_default_endpoint ?getenv ()
;;

let llama_defaults ?getenv () =
  { kind = OpenAI_compat
  ; base_url = default_llama_endpoint ?getenv ()
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

(* Env overrides resolve at call time through the RFC-OAS-024 [?getenv]
   seam; the [*_defaults] thunks below must not be forced at module load
   (see the module-initialization invariant on [initial_llama_endpoints]). *)
let env_or_default ?getenv env_name default_url =
  match Cli_common_env.get ?getenv env_name with
  | Some url -> url
  | None -> default_url
;;

(* ── Endpoint identities ─────────────────────────────── *)

(** Documented endpoint identity for providers whose registry name is
    distinguished by endpoint rather than by wire kind alone.

    [canonical_base_url] is the documented SSOT URL; [base_url_env] is the
    deployment override knob. Registry construction ([default]) resolves the
    override at call time via {!identity_default_base_url}. Identity
    classification ({!provider_name_of_config}) recognizes the canonical URL
    unconditionally and the env override additively (same additive shape as
    [Zai_catalog.configured_base_urls]): an env override selects where the
    default registry connects, but must never erase the documented identity
    of an already-built [Provider_config.t]. *)
type endpoint_identity =
  { identity_name : string
  ; identity_kind : Provider_config.provider_kind
  ; canonical_base_url : string
  ; base_url_env : string option
  ; identity_request_path : string
  }

let ollama_cloud_identity =
  { identity_name = "ollama_cloud"
  ; identity_kind = Ollama
  ; canonical_base_url = "https://ollama.com"
  ; base_url_env = Some "OLLAMA_CLOUD_BASE_URL"
  ; identity_request_path = "/api/chat"
  }
;;

let openrouter_identity =
  { identity_name = "openrouter"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://openrouter.ai/api/v1"
  ; base_url_env = None
  ; identity_request_path = "/chat/completions"
  }
;;

let groq_identity =
  { identity_name = "groq"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://api.groq.com/openai/v1"
  ; base_url_env = Some "GROQ_BASE_URL"
  ; identity_request_path = "/chat/completions"
  }
;;

let deepseek_identity =
  { identity_name = "deepseek"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://api.deepseek.com"
  ; base_url_env = Some "DEEPSEEK_BASE_URL"
  ; identity_request_path = "/chat/completions"
  }
;;

let siliconflow_identity =
  { identity_name = "siliconflow"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://api.siliconflow.cn/v1"
  ; base_url_env = Some "SILICONFLOW_BASE_URL"
  ; identity_request_path = "/chat/completions"
  }
;;

let xai_identity =
  { identity_name = "xai"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://api.x.ai/v1"
  ; base_url_env = Some "XAI_BASE_URL"
  ; identity_request_path = "/chat/completions"
  }
;;

let mistral_identity =
  { identity_name = "mistral"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://api.mistral.ai/v1"
  ; base_url_env = Some "MISTRAL_BASE_URL"
  ; identity_request_path = "/chat/completions"
  }
;;

let cohere_identity =
  { identity_name = "cohere"
  ; identity_kind = OpenAI_compat
  ; canonical_base_url = "https://api.cohere.com/compatibility/v1"
  ; base_url_env = Some "COHERE_BASE_URL"
  ; identity_request_path = "/chat/completions"
  }
;;

(* Deterministic match order for [provider_name_of_config]. Canonical URLs
   are pairwise distinct; if two env overrides are set to the same URL, the
   first entry here wins (canonical matches are resolved in a separate pass
   before any env-derived match, so an override can never shadow another
   provider's documented endpoint). *)
let endpoint_identities =
  [ ollama_cloud_identity
  ; openrouter_identity
  ; groq_identity
  ; deepseek_identity
  ; siliconflow_identity
  ; xai_identity
  ; mistral_identity
  ; cohere_identity
  ]
;;

(** Base URL the default registry connects to right now: the env override
    when set, otherwise the documented canonical URL. *)
let identity_default_base_url ?getenv (identity : endpoint_identity) =
  match identity.base_url_env with
  | None -> identity.canonical_base_url
  | Some env_name -> env_or_default ?getenv env_name identity.canonical_base_url
;;

let gemini_defaults ?getenv () =
  { kind = Gemini
  ; base_url =
      env_or_default
        ?getenv
        "GEMINI_BASE_URL"
        "https://generativelanguage.googleapis.com/v1beta"
  ; api_key_env = "GEMINI_API_KEY"
  ; request_path = ""
  }
;;

let glm_defaults ?getenv () =
  { kind = Glm
  ; base_url = env_or_default ?getenv "ZAI_BASE_URL" Zai_catalog.general_base_url
  ; api_key_env = "ZAI_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let glm_coding_defaults ?getenv () =
  { kind = Glm
  ; base_url = env_or_default ?getenv "ZAI_CODING_BASE_URL" Zai_catalog.coding_base_url
  ; api_key_env = "ZAI_CODING_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let kimi_defaults ?getenv () =
  { kind = Kimi
  ; base_url = env_or_default ?getenv "KIMI_BASE_URL" "https://api.kimi.com/coding"
  ; api_key_env = "KIMI_API_KEY"
  ; request_path = "/v1/messages"
  }
;;

let ollama_defaults ?getenv () =
  { kind = Ollama
  ; base_url = Discovery.resolve_ollama_endpoint ?getenv ()
  ; api_key_env = ""
  ; request_path = "/api/chat"
  }
;;

let ollama_cloud_defaults ?getenv () =
  { kind = ollama_cloud_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv ollama_cloud_identity
  ; api_key_env = "OLLAMA_CLOUD_API_KEY"
  ; request_path = ollama_cloud_identity.identity_request_path
  }
;;

let openrouter_defaults =
  { kind = openrouter_identity.identity_kind
  ; base_url = openrouter_identity.canonical_base_url
  ; api_key_env = "OPENROUTER_API_KEY"
  ; request_path = openrouter_identity.identity_request_path
  }
;;

let provider_i_defaults ?getenv () =
  { kind = groq_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv groq_identity
  ; api_key_env = "GROQ_API_KEY"
  ; request_path = groq_identity.identity_request_path
  }
;;

let deepseek_defaults ?getenv () =
  { kind = deepseek_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv deepseek_identity
  ; api_key_env = "DEEPSEEK_API_KEY"
  ; request_path = deepseek_identity.identity_request_path
  }
;;

let dashscope_defaults ?getenv () =
  { kind = DashScope
  ; base_url =
      env_or_default
        ?getenv
        "DASHSCOPE_BASE_URL"
        "https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
  ; api_key_env = "DASHSCOPE_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let siliconflow_defaults ?getenv () =
  { kind = siliconflow_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv siliconflow_identity
  ; api_key_env = "SILICONFLOW_API_KEY"
  ; request_path = siliconflow_identity.identity_request_path
  }
;;

let xai_defaults ?getenv () =
  { kind = xai_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv xai_identity
  ; api_key_env = "XAI_API_KEY"
  ; request_path = xai_identity.identity_request_path
  }
;;

let mistral_defaults ?getenv () =
  { kind = mistral_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv mistral_identity
  ; api_key_env = "MISTRAL_API_KEY"
  ; request_path = mistral_identity.identity_request_path
  }
;;

let cohere_defaults ?getenv () =
  { kind = cohere_identity.identity_kind
  ; base_url = identity_default_base_url ?getenv cohere_identity
  ; api_key_env = "COHERE_API_KEY"
  ; request_path = cohere_identity.identity_request_path
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

let default ?getenv () =
  (* The default registry uses Stdlib.Mutex because its guarded sections are
     short Hashtbl operations and the returned value still exposes the mutable
     registry API. *)
  let t = create_sync () in
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
  let capabilities_for_registered_label label =
    match Capabilities.capabilities_for_provider_label label with
    | Some caps -> caps
    | None ->
      invalid_arg
        (Printf.sprintf
           "Provider_registry.default: no capabilities for registered provider %S"
           label)
  in
  let register_model_catalog_provider (entry : Model_catalog.provider_entry) =
    let capability_label =
      Option.value entry.capabilities_base ~default:entry.id
      |> String.lowercase_ascii
      |> String.trim
    in
    let caps = capabilities_for_registered_label capability_label in
    let defaults : provider_defaults =
      { kind = entry.kind
      ; base_url =
          (match entry.base_url_env with
           | None -> entry.base_url
           | Some env_name -> env_or_default ?getenv env_name entry.base_url)
      ; api_key_env = entry.api_key_env
      ; request_path = entry.request_path
      }
    in
    let max_context = max_context_from_capabilities ~default:128_000 caps in
    let register_name name =
      let normalized = String.lowercase_ascii (String.trim name) in
      if normalized <> ""
      then
        register
          t
          { name = normalized
          ; defaults
          ; max_context
          ; capabilities = caps
          ; is_available = (fun () -> has_api_key defaults.api_key_env)
          }
    in
    register_name entry.id;
    List.iter register_name entry.aliases
  in
  reg
    "nous"
    (llama_defaults ?getenv ())
    ~max_context:128_000
    Capabilities.openai_compat_chat_extended_capabilities;
  reg "claude" claude_defaults ~max_context:200_000 Capabilities.anthropic_capabilities;
  reg
    "gemini"
    (gemini_defaults ?getenv ())
    ~max_context:1_000_000
    Capabilities.gemini_capabilities;
  reg "glm" (glm_defaults ?getenv ()) ~max_context:200_000 Capabilities.glm_capabilities;
  reg
    "glm-coding"
    (glm_coding_defaults ?getenv ())
    ~max_context:128_000
    Capabilities.glm_capabilities;
  register
    t
    { name = "kimi"
    ; defaults = kimi_defaults ?getenv ()
    ; max_context =
        max_context_from_capabilities ~default:262_144 Capabilities.kimi_capabilities
    ; capabilities = Capabilities.kimi_capabilities
    ; is_available = (fun () -> has_any_api_key [ "KIMI_API_KEY" ])
    };
  reg
    "openrouter"
    openrouter_defaults
    ~max_context:128_000
    Capabilities.openai_compat_chat_extended_capabilities;
  reg
    "groq"
    (provider_i_defaults ?getenv ())
    ~max_context:131_072
    Capabilities.openai_compat_chat_capabilities;
  (* Deepseek v4 series (flash / pro). 1M context, reasoning, tools. *)
  reg
    "deepseek"
    (deepseek_defaults ?getenv ())
    ~max_context:1_000_000
    Capabilities.openai_compat_chat_capabilities;
  reg
    "dashscope"
    (dashscope_defaults ?getenv ())
    ~max_context:131_072
    Capabilities.dashscope_capabilities;
  reg
    "alibaba"
    (dashscope_defaults ?getenv ())
    ~max_context:131_072
    Capabilities.dashscope_capabilities;
  reg
    "siliconflow"
    (siliconflow_defaults ?getenv ())
    ~max_context:128_000
    Capabilities.openai_compat_chat_capabilities;
  reg
    "xai"
    (xai_defaults ?getenv ())
    ~max_context:1_000_000
    (capabilities_for_registered_label "xai");
  reg
    "mistral"
    (mistral_defaults ?getenv ())
    ~max_context:260_000
    (capabilities_for_registered_label "mistral");
  reg
    "cohere"
    (cohere_defaults ?getenv ())
    ~max_context:256_000
    (capabilities_for_registered_label "cohere");
  (match Model_catalog.global () with
   | None -> ()
   | Some catalog ->
     List.iter register_model_catalog_provider (Model_catalog.provider_entries catalog));
  register
    t
    { name = "ollama"
    ; defaults = ollama_defaults ?getenv ()
    ; max_context = 262_144
    ; capabilities = Capabilities.ollama_capabilities
    ; is_available = (fun () -> true)
    };
  reg
    "ollama_cloud"
    (ollama_cloud_defaults ?getenv ())
    ~max_context:262_144
    Capabilities.ollama_cloud_capabilities;
  overlay_provider_catalog t;
  t
;;

(* Identity classification helpers for [provider_name_of_config]. Identity
   of an already-built [Provider_config.t] must be deterministic: it is
   matched against the pure {!endpoint_identities} table, never against a
   call-time registry construction whose base URLs shift with the process
   environment. The documented canonical URL always identifies its provider;
   the env-overridden default URL identifies it additively when set. *)

let identity_matches_canonical (identity : endpoint_identity) ~kind ~base_url =
  identity.identity_kind = kind
  && String.equal (normalize_url identity.canonical_base_url) base_url
;;

let identity_matches_env_override ?getenv (identity : endpoint_identity) ~kind ~base_url =
  identity.identity_kind = kind
  &&
  match identity.base_url_env with
  | None -> false
  | Some env_name ->
    (match Cli_common_env.get ?getenv env_name with
     | None -> false
     | Some override -> String.equal (normalize_url override) base_url)
;;

let provider_name_of_config ?getenv (config : Provider_config.t) =
  match config.kind with
  | Anthropic -> "claude"
  | Kimi -> "kimi"
  | Gemini -> "gemini"
  | Glm -> if Zai_catalog.is_coding_base_url config.base_url then "glm-coding" else "glm"
  | Ollama ->
    let base_url = normalize_url config.base_url in
    if
      identity_matches_canonical ollama_cloud_identity ~kind:config.kind ~base_url
      || identity_matches_env_override
           ?getenv
           ollama_cloud_identity
           ~kind:config.kind
           ~base_url
    then "ollama_cloud"
    else "ollama"
  | DashScope -> "dashscope"
  | OpenAI_compat ->
    (* A local OpenAI-compatible endpoint (llama.cpp / vLLM / LM Studio / ...) is
       a generic gateway that can serve any model; locality is a transport fact,
       not a vendor identity. Resolve it to the neutral kind label rather than the
       "nous" vendor entry, so telemetry is not misattributed to a specific vendor
       and host locality does not, by itself, grant the extended capability
       preset. The canonical local llama endpoint still receives its declared
       capabilities through the exact endpoint binding in
       [Provider_runtime_binding], not from this name. See RFC-OAS-034 (endpoint
       capability boundary). *)
    if Provider_config.is_local config
    then "openai_compat"
    else (
      let request_path = String.trim config.request_path in
      let base_url = normalize_url config.base_url in
      let shape_matches (identity : endpoint_identity) =
        String.equal (String.trim identity.identity_request_path) request_path
      in
      (* Two passes: every canonical match is resolved before any env-derived
         match, so an override such as [DEEPSEEK_BASE_URL=https://api.x.ai/v1]
         cannot reassign another provider's documented endpoint. *)
      let canonical_match =
        List.find_opt
          (fun identity ->
             shape_matches identity
             && identity_matches_canonical identity ~kind:config.kind ~base_url)
          endpoint_identities
      in
      let env_override_match () =
        List.find_opt
          (fun identity ->
             shape_matches identity
             && identity_matches_env_override ?getenv identity ~kind:config.kind ~base_url)
          endpoint_identities
      in
      (* Provider-catalog overlay entries carry explicit endpoint identities of
         their own: the catalog is deployment configuration resolved once per
         process, not per-call process env drift. They are consulted after the
         built-in identities so a catalog entry cannot erase a documented
         default either. *)
      let catalog_match () =
        match Provider_catalog.global () with
        | None -> None
        | Some entries ->
          entries
          |> List.find_opt (fun (entry : Provider_catalog.entry) ->
            entry.kind = config.kind
            && String.equal (normalize_url entry.base_url) base_url
            && String.equal (String.trim entry.request_path) request_path)
          |> Option.map (fun (entry : Provider_catalog.entry) ->
            entry.id :: entry.aliases
            |> List.filter_map (fun name ->
              let normalized = String.lowercase_ascii (String.trim name) in
              if normalized = "" then None else Some normalized))
          |> Option.map (fun names -> List.nth_opt names 0)
          |> Option.join
      in
      let model_catalog_match () =
        match Model_catalog.global () with
        | None -> None
        | Some catalog ->
          Model_catalog.provider_label_for_endpoint
            ?getenv
            catalog
            ~kind:config.kind
            ~base_url:config.base_url
            ~request_path:config.request_path
      in
      match canonical_match with
      | Some identity -> identity.identity_name
      | None ->
        (match env_override_match () with
         | Some identity -> identity.identity_name
         | None ->
           (match catalog_match () with
            | Some name -> name
            | None ->
              (match model_catalog_match () with
               | Some name -> name
               | None ->
                 (* Provider identity must come from the concrete kind or an explicit
                 endpoint identity. Model catalog provider names describe model
                 provenance/capabilities; using them here would infer transport
                 semantics from a model id on arbitrary compatible gateways. *)
                 "openai_compat"))))
;;
