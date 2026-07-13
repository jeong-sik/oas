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

let has_api_key env_name =
  env_name = ""
  ||
  match Cli_common_env.get env_name with
  | Some value -> String.trim value <> ""
  | None -> false
;;

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
    if String.trim entry.id = ""
    then invalid_arg "Provider_registry.register_catalog_entry: empty provider id"
    else
      Hashtbl.replace
        t.entries
        entry.id
        { name = entry.id
        ; defaults
        ; max_context
        ; capabilities = entry.capabilities
        ; is_available = (fun () -> catalog_entry_available entry)
        })
;;

let overlay_provider_catalog t =
  match Provider_catalog.global () with
  | None -> ()
  | Some entries -> List.iter (register_catalog_entry t) entries
;;

(** Mutable endpoint list, protected by atomic snapshot swap.
    Updated by [refresh_llama_endpoints]. *)
let llama_endpoints_ref : Discovery.endpoint array Atomic.t = Atomic.make [||]

(** Round-robin counter for distributing calls across endpoints. *)
let llama_rr_counter = Atomic.make 0

(** Pick the next llama endpoint via round-robin.
    Reads the current endpoint snapshot atomically.
    Called by downstream callers when resolving "llama:*" provider. *)
let next_llama_endpoint () =
  let endpoints = Atomic.get llama_endpoints_ref in
  let n = Array.length endpoints in
  if n = 0
  then None
  else (
    let idx = Atomic.fetch_and_add llama_rr_counter 1 mod n in
    Some endpoints.(idx))
;;

(** Peek at the current llama endpoint without advancing the round-robin.
    Used by context resolution to match the endpoint that will serve
    the next request, without side effects. *)
let current_llama_endpoint () =
  let endpoints = Atomic.get llama_endpoints_ref in
  let n = Array.length endpoints in
  if n = 0
  then None
  else (
    let idx = Atomic.get llama_rr_counter mod n in
    Some endpoints.(idx))
;;

type endpoint_refresh_error =
  | No_endpoints_declared
  | No_healthy_endpoints of Discovery.endpoint_status list

(** Refresh from typed endpoint declarations only. Discovery never scans ports
    or invents a fallback declaration. The previous active snapshot is retained
    when no declaration is supplied or every declared endpoint is unhealthy. *)
let refresh_llama_endpoints ~sw ~net ~endpoints =
  match endpoints with
  | [] -> Error No_endpoints_declared
  | _ ->
    let statuses = Discovery.refresh_and_sync ~sw ~net ~endpoints in
    let healthy_endpoints =
      List.filter_map
        (fun (status : Discovery.endpoint_status) ->
           if status.healthy
           then
             Some
               (Discovery.endpoint
                  ~protocol:status.protocol
                  ~capabilities:status.capabilities
                  status.url)
           else None)
        statuses
    in
    (match healthy_endpoints with
     | [] -> Error (No_healthy_endpoints statuses)
     | _ ->
       Atomic.set llama_endpoints_ref (Array.of_list healthy_endpoints);
       Ok statuses)
;;

(** Current active endpoint list (snapshot). *)
let active_llama_endpoints () = Array.to_list (Atomic.get llama_endpoints_ref)

let discovered_max_context () = Discovery.discovered_per_slot_context ()

let discovered_endpoint_max_context (url : string) =
  Discovery.discovered_context_for_url url
;;

let llama_defaults =
  { kind = OpenAI_compat
  ; base_url = Discovery.default_endpoint
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

let gemini_defaults =
  { kind = Gemini
  ; base_url = "https://generativelanguage.googleapis.com/v1beta"
  ; api_key_env = "GEMINI_API_KEY"
  ; request_path = ""
  }
;;

let glm_defaults =
  { kind = Glm
  ; base_url = Zai_catalog.general_base_url
  ; api_key_env = "ZAI_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let glm_coding_defaults =
  { kind = Glm
  ; base_url = Zai_catalog.coding_base_url
  ; api_key_env = "ZAI_CODING_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let kimi_defaults =
  { kind = Kimi
  ; base_url = "https://api.kimi.com/coding"
  ; api_key_env = "KIMI_API_KEY"
  ; request_path = "/v1/messages"
  }
;;

let ollama_defaults =
  { kind = Ollama
  ; base_url = Discovery.ollama_endpoint
  ; api_key_env = ""
  ; request_path = "/api/chat"
  }
;;

let ollama_cloud_defaults =
  { kind = Ollama
  ; base_url = "https://ollama.com"
  ; api_key_env = "OLLAMA_CLOUD_API_KEY"
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
  ; base_url = "https://api.groq.com/openai/v1"
  ; api_key_env = "GROQ_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let deepseek_defaults =
  { kind = OpenAI_compat
  ; base_url = "https://api.deepseek.com"
  ; api_key_env = "DEEPSEEK_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let dashscope_defaults =
  { kind = DashScope
  ; base_url = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
  ; api_key_env = "DASHSCOPE_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let siliconflow_defaults =
  { kind = OpenAI_compat
  ; base_url = "https://api.siliconflow.cn/v1"
  ; api_key_env = "SILICONFLOW_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let xai_defaults =
  { kind = OpenAI_compat
  ; base_url = "https://api.x.ai/v1"
  ; api_key_env = "XAI_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let mistral_defaults =
  { kind = OpenAI_compat
  ; base_url = "https://api.mistral.ai/v1"
  ; api_key_env = "MISTRAL_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let cohere_defaults =
  { kind = OpenAI_compat
  ; base_url = "https://api.cohere.com/compatibility/v1"
  ; api_key_env = "COHERE_API_KEY"
  ; request_path = "/chat/completions"
  }
;;

let default () =
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
      ; base_url = entry.base_url
      ; api_key_env = entry.api_key_env
      ; request_path = entry.request_path
      }
    in
    let max_context = max_context_from_capabilities ~default:128_000 caps in
    register
      t
      { name = entry.id
      ; defaults
      ; max_context
      ; capabilities = caps
      ; is_available = (fun () -> has_api_key defaults.api_key_env)
      }
  in
  reg
    "nous"
    llama_defaults
    ~max_context:128_000
    Capabilities.openai_compat_chat_extended_capabilities;
  reg "claude" claude_defaults ~max_context:200_000 Capabilities.anthropic_capabilities;
  reg "gemini" gemini_defaults ~max_context:1_000_000 Capabilities.gemini_capabilities;
  reg "glm" glm_defaults ~max_context:200_000 Capabilities.glm_capabilities;
  reg "glm-coding" glm_coding_defaults ~max_context:128_000 Capabilities.glm_capabilities;
  register
    t
    { name = "kimi"
    ; defaults = kimi_defaults
    ; max_context =
        max_context_from_capabilities ~default:262_144 Capabilities.kimi_capabilities
    ; capabilities = Capabilities.kimi_capabilities
    ; is_available = (fun () -> has_api_key kimi_defaults.api_key_env)
    };
  reg
    "openrouter"
    openrouter_defaults
    ~max_context:128_000
    Capabilities.openai_compat_chat_extended_capabilities;
  reg
    "groq"
    groq_defaults
    ~max_context:131_072
    Capabilities.openai_compat_chat_capabilities;
  (* Deepseek v4 series (flash / pro). 1M context, reasoning, tools. *)
  reg
    "deepseek"
    deepseek_defaults
    ~max_context:1_000_000
    Capabilities.openai_compat_chat_capabilities;
  reg
    "dashscope"
    dashscope_defaults
    ~max_context:131_072
    Capabilities.dashscope_capabilities;
  reg
    "siliconflow"
    siliconflow_defaults
    ~max_context:128_000
    Capabilities.openai_compat_chat_capabilities;
  reg "xai" xai_defaults ~max_context:1_000_000 (capabilities_for_registered_label "xai");
  reg
    "mistral"
    mistral_defaults
    ~max_context:260_000
    (capabilities_for_registered_label "mistral");
  reg
    "cohere"
    cohere_defaults
    ~max_context:256_000
    (capabilities_for_registered_label "cohere");
  (match Model_catalog.global () with
   | None -> ()
   | Some catalog ->
     List.iter register_model_catalog_provider (Model_catalog.provider_entries catalog));
  register
    t
    { name = "ollama"
    ; defaults = ollama_defaults
    ; max_context = 262_144
    ; capabilities = Capabilities.ollama_capabilities
    ; is_available = (fun () -> true)
    };
  reg
    "ollama_cloud"
    ollama_cloud_defaults
    ~max_context:262_144
    Capabilities.ollama_cloud_capabilities;
  overlay_provider_catalog t;
  t
;;

let provider_name_of_config (config : Provider_config.t) =
  Provider_config.string_of_provider_kind config.kind
;;
