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

let llama_defaults = {
  kind = OpenAI_compat;
  base_url =
    (match Sys.getenv_opt "LLM_ENDPOINTS" with
     | Some s ->
       (match String.split_on_char ',' s with
        | url :: _ -> String.trim url
        | [] -> "http://127.0.0.1:8085")
     | None -> "http://127.0.0.1:8085");
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
  kind = OpenAI_compat;
  base_url =
    (match Sys.getenv_opt "GEMINI_BASE_URL" with
     | Some url -> url
     | None -> "https://generativelanguage.googleapis.com/v1beta/openai");
  api_key_env = "GEMINI_API_KEY";
  request_path = "/chat/completions";
}

let glm_defaults = {
  kind = OpenAI_compat;
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
  let reg name defaults caps =
    register t { name; defaults; capabilities = caps;
                 is_available = (fun () -> has_api_key defaults.api_key_env) }
  in
  reg "llama" llama_defaults Capabilities.openai_chat_extended_capabilities;
  reg "claude" claude_defaults Capabilities.anthropic_capabilities;
  reg "gemini" gemini_defaults Capabilities.openai_chat_capabilities;
  reg "glm" glm_defaults Capabilities.openai_chat_capabilities;
  reg "openrouter" openrouter_defaults Capabilities.openai_chat_extended_capabilities;
  t
