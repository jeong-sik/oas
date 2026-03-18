(** Provider abstraction for local and cloud LLM endpoints *)

type provider =
  | Local of { base_url: string }
  | Anthropic
  | OpenAICompat of {
      base_url: string;
      auth_header: string option;
      path: string;
      static_token: string option;
    }
  | Custom_registered of { name: string }

type config = {
  provider: provider;
  model_id: string;
  api_key_env: string;
}

type request_kind =
  | Anthropic_messages
  | Openai_chat_completions
  | Custom of string

type capabilities = {
  supports_tools: bool;
  supports_tool_choice: bool;
  supports_reasoning: bool;
  supports_response_format_json: bool;
  supports_multimodal_inputs: bool;
  supports_native_streaming: bool;
  supports_top_k: bool;
  supports_min_p: bool;
}

type model_spec = {
  provider: provider;
  model_id: string;
  api_key_env: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
}

let default_capabilities = {
  supports_tools = false;
  supports_tool_choice = false;
  supports_reasoning = false;
  supports_response_format_json = false;
  supports_multimodal_inputs = false;
  supports_native_streaming = false;
  supports_top_k = false;
  supports_min_p = false;
}

let anthropic_capabilities = {
  default_capabilities with
  supports_tools = true;
  supports_tool_choice = true;
  supports_reasoning = true;
  supports_multimodal_inputs = true;
  supports_native_streaming = true;
}

let openai_chat_capabilities = {
  default_capabilities with
  supports_tools = true;
  supports_tool_choice = true;
  supports_response_format_json = true;
  supports_multimodal_inputs = true;
  supports_native_streaming = true;
}

let qwen_openai_chat_capabilities = {
  openai_chat_capabilities with
  supports_reasoning = true;
  supports_top_k = true;
  supports_min_p = true;
}

let string_contains = Util.string_contains

let is_qwen_family model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  string_contains ~needle:"qwen" normalized

(* ── Provider Registry: runtime registration for custom providers ── *)

type provider_impl = {
  name: string;
  request_kind: request_kind;
  request_path: string;
  capabilities: capabilities;
  build_body:
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    unit -> string;
  parse_response: string -> Types.api_response;
  resolve: config -> (string * string * (string * string) list, Error.sdk_error) result;
}

let registry : (string, provider_impl) Hashtbl.t = Hashtbl.create 8
let registry_mu = Eio.Mutex.create ()

let register_provider impl =
  Eio.Mutex.use_rw ~protect:true registry_mu (fun () ->
    Hashtbl.replace registry impl.name impl)

let find_provider name =
  Eio.Mutex.use_ro registry_mu (fun () ->
    Hashtbl.find_opt registry name)

let registered_providers () =
  Eio.Mutex.use_ro registry_mu (fun () ->
    Hashtbl.fold (fun name _ acc -> name :: acc) registry [])

let capabilities_for_model ~(provider : provider) ~(model_id : string) =
  match provider with
  | Anthropic | Local _ -> anthropic_capabilities
  | OpenAICompat _ ->
      if is_qwen_family model_id then qwen_openai_chat_capabilities
      else openai_chat_capabilities
  | Custom_registered { name } ->
      (match find_provider name with
       | Some impl -> impl.capabilities
       | None -> default_capabilities)

let request_kind = function
  | Local _ | Anthropic -> Anthropic_messages
  | OpenAICompat _ -> Openai_chat_completions
  | Custom_registered { name } ->
      (match find_provider name with
       | Some impl -> impl.request_kind
       | None -> Custom name)

let request_path = function
  | Local _ | Anthropic -> "/v1/messages"
  | OpenAICompat { path; _ } -> path
  | Custom_registered { name } ->
      (match find_provider name with
       | Some impl -> impl.request_path
       | None -> "/v1/chat/completions")

let capabilities_for_config (cfg : config) =
  capabilities_for_model ~provider:cfg.provider ~model_id:cfg.model_id

let model_spec_of_config (cfg : config) =
  {
    provider = cfg.provider;
    model_id = cfg.model_id;
    api_key_env = cfg.api_key_env;
    request_kind = request_kind cfg.provider;
    request_path = request_path cfg.provider;
    capabilities = capabilities_for_config cfg;
  }

let resolve (cfg : config) =
  match cfg.provider with
  | Local { base_url } ->
    Ok (base_url, "dummy", [("Content-Type", "application/json")])
  | Anthropic ->
    (match Sys.getenv_opt cfg.api_key_env with
     | Some key -> Ok ("https://api.anthropic.com", key,
       [("x-api-key", key);
        ("anthropic-version", "2023-06-01");
        ("Content-Type", "application/json")])
     | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))
  | OpenAICompat { base_url; auth_header; static_token; _ } ->
    (match static_token with
     | Some key when String.trim key <> "" ->
         let headers =
           match auth_header with
           | Some header -> [ (header, "Bearer " ^ key); ("Content-Type", "application/json") ]
           | None -> [ ("Content-Type", "application/json") ]
         in
         Ok (base_url, key, headers)
     | _ ->
         (match auth_header with
          | None ->
              Ok (base_url, "", [ ("Content-Type", "application/json") ])
          | Some header ->
              (match Sys.getenv_opt cfg.api_key_env with
               | Some key ->
                   Ok
                     ( base_url,
                       key,
                       [ (header, "Bearer " ^ key); ("Content-Type", "application/json") ] )
               | None -> Error (Error.Config (MissingEnvVar { var_name = cfg.api_key_env })))))
  | Custom_registered { name } ->
    (match find_provider name with
     | Some impl -> impl.resolve cfg
     | None -> Error (Error.Config (InvalidConfig {
         field = "provider";
         detail = Printf.sprintf "Custom provider '%s' not registered" name })))

let local_qwen () = {
  provider = Local { base_url = Defaults.local_qwen_url };
  model_id = "qwen3.5-35b-a3b-ud-q8-xl";
  api_key_env = "DUMMY_KEY";
}

let anthropic_sonnet () = {
  provider = Anthropic;
  model_id = "claude-sonnet-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

let anthropic_haiku () = {
  provider = Anthropic;
  model_id = "claude-haiku-4-5-20251001";
  api_key_env = "ANTHROPIC_API_KEY";
}

let anthropic_opus () = {
  provider = Anthropic;
  model_id = "claude-opus-4-6";
  api_key_env = "ANTHROPIC_API_KEY";
}

let local_mlx () = {
  provider = Local { base_url = Defaults.local_mlx_url };
  model_id = "qwen3.5";
  api_key_env = "DUMMY_KEY";
}

let openrouter ?(model_id="anthropic/claude-sonnet-4-6") () = {
  provider = OpenAICompat {
    base_url = "https://openrouter.ai/api/v1";
    auth_header = Some "Authorization";
    path = "/chat/completions";
    static_token = None;
  };
  model_id;
  api_key_env = "OPENROUTER_API_KEY";
}

(* ── Cascade: multi-provider failover ──────────────────────────── *)

type cascade = {
  primary: config;
  fallbacks: config list;
}

let cascade ~primary ~fallbacks = { primary; fallbacks }

(* ── Pricing: per-model cost estimation ────────────────────────── *)

type pricing = {
  input_per_million: float;
  output_per_million: float;
  cache_write_multiplier: float;  (* cache creation tokens cost input_rate * this *)
  cache_read_multiplier: float;   (* cache read tokens cost input_rate * this *)
}

let pricing_for_model model_id =
  let normalized = String.lowercase_ascii (String.trim model_id) in
  (* Anthropic cache pricing: write = 1.25x input, read = 0.1x input.
     OpenAI/local: no cache pricing (multipliers are 1.0 and 1.0 for no-op). *)
  let anthropic_cache = (1.25, 0.1) in
  let no_cache = (1.0, 1.0) in
  let base, (cw, cr) =
    if string_contains ~needle:"opus-4-6" normalized then
      (15.0, 75.0), anthropic_cache
    else if string_contains ~needle:"opus-4-5" normalized then
      (15.0, 75.0), anthropic_cache
    else if string_contains ~needle:"sonnet-4-6" normalized then
      (3.0, 15.0), anthropic_cache
    else if string_contains ~needle:"sonnet-4" normalized then
      (3.0, 15.0), anthropic_cache
    else if string_contains ~needle:"haiku-4-5" normalized then
      (0.8, 4.0), anthropic_cache
    else if string_contains ~needle:"claude-3-7-sonnet" normalized then
      (3.0, 15.0), anthropic_cache
    else if string_contains ~needle:"gpt-4o-mini" normalized then
      (0.15, 0.6), no_cache
    else if string_contains ~needle:"gpt-4o" normalized then
      (2.5, 10.0), no_cache
    else if string_contains ~needle:"gpt-4.1" normalized then
      (2.0, 8.0), no_cache
    else if string_contains ~needle:"o3-mini" normalized then
      (1.1, 4.4), no_cache
    else if string_contains ~needle:"ollama" normalized
         || string_contains ~needle:"qwen" normalized
         || string_contains ~needle:"llama" normalized then
      (0.0, 0.0), no_cache
    else
      (0.0, 0.0), no_cache
  in
  let input_per_million, output_per_million = base in
  { input_per_million; output_per_million;
    cache_write_multiplier = cw; cache_read_multiplier = cr }

let estimate_cost ~(pricing : pricing)
    ~input_tokens ~output_tokens
    ?(cache_creation_input_tokens=0) ?(cache_read_input_tokens=0) () =
  (* Regular input tokens (excluding cache tokens — those are billed separately) *)
  let regular_input = input_tokens - cache_creation_input_tokens - cache_read_input_tokens in
  let regular_input = max 0 regular_input in
  let rate = pricing.input_per_million /. 1_000_000.0 in
  let input_cost = Float.of_int regular_input *. rate in
  let cache_write_cost =
    Float.of_int cache_creation_input_tokens *. rate *. pricing.cache_write_multiplier in
  let cache_read_cost =
    Float.of_int cache_read_input_tokens *. rate *. pricing.cache_read_multiplier in
  let output_cost = Float.of_int output_tokens *. pricing.output_per_million /. 1_000_000.0 in
  input_cost +. cache_write_cost +. cache_read_cost +. output_cost

(* ── Convenience: create config for a Custom_registered provider ── *)

let custom_provider ~name ?(model_id="custom") ?(api_key_env="DUMMY_KEY") () = {
  provider = Custom_registered { name };
  model_id;
  api_key_env;
}
