(** Sampling parameter defaults and Gemini URL construction.

    Extracted from {!Complete} to keep the main completion module
    focused on request/response lifecycle.

    @since 0.205.9 *)

(** Construct the URL for a Gemini API call.
    Sync: [base_url/models/model_id:generateContent]
    Stream: [base_url/models/model_id:streamGenerateContent?alt=sse]

    The API key is never included in the URL.  Callers must add it to the
    [x-goog-api-key] header via {!Provider_config.auth_headers_for_config}. *)
let gemini_url ~(config : Provider_config.t) ~stream =
  let method_name = if stream then "streamGenerateContent" else "generateContent" in
  let base =
    Printf.sprintf "%s/models/%s:%s" config.base_url config.model_id method_name
  in
  if stream then base ^ "?alt=sse" else base
;;

(** Provider-aware sampling parameter defaults.
    Local providers get min_p=0.05 (2026 llama.cpp standard).
    Anthropic gets no top_p (incompatible with temperature).
    Explicit agent_config values always take priority (overlay pattern). *)
type sampling_defaults =
  { default_min_p : float option
  ; default_top_p : float option
  ; default_top_k : int option
  }

(* Shared by every kind that does not inject a sampling floor. Ollama
   also lands here because the backend applies its own per-model
   defaults in [Backend_ollama]; pre-filling a top-level value here
   would shadow that. Only OpenAI_compat carries the non-empty
   [openai_compat_min_p] floor. *)
let no_sampling_defaults : sampling_defaults =
  { default_min_p = None; default_top_p = None; default_top_k = None }
;;

let provider_sampling_defaults (kind : Provider_config.provider_kind) : sampling_defaults =
  match kind with
  | Provider_config.OpenAI_compat | Provider_config.DashScope | Provider_config.Kimi ->
    { default_min_p = Some Constants.Sampling.openai_compat_min_p
    ; default_top_p = None
    ; default_top_k = None
    }
  | Provider_config.Ollama
  | Provider_config.Anthropic
  | Provider_config.Gemini
  | Provider_config.Glm -> no_sampling_defaults
;;

let openai_compat_should_default_min_p (config : Provider_config.t) : bool =
  match Provider_config.capabilities_for_config_model config with
  | Some caps -> caps.supports_min_p
  | None -> Provider_config.is_local config
;;

(** Apply provider defaults to a config, preserving explicit values (overlay pattern).
    Only fills in None fields; explicit values are never overwritten. *)
let apply_sampling_defaults (config : Provider_config.t) : Provider_config.t =
  let defaults = provider_sampling_defaults config.kind in
  let default_min_p =
    match config.kind with
    | Provider_config.OpenAI_compat when not (openai_compat_should_default_min_p config)
      -> None
    | Anthropic | Kimi | OpenAI_compat | Ollama | Gemini | Glm | DashScope ->
      defaults.default_min_p
  in
  { config with
    min_p =
      (match config.min_p with
       | Some _ -> config.min_p
       | None -> default_min_p)
  ; top_p =
      (match config.top_p with
       | Some _ -> config.top_p
       | None -> defaults.default_top_p)
  ; top_k =
      (match config.top_k with
       | Some _ -> config.top_k
       | None -> defaults.default_top_k)
  }
;;

(** Compute the reasoning_effort string that was sent for the given config.
    Delegates to {!Provider_config.reasoning_effort_of_config}. *)
let reasoning_effort_of_config = Provider_config.reasoning_effort_of_config

(* --- gemini_url tests --- *)

let%test "gemini_url sync no api_key" =
  let config : Provider_config.t =
    { kind = Provider_config.Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let url = gemini_url ~config ~stream:false in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
;;

let%test "gemini_url sync with api_key" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = Secret.of_string "mykey"
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let url = gemini_url ~config ~stream:false in
  url = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent"
;;

let%test "gemini_url stream with api_key" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = Secret.of_string "mykey"
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let url = gemini_url ~config ~stream:true in
  url
  = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=sse"
;;

let%test "gemini_url stream no api_key" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let url = gemini_url ~config ~stream:true in
  url
  = "https://gen.googleapis.com/v1beta/models/gemini-2.5-flash:streamGenerateContent?alt=sse"
;;

let%test "gemini_url never leaks api_key even when set" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = "https://gen.googleapis.com/v1beta"
    ; api_key = Secret.of_string "mykey"
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let contains_substring haystack needle =
    let hlen = String.length haystack in
    let nlen = String.length needle in
    let rec scan i =
      if i + nlen > hlen
      then false
      else if String.sub haystack i nlen = needle
      then true
      else scan (i + 1)
    in
    scan 0
  in
  let url_sync = gemini_url ~config ~stream:false in
  let url_stream = gemini_url ~config ~stream:true in
  (not (contains_substring url_sync "mykey"))
  && not (contains_substring url_stream "mykey")
;;

let%test "gemini_url empty base_url no trailing slash" =
  let config : Provider_config.t =
    { kind = Gemini
    ; model_id = "gemini-2.5-flash"
    ; base_url = ""
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = Some 1024
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let url = gemini_url ~config ~stream:false in
  url = "/models/gemini-2.5-flash:generateContent"
;;

(* --- provider_sampling_defaults tests --- *)

let%test "provider_sampling_defaults OpenAI_compat has min_p 0.05" =
  let d = provider_sampling_defaults Provider_config.OpenAI_compat in
  d.default_min_p = Some Constants.Sampling.openai_compat_min_p
;;

let%test "provider_sampling_defaults Anthropic has no min_p" =
  let d = provider_sampling_defaults Provider_config.Anthropic in
  d.default_min_p = None
;;

let%test "provider_sampling_defaults Gemini has no min_p" =
  let d = provider_sampling_defaults Provider_config.Gemini in
  d.default_min_p = None
;;

let%test "apply_sampling_defaults fills min_p for OpenAI_compat" =
  let config : Provider_config.t =
    { kind = OpenAI_compat
    ; model_id = "local-model"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some Constants.Sampling.openai_compat_min_p
;;

let%test "apply_sampling_defaults OpenAI_compat Gemini model does not set min_p" =
  let config : Provider_config.t =
    { kind = OpenAI_compat
    ; model_id = "gemini-2.5-flash"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = None
;;

let%test "apply_sampling_defaults OpenAI_compat dashscope model keeps min_p default" =
  let config : Provider_config.t =
    { kind = OpenAI_compat
    ; model_id = "qwen-turbo"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some Constants.Sampling.openai_compat_min_p
;;

let%test "apply_sampling_defaults preserves explicit min_p override" =
  let config : Provider_config.t =
    { kind = OpenAI_compat
    ; model_id = "local-model"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = Some 0.01
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = Some 0.01
;;

let%test "apply_sampling_defaults Anthropic does not set min_p" =
  let config : Provider_config.t =
    { kind = Anthropic
    ; model_id = "claude-sonnet-4"
    ; base_url = "https://api.anthropic.com"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.min_p = None
;;

let%test "apply_sampling_defaults preserves all explicit values" =
  let config : Provider_config.t =
    { kind = OpenAI_compat
    ; model_id = "local-model"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = Some 0.5
    ; max_tokens = Some 100
    ; max_context = None
    ; top_p = Some 0.9
    ; top_k = Some 40
    ; min_p = Some 0.01
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.temperature = Some 0.5
  && applied.max_tokens = Some 100
  && applied.top_p = Some 0.9
  && applied.top_k = Some 40
  && applied.min_p = Some 0.01
;;

let%test "apply_sampling_defaults Anthropic preserves explicit top_p" =
  let config : Provider_config.t =
    { kind = Anthropic
    ; model_id = "claude-sonnet-4"
    ; base_url = "https://api.anthropic.com"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = Some 0.95
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  let applied = apply_sampling_defaults config in
  applied.top_p = Some 0.95
;;

let%test "reasoning_effort_of_config Ollama default is none" =
  let config : Provider_config.t =
    { kind = Ollama
    ; model_id = "qwq"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = Some false
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  reasoning_effort_of_config config = Some "none"
;;

let%test "reasoning_effort_of_config Ollama thinking=true budget=4096 is medium" =
  let config : Provider_config.t =
    { kind = Ollama
    ; model_id = "qwq"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = Some true
    ; preserve_thinking = None
    ; thinking_budget = Some 4096
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  reasoning_effort_of_config config = Some "medium"
;;

let%test "reasoning_effort_of_config Ollama thinking=true budget=16384 is high" =
  let config : Provider_config.t =
    { kind = Ollama
    ; model_id = "qwq"
    ; base_url = "http://localhost:11434"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = Some true
    ; preserve_thinking = None
    ; thinking_budget = Some 16384
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  reasoning_effort_of_config config = Some "high"
;;

let%test "reasoning_effort_of_config non-Ollama is None" =
  let config : Provider_config.t =
    { kind = OpenAI_compat
    ; model_id = "gpt-4o"
    ; base_url = "https://api.openai.com"
    ; api_key = Secret.empty
    ; request_path = ""
    ; headers = []
    ; system_prompt = None
    ; temperature = None
    ; max_tokens = None
    ; max_context = None
    ; top_p = None
    ; top_k = None
    ; min_p = None
    ; enable_thinking = None
    ; preserve_thinking = None
    ; thinking_budget = None
    ; clear_thinking = None
    ; tool_stream = false
    ; tool_choice = None
    ; disable_parallel_tool_use = false
    ; response_format = Types.Off
    ; output_schema = None
    ; cache_system_prompt = false
    ; supports_tool_choice_override = None
    ; keep_alive = None
    ; internal_model_rotation_count = None
    ; num_ctx = None
    ; seed = None
    ; previous_response_id = None
    ; connect_timeout_s = None
    }
  in
  reasoning_effort_of_config config = None
;;
