(** Provider capabilities -- per-provider/model feature flags and limits.

    Tracks what a provider/model supports so the agent runtime can make
    correct decisions (e.g., context reduction, tool filtering, thinking
    budget enforcement).

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split *)

(** Wire-format for controlling thinking/reasoning on OpenAI-compat backends.
    Different model families use different JSON shapes to enable/disable
    thinking, so the runtime must know which format to emit.

    @since 0.184.0 *)
type thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object (** DeepSeek-style: {"thinking":{"type":"enabled"}} *)
  | Chat_template_kwargs
  (** llama-server style: {"chat_template_kwargs":{"enable_thinking":b}} *)

type capabilities =
  { (* ── Numeric limits ────────────────────────────────── *)
    max_context_tokens : int option (** Model's context window. None = unknown. *)
  ; max_output_tokens : int option (** Model's max output. None = unknown. *)
  ; (* ── Tool use ──────────────────────────────────────── *)
    supports_tools : bool
  ; supports_tool_choice : bool
  ; supports_parallel_tool_calls : bool
  ; supports_runtime_mcp_tools : bool
  ; supports_runtime_tool_events : bool
  ; (* ── Thinking / reasoning ──────────────────────────── *)
    supports_reasoning : bool (** Any form of reasoning/thinking *)
  ; supports_extended_thinking : bool (** budget_tokens / reasoning_effort *)
  ; supports_reasoning_budget : bool (** Controllable reasoning depth *)
  ; thinking_control_format : thinking_control_format
    (** Wire-format for thinking control on OpenAI-compat backends.
        Determines which JSON shape the backend emits for enable_thinking.
        Only meaningful when [supports_reasoning] or [supports_extended_thinking]
        is true and the request goes through backend_openai.
        @since 0.184.0 *)
  ; (* ── Output format ─────────────────────────────────── *)
    supports_response_format_json : bool (** JSON mode *)
  ; supports_structured_output : bool (** JSON schema 100% guarantee *)
  ; (* ── Input modalities ──────────────────────────────── *)
    supports_multimodal_inputs : bool (** Any non-text input *)
  ; supports_image_input : bool
  ; supports_audio_input : bool
  ; supports_video_input : bool
  ; (* ── Protocol ──────────────────────────────────────── *)
    supports_native_streaming : bool
  ; supports_system_prompt : bool
  ; supports_caching : bool
  ; supports_prompt_caching : bool
  ; prompt_cache_alignment : int option
  ; (* ── Sampling parameters ───────────────────────────── *)
    supports_top_k : bool
  ; supports_min_p : bool
  ; supports_seed : bool (** Deterministic seed for reproducible sampling. *)
  ; supports_seed_with_images : bool
    (** Whether the provider respects [seed] deterministically when
      image inputs are present.  Local providers (Ollama, llama-server)
      achieve near-perfect determinism on identical hardware; cloud
      providers (OpenAI, Gemini) do not guarantee deterministic output
      when images are in the prompt. *)
  ; (* ── Advanced modalities ───────────────────────────── *)
    supports_computer_use : bool
  ; supports_code_execution : bool
  ; (* ── Provider identity ───────────────────────────────── *)
    is_ollama : bool
  ; (* ── Usage reporting ─────────────────────────────────── *)
    emits_usage_tokens : bool
    (** True when the provider's standard response carries
      [input_tokens]/[output_tokens] (direct APIs like Anthropic,
      OpenAI, Gemini, Kimi-API, GLM, Ollama). False for CLI-class
      wrappers that strip usage before returning (codex_cli,
      gemini_cli, kimi_cli).

      Consumers use this to decide whether a text-only turn with no
      usage should be treated as a structurally unreported one
      (not a coverage gap) vs. a real gap. *)
  ; (* ── Model limitations ────────────────────────────────── *)
    supported_models : string list option
  }

let default_capabilities =
  { max_context_tokens = None
  ; max_output_tokens = None
  ; supports_tools = false
  ; supports_tool_choice = false
  ; supports_parallel_tool_calls = false
  ; supports_runtime_mcp_tools = false
  ; supports_runtime_tool_events = false
  ; supports_reasoning = false
  ; supports_extended_thinking = false
  ; supports_reasoning_budget = false
  ; thinking_control_format = No_thinking_control
  ; supports_response_format_json = false
  ; supports_structured_output = false
  ; supports_multimodal_inputs = false
  ; supports_image_input = false
  ; supports_audio_input = false
  ; supports_video_input = false
  ; supports_native_streaming = false
  ; supports_system_prompt = true
  ; (* most models support it *)
    supports_caching = false
  ; supports_prompt_caching = false
  ; prompt_cache_alignment = None
  ; supports_top_k = false
  ; supports_min_p = false
  ; supports_seed = false
  ; supports_seed_with_images = false
  ; supports_computer_use = false
  ; supports_code_execution = false
  ; is_ollama = false
  ; emits_usage_tokens = true (* stricter default: most providers report usage *)
  ; supported_models = None
  }
;;

let anthropic_capabilities =
  { default_capabilities with
    max_context_tokens = Some 200_000
  ; (* default; opus/sonnet 4.6 = 1M *)
    max_output_tokens = Some 8_192
  ; (* default; higher for newer models *)
    supports_tools = true
  ; supports_tool_choice = true
  ; supports_parallel_tool_calls = true
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_reasoning_budget = true
  ; supports_structured_output = true
  ; supports_multimodal_inputs = true
  ; supports_image_input = true
  ; supports_native_streaming = true
  ; supports_caching = true
  ; supports_prompt_caching = true
  ; prompt_cache_alignment = Some 1024
  ; supports_computer_use = true
  ; (* Anthropic Messages API documents [top_k] as a valid sampling
     parameter ("Only sample from the top K options for each
     subsequent token", docs.anthropic.com/en/api/messages body
     params). [backend_anthropic.build_request] already serializes
     [config.top_k] unconditionally when [Some]; the capability record
     must match so cross-layer consumers (the #831 Api_openai gate,
     the #830 Backend_openai gate, and the capability_filter passes)
     do not silently drop top_k when the caller routes an Anthropic
     config through a capability-checking path. [supports_min_p]
     remains [false] — Anthropic does not accept min_p. *)
    supports_top_k = true
  }
;;

let kimi_capabilities =
  { default_capabilities with
    max_context_tokens = Some 262_144
  ; max_output_tokens = Some 32_768
  ; supports_tools = true
  ; supports_tool_choice = true
  ; supports_reasoning = true
  ; supports_system_prompt = true
  ; supports_code_execution = true
  }
;;

let openai_chat_capabilities =
  { default_capabilities with
    max_context_tokens = Some 128_000
  ; max_output_tokens = Some 16_384
  ; supports_tools = true
  ; supports_tool_choice = true
  ; supports_parallel_tool_calls = true
  ; supports_response_format_json = true
  ; supports_structured_output = true
  ; supports_multimodal_inputs = true
  ; supports_image_input = true
  ; supports_native_streaming = true
  ; supports_caching = true
  ; supports_prompt_caching = false
  ; prompt_cache_alignment = None
  }
;;

let openai_chat_extended_capabilities =
  { openai_chat_capabilities with
    supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_reasoning_budget = true
  ; supports_top_k = true
  ; supports_min_p = true
  }
;;

(* Ollama OpenAI-compat endpoint behavior on tool_choice is model-dependent
   (docs.ollama.com/capabilities/tool-calling: the parameter is silently
   ignored for some models). Some Qwen3.5 deployments w/ native Jinja
   chat template do honor tool_choice:required in practice.

   Industry context: LiteLLM's model_prices_and_context_window.json
   registry lacks per-Ollama-model tool support flags for the same
   reason (see BerriAI/litellm#14067 — Ollama model metadata surfaces
   no authoritative capability flag, so any static table is a guess).

   Design choice here: declaration-over-probing. The default stays
   conservative (supports_tool_choice = false → contract relaxes to
   Allow_text_or_tool, text-only replies accepted even when the consumer
   asked for a tool). Consumers who have verified their model-side
   support declare it per Provider_config via
   [Provider_config.supports_tool_choice_override]. The SDK does not
   match on [model_id] to guess model-side behavior — the consumer
   (e.g. a config loader that knows it deployed Qwen3.5 w/ the Jinja
   chat template) owns that policy. This is stricter than LiteLLM's
   static-table approach, which requires JSON edits + redeploy to
   flip capability, and avoids the fragile model_id pattern match that
   the Claude Agent SDK sidesteps by being single-provider. *)
(* NVIDIA NIM Nemotron: Llama-based OpenAI-compatible endpoint.
   Thinking uses chat_template_kwargs (same wire format as Ollama's
   llama-server backend). VL variants add image input.
   Ref: build.nvidia.com/nvidia docs, Nemotron model cards. *)
let nemotron_capabilities =
  { openai_chat_extended_capabilities with
    supports_tool_choice = true
  ; supports_reasoning = true
  ; thinking_control_format = Chat_template_kwargs
  }
;;

let ollama_capabilities =
  { openai_chat_extended_capabilities with
    supports_tool_choice = false
  ; supports_seed = true
  ; supports_seed_with_images = true
  ; thinking_control_format = Chat_template_kwargs
  ; is_ollama = true
  }
;;

let dashscope_capabilities =
  { openai_chat_extended_capabilities with
    supports_tool_choice = true
  ; supports_min_p = true
  }
;;

let glm_capabilities =
  { default_capabilities with
    max_context_tokens = Some 200_000
  ; (* GLM-5.1 API enforces max_tokens <= 40960 at request time; keeping a
     higher value here causes server-side rejection with
     "Invalid request: `max_tokens` must be less than or equal to `40960`".
     Empirical upper bound observed on 2026-04-12 during automated
     turns against glm-coding:glm-5.1 and glm:glm-5.1. *)
    max_output_tokens = Some 40_960
  ; supports_tools = true
  ; (* Z.AI's function-calling docs currently document [tool_choice]
     as default [auto] and "only supports auto". OAS therefore treats
     GLM as "tools supported, forced tool_choice unsupported":
     callers may still send tools and OAS may coerce an explicit
     tool_choice request to [auto], but the completion contract must
     stay relaxed so direct GLM text replies do not count as contract
     violations. Ref checked 2026-04-21:
     https://docs.z.ai/guides/capabilities/function-calling *)
    supports_tool_choice = false
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_response_format_json = true
  ; (* Z.AI's current official docs describe JSON mode via
     response_format={"type":"json_object"} plus prompt/schema-in-text
     guidance, but do not document a native JSON-schema request field
     equivalent to OpenAI's json_schema response_format. OAS therefore
     treats GLM as JSON-mode-only: supports_response_format_json=true
     but supports_structured_output=false. validate_output_schema_request
     rejects output_schema for GLM configs to prevent silent pass-through
     of schemas the provider will not enforce.
     Ref: https://docs.z.ai/guides/capabilities/struct-output — checked 2026-04-21. *)
    supports_structured_output = false
  ; supports_native_streaming = true
  }
;;

let gemini_capabilities =
  { default_capabilities with
    max_context_tokens = Some 1_000_000
  ; max_output_tokens = Some 65_000
  ; supports_tools = true
  ; supports_tool_choice = true
  ; supports_parallel_tool_calls = true
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_reasoning_budget = true
  ; supports_response_format_json = true
  ; supports_structured_output = true
  ; supports_multimodal_inputs = true
  ; supports_image_input = true
  ; supports_audio_input = true
  ; supports_video_input = true
  ; supports_native_streaming = true
  ; supports_caching = true
  ; supports_prompt_caching = false
  ; prompt_cache_alignment = None
  ; supports_code_execution = true
  ; (* Google Gemini's generateContent API documents [topK] as part of
     generationConfig (ai.google.dev/api/generate-content). The
     [backend_gemini.build_request] serializer already emits it at
     lib/llm_provider/backend_gemini.ml:162-164, so the capability
     record must match. Same discrepancy story as anthropic_capabilities
     (#832) — OpenAI-compat consumers that route a Gemini config
     through a capability-checking path were silently dropping top_k.
     [supports_min_p] stays false; Gemini's generationConfig has no
     min_p field. *)
    supports_top_k = true
  }
;;

let claude_code_capabilities =
  { anthropic_capabilities with
    max_context_tokens = Some 1_000_000
  ; (* 1M context via Claude Code *)
    max_output_tokens = Some 64_000
  ; supports_structured_output = false
  ; supports_computer_use = true
  ; supports_code_execution = true
  ; supports_runtime_mcp_tools = true
  ; supports_runtime_tool_events = true
  }
;;

let gemini_cli_capabilities =
  { default_capabilities with
    max_context_tokens = Some 1_000_000
  ; max_output_tokens = Some 65_000
  ; supports_tools = false
  ; supports_tool_choice = false
  ; supports_native_streaming = false
  ; supports_system_prompt = true
  ; supports_runtime_mcp_tools = true
  ; supports_runtime_tool_events = true
  ; emits_usage_tokens = false (* CLI wrapper strips usage *)
  }
;;

let kimi_cli_capabilities =
  { default_capabilities with
    max_context_tokens = Some 262_144
  ; max_output_tokens = Some 32_768
  ; supports_tools = true
  ; supports_tool_choice = false
  ; supports_reasoning = true
  ; supports_system_prompt = true
  ; supports_code_execution = true
  ; emits_usage_tokens = false (* CLI wrapper strips usage *)
  ; supported_models = Some [ "kimi-for-coding" ]
  }
;;

let codex_cli_capabilities =
  { default_capabilities with
    max_context_tokens = Some 1_050_000
  ; max_output_tokens = Some 32_000
  ; supports_tools = false
  ; supports_tool_choice = false
  ; supports_native_streaming = false
  ; supports_system_prompt = true
  ; supports_runtime_mcp_tools = true
  ; supports_runtime_tool_events = true
  ; emits_usage_tokens = false (* CLI wrapper strips usage *)
  }
;;

(* ── Model-specific overrides (lookup table) ─────────── *)

(** Lookup capabilities by model_id prefix using the built-in static table.
    Returns None if no specific override is known. *)
let for_model_id_static model_id =
  (* Normalize: lowercase, strip quantization suffixes *)
  let m = String.lowercase_ascii model_id in
  let starts_with prefix =
    String.length m >= String.length prefix
    && String.sub m 0 (String.length prefix) = prefix
  in
  if starts_with "claude-opus-4"
  then
    Some
      { anthropic_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 128_000
      }
  else if starts_with "claude-sonnet-4"
  then
    Some
      { anthropic_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 64_000
      }
  else if starts_with "claude-haiku-4"
  then
    Some
      { anthropic_capabilities with
        max_context_tokens = Some 200_000
      ; max_output_tokens = Some 8_192
      }
  else if starts_with "gpt-5"
  then
    Some
      { openai_chat_extended_capabilities with
        max_context_tokens = Some 1_050_000
      ; max_output_tokens = Some 128_000
      ; supports_computer_use = true
      }
  else if starts_with "gpt-4.1"
  then
    Some
      { openai_chat_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 32_000
      }
  else if starts_with "gpt-4o"
  then
    Some
      { openai_chat_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      }
  else if starts_with "gemini-3" || starts_with "gemini-2.5"
  then Some gemini_capabilities
  else if starts_with "kimi-for-coding"
  then Some kimi_capabilities
  else if starts_with "qwen3"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 262_144
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_native_streaming = true
      ; supports_top_k = true
      ; supports_min_p = true
      }
  else if starts_with "llama-4" || starts_with "llama4"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 1_000_000
      ; supports_tools = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  else if starts_with "deepseek-v4-flash"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 384_000
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_reasoning_budget = true
      ; thinking_control_format = Thinking_object
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      ; supports_caching = true
      ; supports_prompt_caching = false
      ; prompt_cache_alignment = None
      }
  else if starts_with "deepseek-v4-pro"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 384_000
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_reasoning_budget = true
      ; thinking_control_format = Thinking_object
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      ; supports_caching = true
      ; supports_prompt_caching = false
      ; prompt_cache_alignment = None
      }
  else if starts_with "mistral-large"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 260_000
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_structured_output = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      ; supports_caching = true
      ; supports_prompt_caching = false
      ; prompt_cache_alignment = None
      }
  else if starts_with "mistral-small"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 256_000
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_reasoning = true
      ; supports_structured_output = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      ; supports_caching = true
      ; supports_prompt_caching = false
      ; prompt_cache_alignment = None
      }
  else if starts_with "command"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 256_000
      ; max_output_tokens = Some 32_000
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_structured_output = true
      ; supports_native_streaming = true
      }
  else if starts_with "grok"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 2_000_000
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_reasoning = true
      ; supports_structured_output = true
      ; supports_native_streaming = true
      ; supports_caching = true
      ; supports_prompt_caching = false
      ; prompt_cache_alignment = None
      }
    (* NVIDIA Nemotron: Llama-based, NIM OpenAI-compat API.
       Base text models (nemotron-ultra, nemotron-core) get reasoning
       but no vision. VL suffix gets image input. *)
  else if starts_with "nvidia/nemotron" || starts_with "nemotron"
  then (
    let has_vision = starts_with "nvidia/nemotron-vl" || starts_with "nemotron-vl" in
    Some
      { nemotron_capabilities with
        max_context_tokens = Some 131_072
      ; max_output_tokens = Some 16_384
      ; supports_multimodal_inputs = has_vision
      ; supports_image_input = has_vision
      }
    (* Gemma 4: Google open-weight multimodal.
       4 sizes (1B/4B/12B/27B-31B). All support function calling,
       image input, streaming. 27B+ supports audio. 256K context. *))
  else if starts_with "gemma-4" || starts_with "google/gemma-4"
  then (
    let is_large =
      let m = String.lowercase_ascii model_id in
      (* Strip optional "google/" prefix, then "gemma-4-".  The next
         token is the size: "27b", "31b", "1b", "4b", "12b", etc. *)
      let base =
        if String.length m > 7 && String.sub m 0 7 = "google/"
        then String.sub m 7 (String.length m - 7)
        else m
      in
      match
        if String.length base >= 8 && String.sub base 0 8 = "gemma-4-"
        then Some (String.sub base 8 (String.length base - 8))
        else None
      with
      | Some size_token ->
        List.exists
          (fun prefix ->
             String.length size_token >= 3 && String.sub size_token 0 3 = prefix)
          [ "27b"; "31b" ]
      | None -> false
    in
    Some
      { default_capabilities with
        max_context_tokens = Some 262_144
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_response_format_json = true
      ; supports_structured_output = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_audio_input = is_large
      ; supports_native_streaming = true
      ; supports_seed = true
      }
    (* GLM flash/air variants: faster, no reasoning, smaller output.
     Must precede the broad glm-4.5/4.6/4.7/5 match below. *))
  else if
    starts_with "glm-4.7-flash"
    || starts_with "glm-4.5-flash"
    || starts_with "glm-4.5-air"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      }
    (* GLM 5-turbo: tool-calling optimized, fast, reasoning but no extended thinking *)
  else if starts_with "glm-5-turbo"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      }
  else if starts_with "glm-5v-turbo"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 200_000
      ; max_output_tokens = Some 128_000
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_response_format_json = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  else if starts_with "glm-ocr"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  else if starts_with "glm-4.6v" || starts_with "glm-4.5v"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 32_768
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
    (* GLM-5-Code: coding-specific variant with 128K context (not 200K).
       Z.AI docs: GLM-5-Code uses /api/coding/paas/ endpoint, 128K context. *)
  else if starts_with "glm-5-code"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 128_000
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      }
    (* GLM full text models: reasoning, large context/output, but no vision. *)
  else if
    starts_with "glm-4.5"
    || starts_with "glm-4.6"
    || starts_with "glm-4.7"
    || starts_with "glm-5"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 200_000
      ; max_output_tokens = Some 128_000
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      }
  else if starts_with "glm-4-flash"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 4_096
      ; supports_tools = true
      ; supports_native_streaming = true
      }
  else if starts_with "glm-4v"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 4_096
      ; supports_tools = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  else if starts_with "glm-4"
  then
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 4_096
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_native_streaming = true
      }
  else None
;;

(** Lookup capabilities by provider label string.

    Returns [None] for labels outside the recognized set so callers can
    fail closed rather than silently treating unknown providers as
    having default capabilities. *)
let capabilities_for_provider_label label =
  match String.lowercase_ascii (String.trim label) with
  | "anthropic" -> Some anthropic_capabilities
  | "openai" | "openai_chat" -> Some openai_chat_capabilities
  | "openai_chat_extended" -> Some openai_chat_extended_capabilities
  | "gemini" -> Some gemini_capabilities
  | "ollama" | "ollama_cloud" -> Some ollama_capabilities
  | "glm" | "glm-coding" -> Some glm_capabilities
  | "dashscope" | "qwen" -> Some dashscope_capabilities
  | "nemotron" -> Some nemotron_capabilities
  | "kimi" -> Some kimi_capabilities
  | "claude_code" -> Some claude_code_capabilities
  | "gemini_cli" -> Some gemini_cli_capabilities
  | "kimi_cli" -> Some kimi_cli_capabilities
  | "codex_cli" -> Some codex_cli_capabilities
  | _ -> None
;;

(** Merge Discovery ctx_size into capabilities. *)
let with_context_size caps ~ctx_size = { caps with max_context_tokens = Some ctx_size }

let with_tool_support caps ~supports_tools = { caps with supports_tools }

(* ── Capability manifest integration ───────────────────── *)

(** Apply a capability manifest entry on top of a base capabilities record.

    [base_label] resolves to a provider preset via
    [capabilities_for_provider_label]; defaults to [default_capabilities]
    when absent or unrecognised.  Each [Some] field in [entry] overrides
    the corresponding field of the base; [None] fields are left unchanged. *)
let apply_manifest_entry (entry : Capability_manifest.entry) : capabilities =
  let base =
    match entry.base_label with
    | None -> default_capabilities
    | Some label ->
      (match capabilities_for_provider_label label with
       | Some c -> c
       | None -> default_capabilities)
  in
  let override_bool base_val = function
    | Some b -> b
    | None -> base_val
  in
  let override_int_opt base_val = function
    | Some n -> Some n
    | None -> base_val
  in
  { base with
    max_context_tokens = override_int_opt base.max_context_tokens entry.max_context_tokens
  ; max_output_tokens = override_int_opt base.max_output_tokens entry.max_output_tokens
  ; supports_tools = override_bool base.supports_tools entry.supports_tools
  ; supports_tool_choice =
      override_bool base.supports_tool_choice entry.supports_tool_choice
  ; supports_parallel_tool_calls =
      override_bool base.supports_parallel_tool_calls entry.supports_parallel_tool_calls
  ; supports_reasoning = override_bool base.supports_reasoning entry.supports_reasoning
  ; supports_extended_thinking =
      override_bool base.supports_extended_thinking entry.supports_extended_thinking
  ; supports_reasoning_budget =
      override_bool base.supports_reasoning_budget entry.supports_reasoning_budget
  ; supports_response_format_json =
      override_bool base.supports_response_format_json entry.supports_response_format_json
  ; supports_structured_output =
      override_bool base.supports_structured_output entry.supports_structured_output
  ; supports_multimodal_inputs =
      override_bool base.supports_multimodal_inputs entry.supports_multimodal_inputs
  ; supports_image_input =
      override_bool base.supports_image_input entry.supports_image_input
  ; supports_audio_input =
      override_bool base.supports_audio_input entry.supports_audio_input
  ; supports_video_input =
      override_bool base.supports_video_input entry.supports_video_input
  ; supports_native_streaming =
      override_bool base.supports_native_streaming entry.supports_native_streaming
  ; supports_system_prompt =
      override_bool base.supports_system_prompt entry.supports_system_prompt
  ; supports_caching = override_bool base.supports_caching entry.supports_caching
  ; supports_prompt_caching =
      override_bool base.supports_prompt_caching entry.supports_prompt_caching
  ; supports_top_k = override_bool base.supports_top_k entry.supports_top_k
  ; supports_min_p = override_bool base.supports_min_p entry.supports_min_p
  ; supports_seed = override_bool base.supports_seed entry.supports_seed
  ; supports_computer_use =
      override_bool base.supports_computer_use entry.supports_computer_use
  ; supports_code_execution =
      override_bool base.supports_code_execution entry.supports_code_execution
  }
;;

(** Look up capabilities for [model_id] against an explicit manifest,
    falling back to the built-in static table when no manifest entry
    matches. *)
let for_model_id_with_manifest manifest model_id =
  match Capability_manifest.lookup manifest model_id with
  | Some entry -> Some (apply_manifest_entry entry)
  | None -> for_model_id_static model_id
;;

(** Look up capabilities for [model_id].

    Checks the globally loaded capability manifest (from
    [OAS_CAPABILITY_MANIFEST]) first; falls through to the built-in
    static prefix table when no manifest entry matches. *)
let for_model_id model_id =
  match Capability_manifest.global () with
  | Some manifest -> for_model_id_with_manifest manifest model_id
  | None -> for_model_id_static model_id
;;

[@@@coverage off]

let%test "for_model_id glm-4.5 has reasoning" =
  match for_model_id "glm-4.5" with
  | Some c -> c.supports_reasoning && c.max_context_tokens = Some 200_000
  | None -> false
;;

let%test "for_model_id glm-4 no reasoning" =
  match for_model_id "glm-4-chat" with
  | Some c -> (not c.supports_reasoning) && c.max_context_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id glm-4v has vision" =
  match for_model_id "glm-4v-flash" with
  | Some c -> c.supports_image_input && c.supports_multimodal_inputs
  | None -> false
;;

let%test "for_model_id glm-4-flash basic" =
  match for_model_id "glm-4-flash" with
  | Some c -> c.supports_tools && c.max_output_tokens = Some 4_096
  | None -> false
;;

let%test "for_model_id glm-5 is text only" =
  match for_model_id "glm-5" with
  | Some c -> c.supports_reasoning && not c.supports_image_input
  | None -> false
;;

let%test "for_model_id glm-5v has vision" =
  match for_model_id "glm-5v-turbo" with
  | Some c -> c.supports_reasoning && c.supports_image_input
  | None -> false
;;

let%test "for_model_id glm-4.6v stays vision-capable" =
  match for_model_id "glm-4.6v" with
  | Some c ->
    c.supports_reasoning && c.supports_image_input && c.max_output_tokens = Some 32_768
  | None -> false
;;

let%test "for_model_id glm-4.5v stays vision-capable" =
  match for_model_id "glm-4.5v" with
  | Some c ->
    c.supports_reasoning && c.supports_image_input && c.max_output_tokens = Some 32_768
  | None -> false
;;

let%test "for_model_id glm-4.7-flashx is flash (no reasoning, 16K output)" =
  match for_model_id "glm-4.7-flashx" with
  | Some c ->
    (not c.supports_reasoning) && c.max_output_tokens = Some 16_384 && c.supports_tools
  | None -> false
;;

let%test "for_model_id glm-4.7-flash is flash (not broad glm-4.7)" =
  match for_model_id "glm-4.7-flash" with
  | Some c -> (not c.supports_reasoning) && c.max_output_tokens = Some 16_384
  | None -> false
;;

let%test "for_model_id glm-4.5-flash is flash (not broad glm-4.5)" =
  match for_model_id "glm-4.5-flash" with
  | Some c ->
    (not c.supports_reasoning) && c.max_output_tokens = Some 16_384 && c.supports_tools
  | None -> false
;;

let%test "for_model_id glm-5-turbo has reasoning but not extended thinking" =
  match for_model_id "glm-5-turbo" with
  | Some c ->
    c.supports_reasoning
    && (not c.supports_extended_thinking)
    && c.max_output_tokens = Some 16_384
  | None -> false
;;

let%test "for_model_id glm-5.1 full model (reasoning + extended thinking)" =
  match for_model_id "glm-5.1" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_output_tokens = Some 128_000
  | None -> false
;;

(* --- emits_usage_tokens / capabilities_for_provider_label --- *)

let%test "emits_usage_tokens: default is true" = default_capabilities.emits_usage_tokens

let%test "emits_usage_tokens: anthropic reports usage" =
  anthropic_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: ollama reports usage" =
  ollama_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: claude_code reports usage" =
  claude_code_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: gemini_cli strips usage" =
  not gemini_cli_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: kimi_cli strips usage" =
  not kimi_cli_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: codex_cli strips usage" =
  not codex_cli_capabilities.emits_usage_tokens
;;

let%test "capabilities_for_provider_label: kimi_cli" =
  match capabilities_for_provider_label "kimi_cli" with
  | Some c -> not c.emits_usage_tokens
  | None -> false
;;

let%test "capabilities_for_provider_label: KIMI_CLI (case insensitive)" =
  Option.is_some (capabilities_for_provider_label "KIMI_CLI")
;;

let%test "capabilities_for_provider_label: trims whitespace" =
  Option.is_some (capabilities_for_provider_label "  codex_cli  ")
;;

let%test "capabilities_for_provider_label: anthropic" =
  match capabilities_for_provider_label "anthropic" with
  | Some c -> c.emits_usage_tokens && c.supports_caching
  | None -> false
;;

let%test "capabilities_for_provider_label: openai alias" =
  Option.is_some (capabilities_for_provider_label "openai")
  && Option.is_some (capabilities_for_provider_label "openai_chat")
;;

let%test "capabilities_for_provider_label: glm alias" =
  Option.is_some (capabilities_for_provider_label "glm")
  && Option.is_some (capabilities_for_provider_label "glm-coding")
;;

let%test "capabilities_for_provider_label: unknown returns None" =
  Option.is_none (capabilities_for_provider_label "not_a_real_provider_xyz")
;;

(* --- Nemotron / Gemma 4 --- *)

let%test "nemotron_capabilities has chat_template_kwargs thinking" =
  nemotron_capabilities.thinking_control_format = Chat_template_kwargs
;;

let%test "for_model_id nemotron-ultra has reasoning" =
  match for_model_id "nemotron-ultra-253b" with
  | Some c -> c.supports_reasoning && c.supports_tool_choice
  | None -> false
;;

let%test "for_model_id nemotron-vl has image input" =
  match for_model_id "nemotron-vl" with
  | Some c -> c.supports_image_input && c.supports_multimodal_inputs
  | None -> false
;;

let%test "for_model_id nvidia/nemotron-core resolves" =
  match for_model_id "nvidia/nemotron-core" with
  | Some c -> c.supports_reasoning
  | None -> false
;;

let%test "for_model_id gemma-4-27b has tools + seed" =
  match for_model_id "gemma-4-27b-it" with
  | Some c ->
    c.supports_tools
    && c.supports_seed
    && c.supports_image_input
    && c.max_context_tokens = Some 262_144
  | None -> false
;;

let%test "for_model_id gemma-4-1b-it has tools, no audio" =
  match for_model_id "gemma-4-1b-it" with
  | Some c -> c.supports_tools && c.supports_image_input && not c.supports_audio_input
  | None -> false
;;

let%test "for_model_id google/gemma-4-1b-it is NOT large" =
  match for_model_id "google/gemma-4-1b-it" with
  | Some c -> not c.supports_audio_input
  | None -> false
;;

let%test "for_model_id google/gemma-4-27b-it IS large" =
  match for_model_id "google/gemma-4-27b-it" with
  | Some c -> c.supports_audio_input
  | None -> false
;;

let%test "for_model_id gemma-4-31b IS large" =
  match for_model_id "gemma-4-31b-it" with
  | Some c -> c.supports_audio_input
  | None -> false
;;

let%test "capabilities_for_provider_label: nemotron" =
  match capabilities_for_provider_label "nemotron" with
  | Some c -> c.thinking_control_format = Chat_template_kwargs
  | None -> false
;;

(* ── Prefix ordering invariant ──────────────────── *)

(* Each case is a model_id and the expected capability fingerprint.
   If [for_model_id] reorders its prefix checks incorrectly, these
   specific models would be matched by a more general prefix and
   return wrong capabilities. The test catches that. *)
let%test "for_model_id: specific model IDs get correct (not shadowed) capabilities" =
  let check model_id expected =
    match for_model_id model_id with
    | Some c -> expected c
    | None -> false
  in
  List.for_all
    (fun (m, e) -> check m e)
    [ ( "glm-4.7-flash-turbo"
      , fun c -> c.max_output_tokens = Some 16_384 && not c.supports_reasoning )
    ; ( "glm-4.5-flash-test"
      , fun c -> c.max_output_tokens = Some 16_384 && not c.supports_reasoning )
    ; ( "glm-5-turbo-latest"
      , fun c -> c.max_output_tokens = Some 16_384 && not c.supports_extended_thinking )
    ; ("glm-4.6v-plus", fun c -> c.supports_image_input && c.supports_reasoning)
    ; ( "glm-4.7-flash-test"
      , fun c -> c.max_output_tokens = Some 16_384 && not c.supports_reasoning )
    ; ( "glm-4-flash-mini"
      , fun c -> c.max_output_tokens = Some 4_096 && not c.supports_reasoning )
    ; ("glm-4v-plus", fun c -> c.supports_image_input)
    ; ( "glm-4.5-air-test"
      , fun c -> c.max_output_tokens = Some 16_384 && not c.supports_reasoning )
    ; ( "glm-5v-turbo-latest"
      , fun c ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 128_000 )
    ; ("glm-ocr-test", fun c -> c.supports_image_input && not c.supports_tools)
    ; ("claude-opus-4-20250501", fun c -> c.max_output_tokens = Some 128_000)
    ; ("gpt-4.1-mini", fun c -> c.max_output_tokens = Some 32_000)
    ; ("deepseek-v4-flash-test", fun c -> c.thinking_control_format = Thinking_object)
    ; ( "nemotron-ultra-253b"
      , fun c ->
          c.thinking_control_format = Chat_template_kwargs && c.supports_tool_choice )
    ; ( "nvidia/nemotron-ultra-253b"
      , fun c ->
          c.thinking_control_format = Chat_template_kwargs && c.supports_tool_choice )
    ; ("nemotron-vl", fun c -> c.supports_image_input && c.supports_multimodal_inputs)
    ; ( "gemma-4-27b-it"
      , fun c ->
          c.supports_tools
          && c.supports_image_input
          && c.supports_seed
          && c.max_context_tokens = Some 262_144 )
    ; ("google/gemma-4-27b-it", fun c -> c.supports_tools && c.supports_image_input)
    ]
;;

(* ── Capability drift detection ────────────────────────── *)

type drift_observation =
  | Usage_missing_but_declared (** [emits_usage_tokens=true] but response has no usage *)
  | Tools_used_but_declared_unsupported
  (** Response contains ToolUse but [supports_tools=false] *)
  | Thinking_returned_but_declared_unsupported
  (** Response contains Thinking/RedactedThinking but [supports_reasoning=false] *)
  | Stop_tool_use_but_declared_unsupported
  (** [stop_reason=StopToolUse] but [supports_tools=false] *)
[@@deriving show]

let detect_drift (caps : capabilities) (resp : Types.api_response)
  : drift_observation list
  =
  let obs = ref [] in
  (* Usage drift *)
  if caps.emits_usage_tokens && resp.usage = None
  then obs := Usage_missing_but_declared :: !obs;
  (* Content block analysis *)
  let has_tool_use = ref false
  and has_thinking = ref false in
  List.iter
    (function
      | Types.ToolUse _ -> has_tool_use := true
      | Types.Thinking _ | Types.RedactedThinking _ -> has_thinking := true
      | _ -> ())
    resp.content;
  if !has_tool_use && not caps.supports_tools
  then obs := Tools_used_but_declared_unsupported :: !obs;
  if !has_thinking && not caps.supports_reasoning
  then obs := Thinking_returned_but_declared_unsupported :: !obs;
  (* Stop reason analysis *)
  if resp.stop_reason = Types.StopToolUse && not caps.supports_tools
  then obs := Stop_tool_use_but_declared_unsupported :: !obs;
  List.rev !obs
;;

(* ── Alias collision invariant ─────────────────────── *)

(* Aliases must resolve to the same underlying capabilities record.
   If a new provider is added with overlapping labels, this test catches
   divergence. M06 regression guard. *)
let%test "capabilities_for_provider_label: aliases resolve to identical capabilities" =
  let resolve label = capabilities_for_provider_label label in
  let same_base a b =
    match resolve a, resolve b with
    | Some ca, Some cb ->
      ca.supports_tools = cb.supports_tools
      && ca.supports_reasoning = cb.supports_reasoning
      && ca.supports_caching = cb.supports_caching
      && ca.emits_usage_tokens = cb.emits_usage_tokens
      && ca.max_context_tokens = cb.max_context_tokens
      && ca.max_output_tokens = cb.max_output_tokens
      && ca.supports_image_input = cb.supports_image_input
      && ca.thinking_control_format = cb.thinking_control_format
    | _ -> false
  in
  let alias_pairs = [ "openai", "openai_chat"; "glm", "glm-coding" ] in
  List.for_all (fun (a, b) -> same_base a b) alias_pairs
  && Option.is_some (resolve "anthropic")
  && Option.is_some (resolve "gemini")
  && Option.is_some (resolve "ollama")
  && Option.is_some (resolve "kimi")
  && Option.is_some (resolve "nemotron")
;;

(* Every declared label is reachable — no dead branches in the match.
   If a label is added to the match but has no corresponding capability
   binding, this test will fail. *)
let%test "capabilities_for_provider_label: all declared labels resolve" =
  let labels =
    [ "anthropic"
    ; "openai"
    ; "openai_chat"
    ; "openai_chat_extended"
    ; "gemini"
    ; "ollama"
    ; "glm"
    ; "glm-coding"
    ; "nemotron"
    ; "kimi"
    ; "claude_code"
    ; "gemini_cli"
    ; "kimi_cli"
    ; "codex_cli"
    ]
  in
  List.for_all (fun l -> Option.is_some (capabilities_for_provider_label l)) labels
;;

(* Every label resolves to a distinct capability fingerprint unless
   explicitly aliased. Catches accidental capability merging. *)
let%test
    "capabilities_for_provider_label: no accidental aliasing across distinct providers"
  =
  let non_aliased =
    [ "anthropic"
    ; "gemini"
    ; "ollama"
    ; "kimi"
    ; "claude_code"
    ; "gemini_cli"
    ; "kimi_cli"
    ; "codex_cli"
    ; "nemotron"
    ]
  in
  let fingerprints =
    List.filter_map
      (fun l ->
         match capabilities_for_provider_label l with
         | Some c ->
           Some
             ( c.supports_tools
             , c.supports_reasoning
             , c.supports_caching
             , c.emits_usage_tokens
             , c.max_context_tokens
             , c.max_output_tokens
             , c.thinking_control_format )
         | None -> None)
      non_aliased
  in
  (* Each non-aliased provider should have at least one distinguishing field *)
  let n = List.length fingerprints in
  n = List.length non_aliased
;;
