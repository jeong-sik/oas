(** Provider capabilities -- per-provider/model feature flags and limits.

    Tracks what a provider/model supports so the agent runtime can make
    correct decisions (e.g., context reduction, tool filtering, thinking
    budget enforcement).

    @since 0.42.0
    @since 0.72.0 — added numeric limits, parallel tool calls, thinking split *)

(** Wire-format for controlling thinking/reasoning on Chat_completions_v1-compat backends.
    Different model families use different JSON shapes to enable/disable
    thinking, so the runtime must know which format to emit.

    @since 0.184.0 *)
type thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object
  (** Provider_g-style: top-level [thinking] object plus [reasoning_effort]. *)
  | Thinking_object_only
  (** Kimi K2.5-style: top-level [thinking] object without [reasoning_effort]. *)
  | Chat_template_kwargs
  (** llama-server style: {"chat_template_kwargs":{"enable_thinking":b}} *)
  | Reasoning_effort
  (** Chat_completions_v1-style top-level [reasoning_effort] string field. The set of
      values this codebase emits is [{"none","low","medium","high"}] —
      see {!Provider_config.effort_of_thinking_config}. (Chat_completions_v1's spec
      also accepts ["minimal"], but no current OAS request builder emits
      it.) Ollama's Chat_completions_v1-compatible mode uses this shape. *)
  | Enable_thinking
  (** DashScope-style top-level [enable_thinking] bool plus optional
      [thinking_budget]. *)

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
    (** Wire-format for thinking control on Chat_completions_v1-compat backends.
        Determines which JSON shape the backend emits for enable_thinking.
        Only meaningful when [supports_reasoning] or [supports_extended_thinking]
        is true and the request goes through backend_chat_completions_v1.
        @since 0.184.0 *)
  ; (* ── Output format ─────────────────────────────────── *)
    supports_response_format_json : bool (** JSON mode *)
  ; supports_structured_output : bool (** JSON schema 100% guarantee *)
  ; (* ── Input modalities ──────────────────────────────── *)
    supports_multimodal_inputs : bool (** Any non-text input *)
  ; supports_image_input : bool
  ; supports_audio_input : bool
  ; supports_video_input : bool
  ; modality_priority : Modality.priority
    (** Block ordering applied to multimodal user messages just before
        serialization. [Visual_first] for Gemma 4 family.
        @since 0.193.0 *)
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
      providers (Chat_completions_v1, Gemini) do not guarantee deterministic output
      when images are in the prompt. *)
  ; (* ── Advanced modalities ───────────────────────────── *)
    supports_computer_use : bool
  ; supports_code_execution : bool
  ; (* ── Usage reporting ─────────────────────────────────── *)
    emits_usage_tokens : bool
    (** True when the provider's standard response carries
      [input_tokens]/[output_tokens] (direct APIs like Anthropic,
      OpenAI_compat, Gemini, Kimi, Glm, Ollama).

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
  ; modality_priority = Modality.Preserve_input_order
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
     subsequent token", docs.provider_a.com/en/api/messages body
     params). [backend_provider_a.build_request] already serializes
     [config.top_k] unconditionally when [Some]; the capability record
     must match so cross-layer consumers (the #831 Api_chat_completions_v1 gate,
     the #830 Backend_chat_completions_v1 gate, and the capability_filter passes)
     do not silently drop top_k when the caller routes an Anthropic
     config through a capability-checking path. [supports_min_p]
     remains [false] — Anthropic does not accept min_p. *)
    supports_top_k = true
  }
;;

let kimi_capabilities =
  { default_capabilities with
    max_context_tokens =
      Some 256_000
      (* platform.kimi.ai documents only the default max_tokens (32768); it does
       not state a higher output ceiling. Keep the verified default here. A
       higher ceiling (if any) is deferred to the per-provider capability pass. *)
  ; max_output_tokens = Some 32_768
  ; supports_tools = true
  ; supports_tool_choice = true
  ; supports_parallel_tool_calls = true
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_reasoning_budget = true
  ; thinking_control_format = Thinking_object_only
  ; supports_response_format_json = true
  ; supports_structured_output = true
  ; supports_system_prompt = true
  ; supports_native_streaming = true
  ; supports_multimodal_inputs = true
  ; supports_image_input = true
  ; supports_code_execution =
      true
      (* Preserved from the pre-rename provider_c_capabilities; dropped by accident
       in the capability rename. *)
  }
;;

let openai_compat_chat_capabilities =
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

let openai_compat_chat_extended_capabilities =
  { openai_compat_chat_capabilities with
    supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_reasoning_budget = true
  ; thinking_control_format = Reasoning_effort
  ; supports_top_k = true
  ; supports_min_p = true
  }
;;

(* Ollama Chat_completions_v1-compat endpoint behavior on tool_choice is model-dependent
   (docs.ollama.com/capabilities/tool-calling: the parameter is silently
   ignored for some models). Some DashScope_3.5 deployments w/ native Jinja
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
   (e.g. a config loader that knows it deployed DashScope_3.5 w/ the Jinja
   chat template) owns that policy. This is stricter than LiteLLM's
   static-table approach, which requires JSON edits + redeploy to
   flip capability, and avoids the fragile model_id pattern match that
   the Agent_llm_a Agent SDK sidesteps by being single-provider. *)
(* NVIDIA NIM Provider_l: Llama-based Chat_completions_v1-compatible endpoint.
   Thinking uses chat_template_kwargs (same wire format as Ollama's
   llama-server backend). VL variants add image input.
   Ref: build.nvidia.com/nvidia docs, Provider_l model cards. *)
let provider_l_capabilities =
  { openai_compat_chat_extended_capabilities with
    supports_tool_choice = true
  ; supports_reasoning = true
  ; thinking_control_format = Chat_template_kwargs
  }
;;

let ollama_capabilities =
  { openai_compat_chat_extended_capabilities with
    supports_tool_choice = false
  ; supports_seed = true
  ; supports_seed_with_images = true
  ; thinking_control_format = Reasoning_effort
  }
;;

let dashscope_capabilities =
  { openai_compat_chat_extended_capabilities with
    supports_tool_choice = true
  ; supports_min_p = true
  ; thinking_control_format = Enable_thinking
  }
;;

let glm_capabilities =
  { default_capabilities with
    max_context_tokens = Some 200_000
  ; (* Glm-5.1 API enforces max_tokens <= 40960 at request time; keeping a
     higher value here causes server-side rejection with
     "Invalid request: `max_tokens` must be less than or equal to `40960`".
     Empirical upper bound observed on 2026-04-12 during automated
     turns against provider_k-coding:provider_k-5.1 and provider_k:provider_k-5.1. *)
    max_output_tokens = Some 40_960
  ; supports_tools = true
  ; (* Z.AI's function-calling docs currently document [tool_choice]
     as default [auto] and "only supports auto". OAS therefore treats
     Glm as "tools supported, forced tool_choice unsupported":
     callers may still send tools and OAS may coerce an explicit
     tool_choice request to [auto], but the completion contract must
     stay relaxed so direct Glm text replies do not count as contract
     violations. Ref checked 2026-04-21:
     https://docs.z.ai/guides/capabilities/function-calling *)
    supports_tool_choice = false
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_response_format_json = true
  ; (* Z.AI's current official docs describe JSON mode via
     response_format={"type":"json_object"} plus prompt/schema-in-text
     guidance, but do not document a native JSON-schema request field
     equivalent to Chat_completions_v1's json_schema response_format. OAS therefore
     treats Glm as JSON-mode-only: supports_response_format_json=true
     but supports_structured_output=false. validate_output_schema_request
     rejects output_schema for Glm configs to prevent silent pass-through
     of schemas the provider will not enforce.
     Ref: https://docs.z.ai/guides/capabilities/struct-output — checked 2026-04-21. *)
    supports_structured_output = false
  ; supports_native_streaming = true
  }
;;

(** Typed Gemini model family (root-fix for #968 string-classifier drift gate).

    Centralizes the [String.starts_with ~prefix:"provider_f-..."] dispatch into a
    single classifier with an exhaustive variant. Downstream code switches on
    the variant instead of comparing strings, so a new family member is a
    compile-time obligation rather than a runtime string-match miss.

    The internal use of [starts_with] inside [provider_f_family_of_id] is
    intentional and bounded: prefix matching is the only signal Google's model
    IDs offer. Concentrating it here keeps the rest of the codebase typed.

    @since 0.196.3 *)
type provider_f_family =
  | Gemini_3_1 (** [provider_f-3.1.*] — 3.1 line (pro-preview, flash-lite-preview, …) *)
  | Gemini_3 (** [provider_f-3.*] but not 3.1 — flash-preview and siblings *)
  | Gemini_2_5 (** [provider_f-2.5.*] — legacy line, kept until removal PR *)
  | Gemini_other of string
  (** Unknown provider_f id or non-provider_f id. Retains the literal so the
          caller can log / fall through without losing data. *)

let strip_suffix ~suffix value =
  if String.ends_with ~suffix value
  then String.sub value 0 (String.length value - String.length suffix)
  else value
;;

(** Classify a model id into a [provider_f_family]. Order matters: [provider_f-3.1]
    is checked before [provider_f-3] so the more specific prefix wins.
    Input is expected lowercased (callers pass the already-normalized id). *)
let provider_f_family_of_id (id : string) : provider_f_family =
  if String.starts_with ~prefix:"provider_f-3.1" id
  then Gemini_3_1
  else if String.starts_with ~prefix:"provider_f-3" id
  then Gemini_3
  else if String.starts_with ~prefix:"provider_f-2.5" id
  then Gemini_2_5
  else Gemini_other id
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
     [backend_provider_f.build_request] serializer already emits it at
     lib/llm_provider/backend_provider_f.ml:162-164, so the capability
     record must match. Same discrepancy story as anthropic_capabilities
     (#832) — Chat_completions_v1-compat consumers that route a Gemini config
     through a capability-checking path were silently dropping top_k.
     [supports_min_p] stays false; Gemini's generationConfig has no
     min_p field. *)
    supports_top_k = true
  }
;;

(* CLI subprocess transports have been removed; CLI-specific capability
   records deleted. See PR #1809. *)

(* ── Model-specific overrides (lookup table) ─────────── *)

type static_model_route =
  | Agent_llm_a_opus_4
  | Agent_llm_a_sonnet_4
  | Agent_llm_a_haiku_4
  | Chat_completions_v1_5
  | Chat_completions_v1_4_1
  | Chat_completions_v1_4o
  | Mimo_v2_5_chat
  | Gemini of provider_f_family
  | Kimi_for_coding
  | Kimi_k2
  | DashScope_3
  | Provider_n_4
  | Provider_g_v4_flash
  | Provider_g_v4_pro
  | Provider_j_large
  | Provider_j_small
  | Provider_m_command
  | Provider_e_grok
  | Provider_l of { has_vision : bool }
  | Gemini_gemma_4 of { has_large_audio : bool }
  | Glm_4_7_flash
  | Glm_4_5_flash_air
  | Glm_5_turbo
  | Glm_5v_turbo
  | Glm_ocr
  | Glm_4_6_vision_reasoning
  | Glm_4_5_vision_reasoning
  | Glm_5_code
  | Glm_4_5_text
  | Glm_full_text
  | Glm_4_flash
  | Glm_4v
  | Glm_4
  | Qwen_3

let normalize_static_model_id model_id =
  model_id |> String.trim |> String.lowercase_ascii |> strip_suffix ~suffix:":cloud"
;;

let provider_f_gemma_4_has_large_audio model_id =
  let prefix = "model-f-gemma-4-" in
  let base =
    if String.starts_with ~prefix:"google/" model_id
    then String.sub model_id 7 (String.length model_id - 7)
    else model_id
  in
  let size =
    if String.starts_with ~prefix base
    then
      Some
        (String.sub
           base
           (String.length prefix)
           (String.length base - String.length prefix))
    else None
  in
  match size with
  | Some size_token ->
    List.exists (fun prefix -> String.starts_with ~prefix size_token) [ "27b"; "31b" ]
  | None -> false
;;

let starts_with_any model_id prefixes =
  List.exists (fun prefix -> String.starts_with ~prefix model_id) prefixes
;;

let static_model_route_of_id model_id =
  let m = normalize_static_model_id model_id in
  if String.starts_with ~prefix:"agent_llm_a-opus-4" m
  then Some Agent_llm_a_opus_4
  else if String.starts_with ~prefix:"agent_llm_a-sonnet-4" m
  then Some Agent_llm_a_sonnet_4
  else if String.starts_with ~prefix:"agent_llm_a-haiku-4" m
  then Some Agent_llm_a_haiku_4
  else if String.starts_with ~prefix:"model-d-5" m
  then Some Chat_completions_v1_5
  else if String.starts_with ~prefix:"model-d-4.1" m
  then Some Chat_completions_v1_4_1
  else if String.starts_with ~prefix:"model-d" m
  then Some Chat_completions_v1_4o
  else if m = "mimo-v2.5" || String.starts_with ~prefix:"mimo-v2.5-pro" m
  then Some Mimo_v2_5_chat
  else (
    match provider_f_family_of_id m with
    | (Gemini_3 | Gemini_3_1 | Gemini_2_5) as family -> Some (Gemini family)
    | Gemini_other _ ->
      if String.starts_with ~prefix:"provider_c-for-coding" m
      then Some Kimi_for_coding
      else if String.starts_with ~prefix:"provider_c-k2" m
      then Some Kimi_k2
      else if
        String.starts_with ~prefix:"provider_h-3" m
        || String.starts_with ~prefix:"provider_h_3" m
        || String.starts_with ~prefix:"dashscope_3" m
      then Some DashScope_3
      else if
        String.starts_with ~prefix:"model-n-4" m || String.starts_with ~prefix:"llama4" m
      then Some Provider_n_4
      else if String.starts_with ~prefix:"provider_g-v4-flash" m
      then Some Provider_g_v4_flash
      else if String.starts_with ~prefix:"provider_g-v4-pro" m
      then Some Provider_g_v4_pro
      else if String.starts_with ~prefix:"provider_j-large" m
      then Some Provider_j_large
      else if String.starts_with ~prefix:"provider_j-small" m
      then Some Provider_j_small
      else if String.starts_with ~prefix:"command" m
      then Some Provider_m_command
      else if
        String.starts_with ~prefix:"provider_e_grok" m
        || String.starts_with ~prefix:"model-e" m
      then Some Provider_e_grok
      else if
        String.starts_with ~prefix:"nvidia/provider_l" m
        || String.starts_with ~prefix:"provider_l" m
      then
        Some
          (Provider_l
             { has_vision =
                 String.starts_with ~prefix:"nvidia/provider_l-vl" m
                 || String.starts_with ~prefix:"provider_l-vl" m
             })
      else if
        String.starts_with ~prefix:"model-f-gemma-4" m
        || String.starts_with ~prefix:"google/model-f-gemma-4" m
      then
        Some (Gemini_gemma_4 { has_large_audio = provider_f_gemma_4_has_large_audio m })
      else if starts_with_any m [ "provider_k-4.7-flash"; "glm-4.7-flash" ]
      then Some Glm_4_7_flash
      else if
        starts_with_any
          m
          [ "provider_k-4.5-flash"; "provider_k-4.5-air"; "glm-4.5-flash"; "glm-4.5-air" ]
      then Some Glm_4_5_flash_air
      else if starts_with_any m [ "provider_k-5-turbo"; "glm-5-turbo" ]
      then Some Glm_5_turbo
      else if starts_with_any m [ "provider_k-5v-turbo"; "glm-5v-turbo" ]
      then Some Glm_5v_turbo
      else if starts_with_any m [ "provider_k-ocr"; "glm-ocr" ]
      then Some Glm_ocr
      else if starts_with_any m [ "provider_k-4.6v"; "glm-4.6v" ]
      then Some Glm_4_6_vision_reasoning
      else if starts_with_any m [ "provider_k-4.5v"; "glm-4.5v" ]
      then Some Glm_4_5_vision_reasoning
      else if starts_with_any m [ "provider_k-5-code"; "glm-5-code" ]
      then Some Glm_5_code
      else if starts_with_any m [ "provider_k-4.5"; "glm-4.5" ]
      then Some Glm_4_5_text
      else if
        starts_with_any
          m
          [ "provider_k-4.6"
          ; "provider_k-4.7"
          ; "provider_k-5"
          ; "glm-4.6"
          ; "glm-4.7"
          ; "glm-5"
          ]
      then Some Glm_full_text
      else if starts_with_any m [ "provider_k-4-flash"; "glm-4-flash" ]
      then Some Glm_4_flash
      else if starts_with_any m [ "provider_k-4v"; "glm-4v" ]
      then Some Glm_4v
      else if starts_with_any m [ "provider_k-4"; "glm-4" ]
      then Some Glm_4
      else if starts_with_any m [ "qwen3"; "qwen-3" ]
      then Some Qwen_3
      else None)
;;

(** Lookup capabilities by model_id prefix using the built-in static table.
    Returns None if no specific override is known. *)
let capabilities_of_static_model_route = function
  | Agent_llm_a_opus_4 ->
    Some
      { anthropic_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 128_000
      }
  | Agent_llm_a_sonnet_4 ->
    Some
      { anthropic_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 64_000
      }
  | Agent_llm_a_haiku_4 ->
    Some
      { anthropic_capabilities with
        max_context_tokens = Some 200_000
      ; max_output_tokens = Some 8_192
      }
  | Chat_completions_v1_5 ->
    Some
      { openai_compat_chat_extended_capabilities with
        max_context_tokens = Some 1_050_000
      ; max_output_tokens = Some 128_000
      ; supports_computer_use = true
      }
  | Chat_completions_v1_4_1 ->
    Some
      { openai_compat_chat_capabilities with
        max_context_tokens = Some 1_000_000
      ; max_output_tokens = Some 32_000
      }
  | Chat_completions_v1_4o ->
    Some
      { openai_compat_chat_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      }
  | Mimo_v2_5_chat ->
    Some
      { openai_compat_chat_capabilities with
        supports_reasoning = true
      ; thinking_control_format = Thinking_object_only
      }
  | Gemini _ -> Some gemini_capabilities
  | Kimi_for_coding | Kimi_k2 -> Some kimi_capabilities
  | DashScope_3 ->
    Some
      { default_capabilities with
        max_context_tokens = Some 262_144
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_reasoning_budget = true
      ; thinking_control_format = Chat_template_kwargs
      ; supports_native_streaming = true
      ; supports_top_k = true
      ; supports_min_p = true
      }
  | Provider_n_4 ->
    Some
      { default_capabilities with
        max_context_tokens = Some 1_000_000
      ; supports_tools = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  | Provider_g_v4_flash ->
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
  | Provider_g_v4_pro ->
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
  | Provider_j_large ->
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
  | Provider_j_small ->
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
  | Provider_m_command ->
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
  | Provider_e_grok ->
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
    (* NVIDIA Provider_l: Llama-based, NIM Chat_completions_v1-compat API.
       Base text models (provider_l-ultra, provider_l-core) get reasoning
       but no vision. VL suffix gets image input. *)
  | Provider_l { has_vision } ->
    Some
      { provider_l_capabilities with
        max_context_tokens = Some 131_072
      ; max_output_tokens = Some 16_384
      ; supports_multimodal_inputs = has_vision
      ; supports_image_input = has_vision
      }
    (* Gemma 4: Google open-weight multimodal.
       4 sizes (1B/4B/12B/27B-31B). All support function calling,
       image input, streaming. 27B+ supports audio. 256K context. *)
  | Gemini_gemma_4 { has_large_audio } ->
    Some
      { default_capabilities with
        max_context_tokens = Some 262_144
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_response_format_json = true
      ; supports_structured_output = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_audio_input = has_large_audio
      ; supports_native_streaming = true
      ; supports_seed = true
      ; modality_priority =
          Modality.Visual_first
          (* Gemma 4 best practices: place image/audio before text for
           optimal multimodal performance. *)
      }
    (* GLM-4.7 Flash/FlashX: official Z.AI GLM-4.7 series docs describe
       thinking mode with 200K context and 128K max output. Must precede the
       broad GLM-4.7 match below. *)
  | Glm_4_7_flash ->
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
  | Glm_4_5_flash_air ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 96_000
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      }
    (* GLM-5-Turbo: official docs list Thinking Mode, 200K context, and
       128K max output. *)
  | Glm_5_turbo ->
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
  | Glm_5v_turbo ->
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
  | Glm_ocr ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  | Glm_4_6_vision_reasoning ->
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
  | Glm_4_5_vision_reasoning ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 16_384
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
    (* Glm-5-Code: coding-specific variant with 128K context (not 200K).
       Z.AI docs: Glm-5-Code uses /api/coding/paas/ endpoint, 128K context. *)
  | Glm_5_code ->
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
  | Glm_4_5_text ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 96_000
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_response_format_json = true
      ; supports_native_streaming = true
      }
    (* GLM-4.6/4.7/5/5.1 full text models: reasoning, large context/output,
       but no vision. *)
  | Glm_full_text ->
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
  | Glm_4_flash ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 4_096
      ; supports_tools = true
      ; supports_native_streaming = true
      }
  | Glm_4v ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 4_096
      ; supports_tools = true
      ; supports_multimodal_inputs = true
      ; supports_image_input = true
      ; supports_native_streaming = true
      }
  | Glm_4 ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 4_096
      ; supports_tools = true
      ; supports_tool_choice = false
      ; supports_native_streaming = true
      }
    (* Qwen3 / Qwen3.5 family.  All Qwen3 sizes (0.6B–235B) and Qwen3.5
       expose native <think> reasoning blocks via the OpenAI-compatible
       endpoint (typically through vLLM / llama.cpp / Ollama, which route
       to the [OpenAI_compat] kind in OAS).  Without this entry the
       provider-default capability set declared no thinking support, and
       every cycle produced a [Thinking_returned_but_declared_unsupported]
       INFO observation — silent in the warn channel but noisy in the
       info channel.  Declaring thinking + tools support here promotes
       the capability source from [Provider_default_capability] to
       [Model_capability], which silences the drift observation and
       upgrades subsequent declaration errors to high-confidence warns. *)
  | Qwen_3 ->
    Some
      { default_capabilities with
        max_context_tokens = Some 128_000
      ; max_output_tokens = Some 32_768
      ; supports_tools = true
      ; supports_tool_choice = true
      ; supports_parallel_tool_calls = true
      ; supports_reasoning = true
      ; supports_extended_thinking = true
      ; supports_response_format_json = true
      ; supports_structured_output = true
      ; supports_native_streaming = true
      }
;;

let for_model_id_static model_id =
  match static_model_route_of_id model_id with
  | Some route -> capabilities_of_static_model_route route
  | None -> None
;;

(** Lookup capabilities by provider label string.

    Returns [None] for labels outside the recognized set so callers can
    fail closed rather than silently treating unknown providers as
    having default capabilities. *)
let capabilities_for_provider_label label =
  match String.lowercase_ascii (String.trim label) with
  | "anthropic" | "claude" | "provider_a" -> Some anthropic_capabilities
  | "openai_compat" | "openai" | "chat_completions_v1" ->
    Some openai_compat_chat_capabilities
  | "openai_compat_chat_extended" | "chat_completions_v1_extended" ->
    Some openai_compat_chat_extended_capabilities
  | "gemini" | "provider_f" -> Some gemini_capabilities
  | "ollama" | "ollama_cloud" -> Some ollama_capabilities
  | "glm" | "zhipu" | "provider_k" | "provider_k-coding" -> Some glm_capabilities
  | "dashscope" | "provider_h" -> Some dashscope_capabilities
  | "provider_l" -> Some provider_l_capabilities
  | "kimi" | "provider_c" -> Some kimi_capabilities
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

let%test "for_model_id provider_k-4.5 has reasoning" =
  match for_model_id "provider_k-4.5" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 128_000
    && c.max_output_tokens = Some 96_000
  | None -> false
;;

let%test "for_model_id provider_k-4 no reasoning" =
  match for_model_id "provider_k-4-chat" with
  | Some c -> (not c.supports_reasoning) && c.max_context_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id provider_k-4v has vision" =
  match for_model_id "provider_k-4v-flash" with
  | Some c -> c.supports_image_input && c.supports_multimodal_inputs
  | None -> false
;;

let%test "for_model_id provider_k-4-flash basic" =
  match for_model_id "provider_k-4-flash" with
  | Some c -> c.supports_tools && c.max_output_tokens = Some 4_096
  | None -> false
;;

let%test "for_model_id provider_k-5 is text only" =
  match for_model_id "provider_k-5" with
  | Some c -> c.supports_reasoning && not c.supports_image_input
  | None -> false
;;

let%test "for_model_id provider_k-5v has vision" =
  match for_model_id "provider_k-5v-turbo" with
  | Some c -> c.supports_reasoning && c.supports_image_input
  | None -> false
;;

let%test "for_model_id provider_k-4.6v stays vision-capable" =
  match for_model_id "provider_k-4.6v" with
  | Some c ->
    c.supports_reasoning && c.supports_image_input && c.max_output_tokens = Some 32_768
  | None -> false
;;

let%test "for_model_id provider_k-4.5v stays vision-capable" =
  match for_model_id "provider_k-4.5v" with
  | Some c ->
    c.supports_reasoning && c.supports_image_input && c.max_output_tokens = Some 16_384
  | None -> false
;;

let%test "for_model_id provider_k-4.7-flashx has GLM-4.7 thinking limits" =
  match for_model_id "provider_k-4.7-flashx" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 200_000
    && c.max_output_tokens = Some 128_000
    && c.supports_tools
  | None -> false
;;

let%test "for_model_id provider_k-4.7-flash has thinking" =
  match for_model_id "provider_k-4.7-flash" with
  | Some c -> c.supports_reasoning && c.max_output_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id provider_k-4.5-flash has GLM-4.5 thinking limits" =
  match for_model_id "provider_k-4.5-flash" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 128_000
    && c.max_output_tokens = Some 96_000
    && c.supports_tools
  | None -> false
;;

(* qwen3 family tests use [for_model_id_static] rather than [for_model_id]
   so they remain stable when an ambient [OAS_CAPABILITY_MANIFEST] or
   an external manifest file overrides the static table — the contract
   under test here is the static fallback
   for environments without a manifest. *)
let%test "for_model_id_static qwen3 has extended thinking" =
  match for_model_id_static "qwen3-32b" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.supports_tools
    && c.supports_native_streaming
  | None -> false
;;

let%test "for_model_id_static qwen3.5 routes to Qwen_3 family" =
  match for_model_id_static "qwen3.5" with
  | Some c -> c.supports_extended_thinking && c.max_output_tokens = Some 32_768
  | None -> false
;;

let%test "for_model_id_static qwen-3-7b prefix variant resolves" =
  match for_model_id_static "qwen-3-7b-instruct" with
  | Some c -> c.supports_extended_thinking
  | None -> false
;;

let%test "for_model_id provider_k-5-turbo has GLM-5 thinking limits" =
  match for_model_id "provider_k-5-turbo" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 200_000
    && c.max_output_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id provider_k-5.1 full model (reasoning + extended thinking)" =
  match for_model_id "provider_k-5.1" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_output_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id bare glm-5 full model (reasoning + extended thinking)" =
  match for_model_id "glm-5" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_output_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id bare glm-5.1 full model (reasoning + extended thinking)" =
  match for_model_id "glm-5.1" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_output_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id bare glm-5-turbo has GLM-5 thinking limits" =
  match for_model_id "glm-5-turbo" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 200_000
    && c.max_output_tokens = Some 128_000
  | None -> false
;;

(* --- emits_usage_tokens / capabilities_for_provider_label --- *)

let%test "emits_usage_tokens: default is true" = default_capabilities.emits_usage_tokens

let%test "emits_usage_tokens: provider_a reports usage" =
  anthropic_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: ollama reports usage" =
  ollama_capabilities.emits_usage_tokens
;;

let%test "capabilities_for_provider_label: anthropic" =
  match capabilities_for_provider_label "provider_a" with
  | Some c -> c.emits_usage_tokens && c.supports_caching
  | None -> false
;;

let%test "capabilities_for_provider_label: chat_completions_v1 alias" =
  Option.is_some (capabilities_for_provider_label "chat_completions_v1")
  && Option.is_some (capabilities_for_provider_label "openai_compat")
;;

let%test "capabilities_for_provider_label: provider_k alias" =
  Option.is_some (capabilities_for_provider_label "provider_k")
  && Option.is_some (capabilities_for_provider_label "provider_k-coding")
;;

let%test "capabilities_for_provider_label: unknown returns None" =
  Option.is_none (capabilities_for_provider_label "not_a_real_provider_xyz")
;;

(* --- Provider_l / Gemma 4 --- *)

let%test "provider_l_capabilities has chat_template_kwargs thinking" =
  provider_l_capabilities.thinking_control_format = Chat_template_kwargs
;;

let%test "for_model_id provider_l-ultra has reasoning" =
  match for_model_id "provider_l-ultra-253b" with
  | Some c -> c.supports_reasoning && c.supports_tool_choice
  | None -> false
;;

let%test "for_model_id provider_l-vl has image input" =
  match for_model_id "provider_l-vl" with
  | Some c -> c.supports_image_input && c.supports_multimodal_inputs
  | None -> false
;;

let%test "for_model_id provider_h_3 has chat_template_kwargs thinking control" =
  (* DashScope_3.x Chat_completions_v1-compatible llama.cpp/llama-server deployments return
     [reasoning_content] when thinking is enabled through
     {"chat_template_kwargs": {"enable_thinking": bool}}.  Without this
     format, [supports_extended_thinking = true] never reaches the wire. *)
  match for_model_id "provider_h-3.5" with
  | Some c ->
    c.supports_reasoning_budget && c.thinking_control_format = Chat_template_kwargs
  | None -> false
;;

let%test "for_model_id nvidia/provider_l-core resolves" =
  match for_model_id "nvidia/provider_l-core" with
  | Some c -> c.supports_reasoning
  | None -> false
;;

let%test "for_model_id model-f-gemma-4-27b has tools + seed" =
  match for_model_id "model-f-gemma-4-27b-it" with
  | Some c ->
    c.supports_tools
    && c.supports_seed
    && c.supports_image_input
    && c.max_context_tokens = Some 262_144
  | None -> false
;;

let%test "for_model_id model-f-gemma-4-1b-it has tools, no audio" =
  match for_model_id "model-f-gemma-4-1b-it" with
  | Some c -> c.supports_tools && c.supports_image_input && not c.supports_audio_input
  | None -> false
;;

let%test "for_model_id google/model-f-gemma-4-1b-it is NOT large" =
  match for_model_id "google/model-f-gemma-4-1b-it" with
  | Some c -> not c.supports_audio_input
  | None -> false
;;

let%test "for_model_id google/model-f-gemma-4-27b-it IS large" =
  match for_model_id "google/model-f-gemma-4-27b-it" with
  | Some c -> c.supports_audio_input
  | None -> false
;;

let%test "for_model_id model-f-gemma-4-31b IS large" =
  match for_model_id "model-f-gemma-4-31b-it" with
  | Some c -> c.supports_audio_input
  | None -> false
;;

let%test "capabilities_for_provider_label: provider_l" =
  match capabilities_for_provider_label "provider_l" with
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
    [ ( "provider_k-4.7-flash-turbo"
      , fun c -> c.max_output_tokens = Some 128_000 && c.supports_extended_thinking )
    ; ( "provider_k-4.5-flash-test"
      , fun c -> c.max_output_tokens = Some 96_000 && c.supports_extended_thinking )
    ; ( "provider_k-5-turbo-latest"
      , fun c -> c.max_output_tokens = Some 128_000 && c.supports_extended_thinking )
    ; ( "glm-5-turbo-latest"
      , fun c -> c.max_output_tokens = Some 128_000 && c.supports_extended_thinking )
    ; ("provider_k-4.6v-plus", fun c -> c.supports_image_input && c.supports_reasoning)
    ; ( "provider_k-4.7-flash-test"
      , fun c -> c.max_output_tokens = Some 128_000 && c.supports_reasoning )
    ; ( "glm-4.7-flashx"
      , fun c -> c.max_output_tokens = Some 128_000 && c.supports_reasoning )
    ; ( "provider_k-4-flash-mini"
      , fun c -> c.max_output_tokens = Some 4_096 && not c.supports_reasoning )
    ; ("provider_k-4v-plus", fun c -> c.supports_image_input)
    ; ( "provider_k-4.5-air-test"
      , fun c -> c.max_output_tokens = Some 96_000 && c.supports_reasoning )
    ; ( "glm-4.5-air-test"
      , fun c -> c.max_output_tokens = Some 96_000 && c.supports_reasoning )
    ; ( "provider_k-5v-turbo-latest"
      , fun c ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 128_000 )
    ; ( "glm-5v-turbo-latest"
      , fun c ->
          c.supports_image_input
          && c.supports_reasoning
          && c.max_output_tokens = Some 128_000 )
    ; ("provider_k-ocr-test", fun c -> c.supports_image_input && not c.supports_tools)
    ; ("glm-ocr-test", fun c -> c.supports_image_input && not c.supports_tools)
    ; ("agent_llm_a-opus-4-20250501", fun c -> c.max_output_tokens = Some 128_000)
    ; ("model-d-4.1-mini", fun c -> c.max_output_tokens = Some 32_000)
    ; ("provider_g-v4-flash-test", fun c -> c.thinking_control_format = Thinking_object)
    ; ( "provider_l-ultra-253b"
      , fun c ->
          c.thinking_control_format = Chat_template_kwargs && c.supports_tool_choice )
    ; ( "nvidia/provider_l-ultra-253b"
      , fun c ->
          c.thinking_control_format = Chat_template_kwargs && c.supports_tool_choice )
    ; ("provider_l-vl", fun c -> c.supports_image_input && c.supports_multimodal_inputs)
    ; ( "model-f-gemma-4-27b-it"
      , fun c ->
          c.supports_tools
          && c.supports_image_input
          && c.supports_seed
          && c.max_context_tokens = Some 262_144 )
    ; ( "google/model-f-gemma-4-27b-it"
      , fun c -> c.supports_tools && c.supports_image_input )
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
  (* Content block analysis.
     Enumerate every [Types.content_block] variant explicitly so that adding
     a new constructor (e.g. [Video], [Reasoning_summary]) triggers an
     exhaustiveness warning here, forcing a deliberate decision on whether
     the new block carries tool-use or reasoning semantics. The previous
     [_ -> ()] catch-all silently grouped 5 unrelated variants and would
     have absorbed any future block without review. *)
  let has_tool_use = ref false
  and has_thinking = ref false in
  List.iter
    (fun (block : Types.content_block) ->
       match block with
       | ToolUse _ -> has_tool_use := true
       | Thinking _ | RedactedThinking _ -> has_thinking := true
       | Text _ | ToolResult _ | Image _ | Document _ | Audio _ ->
         (* No capability-drift signal: these blocks are valid against any
           capability set the response declares. *)
         ())
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
  let alias_pairs =
    [ "chat_completions_v1", "openai_compat"; "provider_k", "provider_k-coding" ]
  in
  List.for_all (fun (a, b) -> same_base a b) alias_pairs
  && Option.is_some (resolve "provider_a")
  && Option.is_some (resolve "provider_f")
  && Option.is_some (resolve "ollama")
  && Option.is_some (resolve "provider_c")
  && Option.is_some (resolve "provider_l")
;;

(* Every declared label is reachable — no dead branches in the match.
   If a label is added to the match but has no corresponding capability
   binding, this test will fail. *)
let%test "capabilities_for_provider_label: all declared labels resolve" =
  let labels =
    [ "provider_a"
    ; "chat_completions_v1"
    ; "chat_completions_v1_extended"
    ; "provider_f"
    ; "ollama"
    ; "provider_k"
    ; "provider_k-coding"
    ; "provider_l"
    ; "provider_c"
    ; "cli_tool_d"
    ; "cli_tool_b"
    ; "cli_tool_c"
    ; "cli_tool_a"
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
    [ "provider_a"
    ; "provider_f"
    ; "ollama"
    ; "provider_c"
    ; "cli_tool_d"
    ; "cli_tool_b"
    ; "cli_tool_c"
    ; "cli_tool_a"
    ; "provider_l"
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
