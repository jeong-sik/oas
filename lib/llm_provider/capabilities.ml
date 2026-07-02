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
type thinking_control_format = Capability_vocab.thinking_control_format =
  | No_thinking_control (** No thinking control supported *)
  | Thinking_object (** Top-level [thinking] object plus optional [reasoning_effort]. *)
  | Thinking_object_adaptive
  | Thinking_object_only
  | Chat_template_kwargs
  | Chat_template_token
  | Ollama_think
  | Reasoning_effort
  | Enable_thinking
  (** DashScope-style top-level [enable_thinking] / [preserve_thinking] bools
      plus optional [thinking_budget]. *)
[@@deriving show, eq]

let%test "thinking_control_format derives equal and show" =
  equal_thinking_control_format No_thinking_control No_thinking_control
  && (not (equal_thinking_control_format No_thinking_control Enable_thinking))
  && String.length (show_thinking_control_format Enable_thinking) > 0
;;

type preserve_thinking_control_format =
      Capability_vocab.preserve_thinking_control_format =
  | No_preserve_thinking_control
  | Thinking_object_keep_all
  | Chat_template_kwargs_preserve_thinking
  | Top_level_preserve_thinking
  | Always_preserved_thinking

(** Optional override for the multi-turn reasoning replay policy. Most providers
    inherit the policy implied by [thinking_control_format]; catalog entries set
    this when a model's documented replay contract differs (e.g. Kimi K2 requires
    replaying [reasoning_content] on every turn, so [Force_preserve_always]).

    @since 0.207.16 *)
type reasoning_replay_override = Capability_vocab.reasoning_replay_override =
  | Default_reasoning_replay
  | Force_no_replay
  | Force_drop_without_tool_preserve_with_tool
  | Force_preserve_always

type assistant_tool_content_format = Capability_vocab.assistant_tool_content_format =
  | Assistant_tool_content_null
  | Assistant_tool_content_empty_string

type reasoning_output_format = Capability_vocab.reasoning_output_format =
  | No_reasoning_output_format
  | Split_reasoning_fields

type reasoning_streaming_format = Capability_vocab.reasoning_streaming_format =
  | Default_reasoning_streaming
  | No_reasoning_streaming
  | Delta_reasoning_field of string
  | Template_reasoning_streaming

(** Catalog-declared inference task for non-chat models. [None] on every
    chat/completion model; a value is only ever set by an explicit [task]
    field on a model catalog entry — never inferred from the model id. *)
type task = Capability_vocab.task =
  | Transcription
  | Speech
  | Image_generation
  | Video_generation

type capabilities =
  { (* ── Numeric limits ────────────────────────────────── *)
    max_context_tokens : int option (** Model's context window. None = unknown. *)
  ; max_output_tokens : int option (** Model's max output. None = unknown. *)
  ; (* ── Tool use ──────────────────────────────────────── *)
    supports_tools : bool
  ; supports_tool_choice : bool
  ; supports_required_tool_choice : bool
  ; supports_named_tool_choice : bool
  ; supports_parallel_tool_calls : bool
  ; supports_runtime_mcp_tools : bool
  ; supports_runtime_tool_events : bool
  ; assistant_tool_content_format : assistant_tool_content_format
    (** Wire shape for assistant messages that contain tool calls but no visible
        text. OpenAI-compatible providers disagree here: OpenAI accepts
        [content:null], while GLM's OpenAI-compatible contract keeps
        [content:""]. This is independent from reasoning replay. *)
  ; (* ── Thinking / reasoning ──────────────────────────── *)
    supports_reasoning : bool (** Any form of reasoning/thinking *)
  ; supports_extended_thinking : bool (** budget_tokens / reasoning_effort *)
  ; supports_reasoning_budget : bool (** Controllable reasoning depth *)
  ; accepted_reasoning_efforts : Reasoning_effort.t list option
    (** Model/provider-specific subset of canonical reasoning efforts accepted
        by the request wire format. [None] means no subset is declared and the
        dialect vocabulary applies; [Some values] is enforced before request
        serialization. *)
  ; thinking_control_format : thinking_control_format
    (** Wire-format for thinking control on OpenAI-compat backends.
        Determines which JSON shape the backend emits for enable_thinking.
        Only meaningful when [supports_reasoning] or [supports_extended_thinking]
        is true and the request goes through backend_openai.
        @since 0.184.0 *)
  ; preserve_thinking_control_format : preserve_thinking_control_format
    (** Wire-format for preserving historical reasoning between requests.
        This is intentionally separate from [thinking_control_format]: some
        models use [thinking.keep], some have no request toggle but require
        replay, and Qwen-style chat templates use a nested boolean. *)
  ; reasoning_output_format : reasoning_output_format
    (** Request-side control for where provider reasoning is returned. Some
        OpenAI-compatible providers require an explicit split switch before
        returning reasoning in side-channel fields instead of embedding it in
        visible [content]. *)
  ; reasoning_streaming_format : reasoning_streaming_format
    (** Optional streaming side-channel override. [Default_reasoning_streaming]
        derives the parser field from [thinking_control_format]; catalog entries
        set a concrete value when the endpoint stream field differs from the
        request-side thinking control shape. *)
  ; reasoning_replay_override : reasoning_replay_override
    (** Optional override for the multi-turn reasoning replay policy. Defaults to
        the policy implied by [thinking_control_format]; catalog entries set this
        when the model's documented replay contract differs (e.g. Kimi K2). *)
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
  ; (* ── Inference task ────────────────────────────────── *)
    task : task option
    (** Inference task declared by the model catalog entry (transcription,
        speech, image/video generation). [None] = no declared task; there is
        no model-id inference fallback. *)
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
      providers (Openai, Gemini) do not guarantee deterministic output
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
  ; supports_required_tool_choice = false
  ; supports_named_tool_choice = false
  ; supports_parallel_tool_calls = false
  ; supports_runtime_mcp_tools = false
  ; supports_runtime_tool_events = false
  ; assistant_tool_content_format = Assistant_tool_content_null
  ; supports_reasoning = false
  ; supports_extended_thinking = false
  ; supports_reasoning_budget = false
  ; accepted_reasoning_efforts = None
  ; thinking_control_format = No_thinking_control
  ; preserve_thinking_control_format = No_preserve_thinking_control
  ; reasoning_output_format = No_reasoning_output_format
  ; reasoning_streaming_format = Default_reasoning_streaming
  ; reasoning_replay_override = Default_reasoning_replay
  ; supports_response_format_json = false
  ; supports_structured_output = false
  ; supports_multimodal_inputs = false
  ; supports_image_input = false
  ; supports_audio_input = false
  ; supports_video_input = false
  ; modality_priority = Modality.Preserve_input_order
  ; task = None
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

let effective_disable_parallel_tool_use
      ~caller_disabled
      ~supports_parallel_tool_calls
      ~tools_present
  =
  caller_disabled || (tools_present && not supports_parallel_tool_calls)
;;

type anthropic_thinking_control =
  | Anthropic_manual_budget
  | Anthropic_adaptive_preferred
  | Anthropic_adaptive_only
  | Anthropic_always_adaptive

let anthropic_thinking_control_of_id model_id =
  let id = String.lowercase_ascii (String.trim model_id) in
  let has_prefix prefixes =
    List.exists (fun prefix -> String.starts_with ~prefix id) prefixes
  in
  if
    has_prefix
      [ "claude-fable-5"
      ; "fable-5"
      ; "claude-fable-5"
      ; "claude-mythos-5"
      ; "mythos-5"
      ; "claude-mythos-5"
      ; "claude-mythos-preview"
      ; "mythos-preview"
      ; "claude-mythos-preview"
      ]
  then Anthropic_always_adaptive
  else if
    has_prefix
      [ "claude-opus-4-8"
      ; "opus-4-8"
      ; "claude-opus-4-8"
      ; "claude-opus-4-7"
      ; "opus-4-7"
      ; "claude-opus-4-7"
      ]
  then Anthropic_adaptive_only
  else if
    has_prefix
      [ "claude-opus-4-6"
      ; "opus-4-6"
      ; "claude-opus-4-6"
      ; "claude-sonnet-4-6"
      ; "sonnet-4-6"
      ; "claude-sonnet-4-6"
      ]
  then Anthropic_adaptive_preferred
  else Anthropic_manual_budget
;;

let anthropic_capabilities =
  { default_capabilities with
    max_context_tokens = Some 200_000
  ; (* default; opus/sonnet 4.6 = 1M *)
    max_output_tokens = Some 8_192
  ; (* default; higher for newer models *)
    supports_tools = true
  ; supports_tool_choice = true
  ; supports_required_tool_choice = true
  ; supports_named_tool_choice = true
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
    max_context_tokens =
      Some 256_000
      (* platform.kimi.ai documents only the default max_tokens (32768); it does
       not state a higher output ceiling. Keep the verified default here. A
       higher ceiling (if any) is deferred to the per-provider capability pass. *)
  ; max_output_tokens = Some 32_768
  ; supports_tools = true
  ; supports_tool_choice = true
  ; (* Kimi's chat API documents [tools] but not [tool_choice] in any request
     schema (platform.kimi.ai/docs/api/chat covers KimiK27Code/K26/K25/MoonshotV1
     request bodies; none expose tool_choice), and tool_choice=required is
     unsupported — developers prompt the model to force a tool instead. Plain
     [auto] passes through the OpenAI-compatible endpoint so [supports_tool_choice]
     stays true, but a forced *named* tool_choice has no faithful wire
     representation. OAS therefore treats Kimi like GLM: tools supported, named
     forced tool_choice rejected with a typed [Unsupported_named_tool_choice]
     rather than serialized into a request the model cannot honor. This is the
     conservative declaration-over-probing default (same rationale as the Ollama
     profile); a deployment that verified native named support can still override
     per-entry via the model catalog. Ref checked 2026-06-30:
     platform.kimi.ai/docs/api/chat, platform.kimi.ai/docs/api/tool-use. *)
    supports_required_tool_choice = false
  ; (* Same Kimi contract: named forced tool_choice has no faithful wire
       representation. Keep required/named separated so Auto/None remain valid. *)
    supports_named_tool_choice = false
  ; supports_parallel_tool_calls = true
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_reasoning_budget = false
  ; thinking_control_format = No_thinking_control
  ; preserve_thinking_control_format = Always_preserved_thinking
  ; reasoning_replay_override =
      Force_preserve_always
      (* Kimi K2 docs (platform.moonshot.ai, checked 2026-06-29): reasoning_content
       must be replayed across turns — hard-required on tool-call turns
       ("otherwise an error will be thrown") and recommended on all turns;
       kimi-k2.7-code keeps Preserved Thinking always-on. The base Kimi profile
       carries [Always_preserved_thinking], and this typed override preserves the
       same replay contract for catalog rows whose per-entry capabilities inherit
       base="kimi" without repeating the policy. *)
  ; supports_response_format_json = true
  ; (* Native Moonshot/Kimi docs used for K2/K2.7 do not document an OpenAI
       json_schema-equivalent strict structured-output field. Ollama Cloud
       Kimi rows are provider-qualified and declare their own schema support. *)
    supports_structured_output = false
  ; supports_system_prompt = true
  ; supports_native_streaming = true
  ; supports_multimodal_inputs = true
  ; supports_image_input = true
  ; supports_code_execution =
      true
      (* Preserved from the pre-rename kimi_capabilities; dropped by accident
       in the capability rename. *)
  }
;;

let openai_compat_chat_capabilities =
  { default_capabilities with
    max_context_tokens = Some 128_000
  ; max_output_tokens = Some 16_384
  ; supports_tools = true
  ; supports_tool_choice = true
  ; supports_required_tool_choice = true
  ; supports_named_tool_choice = true
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

(* Ollama OpenAI-compat endpoint behavior on tool_choice is model-dependent
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
   the Claude Agent SDK sidesteps by being single-provider. *)
(* NVIDIA NIM Nvidia: Llama-based OpenAI-compatible endpoint.
   Thinking uses chat_template_kwargs (same wire format as Ollama's
   llama-server backend). VL variants add image input.
   Ref: build.nvidia.com/nvidia docs, Nvidia model cards. *)
let provider_l_capabilities =
  { openai_compat_chat_extended_capabilities with
    supports_tool_choice = true
  ; supports_reasoning = true
  ; thinking_control_format = Chat_template_kwargs
  ; preserve_thinking_control_format = Chat_template_kwargs_preserve_thinking
  }
;;

let ollama_capabilities =
  { openai_compat_chat_extended_capabilities with
    supports_tool_choice = false
  ; supports_required_tool_choice = false
  ; supports_named_tool_choice = false
  ; supports_seed = true
  ; supports_seed_with_images = true
  ; thinking_control_format = Ollama_think
  }
;;

(* Ollama Cloud is a distinct provider identity (its own base_url + auth) but its
   capability profile is currently identical to local Ollama. Reasoning-only
   replies still reach the user through the display path
   ([Api.text_blocks_to_string] includes [Thinking] blocks); they are no longer
   promoted into assistant answer text, which previously re-injected reasoning on
   replay and caused the CoT loop (#2236). *)
let ollama_cloud_capabilities = ollama_capabilities

let dashscope_capabilities =
  { openai_compat_chat_extended_capabilities with
    supports_tool_choice = true
  ; supports_min_p = true
  ; thinking_control_format = Enable_thinking
  ; preserve_thinking_control_format = Top_level_preserve_thinking
  }
;;

let glm_capabilities =
  { default_capabilities with
    max_context_tokens = Some 200_000
  ; (* Glm-5.1 API enforces max_tokens <= 40960 at request time; keeping a
     higher value here causes server-side rejection with
     "Invalid request: `max_tokens` must be less than or equal to `40960`".
     Empirical upper bound observed on 2026-04-12 during automated
     turns against glm-coding:glm-5.1 and glm:glm-5.1. *)
    max_output_tokens = Some 40_960
  ; supports_tools = true
  ; (* Z.AI's function-calling docs currently document [tool_choice]
     as default [auto] and "only supports auto". OAS therefore treats
     Glm as "tools supported, forced tool_choice unsupported":
     callers may still send tools, but named forced tool_choice requests
     are not serialized because GLM has no faithful representation for
     them. The completion contract must stay relaxed so direct Glm text
     replies do not count as contract violations. Ref checked 2026-04-21:
     https://docs.z.ai/guides/capabilities/function-calling *)
    supports_tool_choice = true
  ; supports_required_tool_choice = false
  ; supports_named_tool_choice = false
  ; assistant_tool_content_format = Assistant_tool_content_empty_string
  ; supports_reasoning = true
  ; supports_extended_thinking = true
  ; supports_response_format_json = true
  ; (* Z.AI's current official docs describe JSON mode via
     response_format={"type":"json_object"} plus prompt/schema-in-text
     guidance, but do not document a native JSON-schema request field
     equivalent to Openai's json_schema response_format. OAS therefore
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

    Centralizes the [String.starts_with ~prefix:"gemini-..."] dispatch into a
    single classifier with an exhaustive variant. Downstream code switches on
    the variant instead of comparing strings, so a new family member is a
    compile-time obligation rather than a runtime string-match miss.

    The internal use of [starts_with] inside [gemini_family_of_id] is
    intentional and bounded: prefix matching is the only signal Google's model
    IDs offer. Concentrating it here keeps the rest of the codebase typed.

    @since 0.196.3 *)
type gemini_family =
  | Gemini_3_1 (** [gemini-3.1.*] — 3.1 line (pro-preview, flash-lite-preview, …) *)
  | Gemini_3 (** [gemini-3.*] but not 3.1 — flash-preview and siblings *)
  | Gemini_2_5 (** [gemini-2.5.*] — legacy line, kept until removal PR *)
  | Gemini_other of string
  (** Unknown gemini id or non-gemini id. Retains the literal so the
          caller can log / fall through without losing data. *)

type gemini_thinking_control =
  | Gemini_thinking_budget
  | Gemini_thinking_level of { supports_minimal : bool }
  | Gemini_unknown_thinking_control

let strip_suffix ~suffix value =
  if String.ends_with ~suffix value
  then String.sub value 0 (String.length value - String.length suffix)
  else value
;;

(** Classify a model id into a [gemini_family]. Order matters: [gemini-3.1]
    is checked before [gemini-3] so the more specific prefix wins.
    Input is expected lowercased (callers pass the already-normalized id). *)
let gemini_family_of_id (id : string) : gemini_family =
  let starts p = String.starts_with ~prefix:p id in
  if starts "gemini-3.1"
  then Gemini_3_1
  else if starts "gemini-3"
  then Gemini_3
  else if starts "gemini-2.5"
  then Gemini_2_5
  else Gemini_other id
;;

let gemini_thinking_control_of_id raw_id =
  let id = String.lowercase_ascii (String.trim raw_id) in
  match gemini_family_of_id id with
  | Gemini_3_1 ->
    (* Per ai.google.dev/gemini-api/docs/thinking and the gemini-3.1-flash-lite
       model card (checked 2026-06-29), the [minimal] thinking level is valid
       ONLY for gemini-3.1-flash-lite (where it is the default). gemini-3.1-pro
       and gemini-3.1-flash accept low/medium/high only, so emitting [minimal]
       for them produces an invalid thinkingLevel. The [-lite] check sits beside
       the family classifier's bounded prefix matching (see [gemini_family_of_id]
       doc) and keeps the wire decision typed at the call site. *)
    let supports_minimal = String.starts_with ~prefix:"gemini-3.1-flash-lite" id in
    Gemini_thinking_level { supports_minimal }
  | Gemini_3 ->
    (* gemini-3-flash-preview = low/medium/high; gemini-3-pro-preview = low/high.
       Neither preview line exposes the [minimal] level. *)
    Gemini_thinking_level { supports_minimal = false }
  | Gemini_2_5 -> Gemini_thinking_budget
  | Gemini_other _ -> Gemini_unknown_thinking_control
;;

let gemini_capabilities =
  { default_capabilities with
    max_context_tokens = Some 1_000_000
  ; max_output_tokens = Some 65_000
  ; supports_tools = true
  ; supports_tool_choice = true
  ; supports_required_tool_choice = true
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

(* CLI subprocess transports have been removed; CLI-specific capability
   records deleted. See PR #1809. *)

(* ── Model-specific overrides (lookup table) ─────────── *)

(** Capabilities preset for a canonical {!Provider_kind.t}.

    Typed counterpart of {!capabilities_for_provider_label}: maps the 7 closed
    variants directly to their presets without serialising the kind to a string
    and re-parsing it. Adding a new variant to {!Provider_kind.t} forces a
    compile error here, instead of silently falling through the [_ -> None] arm
    of the label classifier. This is the path callers that already hold a typed
    [kind] should use, so the
    [kind |> string_of_provider_kind |> capabilities_for_provider_label]
    round-trip disappears.

    Wire aliases and presets not expressible as a {!Provider_kind.t}
    (e.g. ["claude"], ["zhipu"], ["openai_chat_extended"], ["ollama_cloud"],
    ["nvidia"]) remain reachable only through {!capabilities_for_provider_label}
    at the catalog/env parse boundary.

    @since 0.209.0 *)
let capabilities_of_kind (kind : Provider_kind.t) : capabilities =
  match kind with
  | Provider_kind.Anthropic -> anthropic_capabilities
  | Provider_kind.Kimi -> kimi_capabilities
  | Provider_kind.OpenAI_compat -> openai_compat_chat_capabilities
  | Provider_kind.Ollama -> ollama_capabilities
  | Provider_kind.Gemini -> gemini_capabilities
  | Provider_kind.Glm -> glm_capabilities
  | Provider_kind.DashScope -> dashscope_capabilities
;;

let provider_kind_alias_of_label label =
  match Provider_kind.of_string label with
  | Some kind -> Some kind
  | None ->
    (match label with
     | "claude" -> Some Provider_kind.Anthropic
     | "openai" | "openai_chat" -> Some Provider_kind.OpenAI_compat
     | "zhipu" | "glm-coding" -> Some Provider_kind.Glm
     | _ -> None)
;;

(** Lookup capabilities by provider label string.

    Canonical labels and aliases for the closed {!Provider_kind.t} space are
    normalized to a typed kind first, then delegated to {!capabilities_of_kind}.
    String-only presets that are not expressible as a provider kind stay at this
    label boundary. Returns [None] for labels outside the recognized set so
    callers can fail closed rather than silently treating unknown providers as
    having default capabilities. *)
let capabilities_for_provider_label label =
  let label = String.lowercase_ascii (String.trim label) in
  match provider_kind_alias_of_label label with
  | Some kind -> Some (capabilities_of_kind kind)
  | None ->
    (match label with
     | "openai_compat_chat_extended" | "openai_chat_extended" ->
       Some openai_compat_chat_extended_capabilities
     | "xai" | "mistral" -> Some openai_compat_chat_extended_capabilities
     | "cohere" | "mimo" -> Some openai_compat_chat_capabilities
     | "ollama_cloud" -> Some ollama_cloud_capabilities
     | "nvidia" -> Some provider_l_capabilities
     | _ -> None)
;;

let%test "capabilities_of_kind aliases the same presets as the label classifier" =
  (* SSOT guard: the label path delegates canonical kinds to the typed variant
     path, so the resolved preset must be the exact same record. *)
  List.for_all
    (fun (kind, label) ->
       let via_kind = capabilities_of_kind kind in
       match capabilities_for_provider_label label with
       | Some via_label -> via_label == via_kind
       | None -> false)
    [ Provider_kind.Anthropic, "anthropic"
    ; Provider_kind.Kimi, "kimi"
    ; Provider_kind.OpenAI_compat, "openai_compat"
    ; Provider_kind.Ollama, "ollama"
    ; Provider_kind.Gemini, "gemini"
    ; Provider_kind.Glm, "glm"
    ; Provider_kind.DashScope, "dashscope"
    ]
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

let thinking_control_format_of_manifest_string raw =
  Capability_vocab.thinking_control_format_of_string raw
;;

let preserve_thinking_control_format_of_manifest_string raw =
  Capability_vocab.preserve_thinking_control_format_of_string raw
;;

let warn_unknown_capability_value ~field raw =
  Diag.warn
    "capabilities"
    "unknown %s value %S; keeping provider base capability"
    field
    raw
;;

let reasoning_replay_override_of_catalog_string raw =
  Capability_vocab.reasoning_replay_override_of_string raw
;;

let assistant_tool_content_format_of_catalog_string raw =
  Capability_vocab.assistant_tool_content_format_of_string raw
;;

let reasoning_output_format_of_catalog_string raw =
  Capability_vocab.reasoning_output_format_of_string raw
;;

let reasoning_streaming_format_of_catalog_string raw =
  Capability_vocab.reasoning_streaming_format_of_string raw
;;

let modality_priority_of_catalog_string raw =
  match String.lowercase_ascii (String.trim raw) with
  | "preserve_input_order" | "preserve-input-order" | "preserve" ->
    Some Modality.Preserve_input_order
  | "visual_first" | "visual-first" -> Some Modality.Visual_first
  | _ -> None
;;

let reasoning_efforts_of_catalog_strings ~field values =
  let parsed =
    List.map
      (fun raw ->
         match Reasoning_effort.of_string raw with
         | Some effort -> Ok effort
         | None -> Error raw)
      values
  in
  let efforts, invalid =
    List.partition_map
      (function
        | Ok effort -> Either.Left effort
        | Error raw -> Either.Right raw)
      parsed
  in
  match invalid with
  | [] -> Some efforts
  | values ->
    Diag.warn
      "capabilities"
      "unknown %s value(s) %s; keeping provider base capability"
      field
      (String.concat ", " values);
    None
;;

type declarative_capability_overrides =
  { base_label : string option
  ; max_context_tokens : int option
  ; max_output_tokens : int option
  ; supports_tools : bool option
  ; supports_tool_choice : bool option
  ; supports_required_tool_choice : bool option
  ; supports_named_tool_choice : bool option
  ; supports_parallel_tool_calls : bool option
  ; assistant_tool_content_format : string option
  ; supports_reasoning : bool option
  ; supports_extended_thinking : bool option
  ; supports_reasoning_budget : bool option
  ; accepted_reasoning_efforts : string list option
  ; supports_response_format_json : bool option
  ; supports_structured_output : bool option
  ; supports_multimodal_inputs : bool option
  ; supports_image_input : bool option
  ; supports_audio_input : bool option
  ; supports_video_input : bool option
  ; modality_priority : string option
  ; task : Capability_vocab.task option
  ; supports_native_streaming : bool option
  ; supports_system_prompt : bool option
  ; supports_caching : bool option
  ; supports_prompt_caching : bool option
  ; supports_top_k : bool option
  ; supports_min_p : bool option
  ; supports_seed : bool option
  ; supports_computer_use : bool option
  ; supports_code_execution : bool option
  ; thinking_control_format : string option
  ; preserve_thinking_control_format : string option
  ; reasoning_output_format : string option
  ; reasoning_streaming_format : string option
  ; reasoning_replay : string option
  }

let overrides_of_manifest_entry (entry : Capability_manifest.entry) =
  { base_label = entry.base_label
  ; max_context_tokens = entry.max_context_tokens
  ; max_output_tokens = entry.max_output_tokens
  ; supports_tools = entry.supports_tools
  ; supports_tool_choice = entry.supports_tool_choice
  ; supports_required_tool_choice = entry.supports_required_tool_choice
  ; supports_named_tool_choice = entry.supports_named_tool_choice
  ; supports_parallel_tool_calls = entry.supports_parallel_tool_calls
  ; assistant_tool_content_format = entry.assistant_tool_content_format
  ; supports_reasoning = entry.supports_reasoning
  ; supports_extended_thinking = entry.supports_extended_thinking
  ; supports_reasoning_budget = entry.supports_reasoning_budget
  ; accepted_reasoning_efforts = entry.accepted_reasoning_efforts
  ; supports_response_format_json = entry.supports_response_format_json
  ; supports_structured_output = entry.supports_structured_output
  ; supports_multimodal_inputs = entry.supports_multimodal_inputs
  ; supports_image_input = entry.supports_image_input
  ; supports_audio_input = entry.supports_audio_input
  ; supports_video_input = entry.supports_video_input
  ; modality_priority = None
  ; (* The JSON capability manifest carries no task field; task is
       catalog-only vocabulary. *)
    task = None
  ; supports_native_streaming = entry.supports_native_streaming
  ; supports_system_prompt = entry.supports_system_prompt
  ; supports_caching = entry.supports_caching
  ; supports_prompt_caching = entry.supports_prompt_caching
  ; supports_top_k = entry.supports_top_k
  ; supports_min_p = entry.supports_min_p
  ; supports_seed = entry.supports_seed
  ; supports_computer_use = entry.supports_computer_use
  ; supports_code_execution = entry.supports_code_execution
  ; thinking_control_format = entry.thinking_control_format
  ; preserve_thinking_control_format = entry.preserve_thinking_control_format
  ; reasoning_output_format = entry.reasoning_output_format
  ; reasoning_streaming_format = entry.reasoning_streaming_format
  ; reasoning_replay = entry.reasoning_replay
  }
;;

let apply_declarative_capability_overrides overrides =
  let base =
    match overrides.base_label with
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
  let supports_tool_choice =
    override_bool base.supports_tool_choice overrides.supports_tool_choice
  in
  let supports_required_tool_choice =
    match overrides.supports_required_tool_choice with
    | Some required -> required && supports_tool_choice
    | None -> base.supports_required_tool_choice && supports_tool_choice
  in
  let supports_named_tool_choice =
    match overrides.supports_named_tool_choice, overrides.supports_tool_choice with
    | Some named, _ -> named && supports_tool_choice
    | None, Some true -> true
    | None, Some false -> false
    | None, None -> base.supports_named_tool_choice && supports_tool_choice
  in
  let override_int_opt base_val = function
    | Some n -> Some n
    | None -> base_val
  in
  { base with
    max_context_tokens =
      override_int_opt base.max_context_tokens overrides.max_context_tokens
  ; max_output_tokens =
      override_int_opt base.max_output_tokens overrides.max_output_tokens
  ; supports_tools = override_bool base.supports_tools overrides.supports_tools
  ; supports_tool_choice
  ; supports_required_tool_choice
  ; supports_named_tool_choice
  ; supports_parallel_tool_calls =
      override_bool
        base.supports_parallel_tool_calls
        overrides.supports_parallel_tool_calls
  ; assistant_tool_content_format =
      (match overrides.assistant_tool_content_format with
       | Some s ->
         (match assistant_tool_content_format_of_catalog_string s with
          | Some format -> format
          | None ->
            warn_unknown_capability_value ~field:"assistant_tool_content_format" s;
            base.assistant_tool_content_format)
       | None -> base.assistant_tool_content_format)
  ; supports_reasoning =
      override_bool base.supports_reasoning overrides.supports_reasoning
  ; supports_extended_thinking =
      override_bool base.supports_extended_thinking overrides.supports_extended_thinking
  ; supports_reasoning_budget =
      override_bool base.supports_reasoning_budget overrides.supports_reasoning_budget
  ; accepted_reasoning_efforts =
      (match overrides.accepted_reasoning_efforts with
       | Some values ->
         (match
            reasoning_efforts_of_catalog_strings
              ~field:"accepted_reasoning_efforts"
              values
          with
          | Some efforts -> Some efforts
          | None -> base.accepted_reasoning_efforts)
       | None -> base.accepted_reasoning_efforts)
  ; supports_response_format_json =
      override_bool
        base.supports_response_format_json
        overrides.supports_response_format_json
  ; supports_structured_output =
      override_bool base.supports_structured_output overrides.supports_structured_output
  ; supports_multimodal_inputs =
      override_bool base.supports_multimodal_inputs overrides.supports_multimodal_inputs
  ; supports_image_input =
      override_bool base.supports_image_input overrides.supports_image_input
  ; supports_audio_input =
      override_bool base.supports_audio_input overrides.supports_audio_input
  ; supports_video_input =
      override_bool base.supports_video_input overrides.supports_video_input
  ; modality_priority =
      (match overrides.modality_priority with
       | Some s ->
         (match modality_priority_of_catalog_string s with
          | Some priority -> priority
          | None ->
            warn_unknown_capability_value ~field:"modality_priority" s;
            base.modality_priority)
       | None -> base.modality_priority)
  ; task =
      (* Already typed at the catalog parse boundary; no string re-parse. *)
      (match overrides.task with
       | Some _ as task -> task
       | None -> base.task)
  ; supports_native_streaming =
      override_bool base.supports_native_streaming overrides.supports_native_streaming
  ; supports_system_prompt =
      override_bool base.supports_system_prompt overrides.supports_system_prompt
  ; supports_caching = override_bool base.supports_caching overrides.supports_caching
  ; supports_prompt_caching =
      override_bool base.supports_prompt_caching overrides.supports_prompt_caching
  ; supports_top_k = override_bool base.supports_top_k overrides.supports_top_k
  ; supports_min_p = override_bool base.supports_min_p overrides.supports_min_p
  ; supports_seed = override_bool base.supports_seed overrides.supports_seed
  ; supports_computer_use =
      override_bool base.supports_computer_use overrides.supports_computer_use
  ; supports_code_execution =
      override_bool base.supports_code_execution overrides.supports_code_execution
  ; thinking_control_format =
      (match overrides.thinking_control_format with
       | Some s ->
         (match thinking_control_format_of_manifest_string s with
          | Some t -> t
          | None ->
            warn_unknown_capability_value ~field:"thinking_control_format" s;
            base.thinking_control_format)
       | None -> base.thinking_control_format)
  ; preserve_thinking_control_format =
      (match overrides.preserve_thinking_control_format with
       | Some s ->
         (match preserve_thinking_control_format_of_manifest_string s with
          | Some t -> t
          | None ->
            warn_unknown_capability_value ~field:"preserve_thinking_control_format" s;
            base.preserve_thinking_control_format)
       | None -> base.preserve_thinking_control_format)
  ; reasoning_output_format =
      (match overrides.reasoning_output_format with
       | Some s ->
         (match reasoning_output_format_of_catalog_string s with
          | Some format -> format
          | None ->
            warn_unknown_capability_value ~field:"reasoning_output_format" s;
            base.reasoning_output_format)
       | None -> base.reasoning_output_format)
  ; reasoning_streaming_format =
      (match overrides.reasoning_streaming_format with
       | Some s ->
         (match reasoning_streaming_format_of_catalog_string s with
          | Some format -> format
          | None ->
            warn_unknown_capability_value ~field:"reasoning_streaming_format" s;
            base.reasoning_streaming_format)
       | None -> base.reasoning_streaming_format)
  ; reasoning_replay_override =
      (match overrides.reasoning_replay with
       | Some s ->
         (match reasoning_replay_override_of_catalog_string s with
          | Some replay -> replay
          | None ->
            warn_unknown_capability_value ~field:"reasoning_replay" s;
            base.reasoning_replay_override)
       | None -> base.reasoning_replay_override)
  }
;;

let apply_manifest_entry (entry : Capability_manifest.entry) : capabilities =
  entry |> overrides_of_manifest_entry |> apply_declarative_capability_overrides
;;

let%test "apply_manifest_entry applies thinking_control_format (RFC-OAS-023)" =
  match
    Capability_manifest.of_json
      (Yojson.Safe.from_string
         {|{"schema_version":1,"models":[{"id_prefix":"m","thinking_control_format":"chat_template_kwargs"}]}|})
  with
  | Ok [ entry ] ->
    (apply_manifest_entry entry).thinking_control_format = Chat_template_kwargs
  | Ok _ | Error _ -> false
;;

let%test "apply_manifest_entry without thinking_control_format keeps base format" =
  match
    Capability_manifest.of_json
      (Yojson.Safe.from_string {|{"schema_version":1,"models":[{"id_prefix":"m"}]}|})
  with
  | Ok [ entry ] ->
    (apply_manifest_entry entry).thinking_control_format = No_thinking_control
  | Ok _ | Error _ -> false
;;

let%test "apply_manifest_entry applies reasoning_replay preserve_always" =
  match
    Capability_manifest.of_json
      (Yojson.Safe.from_string
         {|{"schema_version":1,"models":[{"id_prefix":"m","reasoning_replay":"preserve_always"}]}|})
  with
  | Ok [ entry ] ->
    (apply_manifest_entry entry).reasoning_replay_override = Force_preserve_always
  | Ok _ | Error _ -> false
;;

let%test "apply_manifest_entry applies assistant_tool_content_format" =
  match
    Capability_manifest.of_json
      (Yojson.Safe.from_string
         {|{"schema_version":1,"models":[{"id_prefix":"m","assistant_tool_content_format":"empty_string"}]}|})
  with
  | Ok [ entry ] ->
    (apply_manifest_entry entry).assistant_tool_content_format
    = Assistant_tool_content_empty_string
  | _ -> false
;;

let%test "apply_manifest_entry without reasoning_replay keeps base default" =
  match
    Capability_manifest.of_json
      (Yojson.Safe.from_string {|{"schema_version":1,"models":[{"id_prefix":"m"}]}|})
  with
  | Ok [ entry ] ->
    (apply_manifest_entry entry).reasoning_replay_override = Default_reasoning_replay
  | Ok _ | Error _ -> false
;;

let%test "reasoning_replay_override_of_catalog_string parses vocabulary" =
  reasoning_replay_override_of_catalog_string "preserve_always"
  = Some Force_preserve_always
  && reasoning_replay_override_of_catalog_string "drop_without_tool"
     = Some Force_drop_without_tool_preserve_with_tool
  && reasoning_replay_override_of_catalog_string "no_replay" = Some Force_no_replay
  && reasoning_replay_override_of_catalog_string "" = Some Default_reasoning_replay
  && reasoning_replay_override_of_catalog_string "bogus" = None
;;

let overrides_of_catalog_entry (entry : Model_catalog.model_entry) =
  { base_label = entry.base_label
  ; max_context_tokens = entry.max_context_tokens
  ; max_output_tokens = entry.max_output_tokens
  ; supports_tools = entry.supports_tools
  ; supports_tool_choice = entry.supports_tool_choice
  ; supports_required_tool_choice = entry.supports_required_tool_choice
  ; supports_named_tool_choice = entry.supports_named_tool_choice
  ; supports_parallel_tool_calls = entry.supports_parallel_tool_calls
  ; assistant_tool_content_format = entry.assistant_tool_content_format
  ; supports_reasoning = entry.supports_reasoning
  ; supports_extended_thinking = entry.supports_extended_thinking
  ; supports_reasoning_budget = entry.supports_reasoning_budget
  ; accepted_reasoning_efforts = entry.accepted_reasoning_efforts
  ; supports_response_format_json = entry.supports_response_format_json
  ; supports_structured_output = entry.supports_structured_output
  ; supports_multimodal_inputs = entry.supports_multimodal_inputs
  ; supports_image_input = entry.supports_image_input
  ; supports_audio_input = entry.supports_audio_input
  ; supports_video_input = entry.supports_video_input
  ; modality_priority = entry.modality_priority
  ; task = entry.task
  ; supports_native_streaming = entry.supports_native_streaming
  ; supports_system_prompt = entry.supports_system_prompt
  ; supports_caching = entry.supports_caching
  ; supports_prompt_caching = entry.supports_prompt_caching
  ; supports_top_k = entry.supports_top_k
  ; supports_min_p = entry.supports_min_p
  ; supports_seed = entry.supports_seed
  ; supports_computer_use = entry.supports_computer_use
  ; supports_code_execution = entry.supports_code_execution
  ; thinking_control_format = entry.thinking_control_format
  ; preserve_thinking_control_format = entry.preserve_thinking_control_format
  ; reasoning_output_format = entry.reasoning_output_format
  ; reasoning_streaming_format = entry.reasoning_streaming_format
  ; reasoning_replay = entry.reasoning_replay
  }
;;

let apply_catalog_entry (entry : Model_catalog.model_entry) : capabilities =
  entry |> overrides_of_catalog_entry |> apply_declarative_capability_overrides
;;

(** Look up capabilities for [model_id] in the loaded model catalog only.

    The catalog itself is resolved by {!Model_catalog.global}, in order:
    runtime override installed via {!Model_catalog.set_global}, then the
    [OAS_MODEL_CATALOG] environment variable, then the packaged default
    [models.toml]. Ambient discovery is cached after first load; embedding
    hosts and test harnesses can call [Model_catalog.preload_global], inject
    [OAS_MODEL_CATALOG] during bootstrap, or install an explicit runtime
    override.

    Returns [None] when no catalog is available or when the catalog has
    no entry whose [id_prefix] matches [model_id]. There is no in-code
    fallback table: the former built-in static table was removed when
    model specifications were externalized to the TOML catalog. *)
let provider_qualified_separators = [ "/"; ":"; "." ]

let model_id_has_provider_label ~provider_label ~model_id =
  let provider_label = String.lowercase_ascii (String.trim provider_label) in
  let model_id = String.lowercase_ascii (String.trim model_id) in
  provider_label <> ""
  && List.exists
       (fun separator -> String.starts_with ~prefix:(provider_label ^ separator) model_id)
       provider_qualified_separators
;;

let provider_qualified_model_id_candidates ~provider_label ~model_id =
  let normalized_model_id = String.lowercase_ascii model_id in
  let provider_prefixes =
    List.map (fun separator -> provider_label ^ separator) provider_qualified_separators
  in
  let stripped =
    List.find_map
      (fun prefix ->
         if String.starts_with ~prefix normalized_model_id
         then (
           let prefix_len = String.length prefix in
           Some (String.sub model_id prefix_len (String.length model_id - prefix_len)))
         else None)
      provider_prefixes
  in
  (* When [model_id] is already provider-qualified, only the stripped bare
     suffix is a useful candidate for re-qualification below in
     [provider_qualified_catalog_keys]. Including the original [model_id]
     alongside it used to make that function re-prepend [provider_label] onto
     an already-qualified id (e.g. "vllm-qwen3-mtp/vllm-qwen3-mtp.qwen..."), which
     never matches a real catalog [id_prefix] — pure wasted lookups. *)
  match stripped with
  | Some suffix when String.trim suffix <> "" -> [ suffix ]
  | Some _ | None -> [ model_id ]
;;

let provider_qualified_catalog_keys ~provider_label ~model_id =
  let provider_label = String.lowercase_ascii (String.trim provider_label) in
  let model_id = String.trim model_id in
  let keys =
    provider_qualified_model_id_candidates ~provider_label ~model_id
    |> List.concat_map (fun model_id ->
      List.map
        (fun separator -> provider_label ^ separator ^ model_id)
        provider_qualified_separators)
  in
  let prefixes =
    List.map (fun separator -> provider_label ^ separator) provider_qualified_separators
  in
  keys, prefixes
;;

let for_model_id_catalog model_id =
  match Model_catalog.global () with
  | Some catalog ->
    (match Model_catalog.lookup catalog model_id with
     | Some entry -> Some (apply_catalog_entry entry)
     | None -> None)
  | None -> None
;;

(** Look up capabilities for [model_id] against an explicit manifest,
    falling back to the catalog lookup ({!for_model_id_catalog}) when no
    manifest entry matches. *)
let for_model_id_with_manifest manifest model_id =
  match Capability_manifest.lookup manifest model_id with
  | Some entry -> Some (apply_manifest_entry entry)
  | None -> for_model_id_catalog model_id
;;

(** Look up capabilities for [model_id].

    Checks the globally loaded model catalog first, then the capability
    manifest. Returns [None] when neither source has a matching entry;
    there is no built-in fallback table. *)
let for_model_id model_id =
  match Model_catalog.global () with
  | Some catalog ->
    (match Model_catalog.lookup catalog model_id with
     | Some entry -> Some (apply_catalog_entry entry)
     | None ->
       (match Capability_manifest.global () with
        | Some manifest -> for_model_id_with_manifest manifest model_id
        | None -> for_model_id_catalog model_id))
  | None ->
    (match Capability_manifest.global () with
     | Some manifest -> for_model_id_with_manifest manifest model_id
     | None -> for_model_id_catalog model_id)
;;

let for_provider_model_id_catalog ~(provider_label : string) ~(model_id : string) =
  let candidates, qualified_prefixes =
    provider_qualified_catalog_keys ~provider_label ~model_id
  in
  match Model_catalog.global () with
  | None -> None
  | Some catalog ->
    let rec loop = function
      | [] -> None
      | candidate :: rest ->
        (match Model_catalog.lookup catalog candidate with
         | Some entry
           when List.exists
                  (fun prefix ->
                     String.starts_with
                       ~prefix
                       (String.lowercase_ascii (String.trim entry.id_prefix)))
                  qualified_prefixes -> Some (apply_catalog_entry entry)
         | Some _ | None -> loop rest)
    in
    loop candidates
;;

let for_provider_model_id
      ~(allow_bare_fallback : bool)
      ~(provider_label : string)
      ~(model_id : string)
  =
  match for_provider_model_id_catalog ~provider_label ~model_id with
  | Some _ as caps -> caps
  | None -> if allow_bare_fallback then for_model_id model_id else None
;;

let exact_token = function
  | Some raw when String.trim raw = "" || raw <> String.trim raw -> None
  | Some raw -> Some raw
  | None -> None
;;

let thinking_control_token_for_model_id_catalog model_id =
  match Model_catalog.global () with
  | Some catalog ->
    (match Model_catalog.lookup catalog model_id with
     | Some entry -> exact_token entry.thinking_control_token
     | None -> None)
  | None -> None
;;

let thinking_control_token_for_model_id_with_manifest manifest model_id =
  match Capability_manifest.lookup manifest model_id with
  | Some entry -> exact_token entry.thinking_control_token
  | None -> thinking_control_token_for_model_id_catalog model_id
;;

let thinking_control_token_for_model_id model_id =
  match Model_catalog.global () with
  | Some catalog ->
    (match Model_catalog.lookup catalog model_id with
     | Some entry -> exact_token entry.thinking_control_token
     | None ->
       (match Capability_manifest.global () with
        | Some manifest ->
          thinking_control_token_for_model_id_with_manifest manifest model_id
        | None -> thinking_control_token_for_model_id_catalog model_id))
  | None ->
    (match Capability_manifest.global () with
     | Some manifest ->
       thinking_control_token_for_model_id_with_manifest manifest model_id
     | None -> thinking_control_token_for_model_id_catalog model_id)
;;

let thinking_control_token_for_provider_model_id
      ~(provider_label : string)
      ~(model_id : string)
  =
  let candidates, qualified_prefixes =
    provider_qualified_catalog_keys ~provider_label ~model_id
  in
  let provider_catalog_match =
    match Model_catalog.global () with
    | None -> None
    | Some catalog ->
      let rec loop = function
        | [] -> None
        | candidate :: rest ->
          (match Model_catalog.lookup catalog candidate with
           | Some entry
             when List.exists
                    (fun prefix ->
                       String.starts_with
                         ~prefix
                         (String.lowercase_ascii (String.trim entry.id_prefix)))
                    qualified_prefixes -> Some (exact_token entry.thinking_control_token)
           | Some _ | None -> loop rest)
      in
      loop candidates
  in
  match provider_catalog_match with
  | Some token -> token
  | None -> thinking_control_token_for_model_id model_id
;;

[@@@coverage off]

let%test "for_model_id glm-4.5 has reasoning" =
  match for_model_id "glm-4.5" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 128_000
    && c.max_output_tokens = Some 96_000
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

let%test "gemini minimal thinking level is gemini-3.1-flash-lite only" =
  let supports_minimal id =
    match gemini_thinking_control_of_id id with
    | Gemini_thinking_level { supports_minimal } -> supports_minimal
    | Gemini_thinking_budget | Gemini_unknown_thinking_control -> false
  in
  supports_minimal "gemini-3.1-flash-lite"
  && supports_minimal "gemini-3.1-flash-lite-preview"
  && (not (supports_minimal "gemini-3.1-flash"))
  && (not (supports_minimal "gemini-3.1-pro"))
  && (not (supports_minimal "gemini-3-flash-preview"))
  && not (supports_minimal "gemini-3-pro-preview")
;;

let%test "gemini 2.5 uses thinking budget, not a thinking level" =
  match gemini_thinking_control_of_id "gemini-2.5-flash" with
  | Gemini_thinking_budget -> true
  | Gemini_thinking_level _ | Gemini_unknown_thinking_control -> false
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
    c.supports_reasoning && c.supports_image_input && c.max_output_tokens = Some 16_384
  | None -> false
;;

let%test "for_model_id glm-4.7-flashx has GLM-4.7 thinking limits" =
  match for_model_id "glm-4.7-flashx" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 200_000
    && c.max_output_tokens = Some 128_000
    && c.supports_tools
  | None -> false
;;

let%test "for_model_id glm-4.7-flash has thinking" =
  match for_model_id "glm-4.7-flash" with
  | Some c -> c.supports_reasoning && c.max_output_tokens = Some 128_000
  | None -> false
;;

let%test "for_model_id glm-4.5-flash has GLM-4.5 thinking limits" =
  match for_model_id "glm-4.5-flash" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && c.max_context_tokens = Some 128_000
    && c.max_output_tokens = Some 96_000
    && c.supports_tools
  | None -> false
;;

(* [for_model_id_catalog] tests below install an explicit catalog through
   [Model_catalog.set_global] and restore the override afterwards, so they
   are insulated from BOTH ambient manifest overrides
   ([OAS_CAPABILITY_MANIFEST]) and ambient catalog discovery
   ([OAS_MODEL_CATALOG]). CI injects the repository [models.toml] through
   [OAS_MODEL_CATALOG] for inline tests that exercise the production catalog.

   [test_catalog_entry] fills every field with [None]; each fixture entry
   then sets only the capability-relevant fields, mirroring the
   corresponding [models.toml] entries at the time of writing. The fixture
   is a deterministic snapshot, not auto-synced with [models.toml]. *)
let test_catalog_entry id_prefix : Model_catalog.model_entry =
  { id_prefix
  ; base_label = None
  ; provider_name = None
  ; max_context_tokens = None
  ; max_output_tokens = None
  ; supports_tools = None
  ; supports_tool_choice = None
  ; supports_required_tool_choice = None
  ; supports_named_tool_choice = None
  ; supports_parallel_tool_calls = None
  ; assistant_tool_content_format = None
  ; supports_reasoning = None
  ; supports_extended_thinking = None
  ; supports_reasoning_budget = None
  ; accepted_reasoning_efforts = None
  ; supports_response_format_json = None
  ; supports_structured_output = None
  ; supports_multimodal_inputs = None
  ; supports_image_input = None
  ; supports_audio_input = None
  ; supports_video_input = None
  ; modality_priority = None
  ; task = None
  ; supports_native_streaming = None
  ; supports_system_prompt = None
  ; supports_caching = None
  ; supports_prompt_caching = None
  ; supports_top_k = None
  ; supports_min_p = None
  ; supports_seed = None
  ; supports_computer_use = None
  ; supports_code_execution = None
  ; thinking_control_format = None
  ; thinking_control_token = None
  ; preserve_thinking_control_format = None
  ; reasoning_output_format = None
  ; reasoning_streaming_format = None
  ; reasoning_replay = None
  ; input_per_million = None
  ; output_per_million = None
  ; cache_write_multiplier = None
  ; cache_read_multiplier = None
  }
;;

let test_manifest_entry id_prefix : Capability_manifest.entry =
  { id_prefix
  ; base_label = None
  ; max_context_tokens = None
  ; max_output_tokens = None
  ; supports_tools = None
  ; supports_tool_choice = None
  ; supports_required_tool_choice = None
  ; supports_named_tool_choice = None
  ; supports_parallel_tool_calls = None
  ; assistant_tool_content_format = None
  ; supports_reasoning = None
  ; supports_extended_thinking = None
  ; supports_reasoning_budget = None
  ; accepted_reasoning_efforts = None
  ; supports_response_format_json = None
  ; supports_structured_output = None
  ; supports_multimodal_inputs = None
  ; supports_image_input = None
  ; supports_audio_input = None
  ; supports_video_input = None
  ; supports_native_streaming = None
  ; supports_system_prompt = None
  ; supports_caching = None
  ; supports_prompt_caching = None
  ; supports_top_k = None
  ; supports_min_p = None
  ; supports_seed = None
  ; supports_computer_use = None
  ; supports_code_execution = None
  ; thinking_control_format = None
  ; thinking_control_token = None
  ; preserve_thinking_control_format = None
  ; reasoning_output_format = None
  ; reasoning_streaming_format = None
  ; reasoning_replay = None
  }
;;

let qwen3_family_test_entry id_prefix : Model_catalog.model_entry =
  { (test_catalog_entry id_prefix) with
    base_label = Some "openai_chat"
  ; max_context_tokens = Some 262_144
  ; max_output_tokens = Some 81_920
  ; supports_tools = Some true
  ; supports_tool_choice = Some true
  ; supports_required_tool_choice = Some true
  ; supports_named_tool_choice = Some true
  ; supports_reasoning = Some true
  ; supports_extended_thinking = Some true
  ; thinking_control_format = Some "chat_template_kwargs"
  ; preserve_thinking_control_format = Some "chat_template_kwargs_preserve_thinking"
  ; supports_native_streaming = Some true
  }
;;

let glm_thinking_test_entry id_prefix ~ctx ~out : Model_catalog.model_entry =
  { (test_catalog_entry id_prefix) with
    base_label = Some "glm"
  ; max_context_tokens = Some ctx
  ; max_output_tokens = Some out
  ; supports_tools = Some true
  ; supports_tool_choice = Some false
  ; supports_required_tool_choice = Some false
  ; supports_named_tool_choice = Some false
  ; supports_reasoning = Some true
  ; supports_extended_thinking = Some true
  ; supports_native_streaming = Some true
  }
;;

let deepseek_v4_test_entry id_prefix : Model_catalog.model_entry =
  { (test_catalog_entry id_prefix) with
    base_label = Some "openai_chat"
  ; max_context_tokens = Some 1_000_000
  ; max_output_tokens = Some 384_000
  ; supports_tools = Some true
  ; supports_tool_choice = Some true
  ; (* DeepSeek V4 thinking mode (the default for these thinking_object rows)
       400s on forced tool_choice — both required and named function choice.
       Mirrors the models.toml entries. Ref 2026-06-30: deepseek-ai/DeepSeek-V3
       issue #1376, api-docs.deepseek.com/guides/function_calling. *)
    supports_required_tool_choice = Some false
  ; (* Same DeepSeek V4 thinking-mode forced tool_choice rejection, split by
       required vs named capability. *)
    supports_named_tool_choice = Some false
  ; supports_reasoning = Some true
  ; supports_extended_thinking = Some true
  ; thinking_control_format = Some "thinking_object"
  ; supports_native_streaming = Some true
  }
;;

let gemma4_openai_test_entry id_prefix : Model_catalog.model_entry =
  { (test_catalog_entry id_prefix) with
    base_label = Some "openai_chat"
  ; max_context_tokens = Some 262_144
  ; supports_tools = Some true
  ; supports_tool_choice = Some true
  ; supports_required_tool_choice = Some true
  ; supports_named_tool_choice = Some true
  ; supports_multimodal_inputs = Some true
  ; supports_image_input = Some true
  ; modality_priority = Some "visual_first"
  ; supports_native_streaming = Some true
  ; supports_seed = Some true
  }
;;

let test_catalog : Model_catalog.t =
  [ qwen3_family_test_entry "qwen3"
  ; qwen3_family_test_entry "qwen-3"
  ; qwen3_family_test_entry "qwen/qwen3.6"
  ; glm_thinking_test_entry "glm-5.1" ~ctx:200_000 ~out:128_000
  ; glm_thinking_test_entry "glm-5-turbo" ~ctx:200_000 ~out:128_000
  ; { (glm_thinking_test_entry "glm-5v-turbo" ~ctx:200_000 ~out:128_000) with
      supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    }
  ; { (test_catalog_entry "glm-ocr") with
      base_label = Some "glm"
    ; max_context_tokens = Some 128_000
    ; max_output_tokens = Some 16_384
    ; supports_tools = Some false
    ; supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    }
  ; glm_thinking_test_entry "glm-4.7-flashx" ~ctx:200_000 ~out:128_000
  ; glm_thinking_test_entry "glm-4.7-flash" ~ctx:200_000 ~out:128_000
  ; glm_thinking_test_entry "glm-4.5-air" ~ctx:128_000 ~out:96_000
  ; glm_thinking_test_entry "glm-4.5-flash" ~ctx:128_000 ~out:96_000
  ; glm_thinking_test_entry "glm-5" ~ctx:200_000 ~out:128_000
  ; glm_thinking_test_entry "glm-4.6" ~ctx:200_000 ~out:128_000
  ; glm_thinking_test_entry "glm-4.7" ~ctx:200_000 ~out:128_000
  ; glm_thinking_test_entry "glm-4.5" ~ctx:128_000 ~out:96_000
  ; { (glm_thinking_test_entry "glm-4.6v" ~ctx:128_000 ~out:32_768) with
      supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    }
  ; { (test_catalog_entry "glm-4-flash") with
      max_context_tokens = Some 128_000
    ; max_output_tokens = Some 4_096
    ; supports_tools = Some true
    }
  ; { (test_catalog_entry "glm-4v") with
      max_context_tokens = Some 128_000
    ; max_output_tokens = Some 4_096
    ; supports_tools = Some true
    ; supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    }
  ; { (test_catalog_entry "glm-4") with
      max_context_tokens = Some 128_000
    ; max_output_tokens = Some 4_096
    ; supports_tools = Some true
    }
  ; deepseek_v4_test_entry "deepseek-v4-pro"
  ; deepseek_v4_test_entry "deepseek-ai/deepseek-v4-pro"
  ; deepseek_v4_test_entry "deepseek-v4-flash"
  ; deepseek_v4_test_entry "deepseek-ai/deepseek-v4-flash"
  ; { (test_catalog_entry "nvidia/nvidia-vl") with
      base_label = Some "nvidia"
    ; max_context_tokens = Some 131_072
    ; max_output_tokens = Some 16_384
    ; supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    }
  ; { (test_catalog_entry "nvidia-vl") with
      base_label = Some "nvidia"
    ; max_context_tokens = Some 131_072
    ; max_output_tokens = Some 16_384
    ; supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    }
  ; { (test_catalog_entry "nvidia/nvidia") with
      base_label = Some "nvidia"
    ; max_context_tokens = Some 131_072
    ; max_output_tokens = Some 16_384
    }
  ; { (test_catalog_entry "nvidia") with
      base_label = Some "nvidia"
    ; max_context_tokens = Some 131_072
    ; max_output_tokens = Some 16_384
    }
  ; gemma4_openai_test_entry "google/gemma-4"
  ; gemma4_openai_test_entry "google/gemma-4-26b-a4b"
  ; { (test_catalog_entry "hf.co/unsloth/gemma-4") with
      base_label = Some "ollama"
    ; max_context_tokens = Some 262_144
    ; supports_tools = Some true
    ; supports_tool_choice = Some false
    ; supports_named_tool_choice = Some false
    ; supports_reasoning = Some true
    ; supports_extended_thinking = Some true
    ; thinking_control_format = Some "chat_template_token"
    ; thinking_control_token = Some "<|think|>"
    ; supports_multimodal_inputs = Some true
    ; supports_image_input = Some true
    ; modality_priority = Some "visual_first"
    ; supports_seed = Some true
    }
  ; { (test_catalog_entry "claude-opus-4") with
      base_label = Some "anthropic"
    ; max_context_tokens = Some 1_000_000
    ; max_output_tokens = Some 128_000
    }
  ; { (test_catalog_entry "gpt-4.1") with
      base_label = Some "openai_chat"
    ; max_context_tokens = Some 1_000_000
    ; max_output_tokens = Some 32_000
    }
  ]
;;

(* Installs [test_catalog] as the runtime override for the duration of [f],
   then clears the override (which falls back to ambient discovery, the
   pre-test state in this runner). *)
let with_test_catalog f =
  Model_catalog.set_global test_catalog;
  Fun.protect ~finally:Model_catalog.clear_global f
;;

let%test "for_model_id_catalog qwen3 has extended thinking" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "qwen3-32b" with
    | Some c ->
      c.supports_reasoning
      && c.supports_extended_thinking
      && c.supports_tools
      && c.supports_native_streaming
    | None -> false)
;;

let%test "for_model_id_catalog qwen3.5 routes to Qwen_3 family" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "qwen3.5" with
    | Some c -> c.supports_extended_thinking && c.max_output_tokens = Some 81_920
    | None -> false)
;;

let%test "for_model_id_catalog qwen36 model routes to Qwen_3 family" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "qwen36-35b-a3b-mtp" with
    | Some c ->
      c.supports_extended_thinking && c.thinking_control_format = Chat_template_kwargs
    | None -> false)
;;

let%test "for_model_id_catalog qwen-3-7b prefix variant resolves" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "qwen-3-7b-instruct" with
    | Some c -> c.supports_extended_thinking
    | None -> false)
;;

let%test "for_model_id_catalog glm-5-turbo has GLM-5 thinking limits" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "glm-5-turbo" with
    | Some c ->
      c.supports_reasoning
      && c.supports_extended_thinking
      && c.max_context_tokens = Some 200_000
      && c.max_output_tokens = Some 128_000
    | None -> false)
;;

let%test "for_model_id_catalog glm-5.1 full model (reasoning + extended thinking)" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "glm-5.1" with
    | Some c ->
      c.supports_reasoning
      && c.supports_extended_thinking
      && c.max_output_tokens = Some 128_000
    | None -> false)
;;

let%test "for_model_id_catalog bare glm-5 full model (reasoning + extended thinking)" =
  with_test_catalog (fun () ->
    match for_model_id_catalog "glm-5" with
    | Some c ->
      c.supports_reasoning
      && c.supports_extended_thinking
      && c.max_output_tokens = Some 128_000
    | None -> false)
;;

let%test "for_model_id_catalog catalog miss returns None" =
  with_test_catalog (fun () ->
    Option.is_none (for_model_id_catalog "totally-unknown-model-id"))
;;

let%test "for_model_id_catalog empty catalog returns None" =
  Model_catalog.set_global [];
  Fun.protect ~finally:Model_catalog.clear_global (fun () ->
    Option.is_none (for_model_id_catalog "qwen3-32b"))
;;

let%test "thinking_control_token catalog runtime override rejects blank or padded token" =
  Model_catalog.set_global
    [ { (test_catalog_entry "programmatic-blank-catalog-token") with
        thinking_control_token = Some " \t "
      }
    ; { (test_catalog_entry "programmatic-padded-catalog-token") with
        thinking_control_token = Some " <|think|> "
      }
    ; { (test_catalog_entry "programmatic-exact-catalog-token") with
        thinking_control_token = Some "<|think|>"
      }
    ];
  Fun.protect ~finally:Model_catalog.clear_global (fun () ->
    Option.is_none
      (thinking_control_token_for_model_id "programmatic-blank-catalog-token")
    && Option.is_none
         (thinking_control_token_for_model_id "programmatic-padded-catalog-token")
    && thinking_control_token_for_model_id "programmatic-exact-catalog-token"
       = Some "<|think|>")
;;

let%test "thinking_control_token manifest runtime override rejects blank or padded token" =
  Capability_manifest.set_global
    [ { (test_manifest_entry "programmatic-blank-manifest-token") with
        thinking_control_token = Some " \t "
      }
    ; { (test_manifest_entry "programmatic-padded-manifest-token") with
        thinking_control_token = Some " <|think|> "
      }
    ; { (test_manifest_entry "programmatic-exact-manifest-token") with
        thinking_control_token = Some "<|think|>"
      }
    ];
  Fun.protect ~finally:Capability_manifest.clear_global (fun () ->
    Option.is_none
      (thinking_control_token_for_model_id "programmatic-blank-manifest-token")
    && Option.is_none
         (thinking_control_token_for_model_id "programmatic-padded-manifest-token")
    && thinking_control_token_for_model_id "programmatic-exact-manifest-token"
       = Some "<|think|>")
;;

(* --- emits_usage_tokens / capabilities_for_provider_label --- *)

let%test "emits_usage_tokens: default is true" = default_capabilities.emits_usage_tokens

let%test "emits_usage_tokens: anthropic reports usage" =
  anthropic_capabilities.emits_usage_tokens
;;

let%test "emits_usage_tokens: ollama reports usage" =
  ollama_capabilities.emits_usage_tokens
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

(* --- Nvidia / Gemma 4 --- *)

let%test "provider_l_capabilities has chat_template_kwargs thinking" =
  provider_l_capabilities.thinking_control_format = Chat_template_kwargs
;;

let%test "for_model_id nvidia-ultra has reasoning" =
  match for_model_id "nvidia-ultra-253b" with
  | Some c -> c.supports_reasoning && c.supports_tool_choice
  | None -> false
;;

let%test "for_model_id nvidia-vl has image input" =
  match for_model_id "nvidia-vl" with
  | Some c -> c.supports_image_input && c.supports_multimodal_inputs
  | None -> false
;;

let%test "for_model_id qwen3 has chat_template_kwargs thinking control" =
  (* DashScope_3.x OpenAI-compatible llama.cpp/llama-server deployments return
     [reasoning_content] when thinking is enabled through
     {"chat_template_kwargs": {"enable_thinking": bool}}.  Without this
     format, [supports_extended_thinking = true] never reaches the wire. *)
  match for_model_id "dashscope-3.5" with
  | Some c ->
    c.supports_reasoning_budget && c.thinking_control_format = Chat_template_kwargs
  | None -> false
;;

let%test "for_model_id nvidia/nvidia-core resolves" =
  match for_model_id "nvidia/nvidia-core" with
  | Some c -> c.supports_reasoning
  | None -> false
;;

let%test "for_model_id google/gemma-4-26b-a4b has tools + seed" =
  match for_model_id "google/gemma-4-26B-A4B-it" with
  | Some c ->
    c.supports_tools
    && c.supports_seed
    && c.supports_image_input
    && c.max_context_tokens = Some 262_144
    && c.modality_priority = Modality.Visual_first
  | None -> false
;;

let%test "for_model_id google/gemma-4-e2b-it has tools, audio, and 128K context" =
  match for_model_id "google/gemma-4-E2B-it" with
  | Some c ->
    c.supports_tools
    && c.supports_image_input
    && c.supports_audio_input
    && c.max_context_tokens = Some 131_072
  | None -> false
;;

let%test "for_model_id hf.co/unsloth Gemma 4 QAT uses template token thinking" =
  match for_model_id "hf.co/unsloth/gemma-4-26B-A4B-it-qat-GGUF:UD-Q4_K_XL" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && (not c.supports_reasoning_budget)
    && c.thinking_control_format = Chat_template_token
    && c.modality_priority = Modality.Visual_first
  | None -> false
;;

let%test "for_model_id hf.co/google Gemma 4 QAT uses template token thinking" =
  match for_model_id "hf.co/google/gemma-4-26B-A4B-it-qat-q4_0-gguf" with
  | Some c ->
    c.supports_reasoning
    && c.supports_extended_thinking
    && (not c.supports_reasoning_budget)
    && c.thinking_control_format = Chat_template_token
  | None -> false
;;

let%test "for_model_id google/gemma-4-e2b-it is not a 256K model" =
  match for_model_id "google/gemma-4-E2B-it" with
  | Some c -> c.max_context_tokens = Some 131_072
  | None -> false
;;

let%test "for_model_id google/gemma-4-31b-it resolves" =
  match for_model_id "google/gemma-4-31B-it" with
  | Some c -> c.supports_tools && c.supports_image_input
  | None -> false
;;

let%test "for_model_id Qwen org-prefixed Qwen3.6 uses chat_template_kwargs" =
  match for_model_id "Qwen/Qwen3.6-35B-A3B" with
  | Some c ->
    c.supports_extended_thinking && c.thinking_control_format = Chat_template_kwargs
  | None -> false
;;

let%test "capabilities_for_provider_label: nvidia" =
  match capabilities_for_provider_label "nvidia" with
  | Some c -> c.thinking_control_format = Chat_template_kwargs
  | None -> false
;;

(* ── Prefix ordering invariant ──────────────────── *)

(* Each case is a model_id and the expected capability fingerprint.
   Prefix dispatch now lives in [Model_catalog.lookup], which must pick the
   LONGEST matching [id_prefix]. If that ordering regressed, these specific
   models would be matched by a more general prefix (e.g. [glm-4] instead
   of [glm-4.7-flashx]) and return wrong capabilities. The test catches
   that against the explicit [test_catalog] fixture, which contains both
   the general and the specific prefixes. *)
let%test
    "for_model_id_catalog: specific model IDs get correct (not shadowed) capabilities"
  =
  with_test_catalog (fun () ->
    let check model_id expected =
      match for_model_id_catalog model_id with
      | Some c -> expected c
      | None -> false
    in
    List.for_all
      (fun (m, e) -> check m e)
      [ ( "glm-4.7-flash-turbo"
        , fun c -> c.max_output_tokens = Some 128_000 && c.supports_extended_thinking )
      ; ( "glm-4.5-flash-test"
        , fun c -> c.max_output_tokens = Some 96_000 && c.supports_extended_thinking )
      ; ( "glm-5-turbo-latest"
        , fun c -> c.max_output_tokens = Some 128_000 && c.supports_extended_thinking )
      ; ( "glm-5-turbo-latest"
        , fun c -> c.max_output_tokens = Some 128_000 && c.supports_extended_thinking )
      ; ("glm-4.6v-plus", fun c -> c.supports_image_input && c.supports_reasoning)
      ; ( "glm-4.7-flash-test"
        , fun c -> c.max_output_tokens = Some 128_000 && c.supports_reasoning )
      ; ( "glm-4.7-flashx"
        , fun c -> c.max_output_tokens = Some 128_000 && c.supports_reasoning )
      ; ( "glm-4-flash-mini"
        , fun c -> c.max_output_tokens = Some 4_096 && not c.supports_reasoning )
      ; ("glm-4v-plus", fun c -> c.supports_image_input)
      ; ( "glm-4.5-air-test"
        , fun c -> c.max_output_tokens = Some 96_000 && c.supports_reasoning )
      ; ( "glm-4.5-air-test"
        , fun c -> c.max_output_tokens = Some 96_000 && c.supports_reasoning )
      ; ( "glm-5v-turbo-latest"
        , fun c ->
            c.supports_image_input
            && c.supports_reasoning
            && c.max_output_tokens = Some 128_000 )
      ; ( "glm-5v-turbo-latest"
        , fun c ->
            c.supports_image_input
            && c.supports_reasoning
            && c.max_output_tokens = Some 128_000 )
      ; ("glm-ocr-test", fun c -> c.supports_image_input && not c.supports_tools)
      ; ("glm-ocr-test", fun c -> c.supports_image_input && not c.supports_tools)
      ; ("claude-opus-4-20250501", fun c -> c.max_output_tokens = Some 128_000)
      ; ("gpt-4.1-mini", fun c -> c.max_output_tokens = Some 32_000)
        (* RFC-OAS-023: the real provider model ids ([deepseek-v4-flash],
         [deepseek-v4-pro]) must resolve to the DeepSeek capability route.
         Before the de-anonymization these matched only the anon
         [deepseek-v4-*] prefixes, so the concrete ids missed the lookup
         and silently fell back to provider-default capabilities. The
         anon prefixes are retained as aliases and asserted here too. The
         fingerprint ([Thinking_object] + [384_000] output cap) is specific
         to the DeepSeek record, so a fall-through to defaults fails the
         test. *)
      ; ( "deepseek-v4-flash"
        , fun c ->
            c.thinking_control_format = Thinking_object
            && c.max_output_tokens = Some 384_000 )
      ; ( "deepseek-v4-flash-test"
        , fun c ->
            c.thinking_control_format = Thinking_object
            && c.max_output_tokens = Some 384_000 )
      ; ( "deepseek-v4-pro"
        , fun c ->
            c.thinking_control_format = Thinking_object
            && c.max_output_tokens = Some 384_000 )
      ; ( "deepseek-ai/DeepSeek-V4-Flash"
        , fun c ->
            c.thinking_control_format = Thinking_object
            && c.max_output_tokens = Some 384_000 )
      ; ( "deepseek-ai/DeepSeek-V4-Pro"
        , fun c ->
            c.thinking_control_format = Thinking_object
            && c.max_output_tokens = Some 384_000 )
      ; ( "Qwen/Qwen3.6-35B-A3B"
        , fun c ->
            c.thinking_control_format = Chat_template_kwargs
            && c.supports_extended_thinking )
      ; ( "deepseek-v4-pro-test"
        , fun c ->
            c.thinking_control_format = Thinking_object
            && c.max_output_tokens = Some 384_000 )
      ; ( "nvidia-ultra-253b"
        , fun c ->
            c.thinking_control_format = Chat_template_kwargs && c.supports_tool_choice )
      ; ( "nvidia/nvidia-ultra-253b"
        , fun c ->
            c.thinking_control_format = Chat_template_kwargs && c.supports_tool_choice )
      ; ("nvidia-vl", fun c -> c.supports_image_input && c.supports_multimodal_inputs)
      ; ( "google/gemma-4-26B-A4B-it"
        , fun c ->
            c.supports_tools
            && c.supports_image_input
            && c.supports_seed
            && c.modality_priority = Modality.Visual_first
            && c.max_context_tokens = Some 262_144 )
      ; ( "hf.co/unsloth/gemma-4-26B-A4B-it-qat-GGUF:UD-Q4_K_XL"
        , fun c -> c.supports_reasoning && c.thinking_control_format = Chat_template_token
        )
      ])
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
       | Thinking _ | ReasoningDetails _ | RedactedThinking _ -> has_thinking := true
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
      && ca.accepted_reasoning_efforts = cb.accepted_reasoning_efforts
      && ca.preserve_thinking_control_format = cb.preserve_thinking_control_format
      && ca.reasoning_output_format = cb.reasoning_output_format
      && ca.reasoning_streaming_format = cb.reasoning_streaming_format
      && ca.assistant_tool_content_format = cb.assistant_tool_content_format
      && ca.reasoning_replay_override = cb.reasoning_replay_override
    | _ -> false
  in
  let alias_pairs = [ "openai", "openai_chat"; "glm", "glm-coding" ] in
  List.for_all (fun (a, b) -> same_base a b) alias_pairs
  && Option.is_some (resolve "anthropic")
  && Option.is_some (resolve "gemini")
  && Option.is_some (resolve "ollama")
  && Option.is_some (resolve "kimi")
  && Option.is_some (resolve "xai")
  && Option.is_some (resolve "mistral")
  && Option.is_some (resolve "cohere")
  && Option.is_some (resolve "mimo")
  && Option.is_some (resolve "nvidia")
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
    ; "xai"
    ; "mistral"
    ; "cohere"
    ; "mimo"
    ; "nvidia"
    ; "kimi"
    ]
  in
  List.for_all (fun l -> Option.is_some (capabilities_for_provider_label l)) labels
;;

(* Every label resolves to a distinct capability fingerprint unless
   explicitly aliased. Catches accidental capability merging. *)
let%test
    "capabilities_for_provider_label: no accidental aliasing across distinct providers"
  =
  let non_aliased = [ "anthropic"; "gemini"; "ollama"; "kimi"; "nvidia" ] in
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
