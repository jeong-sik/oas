(** Consolidated magic numbers for the llm_provider library.

    Centralises retry codes, inference defaults, cache TTL, retry
    parameters, jitter range, and sampling params so they are defined
    once and referenced everywhere.

    @since 0.99.0 *)

(* ── HTTP retry / handoff codes ──────────────────── *)

module Env = struct
  let max_tokens_default = "OAS_MAX_TOKENS_DEFAULT"
  let thinking_budget_default = "OAS_THINKING_BUDGET_DEFAULT"
  let anthropic_thinking_budget = "OAS_ANTHROPIC_THINKING_BUDGET"
  let gemini_thinking_budget = "OAS_GEMINI_THINKING_BUDGET"
  let default_seed = "OAS_DEFAULT_SEED"
end

let positive_int_env_opt ?(getenv = Cli_common_env.default_getenv) var =
  match getenv var with
  | None -> None
  | Some s ->
    (match int_of_string_opt s with
     | Some n when n > 0 -> Some n
     | _ ->
       Diag.warn "constants" "%s=%S is not a valid positive int, ignoring" var s;
       None)
;;

let positive_int_env_or ?getenv ~var ~default () =
  match positive_int_env_opt ?getenv var with
  | Some n -> n
  | None -> default
;;

module Http = struct
  (** HTTP status codes that trigger retry logic in {!Complete}. *)
  let retryable_codes = [ 429; 500; 502; 503; 529 ]

  (** HTTP status codes that downstream coordinators may use when deciding
      whether to hand work to another provider. OAS exposes the codes;
      orchestration lives outside the SDK. Superset of [retryable_codes]:
      includes auth/forbidden errors that are not retryable. 498 = Groq
      Flex tier capacity exceeded. *)
  let cascadable_codes = [ 401; 403; 429; 498; 500; 502; 503; 529 ]
end

(* ── Inference profiles ─────────────────────────── *)

(** Named inference parameter profiles.  Single source of truth for
    temperature / max_tokens defaults used by both the SDK and
    downstream coordinators.

    - [coordinator_default]: lightweight coordinator calls (health, routing).
    - [cascade_default]: compatibility alias for [coordinator_default].
    - [agent_default]: full agent turn execution.
    - [low_variance]: evaluation, judging, deterministic extraction. *)
module Inference_profile = struct
  (** Inference parameter profile. Sampling fields ([top_p], [top_k],
      [min_p]) are [option] because provider support is not uniform
      (e.g. Anthropic does not accept [min_p]); [None] means the
      parameter is omitted from the request and the provider's own
      default applies.

      @since 0.99.0
      @since 0.161.0 — added top_p, top_k, min_p (#851) *)
  type t =
    { temperature : float
    ; max_tokens : int
    ; top_p : float option
    ; top_k : int option
    ; min_p : float option
    }

  (** Sampling triple unset — convenience base for profiles that do
      not override [top_p] / [top_k] / [min_p]. Not a standalone
      profile: [temperature] and [max_tokens] are sentinel zero and
      must be overridden via record-update. *)
  let no_sampling_overrides : t =
    { temperature = 0.0; max_tokens = 0; top_p = None; top_k = None; min_p = None }
  ;;

  let coordinator_default =
    { no_sampling_overrides with temperature = 0.3; max_tokens = 500 }
  ;;

  let cascade_default = coordinator_default

  let agent_default =
    { no_sampling_overrides with temperature = 0.7; max_tokens = 16_384 }
  ;;

  let low_variance = { no_sampling_overrides with temperature = 0.1; max_tokens = 2048 }

  let worker_default =
    { no_sampling_overrides with temperature = 0.2; max_tokens = 16_384 }
  ;;

  let deterministic = { no_sampling_overrides with temperature = 0.0; max_tokens = 4096 }
end

(** Fallback [max_tokens] when both caller override and model capability
    are absent. Emitted as a required field by OpenAI-compat and Anthropic
    backends. Env override resolution is intentionally done by
    {!resolve_unknown_model_max_tokens_fallback} at request-build time,
    not during module initialization.

    16384 is a last-resort ceiling for modern high-context models. Models with
    lower caps should be declared in [Capabilities.for_model_id] so the
    capability-gated path (not this fallback) applies.
    @since 0.188.0
    @since 0.185.0 — raised from 4096 to 16384
    @since 0.208.0 — env override moved to call-time resolver *)
let unknown_model_max_tokens_fallback = 16384

let resolve_unknown_model_max_tokens_fallback ?getenv () =
  positive_int_env_or
    ?getenv
    ~var:Env.max_tokens_default
    ~default:unknown_model_max_tokens_fallback
    ()
;;

(* ── Cache ───────────────────────────────────────── *)

module Cache = struct
  let default_ttl_sec = 300
end

(* ── Retry ───────────────────────────────────────── *)

module Retry_common = struct
  let max_retries = 3
  let backoff_multiplier = 2.0
  let initial_delay_sec = 1.0

  (** Jitter range: delay is multiplied by a random factor
      in [jitter_min, jitter_min + jitter_range). *)
  let jitter_min = 0.5

  let jitter_range = 1.0
end

module Retry = struct
  let max_retries = Retry_common.max_retries
  let initial_delay_sec = Retry_common.initial_delay_sec
  let max_delay_sec = 30.0
  let backoff_multiplier = Retry_common.backoff_multiplier
  let jitter_min = Retry_common.jitter_min
  let jitter_range = Retry_common.jitter_range
end

(* ── Structured retry (retry.ml) ─────────────────── *)

module Structured_retry = struct
  let max_retries = Retry_common.max_retries
  let initial_delay = Retry_common.initial_delay_sec
  let max_delay = 60.0
  let backoff_factor = Retry_common.backoff_multiplier
  let jitter_min = Retry_common.jitter_min
  let jitter_range = Retry_common.jitter_range
end

(* ── Sampling ────────────────────────────────────── *)

module Sampling = struct
  (** Default min_p for OpenAI-compatible (llama.cpp) providers.
      2026 llama.cpp standard: min_p = 0.05. *)
  let openai_compat_min_p = 0.05
end

(* ── HTTP response body truncation ───────────────── *)

module Truncation = struct
  let max_error_body_length = 200
end

(* ── Endpoints ──────────────────────────────────── *)

(** Default endpoint URLs for local LLM servers.
    Single source of truth — all code should reference these
    constants instead of hardcoding URL literals.
    @since 0.105.0 *)
module Endpoints = struct
  (** Default port for llama.cpp servers.
      Ollama uses 11434 — configure via endpoint config or LLM_ENDPOINTS env. *)
  let default_llama_port = 8085

  let default_url = "http://127.0.0.1:" ^ string_of_int default_llama_port
  let default_url_localhost = "http://localhost:" ^ string_of_int default_llama_port
  let local_prefix = "http://127.0.0.1"
  let localhost_prefix = "http://localhost"
end

(* ── Thinking ────────────────────────────────────── *)

module Thinking = struct
  let env_budget ?getenv env_var = positive_int_env_opt ?getenv env_var

  (** Default extended thinking budget when not specified by caller.
      Used by Anthropic and Gemini backends.

      16000 tokens covers most single-turn reasoning tasks. Models with
      higher caps (Claude Opus 4: 128K, Gemini 2.5 Pro: 32K) should be
      declared in [Capabilities] so callers can override per-model.
      Override with [OAS_THINKING_BUDGET_DEFAULT] env var through
      {!default_budget_for_env}; provider budget helpers call it at
      request-build time.
      @since 0.185.0 — raised from 10000 to 16000
      @since 0.208.0 — env override moved to call-time resolver *)
  let default_budget = 16000

  let default_budget_for_env ?getenv () =
    match env_budget ?getenv Env.thinking_budget_default with
    | Some n -> n
    | None -> default_budget
  ;;

  (** Per-provider thinking budget overrides. Resolution order:
      1. Explicit [~thinking_budget] in request config
      2. Provider-specific env var ([OAS_ANTHROPIC_THINKING_BUDGET],
         [OAS_GEMINI_THINKING_BUDGET])
      3. [OAS_THINKING_BUDGET_DEFAULT] env var
      4. Hardcoded default (16000)
      @since 0.185.0 *)
  let anthropic_budget ?getenv () =
    match env_budget ?getenv Env.anthropic_thinking_budget with
    | Some n -> n
    | None -> default_budget_for_env ?getenv ()
  ;;

  let gemini_budget ?getenv () =
    match env_budget ?getenv Env.gemini_thinking_budget with
    | Some n -> n
    | None -> default_budget_for_env ?getenv ()
  ;;
end

(* ── Deterministic output ─────────────────────────── *)

module Deterministic = struct
  (** Default seed for providers that support the [seed] parameter.
      Overridden by [OAS_DEFAULT_SEED] env var or explicit [~seed] in
      {!Provider_config.make}.
      @since 0.185.0 *)
  let default_seed = 42

  let seed_of_env ?(getenv = Cli_common_env.default_getenv) () =
    match getenv Env.default_seed with
    | None -> None
    | Some s ->
      (match int_of_string_opt s with
       | Some n -> Some n
       | None ->
         Diag.warn "constants" "%s=%S is not a valid int, ignoring" Env.default_seed s;
         None)
  ;;
end

(* ── Token estimation ───────────────────────────── *)

module Token_estimation = struct
  (** Approximate characters per token for English/mixed-language text.
      Used when a provider reports reasoning text as raw string but the
      telemetry schema requires a token count.
      4 chars/token is the standard GPT/BPE approximation for English.
      CJK-heavy text is closer to 2 chars/token, but reasoning output
      is predominantly English/code.
      @since 0.185.0 *)
  let chars_per_token = 4
end
