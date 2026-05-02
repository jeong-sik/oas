(** Consolidated magic numbers for the llm_provider library.

    Centralises retry codes, inference defaults, cache TTL, retry
    parameters, jitter range, and sampling params so they are defined
    once and referenced everywhere.

    @since 0.99.0 *)

(* ── HTTP retry / handoff codes ──────────────────── *)

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

    - [cascade_default]: lightweight coordinator calls (health, routing).
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

  let cascade_default = { no_sampling_overrides with temperature = 0.3; max_tokens = 500 }

  let agent_default =
    { no_sampling_overrides with temperature = 0.7; max_tokens = 16_384 }
  ;;

  let low_variance = { no_sampling_overrides with temperature = 0.1; max_tokens = 2048 }

  let worker_default =
    { no_sampling_overrides with temperature = 0.2; max_tokens = 16_384 }
  ;;

  let deterministic = { no_sampling_overrides with temperature = 0.0; max_tokens = 4096 }
end

(** Backward-compatible aliases — existing callers of
    [Constants.Inference.default_temperature] continue to compile.
    New code should prefer {!Inference_profile}. *)
module Inference = struct
  let default_temperature = Inference_profile.cascade_default.temperature
  let default_max_tokens = Inference_profile.cascade_default.max_tokens

  (** Fallback [max_tokens] when both caller override and model capability
      are absent. Emitted as a required field by OpenAI-compat and Anthropic
      backends. Kept conservative to avoid overrunning unknown model limits,
      but high enough to avoid silent truncation on modern models.
      @since 0.188.0 *)
  let unknown_model_max_tokens_fallback = 4096
end

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
  (** Default extended thinking budget when not specified by caller.
      Used by Anthropic and Gemini backends. *)
  let default_budget = 10000
end

(* ── Anthropic ──────────────────────────────────── *)

module Anthropic = struct
  (** Minimum system prompt length (chars) to enable prompt caching.
      Approximation: ~1024 tokens at ~3.4 chars/token. *)
  let prompt_cache_min_chars = 3500
end
