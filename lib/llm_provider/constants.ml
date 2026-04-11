(** Consolidated magic numbers for the llm_provider library.

    Centralises retry codes, inference defaults, cache TTL, retry
    parameters, jitter range, and sampling params so they are defined
    once and referenced everywhere.

    @since 0.99.0 *)

(* ── HTTP retry / cascade codes ──────────────────── *)

module Http = struct
  (** HTTP status codes that trigger retry logic in {!Complete}. *)
  let retryable_codes = [429; 500; 502; 503; 529]

  (** HTTP status codes that trigger cascade fallback in {!Cascade_config}.
      Superset of [retryable_codes]: includes auth/forbidden errors
      that are not retryable but should cascade to the next provider.
      498 = Groq Flex tier capacity exceeded. *)
  let cascadable_codes = [401; 403; 429; 498; 500; 502; 503; 529]
end

(* ── Inference profiles ─────────────────────────── *)

(** Named inference parameter profiles.  Single source of truth for
    temperature / max_tokens defaults used by both the SDK and
    downstream coordinators.

    - [cascade_default]: lightweight cascade calls (health, routing).
    - [agent_default]: full agent turn execution.
    - [low_variance]: evaluation, judging, deterministic extraction. *)
module Inference_profile = struct
  type t = {
    temperature : float;
    max_tokens : int;
  }

  let cascade_default = { temperature = 0.3; max_tokens = 500 }
  let agent_default   = { temperature = 0.7; max_tokens = 16_384 }
  let low_variance    = { temperature = 0.1; max_tokens = 2048 }
  let worker_default  = { temperature = 0.2; max_tokens = 16_384 }
  let deterministic   = { temperature = 0.0; max_tokens = 4096 }
end

(** Backward-compatible aliases — existing callers of
    [Constants.Inference.default_temperature] continue to compile.
    New code should prefer {!Inference_profile}. *)
module Inference = struct
  let default_temperature = Inference_profile.cascade_default.temperature
  let default_max_tokens  = Inference_profile.cascade_default.max_tokens
end

(* ── Cache ───────────────────────────────────────── *)

module Cache = struct
  let default_ttl_sec = 300
end

(* ── Retry ───────────────────────────────────────── *)

module Retry = struct
  let max_retries = 3
  let initial_delay_sec = 1.0
  let max_delay_sec = 30.0
  let backoff_multiplier = 2.0

  (** Jitter range: delay is multiplied by a random factor
      in [jitter_min, jitter_min + jitter_range). *)
  let jitter_min = 0.5
  let jitter_range = 1.0
end

(* ── Structured retry (retry.ml) ─────────────────── *)

module Structured_retry = struct
  let max_retries = 3
  let initial_delay = 1.0
  let max_delay = 60.0
  let backoff_factor = 2.0
  let jitter_min = 0.5
  let jitter_range = 1.0
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
      Ollama uses 11434 — configure via cascade endpoint or LLM_ENDPOINTS env. *)
  let default_llama_port = 8085
  let default_url = "http://127.0.0.1:" ^ string_of_int default_llama_port
  let default_url_localhost = "http://localhost:" ^ string_of_int default_llama_port
  let local_prefix = "http://127.0.0.1"
  let localhost_prefix = "http://localhost"
end

(* ── Anthropic ──────────────────────────────────── *)

module Anthropic = struct
  (** Minimum system prompt length (chars) to enable prompt caching.
      Approximation: ~1024 tokens at ~3.4 chars/token. *)
  let prompt_cache_min_chars = 3500
end
