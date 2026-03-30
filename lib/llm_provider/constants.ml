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
      that are not retryable but should cascade to the next provider. *)
  let cascadable_codes = [401; 403; 429; 500; 502; 503; 529]
end

(* ── Inference defaults ──────────────────────────── *)

module Inference = struct
  let default_temperature = 0.3
  let default_max_tokens = 500
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
