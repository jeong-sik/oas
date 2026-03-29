(** Centralized constants for LLM provider configuration.

    This module defines all magic numbers and hardcoded values used
    throughout the llm_provider library in one place for easy tuning.

    @since 0.95.0 *)

(** HTTP status codes that should trigger cascade to next provider. *)
let retryable_http_codes = [401; 403; 429; 500; 502; 503; 529]

(** Default cache TTL in seconds for completion responses. *)
let default_cache_ttl_sec = 300

(** Maximum length of HTTP response body to include in error messages before truncation. *)
let max_error_body_length = 200

(** Default inference parameters for cascade configurations. *)
module Cascade = struct
  (** Default temperature for model completions. *)
  let default_temperature = 0.3

  (** Default maximum number of tokens to generate. *)
  let default_max_tokens = 500
end

(** Retry configuration constants. *)
module Retry = struct
  (** Default backoff jitter multiplier range: 0.5 to 1.5. *)
  let backoff_jitter_min = 0.5
  let backoff_jitter_max = 1.5

  (** Default agent-level retry configuration (used in swarm runner). *)
  let default_agent_max_retries = 2
  let default_agent_initial_delay = 1.0
  let default_agent_backoff = 2.0
end

(** Sampling defaults for local providers (llama.cpp). *)
module Sampling = struct
  (** Standard min_p value for local llama.cpp providers (2026 convention). *)
  let local_min_p = 0.05
end

(** Discovery configuration for local LLM endpoints. *)
module Discovery = struct
  (** Default ports to scan when discovering local LLM servers. *)
  let default_scan_ports = [8085; 8086; 8087; 8088; 8089; 8090]

  (** Default single endpoint if no LLM_ENDPOINTS env var is set. *)
  let default_endpoint = "http://127.0.0.1:8085"
end
