(** Multi-provider cascade completion with health-aware fallback.

    Sits above {!Complete.complete_with_retry}: each cascade step
    delegates to a single provider with its own retry budget. The cascade
    layer handles cross-provider failover and circuit breaking.

    @since 0.185.0
    @stability Internal *)

(** {1 Configuration} *)

(** Cascade-level configuration for circuit breaking. *)
type cascade_config =
  { circuit_threshold : int
    (** Consecutive failures before a provider is circuit-broken (skipped).
        Default: 3. *)
  ; circuit_cooldown_s : float
    (** Seconds before a circuit-broken provider is retried (half-open).
        Default: 30.0. *)
  }

(** Default cascade config: circuit_threshold=3, circuit_cooldown_s=30.0. *)
val default_cascade_config : cascade_config

(** {1 Health Tracking} *)

(** Fiber-safe per-provider health state for circuit breaking.
    Create once and share across cascade calls in the same Eio runtime to
    maintain circuit state. *)
type provider_health

(** Create an empty health tracker.
    When [clock] is provided, uses {!Eio.Time.now} for timestamps.
    Otherwise falls back to [Unix.time]. *)
val create_health : ?clock:_ Eio.Time.clock -> unit -> provider_health

(** Derive the health-tracker key for a provider config.
    Format: [model_id@base_url]. *)
val provider_key : Provider_config.t -> string

(** Record a successful completion for the given key.
    Resets the consecutive failure count (closes the circuit). *)
val record_success : provider_health -> string -> unit

(** Record a failure for the given key.
    Increments the consecutive failure count and updates the timestamp.
    After [circuit_threshold] consecutive failures, the circuit opens. *)
val record_failure : provider_health -> string -> unit

(** Provider health snapshot derived from the circuit-breaker state. *)
type provider_health_info =
  { provider_key : string
  ; health_score : float
    (** [0.0, 1.0], where [1.0] means no active failures and [0.0] means the
        provider has reached the circuit threshold. *)
  ; consecutive_failures : int
  ; circuit_open : bool
  ; cooldown_remaining_s : float option
    (** [Some t] (where [t > 0.0]) when [circuit_open = true]: seconds remaining
        until the circuit may close. [None] when the circuit is closed — either
        because the consecutive failure count is below the threshold or because
        the cooldown has already elapsed. Consumers can therefore treat
        [None] as "no active cooldown" without having to special-case
        [Some 0.0]. *)
  }

val provider_health_info
  :  provider_health
  -> cascade_config:cascade_config
  -> provider_key:string
  -> provider_health_info

val provider_health_scores
  :  provider_health
  -> cascade_config:cascade_config
  -> provider_keys:string list
  -> (string * float) list

(** {1 Result} *)

(** Why a cascade step was skipped. *)
type skip_reason =
  | Circuit_breaker_open of { provider : string }
  (** Provider is circuit-broken (consecutive failures above threshold
          and cooldown has not elapsed). *)

(** Outcome of a cascade attempt. *)
type cascade_result =
  | Success of
      { response : Types.api_response
      ; step_index : int
      ; model_id : string
      }
  (** A provider returned a successful response. [step_index] is the
          0-based index into the [steps] list. *)
  | All_failed of
      { errors : (Provider_config.t * Http_client.http_error) list
      ; skipped : (Provider_config.t * skip_reason) list
      }
  (** Every provider failed or was skipped. [errors] lists providers
          that were tried; [skipped] lists providers that were
          circuit-broken. *)
  | Hard_quota of
      { config : Provider_config.t
      ; error : Http_client.http_error
      }
  (** A provider hit hard account-level quota (balance depleted,
          monthly limit). Cascade stops immediately because retrying
          another provider on the same account will also fail. *)

(** {1 Execution} *)

(** Execute a multi-provider cascade completion.

    For each step in order:
    1. Check circuit breaker — skip if open
    2. Call {!Complete.complete_with_retry} with the step's config
    3. On success, clear the provider's failure count and return
    4. On hard quota error, record failure and return [Hard_quota]
    5. On other error, record failure and try next step

    [?attempt_timeout_s] caps one provider step, including its internal
    retry loop. Timeout errors include the cascade phase, provider
    attempt index, model id, and provider key in the structured
    [NetworkError] message so downstream receipts can distinguish a
    provider-step timeout from a caller-side budget gate:
    - omitted (no argument): provider-specific defaults from
      {!Provider_config.default_attempt_timeout_s} apply when present
      (Ollama, Claude_code, Kimi_cli, Gemini_cli have positive defaults;
      others have none).
    - [Some t] with [t > 0.0]: use [t] seconds for this call, regardless
      of the provider default.
    - [Some t] with [t <= 0.0]: disable the cascade-level timeout for
      this call. Use this to opt out for long-running local models
      (e.g. an Ollama instance loading a large MoE) where the provider
      default is too aggressive.

    When [health] is [None], a fresh tracker is created per call.
    Pass a shared tracker to maintain circuit state across calls. *)
val complete_cascade
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> clock:_ Eio.Time.clock
  -> ?transport:Llm_transport.t
  -> ?cache:Cache.t
  -> ?metrics:Metrics.t
  -> ?retry_config:Complete.retry_config
  -> ?attempt_timeout_s:float
  -> ?cascade_config:cascade_config
  -> ?health:provider_health
  -> steps:Provider_config.t list
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> cascade_result
