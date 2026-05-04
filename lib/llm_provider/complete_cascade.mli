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
        Default: 60.0. *)
  }

(** Default cascade config: circuit_threshold=3, circuit_cooldown_s=60.0. *)
val default_cascade_config : cascade_config

(** {1 Health Tracking} *)

(** Thread-safe per-provider health state for circuit breaking.
    Create once and share across cascade calls to maintain circuit state. *)
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
  -> ?cascade_config:cascade_config
  -> ?health:provider_health
  -> steps:Provider_config.t list
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> cascade_result
