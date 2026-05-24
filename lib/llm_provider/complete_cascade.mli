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

(** Serializable provider health entry. *)
type provider_health_snapshot_entry =
  { snapshot_provider_key : string
  ; snapshot_consecutive_failures : int
  ; snapshot_last_failure_time : float option
  }

(** Serializable provider health state.

    Consumers that run long-lived agents can persist this value outside OAS
    and restore it after process restart so circuit state is not erased by a
    supervisor restart. *)
type provider_health_snapshot = provider_health_snapshot_entry list

(** Export the current provider health table as a stable snapshot. *)
val snapshot_health : provider_health -> provider_health_snapshot

(** Replace the contents of an existing provider health table from a snapshot. *)
val replace_health_snapshot : provider_health -> provider_health_snapshot -> unit

(** Create a provider health table from a previously exported snapshot. *)
val restore_health
  :  ?clock:_ Eio.Time.clock
  -> provider_health_snapshot
  -> provider_health

(** Convert a provider health snapshot to JSON. *)
val provider_health_snapshot_to_yojson : provider_health_snapshot -> Yojson.Safe.t

(** Parse a provider health snapshot from JSON. *)
val provider_health_snapshot_of_yojson
  :  Yojson.Safe.t
  -> (provider_health_snapshot, string) result

(** Atomically persist the current provider health snapshot as pretty JSON.
    Parent directories are created before writing with a writer-unique
    temporary file and rename. *)
val save_health_snapshot_json : provider_health -> path:string -> (unit, string) result

(** Load a provider health tracker from a JSON snapshot file.
    The file is parsed through {!provider_health_snapshot_of_yojson}; malformed
    snapshots are returned as [Error] instead of silently resetting health. *)
val load_health_snapshot_json
  :  ?clock:_ Eio.Time.clock
  -> path:string
  -> unit
  -> (provider_health, string) result

(** Load a provider health tracker when [path] exists; otherwise create an
    empty tracker.  Malformed existing snapshots are still [Error] so startup
    does not silently erase a corrupt circuit-breaker state. *)
val load_or_create_health_snapshot_json
  :  ?clock:_ Eio.Time.clock
  -> path:string
  -> unit
  -> (provider_health, string) result

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
  | Provider_terminal of
      { config : Provider_config.t
      ; kind : Http_client.provider_terminal_kind
      ; message : string
      }
  (** A provider reported a structured terminal condition, such as
          [Max_turns]. Cascade stops immediately so the agent/runtime
          layer can checkpoint, resume, or surface the typed terminal
          condition instead of hiding it by falling through to another
          provider. *)

(** {1 Execution} *)

(** Execute a multi-provider cascade completion.

    For each step in order:
    1. Check circuit breaker — skip if open
    2. Call {!Complete.complete_with_retry} with the step's config
    3. On success, clear the provider's failure count and return
    4. On hard quota error, record failure and return [Hard_quota]
    5. On provider terminal condition, return [Provider_terminal]
       without falling through or poisoning provider health
    6. On other error, record failure and try next step

    [?attempt_timeout_s] caps one admitted provider step, including its internal
    retry loop but not an explicitly separated admission queue wait. Timeout
    errors include the cascade phase, provider
    attempt index, model id, and provider key in
    [TimeoutError { phase = Provider_step; _ }] so downstream receipts
    can distinguish a provider-step timeout from a caller-side budget gate:
    - omitted (no argument): provider-specific defaults from
      {!Provider_config.default_attempt_timeout_s} apply when present
      (Cli_tool_d, Cli_tool_c, and Cli_tool_b have positive defaults;
      Ollama and other HTTP/API providers have none).
    - [Some t] with [t > 0.0]: use [t] seconds for this call, regardless
      of the provider default.
    - [Some t] with [t <= 0.0]: disable the cascade-level timeout for
      this call. Use this to opt out for long-running local models
      when the caller supplied a tighter attempt budget.

    [?admission_queue_timeout_s] separates provider-throttle admission from the
    provider body/attempt budget:
    - omitted (no argument): preserve legacy throttle behavior and queue inside
      the provider step.
    - [Some t] with [t > 0.0]: wait up to [t] seconds for the provider throttle
      permit before starting the provider attempt; timeout surfaces as
      [TimeoutError { phase = Queue; _ }].
    - [Some t] with [t <= 0.0]: perform a non-blocking admission check; if no
      permit is immediately available, surface
      [TimeoutError { phase = Capacity_backpressure; _ }].

    When [health] is [None], a fresh tracker is created per call.
    Pass a shared tracker to maintain circuit state across calls.

    [?throttle_resolver] maps each provider config to the shared
    provider-level throttle that gates the attempt. The default resolver
    creates one fallback throttle per provider kind/base URL/account key, so
    concurrent cascade calls in this process queue before reaching the backend
    instead of stampeding a limited-capacity provider. Return [None] from a
    custom resolver to opt a config out of throttling. *)
val complete_cascade
  :  sw:Eio.Switch.t
  -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> clock:_ Eio.Time.clock
  -> ?transport:Llm_transport.t
  -> ?cache:Cache.t
  -> ?metrics:Metrics.t
  -> ?retry_config:Complete.retry_config
  -> ?attempt_timeout_s:float
  -> ?admission_queue_timeout_s:float
  -> ?cascade_config:cascade_config
  -> ?health:provider_health
  -> ?priority:Request_priority.t
  -> ?throttle_resolver:(Provider_config.t -> Provider_throttle.t option)
  -> steps:Provider_config.t list
  -> messages:Types.message list
  -> ?tools:Yojson.Safe.t list
  -> unit
  -> cascade_result
