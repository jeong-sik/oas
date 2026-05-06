(** Execution Manifest: The single point of intersection between the coordinator and OAS. *)

(** Provider key and normalized health score in [0.0, 1.0]. *)
type provider_health_score = string * float

type cascade_config =
  { max_steps : int
  ; step_timeout_s : float
  ; global_timeout_s : float
  ; backoff_base_s : float
  ; backoff_max_s : float
  ; jitter : float
  ; circuit_threshold : int
  ; circuit_cooldown_s : float
  ; health_check_interval_s : float
  }

type t =
  { contract : Contract.t
  ; mode : Execution_mode.t
  ; risk_class : Risk_class.t
  ; provider_health : provider_health_score list
  ; cascade_config : cascade_config option
  }

val make
  :  ?provider_health:provider_health_score list
  -> ?cascade_config:cascade_config
  -> contract:Contract.t
  -> mode:Execution_mode.t
  -> risk_class:Risk_class.t
  -> unit
  -> t

(** Product-neutral advisory cascade defaults for a risk class.

    OAS only carries the contract policy here; it does not own the distributed
    cascade runtime, provider quota manager, or health tracker. *)
val cascade_config_for_risk_class : Risk_class.t -> cascade_config

val cascade_config_of_complete_cascade
  :  Llm_provider.Complete_cascade.cascade_config
  -> cascade_config
