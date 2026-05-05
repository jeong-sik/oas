type provider_health_score = string * float

type cascade_config =
  { circuit_threshold : int
  ; circuit_cooldown_s : float
  }

type t =
  { contract : Contract.t
  ; mode : Execution_mode.t
  ; risk_class : Risk_class.t
  ; provider_health : provider_health_score list
  ; cascade_config : cascade_config option
  }

let make ?(provider_health = []) ?cascade_config ~contract ~mode ~risk_class () =
  { contract; mode; risk_class; provider_health; cascade_config }
;;

let cascade_config_of_complete_cascade
      (config : Llm_provider.Complete_cascade.cascade_config)
  =
  { circuit_threshold = config.circuit_threshold
  ; circuit_cooldown_s = config.circuit_cooldown_s
  }
;;
