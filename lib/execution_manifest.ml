type provider_health_score = string * float

type cascade_config =
  { circuit_threshold : int
  ; circuit_cooldown_s : float
  }

type cascade_strategy =
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

let make ?(provider_health = []) ?cascade_config ~contract ~mode ~risk_class () =
  { contract; mode; risk_class; provider_health; cascade_config }
;;

let cascade_config_of_strategy (strategy : cascade_strategy) : cascade_config =
  { circuit_threshold = strategy.circuit_threshold
  ; circuit_cooldown_s = strategy.circuit_cooldown_s
  }
;;

let neutral_cascade_strategy =
  { max_steps = 3
  ; step_timeout_s = 20.0
  ; global_timeout_s = 60.0
  ; backoff_base_s = 0.5
  ; backoff_max_s = 5.0
  ; jitter = 0.1
  ; circuit_threshold = 10
  ; circuit_cooldown_s = 120.0
  ; health_check_interval_s = 30.0
  }
;;

let cascade_strategy_for_risk_class = function
  | Risk_class.Critical ->
    { max_steps = 5
    ; step_timeout_s = 5.0
    ; global_timeout_s = 15.0
    ; backoff_base_s = 0.1
    ; backoff_max_s = 1.0
    ; jitter = 0.3
    ; circuit_threshold = 3
    ; circuit_cooldown_s = 30.0
    ; health_check_interval_s = 5.0
    }
  | Risk_class.High ->
    { max_steps = 4
    ; step_timeout_s = 10.0
    ; global_timeout_s = 30.0
    ; backoff_base_s = 0.2
    ; backoff_max_s = 2.0
    ; jitter = 0.2
    ; circuit_threshold = 5
    ; circuit_cooldown_s = 60.0
    ; health_check_interval_s = 10.0
    }
  | Risk_class.Medium -> neutral_cascade_strategy
  | Risk_class.Low ->
    { max_steps = 2
    ; step_timeout_s = 30.0
    ; global_timeout_s = 120.0
    ; backoff_base_s = 1.0
    ; backoff_max_s = 10.0
    ; jitter = 0.1
    ; circuit_threshold = 20
    ; circuit_cooldown_s = 300.0
    ; health_check_interval_s = 60.0
    }
;;

let cascade_config_for_risk_class risk_class =
  cascade_config_of_strategy (cascade_strategy_for_risk_class risk_class)
;;

let cascade_config_of_complete_cascade
      (config : Llm_provider.Complete_cascade.cascade_config)
  =
  { circuit_threshold = config.circuit_threshold
  ; circuit_cooldown_s = config.circuit_cooldown_s
  }
;;

let cascade_strategy_of_complete_cascade
      (config : Llm_provider.Complete_cascade.cascade_config)
  =
  { neutral_cascade_strategy with
    circuit_threshold = config.circuit_threshold
  ; circuit_cooldown_s = config.circuit_cooldown_s
  }
;;
