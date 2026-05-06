open Alcotest
module EM = Agent_sdk.Execution_manifest

let test_make_defaults () =
  let manifest =
    EM.make
      ~contract:Agent_sdk.Contract.empty
      ~mode:Agent_sdk.Execution_mode.Diagnose
      ~risk_class:Agent_sdk.Risk_class.Low
      ()
  in
  check int "provider_health defaults empty" 0 (List.length manifest.provider_health);
  check
    bool
    "cascade_config defaults absent"
    true
    (Option.is_none manifest.cascade_config)
;;

let test_make_carries_provider_health_and_cascade_config () =
  let cascade_config =
    EM.cascade_config_of_complete_cascade
      Llm_provider.Complete_cascade.default_cascade_config
  in
  let manifest =
    EM.make
      ~contract:Agent_sdk.Contract.empty
      ~mode:Agent_sdk.Execution_mode.Draft
      ~risk_class:Agent_sdk.Risk_class.High
      ~provider_health:[ "anthropic", 0.0; "moonshot", 1.0 ]
      ~cascade_config
      ()
  in
  check
    (float 0.001)
    "anthropic health"
    0.0
    (List.assoc "anthropic" manifest.provider_health);
  check
    (float 0.001)
    "moonshot health"
    1.0
    (List.assoc "moonshot" manifest.provider_health);
  match manifest.cascade_config with
  | None -> fail "expected cascade_config"
  | Some cfg ->
    check int "circuit_threshold" 3 cfg.circuit_threshold;
    check (float 0.001) "circuit_cooldown_s" 30.0 cfg.circuit_cooldown_s;
    check int "max_steps defaulted" 3 cfg.max_steps;
    check (float 0.001) "step_timeout_s defaulted" 20.0 cfg.step_timeout_s
;;

let test_cascade_config_for_risk_class_critical () =
  let cfg = EM.cascade_config_for_risk_class Agent_sdk.Risk_class.Critical in
  check int "critical max_steps" 5 cfg.max_steps;
  check (float 0.001) "critical step timeout" 5.0 cfg.step_timeout_s;
  check (float 0.001) "critical global timeout" 15.0 cfg.global_timeout_s;
  check (float 0.001) "critical backoff base" 0.1 cfg.backoff_base_s;
  check (float 0.001) "critical backoff max" 1.0 cfg.backoff_max_s;
  check (float 0.001) "critical jitter" 0.3 cfg.jitter;
  check int "critical circuit threshold" 3 cfg.circuit_threshold;
  check (float 0.001) "critical cooldown" 30.0 cfg.circuit_cooldown_s;
  check (float 0.001) "critical health check" 5.0 cfg.health_check_interval_s
;;

let test_cascade_config_for_risk_class_low () =
  let cfg = EM.cascade_config_for_risk_class Agent_sdk.Risk_class.Low in
  check int "low max_steps" 2 cfg.max_steps;
  check (float 0.001) "low step timeout" 30.0 cfg.step_timeout_s;
  check (float 0.001) "low global timeout" 120.0 cfg.global_timeout_s;
  check (float 0.001) "low backoff base" 1.0 cfg.backoff_base_s;
  check (float 0.001) "low backoff max" 10.0 cfg.backoff_max_s;
  check (float 0.001) "low jitter" 0.1 cfg.jitter;
  check int "low circuit threshold" 20 cfg.circuit_threshold;
  check (float 0.001) "low cooldown" 300.0 cfg.circuit_cooldown_s;
  check (float 0.001) "low health check" 60.0 cfg.health_check_interval_s
;;

let test_high_risk_is_stricter_than_low_for_timeout_and_circuit () =
  let high = EM.cascade_config_for_risk_class Agent_sdk.Risk_class.High in
  let low = EM.cascade_config_for_risk_class Agent_sdk.Risk_class.Low in
  check bool "high has lower step timeout" true (high.step_timeout_s < low.step_timeout_s);
  check bool "high has lower cooldown" true (high.circuit_cooldown_s < low.circuit_cooldown_s);
  check bool "high opens circuit sooner" true (high.circuit_threshold < low.circuit_threshold)
;;

let () =
  run
    "execution_manifest"
    [ ( "manifest"
      , [ test_case "make defaults" `Quick test_make_defaults
        ; test_case
            "make carries provider health and cascade config"
            `Quick
            test_make_carries_provider_health_and_cascade_config
        ; test_case
            "critical risk cascade defaults"
            `Quick
            test_cascade_config_for_risk_class_critical
        ; test_case
            "low risk cascade defaults"
            `Quick
            test_cascade_config_for_risk_class_low
        ; test_case
            "high risk cascade defaults are stricter than low"
            `Quick
            test_high_risk_is_stricter_than_low_for_timeout_and_circuit
        ] )
    ]
;;
