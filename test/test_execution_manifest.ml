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
    check (float 0.001) "circuit_cooldown_s" 30.0 cfg.circuit_cooldown_s
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
        ] )
    ]
;;
