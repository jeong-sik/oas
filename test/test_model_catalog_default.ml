open Alcotest
module Capabilities = Llm_provider.Capabilities
module Model_catalog = Llm_provider.Model_catalog

let first_id_prefix ~suite catalog =
  match Model_catalog.model_entries catalog with
  | [] -> failf "%s: repo model catalog should not be empty" suite
  | (entry : Model_catalog.model_entry) :: _ -> entry.id_prefix
;;

let with_clean_model_catalog_override f =
  Model_catalog.clear_global ();
  Fun.protect ~finally:Model_catalog.clear_global f
;;

let test_load_default_catalog () =
  let expected =
    Model_catalog_test_support.load_repo_model_catalog ~suite:"model catalog default"
  in
  match Model_catalog.load_default () with
  | Error msg -> failf "default model catalog should load: %s" msg
  | Ok catalog ->
    check
      bool
      "embedded default is exactly the OAS models.toml catalog"
      true
      (Model_catalog.model_entries expected = Model_catalog.model_entries catalog
       && Model_catalog.provider_entries expected = Model_catalog.provider_entries catalog
      )
;;

let test_in_memory_catalog_rejects_invalid_generated_input () =
  match
    Model_catalog.of_toml_string
      ~source:"invalid embedded candidate"
      "[[models]]\nid_prefix = \"broken\"\nsupports_tools = \"yes\""
  with
  | Error msg ->
    check
      string
      "invalid field is diagnosed"
      "model entry \"broken\" field \"supports_tools\" expected bool"
      msg
  | Ok _ -> fail "invalid in-memory catalog must fail validation"
;;

let test_global_loads_default_catalog_for_capabilities () =
  let expected =
    Model_catalog_test_support.load_repo_model_catalog
      ~suite:"model catalog default production path"
  in
  let model_id =
    first_id_prefix ~suite:"model catalog default production path" expected
  in
  with_clean_model_catalog_override (fun () ->
    match Capabilities.for_model_id_catalog model_id with
    | Some _ -> ()
    | None ->
      failf
        "Capabilities.for_model_id_catalog should resolve %S through embedded/default \
         Model_catalog.global"
        model_id)
;;

let () =
  run
    "model catalog default"
    [ ( "embedded catalog"
      , [ test_case "load_default" `Quick test_load_default_catalog
        ; test_case
            "invalid generated input fails closed"
            `Quick
            test_in_memory_catalog_rejects_invalid_generated_input
        ; test_case
            "global uses embedded default"
            `Quick
            test_global_loads_default_catalog_for_capabilities
        ] )
    ]
;;
