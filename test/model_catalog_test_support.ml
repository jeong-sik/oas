let load_packaged_model_catalog ~suite =
  match Llm_provider.Model_catalog.load_default () with
  | Ok catalog -> catalog
  | Error msg ->
    Alcotest.failf "packaged models.toml should parse for %s tests: %s" suite msg
;;

let install_packaged_model_catalog ~suite =
  Llm_provider.Model_catalog.set_global (load_packaged_model_catalog ~suite)
;;
