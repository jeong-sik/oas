module Model_catalog = Llm_provider.Model_catalog

let fail message =
  Printf.eprintf "model_catalog_standalone_probe: %s\n" message;
  exit 1
;;

let () =
  Model_catalog.clear_global ();
  match Model_catalog.global () with
  | None -> fail "embedded default catalog was unavailable"
  | Some catalog ->
    let model_count = List.length (Model_catalog.model_entries catalog) in
    let provider_count = List.length (Model_catalog.provider_entries catalog) in
    if model_count = 0
    then fail "embedded default catalog contained no model entries"
    else
      Printf.printf
        "embedded model catalog: %d models, %d providers\n"
        model_count
        provider_count
;;
