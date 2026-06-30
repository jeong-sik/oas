let dune_model_catalog_path () =
  let executable_dir = Filename.dirname Sys.executable_name in
  if executable_dir = "." || executable_dir = ""
  then None
  else Some (Filename.concat (Filename.dirname executable_dir) "models.toml")
;;

let repository_model_catalog_path ~suite =
  match Sys.getenv_opt "OAS_MODEL_CATALOG" with
  | Some path when path <> "" -> path
  | Some _ | None ->
    (match dune_model_catalog_path () with
     | Some path -> path
     | None ->
       Alcotest.failf
         "models.toml path unavailable for %s tests; set OAS_MODEL_CATALOG or run the \
          suite through Dune"
         suite)
;;

let load_repo_model_catalog ~suite =
  let path = repository_model_catalog_path ~suite in
  match Llm_provider.Model_catalog.load_file path with
  | Ok catalog -> catalog
  | Error msg ->
    Alcotest.failf "models.toml should parse for %s tests (%s): %s" suite path msg
;;

let install_repo_model_catalog ~suite =
  Llm_provider.Model_catalog.set_global (load_repo_model_catalog ~suite)
;;
