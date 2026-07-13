open Alcotest
module Capabilities = Llm_provider.Capabilities
module Model_catalog = Llm_provider.Model_catalog

let id_prefixes catalog =
  catalog
  |> Model_catalog.model_entries
  |> List.map (fun (entry : Model_catalog.model_entry) -> entry.id_prefix)
;;

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
  match Model_catalog.load_default () with
  | Error msg -> failf "default model catalog should load: %s" msg
  | Ok catalog ->
    check bool "default catalog has model declarations" true (id_prefixes catalog <> [])
;;

let test_global_loads_default_catalog_for_capabilities () =
  let expected =
    Model_catalog_test_support.load_packaged_model_catalog
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
        "Capabilities.for_model_id_catalog should resolve %S through packaged/default \
         Model_catalog.global"
        model_id)
;;

let () =
  run
    "model catalog default"
    [ ( "packaged catalog"
      , [ test_case "load_default" `Quick test_load_default_catalog
        ; test_case
            "global uses packaged default"
            `Quick
            test_global_loads_default_catalog_for_capabilities
        ] )
    ]
;;
