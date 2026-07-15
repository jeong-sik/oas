open Runtime

type execution_resolution =
  { selected_provider : string
  ; requested_model : string option
  ; resolved_provider : string option
  ; resolved_model : string option
  ; provider_cfg : Provider.config
  }

let unsupported_provider detail =
  Error (Error.Config (Error.UnsupportedProvider { detail }))
;;

let valid_provider_detail () =
  Provider_runtime_binding.known_labels () |> String.concat ", "
;;

let required_provider_error () =
  Error
    (Error.Config
       (InvalidConfig
          { field = "provider"
          ; detail =
              "runtime execution requires an explicit provider id or a catalog-declared \
               alias"
          }))
;;

let unknown_provider_error selector =
  unsupported_provider
    (Printf.sprintf
       "unknown provider %S; catalog/registry selectors: %s"
       selector
       (valid_provider_detail ()))
;;

let validate_provider_identity ~provider =
  match Util.trim_non_empty_opt (Some provider) with
  | None -> required_provider_error ()
  | Some selector ->
    (match Provider_runtime_binding.find selector with
     | Some _binding -> Ok ()
     | None -> unknown_provider_error selector)
;;

let resolve_provider ?provider ?model () =
  match Util.trim_non_empty_opt provider with
  | None -> required_provider_error ()
  | Some selector ->
    (match Provider_runtime_binding.resolve ?model selector with
     | Some (Ok (_binding, cfg)) -> Ok cfg
     | Some (Error _ as error) -> error
     | None -> unknown_provider_error selector)
;;

let resolve_execution (session : session) (detail : spawn_agent_request) =
  let requested_model =
    Util.first_some
      (Util.trim_non_empty_opt detail.model)
      (Util.trim_non_empty_opt session.model)
  in
  match
    Util.first_some
      (Util.trim_non_empty_opt detail.provider)
      (Util.trim_non_empty_opt session.provider)
  with
  | None -> required_provider_error ()
  | Some selector ->
    (match Provider_runtime_binding.resolve ?model:requested_model selector with
     | None -> unknown_provider_error selector
     | Some (Error _ as error) -> error
     | Some (Ok (binding, provider_cfg)) ->
       Ok
         { selected_provider = binding.id
         ; requested_model
         ; resolved_provider = Some binding.id
         ; resolved_model = Some provider_cfg.model_id
         ; provider_cfg
         })
;;
