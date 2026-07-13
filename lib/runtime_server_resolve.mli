(** Provider resolution for the runtime server.

    Runtime does not own provider aliases or fallbacks. It forwards exact
    provider ids and catalog-declared aliases to {!Provider_runtime_binding}; a
    missing or unknown selector is an explicit error.

    @stability Internal
    @since 0.93.1 *)

type execution_resolution =
  { selected_provider : string
  ; requested_model : string option
  ; resolved_provider : string option
  ; resolved_model : string option
  ; provider_cfg : Provider.config
  }

val resolve_provider
  :  ?provider:string
  -> ?model:string
  -> unit
  -> (Provider.config, Error.sdk_error) result

val resolve_execution
  :  Runtime.session
  -> Runtime.spawn_agent_request
  -> (execution_resolution, Error.sdk_error) result
