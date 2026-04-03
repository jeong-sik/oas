(** Provider resolution for the runtime server.

    @stability Internal
    @since 0.93.1 *)

type execution_resolution = {
  selected_provider: string;
  requested_model: string option;
  resolved_provider: string option;
  resolved_model: string option;
  provider_cfg: Provider.config option;
}

val provider_runtime_name :
  string -> Provider.config option -> string

val resolve_provider :
  ?provider:string -> ?model:string -> unit ->
  (Provider.config option, Error.sdk_error) result

val resolve_execution :
  Runtime.session -> Runtime.spawn_agent_request ->
  (execution_resolution, Error.sdk_error) result
