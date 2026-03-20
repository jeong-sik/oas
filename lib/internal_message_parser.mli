(** Convenience constructors for {!Sdk_client_types.message}. *)

val status : Runtime.session -> Sdk_client_types.message
val events : Runtime.event list -> Sdk_client_types.message
val partial_output : string -> string -> Sdk_client_types.message
val report : Runtime.report -> Sdk_client_types.message
val proof : Runtime.proof -> Sdk_client_types.message
val system : string -> Sdk_client_types.message
