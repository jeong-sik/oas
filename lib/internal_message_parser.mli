(** Internal message constructors for {!Internal_query_engine}.

    Converts runtime types into {!Sdk_client_types.message} values.

    @stability Internal
    @since 0.93.0 *)

val status : Runtime.session -> Sdk_client_types.message
val events : Runtime.event list -> Sdk_client_types.message
val partial_output : string -> string -> Sdk_client_types.message
val report : Runtime.report -> Sdk_client_types.message
val proof : Runtime.proof -> Sdk_client_types.message
val system : string -> Sdk_client_types.message
