(** Private durable-execution ownership for one pipeline turn. *)

type t

val open_turn
  :  Execution_agent_scope.t option
  -> ordinal:int
  -> (t, Error.sdk_error) result

val before_provider_attempt : t -> Binding_identity.t -> (unit, Error.sdk_error) result
val provider : t -> Execution_agent_scope.provider_attempt option
val close_success : t -> (unit, Error.sdk_error) result
