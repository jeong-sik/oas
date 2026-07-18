(** Private restart recovery for one already-open durable provider turn. *)

val run
  :  Agent_types.t
  -> Pipeline_execution_scope.t
  -> execute:(Types.content_block Nonempty.t -> ('a, Error.sdk_error) result)
  -> already_settled:'a
  -> ('a, Error.sdk_error) result
