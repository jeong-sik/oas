(** Private fiber-local authority for recursive Agent runs inside Tool effects. *)

val with_child_scope_factory
  :  (agent_name:string -> (Execution_agent_scope.t, Execution_agent_scope.error) result)
  -> (unit -> 'a)
  -> 'a

val child_scope_factory
  :  unit
  -> (agent_name:string -> (Execution_agent_scope.t, Execution_agent_scope.error) result)
       option
