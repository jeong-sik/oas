(** Private ownership wrapper for one fresh Agent execution journal. *)

type runtime
type store
type locator

val create_runtime
  :  sw:Eio.Switch.t
  -> domain_mgr:_ Eio.Domain_manager.t
  -> domain_count:int
  -> (runtime, Error.sdk_error) result

val store
  :  runtime:runtime
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> ?on_scope_ready:(locator -> (unit, string) result)
  -> ?resume:locator
  -> unit
  -> store

val locator_to_yojson : locator -> Yojson.Safe.t
val locator_of_yojson : Yojson.Safe.t -> (locator, string) result

val with_fresh
  :  store
  -> Agent_types.t
  -> (sw:Eio.Switch.t
      -> Execution_agent_scope.t
      -> ('a, Provider_failure_attribution.detailed_error) result)
  -> ('a, Provider_failure_attribution.detailed_error) result

val with_store
  :  store
  -> Agent_types.t
  -> (sw:Eio.Switch.t
      -> Execution_agent_scope.t
      -> ('a, Provider_failure_attribution.detailed_error) result)
  -> ('a, Provider_failure_attribution.detailed_error) result

val is_resume : store -> bool

val with_scope
  :  Execution_agent_scope.t
  -> (unit -> ('a, Provider_failure_attribution.detailed_error) result)
  -> ('a, Provider_failure_attribution.detailed_error) result
