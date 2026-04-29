(** Convenience wrapper for one-shot runtime protocol queries.

    @stability Internal
    @since 0.93.1 *)

val query
  :  sw:Eio.Switch.t
  -> mgr:_ Eio.Process.mgr
  -> ?runtime_path:string
  -> ?session_root:string
  -> Runtime.request
  -> (Runtime.response, Error.sdk_error) result
