(** Private restart recovery and turn dispatch for one durable provider turn. *)

(** Dispatch one pipeline turn against the restored durable-execution scope.
    Consumes the one-shot resume flag, then routes on what the scope found at
    [ordinal]: an in-progress turn/provider is resumed, an already-settled turn
    boundary is replayed ([tools_settled] for a completed tool turn, [terminal]
    reconstructing the final assistant response), and no resume runs [fresh].
    Fails closed on inconsistent restored topology. *)
val dispatch
  :  Agent_types.t
  -> ordinal:int
  -> execute:(Types.content_block Nonempty.t -> ('a, Error.sdk_error) result)
  -> tools_settled:'a
  -> terminal:(Types.api_response -> 'a)
  -> fresh:(unit -> ('a, Error.sdk_error) result)
  -> ('a, Error.sdk_error) result
