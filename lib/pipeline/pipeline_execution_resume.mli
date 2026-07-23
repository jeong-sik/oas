(** Private restart recovery and turn dispatch for one durable provider turn. *)

(** Dispatch one pipeline turn against the restored durable-execution scope.
    Consumes the one-shot resume flag, then routes on what the scope found at the
    durable turn frontier: an in-progress turn/provider is resumed, an
    already-settled turn boundary is replayed ([tools_settled] for a completed
    tool turn, [terminal] reconstructing the final assistant response), and no
    resume runs [fresh]. [tools_settled_before_checkpoint] repairs only the
    crash window where invocation results settled before the Agent checkpoint;
    its invocation and result authority comes exclusively from the journal.
    [execute] receives the durable turn identity ([turn]), owned by the journal
    rather than reconstructed from mutable agent state. Fails closed on
    inconsistent restored topology. *)
val dispatch
  :  Agent_types.t
  -> execute:(turn:int -> Types.content_block Nonempty.t -> ('a, Error.sdk_error) result)
  -> tools_settled_before_checkpoint:
       (turn:int
        -> invocations:Tool.Invocation.t list
        -> tool_results:Types.content_block list
        -> Types.content_block Nonempty.t
        -> ('a, Error.sdk_error) result)
  -> tools_settled:
       (turn:int
        -> invocations:Tool.Invocation.t list
        -> tool_results:Types.content_block list
        -> Types.content_block Nonempty.t
        -> ('a, Error.sdk_error) result)
  -> terminal:(Types.api_response -> 'a)
  -> fresh:(unit -> ('a, Error.sdk_error) result)
  -> ('a, Error.sdk_error) result
