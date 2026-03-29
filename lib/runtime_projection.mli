(** Runtime session state machine and collaboration bridge.

    Provides the core event-sourcing logic for {!Runtime.session}:
    create sessions, apply events to evolve state, and generate
    reports/proofs.

    Also provides lossy projections between {!Runtime.session} (18 fields)
    and {!Collaboration.t} (12 fields) for the ongoing migration.

    @stability Internal
    @since 0.93.1 *)

(** {1 Session lifecycle} *)

(** Create an initial session from a start request.
    Sets phase to [Bootstrapping], turn count to 0, and
    initialises participants as [Planned]. *)
val initial_session : Runtime.start_request -> Runtime.session

(** Apply a single event to a session, evolving its state.
    Returns [Error] if the session is in a terminal phase
    ([Completed], [Failed], [Cancelled]) when a non-terminal
    event is applied. *)
val apply_event :
  Runtime.session -> Runtime.event -> (Runtime.session, Error.sdk_error) result

(** {1 Reporting and proofs} *)

(** Build a human-readable report from a session and its event trace. *)
val build_report : Runtime.session -> Runtime.event list -> Runtime.report

(** Build a machine-verifiable proof from a session and its event trace.
    Checks: session started, turn recorded, participant outcome,
    terminal phase/event, sequence contiguity, artifact ID uniqueness. *)
val build_proof : Runtime.session -> Runtime.event list -> Runtime.proof

(** {1 Collaboration bridge}

    Lossy projections between {!Runtime.session} and {!Collaboration.t}.
    [Runtime.participant] (21 fields) compresses to
    [Collaboration.participant] (6 fields). *)

(** Extract a {!Collaboration.t} from a {!Runtime.session}.
    [shared_context] is set to a fresh empty context. *)
val collaboration_of_session : Runtime.session -> Collaboration.t

(** Sync collaboration-owned fields back into a {!Runtime.session}.
    Only updates [goal], [phase], [outcome], [updated_at].
    Does {b not} touch [participants], [artifacts], or [contributions]. *)
val update_session_from_collaboration :
  Runtime.session -> Collaboration.t -> Runtime.session
