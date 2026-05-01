(** Runtime session state machine.

    Provides the core event-sourcing logic for {!Runtime.session}:
    create sessions, apply events to evolve state, and generate
    reports/proofs.

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
val apply_event
  :  Runtime.session
  -> Runtime.event
  -> (Runtime.session, Error.sdk_error) result

(** Map runtime participant lifecycle states to generic presence statuses for
    collaboration UIs. *)
val participant_presence_status_of_state
  :  Runtime.participant_state
  -> Runtime.participant_presence_status

(** Default persistence and QoS contract for a collaboration channel. *)
val collaboration_metadata_of_channel
  :  Runtime.collaboration_channel
  -> Runtime.collaboration_metadata

(** Project a runtime event into zero or more generic collaboration events.
    Presence outputs are ephemeral; activity outputs are append-only; system
    outputs are aggregated status signals. *)
val collaboration_events_of_event : Runtime.event -> Runtime.collaboration_event list

(** {1 Reporting and proofs} *)

(** Build a human-readable report from a session and its event trace. *)
val build_report : Runtime.session -> Runtime.event list -> Runtime.report

(** Build a machine-verifiable proof from a session and its event trace.
    Checks: session started, turn recorded, participant outcome,
    terminal phase/event, sequence contiguity, artifact ID uniqueness. *)
val build_proof : Runtime.session -> Runtime.event list -> Runtime.proof
