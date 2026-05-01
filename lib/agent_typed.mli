(** Agent_typed — phantom-type lifecycle state machine.

    Wraps {!Agent.t} with phantom type parameters that encode lifecycle
    state at the type level. Prevents calling [run] on a completed agent
    or [close] before completion at compile time.

    This is an {b experimental} opt-in module. Existing [Agent.t] API
    is unaffected.

    Usage:
    {[
      let agent = Agent_typed.create ~net () in
      let completed = Agent_typed.run ~sw agent "hello" in
      Agent_typed.close completed
      (* Agent_typed.run ~sw completed "again"  -- TYPE ERROR

    @stability Evolving
    @since 0.93.1 *)
    ]}

    @since 0.55.0 *)

(** Phantom state tags. Not constructible — used only as type parameters. *)
type created

type completed

(** Agent with phantom lifecycle state. *)
type _ t

(** {1 Construction} *)

(** Create a new agent in the [created] state. *)
val create
  :  net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
  -> ?config:Types.agent_config
  -> ?tools:Tool.t list
  -> ?context:Context.t
  -> ?options:Agent.options
  -> unit
  -> created t

(** {1 Execution} *)

(** Run the agent. Transitions [created -> completed].
    Returns the response and the agent in [completed] state. *)
val run
  :  sw:Eio.Switch.t
  -> ?clock:_ Eio.Time.clock
  -> created t
  -> string
  -> (Types.api_response * completed t, Error.sdk_error) result

(** {1 Cleanup} *)

(** Close a completed agent, releasing resources. *)
val close : completed t -> unit

(** {1 Accessors (any state)} *)

(** Access the underlying {!Agent.t}. Escape hatch for interop. *)
val inner : _ t -> Agent.t

(** Get the agent card. *)
val card : _ t -> Agent_card.agent_card

(** Get the last raw trace run reference. *)
val last_trace : _ t -> Raw_trace.run_ref option
