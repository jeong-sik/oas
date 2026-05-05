(** Agent_typed — phantom-type lifecycle state machine.

    @stability Stable
    @since 0.55.0 *)

(* Phantom state tags *)
type created
type completed

(* The phantom type parameter is unused at runtime —
   the underlying representation is always Agent.t.
   OCaml's type system enforces state transitions at compile time. *)
type _ t = T : Agent.t -> _ t

let create ~net ?config ?tools ?context ?options () =
  T (Agent.create ~net ?config ?tools ?context ?options ())
;;

let run ~sw ?clock (T agent) prompt =
  match Agent.run ~sw ?clock agent prompt with
  | Ok response -> Ok (response, T agent)
  | Error e -> Error e
;;

let close (T agent) = Agent.close agent
let inner (T agent) = agent
let card (T agent) = Agent.card agent
let last_trace (T agent) = Agent.last_raw_trace_run agent
