(** Typed execution-cause validation and closed durable codec. *)

module Make (Event_id : Execution_id.S) : sig
  type t =
    | Internal_event of Event_id.t
    | External_event of
        { source : string
        ; event_id : string
        }

  val validate_all : t list -> (unit, string) result
  val equal : t -> t -> bool
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result
end
