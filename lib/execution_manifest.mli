(** Execution Manifest: The single point of intersection between the coordinator and OAS. *)

type t =
  { contract : Contract.t
  ; mode : Execution_mode.t
  ; risk_class : Risk_class.t
  }
