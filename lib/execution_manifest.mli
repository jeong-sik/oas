(** Execution Manifest: The single point of intersection between MASC and OAS. *)

type t = {
  contract : Contract.t;
  mode : Execution_mode.t;
  risk_class : Risk_class.t;
}
