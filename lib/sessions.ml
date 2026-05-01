(** Sessions — public API facade.

    Re-exports types from Sessions_types, store operations from
    Sessions_store, and proof assembly from Sessions_proof.
    This module is the single entry point referenced by agent_sdk.mli. *)

include Sessions_types
include Sessions_store
include Sessions_proof
