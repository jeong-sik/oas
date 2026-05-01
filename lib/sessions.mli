open Base
(** Sessions — public API facade.

    Re-exports types from {!Sessions_types}, store operations from
    {!Sessions_store}, and proof assembly from {!Sessions_proof}.
    This module is the single entry point for session access.

    @stability Evolving
    @since 0.93.1 *)

include module type of Sessions_types
include module type of Sessions_store
include module type of Sessions_proof
