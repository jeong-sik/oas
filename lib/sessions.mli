(** Sessions — public API facade.

    Re-exports types from {!Sessions_types} and store operations from
    {!Sessions_store}. Proof-bundle assembly previously included via
    {!Sessions_proof} has migrated to masc_mcp.cdal_runtime  (* boundary-allow *)
    (RFC-OAS-011 OAS-E PR-6).

    @stability Evolving
    @since 0.93.1 *)

include module type of Sessions_types
include module type of Sessions_store
