(** Sessions — public API facade.

    Re-exports types from Sessions_types and store operations from
    Sessions_store. Proof-bundle assembly migrated to
    masc_mcp.cdal_runtime (RFC-OAS-011 OAS-E PR-6). *)

include Sessions_types
include Sessions_store
