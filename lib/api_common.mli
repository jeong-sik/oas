open Base
(** Re-export from {!Llm_provider.Api_common} for backward compatibility.

    @stability Internal
    @since 0.93.1 *)
include module type of Llm_provider.Api_common
