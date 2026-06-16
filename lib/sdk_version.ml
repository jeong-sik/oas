(** Single source of truth for the SDK version string.
    All other modules reference this instead of hardcoding. *)

let version = "0.206.12.1" (* x-release-please-version; pin-only bump for masc-pin-timeout-2099 *)
let sdk_name = "agent_sdk"
