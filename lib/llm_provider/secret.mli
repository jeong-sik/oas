(** Minimal abstract secret holder.

    [t] is a private string so it can be coerced explicitly when needed (e.g.
    in unit tests) but cannot be passed directly to functions expecting a
    plain string without an explicit cast.  This makes accidental logging or
    serialization of API keys a deliberate act rather than a typo.

    @since 0.207.0 *)

type t = private string

val of_string : string -> t

(** [of_env ?getenv var] reads [var] through the canonical environment
    boundary and wraps a non-empty value as a secret. [?getenv] is useful for
    tests/callers that need to resolve values without reading process env. *)
val of_env : ?getenv:(string -> string option) -> string -> t option

val empty : t
val is_empty : t -> bool

(** Return the raw secret value.  This MUST only be used to construct HTTP
    Authorization headers inside the provider transport layer.  Never log,
    serialize, or otherwise persist the returned string. *)
val header_value : t -> string

val length : t -> int

(** SHA-256 digest of the secret rendered as the first 8 hex characters.
    Suitable for distinguishing different keys in telemetry without leaking
    the key material. *)
val fingerprint : t -> string
