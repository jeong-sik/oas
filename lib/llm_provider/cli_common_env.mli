open Base
(** Environment-variable driven flag helpers for CLI transports.

    The three [transport_*_cli] modules each translate a small,
    transport-specific set of OAS_* env vars into extra CLI arguments.
    Keeping the lookup centralised avoids drift (e.g. one transport
    accepting "1"/"true" and another only "true").

    Design:
    - config record fields are never extended — env vars are the only
      user-visible surface added in this module.
    - [lookup_*] helpers return None when the variable is unset or
      empty, so call sites use [Option.iter] / pattern match to append
      flags idempotently.
    - Boolean envs accept [1 | true | yes | on] (case-insensitive);
      anything else is treated as unset.

    @since 0.159.0 *)

(** [get name] returns [Some v] if [name] is set to a non-empty string,
    [None] otherwise.  Whitespace-only values are treated as unset. *)
val get : string -> string option

(** [bool name] returns [true] when [name] is set to [1], [true], [yes],
    or [on] (case-insensitive).  Any other value (including unset)
    returns [false]. *)
val bool : string -> bool

(** [list ?sep name] splits the value of [name] on [sep] (default
    comma) and trims each token.  Empty tokens are dropped.  Unset,
    empty, and whitespace-only all return [None] — callers wanting a
    distinct "disable-all" signal must use a dedicated boolean env
    variable, since [Unix.putenv] cannot truly unset and an empty
    value would otherwise leak across processes/tests. *)
val list : ?sep:char -> string -> string list option

(** [kv_pairs name] parses a comma-separated list of [key=value]
    entries into an association list.  Whitespace around keys/values
    is trimmed.  Entries without an [=] separator are dropped.
    Returns [None] when [name] is unset. *)
val kv_pairs : string -> (string * string) list option
