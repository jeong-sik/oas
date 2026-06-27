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

(** Structured description of an invalid environment variable value.
    Passed to the optional [on_invalid] callback of the parsers below so
    callers can emit warnings in their own logging schema while reusing
    the canonical parse logic. *)
type invalid_env =
  { var : string
  ; raw : string
  ; expected : string
  }

(** [bool ?default ?on_invalid name] returns [true] when [name] is set to [1],
    [true], [yes], or [on] (case-insensitive), and [false] when set to [0],
    [false], [no], or [off].  Unset or empty values return [default] (default
    [false]).  Invalid values call [on_invalid] if provided, otherwise emit a
    diagnostic warning before returning [default]. *)
val bool : ?default:bool -> ?on_invalid:(invalid_env -> unit) -> string -> bool

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

(** Filter out empty strings from a list. *)
val filter_non_empty : string list -> string list

(** Split on [sep], trim each fragment, discard empty results. *)
val split_on_char_trim : char -> string -> string list

(** [trim_non_empty s] trims [s] and returns [Some trimmed] if non-empty,
    [None] otherwise. *)
val trim_non_empty : string -> string option

(** [trim_non_empty_opt opt] maps [trim_non_empty] over an option. *)
val trim_non_empty_opt : string option -> string option

(** [int ?allow_negative ?on_invalid ~default var] parses env var [var] as an
    integer.  Returns [default] when unset or empty.  Negative values are
    rejected unless [allow_negative] is [true].  Rejected values call
    [on_invalid] if provided, otherwise emit a diagnostic warning before
    falling back to [default]. *)
val int : ?allow_negative:bool -> ?on_invalid:(invalid_env -> unit) -> default:int -> string -> int

(** [float ?allow_negative ?on_invalid ~default var] parses env var [var] as a
    float.  Returns [default] when unset or empty.  Negative and non-finite
    values are rejected unless [allow_negative] is [true] (non-finite values
    are always rejected).  Rejected values call [on_invalid] if provided,
    otherwise emit a diagnostic warning before falling back to [default]. *)
val float : ?allow_negative:bool -> ?on_invalid:(invalid_env -> unit) -> default:float -> string -> float
