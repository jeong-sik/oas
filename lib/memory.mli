(** Working memory: 3-tier facade over {!Context.t}.

    - {b Scratchpad} — per-turn ephemeral data (cleared between turns).
    - {b Working} — cross-turn data (survives within session).
    - {b Long_term} — external persistence via callback.

    @since 0.65.0 *)

type tier =
  | Scratchpad
  | Working
  | Long_term

(** Callback for long-term memory persistence. *)
type long_term_backend = {
  persist: key:string -> Yojson.Safe.t -> unit;
  retrieve: key:string -> Yojson.Safe.t option;
  remove: key:string -> unit;
}

type t

val create : ?ctx:Context.t -> ?long_term:long_term_backend -> unit -> t

(** Set or replace the long-term backend. *)
val set_long_term_backend : t -> long_term_backend -> unit

(** Store a value at the given tier. Long_term also invokes the backend. *)
val store : t -> tier:tier -> string -> Yojson.Safe.t -> unit

(** Recall a value. Falls back through lower tiers:
    Scratchpad -> Working -> Long_term. *)
val recall : t -> tier:tier -> string -> Yojson.Safe.t option

(** Recall from a specific tier only, no fallback. *)
val recall_exact : t -> tier:tier -> string -> Yojson.Safe.t option

(** Remove a key from the given tier. *)
val forget : t -> tier:tier -> string -> unit

(** Promote a key from Scratchpad to Working.
    Returns [true] if the key existed in Scratchpad. *)
val promote : t -> string -> bool

(** All entries in the Working tier. *)
val working_entries : t -> (string * Yojson.Safe.t) list

(** All entries in the Scratchpad tier. *)
val scratchpad_entries : t -> (string * Yojson.Safe.t) list

(** Clear all Scratchpad entries (call between turns). *)
val clear_scratchpad : t -> unit

(** Count (scratchpad, working, long_term) entries. *)
val stats : t -> int * int * int

(** Access the underlying context. *)
val context : t -> Context.t
