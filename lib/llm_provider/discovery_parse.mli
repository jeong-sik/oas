(** Discovery_parse — pure JSON parsers for the llama-server discovery
    surface.

    Extracted from {!Discovery} (lines 122-195 in the pre-split
    [discovery.ml]) so the parse layer can be reused by callers that
    just need to interpret one of the discovery JSON shapes without
    pulling in the I/O + Atomic-state half of [Discovery].

    {!Discovery} re-exports {!model_info}, {!server_props}, and
    {!slot_status} as transparent record aliases so existing imports
    of [Discovery.model_info] etc. continue to type-check and continue
    to pattern-match against record literals.

    Pure module: no I/O, no env reads, no async. *)

(** Model info from [/v1/models]. *)
type model_info =
  { id : string
  ; owned_by : string
  }

(** Server properties from [/props]. *)
type server_props =
  { total_slots : int
  ; ctx_size : int
  ; model : string
  ; supports_tools : bool option
  }

(** Slot utilization from [/slots]. *)
type slot_status =
  { total : int
  ; busy : int
  ; idle : int
  }

val parse_models : Yojson.Safe.t -> model_info list
(** OpenAI-compatible [/v1/models] response → list of [{ id; owned_by }].
    Returns the empty list on missing/non-list [data], or when no item
    has a string [id].  [owned_by] defaults to ["unknown"] when absent. *)

val parse_props : Yojson.Safe.t -> server_props option
(** llama-server [/props] response → typed {!server_props}.  Requires a
    numeric [total_slots]; falls back to [0] for missing [n_ctx] and
    to [""] for missing [model].  Returns [None] when [total_slots] is
    missing or non-numeric. *)

val parse_slots : Yojson.Safe.t -> slot_status option
(** llama-server [/slots] response → typed {!slot_status}.  Counts a
    slot as busy when [is_processing] is [true] OR when [state] is a
    non-zero integer.  Returns [None] when the input is not a non-empty
    list. *)
