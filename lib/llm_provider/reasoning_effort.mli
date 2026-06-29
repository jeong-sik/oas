(** Canonical OpenAI-compatible reasoning effort values.

    This module is the single source of truth for the typed effort set,
    canonical wire serialization, parsing at env/API boundaries, and budget
    threshold mapping. Provider-specific aliasing belongs in
    {!Reasoning_dialect}. *)

type t =
  | None_
  | Minimal
  | Low
  | Medium
  | High
  | XHigh

val all : t list
val to_string : t -> string
val all_wire_values : string list
val of_string : string -> t option
val values_for_log : string
val low_budget_max_tokens : int
val medium_budget_max_tokens : int
val high_budget_max_tokens : int
val of_budget : int -> t option

(** Budget mapping for providers that expose the top effort tier separately. *)
val of_budget_with_xhigh : int -> t option
