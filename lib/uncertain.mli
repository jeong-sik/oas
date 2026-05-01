open Base
(** Uncertain — typed wrapper for non-deterministic values.

    Wraps LLM outputs and heuristic results with provenance and
    confidence metadata.  Forces callers to explicitly acknowledge
    uncertainty before consuming the inner value.

    The type is [private]: callers can read fields via pattern match
    or dot-access but must use smart constructors to create values.

    @since 0.119.0
    @see "RFC-0001" Det/NonDet boundary hardening *)

(** How the value was produced. *)
type provenance =
  | LLM of
      { model : string
      ; temperature : float option
      }
  | Heuristic of { name : string }
  | Deterministic
  | User

(** Resource pressure at the time of production. *)
type stress =
  { context_pressure : float option
    (** 0.0–1.0 fraction of context window consumed.  [None] if unknown. *)
  ; time_pressure : bool (** [true] when the producer was under a timeout warning. *)
  ; retry_count : int (** How many retries preceded this value. *)
  }

(** A value of type ['a] with provenance and confidence metadata. *)
type 'a t = private
  { value : 'a
  ; confidence : float
  ; provenance : provenance
  ; stress : stress
  }

(** {1 Smart constructors} *)

(** Wrap an LLM-produced value.  Default confidence: [0.5]. *)
val from_llm
  :  model:string
  -> ?temperature:float
  -> ?confidence:float
  -> ?stress:stress
  -> 'a
  -> 'a t

(** Wrap a heuristic-produced value.  Default confidence: [0.3]. *)
val from_heuristic : name:string -> ?confidence:float -> ?stress:stress -> 'a -> 'a t

(** Wrap a value known to be deterministic.
    Confidence is [1.0], provenance is [Deterministic], stress is zero. *)
val deterministic : 'a -> 'a t

(** Wrap a user-supplied value.
    Confidence is [1.0], provenance is [User], stress is zero. *)
val from_user : 'a -> 'a t

(** {1 Accessors} *)

val value : 'a t -> 'a
val confidence : 'a t -> float
val provenance_of : 'a t -> provenance
val stress_of : 'a t -> stress

(** {1 Predicates} *)

(** [is_confident ~threshold u] is [u.confidence >= threshold]. *)
val is_confident : threshold:float -> 'a t -> bool

(** [true] iff provenance is [Deterministic]. *)
val is_deterministic : 'a t -> bool

(** {1 Transformations} *)

(** Transform the inner value, preserving metadata. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** Monadic bind.  Resulting confidence is [min] of both. *)
val bind : ('a -> 'b t) -> 'a t -> 'b t

(** Override confidence (clamped to [0.0, 1.0]). *)
val with_confidence : float -> 'a t -> 'a t

(** {1 Unwrapping — explicit boundary crossing} *)

(** Extract the value.  Caller assumes responsibility for the
    non-deterministic nature.  Prefer [to_result] when possible. *)
val unwrap : 'a t -> 'a

(** [Ok value] when confidence >= [min_confidence],
    [Error reason] otherwise.  The reason includes provenance info. *)
val to_result : min_confidence:float -> 'a t -> ('a, string) result

(** {1 Serialization} *)

(** Zero stress: no context pressure, no time pressure, zero retries. *)
val stress_zero : stress

val provenance_to_yojson : provenance -> Yojson.Safe.t
val provenance_of_yojson : Yojson.Safe.t -> (provenance, string) result
val stress_to_yojson : stress -> Yojson.Safe.t
val stress_of_yojson : Yojson.Safe.t -> (stress, string) result
val to_yojson : ('a -> Yojson.Safe.t) -> 'a t -> Yojson.Safe.t

val of_yojson
  :  (Yojson.Safe.t -> ('a, string) result)
  -> Yojson.Safe.t
  -> ('a t, string) result
