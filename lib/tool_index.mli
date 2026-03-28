(** Tool index: BM25-based retrieval for dynamic tool exposure.

    Indexes tool descriptions at agent startup and retrieves the
    most relevant tools for a given query (turn context).

    Implements Okapi BM25 scoring with configurable k1 and b parameters.
    Designed for large tool sets (hundreds of tools): O(n) build, O(n*q) query
    where n = tools, q = query terms.

    @since 0.89.0

    @stability Evolving
    @since 0.93.0 *)

(** {1 Index} *)

type t

(** {1 Tool entry} *)

(** A tool entry in the index: name + description + optional group tag. *)
type entry = {
  name: string;
  description: string;
  group: string option;  (** Tools in the same group are co-retrieved *)
}

(** {1 Configuration} *)

type config = {
  k1: float;       (** Term frequency saturation. Default: 1.5 *)
  b: float;        (** Length normalization. Default: 0.75 *)
  top_k: int;      (** Max results per query. Default: 10 *)
  min_score: float; (** Minimum BM25 score to include. Default: 0.0 *)
}

val default_config : config

(** {1 Construction} *)

(** Build an index from a list of tool entries.
    Tokenizes descriptions and computes IDF for each term. *)
val build : ?config:config -> entry list -> t

(** Build from [Tool.t list], extracting name and description from schemas. *)
val of_tools : ?config:config -> Tool.t list -> t

(** {1 Query} *)

(** Retrieve top-K tool names matching a query string.
    Returns [(name, score)] pairs sorted by score descending.
    Tools in the same group as a matched tool are included. *)
val retrieve : t -> string -> (string * float) list

(** Retrieve tool names only (no scores). *)
val retrieve_names : t -> string -> string list

(** {1 Confidence gate} *)

(** Check if the top result exceeds a confidence threshold.
    Returns [true] if the highest score is >= [threshold].
    If [false], the caller should fall back to the full tool catalog. *)
val confident : t -> string -> threshold:float -> bool

(** {1 Stats} *)

val size : t -> int
val vocabulary_size : t -> int
