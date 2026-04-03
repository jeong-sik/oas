(** Tool index: BM25-based retrieval for dynamic tool exposure.

    Indexes tool descriptions at agent startup and retrieves the
    most relevant tools for a given query (turn context).

    Implements Okapi BM25 scoring with configurable k1 and b parameters.
    Designed for large tool sets (hundreds of tools): O(n) build, O(n*q) query
    where n = tools, q = query terms.

    @since 0.89.0

    @stability Evolving
    @since 0.93.1 *)

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

(** {1 Tokenization} *)

(** Default tokenizer: UTF-8 aware, splits on whitespace/punctuation,
    lowercases ASCII, min-length 2 for ASCII tokens.
    Exposed so callers can compose: [fun s -> tokenize s |> my_filter]. *)
val tokenize : string -> string list

(** {1 Construction} *)

(** Build an index from a list of tool entries.
    Tokenizes descriptions and computes IDF for each term.
    Pass [~tokenizer] to override the default tokenizer (e.g. for
    language-specific stemming or particle stripping). *)
val build : ?config:config -> ?tokenizer:(string -> string list) -> entry list -> t

(** Build from [Tool.t list], extracting name and description from schemas. *)
val of_tools : ?config:config -> ?tokenizer:(string -> string list) -> Tool.t list -> t

(** {1 Query} *)

(** Retrieve top-K tool names matching a query string.
    Returns [(name, score)] pairs sorted by score descending.
    Tools in the same group as a matched tool are included. *)
val retrieve : t -> string -> (string * float) list

(** Retrieve tool names only (no scores). *)
val retrieve_names : t -> string -> string list

(** Like [retrieve] but only returns results where the tool name is in [active].
    Useful when callers have a pre-selected set of allowed tools.
    Implementation: calls [retrieve] then post-filters. O(k) where k = result size. *)
val retrieve_within : t -> active:string list -> string -> (string * float) list

(** Like [retrieve] but only returns results where [filter name] is true.
    Generalizes [retrieve_within] for arbitrary predicates (e.g. prefix match,
    capability check).
    Implementation: calls [retrieve] then post-filters. O(k) where k = result size. *)
val retrieve_filtered : t -> filter:(string -> bool) -> string -> (string * float) list

(** {1 Confidence gate} *)

(** Check if the top result exceeds a confidence threshold.
    Returns [true] if the highest score is >= [threshold].
    If [false], the caller should fall back to the full tool catalog. *)
val confident : t -> string -> threshold:float -> bool

(** {1 Stats} *)

val size : t -> int
val vocabulary_size : t -> int
