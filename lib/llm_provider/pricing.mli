(** Per-model cost estimation.

    @stability Internal
    @since 0.93.1 *)

type pricing =
  { input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

(** A runtime pricing override entry.
    [pattern] is matched as a case-insensitive substring against the model ID.
    Entries are checked in order; the first match wins.

    @since 0.185.0 *)
type pricing_entry =
  { pattern : string
  ; input_per_million : float
  ; output_per_million : float
  ; cache_write_multiplier : float
  ; cache_read_multiplier : float
  }

(** Substring match helper. *)
val string_contains : needle:string -> string -> bool

(** Look up pricing for a model ID (case-insensitive, prefix-matched).
    Returns [None] when the model is not in the pricing catalog.
    Use this to distinguish unknown models from genuinely free ones. *)
val pricing_for_model_opt : string -> pricing option

(** Like [pricing_for_model_opt] but returns zero pricing for unknown models.
    Backward-compatible: callers that do not need to distinguish
    unknown from free should use this. *)
val pricing_for_model : string -> pricing

(** Zero-cost pricing constant for local/free models. *)
val zero_pricing : pricing

(** Estimate USD cost from token counts. *)
val estimate_cost
  :  pricing:pricing
  -> input_tokens:int
  -> output_tokens:int
  -> ?cache_creation_input_tokens:int
  -> ?cache_read_input_tokens:int
  -> unit
  -> float

(** Estimate cost for a usage record from its model ID. *)
val estimate_usage_cost : model_id:string -> Types.api_usage -> float

(** Fill [usage.cost_usd] using the pricing table when absent. *)
val annotate_usage_cost : model_id:string -> Types.api_usage -> Types.api_usage

(** Fill [response.usage.cost_usd] using [response.model] when absent. *)
val annotate_response_cost : Types.api_response -> Types.api_response

(** {2 Dynamic pricing overrides}

    Override entries take priority over the built-in static pricing table.
    This allows pricing to be updated at runtime — e.g. at startup via
    {!pricing_overrides_from_env} — without requiring a new code release.

    The override table is process-wide and thread-safe ({!Atomic.t}).
    A {!Diag.warn} is emitted once per process lifetime when the loaded
    overrides are older than 24 h (see {!pricing_ttl_s}).

    @since 0.185.0 *)

(** TTL for loaded pricing overrides (seconds). 86 400 = 24 h. *)
val pricing_ttl_s : float

(** Install [entries] as the process-wide override table.
    Resets the loaded-at timestamp and clears the staleness-warned flag.
    Subsequent {!pricing_for_model_opt} calls check these entries first. *)
val install_pricing_overrides : pricing_entry list -> unit

(** Remove all installed overrides; subsequent lookups use the static table. *)
val clear_pricing_overrides : unit -> unit

(** Parse a single override entry from a JSON object.
    Required keys: [pattern] (non-empty string), [input_per_million] (float),
    [output_per_million] (float).
    Optional keys: [cache_write_multiplier] (float, default 1.0),
    [cache_read_multiplier] (float, default 1.0). *)
val pricing_entry_of_json : Yojson.Safe.t -> (pricing_entry, string) result

(** Parse a JSON array of override entries.
    Returns [Error] when the top-level value is not an array, or any entry
    is malformed. *)
val parse_pricing_entries_json : Yojson.Safe.t -> (pricing_entry list, string) result

(** Load pricing overrides from a JSON file.
    On success the overrides are installed and [Ok ()] is returned.
    On failure the existing overrides are unchanged and [Error msg] is returned. *)
val load_pricing_file : string -> (unit, string) result

(** Read pricing overrides from environment variables and install them.

    Checks [OAS_PRICING_FILE] first: if set and non-empty, reads the file and
    installs its entries, logging success/failure via {!Diag}.

    Falls back to [OAS_PRICING_OVERRIDES] if [OAS_PRICING_FILE] is absent:
    expects an inline JSON array string; logs parse errors as warnings.

    Call this once at application startup before serving requests. *)
val pricing_overrides_from_env : unit -> unit
