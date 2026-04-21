(** Provider kind sum type, split from [Provider_config] so it can be shared
    by {!Types.inference_telemetry} without introducing a
    [Provider_config <-> Types] dependency cycle.

    {!Provider_config} rebinds this type so [Provider_config.Anthropic],
    [Provider_config.string_of_provider_kind], etc. remain valid entry
    points for existing callers.

    @since 0.165.0 *)

type t =
  | Anthropic
  | Kimi
  | OpenAI_compat
  | Ollama
  | Gemini
  | Glm
  | Claude_code
  | Gemini_cli
  | Kimi_cli
  | Codex_cli

val to_string : t -> string
(** Canonical lowercase wire form (e.g. [Anthropic -> "anthropic"]).
    Exhaustive — adding a new variant forces a compile error. *)

val of_string : string -> t option
(** Canonical inverse of {!to_string}. Accepts the 8 canonical forms plus
    the documented legacy aliases [claude -> Anthropic],
    [openai -> OpenAI_compat], [llama -> Ollama]. Match is case-insensitive
    with leading/trailing whitespace trimmed. Returns [None] for anything
    else so callers fail fast instead of silently defaulting. *)

val pp : Format.formatter -> t -> unit
val show : t -> string

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
