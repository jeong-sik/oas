(** Provider kind sum type, split from [Provider_config] so it can be shared
    by {!Types.inference_telemetry} without introducing a
    [Provider_config <-> Types] dependency cycle.

    {!Provider_config} rebinds this type so [Provider_config.Provider_a],
    [Provider_config.string_of_provider_kind], etc. remain valid entry
    points for existing callers.

    @since 0.165.0 *)

type t =
  | Provider_a
  | Provider_c
  | Provider_d_compat
  | Ollama
  | Provider_f
  | Provider_k
  | Provider_h
  | Cli_tool_d
  | Cli_tool_b
  | Cli_tool_c
  | Cli_tool_a

(** All variants in declaration order. Maintained exhaustively alongside
    {!t}; adding a new variant without extending this list is a bug that
    the property test in [test_provider_config] flags immediately. Useful
    for QCheck generators, CLI completion, and iterative exhaustive
    checks. *)
val all : t list

(** Canonical environment variable conventionally consulted for the kind's
    API key (e.g. [Provider_a -> Some "PROVIDER_A_API_KEY"]). Returns [None]
    for kinds that do not have a universally-agreed env var — either the
    kind is local ({!Ollama}), embedded in a subprocess transport
    ({!Cli_tool_d}, {!Cli_tool_b}, {!Cli_tool_a}), or shares a space where
    OAS does not dictate the env name ({!Provider_d_compat}). *)
val default_api_key_env : t -> string option

(** [true] for kinds whose transport is a subprocess CLI
    ({!Cli_tool_d}, {!Cli_tool_b}, {!Cli_tool_c}, {!Cli_tool_a}) rather
    than a direct HTTP request. Centralizes a discrimination that used
    to live as an inline [match _ with Cli_tool_d | … -> true | _ ->
    false] in several modules; lifting it here makes future variant
    additions force an explicit categorization in the compiler.
    @since 0.170.0 *)
val is_subprocess_cli : t -> bool

(** Canonical lowercase wire form (e.g. [Provider_a -> "provider_a"]).
    Exhaustive — adding a new variant forces a compile error. *)
val to_string : t -> string

(** Canonical inverse of {!to_string}. Accepts the 8 canonical forms plus
    the documented legacy aliases [agent_llm_a -> Provider_a],
    [provider_d -> Provider_d_compat], [llama -> Ollama]. Match is case-insensitive
    with leading/trailing whitespace trimmed. Returns [None] for anything
    else so callers fail fast instead of silently defaulting. *)
val of_string : string -> t option

val pp : Format.formatter -> t -> unit
val show : t -> string
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or
