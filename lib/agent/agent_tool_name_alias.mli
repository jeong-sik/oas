(** Tool name alias resolution.

    OAS provides a small set of generic, provider-agnostic aliases
    (e.g. [Bash] -> [Execute]) built-in.  SDK-specific aliases
    (e.g. MASC-specific names such as [masc_code_shell]) must be
    registered by the consumer at initialization time via
    {!register_alias}.

    @since 0.93.1 *)

(** [register_alias ~alias ~canonical] adds a mapping from [alias] to
    [canonical] in the mutable alias registry.  If [alias] is already
    registered, the previous mapping is overwritten.

    Consumers such as MASC should call this during their own
    initialization to register SDK-specific tool name aliases. *)
val register_alias : alias:string -> canonical:string -> unit

(** [resolve_alias alias] looks up [alias] in the registry and returns
    the canonical name if found. *)
val resolve_alias : string -> string option

(** [resolve ~requested ~input] attempts to resolve a requested tool
    name to its canonical form, applying input normalization for the
    built-in generic aliases.  Falls back to the mutable registry for
    consumer-registered aliases (no input normalization is applied for
    registry hits). *)
val resolve : requested:string -> input:Yojson.Safe.t -> (string * Yojson.Safe.t) option
