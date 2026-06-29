(** Tool name alias resolution.

    OAS core does not ship consumer tool aliases.  Downstream consumers
    that expose provider-facing aliases must register each alias during
    their own initialization via {!register_alias}.

    @since 0.93.1 *)

(** [register_alias ~alias ~canonical] adds a mapping from [alias] to
    [canonical] in the mutable alias registry.  If [alias] is already
    registered, the previous mapping is overwritten.

    Downstream consumers should call this during their own
    initialization to register SDK-specific tool name aliases. *)
val register_alias : alias:string -> canonical:string -> unit

(** [resolve_alias alias] looks up [alias] in the registry and returns
    the canonical name if found. *)
val resolve_alias : string -> string option

(** [resolve ~requested ~input] attempts to resolve a requested tool
    name to its consumer-registered canonical form.  Alias resolution
    preserves the original JSON input unchanged. *)
val resolve : requested:string -> input:Yojson.Safe.t -> (string * Yojson.Safe.t) option
