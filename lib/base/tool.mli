(** Tool definition and execution.

    @stability Stable
    @since 0.93.1 *)

type tool_handler = Yojson.Safe.t -> Types.tool_result
type context_tool_handler = Context.t -> Yojson.Safe.t -> Types.tool_result

type workdir_policy =
  | Required
  | Recommended
  | None_expected
[@@deriving yojson, show]

type concurrency_class =
  | Parallel_read
  | Sequential_workspace
  | Exclusive_external
[@@deriving yojson, show]

(** Permission level for tool execution.
    Consumers use this to decide approval policy per tool.
    @since 0.103.0 *)
type permission =
  | ReadOnly (** No side effects. Safe to execute without confirmation. *)
  | Write (** Local workspace mutations. Policy-dependent approval. *)
  | Destructive (** External effects or irreversible operations. *)
[@@deriving yojson, show]

type shell_constraints =
  { single_command_only : bool
  ; shell_metacharacters_allowed : bool
  ; chaining_allowed : bool
  ; redirection_allowed : bool
  ; pipes_allowed : bool
  ; workdir_policy : workdir_policy option
  }
[@@deriving yojson, show]

type descriptor =
  { kind : string option
  ; mutation_class : string option
  ; concurrency_class : concurrency_class option
  ; permission : permission option
    (** When [Some], indicates the tool's side-effect level.
          Approval hooks can use this to skip confirmation for [ReadOnly]
          tools or require explicit approval for [Destructive] ones.
          [None] means unclassified (legacy tools). *)
  ; shell : shell_constraints option
  ; notes : string list
  ; examples : string list
  }

type handler_kind =
  | Simple of tool_handler
  | WithContext of context_tool_handler

type t =
  { schema : Types.tool_schema
  ; descriptor : descriptor option
  ; handler : handler_kind
  }

val create
  :  ?descriptor:descriptor
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> tool_handler
  -> t

val create_with_context
  :  ?descriptor:descriptor
  -> name:string
  -> description:string
  -> parameters:Types.tool_param list
  -> context_tool_handler
  -> t

val execute : ?context:Context.t -> t -> Yojson.Safe.t -> Types.tool_result
val descriptor : t -> descriptor option

(** Extract permission from a tool's descriptor. [None] if no descriptor
      or no permission set. *)
val permission : t -> permission option

(** Extract permission from a tool's descriptor. [None] if no descriptor
      or no permission set. *)
val is_read_only : t -> bool
(** [true] when [permission t = Some ReadOnly]. *)

(** [true] when [permission t = Some ReadOnly]. *)
val permission_to_string : permission -> string
(** Snake_case string for a permission value.
      [ReadOnly -> "read_only"], [Write -> "write"], [Destructive -> "destructive"].
      Use this for stable consumer-facing output; [show_permission] from
      [\[@@deriving show\]] produces module-qualified CamelCase and is intended
      for diagnostics only.
      @since 0.120.0 *)

(** Snake_case string for a permission value.
      [ReadOnly -> "read_only"], [Write -> "write"], [Destructive -> "destructive"].
      Use this for stable consumer-facing output; [show_permission] from
      [\[@@deriving show\]] produces module-qualified CamelCase and is intended
      for diagnostics only.
      @since 0.120.0 *)
val validate_descriptor : descriptor -> (unit, string) result

val descriptor_to_yojson : descriptor option -> Yojson.Safe.t
val schema_to_json : t -> Yojson.Safe.t

(** Wrap a tool to inject default arguments when not provided.
    Defaults are merged into the input JSON before the handler runs. *)
val with_defaults : (string * Yojson.Safe.t) list -> t -> t
