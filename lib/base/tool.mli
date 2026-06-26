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

(** Side-effect classification for a tool.
    Used to infer the required {!concurrency_class} when the latter is not set
    explicitly. *)
type mutation_class =
  | Read_only
  | Workspace
  | Workspace_mutating
  | Local_mutation
  | External
  | External_effect
[@@deriving show]

val mutation_class_to_string : mutation_class -> string
val mutation_class_of_string : string -> mutation_class option
val mutation_class_to_yojson : mutation_class -> Yojson.Safe.t
val mutation_class_of_yojson : Yojson.Safe.t -> (mutation_class, string) result

(** Permission level for tool execution.
    Consumers use this to decide approval policy per tool.
    @since 0.103.0 *)
type permission =
  | ReadOnly (** No side effects. Safe to execute without confirmation. *)
  | Write (** Local workspace mutations. Policy-dependent approval. *)
  | Destructive (** External effects or irreversible operations. *)
[@@deriving yojson, show]

type evidence_role =
  | File_write
  | Verification
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
  ; mutation_class : mutation_class option
  ; concurrency_class : concurrency_class option
  ; permission : permission option
    (** When [Some], indicates the tool's side-effect level.
          Approval hooks can use this to skip confirmation for [ReadOnly]
          tools or require explicit approval for [Destructive] ones.
          [None] means unclassified (legacy tools). *)
  ; evidence_role : evidence_role option
    (** Optional proof role emitted into raw traces when this tool completes.
          This is declarative tool metadata, not inferred from the tool name
          or textual result. *)
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

(** Stable snake_case string for a concurrency class.
    Use this for logs, hooks, and JSON-adjacent diagnostic output. *)
val concurrency_class_name : concurrency_class -> string

(** Interpret legacy descriptor [mutation_class] strings as concurrency
    classes. This is the single policy surface for old descriptors that have
    not yet been migrated to typed [concurrency_class]. *)
val expected_concurrency_class_of_mutation_class : string -> concurrency_class option

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
val evidence_role_to_string : evidence_role -> string
(** Stable snake_case string for raw-trace proof roles. *)

val validate_descriptor : descriptor -> (unit, string) result
val descriptor_to_yojson : descriptor option -> Yojson.Safe.t
val schema_to_json : t -> Yojson.Safe.t

(** Disclosure depth for tool schema serialization to LLM providers.

    Background: when an agent exposes many tools, sending the full
    [input_schema] for every tool on every turn inflates prompt tokens.
    [Tool_selector] narrows the candidate set, but [schema_to_json] still
    emits the full schema for whatever survives selection. This type
    lets callers choose how much of each surviving tool's schema is sent.

    Risk: [Minimal_index] omits [input_schema]. Models that rely on the
    schema to compose [function_call] arguments may fail to populate
    parameters correctly. Use [Full_schema] (the default) unless model
    compatibility has been verified.

    @since 0.194.0 *)
type disclosure_level =
  | Full_schema
  (** Emit [name], [description], and [input_schema]. Identical to
        legacy [schema_to_json] output. *)
  | Minimal_index
  (** Emit [name] and [description] only. [input_schema] is omitted.
        Used to give the model an "index" of available tools without
        paying for full parameter schemas. *)
  | Hybrid of { full_names : string list }
  (** Tools whose [schema.name] is in [full_names] are rendered with
        [Full_schema]; all others with [Minimal_index]. Useful for the
        2-stage pattern: pre-selected top-K tools get full schemas,
        the remainder are visible by name only. *)
[@@deriving show]

(** Serialize a tool's schema at the requested disclosure level.

    [schema_to_json_with_disclosure Full_schema t] is byte-identical to
    [schema_to_json t]. Callers that don't care about disclosure should
    continue using [schema_to_json].

    @since 0.194.0 *)
val schema_to_json_with_disclosure : disclosure_level -> t -> Yojson.Safe.t

(** Wrap a tool to inject default arguments when not provided.
    Defaults are merged into the input JSON before the handler runs. *)
val with_defaults : (string * Yojson.Safe.t) list -> t -> t
