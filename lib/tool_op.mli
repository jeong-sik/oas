(** Tool_op -- algebraic operations on tool name sets.

    Composable, serializable transformations for dynamic tool filtering.
    Unlike {!Guardrails.tool_filter} (predicate over schemas),
    [t] transforms concrete name lists -- suitable for sequential
    composition across multiple hook stages.

    {2 Usage: hook-based dynamic tool gating}

    {[
      (* Grant/revoke tools at runtime via BeforeTurnParams hook *)
      let my_hook current_tools : Hooks.hook = fun event ->
        match event with
        | Hooks.BeforeTurnParams { turn; _ } ->
          let op = Tool_op.compose [
            Tool_op.Remove ["dangerous_shell"];
            (if turn >= 3 then Tool_op.Add ["deploy"] else Tool_op.Keep_all);
          ] in
          let filter = Tool_op.to_tool_filter op current_tools in
          Hooks.AdjustParams {
            Hooks.default_turn_params with
            tool_filter_override = Some filter }
        | _ -> Hooks.Continue
    ]}

    {2 Usage: restrict spawned agent tools}

    {[
      (* Parent has 50 tools; child gets subset *)
      let child_tools =
        Tool_op.apply_to_tool_set
          (Tool_op.Intersect_with ["read"; "search"; "status"])
          parent_tool_set
    ]}

    {b Closed-set constraint}: [apply_to_tool_set] always returns a subset
    of the original [Tool_set.t]. Names not present in the original set are
    silently ignored by {i all} ops (including [Add] and [Replace_with]).
    To introduce entirely new tools, callers must use [Tool_set.merge]
    on the input set first.

    @since 0.100.0
    @stability Evolving *)

(** Operations that transform a tool name set. *)
type t =
  | Keep_all (** Identity -- no change *)
  | Clear_all (** Empty the set *)
  | Add of string list (** Set union *)
  | Remove of string list (** Set difference *)
  | Replace_with of string list (** Replace entire set *)
  | Intersect_with of string list (** Set intersection *)
  | Seq of t list (** Sequential composition (fold_left) *)

(** {1 Application} *)

(** Apply [op] to a tool name set. Result is deduplicated,
    preserving first-occurrence order. *)
val apply : t -> string list -> string list

(** Apply [op] to a {!Tool_set.t}. Result is always a subset of the
    original set — names not present in [ts] are silently ignored
    by all ops. Output order follows [apply] semantics (first-occurrence
    in the op result), not the original [Tool_set.t] order. *)
val apply_to_tool_set : t -> Tool_set.t -> Tool_set.t

(** {1 Composition} *)

(** Smart [Seq] constructor.
    - [[]] -> [Keep_all]
    - [[x]] -> [x]
    - Nested [Seq] flattened recursively.
    - Identity ops ([Keep_all], [Add []], [Remove []]) eliminated. *)
val compose : t list -> t

(** {1 Conversion} *)

(** [to_tool_filter op current] applies [op] to [current] and returns
    the result as {!Guardrails.AllowList}. Bridge function for
    [tool_filter_override] in hooks. *)
val to_tool_filter : t -> string list -> Guardrails.tool_filter

(** {1 Predicates} *)

(** [Keep_all], [Add []], [Remove []], [Seq] of all identities. *)
val is_identity : t -> bool

(** [true] for [Clear_all], [Replace_with], [Intersect_with],
    or [Seq] containing any destructive op.
    "Destructive" means the op can drop tools implicitly, without
    enumerating them (e.g. clear, replace, or intersect with a subset). *)
val is_destructive : t -> bool

(** {1 Serialization} *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

(** {1 Comparison} *)

(** Structural equality with name normalization (sort + dedup).
    Compares normalized structure, NOT [apply] semantics.
    [Add ["a";"b"]] and [Add ["b";"a"]] are equal;
    [Add ["a"]] and [Seq [Add ["a"]]] are not. *)
val equal : t -> t -> bool
