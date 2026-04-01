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

    {b Add constraint}: [apply_to_tool_set (Add names)] can only retain
    tools already present in the original [Tool_set.t]. To introduce
    entirely new tools, callers must use [Tool_set.merge] first.

    @since 0.100.0
    @stability Evolving *)

(** Operations that transform a tool name set. *)
type t =
  | Keep_all                      (** Identity -- no change *)
  | Clear_all                     (** Empty the set *)
  | Add of string list            (** Set union *)
  | Remove of string list         (** Set difference *)
  | Replace_with of string list   (** Replace entire set *)
  | Intersect_with of string list (** Set intersection *)
  | Seq of t list                 (** Sequential composition (fold_left) *)

(** {1 Application} *)

val apply : t -> string list -> string list
(** Apply [op] to a tool name set. Result is deduplicated,
    preserving first-occurrence order. *)

val apply_to_tool_set : t -> Tool_set.t -> Tool_set.t
(** Apply [op] to a {!Tool_set.t}. Operates on tool names internally;
    tools not in the original set cannot be added via [Add]. *)

(** {1 Composition} *)

val compose : t list -> t
(** Smart [Seq] constructor.
    - [[]] -> [Keep_all]
    - [[x]] -> [x]
    - Nested [Seq] flattened recursively.
    - Identity ops ([Keep_all], [Add []], [Remove []]) eliminated. *)

(** {1 Conversion} *)

val to_tool_filter : t -> string list -> Guardrails.tool_filter
(** [to_tool_filter op current] applies [op] to [current] and returns
    the result as {!Guardrails.AllowList}. Bridge function for
    [tool_filter_override] in hooks. *)

(** {1 Predicates} *)

val is_identity : t -> bool
(** [Keep_all], [Add []], [Remove []], [Seq] of all identities. *)

val is_destructive : t -> bool
(** [true] for [Clear_all], [Replace_with], [Intersect_with],
    or [Seq] containing any destructive op.
    "Destructive" means information about the previous set is lost. *)

(** {1 Serialization} *)

val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

(** {1 Comparison} *)

val equal : t -> t -> bool
(** Structural equality with name normalization (sort + dedup).
    Compares normalized structure, NOT [apply] semantics.
    [Add ["a";"b"]] and [Add ["b";"a"]] are equal;
    [Add ["a"]] and [Seq [Add ["a"]]] are not. *)
