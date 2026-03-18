(** Append Instruction — hook-based dynamic instruction injection.

    Extends the static {!Contract.add_instruction_layer} with runtime-
    dynamic instruction sources. Converts an instruction config into a
    {!Hooks.hook} that injects extra system context via [AdjustParams]
    on each [BeforeTurnParams] event.

    @since 0.55.0 *)

(** Instruction source — where to read instruction text from. *)
type instruction_source =
  | Static of string
      (** Fixed text, injected every turn. *)
  | FromContext of string
      (** Read from a {!Context.t} key (string value). *)
  | FromFile of string
      (** Read from a file path at invocation time. *)
  | Dynamic of (int -> string option)
      (** Turn number -> optional instruction. *)

(** Position in the system prompt where instructions are inserted. *)
type position =
  | Before_system
  | After_system
  | Per_turn

(** Configuration for instruction injection. *)
type config = {
  sources: instruction_source list;
  position: position;
}

(** Render all sources into a single instruction string for the given turn.
    Returns [None] if all sources produce no output. *)
val render :
  ?context:Context.t ->
  turn:int ->
  config ->
  string option

(** Convert an instruction config into a {!Hooks.hook} that returns
    [AdjustParams { extra_system_context }] on [BeforeTurnParams] events.
    All other events receive [Continue]. *)
val as_hook :
  ?context:Context.t ->
  config ->
  Hooks.hook
