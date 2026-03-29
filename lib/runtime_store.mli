(** File-based session store for the OAS runtime.

    Provides path construction, text I/O, and serialization for
    sessions, events, artifacts, reports, and proofs.

    @stability Internal
    @since 0.93.1 *)

(** Store handle wrapping a root directory. *)
type t = { root: string }

(** {1 Store creation} *)

val default_root : unit -> string
val create : ?root:string -> unit -> (t, Error.sdk_error) result
val ensure_dir : string -> (unit, Error.sdk_error) result
val ensure_tree : t -> string -> (unit, Error.sdk_error) result

(** {1 Path constructors} *)

val sessions_dir : t -> string
val session_dir : t -> string -> string
val session_path : t -> string -> string
val events_path : t -> string -> string
val snapshots_dir : t -> string -> string
val artifacts_dir : t -> string -> string
val raw_traces_dir : t -> string -> string
val report_json_path : t -> string -> string
val report_md_path : t -> string -> string
val proof_json_path : t -> string -> string
val proof_md_path : t -> string -> string

(** {1 Text I/O} *)

val save_text : string -> string -> (unit, Error.sdk_error) result
val load_text : string -> (string, Error.sdk_error) result

(** {1 Session I/O} *)

val save_session : t -> Runtime.session -> (unit, Error.sdk_error) result
val load_session : t -> string -> (Runtime.session, Error.sdk_error) result

(** {1 Event I/O} *)

val append_event : t -> string -> Runtime.event -> (unit, Error.sdk_error) result
val read_events :
  t -> string -> ?after_seq:int -> unit ->
  (Runtime.event list, Error.sdk_error) result

(** {1 Snapshots} *)

val snapshot_path : t -> string -> seq:int -> label:string option -> string
val save_snapshot :
  t -> Runtime.session -> label:string option ->
  (string, Error.sdk_error) result

(** {1 Artifacts} *)

val save_artifact_text :
  t -> string -> name:string -> kind:string -> content:string ->
  (string, Error.sdk_error) result

(** {1 Reports and proofs} *)

val save_report : t -> Runtime.report -> (unit, Error.sdk_error) result
val save_proof : t -> Runtime.proof -> (unit, Error.sdk_error) result
