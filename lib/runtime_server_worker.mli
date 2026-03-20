(** Runtime server worker — agent execution and event persistence. *)

open Runtime_server_types
open Runtime_server_resolve

(** {1 Helpers} *)

val extract_text : Types.api_response -> string
val make_event : Runtime.session -> Runtime.event_kind -> Runtime.event

(** {1 Event persistence} *)

val with_store_lock : state -> (unit -> 'a) -> 'a

val persist_event_locked :
  Runtime_store.t -> state -> Runtime.session -> Runtime.event_kind ->
  (Runtime.session * Runtime.event, Error.sdk_error) result

val persist_event :
  Runtime_store.t -> state -> string -> Runtime.event_kind ->
  (Runtime.session * Runtime.event, Error.sdk_error) result

(** {1 Report generation} *)

val generate_report_and_proof :
  Runtime_store.t -> state -> string ->
  (Runtime.session * Runtime.report * Runtime.proof, Error.sdk_error) result

(** {1 Output streaming} *)

val emit_output_delta :
  Runtime_store.t -> state -> string -> string -> string ->
  (unit, Error.sdk_error) result

(** {1 Participant execution} *)

val run_participant :
  Runtime_store.t -> state -> string ->
  execution_resolution -> Runtime.spawn_agent_request ->
  (string, Error.sdk_error) result
