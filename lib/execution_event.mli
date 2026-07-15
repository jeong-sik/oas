(** Canonical recursive execution events.

    This module describes execution topology and lifecycle only. Provider
    payloads remain opaque JSON owned by their provider codec; execution
    control must use the typed variants and identifiers below rather than
    inspecting those payloads.

    Events are immutable values. {!Execution_journal} is the authority that
    allocates identifiers and sequence numbers for live execution. *)

module type ID = sig
  type t

  val fresh : unit -> t
  val of_string : string -> (t, string) result
  val to_string : t -> string
  val equal : t -> t -> bool
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
end

module Event_id : ID
module Run_id : ID
module Node_id : ID

type output_block_kind =
  | Text_block
  | Thinking_block
  | Reasoning_details_block
  | Redacted_thinking_block
  | Tool_result_block
  | Image_block
  | Document_block
  | Audio_block
[@@deriving show]

(** A node's immutable semantic identity.

    [provider_tool_use_id] is provider correlation evidence, not execution
    identity. It may be absent or reused by a provider; {!Node_id.t} remains
    authoritative.

    A tool [input] is [Some json] only when the provider codec has already
    materialized canonical parsed input when opening the node. Streaming opens
    use [None], retain raw chunks as [Tool_input_delta], and commit exactly one
    [Tool_input_snapshot]. JSON [null] remains representable as [Some `Null]; no
    sentinel value is used. *)
type node_kind =
  | Agent_run of { agent_name : string }
  | Provider_turn of
      { turn : int
      ; model : string
      ; provider_response_id : string option
      }
  | Output_block of
      { ordinal : int
      ; block_kind : output_block_kind
      }
  | Tool_invocation of
      { provider_tool_use_id : string option
      ; tool_name : string
      ; input : Yojson.Safe.t option
      ; schedule : Hooks.tool_schedule
      }
  | Tool_attempt

val pp_node_kind : Format.formatter -> node_kind -> unit
val show_node_kind : node_kind -> string

type node

val make_node
  :  node_id:Node_id.t
  -> run_id:Run_id.t
  -> parent_node_id:Node_id.t option
  -> kind:node_kind
  -> (node, string) result

val node_id : node -> Node_id.t
val node_run_id : node -> Run_id.t
val parent_node_id : node -> Node_id.t option
val node_kind : node -> node_kind
val equal_node : node -> node -> bool

(** Typed progress carriers. JSON values are opaque evidence produced and
    consumed by the relevant provider/tool codec; they are never discriminated
    by the execution journal. *)
type node_update =
  | Provider_event of Yojson.Safe.t
  | Output_delta of Yojson.Safe.t
  | Output_snapshot of Yojson.Safe.t
  | Tool_input_delta of Yojson.Safe.t
  | Tool_input_snapshot of Yojson.Safe.t
  | Tool_progress of Yojson.Safe.t
  | Tool_result of Yojson.Safe.t
[@@deriving show]

type failure_kind =
  | Provider_failure
  | Tool_failure
  | Hook_failure
  | Observer_failure
  | Persistence_failure
  | Protocol_failure
  | Internal_failure
[@@deriving show]

type failure =
  { kind : failure_kind
  ; detail : string
  ; data : Yojson.Safe.t option
  }
[@@deriving show]

type terminal =
  | Succeeded
  | Failed of failure
  | Cancelled of
      { reason : string option
      ; data : Yojson.Safe.t option
      }
[@@deriving show]

type payload =
  | Node_opened of node
  | Node_updated of
      { node_id : Node_id.t
      ; update : node_update
      }
  | Node_closed of
      { node_id : Node_id.t
      ; terminal : terminal
      }
[@@deriving show]

type t

(** Build a decoded or journal-issued event. The envelope must contain a
    positive sequence number and its [run_id] must match the payload's node or
    target node at reducer time. Graph invariants are enforced by
    {!Execution_journal.Reducer.apply}. *)
val make : envelope:Event_envelope.t -> payload:payload -> (t, string) result

val envelope : t -> Event_envelope.t
val event_id : t -> Event_id.t
val run_id : t -> Run_id.t
val seq : t -> int
val payload : t -> payload
val equal : t -> t -> bool
val node_kind_to_yojson : node_kind -> Yojson.Safe.t
val node_kind_of_yojson : Yojson.Safe.t -> (node_kind, string) result
val node_update_to_yojson : node_update -> Yojson.Safe.t
val node_update_of_yojson : Yojson.Safe.t -> (node_update, string) result
val terminal_to_yojson : terminal -> Yojson.Safe.t
val terminal_of_yojson : Yojson.Safe.t -> (terminal, string) result
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

(** One canonical JSON object. Newline framing is owned by
    {!Execution_journal}. *)
val to_json_string : t -> string

val of_json_string : string -> (t, string) result
