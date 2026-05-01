open Base
(** Runtime health snapshot primitives.

    This is a small, provider-agnostic shape for reporting whether the
    runtime can safely continue: provider, transport, checkpoint, context,
    event bus, or adapter-specific probes can all be represented here. *)

type status =
  | Status_ok
  | Degraded
  | Failed
  | Unknown

type probe_name =
  | Provider
  | Transport
  | Checkpoint
  | Context
  | Event_bus
  | Custom of string

type probe =
  { name : probe_name
  ; status : status
  ; detail : string option
  ; checked_at : float
  ; latency_ms : float option
  }

type t =
  { generated_at : float
  ; overall : status
  ; probes : probe list
  }

val status_to_string : status -> string
val status_of_string : string -> (status, string) result
val probe_name_to_string : probe_name -> string
val probe_name_of_string : string -> probe_name

val make_probe
  :  name:probe_name
  -> status:status
  -> ?detail:string
  -> ?checked_at:float
  -> ?latency_ms:float
  -> unit
  -> probe

val overall_status : probe list -> status
val make : ?generated_at:float -> probe list -> t
val probe_to_json : probe -> Yojson.Safe.t
val probe_of_json : Yojson.Safe.t -> (probe, string) result
val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (t, string) result
