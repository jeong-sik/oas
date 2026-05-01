open Base
(** Agent Registry -- capability-based multi-agent discovery.

    Maintains a registry of local and remote agents, supporting
    capability-based lookup for orchestration routing.

    @stability Internal
    @since 0.93.1 *)

type agent_entry =
  | Local of
      { agent : Agent.t
      ; card : Agent_card.agent_card
      }
  | Remote of
      { url : string
      ; card : Agent_card.agent_card
      }

type t

val create : unit -> t
val register_local : t -> name:string -> Agent.t -> unit
val register_remote : t -> name:string -> url:string -> Agent_card.agent_card -> unit
val unregister : t -> string -> unit
val lookup : t -> string -> agent_entry option
val list_all : t -> (string * agent_entry) list
val list_by_capability : t -> Agent_card.capability -> (string * agent_entry) list
val list_by_tool : t -> string -> (string * agent_entry) list

val fetch_remote_card
  :  sw:Eio.Switch.t
  -> net:_ Eio.Net.t
  -> string
  -> (Agent_card.agent_card, Error.sdk_error) result

val discover_and_register
  :  sw:Eio.Switch.t
  -> net:_ Eio.Net.t
  -> t
  -> name:string
  -> url:string
  -> (unit, Error.sdk_error) result

val count : t -> int
val card_of_entry : agent_entry -> Agent_card.agent_card
