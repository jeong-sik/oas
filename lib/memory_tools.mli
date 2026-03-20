(** Ready-made memory tools for attaching structured memory to agents.

    This module provides explicit tool factories over {!Memory.t} and
    {!Memory_access.t}. Tools are never auto-injected; callers opt in by
    attaching the returned {!Tool.t} values.

    The v1 surface is intentionally focused:
    - generic key/value remember/recall on Scratchpad / Working / Long_term
    - episodic write
    - procedural lookup

    ACL-aware variants bind [agent_name] at factory time so the model cannot
    spoof an agent identity through tool parameters. *)

(** {1 Plain memory factories} *)

val remember : Memory.t -> Tool.t
val recall : Memory.t -> Tool.t
val remember_episode : Memory.t -> Tool.t
val find_procedure : Memory.t -> Tool.t
val all : Memory.t -> Tool.t list

(** {1 ACL-aware factories} *)

val remember_acl : Memory_access.t -> agent_name:string -> Tool.t
val recall_acl : Memory_access.t -> agent_name:string -> Tool.t
val remember_episode_acl : Memory_access.t -> agent_name:string -> Tool.t
val find_procedure_acl : Memory_access.t -> agent_name:string -> Tool.t
val all_acl : Memory_access.t -> agent_name:string -> Tool.t list
