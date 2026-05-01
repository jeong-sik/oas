open Base
(** Memory access control — agent-scoped permission layer.

    Wraps {!Memory.t} with per-agent, per-tier, per-key permissions.
    Agents can only access memory entries they are authorized for.

    Inspired by Collaborative Memory (arXiv 2505.18279):
    Dynamic Bipartite Access Graph with Read/Write permissions.

    Usage:
    {[
      let acl = Memory_access.create mem in
      Memory_access.grant acl
        { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = ReadWrite };
      Memory_access.grant acl
        { agent_name = "bob"; tier = Working; key_pattern = "shared_*"; permission = Read };

      (* alice can write

    @stability Evolving
    @since 0.93.1 *)
      Memory_access.store acl ~agent:"alice" ~tier:Working "shared_goal" value;  (* Ok *)
      (* bob can only read *)
      Memory_access.store acl ~agent:"bob" ~tier:Working "shared_goal" value;  (* Error *)
    ]}

    @since 0.76.0 *)

(** {1 Permissions} *)

type permission =
  | Read
  | Write
  | ReadWrite

(** A single access policy entry. [key_pattern] supports ["*"] for
    all keys, or a prefix match (e.g., ["shared_"] matches ["shared_goal"]). *)
type policy =
  { agent_name : string
  ; tier : Memory.tier
  ; key_pattern : string
  ; permission : permission
  }

(** {1 Access-controlled memory} *)

type t

(** Create an access-controlled wrapper around a {!Memory.t}.
    By default, no agent has any access (deny-by-default). *)
val create : Memory.t -> t

(** Access the underlying memory (bypasses ACL). *)
val memory : t -> Memory.t

(** {1 Policy management} *)

(** Grant access to an agent. Multiple policies can be granted per agent.
    Later grants do not revoke earlier ones. *)
val grant : t -> policy -> unit

(** Revoke all policies for an agent on a specific tier. *)
val revoke : t -> agent_name:string -> tier:Memory.tier -> unit

(** Revoke all policies for an agent across all tiers. *)
val revoke_all : t -> agent_name:string -> unit

(** List all policies for an agent. *)
val policies_for : t -> string -> policy list

(** {1 Access-controlled operations} *)

(** Access error returned when an operation is denied due to insufficient
    permissions, or when the underlying storage backend fails. *)
type access_error =
  | Denied of
      { agent_name : string
      ; tier : Memory.tier
      ; key : string
      ; needed : permission
      }
  | Backend_failed of
      { agent_name : string
      ; tier : Memory.tier
      ; key : string
      ; op : string
      ; detail : string
      }

(** Store a value, checking Write permission.
    Returns [Error (Denied _)] if the agent lacks write access,
    or [Error (Backend_failed _)] if the underlying store operation fails.

    Note: [Backend_failed] reports a failure in the underlying store/persistence
    step; it does not guarantee that the operation was fully rolled back. In
    particular, for tiers such as [Long_term], the value may already be
    visible in the local in-memory context even though persistence failed.
    Similarly, for [forget], the key may already be removed from the local
    in-memory context even though the backend removal failed. *)
val store
  :  t
  -> agent:string
  -> tier:Memory.tier
  -> string
  -> Yojson.Safe.t
  -> (unit, access_error) result

(** Recall a value, checking Read permission. *)
val recall
  :  t
  -> agent:string
  -> tier:Memory.tier
  -> string
  -> (Yojson.Safe.t option, access_error) result

(** Recall without fallback, checking Read permission. *)
val recall_exact
  :  t
  -> agent:string
  -> tier:Memory.tier
  -> string
  -> (Yojson.Safe.t option, access_error) result

(** Forget a key, checking Write permission. *)
val forget
  :  t
  -> agent:string
  -> tier:Memory.tier
  -> string
  -> (unit, access_error) result

(** {1 Episodic access control} *)

val store_episode : t -> agent:string -> Memory.episode -> (unit, access_error) result

val recall_episodes
  :  t
  -> agent:string
  -> ?now:float
  -> ?decay_rate:float
  -> ?min_salience:float
  -> ?limit:int
  -> unit
  -> (Memory.episode list, access_error) result

(** {1 Procedural access control} *)

val store_procedure : t -> agent:string -> Memory.procedure -> (unit, access_error) result

val find_procedure
  :  t
  -> agent:string
  -> pattern:string
  -> ?min_confidence:float
  -> ?touch:bool
  -> unit
  -> (Memory.procedure option, access_error) result

val best_procedure
  :  t
  -> agent:string
  -> pattern:string
  -> (Memory.procedure option, access_error) result

(** {1 Utilities} *)

(** Check if an agent has a specific permission for a tier and key. *)
val check : t -> agent:string -> tier:Memory.tier -> key:string -> permission -> bool

(** Format an access error for logging. *)
val access_error_to_string : access_error -> string
