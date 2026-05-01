open Base
(** Generic VCS graph snapshot contract.

    This module contains pure data types only. It does not execute VCS
    commands, infer downstream coordinator semantics, or carry renderer-specific fields. *)

type ref_pointer =
  { name : string
  ; target : string
  ; kind : string
  }
[@@deriving yojson, show]

type commit_node =
  { id : string
  ; summary : string option
  ; author_name : string option
  ; author_email : string option
  ; authored_at : string option
  ; committer_name : string option
  ; committer_email : string option
  ; committed_at : string option
  ; parents : string list
  }
[@@deriving yojson, show]

type commit_edge =
  { parent : string
  ; child : string
  ; kind : string
  }
[@@deriving yojson, show]

type worktree_status =
  { path : string
  ; index : string option
  ; working_tree : string option
  ; original_path : string option
  }
[@@deriving yojson, show]

type conflict_entry =
  { path : string
  ; kind : string
  ; ours : string option
  ; theirs : string option
  ; ancestor : string option
  }
[@@deriving yojson, show]

type t =
  { schema_version : int
  ; repo_root : string
  ; captured_at : string
  ; head : string option
  ; refs : ref_pointer list
  ; commits : commit_node list
  ; edges : commit_edge list
  ; worktree_status : worktree_status list
  ; conflicts : conflict_entry list
  }
[@@deriving yojson, show]

val schema_version_current : int
val to_json : t -> Yojson.Safe.t
val of_json : Yojson.Safe.t -> (t, string) result
