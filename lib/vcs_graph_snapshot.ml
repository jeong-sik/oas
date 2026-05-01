open Base
type ref_pointer =
  { name : string
  ; target : string
  ; kind : string
  }
[@@deriving yojson, show]

type commit_node =
  { id : string
  ; summary : string option [@yojson.default None]
  ; author_name : string option [@yojson.default None]
  ; author_email : string option [@yojson.default None]
  ; authored_at : string option [@yojson.default None]
  ; committer_name : string option [@yojson.default None]
  ; committer_email : string option [@yojson.default None]
  ; committed_at : string option [@yojson.default None]
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
  ; index : string option [@yojson.default None]
  ; working_tree : string option [@yojson.default None]
  ; original_path : string option [@yojson.default None]
  }
[@@deriving yojson, show]

type conflict_entry =
  { path : string
  ; kind : string
  ; ours : string option [@yojson.default None]
  ; theirs : string option [@yojson.default None]
  ; ancestor : string option [@yojson.default None]
  }
[@@deriving yojson, show]

type t =
  { schema_version : int
  ; repo_root : string
  ; captured_at : string
  ; head : string option [@yojson.default None]
  ; refs : ref_pointer list
  ; commits : commit_node list
  ; edges : commit_edge list
  ; worktree_status : worktree_status list
  ; conflicts : conflict_entry list
  }
[@@deriving yojson, show]

let schema_version_current = 1
let to_json = to_yojson
let of_json = of_yojson
