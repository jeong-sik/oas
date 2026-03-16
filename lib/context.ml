(** Cross-turn shared state for agent execution.
    Inspired by Google ADK's session.state pattern.

    Uses Hashtbl internally -- mutable, safe within a single Eio domain.
    Values are Yojson.Safe.t for flexibility while maintaining serializability. *)

type t = (string, Yojson.Safe.t) Hashtbl.t
type scope =
  | App
  | User
  | Session
  | Temp
  | Custom of string

type diff = {
  added: (string * Yojson.Safe.t) list;
  removed: string list;
  changed: (string * Yojson.Safe.t) list;
}

let create () : t = Hashtbl.create 16

let get (ctx : t) key =
  Hashtbl.find_opt ctx key

let set (ctx : t) key value =
  Hashtbl.replace ctx key value

let delete (ctx : t) key =
  Hashtbl.remove ctx key

let keys (ctx : t) =
  Hashtbl.fold (fun k _ acc -> k :: acc) ctx []

let snapshot (ctx : t) =
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx []
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

let scope_prefix = function
  | App -> "app:"
  | User -> "user:"
  | Session -> "session:"
  | Temp -> "temp:"
  | Custom raw ->
      let raw = String.trim raw in
      if raw = "" then "custom:"
      else if String.ends_with ~suffix:":" raw then raw
      else raw ^ ":"

let scoped_key scope key =
  scope_prefix scope ^ key

let get_scoped (ctx : t) scope key =
  get ctx (scoped_key scope key)

let set_scoped (ctx : t) scope key value =
  set ctx (scoped_key scope key) value

let delete_scoped (ctx : t) scope key =
  delete ctx (scoped_key scope key)

let keys_in_scope (ctx : t) scope =
  let prefix = scope_prefix scope in
  let prefix_len = String.length prefix in
  keys ctx
  |> List.filter_map (fun key ->
         if String.length key >= prefix_len
            && String.sub key 0 prefix_len = prefix
         then
           Some (String.sub key prefix_len (String.length key - prefix_len))
         else
           None)
  |> List.sort String.compare

let merge (ctx : t) (pairs : (string * Yojson.Safe.t) list) =
  List.iter (fun (k, v) -> Hashtbl.replace ctx k v) pairs

let diff before after =
  let before_snapshot = snapshot before in
  let after_snapshot = snapshot after in
  let before_map = Hashtbl.create (List.length before_snapshot + 1) in
  let after_map = Hashtbl.create (List.length after_snapshot + 1) in
  List.iter (fun (k, v) -> Hashtbl.replace before_map k v) before_snapshot;
  List.iter (fun (k, v) -> Hashtbl.replace after_map k v) after_snapshot;
  let added =
    after_snapshot
    |> List.filter (fun (k, _) -> not (Hashtbl.mem before_map k))
  in
  let removed =
    before_snapshot
    |> List.filter_map (fun (k, _) ->
           if Hashtbl.mem after_map k then None else Some k)
  in
  let changed =
    after_snapshot
    |> List.filter_map (fun (k, v) ->
           match Hashtbl.find_opt before_map k with
           | Some prev when prev = v -> None
           | Some _ -> Some (k, v)
           | None -> None)
  in
  { added; removed; changed }

let to_json (ctx : t) : Yojson.Safe.t =
  let pairs = snapshot ctx in
  `Assoc pairs

let of_json (json : Yojson.Safe.t) : t =
  let ctx = create () in
  (match json with
   | `Assoc pairs -> List.iter (fun (k, v) -> Hashtbl.replace ctx k v) pairs
   | _ -> ());
  ctx

let copy (ctx : t) : t =
  let new_ctx = create () in
  Hashtbl.iter (fun k v -> Hashtbl.replace new_ctx k v) ctx;
  new_ctx

(* ── Scoped isolation for sub-agent delegation ───────────────── *)

(** An isolated scope for sub-agent execution.
    [parent] is the parent context (read-only reference).
    [local] is the sub-agent's working context.
    [propagate_up] lists keys that should be merged back to parent.
    [propagate_down] lists keys inherited from parent at creation. *)
type isolated_scope = {
  parent: t;
  local: t;
  propagate_up: string list;
  propagate_down: string list;
}

(** Create an isolated scope from a parent context.
    Only keys listed in [propagate_down] are copied to the local context. *)
let create_scope ~parent ~propagate_down ~propagate_up =
  let local = create () in
  List.iter (fun key ->
    match get parent key with
    | Some v -> set local key v
    | None -> ()
  ) propagate_down;
  { parent; local; propagate_up; propagate_down }

(** Merge specified keys from the local context back into the parent.
    Only keys listed in [propagate_up] are merged. *)
let merge_back scope =
  List.iter (fun key ->
    match get scope.local key with
    | Some v -> set scope.parent key v
    | None -> ()
  ) scope.propagate_up
