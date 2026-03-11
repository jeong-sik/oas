(** Cross-turn shared state for agent execution.
    Inspired by Google ADK's session.state pattern.

    Uses Hashtbl internally -- mutable, safe within a single Eio domain.
    Values are Yojson.Safe.t for flexibility while maintaining serializability. *)

type t = (string, Yojson.Safe.t) Hashtbl.t

let create () : t = Hashtbl.create 16

let get (ctx : t) key =
  Hashtbl.find_opt ctx key

let set (ctx : t) key value =
  Hashtbl.replace ctx key value

let keys (ctx : t) =
  Hashtbl.fold (fun k _ acc -> k :: acc) ctx []

let merge (ctx : t) (pairs : (string * Yojson.Safe.t) list) =
  List.iter (fun (k, v) -> Hashtbl.replace ctx k v) pairs

let to_json (ctx : t) : Yojson.Safe.t =
  let pairs = Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx [] in
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
