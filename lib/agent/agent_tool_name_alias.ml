(** Lock-free registry for consumer-registered aliases.
    Maps alias -> canonical tool name. Stored as an atomic association list
    so reads and writes are safe from parallel tool-execution fibers without
    requiring an Eio scheduler (the registry may be accessed from tests that
    run outside of a scheduler). *)
let registry : (string * string) list Atomic.t = Atomic.make []

let register_alias ~alias ~canonical =
  let rec loop () =
    let old = Atomic.get registry in
    let new_registry = (alias, canonical) :: List.remove_assoc alias old in
    if Atomic.compare_and_set registry old new_registry then () else loop ()
  in
  loop ()
;;

let resolve_alias alias = List.assoc_opt alias (Atomic.get registry)

let resolve ~requested ~input =
  match resolve_alias requested with
  | Some canonical -> Some (canonical, input)
  | None -> None
;;
