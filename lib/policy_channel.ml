open Base
(** Policy_channel -- shared ref for lazy tool policy propagation.

    Implementation uses [Atomic.t] for lock-free reads and a CAS loop
    for writes.  Under Eio's cooperative scheduling the CAS loop
    completes in one iteration, but the algorithm is correct even with
    preemptive domain scheduling. *)

type snapshot =
  { op : Tool_op.t option
  ; version : int
  }

type t = snapshot Atomic.t

let create () : t = Atomic.make { op = None; version = 0 }

let push (ch : t) new_op =
  let rec cas () =
    let old = Atomic.get ch in
    let composed =
      match old.op with
      | None -> new_op
      | Some prev -> Tool_op.compose [ prev; new_op ]
    in
    let next = { op = Some composed; version = old.version + 1 } in
    if Atomic.compare_and_set ch old next
    then ()
    else (
      Domain.cpu_relax ();
      cas ())
  in
  cas ()
;;

let poll (ch : t) =
  let snap = Atomic.get ch in
  snap.op
;;

let version (ch : t) =
  let snap = Atomic.get ch in
  snap.version
;;
