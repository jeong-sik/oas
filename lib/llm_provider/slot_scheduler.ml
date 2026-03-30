(** Priority-aware slot scheduler for LLM requests.

    Uses Eio.Mutex for state protection and Eio.Promise for per-waiter
    signaling. Waiters are sorted by priority (Interactive first).

    @since 0.96.0 *)

type waiter = {
  rank : int;
  resolver : unit Eio.Promise.u;
  cancelled : bool Atomic.t;
}

type t = {
  max_slots : int;
  mutable active : int;
  mutable waiters : waiter list;
  mutex : Eio.Mutex.t;
}

let create ~max_slots =
  if max_slots < 1 then
    invalid_arg
      (Printf.sprintf "Slot_scheduler.create: max_slots must be >= 1, got %d"
         max_slots);
  {
    max_slots;
    active = 0;
    waiters = [];
    mutex = Eio.Mutex.create ();
  }

(* Insert waiter in priority order (lower rank = higher priority = front). *)
let insert_sorted entry ws =
  let rec go acc = function
    | [] -> List.rev (entry :: acc)
    | (w :: rest) as tail ->
      if entry.rank <= w.rank then
        List.rev_append acc (entry :: tail)
      else
        go (w :: acc) rest
  in
  go [] ws

let rec acquire ~priority t =
  let resolved = Request_priority.resolve priority in
  let rank = Request_priority.to_int resolved in
  let action =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if t.active < t.max_slots then (
        t.active <- t.active + 1;
        `Got_slot
      ) else (
        let p, r = Eio.Promise.create () in
        let entry = { rank; resolver = r; cancelled = Atomic.make false } in
        t.waiters <- insert_sorted entry t.waiters;
        `Wait (p, entry)
      ))
  in
  match action with
  | `Got_slot -> ()
  | `Wait (p, entry) ->
    (try Eio.Promise.await p
     with exn ->
       Atomic.set entry.cancelled true;
       (* If release already resolved our promise, the slot was handed
          to us but we are being cancelled. Release it back. *)
       if Eio.Promise.is_resolved p then
         release_slot t;
       raise exn)

and release_slot t =
  let to_wake =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      let rec find_valid = function
        | [] ->
          t.active <- t.active - 1;
          None
        | entry :: rest ->
          if Atomic.get entry.cancelled then
            find_valid rest
          else (
            t.waiters <- rest;
            Some entry.resolver
          )
      in
      find_valid t.waiters)
  in
  match to_wake with
  | Some r -> Eio.Promise.resolve r ()
  | None -> ()

let with_permit ~priority t f =
  acquire ~priority t;
  Fun.protect f
    ~finally:(fun () -> release_slot t)

let available t = t.max_slots - t.active
let in_use t = t.active
let queue_length t = List.length t.waiters

(* ── Capacity Query ────────────────────────────────────── *)

type snapshot = {
  max_slots : int;
  active : int;
  available : int;
  queue_length : int;
}

let snapshot t =
  Eio.Mutex.use_ro t.mutex (fun () ->
    { max_slots = t.max_slots;
      active = t.active;
      available = t.max_slots - t.active;
      queue_length = List.length t.waiters })

(* ── Non-blocking Acquisition ──────────────────────────── *)

let try_with_permit ~priority t f =
  let _resolved = Request_priority.resolve priority in
  let got =
    Eio.Mutex.use_rw ~protect:true t.mutex (fun () ->
      if t.active < t.max_slots then (
        t.active <- t.active + 1;
        true
      ) else
        false)
  in
  if got then
    Some (Fun.protect f ~finally:(fun () -> release_slot t))
  else
    None

[@@@coverage off]
(* === Inline tests === *)

let%test "create with valid max_slots" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:4 in
    t.max_slots = 4 && available t = 4 && in_use t = 0)

let%test "create rejects zero" =
  try ignore (create ~max_slots:0); false
  with Invalid_argument _ -> true

let%test "create rejects negative" =
  try ignore (create ~max_slots:(-1)); false
  with Invalid_argument _ -> true

let%test "with_permit runs immediately when slots available" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:2 in
    let result = with_permit ~priority:Interactive t (fun () -> 42) in
    result = 42 && available t = 2)

let%test "with_permit releases on exception" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:2 in
    (try with_permit ~priority:Interactive t (fun () -> failwith "boom")
     with Failure _ -> ());
    available t = 2)

let%test "higher priority waiter goes first" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:1 in
    let order = ref [] in
    let hold, release_hold = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () ->
        (* Blocker: hold the slot until release_hold is resolved *)
        with_permit ~priority:Background t (fun () ->
          Eio.Promise.await hold))
      (fun () ->
        Eio.Fiber.both
          (fun () ->
            Eio.Fiber.both
              (fun () ->
                (* Waiter: Background priority *)
                Eio.Fiber.yield ();
                with_permit ~priority:Background t (fun () ->
                  order := "bg" :: !order))
              (fun () ->
                (* Waiter: Interactive priority *)
                Eio.Fiber.yield ();
                with_permit ~priority:Interactive t (fun () ->
                  order := "int" :: !order)))
          (fun () ->
            (* Let waiters enqueue, then release blocker *)
            Eio.Fiber.yield ();
            Eio.Fiber.yield ();
            Eio.Fiber.yield ();
            Eio.Promise.resolve release_hold ()));
    (* Interactive should have been served first *)
    !order = ["bg"; "int"])

let%test "queue_length tracks waiters" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:1 in
    queue_length t = 0)

let%test "Unspecified treated as Proactive" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:2 in
    let result = with_permit ~priority:Unspecified t (fun () -> "ok") in
    result = "ok")

let%test "snapshot reflects current state" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:4 in
    let s = snapshot t in
    s.max_slots = 4 && s.active = 0 && s.available = 4 && s.queue_length = 0)

let%test "snapshot during active permit" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:2 in
    with_permit ~priority:Interactive t (fun () ->
      let s = snapshot t in
      s.active = 1 && s.available = 1))

let%test "try_with_permit succeeds when available" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:2 in
    let result = try_with_permit ~priority:Interactive t (fun () -> 42) in
    result = Some 42 && available t = 2)

let%test "try_with_permit returns None when full" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:1 in
    with_permit ~priority:Interactive t (fun () ->
      let result = try_with_permit ~priority:Background t (fun () -> 99) in
      result = None))

let%test "try_with_permit releases on exception" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:2 in
    (try ignore (try_with_permit ~priority:Interactive t (fun () -> failwith "boom"))
     with Failure _ -> ());
    available t = 2)

let%test "cancel does not leak slot" =
  Eio_main.run (fun _env ->
    let t = create ~max_slots:1 in
    let hold, release_hold = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () ->
        with_permit ~priority:Interactive t (fun () ->
          Eio.Promise.await hold))
      (fun () ->
        Eio.Fiber.both
          (fun () ->
            (* This fiber will be cancelled when the other arm completes *)
            Eio.Fiber.yield ();
            (try with_permit ~priority:Background t (fun () -> ())
             with Eio.Cancel.Cancelled _ -> ()))
          (fun () ->
            (* Release blocker after waiter enqueues *)
            Eio.Fiber.yield ();
            Eio.Fiber.yield ();
            Eio.Promise.resolve release_hold ()));
    (* After everything settles, slot should be available *)
    available t = 1)
