(* Atomic stores a wrapper of type [(unit -> unit) -> unit]. The
   wrapper takes a thunk and is responsible for invoking it; identity
   default is [fun thunk -> thunk ()]. We store the wrapper rather
   than an option so the hot path is one Atomic.get + one indirect
   call — no branch on Some/None per invocation. *)

let identity_wrapper : (unit -> unit) -> unit = fun thunk -> thunk ()
let _handler : ((unit -> unit) -> unit) Atomic.t = Atomic.make identity_wrapper
let _installed : bool Atomic.t = Atomic.make false

let with_slot (type a) (f : unit -> a) : a =
  let wrapper = Atomic.get _handler in
  (* Cross the unit-returning wrapper boundary using a ref. The wrapper
     never sees the typed result; OCaml's value restriction is fine
     because [result] is a fresh ref per call. Exceptions from [f]
     propagate through the wrapper (wrappers are expected to be
     transparent on exception, like [Fun.protect] or [Eio.Switch]). *)
  let result : a option ref = ref None in
  wrapper (fun () -> result := Some (f ()));
  match !result with
  | Some v -> v
  | None ->
    (* A non-conformant wrapper that swallowed its thunk. Treat as
         programmer error — the contract is "must call thunk exactly
         once". *)
    failwith
      "Fd_throttle_hook: installed handler did not invoke its thunk; wrapper contract \
       violated"
;;

let set_handler h =
  Atomic.set _handler h;
  Atomic.set _installed true
;;

let reset_handler () =
  Atomic.set _handler identity_wrapper;
  Atomic.set _installed false
;;

let is_installed () = Atomic.get _installed
