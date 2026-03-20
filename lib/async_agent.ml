(** Async Agent — Eio fiber-based background agent execution.

    @since 0.55.0 *)

(* ── Internal cancellation exception ──────────────────────────── *)

exception Cancelled

(* ── Future type ──────────────────────────────────────────────── *)

type 'a future = {
  promise: ('a, Error.sdk_error) result Eio.Promise.t;
  resolver: ('a, Error.sdk_error) result Eio.Promise.u;
  resolved: bool Atomic.t;
  mutable cancel_fn: (unit -> unit) option;
}

(** Resolve the future exactly once. Subsequent calls are no-ops. *)
let resolve_once future result =
  if not (Atomic.exchange future.resolved true) then
    Eio.Promise.resolve future.resolver result

(* ── Agent name extraction ────────────────────────────────────── *)

let agent_name agent =
  let card = Agent.card agent in
  card.Agent_card.name

(* ── Spawning ─────────────────────────────────────────────────── *)

let spawn ~sw ?clock agent prompt =
  let promise, resolver = Eio.Promise.create () in
  let resolved = Atomic.make false in
  let future = { promise; resolver; resolved; cancel_fn = None } in
  Eio.Fiber.fork ~sw (fun () ->
    (* Run agent inside a sub-switch so cancel can terminate the fiber.
       Eio.Switch.run creates an independent error domain — failing it
       cancels all I/O (HTTP, DNS, etc.) within Agent.run. *)
    let result =
      try
        Eio.Switch.run (fun sub_sw ->
          future.cancel_fn <- Some (fun () ->
            Eio.Switch.fail sub_sw Cancelled);
          Agent.run ~sw:sub_sw ?clock agent prompt)
      with
      | Cancelled -> Error (Error.Internal "cancelled")
      | Raw_trace.Trace_error e -> Error e
      | Out_of_memory -> raise Out_of_memory
      | Stack_overflow -> raise Stack_overflow
      | Sys.Break -> raise Sys.Break
      | exn -> Error (Error.Internal (Printexc.to_string exn))
    in
    resolve_once future result);
  future

(* ── Awaiting ─────────────────────────────────────────────────── *)

let await future =
  Eio.Promise.await future.promise

let is_ready future =
  Option.is_some (Eio.Promise.peek future.promise)

(* ── Cancellation ─────────────────────────────────────────────── *)

let cancel future =
  resolve_once future (Error (Error.Internal "cancelled"));
  match future.cancel_fn with
  | Some f -> (try f () with Eio.Io _ | Unix.Unix_error _ | Failure _ -> ())
  | None -> ()

(* ── Combinators ──────────────────────────────────────────────── *)

let race ~sw ?clock agents =
  match agents with
  | [] ->
    Error (Error.Internal "race: no agents provided")
  | [(agent, prompt)] ->
    let name = agent_name agent in
    (match Agent.run ~sw ?clock agent prompt with
     | Ok resp -> Ok (name, resp)
     | Error e -> Error e)
  | _ ->
    (* Each closure returns (name, result) — both Ok and Error are
       normal completions. Eio.Fiber.first returns the first fiber to
       finish and cancels the rest via structured concurrency. *)
    let fns =
      List.map (fun (agent, prompt) ->
        fun () ->
          let name = agent_name agent in
          let result =
            try Agent.run ~sw ?clock agent prompt
            with
            | Raw_trace.Trace_error e -> Error e
            | Out_of_memory -> raise Out_of_memory
            | Stack_overflow -> raise Stack_overflow
            | Sys.Break -> raise Sys.Break
            | exn -> Error (Error.Internal (Printexc.to_string exn))
          in
          (name, result))
        agents
    in
    let rec any_of = function
      | [] -> assert false
      | [f] -> f ()
      | f :: rest -> Eio.Fiber.first f (fun () -> any_of rest)
    in
    let (name, result) = any_of fns in
    Result.map (fun resp -> (name, resp)) result

let all ~sw ?clock ?max_fibers agents =
  let run_one (agent, prompt) =
    let name = agent_name agent in
    let result = Agent.run ~sw ?clock agent prompt in
    (name, result)
  in
  match max_fibers with
  | Some n ->
    Eio.Fiber.List.map ~max_fibers:n run_one agents
  | None ->
    Eio.Fiber.List.map run_one agents
