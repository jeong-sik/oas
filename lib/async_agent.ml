(** Async Agent — Eio fiber-based background agent execution.

    @since 0.55.0 *)

(* ── Future type ──────────────────────────────────────────────────── *)

type 'a future = {
  promise: ('a, Error.sdk_error) result Eio.Promise.t;
  resolver: ('a, Error.sdk_error) result Eio.Promise.u;
  resolved: bool Atomic.t;
}

(** Resolve the future exactly once. Subsequent calls are no-ops. *)
let resolve_once future result =
  if not (Atomic.exchange future.resolved true) then
    Eio.Promise.resolve future.resolver result

(* ── Agent name extraction ────────────────────────────────────────── *)

let agent_name agent =
  let card = Agent.card agent in
  card.Agent_card.name

(* ── Spawning ─────────────────────────────────────────────────────── *)

let spawn ~sw ?clock agent prompt =
  let promise, resolver = Eio.Promise.create () in
  let resolved = Atomic.make false in
  let future = { promise; resolver; resolved } in
  Eio.Fiber.fork ~sw (fun () ->
    let result = Agent.run ~sw ?clock agent prompt in
    resolve_once future result);
  future

(* ── Awaiting ─────────────────────────────────────────────────────── *)

let await future =
  Eio.Promise.await future.promise

let is_ready future =
  Option.is_some (Eio.Promise.peek future.promise)

(* ── Cancellation ─────────────────────────────────────────────────── *)

let cancel future =
  resolve_once future (Error (Error.Internal "cancelled"))

(* ── Combinators ──────────────────────────────────────────────────── *)

(** Internal exception for propagating sdk_error across fiber boundaries.
    Used only within [race] to convert Result errors into exceptions
    that Eio.Fiber.first can propagate. *)
exception Race_error of Error.sdk_error

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
    (* Build (unit -> 'a) closures for each agent.
       Eio.Fiber.first cancels the loser; recursive nesting
       cancels all remaining when one finishes. *)
    let fns =
      List.map (fun (agent, prompt) ->
        fun () ->
          let name = agent_name agent in
          match Agent.run ~sw ?clock agent prompt with
          | Ok resp -> (name, resp)
          | Error e -> raise (Race_error e))
        agents
    in
    let rec any_of = function
      | [] -> assert false
      | [f] -> f ()
      | f :: rest -> Eio.Fiber.first f (fun () -> any_of rest)
    in
    (try Ok (any_of fns)
     with Race_error e -> Error e)

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
