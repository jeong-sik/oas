(** Agent selection strategies for swarm orchestration.

    @since 0.60.0 *)

type 'a strategy =
  | RoundRobin
  | Random
  | Custom of ('a list -> 'a option)

let round_robin_counter = Atomic.make 0

let () = Random.self_init ()

let positive_mod value divisor =
  let remainder = value mod divisor in
  if remainder < 0 then remainder + divisor else remainder

let next_round_robin_index counter ~size =
  Atomic.fetch_and_add counter 1
  |> fun current -> positive_mod current size

let select ~(strategy : 'a strategy) ~(candidates : 'a list) : 'a option =
  match candidates with
  | [] -> None
  | _ ->
    match strategy with
    | RoundRobin ->
      let n = List.length candidates in
      let idx = next_round_robin_index round_robin_counter ~size:n in
      Some (List.nth candidates idx)
    | Random ->
      let n = List.length candidates in
      let idx = Random.int n in
      Some (List.nth candidates idx)
    | Custom f -> f candidates

let%test "next_round_robin_index cycles deterministically" =
  let counter = Atomic.make 0 in
  next_round_robin_index counter ~size:3 = 0
  && next_round_robin_index counter ~size:3 = 1
  && next_round_robin_index counter ~size:3 = 2
  && next_round_robin_index counter ~size:3 = 0

let%test "positive_mod normalizes negative input" =
  positive_mod (-1) 3 = 2
