(** Agent selection strategies for swarm orchestration.

    @since 0.60.0 *)

type 'a strategy =
  | RoundRobin
  | Random
  | Custom of ('a list -> 'a option)

let round_robin_counter = ref 0

let () = Random.self_init ()

let select ~(strategy : 'a strategy) ~(candidates : 'a list) : 'a option =
  match candidates with
  | [] -> None
  | _ ->
    match strategy with
    | RoundRobin ->
      let n = List.length candidates in
      let idx = !round_robin_counter mod n in
      round_robin_counter := !round_robin_counter + 1;
      Some (List.nth candidates idx)
    | Random ->
      let n = List.length candidates in
      let idx = Random.int n in
      Some (List.nth candidates idx)
    | Custom f -> f candidates
