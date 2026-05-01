(** Episodic memory: interaction history with time-decaying salience.

    Stores structured episode records (who, what, when, outcome)
    with exponential salience decay for relevance ranking.

    @since 0.75.0
    @since 0.92.0 extracted from Memory *)

(* ── Types ──────────────────────────────────────────────── *)

type outcome =
  | Success of string
  | Failure of string
  | Neutral

type episode =
  { id : string
  ; timestamp : float
  ; participants : string list
  ; action : string
  ; outcome : outcome
  ; salience : float
  ; metadata : (string * Yojson.Safe.t) list
  }

(* ── JSON serialization ─────────────────────────────────── *)

let outcome_to_json = function
  | Success s -> `Assoc [ "type", `String "success"; "detail", `String s ]
  | Failure s -> `Assoc [ "type", `String "failure"; "detail", `String s ]
  | Neutral -> `Assoc [ "type", `String "neutral" ]
;;

let outcome_of_json = function
  | `Assoc pairs ->
    (match List.assoc_opt "type" pairs with
     | Some (`String "success") ->
       let detail =
         match List.assoc_opt "detail" pairs with
         | Some (`String s) -> s
         | _ -> ""
       in
       Success detail
     | Some (`String "failure") ->
       let detail =
         match List.assoc_opt "detail" pairs with
         | Some (`String s) -> s
         | _ -> ""
       in
       Failure detail
     | _ -> Neutral)
  | _ -> Neutral
;;

let episode_to_json (ep : episode) : Yojson.Safe.t =
  `Assoc
    [ "id", `String ep.id
    ; "timestamp", `Float ep.timestamp
    ; "participants", Util.json_of_string_list ep.participants
    ; "action", `String ep.action
    ; "outcome", outcome_to_json ep.outcome
    ; "salience", `Float ep.salience
    ; "metadata", `Assoc ep.metadata
    ]
;;

let episode_of_json (json : Yojson.Safe.t) : episode option =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let timestamp = json |> member "timestamp" |> to_float in
    let participants =
      json |> member "participants" |> to_list |> Util.string_list_of_json
    in
    let action = json |> member "action" |> to_string in
    let outcome = outcome_of_json (json |> member "outcome") in
    let salience = json |> member "salience" |> to_float in
    let metadata =
      match json |> member "metadata" with
      | `Assoc pairs -> pairs
      | _ -> []
    in
    Some { id; timestamp; participants; action; outcome; salience; metadata }
  with
  | Yojson.Safe.Util.Type_error _ | Not_found -> None
;;

(* ── Operations ─────────────────────────────────────────── *)

let scope = Context.Custom "ep"
let store ctx (ep : episode) = Context.set_scoped ctx scope ep.id (episode_to_json ep)

let recall_one ctx id =
  match Context.get_scoped ctx scope id with
  | Some json -> episode_of_json json
  | None -> None
;;

let all ctx =
  Context.keys_in_scope ctx scope
  |> List.filter_map (fun key ->
    match Context.get_scoped ctx scope key with
    | Some json -> episode_of_json json
    | None -> None)
;;

let decayed_salience ~now ~decay_rate (ep : episode) =
  let age = now -. ep.timestamp in
  ep.salience *. exp (-.decay_rate *. age)
;;

let recall
      ctx
      ?(now = Unix.gettimeofday ())
      ?(decay_rate = 0.01)
      ?(min_salience = 0.1)
      ?(limit = 50)
      ?filter
      ()
  =
  all ctx
  |> List.map (fun ep ->
    let effective = decayed_salience ~now ~decay_rate ep in
    { ep with salience = effective }, effective)
  |> List.filter (fun (_, s) -> s >= min_salience)
  |> List.filter (fun (ep, _) ->
    match filter with
    | Some predicate -> predicate ep
    | None -> true)
  |> List.sort (fun (_, a) (_, b) -> Float.compare b a)
  |> fun list ->
  let rec take n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | (ep, _) :: rest -> take (n - 1) (ep :: acc) rest
  in
  take limit [] list
;;

let boost_salience ctx id amount =
  match recall_one ctx id with
  | Some ep ->
    let boosted = Float.min 1.0 (ep.salience +. amount) in
    store ctx { ep with salience = boosted }
  | None -> ()
;;

let forget ctx id = Context.delete_scoped ctx scope id
let count ctx = List.length (Context.keys_in_scope ctx scope)
