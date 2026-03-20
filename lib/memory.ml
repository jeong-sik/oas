(** Working memory: 5-tier facade over {!Context.t}.

    Tiers map to {!Context.scope}:
    - Scratchpad → Temp
    - Working → Session
    - Episodic → Custom "ep"
    - Procedural → Custom "pr"
    - Long_term → Custom "lt"

    Episodic and Procedural store typed records as JSON.

    @since 0.65.0 (3-tier)
    @since 0.75.0 (5-tier: Episodic + Procedural) *)

type tier =
  | Scratchpad
  | Working
  | Episodic
  | Procedural
  | Long_term

type long_term_backend = {
  persist: key:string -> Yojson.Safe.t -> (unit, string) result;
  retrieve: key:string -> Yojson.Safe.t option;
  remove: key:string -> (unit, string) result;
  batch_persist: (string * Yojson.Safe.t) list -> (unit, string) result;
  query: prefix:string -> limit:int -> (string * Yojson.Safe.t) list;
}

let legacy_backend ~persist ~retrieve ~remove =
  {
    persist = (fun ~key value -> persist ~key value; Ok ());
    retrieve;
    remove = (fun ~key -> remove ~key; Ok ());
    batch_persist = (fun pairs ->
      List.iter (fun (k, v) -> persist ~key:k v) pairs; Ok ());
    query = (fun ~prefix:_ ~limit:_ -> []);
  }

type t = {
  ctx: Context.t;
  mutable long_term: long_term_backend option;
}

let scope_of_tier = function
  | Scratchpad -> Context.Temp
  | Working -> Context.Session
  | Episodic -> Context.Custom "ep"
  | Procedural -> Context.Custom "pr"
  | Long_term -> Context.Custom "lt"

let create ?(ctx = Context.create ()) ?long_term () =
  { ctx; long_term }

let set_long_term_backend t backend =
  t.long_term <- Some backend

let store t ~tier key value =
  match tier with
  | Long_term ->
    Context.set_scoped t.ctx (scope_of_tier Long_term) key value;
    (match t.long_term with
     | Some backend ->
       (match backend.persist ~key value with
        | Ok () -> ()
        | Error reason -> failwith (Printf.sprintf "long_term persist failed: %s" reason))
     | None -> ())
  | _ ->
    Context.set_scoped t.ctx (scope_of_tier tier) key value

let recall t ~tier key =
  match Context.get_scoped t.ctx (scope_of_tier tier) key with
  | Some _ as found -> found
  | None ->
    match tier with
    | Scratchpad ->
      (match Context.get_scoped t.ctx (scope_of_tier Working) key with
       | Some _ as found -> found
       | None ->
         match t.long_term with
         | Some backend -> backend.retrieve ~key
         | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
    | Working ->
      (match t.long_term with
       | Some backend -> backend.retrieve ~key
       | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
    | Long_term ->
      (match t.long_term with
       | Some backend -> backend.retrieve ~key
       | None -> None)
    | Episodic | Procedural ->
      None

let recall_exact t ~tier key =
  match tier with
  | Long_term ->
    (match t.long_term with
     | Some backend -> backend.retrieve ~key
     | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
  | _ ->
    Context.get_scoped t.ctx (scope_of_tier tier) key

let forget t ~tier key =
  match tier with
  | Long_term ->
    Context.delete_scoped t.ctx (scope_of_tier Long_term) key;
    (match t.long_term with
     | Some backend ->
       (match backend.remove ~key with
        | Ok () -> ()
        | Error reason -> failwith (Printf.sprintf "long_term remove failed: %s" reason))
     | None -> ())
  | _ ->
    Context.delete_scoped t.ctx (scope_of_tier tier) key

let promote t key =
  match Context.get_scoped t.ctx (scope_of_tier Scratchpad) key with
  | Some value ->
    Context.set_scoped t.ctx (scope_of_tier Working) key value;
    Context.delete_scoped t.ctx (scope_of_tier Scratchpad) key;
    true
  | None -> false

let working_entries t =
  Context.keys_in_scope t.ctx (scope_of_tier Working)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Working) key with
    | Some v -> Some (key, v)
    | None -> None)

let scratchpad_entries t =
  Context.keys_in_scope t.ctx (scope_of_tier Scratchpad)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Scratchpad) key with
    | Some v -> Some (key, v)
    | None -> None)

let clear_scratchpad t =
  let keys = Context.keys_in_scope t.ctx (scope_of_tier Scratchpad) in
  List.iter (fun key ->
    Context.delete_scoped t.ctx (scope_of_tier Scratchpad) key
  ) keys

let keys_in_tier t tier =
  Context.keys_in_scope t.ctx (scope_of_tier tier)

let stats t =
  let count tier = List.length (keys_in_tier t tier) in
  (count Scratchpad, count Working, count Episodic, count Procedural, count Long_term)

let context t = t.ctx

(* ── Episodic memory ─────────────────────────────────────── *)

type outcome =
  | Success of string
  | Failure of string
  | Neutral

type episode = {
  id: string;
  timestamp: float;
  participants: string list;
  action: string;
  outcome: outcome;
  salience: float;
  metadata: (string * Yojson.Safe.t) list;
}

let outcome_to_json = function
  | Success s -> `Assoc [("type", `String "success"); ("detail", `String s)]
  | Failure s -> `Assoc [("type", `String "failure"); ("detail", `String s)]
  | Neutral -> `Assoc [("type", `String "neutral")]

let outcome_of_json = function
  | `Assoc pairs ->
    (match List.assoc_opt "type" pairs with
     | Some (`String "success") ->
       let detail = match List.assoc_opt "detail" pairs with
         | Some (`String s) -> s | _ -> "" in
       Success detail
     | Some (`String "failure") ->
       let detail = match List.assoc_opt "detail" pairs with
         | Some (`String s) -> s | _ -> "" in
       Failure detail
     | _ -> Neutral)
  | _ -> Neutral

let episode_to_json (ep : episode) : Yojson.Safe.t =
  `Assoc [
    ("id", `String ep.id);
    ("timestamp", `Float ep.timestamp);
    ("participants", `List (List.map (fun s -> `String s) ep.participants));
    ("action", `String ep.action);
    ("outcome", outcome_to_json ep.outcome);
    ("salience", `Float ep.salience);
    ("metadata", `Assoc ep.metadata);
  ]

let episode_of_json (json : Yojson.Safe.t) : episode option =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let timestamp = json |> member "timestamp" |> to_float in
    let participants = json |> member "participants" |> to_list
      |> List.filter_map (function `String s -> Some s | _ -> None) in
    let action = json |> member "action" |> to_string in
    let outcome = outcome_of_json (json |> member "outcome") in
    let salience = json |> member "salience" |> to_float in
    let metadata = match json |> member "metadata" with
      | `Assoc pairs -> pairs | _ -> [] in
    Some { id; timestamp; participants; action; outcome; salience; metadata }
  with _ -> None

let store_episode t (ep : episode) =
  Context.set_scoped t.ctx (scope_of_tier Episodic) ep.id (episode_to_json ep)

let recall_episode t id =
  match Context.get_scoped t.ctx (scope_of_tier Episodic) id with
  | Some json -> episode_of_json json
  | None -> None

let all_episodes t =
  keys_in_tier t Episodic
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Episodic) key with
    | Some json -> episode_of_json json
    | None -> None)

let decayed_salience ~now ~decay_rate (ep : episode) =
  let age = now -. ep.timestamp in
  ep.salience *. exp (-. decay_rate *. age)

let recall_episodes t ?(now = Unix.gettimeofday ()) ?(decay_rate = 0.01)
    ?(min_salience = 0.1) ?(limit = 50) ?filter () =
  all_episodes t
  |> List.map (fun ep ->
    let effective = decayed_salience ~now ~decay_rate ep in
    ({ ep with salience = effective }, effective))
  |> List.filter (fun (_, s) -> s >= min_salience)
  |> List.filter (fun (ep, _) ->
    match filter with
    | Some predicate -> predicate ep
    | None -> true)
  |> List.sort (fun (_, a) (_, b) -> Float.compare b a)
  |> (fun list ->
    let rec take n acc = function
      | [] -> List.rev acc
      | _ when n <= 0 -> List.rev acc
      | (ep, _) :: rest -> take (n - 1) (ep :: acc) rest
    in
    take limit [] list)

let boost_salience t id amount =
  match recall_episode t id with
  | Some ep ->
    let boosted = Float.min 1.0 (ep.salience +. amount) in
    store_episode t { ep with salience = boosted }
  | None -> ()

let forget_episode t id =
  Context.delete_scoped t.ctx (scope_of_tier Episodic) id

let episode_count t =
  List.length (keys_in_tier t Episodic)

(* ── Procedural memory ──────────────────────────────────── *)

type procedure = {
  id: string;
  pattern: string;
  action: string;
  success_count: int;
  failure_count: int;
  confidence: float;
  last_used: float;
  metadata: (string * Yojson.Safe.t) list;
}

let compute_confidence ~success_count ~failure_count =
  let total = success_count + failure_count in
  if total = 0 then 0.0
  else Float.of_int success_count /. Float.of_int total

let procedure_to_json (proc : procedure) : Yojson.Safe.t =
  `Assoc [
    ("id", `String proc.id);
    ("pattern", `String proc.pattern);
    ("action", `String proc.action);
    ("success_count", `Int proc.success_count);
    ("failure_count", `Int proc.failure_count);
    ("confidence", `Float proc.confidence);
    ("last_used", `Float proc.last_used);
    ("metadata", `Assoc proc.metadata);
  ]

let procedure_of_json (json : Yojson.Safe.t) : procedure option =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let pattern = json |> member "pattern" |> to_string in
    let action = json |> member "action" |> to_string in
    let success_count = json |> member "success_count" |> to_int in
    let failure_count = json |> member "failure_count" |> to_int in
    let confidence = json |> member "confidence" |> to_float in
    let last_used = json |> member "last_used" |> to_float in
    let metadata = match json |> member "metadata" with
      | `Assoc pairs -> pairs | _ -> [] in
    Some { id; pattern; action; success_count; failure_count;
           confidence; last_used; metadata }
  with _ -> None

let store_procedure t (proc : procedure) =
  Context.set_scoped t.ctx (scope_of_tier Procedural) proc.id (procedure_to_json proc)

let all_procedures t =
  keys_in_tier t Procedural
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Procedural) key with
    | Some json -> procedure_of_json json
    | None -> None)

let string_contains ~needle haystack =
  let nlen = String.length needle in
  let hlen = String.length haystack in
  if nlen = 0 then true
  else
    let rec loop i =
      if i + nlen > hlen then false
      else if String.sub haystack i nlen = needle then true
      else loop (i + 1)
    in
    loop 0

let matching_procedures t ~pattern ?(min_confidence = 0.0) ?filter () =
  all_procedures t
  |> List.filter (fun proc ->
    string_contains ~needle:pattern proc.pattern
    && proc.confidence >= min_confidence
    &&
    match filter with
    | Some predicate -> predicate proc
    | None -> true)
  |> List.sort (fun a b -> Float.compare b.confidence a.confidence)

let find_procedure t ~pattern ?(min_confidence = 0.0) ?filter ?(touch = false) () =
  match matching_procedures t ~pattern ~min_confidence ?filter () with
  | best :: _ ->
    if touch then begin
      let touched = { best with last_used = Unix.gettimeofday () } in
      store_procedure t touched;
      Some touched
    end else Some best
  | [] -> None

let best_procedure t ~pattern =
  find_procedure t ~pattern ()

let update_procedure t id f =
  match Context.get_scoped t.ctx (scope_of_tier Procedural) id with
  | Some json ->
    (match procedure_of_json json with
     | Some proc ->
       let updated = f proc in
       store_procedure t updated
     | None -> ())
  | None -> ()

let record_success t id =
  update_procedure t id (fun proc ->
    let success_count = proc.success_count + 1 in
    let confidence = compute_confidence ~success_count ~failure_count:proc.failure_count in
    { proc with success_count; confidence; last_used = Unix.gettimeofday () })

let record_failure t id =
  update_procedure t id (fun proc ->
    let failure_count = proc.failure_count + 1 in
    let confidence = compute_confidence ~success_count:proc.success_count ~failure_count in
    { proc with failure_count; confidence; last_used = Unix.gettimeofday () })

let forget_procedure t id =
  Context.delete_scoped t.ctx (scope_of_tier Procedural) id

let procedure_count t =
  List.length (keys_in_tier t Procedural)
