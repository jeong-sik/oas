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
  with Yojson.Safe.Util.Type_error _ | Not_found -> None

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
  with Yojson.Safe.Util.Type_error _ | Not_found -> None

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

[@@@coverage off]
(* === Inline tests === *)

(* --- scope_of_tier --- *)

let%test "scope_of_tier Scratchpad" =
  scope_of_tier Scratchpad = Context.Temp

let%test "scope_of_tier Working" =
  scope_of_tier Working = Context.Session

let%test "scope_of_tier Episodic" =
  scope_of_tier Episodic = Context.Custom "ep"

let%test "scope_of_tier Procedural" =
  scope_of_tier Procedural = Context.Custom "pr"

let%test "scope_of_tier Long_term" =
  scope_of_tier Long_term = Context.Custom "lt"

(* --- create / store / recall --- *)

let%test "create default memory" =
  let mem = create () in
  stats mem = (0, 0, 0, 0, 0)

let%test "store and recall Working" =
  let mem = create () in
  store mem ~tier:Working "k1" (`String "v1");
  recall mem ~tier:Working "k1" = Some (`String "v1")

let%test "store and recall Scratchpad" =
  let mem = create () in
  store mem ~tier:Scratchpad "sp" (`Int 42);
  recall mem ~tier:Scratchpad "sp" = Some (`Int 42)

let%test "recall Working falls through to Long_term" =
  let mem = create () in
  store mem ~tier:Long_term "lt_key" (`String "lt_val");
  recall mem ~tier:Working "lt_key" = Some (`String "lt_val")

let%test "recall Scratchpad falls through to Working" =
  let mem = create () in
  store mem ~tier:Working "wk" (`String "from_working");
  recall mem ~tier:Scratchpad "wk" = Some (`String "from_working")

let%test "recall Scratchpad falls through to Long_term" =
  let mem = create () in
  store mem ~tier:Long_term "deep" (`Bool true);
  recall mem ~tier:Scratchpad "deep" = Some (`Bool true)

let%test "recall Episodic no fallback" =
  let mem = create () in
  store mem ~tier:Working "wk" (`String "val");
  recall mem ~tier:Episodic "wk" = None

let%test "recall Procedural no fallback" =
  let mem = create () in
  store mem ~tier:Working "wk" (`String "val");
  recall mem ~tier:Procedural "wk" = None

let%test "recall missing key" =
  let mem = create () in
  recall mem ~tier:Working "nonexistent" = None

(* --- recall_exact --- *)

let%test "recall_exact no fallback" =
  let mem = create () in
  store mem ~tier:Scratchpad "sp" (`Int 1);
  store mem ~tier:Working "sp" (`Int 2);
  recall_exact mem ~tier:Scratchpad "sp" = Some (`Int 1)

let%test "recall_exact Working does not fall through" =
  let mem = create () in
  store mem ~tier:Long_term "lt" (`String "val");
  recall_exact mem ~tier:Working "lt" = None

let%test "recall_exact Long_term" =
  let mem = create () in
  store mem ~tier:Long_term "lt" (`Bool true);
  recall_exact mem ~tier:Long_term "lt" = Some (`Bool true)

(* --- forget --- *)

let%test "forget removes key" =
  let mem = create () in
  store mem ~tier:Working "k" (`Int 1);
  forget mem ~tier:Working "k";
  recall_exact mem ~tier:Working "k" = None

let%test "forget Long_term without backend" =
  let mem = create () in
  store mem ~tier:Long_term "k" (`Int 1);
  forget mem ~tier:Long_term "k";
  recall_exact mem ~tier:Long_term "k" = None

(* --- promote --- *)

let%test "promote moves from Scratchpad to Working" =
  let mem = create () in
  store mem ~tier:Scratchpad "item" (`String "data");
  let result = promote mem "item" in
  result = true
  && recall_exact mem ~tier:Working "item" = Some (`String "data")
  && recall_exact mem ~tier:Scratchpad "item" = None

let%test "promote missing key returns false" =
  let mem = create () in
  promote mem "nonexistent" = false

(* --- entries / clear / keys --- *)

let%test "working_entries returns stored items" =
  let mem = create () in
  store mem ~tier:Working "a" (`Int 1);
  store mem ~tier:Working "b" (`Int 2);
  List.length (working_entries mem) = 2

let%test "scratchpad_entries returns stored items" =
  let mem = create () in
  store mem ~tier:Scratchpad "x" (`Bool true);
  List.length (scratchpad_entries mem) = 1

let%test "clear_scratchpad removes all" =
  let mem = create () in
  store mem ~tier:Scratchpad "a" (`Int 1);
  store mem ~tier:Scratchpad "b" (`Int 2);
  clear_scratchpad mem;
  scratchpad_entries mem = []

let%test "keys_in_tier" =
  let mem = create () in
  store mem ~tier:Working "k1" (`Null);
  store mem ~tier:Working "k2" (`Null);
  List.length (keys_in_tier mem Working) = 2

(* --- stats --- *)

let%test "stats counts tiers" =
  let mem = create () in
  store mem ~tier:Scratchpad "s" (`Null);
  store mem ~tier:Working "w1" (`Null);
  store mem ~tier:Working "w2" (`Null);
  let (sp, wk, ep, pr, lt) = stats mem in
  sp = 1 && wk = 2 && ep = 0 && pr = 0 && lt = 0

(* --- outcome JSON roundtrip --- *)

let%test "outcome_to_json success" =
  match outcome_to_json (Success "done") with
  | `Assoc pairs ->
    List.assoc "type" pairs = `String "success"
    && List.assoc "detail" pairs = `String "done"
  | _ -> false

let%test "outcome_to_json failure" =
  match outcome_to_json (Failure "bad") with
  | `Assoc pairs ->
    List.assoc "type" pairs = `String "failure"
  | _ -> false

let%test "outcome_to_json neutral" =
  match outcome_to_json Neutral with
  | `Assoc pairs ->
    List.assoc "type" pairs = `String "neutral"
  | _ -> false

let%test "outcome_of_json success" =
  let json = `Assoc [("type", `String "success"); ("detail", `String "ok")] in
  outcome_of_json json = Success "ok"

let%test "outcome_of_json failure" =
  let json = `Assoc [("type", `String "failure"); ("detail", `String "err")] in
  outcome_of_json json = Failure "err"

let%test "outcome_of_json neutral" =
  let json = `Assoc [("type", `String "neutral")] in
  outcome_of_json json = Neutral

let%test "outcome_of_json unknown type defaults to Neutral" =
  let json = `Assoc [("type", `String "other")] in
  outcome_of_json json = Neutral

let%test "outcome_of_json non-assoc defaults to Neutral" =
  outcome_of_json (`String "bad") = Neutral

let%test "outcome_of_json success missing detail" =
  let json = `Assoc [("type", `String "success")] in
  outcome_of_json json = Success ""

(* --- episode roundtrip --- *)

let%test "episode_to_json and episode_of_json roundtrip" =
  let ep = {
    id = "e1"; timestamp = 100.0;
    participants = ["alice"; "bob"];
    action = "deployed";
    outcome = Success "ok";
    salience = 0.8;
    metadata = [("env", `String "prod")];
  } in
  let json = episode_to_json ep in
  match episode_of_json json with
  | Some ep2 ->
    ep2.id = "e1"
    && ep2.action = "deployed"
    && ep2.salience = 0.8
    && List.length ep2.participants = 2
  | None -> false

let%test "episode_of_json invalid json" =
  episode_of_json (`String "bad") = None

(* --- episodic memory operations --- *)

let%test "store_episode and recall_episode" =
  let mem = create () in
  let ep = {
    id = "ep1"; timestamp = 100.0;
    participants = []; action = "test";
    outcome = Neutral; salience = 0.5;
    metadata = [];
  } in
  store_episode mem ep;
  match recall_episode mem "ep1" with
  | Some ep2 -> ep2.id = "ep1"
  | None -> false

let%test "recall_episode missing" =
  let mem = create () in
  recall_episode mem "nope" = None

let%test "all_episodes returns stored" =
  let mem = create () in
  let ep1 = { id = "e1"; timestamp = 100.0; participants = [];
              action = "a1"; outcome = Neutral; salience = 0.5; metadata = [] } in
  let ep2 = { id = "e2"; timestamp = 200.0; participants = [];
              action = "a2"; outcome = Neutral; salience = 0.7; metadata = [] } in
  store_episode mem ep1;
  store_episode mem ep2;
  List.length (all_episodes mem) = 2

let%test "forget_episode removes" =
  let mem = create () in
  let ep = { id = "e1"; timestamp = 100.0; participants = [];
             action = "a"; outcome = Neutral; salience = 0.5; metadata = [] } in
  store_episode mem ep;
  forget_episode mem "e1";
  recall_episode mem "e1" = None

let%test "episode_count" =
  let mem = create () in
  let ep = { id = "e1"; timestamp = 100.0; participants = [];
             action = "a"; outcome = Neutral; salience = 0.5; metadata = [] } in
  store_episode mem ep;
  episode_count mem = 1

(* --- decayed_salience --- *)

let%test "decayed_salience no age returns same" =
  let ep = { id = "e"; timestamp = 100.0; participants = [];
             action = "a"; outcome = Neutral; salience = 0.8; metadata = [] } in
  let result = decayed_salience ~now:100.0 ~decay_rate:0.01 ep in
  Float.abs (result -. 0.8) < 0.001

let%test "decayed_salience with age reduces" =
  let ep = { id = "e"; timestamp = 0.0; participants = [];
             action = "a"; outcome = Neutral; salience = 1.0; metadata = [] } in
  let result = decayed_salience ~now:100.0 ~decay_rate:0.01 ep in
  result < 1.0 && result > 0.0

(* --- recall_episodes --- *)

let%test "recall_episodes filters by salience" =
  let mem = create () in
  let ep = { id = "e1"; timestamp = 0.0; participants = [];
             action = "old"; outcome = Neutral; salience = 0.001; metadata = [] } in
  store_episode mem ep;
  let results = recall_episodes mem ~now:10000.0 ~min_salience:0.1 () in
  results = []

let%test "recall_episodes respects limit" =
  let mem = create () in
  for i = 1 to 10 do
    let ep = { id = Printf.sprintf "e%d" i;
               timestamp = Unix.gettimeofday ();
               participants = []; action = "a";
               outcome = Neutral; salience = 0.9; metadata = [] } in
    store_episode mem ep
  done;
  let results = recall_episodes mem ~limit:3 () in
  List.length results <= 3

let%test "recall_episodes with filter" =
  let mem = create () in
  let now = Unix.gettimeofday () in
  let ep1 = { id = "e1"; timestamp = now; participants = ["alice"];
              action = "deploy"; outcome = Neutral; salience = 0.9; metadata = [] } in
  let ep2 = { id = "e2"; timestamp = now; participants = ["bob"];
              action = "review"; outcome = Neutral; salience = 0.9; metadata = [] } in
  store_episode mem ep1;
  store_episode mem ep2;
  let results = recall_episodes mem ~filter:(fun ep -> List.mem "alice" ep.participants) () in
  List.length results = 1

(* --- boost_salience --- *)

let%test "boost_salience increases" =
  let mem = create () in
  let ep = { id = "e1"; timestamp = Unix.gettimeofday (); participants = [];
             action = "a"; outcome = Neutral; salience = 0.5; metadata = [] } in
  store_episode mem ep;
  boost_salience mem "e1" 0.3;
  match recall_episode mem "e1" with
  | Some ep2 -> ep2.salience = 0.8
  | None -> false

let%test "boost_salience caps at 1.0" =
  let mem = create () in
  let ep = { id = "e1"; timestamp = Unix.gettimeofday (); participants = [];
             action = "a"; outcome = Neutral; salience = 0.9; metadata = [] } in
  store_episode mem ep;
  boost_salience mem "e1" 0.5;
  match recall_episode mem "e1" with
  | Some ep2 -> ep2.salience = 1.0
  | None -> false

let%test "boost_salience missing id no-op" =
  let mem = create () in
  boost_salience mem "nonexistent" 0.5;
  true

(* --- procedural memory --- *)

let%test "compute_confidence zero total" =
  compute_confidence ~success_count:0 ~failure_count:0 = 0.0

let%test "compute_confidence all success" =
  compute_confidence ~success_count:10 ~failure_count:0 = 1.0

let%test "compute_confidence mixed" =
  let c = compute_confidence ~success_count:3 ~failure_count:1 in
  Float.abs (c -. 0.75) < 0.001

let%test "procedure_to_json and procedure_of_json roundtrip" =
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make all";
    success_count = 5; failure_count = 2;
    confidence = 0.71; last_used = 1000.0;
    metadata = [];
  } in
  let json = procedure_to_json proc in
  match procedure_of_json json with
  | Some p2 ->
    p2.id = "p1" && p2.pattern = "build"
    && p2.success_count = 5 && p2.failure_count = 2
  | None -> false

let%test "procedure_of_json invalid" =
  procedure_of_json (`String "bad") = None

let%test "store_procedure and all_procedures" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "deploy"; action = "run deploy.sh";
    success_count = 1; failure_count = 0;
    confidence = 1.0; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  List.length (all_procedures mem) = 1

let%test "string_contains empty needle" =
  string_contains ~needle:"" "anything" = true

let%test "string_contains found" =
  string_contains ~needle:"world" "hello world" = true

let%test "string_contains not found" =
  string_contains ~needle:"xyz" "hello" = false

let%test "string_contains same string" =
  string_contains ~needle:"abc" "abc" = true

let%test "string_contains needle longer than haystack" =
  string_contains ~needle:"longer" "abc" = false

let%test "matching_procedures finds match" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build ocaml"; action = "dune build";
    success_count = 5; failure_count = 0;
    confidence = 1.0; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  let results = matching_procedures mem ~pattern:"build" () in
  List.length results = 1

let%test "matching_procedures min_confidence filter" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make";
    success_count = 1; failure_count = 9;
    confidence = 0.1; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  let results = matching_procedures mem ~pattern:"build" ~min_confidence:0.5 () in
  results = []

let%test "find_procedure returns best" =
  let mem = create () in
  let proc1 : procedure = {
    id = "p1"; pattern = "deploy"; action = "slow deploy";
    success_count = 1; failure_count = 1;
    confidence = 0.5; last_used = 100.0; metadata = [];
  } in
  let proc2 : procedure = {
    id = "p2"; pattern = "deploy fast"; action = "fast deploy";
    success_count = 9; failure_count = 1;
    confidence = 0.9; last_used = 200.0; metadata = [];
  } in
  store_procedure mem proc1;
  store_procedure mem proc2;
  match find_procedure mem ~pattern:"deploy" () with
  | Some p -> p.id = "p2"
  | None -> false

let%test "find_procedure no match" =
  let mem = create () in
  find_procedure mem ~pattern:"nonexistent" () = None

let%test "find_procedure with touch updates last_used" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make";
    success_count = 5; failure_count = 0;
    confidence = 1.0; last_used = 0.0; metadata = [];
  } in
  store_procedure mem proc;
  match find_procedure mem ~pattern:"build" ~touch:true () with
  | Some p -> p.last_used > 0.0
  | None -> false

let%test "best_procedure delegates to find_procedure" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "test"; action = "run tests";
    success_count = 3; failure_count = 0;
    confidence = 1.0; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  match best_procedure mem ~pattern:"test" with
  | Some p -> p.id = "p1"
  | None -> false

let%test "record_success increments" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make";
    success_count = 2; failure_count = 1;
    confidence = 0.67; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  record_success mem "p1";
  match find_procedure mem ~pattern:"build" () with
  | Some p -> p.success_count = 3
  | None -> false

let%test "record_failure increments" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make";
    success_count = 2; failure_count = 1;
    confidence = 0.67; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  record_failure mem "p1";
  match find_procedure mem ~pattern:"build" () with
  | Some p -> p.failure_count = 2
  | None -> false

let%test "forget_procedure removes" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make";
    success_count = 1; failure_count = 0;
    confidence = 1.0; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  forget_procedure mem "p1";
  procedure_count mem = 0

let%test "procedure_count" =
  let mem = create () in
  let proc : procedure = {
    id = "p1"; pattern = "build"; action = "make";
    success_count = 1; failure_count = 0;
    confidence = 1.0; last_used = 100.0; metadata = [];
  } in
  store_procedure mem proc;
  procedure_count mem = 1

let%test "update_procedure missing id no-op" =
  let mem = create () in
  update_procedure mem "nonexistent" (fun p -> p);
  true

(* --- legacy_backend --- *)

let%test "legacy_backend persist and retrieve" =
  let tbl = Hashtbl.create 4 in
  let backend = legacy_backend
    ~persist:(fun ~key value -> Hashtbl.replace tbl key value)
    ~retrieve:(fun ~key -> Hashtbl.find_opt tbl key)
    ~remove:(fun ~key -> Hashtbl.remove tbl key) in
  (match backend.persist ~key:"k" (`String "v") with
   | Ok () -> backend.retrieve ~key:"k" = Some (`String "v")
   | Error _ -> false)

let%test "legacy_backend remove" =
  let tbl = Hashtbl.create 4 in
  let backend = legacy_backend
    ~persist:(fun ~key value -> Hashtbl.replace tbl key value)
    ~retrieve:(fun ~key -> Hashtbl.find_opt tbl key)
    ~remove:(fun ~key -> Hashtbl.remove tbl key) in
  ignore (backend.persist ~key:"k" (`Null));
  ignore (backend.remove ~key:"k");
  backend.retrieve ~key:"k" = None

let%test "legacy_backend batch_persist" =
  let tbl = Hashtbl.create 4 in
  let backend = legacy_backend
    ~persist:(fun ~key value -> Hashtbl.replace tbl key value)
    ~retrieve:(fun ~key -> Hashtbl.find_opt tbl key)
    ~remove:(fun ~key -> Hashtbl.remove tbl key) in
  (match backend.batch_persist [("a", `Int 1); ("b", `Int 2)] with
   | Ok () ->
     Hashtbl.find_opt tbl "a" = Some (`Int 1)
     && Hashtbl.find_opt tbl "b" = Some (`Int 2)
   | Error _ -> false)

let%test "legacy_backend query always empty" =
  let backend = legacy_backend
    ~persist:(fun ~key:_ _v -> ())
    ~retrieve:(fun ~key:_ -> None)
    ~remove:(fun ~key:_ -> ()) in
  backend.query ~prefix:"" ~limit:10 = []

(* --- long_term with backend --- *)

let%test "store Long_term with backend persists" =
  let tbl = Hashtbl.create 4 in
  let backend = legacy_backend
    ~persist:(fun ~key value -> Hashtbl.replace tbl key value)
    ~retrieve:(fun ~key -> Hashtbl.find_opt tbl key)
    ~remove:(fun ~key -> Hashtbl.remove tbl key) in
  let mem = create () in
  set_long_term_backend mem backend;
  store mem ~tier:Long_term "k" (`String "v");
  Hashtbl.find_opt tbl "k" = Some (`String "v")

let%test "recall Long_term with backend retrieves" =
  let tbl = Hashtbl.create 4 in
  Hashtbl.replace tbl "bk" (`Int 99);
  let backend = legacy_backend
    ~persist:(fun ~key value -> Hashtbl.replace tbl key value)
    ~retrieve:(fun ~key -> Hashtbl.find_opt tbl key)
    ~remove:(fun ~key -> Hashtbl.remove tbl key) in
  let mem = create () in
  set_long_term_backend mem backend;
  recall mem ~tier:Long_term "bk" = Some (`Int 99)

let%test "recall_exact Long_term with backend" =
  let tbl = Hashtbl.create 4 in
  Hashtbl.replace tbl "bk" (`Bool true);
  let backend = legacy_backend
    ~persist:(fun ~key value -> Hashtbl.replace tbl key value)
    ~retrieve:(fun ~key -> Hashtbl.find_opt tbl key)
    ~remove:(fun ~key -> Hashtbl.remove tbl key) in
  let mem = create () in
  set_long_term_backend mem backend;
  recall_exact mem ~tier:Long_term "bk" = Some (`Bool true)

let%test "context accessor" =
  let mem = create () in
  let _ctx = context mem in
  true
