open Base
(** Working memory: 5-tier facade over {!Context.t}.

    Tiers map to {!Context.scope}:
    - Scratchpad -> Temp
    - Working -> Session
    - Episodic -> Custom "ep"
    - Procedural -> Custom "pr"
    - Long_term -> Custom "lt"

    Episodic and Procedural store typed records as JSON.

    @since 0.65.0 (3-tier)
    @since 0.75.0 (5-tier: Episodic + Procedural)
    @since 0.92.0 decomposed into Memory_episodic, Memory_procedural *)

type tier =
  | Scratchpad
  | Working
  | Episodic
  | Procedural
  | Long_term

type long_term_backend =
  { persist : key:string -> Yojson.Safe.t -> (unit, string) result
  ; retrieve : key:string -> Yojson.Safe.t option
  ; remove : key:string -> (unit, string) result
  ; batch_persist : (string * Yojson.Safe.t) list -> (unit, string) result
  ; query : prefix:string -> limit:int -> (string * Yojson.Safe.t) list
  }

let legacy_backend ~persist ~retrieve ~remove =
  { persist =
      (fun ~key value ->
        persist ~key value;
        Ok ())
  ; retrieve
  ; remove =
      (fun ~key ->
        remove ~key;
        Ok ())
  ; batch_persist =
      (fun pairs ->
        List.iter (fun (k, v) -> persist ~key:k v) pairs;
        Ok ())
  ; query = (fun ~prefix:_ ~limit:_ -> [])
  }
;;

type t =
  { ctx : Context.t
  ; mutable long_term : long_term_backend option
  }

let scope_of_tier = function
  | Scratchpad -> Context.Temp
  | Working -> Context.Session
  | Episodic -> Context.Custom "ep"
  | Procedural -> Context.Custom "pr"
  | Long_term -> Context.Custom "lt"
;;

let create ?(ctx = Context.create ()) ?long_term () = { ctx; long_term }
let set_long_term_backend t backend = t.long_term <- Some backend

let store t ~tier key value =
  match tier with
  | Long_term ->
    Context.set_scoped t.ctx (scope_of_tier Long_term) key value;
    (match t.long_term with
     | Some backend ->
       (match backend.persist ~key value with
        | Ok () -> Ok ()
        | Error reason -> Error reason)
     | None -> Ok ())
  | _ ->
    Context.set_scoped t.ctx (scope_of_tier tier) key value;
    Ok ()
;;

let recall t ~tier key =
  match Context.get_scoped t.ctx (scope_of_tier tier) key with
  | Some _ as found -> found
  | None ->
    (match tier with
     | Scratchpad ->
       (match Context.get_scoped t.ctx (scope_of_tier Working) key with
        | Some _ as found -> found
        | None ->
          (match t.long_term with
           | Some backend -> backend.retrieve ~key
           | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key))
     | Working ->
       (match t.long_term with
        | Some backend -> backend.retrieve ~key
        | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
     | Long_term ->
       (match t.long_term with
        | Some backend -> backend.retrieve ~key
        | None -> None)
     | Episodic | Procedural -> None)
;;

let recall_exact t ~tier key =
  match tier with
  | Long_term ->
    (match t.long_term with
     | Some backend -> backend.retrieve ~key
     | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
  | _ -> Context.get_scoped t.ctx (scope_of_tier tier) key
;;

let forget t ~tier key =
  match tier with
  | Long_term ->
    Context.delete_scoped t.ctx (scope_of_tier Long_term) key;
    (match t.long_term with
     | Some backend ->
       (match backend.remove ~key with
        | Ok () -> Ok ()
        | Error reason -> Error reason)
     | None -> Ok ())
  | _ ->
    Context.delete_scoped t.ctx (scope_of_tier tier) key;
    Ok ()
;;

let take_unique limit entries =
  if limit <= 0
  then []
  else (
    let seen = Hashtbl.create (limit + 1) in
    let rec loop remaining acc = function
      | [] -> List.rev acc
      | _ when remaining <= 0 -> List.rev acc
      | (key, value) :: rest ->
        if Hashtbl.mem seen key
        then loop remaining acc rest
        else (
          Hashtbl.replace seen key ();
          loop (remaining - 1) ((key, value) :: acc) rest)
    in
    loop limit [] entries)
;;

let query_context t ~tier ~prefix =
  Context.keys_in_scope t.ctx (scope_of_tier tier)
  |> List.filter (fun key -> String.starts_with ~prefix key)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier tier) key with
    | Some value -> Some (key, value)
    | None -> None)
;;

let query t ~tier ~prefix ~limit =
  if limit <= 0
  then []
  else (
    match tier with
    | Long_term ->
      let backend_entries =
        match t.long_term with
        | Some backend -> backend.query ~prefix ~limit
        | None -> []
      in
      take_unique limit (backend_entries @ query_context t ~tier:Long_term ~prefix)
    | _ -> take_unique limit (query_context t ~tier ~prefix))
;;

let promote t key =
  match Context.get_scoped t.ctx (scope_of_tier Scratchpad) key with
  | Some value ->
    Context.set_scoped t.ctx (scope_of_tier Working) key value;
    Context.delete_scoped t.ctx (scope_of_tier Scratchpad) key;
    true
  | None -> false
;;

let working_entries t =
  Context.keys_in_scope t.ctx (scope_of_tier Working)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Working) key with
    | Some v -> Some (key, v)
    | None -> None)
;;

let scratchpad_entries t =
  Context.keys_in_scope t.ctx (scope_of_tier Scratchpad)
  |> List.filter_map (fun key ->
    match Context.get_scoped t.ctx (scope_of_tier Scratchpad) key with
    | Some v -> Some (key, v)
    | None -> None)
;;

let clear_scratchpad t =
  let keys = Context.keys_in_scope t.ctx (scope_of_tier Scratchpad) in
  List.iter (fun key -> Context.delete_scoped t.ctx (scope_of_tier Scratchpad) key) keys
;;

let keys_in_tier t tier = Context.keys_in_scope t.ctx (scope_of_tier tier)

let stats t =
  let count tier = List.length (keys_in_tier t tier) in
  count Scratchpad, count Working, count Episodic, count Procedural, count Long_term
;;

let context t = t.ctx

(* ── Episodic memory (delegated to Memory_episodic) ───── *)

type outcome = Memory_episodic.outcome =
  | Success of string
  | Failure of string
  | Neutral

type episode = Memory_episodic.episode =
  { id : string
  ; timestamp : float
  ; participants : string list
  ; action : string
  ; outcome : outcome
  ; salience : float
  ; metadata : (string * Yojson.Safe.t) list
  }

let store_episode t ep = Memory_episodic.store t.ctx ep
let recall_episode t id = Memory_episodic.recall_one t.ctx id

let recall_episodes t ?now ?decay_rate ?min_salience ?limit ?filter () =
  Memory_episodic.recall t.ctx ?now ?decay_rate ?min_salience ?limit ?filter ()
;;

let boost_salience t id amount = Memory_episodic.boost_salience t.ctx id amount
let forget_episode t id = Memory_episodic.forget t.ctx id
let episode_count t = Memory_episodic.count t.ctx

(* ── Procedural memory (delegated to Memory_procedural) ── *)

type procedure = Memory_procedural.procedure =
  { id : string
  ; pattern : string
  ; action : string
  ; success_count : int
  ; failure_count : int
  ; confidence : float
  ; last_used : float
  ; metadata : (string * Yojson.Safe.t) list
  }

let store_procedure t proc = Memory_procedural.store t.ctx proc

let matching_procedures t ~pattern ?min_confidence ?filter () =
  Memory_procedural.matching t.ctx ~pattern ?min_confidence ?filter ()
;;

let find_procedure t ~pattern ?min_confidence ?filter ?touch () =
  Memory_procedural.find t.ctx ~pattern ?min_confidence ?filter ?touch ()
;;

let best_procedure t ~pattern = Memory_procedural.best t.ctx ~pattern
let record_success t id = Memory_procedural.record_success t.ctx id
let record_failure t id = Memory_procedural.record_failure t.ctx id
let forget_procedure t id = Memory_procedural.forget t.ctx id
let procedure_count t = Memory_procedural.count t.ctx
