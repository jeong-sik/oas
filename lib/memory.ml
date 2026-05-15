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

type episodic_backend =
  { persist_episode : episode -> unit
  ; retrieve_episode : id:string -> episode option
  ; remove_episode : id:string -> unit
  ; all_episodes : unit -> episode list
  }

type procedural_backend =
  { persist_procedure : procedure -> unit
  ; retrieve_procedure : id:string -> procedure option
  ; remove_procedure : id:string -> unit
  ; all_procedures : unit -> procedure list
  }

type t =
  { ctx : Context.t
  ; mutable long_term : long_term_backend option
  ; mutable episodic : episodic_backend option
  ; mutable procedural : procedural_backend option
  }

let scope_of_tier = function
  | Scratchpad -> Context.Temp
  | Working -> Context.Session
  | Episodic -> Context.Custom "ep"
  | Procedural -> Context.Custom "pr"
  | Long_term -> Context.Custom "lt"
;;

let create ?(ctx = Context.create ()) ?long_term ?episodic ?procedural () =
  { ctx; long_term; episodic; procedural }
;;

let set_long_term_backend t backend = t.long_term <- Some backend
let set_episodic_backend t backend = t.episodic <- Some backend
let set_procedural_backend t backend = t.procedural <- Some backend

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
     | Episodic ->
       (match t.episodic with
        | Some backend ->
          Option.map Memory_episodic.episode_to_json (backend.retrieve_episode ~id:key)
        | None -> None)
     | Procedural ->
       (match t.procedural with
        | Some backend ->
          Option.map
            Memory_procedural.procedure_to_json
            (backend.retrieve_procedure ~id:key)
        | None -> None))
;;

let context_episode_json t key =
  match Memory_episodic.recall_one t.ctx key with
  | Some ep -> Some (Memory_episodic.episode_to_json ep)
  | None -> Context.get_scoped t.ctx (scope_of_tier Episodic) key
;;

let context_procedure_json t key =
  match Context.get_scoped t.ctx (scope_of_tier Procedural) key with
  | Some _ as found -> found
  | None -> None
;;

let recall_exact t ~tier key =
  match tier with
  | Long_term ->
    (match t.long_term with
     | Some backend -> backend.retrieve ~key
     | None -> Context.get_scoped t.ctx (scope_of_tier Long_term) key)
  | Episodic ->
    (match t.episodic with
     | Some backend ->
       (match backend.retrieve_episode ~id:key with
        | Some ep -> Some (Memory_episodic.episode_to_json ep)
        | None -> context_episode_json t key)
     | None -> context_episode_json t key)
  | Procedural ->
    (match t.procedural with
     | Some backend ->
       (match backend.retrieve_procedure ~id:key with
        | Some proc -> Some (Memory_procedural.procedure_to_json proc)
        | None -> context_procedure_json t key)
     | None -> context_procedure_json t key)
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
  | Episodic ->
    Context.delete_scoped t.ctx (scope_of_tier Episodic) key;
    Option.iter (fun backend -> backend.remove_episode ~id:key) t.episodic;
    Ok ()
  | Procedural ->
    Context.delete_scoped t.ctx (scope_of_tier Procedural) key;
    Option.iter (fun backend -> backend.remove_procedure ~id:key) t.procedural;
    Ok ()
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

let query_episodic_backend t ~prefix =
  match t.episodic with
  | None -> []
  | Some (backend : episodic_backend) ->
    backend.all_episodes ()
    |> List.filter (fun (ep : episode) -> String.starts_with ~prefix ep.id)
    |> List.map (fun (ep : episode) -> ep.id, Memory_episodic.episode_to_json ep)
;;

let query_procedural_backend t ~prefix =
  match t.procedural with
  | None -> []
  | Some (backend : procedural_backend) ->
    backend.all_procedures ()
    |> List.filter (fun (proc : procedure) -> String.starts_with ~prefix proc.id)
    |> List.map (fun (proc : procedure) ->
      proc.id, Memory_procedural.procedure_to_json proc)
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
    | Episodic ->
      take_unique limit (query_episodic_backend t ~prefix @ query_context t ~tier ~prefix)
    | Procedural ->
      take_unique
        limit
        (query_procedural_backend t ~prefix @ query_context t ~tier ~prefix)
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
let context t = t.ctx

(* ── Episodic memory (delegated to Memory_episodic) ───── *)

let unique_episodes episodes =
  let seen = Hashtbl.create (List.length episodes + 1) in
  List.filter
    (fun (ep : episode) ->
       if Hashtbl.mem seen ep.id
       then false
       else (
         Hashtbl.replace seen ep.id ();
         true))
    episodes
;;

let unique_procedures procedures =
  let seen = Hashtbl.create (List.length procedures + 1) in
  List.filter
    (fun (proc : procedure) ->
       if Hashtbl.mem seen proc.id
       then false
       else (
         Hashtbl.replace seen proc.id ();
         true))
    procedures
;;

let context_episodes t = Memory_episodic.all t.ctx

let backend_episodes t =
  match t.episodic with
  | Some backend -> backend.all_episodes ()
  | None -> []
;;

let all_episodes t = unique_episodes (backend_episodes t @ context_episodes t)

let store_episode t ep =
  Memory_episodic.store t.ctx ep;
  Option.iter (fun backend -> backend.persist_episode ep) t.episodic
;;

let recall_episode t id =
  match t.episodic with
  | Some backend ->
    (match backend.retrieve_episode ~id with
     | Some _ as found -> found
     | None -> Memory_episodic.recall_one t.ctx id)
  | None -> Memory_episodic.recall_one t.ctx id
;;

let recall_episodes t ?now ?decay_rate ?min_salience ?limit ?filter () =
  let now = Option.value now ~default:(Unix.gettimeofday ()) in
  let decay_rate = Option.value decay_rate ~default:0.01 in
  let min_salience = Option.value min_salience ~default:0.1 in
  let limit = Option.value limit ~default:50 in
  all_episodes t
  |> List.map (fun ep ->
    let effective = Memory_episodic.decayed_salience ~now ~decay_rate ep in
    { ep with salience = effective }, effective)
  |> List.filter (fun (_, salience) -> salience >= min_salience)
  |> List.filter (fun (ep, _) ->
    match filter with
    | Some predicate -> predicate ep
    | None -> true)
  |> List.sort (fun (_, left) (_, right) -> Float.compare right left)
  |> fun episodes ->
  let rec take n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | (ep, _) :: rest -> take (n - 1) (ep :: acc) rest
  in
  take limit [] episodes
;;

let boost_salience t id amount =
  match recall_episode t id with
  | Some ep ->
    let boosted = Float.min 1.0 (ep.salience +. amount) in
    store_episode t { ep with salience = boosted }
  | None -> ()
;;

let forget_episode t id =
  Memory_episodic.forget t.ctx id;
  Option.iter (fun backend -> backend.remove_episode ~id) t.episodic
;;

let episode_count t = List.length (all_episodes t)

(* ── Procedural memory (delegated to Memory_procedural) ── *)

let context_procedures t = Memory_procedural.all t.ctx

let backend_procedures t =
  match t.procedural with
  | Some backend -> backend.all_procedures ()
  | None -> []
;;

let all_procedures t = unique_procedures (backend_procedures t @ context_procedures t)

let recall_procedure t id =
  match t.procedural with
  | Some backend ->
    (match backend.retrieve_procedure ~id with
     | Some _ as found -> found
     | None ->
       (match Context.get_scoped t.ctx (scope_of_tier Procedural) id with
        | Some json -> Memory_procedural.procedure_of_json json
        | None -> None))
  | None ->
    (match Context.get_scoped t.ctx (scope_of_tier Procedural) id with
     | Some json -> Memory_procedural.procedure_of_json json
     | None -> None)
;;

let store_procedure t proc =
  Memory_procedural.store t.ctx proc;
  Option.iter (fun backend -> backend.persist_procedure proc) t.procedural
;;

let matching_procedures t ~pattern ?min_confidence ?filter () =
  let min_confidence = Option.value min_confidence ~default:0.0 in
  all_procedures t
  |> List.filter (fun proc ->
    Memory_procedural.string_contains ~needle:pattern proc.pattern
    && proc.confidence >= min_confidence
    &&
    match filter with
    | Some predicate -> predicate proc
    | None -> true)
  |> List.sort (fun left right -> Float.compare right.confidence left.confidence)
;;

let find_procedure t ~pattern ?min_confidence ?filter ?touch () =
  let touch = Option.value touch ~default:false in
  match matching_procedures t ~pattern ?min_confidence ?filter () with
  | best :: _ ->
    if touch
    then (
      let touched = { best with last_used = Unix.gettimeofday () } in
      store_procedure t touched;
      Some touched)
    else Some best
  | [] -> None
;;

let best_procedure t ~pattern = find_procedure t ~pattern ()

let update_procedure t id f =
  match recall_procedure t id with
  | Some proc -> store_procedure t (f proc)
  | None -> ()
;;

let record_success t id =
  update_procedure t id (fun proc ->
    let success_count = proc.success_count + 1 in
    let confidence =
      Memory_procedural.compute_confidence
        ~success_count
        ~failure_count:proc.failure_count
    in
    { proc with success_count; confidence; last_used = Unix.gettimeofday () })
;;

let record_failure t id =
  update_procedure t id (fun proc ->
    let failure_count = proc.failure_count + 1 in
    let confidence =
      Memory_procedural.compute_confidence
        ~success_count:proc.success_count
        ~failure_count
    in
    { proc with failure_count; confidence; last_used = Unix.gettimeofday () })
;;

let forget_procedure t id =
  Memory_procedural.forget t.ctx id;
  Option.iter (fun backend -> backend.remove_procedure ~id) t.procedural
;;

let procedure_count t = List.length (all_procedures t)

let stats t =
  let count tier = List.length (keys_in_tier t tier) in
  count Scratchpad, count Working, episode_count t, procedure_count t, count Long_term
;;
