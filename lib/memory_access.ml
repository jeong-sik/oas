(** Memory access control — agent-scoped permission layer.

    @since 0.76.0 *)

open Result_syntax

type permission =
  | Read
  | Write
  | ReadWrite

type policy =
  { agent_name : string
  ; tier : Memory.tier
  ; key_pattern : string
  ; permission : permission
  }

type access_error =
  | Denied of
      { agent_name : string
      ; tier : Memory.tier
      ; key : string
      ; needed : permission
      }
  | Backend_failed of
      { agent_name : string
      ; tier : Memory.tier
      ; key : string
      ; op : string
      ; detail : string
      }

type t =
  { mem : Memory.t
  ; policies : (string, policy list) Hashtbl.t (* agent_name -> policies *)
  }

let create mem = { mem; policies = Hashtbl.create 8 }
let memory t = t.mem

let grant t policy =
  let existing =
    match Hashtbl.find_opt t.policies policy.agent_name with
    | Some ps -> ps
    | None -> []
  in
  Hashtbl.replace t.policies policy.agent_name (policy :: existing)
;;

let revoke t ~agent_name ~tier =
  match Hashtbl.find_opt t.policies agent_name with
  | None -> ()
  | Some ps ->
    let filtered = List.filter (fun p -> p.tier <> tier) ps in
    if filtered = []
    then Hashtbl.remove t.policies agent_name
    else Hashtbl.replace t.policies agent_name filtered
;;

let revoke_all t ~agent_name = Hashtbl.remove t.policies agent_name

let policies_for t agent_name =
  match Hashtbl.find_opt t.policies agent_name with
  | Some ps -> ps
  | None -> []
;;

(* Pattern matching: "*" matches everything, otherwise prefix match *)
let pattern_matches ~pattern key =
  pattern = "*"
  ||
  let plen = String.length pattern in
  let klen = String.length key in
  klen >= plen && String.sub key 0 plen = pattern
;;

let permission_includes ~needed ~granted =
  match needed, granted with
  | Read, (Read | ReadWrite) -> true
  | Write, (Write | ReadWrite) -> true
  | ReadWrite, ReadWrite -> true
  | _ -> false
;;

let check t ~agent ~tier ~key needed =
  let ps = policies_for t agent in
  List.exists
    (fun p ->
       p.tier = tier
       && pattern_matches ~pattern:p.key_pattern key
       && permission_includes ~needed ~granted:p.permission)
    ps
;;

let require t ~agent ~tier ~key needed =
  if check t ~agent ~tier ~key needed
  then Ok ()
  else Error (Denied { agent_name = agent; tier; key; needed })
;;

let store t ~agent ~tier key value =
  let* () = require t ~agent ~tier ~key Write in
  Memory.store t.mem ~tier key value
  |> Result.map_error (fun msg ->
    Backend_failed { agent_name = agent; tier; key; op = "store"; detail = msg })
;;

let recall t ~agent ~tier key =
  let* () = require t ~agent ~tier ~key Read in
  Ok (Memory.recall t.mem ~tier key)
;;

let recall_exact t ~agent ~tier key =
  let* () = require t ~agent ~tier ~key Read in
  Ok (Memory.recall_exact t.mem ~tier key)
;;

let forget t ~agent ~tier key =
  let* () = require t ~agent ~tier ~key Write in
  Memory.forget t.mem ~tier key
  |> Result.map_error (fun msg ->
    Backend_failed { agent_name = agent; tier; key; op = "forget"; detail = msg })
;;

let store_episode t ~agent (ep : Memory.episode) =
  let* () = require t ~agent ~tier:Episodic ~key:ep.id Write in
  Memory.store_episode t.mem ep;
  Ok ()
;;

let recall_episodes t ~agent ?now ?decay_rate ?min_salience ?limit () =
  let* () = require t ~agent ~tier:Episodic ~key:"*" Read in
  Ok (Memory.recall_episodes t.mem ?now ?decay_rate ?min_salience ?limit ())
;;

let store_procedure t ~agent (proc : Memory.procedure) =
  let* () = require t ~agent ~tier:Procedural ~key:proc.id Write in
  Memory.store_procedure t.mem proc;
  Ok ()
;;

let find_procedure t ~agent ~pattern ?min_confidence ?(touch = false) () =
  let* () = require t ~agent ~tier:Procedural ~key:"*" Read in
  Ok (Memory.find_procedure t.mem ~pattern ?min_confidence ~touch ())
;;

let best_procedure t ~agent ~pattern = find_procedure t ~agent ~pattern ()

let tier_to_string = function
  | Memory.Scratchpad -> "Scratchpad"
  | Memory.Working -> "Working"
  | Memory.Episodic -> "Episodic"
  | Memory.Procedural -> "Procedural"
  | Memory.Long_term -> "Long_term"
;;

let access_error_to_string = function
  | Denied { agent_name; tier; key; needed } ->
    let perm_str =
      match needed with
      | Read -> "Read"
      | Write -> "Write"
      | ReadWrite -> "ReadWrite"
    in
    Printf.sprintf
      "Access denied: agent '%s' needs %s on %s:%s"
      agent_name
      perm_str
      (tier_to_string tier)
      key
  | Backend_failed { agent_name; tier; key; op; detail } ->
    Printf.sprintf
      "Memory operation failed: agent '%s' on %s:%s (%s) — %s"
      agent_name
      (tier_to_string tier)
      key
      op
      detail
;;
