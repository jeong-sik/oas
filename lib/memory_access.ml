(** Memory access control — agent-scoped permission layer.

    @since 0.76.0 *)

type permission =
  | Read
  | Write
  | ReadWrite

type policy = {
  agent_name: string;
  tier: Memory.tier;
  key_pattern: string;
  permission: permission;
}

type access_error =
  | Denied of { agent_name: string; tier: Memory.tier; key: string; needed: permission }
  | Store_failed of { agent_name: string; tier: Memory.tier; key: string; detail: string }

type t = {
  mem: Memory.t;
  policies: (string, policy list) Hashtbl.t;  (* agent_name -> policies *)
}

let create mem =
  { mem; policies = Hashtbl.create 8 }

let memory t = t.mem

let grant t policy =
  let existing = match Hashtbl.find_opt t.policies policy.agent_name with
    | Some ps -> ps
    | None -> []
  in
  Hashtbl.replace t.policies policy.agent_name (policy :: existing)

let revoke t ~agent_name ~tier =
  match Hashtbl.find_opt t.policies agent_name with
  | None -> ()
  | Some ps ->
    let filtered = List.filter (fun p -> p.tier <> tier) ps in
    if filtered = [] then Hashtbl.remove t.policies agent_name
    else Hashtbl.replace t.policies agent_name filtered

let revoke_all t ~agent_name =
  Hashtbl.remove t.policies agent_name

let policies_for t agent_name =
  match Hashtbl.find_opt t.policies agent_name with
  | Some ps -> ps
  | None -> []

(* Pattern matching: "*" matches everything, otherwise prefix match *)
let pattern_matches ~pattern key =
  pattern = "*" ||
  let plen = String.length pattern in
  let klen = String.length key in
  klen >= plen && String.sub key 0 plen = pattern

let permission_includes ~needed ~granted =
  match needed, granted with
  | Read, (Read | ReadWrite) -> true
  | Write, (Write | ReadWrite) -> true
  | ReadWrite, ReadWrite -> true
  | _ -> false

let check t ~agent ~tier ~key needed =
  let ps = policies_for t agent in
  List.exists (fun p ->
    p.tier = tier &&
    pattern_matches ~pattern:p.key_pattern key &&
    permission_includes ~needed ~granted:p.permission
  ) ps

let require t ~agent ~tier ~key needed =
  if check t ~agent ~tier ~key needed then Ok ()
  else Error (Denied { agent_name = agent; tier; key; needed })

let store t ~agent ~tier key value =
  match require t ~agent ~tier ~key Write with
  | Ok () ->
    (match Memory.store t.mem ~tier key value with
     | Ok () -> Ok ()
     | Error msg -> Error (Store_failed { agent_name = agent; tier; key; detail = msg }))
  | Error _ as err -> err

let recall t ~agent ~tier key =
  match require t ~agent ~tier ~key Read with
  | Ok () -> Ok (Memory.recall t.mem ~tier key)
  | Error _ as err -> err

let recall_exact t ~agent ~tier key =
  match require t ~agent ~tier ~key Read with
  | Ok () -> Ok (Memory.recall_exact t.mem ~tier key)
  | Error _ as err -> err

let forget t ~agent ~tier key =
  match require t ~agent ~tier ~key Write with
  | Ok () ->
    (match Memory.forget t.mem ~tier key with
     | Ok () -> Ok ()
     | Error msg -> Error (Store_failed { agent_name = agent; tier; key; detail = msg }))
  | Error _ as err -> err

let store_episode t ~agent (ep : Memory.episode) =
  match require t ~agent ~tier:Episodic ~key:ep.id Write with
  | Ok () -> Memory.store_episode t.mem ep; Ok ()
  | Error _ as err -> err

let recall_episodes t ~agent ?now ?decay_rate ?min_salience ?limit () =
  match require t ~agent ~tier:Episodic ~key:"*" Read with
  | Ok () -> Ok (Memory.recall_episodes t.mem ?now ?decay_rate ?min_salience ?limit ())
  | Error _ as err -> err

let store_procedure t ~agent (proc : Memory.procedure) =
  match require t ~agent ~tier:Procedural ~key:proc.id Write with
  | Ok () -> Memory.store_procedure t.mem proc; Ok ()
  | Error _ as err -> err

let find_procedure t ~agent ~pattern ?min_confidence ?(touch = false) () =
  match require t ~agent ~tier:Procedural ~key:"*" Read with
  | Ok () -> Ok (Memory.find_procedure t.mem ~pattern ?min_confidence ~touch ())
  | Error _ as err -> err

let best_procedure t ~agent ~pattern =
  find_procedure t ~agent ~pattern ()

let tier_to_string = function
  | Memory.Scratchpad -> "Scratchpad"
  | Memory.Working -> "Working"
  | Memory.Episodic -> "Episodic"
  | Memory.Procedural -> "Procedural"
  | Memory.Long_term -> "Long_term"

let access_error_to_string = function
  | Denied { agent_name; tier; key; needed } ->
    let perm_str = match needed with
      | Read -> "Read" | Write -> "Write" | ReadWrite -> "ReadWrite"
    in
    Printf.sprintf "Access denied: agent '%s' needs %s on %s:%s"
      agent_name perm_str (tier_to_string tier) key
  | Store_failed { agent_name; tier; key; detail } ->
    Printf.sprintf "Store failed: agent '%s' on %s:%s — %s"
      agent_name (tier_to_string tier) key detail
