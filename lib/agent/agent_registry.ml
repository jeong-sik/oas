(** Agent Registry — capability-based multi-agent discovery.

    Maintains a registry of local and remote agents, supporting:
    - Local agents: direct Agent.t references
    - Remote agents: URL + Agent Card (fetched via HTTP)
    - Capability-based lookup for agent routing

    Design:
    - Flat Hashtbl registry (name -> agent_entry)
    - Remote discovery via GET <url>/.well-known/agent.json
    - No dependency on legacy coordinator modules — can be used standalone *)

open Result_syntax

(* ── Agent entry ─────────────────────────────────────────── *)

type agent_entry =
  | Local of
      { agent : Agent.t
      ; card : Agent_card.agent_card
      }
  | Remote of
      { url : string
      ; card : Agent_card.agent_card
      }

type mutex =
  | Stdlib_mu of Mutex.t
  | Eio_mu of Eio.Mutex.t

type t =
  { mu : mutex
  ; agents : (string, agent_entry) Hashtbl.t
  ; log : Log.t
  }

(* ── Constructor ─────────────────────────────────────────── *)

let create () =
  { mu = Eio_mu (Eio.Mutex.create ())
  ; agents = Hashtbl.create 16
  ; log = Log.create ~module_name:"agent_registry" ()
  }
;;

let create_sync () =
  { mu = Stdlib_mu (Mutex.create ())
  ; agents = Hashtbl.create 16
  ; log = Log.create ~module_name:"agent_registry" ()
  }
;;

let with_lock t f =
  match t.mu with
  | Stdlib_mu mu ->
    Mutex.lock mu;
    Fun.protect f ~finally:(fun () -> Mutex.unlock mu)
  | Eio_mu mu -> Eio.Mutex.use_rw ~protect:true mu f
;;

(* ── Registration ────────────────────────────────────────── *)

let register_local t ~name agent =
  let card = Agent.card agent in
  with_lock t (fun () -> Hashtbl.replace t.agents name (Local { agent; card }));
  Log.info t.log "registered local agent" [ Log.S ("name", name) ]
;;

let register_remote t ~name ~url card =
  with_lock t (fun () -> Hashtbl.replace t.agents name (Remote { url; card }));
  Log.info t.log "registered remote agent" [ Log.S ("name", name); Log.S ("url", url) ]
;;

(* ── Lookup ──────────────────────────────────────────────── *)

let lookup t name = with_lock t (fun () -> Hashtbl.find_opt t.agents name)

let list_all t =
  with_lock t (fun () ->
    Hashtbl.fold (fun name entry acc -> (name, entry) :: acc) t.agents [])
;;

let list_by_capability t (cap : Agent_card.capability) =
  with_lock t (fun () ->
    Hashtbl.fold
      (fun name entry acc ->
         let card =
           match entry with
           | Local { card; _ } -> card
           | Remote { card; _ } -> card
         in
         if Agent_card.has_capability card cap then (name, entry) :: acc else acc)
      t.agents
      [])
;;

let list_by_tool t tool_name =
  with_lock t (fun () ->
    Hashtbl.fold
      (fun name entry acc ->
         let card =
           match entry with
           | Local { card; _ } -> card
           | Remote { card; _ } -> card
         in
         if Agent_card.can_handle_tool card tool_name then (name, entry) :: acc else acc)
      t.agents
      [])
;;

(* ── Remote discovery ────────────────────────────────────── *)

(** Fetch agent card from a remote URL via GET <url>/.well-known/agent.json. *)
let fetch_remote_card ~sw ~net url =
  let card_url = url ^ "/.well-known/agent.json" in
  let error_detail = function
    | Llm_provider.Http_client.HttpError { code; body; _ } ->
      Printf.sprintf "HTTP %d: %s" code body
    | NetworkError { message; _ } -> message
    | TimeoutError { message; _ } -> message
    | AcceptRejected { reason } -> "Response rejected: " ^ reason
    | ProviderTerminal { message; _ } -> message
    | ProviderFailure { kind; message } ->
      Llm_provider.Http_client.provider_failure_to_string ~kind ~message
  in
  match Llm_provider.Http_client.get_sync ~sw ~net ~url:card_url ~headers:[] () with
  | Ok (200, body_str) ->
    (try
       let json = Yojson.Safe.from_string body_str in
       Agent_card.of_json json
     with
     | Yojson.Json_error msg ->
       Error
         (Error.Orchestration
            (DiscoveryFailed { url; detail = "JSON parse error: " ^ msg })))
  | Ok (code, _) ->
    Error
      (Error.Orchestration
         (DiscoveryFailed { url; detail = Printf.sprintf "HTTP %d" code }))
  | Error err ->
    Error (Error.Orchestration (DiscoveryFailed { url; detail = error_detail err }))
;;

(** Discover a remote agent by fetching its card and registering it. *)
let discover_and_register ~sw ~net t ~name ~url =
  let* card = fetch_remote_card ~sw ~net url in
  with_lock t (fun () -> Hashtbl.replace t.agents name (Remote { url; card }));
  Log.info t.log "registered remote agent" [ Log.S ("name", name); Log.S ("url", url) ];
  Ok ()
;;

(* ── Unregister ──────────────────────────────────────────── *)

let unregister t name = with_lock t (fun () -> Hashtbl.remove t.agents name)
let count t = with_lock t (fun () -> Hashtbl.length t.agents)

(* ── Card accessor ───────────────────────────────────────── *)

let card_of_entry = function
  | Local { card; _ } -> card
  | Remote { card; _ } -> card
;;
