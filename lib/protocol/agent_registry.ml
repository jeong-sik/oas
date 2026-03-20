(** Agent Registry — capability-based multi-agent discovery.

    Maintains a registry of local and remote agents, supporting:
    - Local agents: direct Agent.t references
    - Remote agents: URL + Agent Card (fetched via HTTP)
    - Capability-based lookup for orchestration routing

    Design:
    - Flat Hashtbl registry (name -> agent_entry)
    - Remote discovery via GET <url>/.well-known/agent.json
    - No dependency on Orchestrator — can be used standalone *)

let ( let* ) = Result.bind

(* ── Agent entry ─────────────────────────────────────────── *)

type agent_entry =
  | Local of { agent: Agent.t; card: Agent_card.agent_card }
  | Remote of { url: string; card: Agent_card.agent_card }

type t = {
  agents: (string, agent_entry) Hashtbl.t;
  log: Log.t;
}

(* ── Constructor ─────────────────────────────────────────── *)

let create () = {
  agents = Hashtbl.create 16;
  log = Log.create ~module_name:"agent_registry" ();
}

(* ── Registration ────────────────────────────────────────── *)

let register_local t ~name agent =
  let card = Agent.card agent in
  Hashtbl.replace t.agents name (Local { agent; card });
  Log.info t.log "registered local agent" [Log.S ("name", name)]

let register_remote t ~name ~url card =
  Hashtbl.replace t.agents name (Remote { url; card });
  Log.info t.log "registered remote agent"
    [Log.S ("name", name); Log.S ("url", url)]

(* ── Lookup ──────────────────────────────────────────────── *)

let lookup t name =
  Hashtbl.find_opt t.agents name

let list_all t =
  Hashtbl.fold (fun name entry acc -> (name, entry) :: acc) t.agents []

let list_by_capability t (cap : Agent_card.capability) =
  Hashtbl.fold (fun name entry acc ->
    let card = match entry with
      | Local { card; _ } -> card
      | Remote { card; _ } -> card
    in
    if Agent_card.has_capability card cap then
      (name, entry) :: acc
    else acc
  ) t.agents []

let list_by_tool t tool_name =
  Hashtbl.fold (fun name entry acc ->
    let card = match entry with
      | Local { card; _ } -> card
      | Remote { card; _ } -> card
    in
    if Agent_card.can_handle_tool card tool_name then
      (name, entry) :: acc
    else acc
  ) t.agents []

(* ── Remote discovery ────────────────────────────────────── *)

(** Fetch agent card from a remote URL via GET <url>/.well-known/agent.json. *)
let fetch_remote_card ~sw ~net url =
  let card_url = url ^ "/.well-known/agent.json" in
  let uri = Uri.of_string card_url in
  let https = Api.make_https () in
  let client = Cohttp_eio.Client.make ~https net in
  try
    let resp, body =
      Cohttp_eio.Client.get ~sw client uri
    in
    match Cohttp.Response.status resp with
    | `OK ->
      let body_str = Eio.Buf_read.(of_flow ~max_size:Llm_provider.Api_common.max_response_body body |> take_all) in
      (try
         let json = Yojson.Safe.from_string body_str in
         Agent_card.of_json json
       with Yojson.Json_error msg ->
         Error (Error.Orchestration (DiscoveryFailed {
           url; detail = "JSON parse error: " ^ msg })))
    | status ->
      let code = Cohttp.Code.code_of_status status in
      Error (Error.Orchestration (DiscoveryFailed {
        url; detail = Printf.sprintf "HTTP %d" code }))
  with
  | Eio.Io _ as exn ->
    Error (Error.Orchestration (DiscoveryFailed {
      url; detail = Printexc.to_string exn }))
  | Unix.Unix_error _ as exn ->
    Error (Error.Orchestration (DiscoveryFailed {
      url; detail = Printexc.to_string exn }))
  | Failure msg ->
    Error (Error.Orchestration (DiscoveryFailed { url; detail = msg }))

(** Discover a remote agent by fetching its card and registering it. *)
let discover_and_register ~sw ~net t ~name ~url =
  let* card = fetch_remote_card ~sw ~net url in
  register_remote t ~name ~url card;
  Ok ()

(* ── Unregister ──────────────────────────────────────────── *)

let unregister t name =
  Hashtbl.remove t.agents name

let count t =
  Hashtbl.length t.agents

(* ── Card accessor ───────────────────────────────────────── *)

let card_of_entry = function
  | Local { card; _ } -> card
  | Remote { card; _ } -> card
