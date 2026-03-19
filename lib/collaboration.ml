(** Collaboration context — shared state for multi-agent coordination. *)

type phase =
  | Bootstrapping
  | Active
  | Waiting_on_participants
  | Finalizing
  | Completed
  | Failed
  | Cancelled
[@@deriving yojson, show]

type participant_state =
  | Planned
  | Joined
  | Working
  | Done
  | Failed_participant
  | Left
[@@deriving yojson, show]

type participant = {
  name: string;
  role: string option;
  state: participant_state;
  joined_at: float option;
  finished_at: float option;
  summary: string option;
}
[@@deriving yojson, show]

type artifact = {
  id: string;
  name: string;
  kind: string;
  producer: string;
  created_at: float;
}
[@@deriving yojson, show]

type contribution = {
  agent: string;
  kind: string;
  content: string;
  created_at: float;
}
[@@deriving yojson, show]

type t = {
  id: string;
  goal: string;
  phase: phase;
  participants: participant list;
  artifacts: artifact list;
  contributions: contribution list;
  shared_context: Context.t;
  created_at: float;
  updated_at: float;
  outcome: string option;
  max_participants: int option;
  metadata: (string * Yojson.Safe.t) list;
}

(* --- ID generation (same pattern as Session) --- *)

let generate_id () =
  let t = Unix.gettimeofday () in
  let hi = Float.to_int (Float.rem t 1_000_000.) in
  let lo = Random.int 0xFFFF in
  Printf.sprintf "collab-%06x%04x" hi lo

(* --- Construction --- *)

let create ?id ?shared_context ~goal () =
  let now = Unix.gettimeofday () in
  {
    id = Option.value id ~default:(generate_id ());
    goal;
    phase = Bootstrapping;
    participants = [];
    artifacts = [];
    contributions = [];
    shared_context = Option.value shared_context ~default:(Context.create ());
    created_at = now;
    updated_at = now;
    outcome = None;
    max_participants = None;
    metadata = [];
  }

(* --- Timestamp --- *)

let touch t =
  { t with updated_at = Unix.gettimeofday () }

(* --- Participant operations --- *)

let add_participant t p =
  { t with participants = t.participants @ [p] } |> touch

let find_participant t name =
  List.find_opt (fun (p : participant) -> p.name = name) t.participants

let update_participant t name f =
  let participants =
    List.map (fun (p : participant) ->
      if p.name = name then f p else p
    ) t.participants
  in
  { t with participants } |> touch

let remove_participant t name =
  let participants =
    List.filter (fun (p : participant) -> p.name <> name) t.participants
  in
  { t with participants } |> touch

let active_participants t =
  List.filter (fun (p : participant) ->
    match p.state with
    | Joined | Working -> true
    | _ -> false
  ) t.participants

(* --- Artifact and contribution operations --- *)

let add_artifact t a =
  { t with artifacts = t.artifacts @ [a] } |> touch

let add_contribution t c =
  { t with contributions = t.contributions @ [c] } |> touch

(* --- Phase and outcome --- *)

let set_phase t phase =
  { t with phase } |> touch

let set_outcome t outcome =
  { t with outcome = Some outcome } |> touch

let is_terminal t =
  match t.phase with
  | Completed | Failed | Cancelled -> true
  | Bootstrapping | Active | Waiting_on_participants | Finalizing -> false

(* --- Serialization --- *)

let to_json t =
  `Assoc [
    ("id", `String t.id);
    ("goal", `String t.goal);
    ("phase", phase_to_yojson t.phase);
    ("participants", `List (List.map participant_to_yojson t.participants));
    ("artifacts", `List (List.map artifact_to_yojson t.artifacts));
    ("contributions", `List (List.map contribution_to_yojson t.contributions));
    ("shared_context", Context.to_json t.shared_context);
    ("created_at", `Float t.created_at);
    ("updated_at", `Float t.updated_at);
    ("outcome",
      (match t.outcome with Some s -> `String s | None -> `Null));
    ("max_participants",
      (match t.max_participants with Some n -> `Int n | None -> `Null));
    ("metadata", `Assoc t.metadata);
  ]

let of_json json =
  try
    let open Yojson.Safe.Util in
    let unwrap = function
      | Ok v -> v
      | Error e -> failwith e
    in
    let parse_list parser j =
      match j with
      | `List xs -> List.map (fun x -> unwrap (parser x)) xs
      | other -> raise (Type_error ("expected JSON array", other))
    in
    let shared_context =
      match json |> member "shared_context" with
      | `Null -> Context.create ()
      | v -> Context.of_json v
    in
    let metadata =
      match json |> member "metadata" with
      | `Assoc kvs -> kvs
      | `Null -> []
      | _ -> []
    in
    Ok {
      id = json |> member "id" |> to_string;
      goal = json |> member "goal" |> to_string;
      phase = unwrap (json |> member "phase" |> phase_of_yojson);
      participants =
        parse_list participant_of_yojson (json |> member "participants");
      artifacts =
        parse_list artifact_of_yojson (json |> member "artifacts");
      contributions =
        (match json |> member "contributions" with
         | `Null ->
           (* Backward compat: parse legacy "votes" key as contributions *)
           (match json |> member "votes" with
            | `Null -> []
            | `List votes_json ->
              List.map (fun vj ->
                let topic =
                  (try to_string (vj |> member "topic") with _ -> "") in
                let choice =
                  (try to_string (vj |> member "choice") with _ -> "") in
                let voter =
                  (try to_string (vj |> member "voter") with _ -> "anonymous") in
                let cast_at =
                  (try to_float (vj |> member "cast_at") with _ -> 0.0) in
                { agent = voter;
                  kind = "vote";
                  content = topic ^ ": " ^ choice;
                  created_at = cast_at; }
              ) votes_json
            | _ -> [])
         | contributions_json ->
           parse_list contribution_of_yojson contributions_json);
      shared_context;
      created_at = json |> member "created_at" |> to_float;
      updated_at = json |> member "updated_at" |> to_float;
      outcome = json |> member "outcome" |> to_string_option;
      max_participants =
        json |> member "max_participants" |> to_int_option;
      metadata;
    }
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Error.Serialization
      (JsonParseError { detail = "Collaboration.of_json: " ^ msg }))
  | exn ->
    Error (Error.Serialization
      (JsonParseError
        { detail = "Collaboration.of_json: " ^ Printexc.to_string exn }))
