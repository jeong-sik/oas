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

let ( let* ) = Result.bind

let decode_error detail =
  Error
    (Error.Serialization
       (JsonParseError { detail = "Collaboration.of_json: " ^ detail }))

let expect_object what = function
  | `Assoc fields -> Ok fields
  | _ -> decode_error (what ^ " must be an object")

let required_field what fields key =
  match List.assoc_opt key fields with
  | Some value -> Ok value
  | None -> decode_error (Printf.sprintf "%s missing '%s'" what key)

let parse_required what fields key parser =
  let* value = required_field what fields key in
  parser value |> Result.map_error (fun err ->
      Error.Serialization
        (JsonParseError
           { detail = Printf.sprintf "Collaboration.of_json: %s.%s: %s" what key err }))

let parse_optional fields key parser =
  match List.assoc_opt key fields with
  | None | Some `Null -> Ok None
  | Some value ->
      parser value
      |> Result.map Option.some
      |> Result.map_error (fun detail ->
             Error.Serialization
               (JsonParseError { detail = "Collaboration.of_json: " ^ detail }))

let parse_list_field what fields key parser =
  let* value = required_field what fields key in
  match value with
  | `List items ->
      List.mapi
        (fun idx item ->
          parser item |> Result.map_error (fun err ->
              Printf.sprintf "%s.%s[%d]: %s" what key idx err))
        items
      |> List.fold_left
           (fun acc item ->
             let* parsed = acc in
             let* value = item in
             Ok (value :: parsed))
           (Ok [])
      |> Result.map List.rev
      |> Result.map_error (fun detail ->
             Error.Serialization
               (JsonParseError { detail = "Collaboration.of_json: " ^ detail }))
  | _ ->
      decode_error (Printf.sprintf "%s.%s must be an array" what key)

let string_field what fields key =
  parse_required what fields key (function
    | `String s -> Ok s
    | _ -> Error "must be a string")

let float_field what fields key =
  parse_required what fields key (function
    | `Float f -> Ok f
    | `Int i -> Ok (float_of_int i)
    | _ -> Error "must be a float")

let string_option_field what fields key =
  parse_optional fields key (function
    | `String s -> Ok s
    | _ -> Error (Printf.sprintf "%s.%s must be a string" what key))

let int_option_field what fields key =
  parse_optional fields key (function
    | `Int i -> Ok i
    | _ -> Error (Printf.sprintf "%s.%s must be an int" what key))

let shared_context_json () = Context.to_json (Context.create ())

let vote_to_contribution idx json =
  let* fields = expect_object (Printf.sprintf "legacy vote[%d]" idx) json in
  let* topic = string_field "legacy vote" fields "topic" in
  let* choice = string_field "legacy vote" fields "choice" in
  let voter =
    match List.assoc_opt "voter" fields with
    | Some (`String s) -> Ok s
    | None | Some `Null -> Ok "anonymous"
    | Some _ -> decode_error (Printf.sprintf "legacy vote[%d].voter must be a string" idx)
  in
  let* voter = voter in
  let cast_at =
    match List.assoc_opt "cast_at" fields with
    | Some (`Float f) -> Ok f
    | Some (`Int i) -> Ok (float_of_int i)
    | None | Some `Null -> Ok 0.0
    | Some _ -> decode_error (Printf.sprintf "legacy vote[%d].cast_at must be a float" idx)
  in
  let* cast_at = cast_at in
  Ok
    (contribution_to_yojson
       { agent = voter; kind = "vote"; content = topic ^ ": " ^ choice; created_at = cast_at })

let normalize_legacy_json = function
  | `Assoc fields ->
      let contributions_value =
        match List.assoc_opt "contributions" fields with
        | Some (`List _ as value) -> Ok value
        | Some `Null | None -> (
            match List.assoc_opt "votes" fields with
            | Some (`List votes) ->
                List.mapi vote_to_contribution votes
                |> List.fold_left
                     (fun acc item ->
                       let* parsed = acc in
                       let* value = item in
                       Ok (value :: parsed))
                     (Ok [])
                |> Result.map (fun values -> `List (List.rev values))
            | Some `Null | None -> Ok (`List [])
            | Some _ -> decode_error "legacy votes must be an array")
        | Some _ -> decode_error "contributions must be an array"
      in
      let* contributions_value = contributions_value in
      let metadata_value =
        match List.assoc_opt "metadata" fields with
        | Some (`Assoc _ as value) -> Ok value
        | Some `Null | None -> Ok (`Assoc [])
        | Some _ -> decode_error "metadata must be an object"
      in
      let* metadata_value = metadata_value in
      let shared_context_value =
        match List.assoc_opt "shared_context" fields with
        | Some (`Assoc _ as value) -> Ok value
        | Some `Null | None -> Ok (shared_context_json ())
        | Some _ -> decode_error "shared_context must be an object"
      in
      let* shared_context_value = shared_context_value in
      Ok
        (`Assoc
           (List.remove_assoc "votes"
              (List.remove_assoc "contributions"
                 (List.remove_assoc "metadata"
                    (List.remove_assoc "shared_context" fields)))
           @ [
               ("contributions", contributions_value);
               ("metadata", metadata_value);
               ("shared_context", shared_context_value);
             ]))
  | json -> Ok json

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
  let* json = normalize_legacy_json json in
  let* fields = expect_object "root" json in
  let* id = string_field "root" fields "id" in
  let* goal = string_field "root" fields "goal" in
  let* phase = parse_required "root" fields "phase" phase_of_yojson in
  let* participants = parse_list_field "root" fields "participants" participant_of_yojson in
  let* artifacts = parse_list_field "root" fields "artifacts" artifact_of_yojson in
  let* contributions =
    parse_list_field "root" fields "contributions" contribution_of_yojson
  in
  let* shared_context_json = required_field "root" fields "shared_context" in
  let shared_context =
    match shared_context_json with
    | `Assoc _ as value -> Context.of_json value
    | _ -> Context.create ()
  in
  let* created_at = float_field "root" fields "created_at" in
  let* updated_at = float_field "root" fields "updated_at" in
  let* outcome = string_option_field "root" fields "outcome" in
  let* max_participants = int_option_field "root" fields "max_participants" in
  let* metadata_json = required_field "root" fields "metadata" in
  let* metadata_fields = expect_object "metadata" metadata_json in
  Ok
    {
      id;
      goal;
      phase;
      participants;
      artifacts;
      contributions;
      shared_context;
      created_at;
      updated_at;
      outcome;
      max_participants;
      metadata = metadata_fields;
    }
