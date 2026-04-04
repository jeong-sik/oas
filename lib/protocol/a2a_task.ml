(** A2A Task Lifecycle — Agent-to-Agent protocol task state machine.

    Implements the A2A task lifecycle with typed state transitions,
    message parts, and artifact management. Tasks flow through a
    well-defined state machine; illegal transitions return Error.

    Design:
    - Flat variant state machine with [valid_transitions] function.
    - [message_part] is protocol-level (distinct from [Types.content_block]).
    - In-memory task storage via Hashtbl. Persistent storage in v0.36+.
    - JSON serialization for wire format compatibility. *)

(* ── Task state ───────────────────────────────────────────────── *)

type task_state =
  | Submitted
  | Working
  | Input_required
  | Completed
  | Failed
  | Canceled

let task_state_to_wire_string = function
  | Submitted -> "TASK_STATE_SUBMITTED"
  | Working -> "TASK_STATE_WORKING"
  | Input_required -> "TASK_STATE_INPUT_REQUIRED"
  | Completed -> "TASK_STATE_COMPLETED"
  | Failed -> "TASK_STATE_FAILED"
  | Canceled -> "TASK_STATE_CANCELED"

let task_state_to_string = function
  | Submitted -> "submitted"
  | Working -> "working"
  | Input_required -> "input-required"
  | Completed -> "completed"
  | Failed -> "failed"
  | Canceled -> "canceled"

let task_state_of_string = function
  | "submitted" | "TASK_STATE_SUBMITTED" -> Ok Submitted
  | "working" | "TASK_STATE_WORKING" -> Ok Working
  | "input-required" | "input_required" | "TASK_STATE_INPUT_REQUIRED" -> Ok Input_required
  | "completed" | "TASK_STATE_COMPLETED" -> Ok Completed
  | "failed" | "TASK_STATE_FAILED" -> Ok Failed
  | "canceled" | "TASK_STATE_CANCELED" -> Ok Canceled
  | s -> Error (Printf.sprintf "unknown task state: %s" s)

let task_state_to_yojson s = `String (task_state_to_wire_string s)

let task_state_of_yojson = function
  | `String s ->
    (match task_state_of_string s with
     | Ok s -> Ok s
     | Error msg -> Error msg)
  | _ -> Error "expected string for task_state"

let pp_task_state fmt s = Format.fprintf fmt "%s" (task_state_to_string s)
let show_task_state = task_state_to_string

(* ── Valid transitions ────────────────────────────────────────── *)

let valid_transitions = function
  | Submitted -> [Working; Canceled; Failed]
  | Working -> [Input_required; Completed; Failed; Canceled]
  | Input_required -> [Working; Completed; Failed; Canceled]
  | Completed -> []
  | Failed -> []
  | Canceled -> []

let is_terminal = function
  | Completed | Failed | Canceled -> true
  | Submitted | Working | Input_required -> false

(* ── Transition error ─────────────────────────────────────────── *)

type transition_error =
  | InvalidTransition of { from_state: task_state; to_state: task_state }
  | TaskAlreadyTerminal of { state: task_state }

let transition_error_to_string = function
  | InvalidTransition r ->
    Printf.sprintf "invalid transition: %s -> %s"
      (task_state_to_string r.from_state)
      (task_state_to_string r.to_state)
  | TaskAlreadyTerminal r ->
    Printf.sprintf "task already terminal: %s" (task_state_to_string r.state)

(* ── Message parts ────────────────────────────────────────────── *)

type message_part =
  | Text_part of string
  | File_part of { name: string; mime_type: string; data: string }
  | Data_part of Yojson.Safe.t

let message_part_to_yojson = function
  | Text_part s ->
    `Assoc [("text", `String s)]
  | File_part { name; mime_type; data } ->
    `Assoc [
      ("raw", `String data);
      ("filename", `String name);
      ("mediaType", `String mime_type);
    ]
  | Data_part json ->
    `Assoc [("data", json)]

let message_part_of_yojson json =
  let open Yojson.Safe.Util in
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String "text") ->
       let text = json |> member "text" |> to_string in
       Ok (Text_part text)
     | Some (`String "file") ->
       let file = json |> member "file" in
       let name = file |> member "name" |> to_string in
       let mime_type = file |> member "mimeType" |> to_string in
       let data = file |> member "bytes" |> to_string in
       Ok (File_part { name; mime_type; data })
     | Some (`String "data") ->
       let data = json |> member "data" in
       Ok (Data_part data)
     | Some _ -> Error "unknown message_part type"
     | None ->
       (match List.assoc_opt "text" fields with
        | Some (`String text) -> Ok (Text_part text)
        | _ ->
          (match List.assoc_opt "raw" fields with
           | Some (`String raw) ->
             let name =
               match List.assoc_opt "filename" fields with
               | Some (`String n) -> n
               | _ -> ""
             in
             let mime_type =
               match List.assoc_opt "mediaType" fields with
               | Some (`String m) -> m
               | _ -> "application/octet-stream"
             in
             Ok (File_part { name; mime_type; data = raw })
           | _ ->
             (match List.assoc_opt "url" fields with
              | Some (`String url) ->
                let name =
                  match List.assoc_opt "filename" fields with
                  | Some (`String n) -> n
                  | _ -> ""
                in
                let mime_type =
                  match List.assoc_opt "mediaType" fields with
                  | Some (`String m) -> m
                  | _ -> "application/octet-stream"
                in
                Ok (File_part { name; mime_type; data = url })
              | _ ->
                match List.assoc_opt "data" fields with
                | Some data -> Ok (Data_part data)
                | None -> Error "unknown message_part type"))))
  | _ -> Error "unknown message_part type"

let pp_message_part fmt = function
  | Text_part s -> Format.fprintf fmt "Text(%s)" s
  | File_part { name; _ } -> Format.fprintf fmt "File(%s)" name
  | Data_part _ -> Format.fprintf fmt "Data(...)"

let show_message_part mp =
  Format.asprintf "%a" pp_message_part mp

(* ── Task message ─────────────────────────────────────────────── *)

type task_role = TaskUser | TaskAgent

let task_role_to_wire_string = function
  | TaskUser -> "ROLE_USER"
  | TaskAgent -> "ROLE_AGENT"

let task_role_of_string = function
  | "user" | "ROLE_USER" -> Ok TaskUser
  | "agent" | "ROLE_AGENT" -> Ok TaskAgent
  | s -> Error (Printf.sprintf "unknown task_role: %s" s)

type task_message = {
  role: task_role;
  parts: message_part list;
  metadata: (string * Yojson.Safe.t) list;
}

let task_message_to_yojson m =
  `Assoc [
    ("role", `String (task_role_to_wire_string m.role));
    ("parts", `List (List.map message_part_to_yojson m.parts));
    ("metadata", `Assoc m.metadata);
  ]

let task_message_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let role_s = json |> member "role" |> to_string in
    match task_role_of_string role_s with
    | Error e -> Error e
    | Ok role ->
      let parts_json = json |> member "parts" |> to_list in
      let parts_result = List.fold_left (fun acc j ->
        match acc with
        | Error _ as e -> e
        | Ok ps ->
          match message_part_of_yojson j with
          | Ok p -> Ok (p :: ps)
          | Error e -> Error e
      ) (Ok []) parts_json in
      match parts_result with
      | Error e -> Error e
      | Ok parts ->
        let metadata = match json |> member "metadata" with
          | `Assoc kvs -> kvs
          | _ -> []
        in
        Ok { role; parts = List.rev parts; metadata }
  with Type_error (msg, _) -> Error msg

(* ── Artifact ─────────────────────────────────────────────────── *)

type artifact = {
  name: string;
  parts: message_part list;
  metadata: (string * Yojson.Safe.t) list;
}

let artifact_to_yojson a =
  `Assoc [
    ("name", `String a.name);
    ("parts", `List (List.map message_part_to_yojson a.parts));
    ("metadata", `Assoc a.metadata);
  ]

let artifact_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let parts_json = json |> member "parts" |> to_list in
    let parts_result = List.fold_left (fun acc j ->
      match acc with
      | Error _ as e -> e
      | Ok ps ->
        match message_part_of_yojson j with
        | Ok p -> Ok (p :: ps)
        | Error e -> Error e
    ) (Ok []) parts_json in
    match parts_result with
    | Error e -> Error e
    | Ok parts ->
      let parts = List.rev parts in
      let metadata = match json |> member "metadata" with
        | `Assoc kvs -> kvs
        | _ -> []
      in
      Ok { name; parts; metadata }
  with Type_error (msg, _) -> Error msg

(* ── Task ─────────────────────────────────────────────────────── *)

type task_id = string

type task = {
  id: task_id;
  state: task_state;
  messages: task_message list;
  artifacts: artifact list;
  metadata: (string * Yojson.Safe.t) list;
  created_at: float;
  updated_at: float;
}

let create (msg : task_message) : task =
  let now = Unix.gettimeofday () in
  let id = Printf.sprintf "task_%s_%d"
    (String.sub (Digest.to_hex (Digest.string (string_of_float now))) 0 8)
    (int_of_float (now *. 1000.0) mod 100_000) in
  { id; state = Submitted; messages = [msg]; artifacts = [];
    metadata = []; created_at = now; updated_at = now }

let transition (t : task) (new_state : task_state) : (task, transition_error) result =
  if is_terminal t.state then
    Error (TaskAlreadyTerminal { state = t.state })
  else if List.mem new_state (valid_transitions t.state) then
    Ok { t with state = new_state; updated_at = Unix.gettimeofday () }
  else
    Error (InvalidTransition { from_state = t.state; to_state = new_state })

let add_message (t : task) (msg : task_message) : task =
  { t with messages = msg :: t.messages; updated_at = Unix.gettimeofday () }

let add_artifact (t : task) (art : artifact) : task =
  { t with artifacts = art :: t.artifacts; updated_at = Unix.gettimeofday () }

(* ── JSON serialization ───────────────────────────────────────── *)

let task_to_yojson t =
  `Assoc [
    ("id", `String t.id);
    ("state", task_state_to_yojson t.state);
    ("messages", `List (List.rev_map task_message_to_yojson t.messages));
    ("artifacts", `List (List.rev_map artifact_to_yojson t.artifacts));
    ("metadata", `Assoc t.metadata);
    ("created_at", `Float t.created_at);
    ("updated_at", `Float t.updated_at);
  ]

let task_of_yojson json =
  let open Yojson.Safe.Util in
  try
    let id = json |> member "id" |> to_string in
    let state_s = json |> member "state" |> to_string in
    match task_state_of_string state_s with
    | Error e -> Error e
    | Ok state ->
      let messages_json = json |> member "messages" |> to_list in
      let messages_result = List.fold_left (fun acc j ->
        match acc with
        | Error _ as e -> e
        | Ok ms ->
          match task_message_of_yojson j with
          | Ok m -> Ok (m :: ms)
          | Error e -> Error e
      ) (Ok []) messages_json in
      match messages_result with
      | Error e -> Error e
      | Ok messages ->
        (* messages stored in reverse chronological order (newest first) *)
        let artifacts_json = json |> member "artifacts" |> to_list in
        let artifacts_result = List.fold_left (fun acc j ->
          match acc with
          | Error _ as e -> e
          | Ok as_ ->
            match artifact_of_yojson j with
            | Ok a -> Ok (a :: as_)
            | Error e -> Error e
        ) (Ok []) artifacts_json in
        match artifacts_result with
        | Error e -> Error e
        | Ok artifacts ->
          (* artifacts stored in reverse chronological order (newest first) *)
          let metadata = match json |> member "metadata" with
            | `Assoc kvs -> kvs
            | _ -> []
          in
          let created_at = json |> member "created_at" |> to_float in
          let updated_at = json |> member "updated_at" |> to_float in
          Ok { id; state; messages; artifacts; metadata; created_at; updated_at }
  with Type_error (msg, _) -> Error msg

(* ── In-memory store ──────────────────────────────────────────── *)

let default_max_tasks = 10_000

type store = {
  tasks : (task_id, task) Hashtbl.t;
  max_tasks : int;
}

let create_store ?(max_tasks = default_max_tasks) () : store =
  { tasks = Hashtbl.create 64; max_tasks }

(** Evict terminal tasks (oldest updated_at first) when store is at capacity. *)
let evict_if_needed (s : store) : unit =
  if Hashtbl.length s.tasks < s.max_tasks then ()
  else
    let terminals =
      Hashtbl.fold (fun id t acc ->
        if is_terminal t.state then (id, t.updated_at) :: acc
        else acc
      ) s.tasks []
    in
    match terminals with
    | [] -> () (* all tasks are active; cannot evict *)
    | _ ->
      let sorted =
        List.sort (fun (_, a) (_, b) -> Float.compare a b) terminals
      in
      (* Remove oldest terminal tasks to free 10% capacity *)
      let to_remove = max 1 (s.max_tasks / 10) in
      let rec remove n = function
        | [] -> ()
        | _ when n <= 0 -> ()
        | (id, _) :: rest ->
          Hashtbl.remove s.tasks id;
          remove (n - 1) rest
      in
      remove to_remove sorted

let store_task (s : store) (t : task) : unit =
  evict_if_needed s;
  Hashtbl.replace s.tasks t.id t

let get_task (s : store) (id : task_id) : task option =
  Hashtbl.find_opt s.tasks id

let list_tasks (s : store) : task list =
  Hashtbl.fold (fun _ t acc -> t :: acc) s.tasks []
