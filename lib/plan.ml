(** Plan — goal decomposition and adaptive execution.

    @since 0.77.0 *)

type step_status =
  | Pending
  | Running
  | Done
  | Failed of string
  | Skipped

type step = {
  id: string;
  description: string;
  status: step_status;
  result: Yojson.Safe.t option;
  depends_on: string list;
}

type plan_status =
  | Planning
  | Executing
  | Replanning
  | Completed
  | Abandoned of string

type t = {
  goal: string;
  planner: string;
  steps: step list;
  status: plan_status;
  created_at: float;
}

let create ~goal ~planner () = {
  goal; planner;
  steps = [];
  status = Planning;
  created_at = Unix.gettimeofday ();
}

(* ── Step management ──────────────────────────────── *)

let add_step t ~id ~description ?(depends_on=[]) () =
  let step = { id; description; status = Pending; result = None; depends_on } in
  { t with steps = t.steps @ [step] }

let update_step t step_id f =
  let steps = List.map (fun s ->
    if s.id = step_id then f s else s
  ) t.steps in
  { t with steps }

let start_step t step_id =
  update_step t step_id (fun s -> { s with status = Running })

let complete_step t step_id ~result =
  update_step t step_id (fun s ->
    { s with status = Done; result = Some result })

let fail_step t step_id ~reason =
  update_step t step_id (fun s ->
    { s with status = Failed reason })

let skip_step t step_id =
  update_step t step_id (fun s -> { s with status = Skipped })

(* ── Re-planning ──────────────────────────────────── *)

let replan t ~new_steps =
  (* Keep non-Pending steps, replace Pending with new_steps *)
  let kept = List.filter (fun (s : step) ->
    match s.status with Pending -> false | _ -> true
  ) t.steps in
  { t with steps = kept @ new_steps; status = Replanning }

(* ── Lifecycle ────────────────────────────────────── *)

let start t = { t with status = Executing }
let finish t = { t with status = Completed }
let abandon t ~reason = { t with status = Abandoned reason }

(* ── Queries ──────────────────────────────────────── *)

let goal t = t.goal
let planner t = t.planner
let status t = t.status
let steps t = t.steps
let step_count t = List.length t.steps

let current_step t =
  List.find_opt (fun (s : step) ->
    match s.status with Running -> true | _ -> false
  ) t.steps
  |> function
  | Some _ as r -> r
  | None ->
    (* No running step — find first Pending *)
    List.find_opt (fun (s : step) ->
      match s.status with Pending -> true | _ -> false
    ) t.steps

let find_step t step_id =
  List.find_opt (fun s -> s.id = step_id) t.steps

let progress t =
  let total = List.length t.steps in
  if total = 0 then 1.0
  else
    let done_count = List.length (List.filter (fun (s : step) ->
      match s.status with Done | Skipped -> true | _ -> false
    ) t.steps) in
    float_of_int done_count /. float_of_int total

let is_done t =
  match t.status with Completed | Abandoned _ -> true | _ -> false

let deps_satisfied t step_id =
  match find_step t step_id with
  | None -> false
  | Some step ->
    List.for_all (fun dep_id ->
      match find_step t dep_id with
      | Some { status = Done; _ } -> true
      | _ -> false
    ) step.depends_on

(* ── Serialization ────────────────────────────────── *)

let step_status_to_string = function
  | Pending -> "Pending"
  | Running -> "Running"
  | Done -> "Done"
  | Failed reason -> Printf.sprintf "Failed(%s)" reason
  | Skipped -> "Skipped"

let plan_status_to_string = function
  | Planning -> "Planning"
  | Executing -> "Executing"
  | Replanning -> "Replanning"
  | Completed -> "Completed"
  | Abandoned reason -> Printf.sprintf "Abandoned(%s)" reason

let step_status_to_json = function
  | Pending -> `Assoc [("status", `String "Pending")]
  | Running -> `Assoc [("status", `String "Running")]
  | Done -> `Assoc [("status", `String "Done")]
  | Failed reason -> `Assoc [("status", `String "Failed"); ("reason", `String reason)]
  | Skipped -> `Assoc [("status", `String "Skipped")]

let step_status_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "status" |> to_string with
  | "Pending" -> Ok Pending
  | "Running" -> Ok Running
  | "Done" -> Ok Done
  | "Failed" -> Ok (Failed (json |> member "reason" |> to_string))
  | "Skipped" -> Ok Skipped
  | s -> Error (Printf.sprintf "unknown step_status: %s" s)

let step_to_json s =
  `Assoc [
    ("id", `String s.id);
    ("description", `String s.description);
    ("status", step_status_to_json s.status);
    ("result", (match s.result with None -> `Null | Some j -> j));
    ("depends_on", `List (List.map (fun d -> `String d) s.depends_on));
  ]

let step_of_json json =
  let open Yojson.Safe.Util in
  try
    let status_json = json |> member "status" in
    match step_status_of_json status_json with
    | Error e -> Error e
    | Ok status ->
      Ok {
        id = json |> member "id" |> to_string;
        description = json |> member "description" |> to_string;
        status;
        result = (match json |> member "result" with
          | `Null -> None | j -> Some j);
        depends_on = json |> member "depends_on" |> to_list
          |> List.map to_string;
      }
  with exn -> Error (Printexc.to_string exn)

let plan_status_to_json = function
  | Planning -> `Assoc [("status", `String "Planning")]
  | Executing -> `Assoc [("status", `String "Executing")]
  | Replanning -> `Assoc [("status", `String "Replanning")]
  | Completed -> `Assoc [("status", `String "Completed")]
  | Abandoned reason ->
    `Assoc [("status", `String "Abandoned"); ("reason", `String reason)]

let plan_status_of_json json =
  let open Yojson.Safe.Util in
  match json |> member "status" |> to_string with
  | "Planning" -> Ok Planning
  | "Executing" -> Ok Executing
  | "Replanning" -> Ok Replanning
  | "Completed" -> Ok Completed
  | "Abandoned" -> Ok (Abandoned (json |> member "reason" |> to_string))
  | s -> Error (Printf.sprintf "unknown plan_status: %s" s)

let to_json t =
  `Assoc [
    ("goal", `String t.goal);
    ("planner", `String t.planner);
    ("steps", `List (List.map step_to_json t.steps));
    ("status", plan_status_to_json t.status);
    ("created_at", `Float t.created_at);
  ]

let of_json json =
  let open Yojson.Safe.Util in
  try
    let status_json = json |> member "status" in
    match plan_status_of_json status_json with
    | Error e -> Error e
    | Ok status ->
      let steps_json = json |> member "steps" |> to_list in
      let rec collect_steps acc = function
        | [] -> Ok (List.rev acc)
        | j :: rest ->
          match step_of_json j with
          | Ok s -> collect_steps (s :: acc) rest
          | Error e -> Error e
      in
      (match collect_steps [] steps_json with
       | Error e -> Error e
       | Ok steps ->
         Ok {
           goal = json |> member "goal" |> to_string;
           planner = json |> member "planner" |> to_string;
           steps;
           status;
           created_at = json |> member "created_at" |> to_float;
         })
  with exn -> Error (Printexc.to_string exn)
