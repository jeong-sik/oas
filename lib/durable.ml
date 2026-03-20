(** Durable state machine — typed step chains with execution journal.

    Enables long-running agent workflows to survive process crashes
    and resume from the exact point of interruption.

    @since 0.76.0 *)

type journal_entry = {
  step_name: string;
  started_at: float;
  completed_at: float option;
  input_json: Yojson.Safe.t;
  output_json: Yojson.Safe.t option;
  error: string option;
  attempt: int;
}

type execution_state =
  | NotStarted
  | InProgress of { current_step: string; attempt: int; journal: journal_entry list }
  | Suspended of { at_step: string; journal: journal_entry list; reason: string }
  | Completed of { journal: journal_entry list; final_output: Yojson.Safe.t }
  | Failed of { at_step: string; journal: journal_entry list; error: string }

type step = {
  name: string;
  execute: Yojson.Safe.t -> (Yojson.Safe.t, string) result;
  retry_limit: int;
}

type t = {
  name: string;
  steps: step list;
}

let create ~name () = { name; steps = [] }

(* Steps are stored in reverse insertion order; reversed at execution time. *)
let add_step t step = { t with steps = step :: t.steps }

let name t = t.name
let step_count t = List.length t.steps
let step_names t = List.rev_map (fun (s : step) -> s.name) t.steps

let is_terminal = function
  | Completed _ | Failed _ -> true
  | NotStarted | InProgress _ | Suspended _ -> false

(** Run steps starting from index [start_idx] with given [input]
    and accumulated [journal]. *)
let run_from t ~start_idx ~input ~journal =
  let steps = Array.of_list (List.rev t.steps) in
  let total = Array.length steps in
  let rec loop idx cur_input acc_journal =
    if idx >= total then
      Completed { journal = List.rev acc_journal; final_output = cur_input }
    else begin
      let step = steps.(idx) in
      let rec try_step attempt =
        let started_at = Unix.gettimeofday () in
        match step.execute cur_input with
        | Ok output ->
          let completed_at = Unix.gettimeofday () in
          let je = {
            step_name = step.name;
            started_at;
            completed_at = Some completed_at;
            input_json = cur_input;
            output_json = Some output;
            error = None;
            attempt;
          } in
          loop (idx + 1) output (je :: acc_journal)
        | Error err ->
          let completed_at = Unix.gettimeofday () in
          let je = {
            step_name = step.name;
            started_at;
            completed_at = Some completed_at;
            input_json = cur_input;
            output_json = None;
            error = Some err;
            attempt;
          } in
          if attempt < step.retry_limit then
            try_step (attempt + 1)
          else
            Failed {
              at_step = step.name;
              journal = List.rev (je :: acc_journal);
              error = err;
            }
      in
      try_step 1
    end
  in
  loop start_idx input journal

let execute t input =
  run_from t ~start_idx:0 ~input ~journal:[]

let resume t state =
  match state with
  | Suspended { at_step; journal; _ } | Failed { at_step; journal; _ } ->
    (* Find the step index for at_step *)
    let steps = List.rev t.steps in
    let rec find_idx idx = function
      | [] -> None
      | (s : step) :: rest ->
        if s.name = at_step then Some idx
        else find_idx (idx + 1) rest
    in
    (match find_idx 0 steps with
     | None ->
       Failed {
         at_step;
         journal;
         error = Printf.sprintf "step '%s' not found in pipeline" at_step;
       }
     | Some idx ->
       (* Determine input: use the last successful step's output,
          or if resuming at first step, look at the journal's first entry input *)
       let input =
         match journal with
         | [] -> `Null
         | _ ->
           (* Find the last entry before at_step that has output.
              journal is newest-first; scan for the failed step or
              the most recent predecessor with output. *)
           let rec find_last_output = function
             | [] -> `Null
             | e :: rest ->
               if e.step_name = at_step then
                 e.input_json
               else
                 (match e.output_json with
                  | Some o -> o
                  | None -> find_last_output rest)
           in
           find_last_output journal
       in
       (* Keep journal entries before the current step *)
       let prior_journal =
         List.filter (fun e -> e.step_name <> at_step) journal
       in
       run_from t ~start_idx:idx ~input ~journal:(List.rev prior_journal))
  | NotStarted -> execute t `Null
  | InProgress _ -> state  (* already running *)
  | Completed _ -> state   (* already done *)

let suspend _t state ~reason =
  match state with
  | InProgress { current_step; journal; _ } ->
    Suspended { at_step = current_step; journal; reason }
  | _ -> state

(* ── Serialization ────────────────────────────────── *)

let journal_entry_to_json je =
  `Assoc [
    ("step_name", `String je.step_name);
    ("started_at", `Float je.started_at);
    ("completed_at",
     (match je.completed_at with None -> `Null | Some f -> `Float f));
    ("input_json", je.input_json);
    ("output_json",
     (match je.output_json with None -> `Null | Some j -> j));
    ("error",
     (match je.error with None -> `Null | Some e -> `String e));
    ("attempt", `Int je.attempt);
  ]

let journal_entry_of_json json =
  let open Yojson.Safe.Util in
  try
    let step_name = json |> member "step_name" |> to_string in
    let started_at = json |> member "started_at" |> to_float in
    let completed_at =
      match json |> member "completed_at" with
      | `Null -> None | j -> Some (to_float j) in
    let input_json = json |> member "input_json" in
    let output_json =
      match json |> member "output_json" with
      | `Null -> None | j -> Some j in
    let error =
      match json |> member "error" with
      | `Null -> None | j -> Some (to_string j) in
    let attempt = json |> member "attempt" |> to_int in
    Ok { step_name; started_at; completed_at; input_json; output_json;
         error; attempt }
  with exn -> Error (Printexc.to_string exn)

let journal_list_to_json journal =
  `List (List.map journal_entry_to_json journal)

let journal_list_of_json json =
  let open Yojson.Safe.Util in
  Util.result_traverse ~f:journal_entry_of_json (to_list json)

let execution_state_to_json = function
  | NotStarted ->
    `Assoc [("state", `String "NotStarted")]
  | InProgress { current_step; attempt; journal } ->
    `Assoc [
      ("state", `String "InProgress");
      ("current_step", `String current_step);
      ("attempt", `Int attempt);
      ("journal", journal_list_to_json journal);
    ]
  | Suspended { at_step; journal; reason } ->
    `Assoc [
      ("state", `String "Suspended");
      ("at_step", `String at_step);
      ("journal", journal_list_to_json journal);
      ("reason", `String reason);
    ]
  | Completed { journal; final_output } ->
    `Assoc [
      ("state", `String "Completed");
      ("journal", journal_list_to_json journal);
      ("final_output", final_output);
    ]
  | Failed { at_step; journal; error } ->
    `Assoc [
      ("state", `String "Failed");
      ("at_step", `String at_step);
      ("journal", journal_list_to_json journal);
      ("error", `String error);
    ]

let execution_state_of_json json =
  let open Yojson.Safe.Util in
  try
    let state = json |> member "state" |> to_string in
    match state with
    | "NotStarted" -> Ok NotStarted
    | "InProgress" ->
      let current_step = json |> member "current_step" |> to_string in
      let attempt = json |> member "attempt" |> to_int in
      (match journal_list_of_json (json |> member "journal") with
       | Ok journal -> Ok (InProgress { current_step; attempt; journal })
       | Error e -> Error e)
    | "Suspended" ->
      let at_step = json |> member "at_step" |> to_string in
      let reason = json |> member "reason" |> to_string in
      (match journal_list_of_json (json |> member "journal") with
       | Ok journal -> Ok (Suspended { at_step; journal; reason })
       | Error e -> Error e)
    | "Completed" ->
      let final_output = json |> member "final_output" in
      (match journal_list_of_json (json |> member "journal") with
       | Ok journal -> Ok (Completed { journal; final_output })
       | Error e -> Error e)
    | "Failed" ->
      let at_step = json |> member "at_step" |> to_string in
      let error = json |> member "error" |> to_string in
      (match journal_list_of_json (json |> member "journal") with
       | Ok journal -> Ok (Failed { at_step; journal; error })
       | Error e -> Error e)
    | unknown -> Error (Printf.sprintf "unknown execution state: %s" unknown)
  with exn -> Error (Printexc.to_string exn)
