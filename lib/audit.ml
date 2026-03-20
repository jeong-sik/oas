(** Audit trail — immutable log of policy decisions and agent actions.

    Records every decision point evaluation, providing accountability
    and transparency for agent behavior in a society.

    @since 0.76.0 *)

type entry = {
  id: string;
  timestamp: float;
  agent_name: string;
  action: string;
  decision_point: Policy.decision_point option;
  verdict: Policy.verdict option;
  detail: Yojson.Safe.t;
}

type t = {
  mutable entries: entry list;
  max_entries: int option;
}

let create ?max_entries () =
  { entries = []; max_entries }

let record t entry =
  t.entries <- entry :: t.entries;
  match t.max_entries with
  | None -> ()
  | Some max when List.length t.entries > max ->
    (* Keep only the newest [max] entries.
       entries are stored newest-first, so take the first [max]. *)
    t.entries <- List.filteri (fun i _ -> i < max) t.entries
  | Some _ -> ()

let query t ?agent ?action ?since () =
  t.entries
  |> List.filter (fun e ->
    (match agent with None -> true | Some a -> e.agent_name = a)
    && (match action with None -> true | Some a -> e.action = a)
    && (match since with None -> true | Some ts -> e.timestamp >= ts))

let count t = List.length t.entries

let latest t n =
  (* entries are stored newest-first, take first n *)
  List.filteri (fun i _ -> i < n) t.entries

let entry_to_json e =
  let verdict_json = match e.verdict with
    | None -> `Null
    | Some v -> `String (Policy.verdict_to_string v)
  in
  let dp_json = match e.decision_point with
    | None -> `Null
    | Some dp -> `String (Policy.decision_point_to_string dp)
  in
  `Assoc [
    ("id", `String e.id);
    ("timestamp", `Float e.timestamp);
    ("agent_name", `String e.agent_name);
    ("action", `String e.action);
    ("decision_point", dp_json);
    ("verdict", verdict_json);
    ("detail", e.detail);
  ]

let entries_to_json entries =
  `List (List.map entry_to_json entries)

let to_json t =
  entries_to_json t.entries
