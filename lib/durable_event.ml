(** Event-sourced agent loop journal.

    Append-only event log for agent loop actions.
    Enables crash recovery via journal replay.

    @since 0.89.0 *)

(* ── Event types ──────────────────────────────────── *)

type event =
  | Turn_started of { turn: int; timestamp: float }
  | Llm_request of {
      turn: int;
      model: string;
      input_tokens: int;
      timestamp: float;
    }
  | Llm_response of {
      turn: int;
      output_tokens: int;
      stop_reason: string;
      duration_ms: float;
      timestamp: float;
    }
  | Tool_called of {
      turn: int;
      tool_name: string;
      idempotency_key: string;
      input_hash: string;
      timestamp: float;
    }
  | Tool_completed of {
      turn: int;
      tool_name: string;
      idempotency_key: string;
      output_json: Yojson.Safe.t;
      is_error: bool;
      duration_ms: float;
      timestamp: float;
    }
  | State_transition of {
      from_state: string;
      to_state: string;
      reason: string;
      timestamp: float;
    }
  | Checkpoint_saved of {
      checkpoint_id: string;
      timestamp: float;
    }
  | Error_occurred of {
      turn: int;
      error_domain: string;
      detail: string;
      timestamp: float;
    }

(* ── Journal ──────────────────────────────────────── *)

type journal = {
  mutable entries: event list;  (** Stored in reverse chronological order *)
  mutable size: int;
  on_append: (event -> unit) option;
    (** Optional fan-out callback invoked after every append.
        Used to project journal events onto Event_bus or other sinks. *)
}

let create ?on_append () = { entries = []; size = 0; on_append }

let append journal event =
  journal.entries <- event :: journal.entries;
  journal.size <- journal.size + 1;
  Option.iter (fun f -> f event) journal.on_append

let events journal = List.rev journal.entries

let length journal = journal.size

(* ── Idempotency ──────────────────────────────────── *)

(** Stable idempotency key using FNV-1a hash for better distribution.
    Not cryptographic, but sufficient for deduplication within a single
    journal. Collisions are theoretically possible but practically
    unlikely for distinct tool inputs. *)
let fnv1a_hash (s : string) : int =
  let basis = 0x811c9dc5 in
  let prime = 0x01000193 in
  String.fold_left (fun h c ->
    (h lxor Char.code c) * prime
  ) basis s
  land max_int  (* ensure positive, 63-bit on 64-bit OCaml *)

let make_idempotency_key ~tool_name ~input =
  let input_str = Yojson.Safe.to_string input in
  let hash = fnv1a_hash (tool_name ^ ":" ^ input_str) in
  Printf.sprintf "%s:%08x" tool_name hash

let find_completed_activity journal key =
  List.find_map (fun event ->
    match event with
    | Tool_completed { idempotency_key; output_json; _ }
      when idempotency_key = key -> Some output_json
    | _ -> None
  ) journal.entries  (* entries is reversed, so finds most recent first *)

(* ── Replay ───────────────────────────────────────── *)

type replay_summary = {
  last_turn: int;
  completed_tools: (string * Yojson.Safe.t) list;
  last_state: string;
  total_input_tokens: int;
  total_output_tokens: int;
  error_count: int;
}

(* Fold over entries directly (reverse chronological) — avoids List.rev allocation *)
let replay_summary journal =
  let acc =
    List.fold_left (fun (lt, ct, ls, it, ot, ec) event ->
      match event with
      | Turn_started { turn; _ } ->
        (max lt turn, ct, ls, it, ot, ec)
      | Llm_request { input_tokens = n; _ } ->
        (lt, ct, ls, it + n, ot, ec)
      | Llm_response { output_tokens = n; _ } ->
        (lt, ct, ls, it, ot + n, ec)
      | Tool_completed { idempotency_key; output_json; _ } ->
        (lt, (idempotency_key, output_json) :: ct, ls, it, ot, ec)
      | State_transition { to_state; _ } ->
        (lt, ct, to_state, it, ot, ec)
      | Error_occurred _ ->
        (lt, ct, ls, it, ot, ec + 1)
      | Tool_called _ | Checkpoint_saved _ ->
        (lt, ct, ls, it, ot, ec)
    ) (0, [], "unknown", 0, 0, 0) journal.entries
  in
  let (last_turn, completed_tools_rev, last_state,
       total_input_tokens, total_output_tokens, error_count) = acc in
  {
    last_turn;
    completed_tools = List.rev completed_tools_rev;
    last_state;
    total_input_tokens;
    total_output_tokens;
    error_count;
  }

(* ── Queries ──────────────────────────────────────── *)

let events_for_turn journal turn =
  List.filter (fun event ->
    match event with
    | Turn_started { turn = t; _ }
    | Llm_request { turn = t; _ }
    | Llm_response { turn = t; _ }
    | Tool_called { turn = t; _ }
    | Tool_completed { turn = t; _ }
    | Error_occurred { turn = t; _ } -> t = turn
    | State_transition _ | Checkpoint_saved _ -> false
  ) (events journal)

let last_timestamp journal =
  match journal.entries with
  | [] -> None
  | first :: _ ->
    let ts = match first with
      | Turn_started { timestamp; _ }
      | Llm_request { timestamp; _ }
      | Llm_response { timestamp; _ }
      | Tool_called { timestamp; _ }
      | Tool_completed { timestamp; _ }
      | State_transition { timestamp; _ }
      | Checkpoint_saved { timestamp; _ }
      | Error_occurred { timestamp; _ } -> timestamp
    in
    Some ts

let tool_completions journal =
  List.filter_map (fun event ->
    match event with
    | Tool_completed { idempotency_key; output_json; is_error; _ } ->
      Some (idempotency_key, output_json, is_error)
    | _ -> None
  ) (events journal)

(* ── Serialization ────────────────────────────────── *)

let event_to_json = function
  | Turn_started { turn; timestamp } ->
    `Assoc [("type", `String "turn_started");
            ("turn", `Int turn); ("timestamp", `Float timestamp)]
  | Llm_request { turn; model; input_tokens; timestamp } ->
    `Assoc [("type", `String "llm_request");
            ("turn", `Int turn); ("model", `String model);
            ("input_tokens", `Int input_tokens);
            ("timestamp", `Float timestamp)]
  | Llm_response { turn; output_tokens; stop_reason; duration_ms; timestamp } ->
    `Assoc [("type", `String "llm_response");
            ("turn", `Int turn); ("output_tokens", `Int output_tokens);
            ("stop_reason", `String stop_reason);
            ("duration_ms", `Float duration_ms);
            ("timestamp", `Float timestamp)]
  | Tool_called { turn; tool_name; idempotency_key; input_hash; timestamp } ->
    `Assoc [("type", `String "tool_called");
            ("turn", `Int turn); ("tool_name", `String tool_name);
            ("idempotency_key", `String idempotency_key);
            ("input_hash", `String input_hash);
            ("timestamp", `Float timestamp)]
  | Tool_completed { turn; tool_name; idempotency_key; output_json;
                     is_error; duration_ms; timestamp } ->
    `Assoc [("type", `String "tool_completed");
            ("turn", `Int turn); ("tool_name", `String tool_name);
            ("idempotency_key", `String idempotency_key);
            ("output_json", output_json); ("is_error", `Bool is_error);
            ("duration_ms", `Float duration_ms);
            ("timestamp", `Float timestamp)]
  | State_transition { from_state; to_state; reason; timestamp } ->
    `Assoc [("type", `String "state_transition");
            ("from_state", `String from_state);
            ("to_state", `String to_state);
            ("reason", `String reason);
            ("timestamp", `Float timestamp)]
  | Checkpoint_saved { checkpoint_id; timestamp } ->
    `Assoc [("type", `String "checkpoint_saved");
            ("checkpoint_id", `String checkpoint_id);
            ("timestamp", `Float timestamp)]
  | Error_occurred { turn; error_domain; detail; timestamp } ->
    `Assoc [("type", `String "error_occurred");
            ("turn", `Int turn); ("error_domain", `String error_domain);
            ("detail", `String detail); ("timestamp", `Float timestamp)]

let event_of_json json =
  let open Yojson.Safe.Util in
  try
    let typ = json |> member "type" |> to_string in
    match typ with
    | "turn_started" ->
      Ok (Turn_started {
        turn = json |> member "turn" |> to_int;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "llm_request" ->
      Ok (Llm_request {
        turn = json |> member "turn" |> to_int;
        model = json |> member "model" |> to_string;
        input_tokens = json |> member "input_tokens" |> to_int;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "llm_response" ->
      Ok (Llm_response {
        turn = json |> member "turn" |> to_int;
        output_tokens = json |> member "output_tokens" |> to_int;
        stop_reason = json |> member "stop_reason" |> to_string;
        duration_ms = json |> member "duration_ms" |> to_float;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "tool_called" ->
      Ok (Tool_called {
        turn = json |> member "turn" |> to_int;
        tool_name = json |> member "tool_name" |> to_string;
        idempotency_key = json |> member "idempotency_key" |> to_string;
        input_hash = json |> member "input_hash" |> to_string;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "tool_completed" ->
      Ok (Tool_completed {
        turn = json |> member "turn" |> to_int;
        tool_name = json |> member "tool_name" |> to_string;
        idempotency_key = json |> member "idempotency_key" |> to_string;
        output_json = json |> member "output_json";
        is_error = json |> member "is_error" |> to_bool;
        duration_ms = json |> member "duration_ms" |> to_float;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "state_transition" ->
      Ok (State_transition {
        from_state = json |> member "from_state" |> to_string;
        to_state = json |> member "to_state" |> to_string;
        reason = json |> member "reason" |> to_string;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "checkpoint_saved" ->
      Ok (Checkpoint_saved {
        checkpoint_id = json |> member "checkpoint_id" |> to_string;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | "error_occurred" ->
      Ok (Error_occurred {
        turn = json |> member "turn" |> to_int;
        error_domain = json |> member "error_domain" |> to_string;
        detail = json |> member "detail" |> to_string;
        timestamp = json |> member "timestamp" |> to_float;
      })
    | unknown -> Error (Printf.sprintf "unknown event type: %s" unknown)
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg

let journal_to_json journal =
  `List (List.map event_to_json (events journal))

let journal_of_json json =
  let open Yojson.Safe.Util in
  try
    let items = to_list json in
    (* acc accumulates in reverse — matches journal.entries internal format *)
    let rec parse acc count = function
      | [] ->
        Ok { entries = acc; size = count; on_append = None }
      | item :: rest ->
        match event_of_json item with
        | Ok evt -> parse (evt :: acc) (count + 1) rest
        | Error e -> Error e
    in
    parse [] 0 items
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
