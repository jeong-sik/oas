(** Swarm state checkpoint — save and restore convergence loop progress.

    Closures (agent_entry.run) are not serializable, so we save entry names
    and rebind via [agent_lookup] on restore.

    @since 0.43.0 *)

open Agent_sdk

(* ── Checkpoint type ─────────────────────────────────────────── *)

type config_snapshot = {
  entry_names: string list;
  mode: Swarm_types.orchestration_mode;
  max_parallel: int;
  prompt: string;
  timeout_sec: float option;
  convergence_target: float option;
  convergence_max_iterations: int option;
  convergence_patience: int option;
}

type t = {
  version: int;
  config_snapshot: config_snapshot;
  iteration: int;
  best_metric: float option;
  best_iteration: int;
  patience_counter: int;
  history: Swarm_types.iteration_record list;
  created_at: float;
}

let checkpoint_version = 1

(* ── Snapshot helpers ────────────────────────────────────────── *)

let snapshot_of_config (cfg : Swarm_types.swarm_config) : config_snapshot =
  let conv_target, conv_max, conv_patience =
    match cfg.convergence with
    | None -> (None, None, None)
    | Some c -> (Some c.target, Some c.max_iterations, Some c.patience)
  in
  { entry_names = List.map (fun (e : Swarm_types.agent_entry) -> e.name) cfg.entries;
    mode = cfg.mode;
    max_parallel = cfg.max_parallel;
    prompt = cfg.prompt;
    timeout_sec = cfg.timeout_sec;
    convergence_target = conv_target;
    convergence_max_iterations = conv_max;
    convergence_patience = conv_patience;
  }

(* ── Save ────────────────────────────────────────────────────── *)

let of_state (state : Swarm_types.swarm_state) : t =
  { version = checkpoint_version;
    config_snapshot = snapshot_of_config state.config;
    iteration = state.current_iteration;
    best_metric = state.best_metric;
    best_iteration = state.best_iteration;
    patience_counter = state.patience_counter;
    history = state.history;
    created_at = Unix.gettimeofday ();
  }

(* ── JSON serialization ──────────────────────────────────────── *)

let iteration_record_to_json (r : Swarm_types.iteration_record) : Yojson.Safe.t =
  let agent_results = List.map (fun (name, status) ->
    `Assoc [
      ("name", `String name);
      ("status", `String (Swarm_types.show_agent_status status));
    ]
  ) r.agent_results in
  `Assoc [
    ("iteration", `Int r.iteration);
    ("metric_value", match r.metric_value with Some v -> `Float v | None -> `Null);
    ("agent_results", `List agent_results);
    ("elapsed", `Float r.elapsed);
    ("timestamp", `Float r.timestamp);
  ]

(** Reconstruct an iteration_record from JSON.
    agent_results statuses are restored as Idle (the original status
    text is preserved in JSON but parsing show-format back is fragile).
    Core fields (iteration, metric_value, elapsed, timestamp) are exact. *)
let iteration_record_of_json (json : Yojson.Safe.t) : Swarm_types.iteration_record =
  let open Yojson.Safe.Util in
  let agent_results =
    json |> member "agent_results" |> to_list
    |> List.map (fun ar ->
      let name = ar |> member "name" |> to_string in
      (name, Swarm_types.Idle))
  in
  { iteration = json |> member "iteration" |> to_int;
    metric_value = json |> member "metric_value" |> to_float_option;
    agent_results;
    elapsed = json |> member "elapsed" |> to_float;
    timestamp = json |> member "timestamp" |> to_float;
    trace_refs = [];
  }

let config_snapshot_to_json (s : config_snapshot) : Yojson.Safe.t =
  `Assoc [
    ("entry_names", `List (List.map (fun n -> `String n) s.entry_names));
    ("mode", `String (Swarm_types.show_orchestration_mode s.mode));
    ("max_parallel", `Int s.max_parallel);
    ("prompt", `String s.prompt);
    ("timeout_sec", match s.timeout_sec with Some v -> `Float v | None -> `Null);
    ("convergence_target", match s.convergence_target with Some v -> `Float v | None -> `Null);
    ("convergence_max_iterations", match s.convergence_max_iterations with Some v -> `Int v | None -> `Null);
    ("convergence_patience", match s.convergence_patience with Some v -> `Int v | None -> `Null);
  ]

let to_json (cp : t) : Yojson.Safe.t =
  `Assoc [
    ("version", `Int cp.version);
    ("config_snapshot", config_snapshot_to_json cp.config_snapshot);
    ("iteration", `Int cp.iteration);
    ("best_metric", match cp.best_metric with Some v -> `Float v | None -> `Null);
    ("best_iteration", `Int cp.best_iteration);
    ("patience_counter", `Int cp.patience_counter);
    ("history", `List (List.map iteration_record_to_json cp.history));
    ("created_at", `Float cp.created_at);
  ]

let save (cp : t) ~path =
  let json = to_json cp in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () ->
      output_string oc (Yojson.Safe.pretty_to_string json);
      output_char oc '\n')

(* ── Load ────────────────────────────────────────────────────── *)

let load ~path : (t, Error.sdk_error) result =
  if not (Sys.file_exists path) then
    Error (Error.Io (FileOpFailed { op = "read"; path; detail = "file not found" }))
  else
    try
      let ic = open_in path in
      let content = Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () -> In_channel.input_all ic) in
      let json = Yojson.Safe.from_string content in
      let open Yojson.Safe.Util in
      let version = json |> member "version" |> to_int in
      if version <> checkpoint_version then
        Error (Error.Serialization (VersionMismatch { expected = checkpoint_version; got = version }))
      else
        let cs = json |> member "config_snapshot" in
        let config_snapshot = {
          entry_names = cs |> member "entry_names" |> to_list |> List.map to_string;
          mode = (match cs |> member "mode" |> to_string with
                  | "Decentralized" -> Decentralized
                  | "Supervisor" -> Supervisor
                  | _ -> Pipeline_mode);
          max_parallel = cs |> member "max_parallel" |> to_int;
          prompt = cs |> member "prompt" |> to_string;
          timeout_sec = cs |> member "timeout_sec" |> to_float_option;
          convergence_target = cs |> member "convergence_target" |> to_float_option;
          convergence_max_iterations = cs |> member "convergence_max_iterations" |> to_int_option;
          convergence_patience = cs |> member "convergence_patience" |> to_int_option;
        } in
        Ok {
          version;
          config_snapshot;
          iteration = json |> member "iteration" |> to_int;
          best_metric = json |> member "best_metric" |> to_float_option;
          best_iteration = json |> member "best_iteration" |> to_int;
          patience_counter = json |> member "patience_counter" |> to_int;
          history =
            json |> member "history" |> to_list
            |> List.map iteration_record_of_json;
          created_at = json |> member "created_at" |> to_float;
        }
    with
    | Yojson.Json_error msg ->
      Error (Error.Serialization (JsonParseError { detail = msg }))
    | exn ->
      Error (Error.Io (FileOpFailed { op = "read"; path; detail = Printexc.to_string exn }))

(* ── Restore ─────────────────────────────────────────────────── *)

let restore (cp : t) ~agent_lookup
    ~(base_config : Swarm_types.swarm_config)
    : (Swarm_types.swarm_state, Error.sdk_error) result =
  let missing = List.filter (fun name ->
    match agent_lookup name with Some _ -> false | None -> true
  ) cp.config_snapshot.entry_names in
  match missing with
  | _ :: _ ->
    let detail = Printf.sprintf "missing agents: %s" (String.concat ", " missing) in
    Error (Error.Config (InvalidConfig { field = "agent_lookup"; detail }))
  | [] ->
    let state = Swarm_types.create_state base_config in
    state.current_iteration <- cp.iteration;
    state.best_metric <- cp.best_metric;
    state.best_iteration <- cp.best_iteration;
    state.patience_counter <- cp.patience_counter;
    state.history <- cp.history;
    Ok state
