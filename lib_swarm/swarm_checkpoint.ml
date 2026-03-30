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

let checkpoint_version = 2

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
      ("status", Swarm_types.agent_status_to_yojson status);
    ]
  ) r.agent_results in
  let trace_refs = `List (List.map Raw_trace.run_ref_to_yojson r.trace_refs) in
  `Assoc [
    ("iteration", `Int r.iteration);
    ("metric_value", match r.metric_value with Some v -> `Float v | None -> `Null);
    ("agent_results", `List agent_results);
    ("elapsed", `Float r.elapsed);
    ("timestamp", `Float r.timestamp);
    ("trace_refs", trace_refs);
  ]

(** Reconstruct an iteration_record from JSON with full fidelity.
    Version 2: All fields are preserved including agent_status variants and trace_refs.
    Version 1: Falls back to Idle status and empty trace_refs for backward compatibility. *)
let iteration_record_of_json ~version (json : Yojson.Safe.t) : Swarm_types.iteration_record =
  let open Yojson.Safe.Util in
  let agent_results =
    json |> member "agent_results" |> to_list
    |> List.map (fun ar ->
      let name = ar |> member "name" |> to_string in
      let status =
        if version >= 2 then
          match Swarm_types.agent_status_of_yojson (ar |> member "status") with
          | Ok s -> s
          | Error _ -> Swarm_types.Idle  (* Fallback on parse error *)
        else
          Swarm_types.Idle  (* Version 1: always Idle *)
      in
      (name, status))
  in
  let trace_refs =
    if version >= 2 then
      try
        json |> member "trace_refs" |> to_list
        |> List.filter_map (fun tr ->
          match Raw_trace.run_ref_of_yojson tr with
          | Ok ref -> Some ref
          | Error _ -> None)
      with _ -> []
    else
      []  (* Version 1: empty trace_refs *)
  in
  { iteration = json |> member "iteration" |> to_int;
    metric_value = json |> member "metric_value" |> to_float_option;
    agent_results;
    elapsed = json |> member "elapsed" |> to_float;
    timestamp = json |> member "timestamp" |> to_float;
    trace_refs;
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
      (* Support both version 1 and 2 *)
      if version < 1 || version > checkpoint_version then
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
            |> List.map (iteration_record_of_json ~version);
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

[@@@coverage off]
(* === Inline tests === *)

(* --- helpers --- *)

let mock_run _text ~sw:_ _prompt =
  Ok { Types.id = "m"; model = "m"; stop_reason = Types.EndTurn;
       content = [Types.Text "ok"]; usage = None }

let make_entry name =
  { Swarm_types.name; run = mock_run "ok"; role = Swarm_types.Execute;
    get_telemetry = None; extensions = [] }

let make_config ?(entries=[make_entry "a1"; make_entry "a2"])
    ?(mode=Swarm_types.Pipeline_mode)
    ?(prompt="test prompt")
    ?(max_parallel=2)
    ?(convergence=None)
    ?(timeout_sec=None)
    () : Swarm_types.swarm_config =
  { entries; mode; convergence; max_parallel; prompt; timeout_sec;
    budget = Swarm_types.no_budget; max_agent_retries = 0;
    collaboration = None; resource_check = None; max_concurrent_agents = None;
    enable_streaming = false }

let make_iteration ~iteration ?(metric_value=None) ?(agent_results=[])
    ?(elapsed=1.0) ?(timestamp=100.0) () : Swarm_types.iteration_record =
  { iteration; metric_value; agent_results; elapsed; timestamp; trace_refs = [] }

(* --- checkpoint_version --- *)

let%test "checkpoint_version is 2" =
  checkpoint_version = 2

(* --- snapshot_of_config --- *)

let%test "snapshot_of_config: entry names extracted" =
  let cfg = make_config () in
  let snap = snapshot_of_config cfg in
  snap.entry_names = ["a1"; "a2"]

let%test "snapshot_of_config: mode preserved" =
  let cfg = make_config ~mode:Swarm_types.Decentralized () in
  let snap = snapshot_of_config cfg in
  snap.mode = Swarm_types.Decentralized

let%test "snapshot_of_config: prompt preserved" =
  let cfg = make_config ~prompt:"my prompt" () in
  let snap = snapshot_of_config cfg in
  snap.prompt = "my prompt"

let%test "snapshot_of_config: max_parallel preserved" =
  let cfg = make_config ~max_parallel:8 () in
  let snap = snapshot_of_config cfg in
  snap.max_parallel = 8

let%test "snapshot_of_config: no convergence" =
  let cfg = make_config ~convergence:None () in
  let snap = snapshot_of_config cfg in
  snap.convergence_target = None
  && snap.convergence_max_iterations = None
  && snap.convergence_patience = None

let%test "snapshot_of_config: with convergence" =
  let conv = Some {
    Swarm_types.metric = Swarm_types.Callback (fun () -> 0.9);
    target = 0.95; max_iterations = 10; patience = 3;
    aggregate = Swarm_types.Best_score } in
  let cfg = make_config ~convergence:conv () in
  let snap = snapshot_of_config cfg in
  snap.convergence_target = Some 0.95
  && snap.convergence_max_iterations = Some 10
  && snap.convergence_patience = Some 3

let%test "snapshot_of_config: timeout_sec" =
  let cfg = make_config ~timeout_sec:(Some 30.0) () in
  let snap = snapshot_of_config cfg in
  snap.timeout_sec = Some 30.0

(* --- of_state --- *)

let%test "of_state: version is checkpoint_version" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  let cp = of_state state in
  cp.version = checkpoint_version

let%test "of_state: iteration from state" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.current_iteration <- 5;
  let cp = of_state state in
  cp.iteration = 5

let%test "of_state: best_metric from state" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.best_metric <- Some 0.85;
  let cp = of_state state in
  cp.best_metric = Some 0.85

let%test "of_state: created_at is positive" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  let cp = of_state state in
  cp.created_at > 0.0

(* --- iteration_record JSON roundtrip --- *)

let%test "iteration_record_to_json then of_json: core fields preserved" =
  let rec_ = make_iteration ~iteration:3 ~metric_value:(Some 0.75)
    ~agent_results:[("a1", Swarm_types.Idle); ("a2", Swarm_types.Idle)]
    ~elapsed:2.5 ~timestamp:12345.0 () in
  let json = iteration_record_to_json rec_ in
  let restored = iteration_record_of_json ~version:checkpoint_version json in
  restored.iteration = 3
  && restored.metric_value = Some 0.75
  && restored.elapsed = 2.5
  && restored.timestamp = 12345.0
  && List.length restored.agent_results = 2

let%test "iteration_record_to_json: None metric_value" =
  let rec_ = make_iteration ~iteration:1 ~metric_value:None () in
  let json = iteration_record_to_json rec_ in
  let restored = iteration_record_of_json ~version:checkpoint_version json in
  restored.metric_value = None

(* --- config_snapshot JSON roundtrip --- *)

let%test "config_snapshot_to_json preserves fields" =
  let snap = {
    entry_names = ["x"; "y"];
    mode = Swarm_types.Supervisor;
    max_parallel = 4;
    prompt = "hello";
    timeout_sec = Some 10.0;
    convergence_target = Some 0.9;
    convergence_max_iterations = Some 20;
    convergence_patience = Some 5;
  } in
  let json = config_snapshot_to_json snap in
  let open Yojson.Safe.Util in
  let names = json |> member "entry_names" |> to_list |> List.map to_string in
  names = ["x"; "y"]
  && (json |> member "max_parallel" |> to_int) = 4
  && (json |> member "prompt" |> to_string) = "hello"

(* --- to_json / full checkpoint roundtrip via JSON --- *)

let%test "to_json preserves all top-level fields" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.current_iteration <- 7;
  state.best_metric <- Some 0.88;
  state.best_iteration <- 5;
  state.patience_counter <- 2;
  let cp = of_state state in
  let json = to_json cp in
  let open Yojson.Safe.Util in
  (json |> member "version" |> to_int) = 1
  && (json |> member "iteration" |> to_int) = 7
  && (json |> member "best_iteration" |> to_int) = 5
  && (json |> member "patience_counter" |> to_int) = 2

let%test "to_json: history serialized" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.history <- [
    make_iteration ~iteration:1 ~metric_value:(Some 0.5) ();
    make_iteration ~iteration:2 ~metric_value:(Some 0.7) ();
  ];
  let cp = of_state state in
  let json = to_json cp in
  let open Yojson.Safe.Util in
  let hist = json |> member "history" |> to_list in
  List.length hist = 2

(* --- load: error cases (no file I/O, testing in-memory parsing via from_string) --- *)

let%test "load: nonexistent file returns error" =
  match load ~path:"/nonexistent/path/checkpoint.json" with
  | Error (Error.Io (FileOpFailed { op = "read"; _ })) -> true
  | _ -> false

(* --- restore --- *)

let%test "restore: all agents found succeeds" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.current_iteration <- 3;
  state.best_metric <- Some 0.9;
  state.patience_counter <- 1;
  let cp = of_state state in
  let lookup name =
    if name = "a1" || name = "a2" then Some (make_entry name) else None in
  match restore cp ~agent_lookup:lookup ~base_config:cfg with
  | Ok restored ->
      restored.current_iteration = 3
      && restored.best_metric = Some 0.9
      && restored.patience_counter = 1
  | Error _ -> false

let%test "restore: missing agent returns error" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  let cp = of_state state in
  let lookup _name = None in
  match restore cp ~agent_lookup:lookup ~base_config:cfg with
  | Error (Error.Config (InvalidConfig { field = "agent_lookup"; _ })) -> true
  | _ -> false

let%test "restore: partial missing agents returns error with detail" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  let cp = of_state state in
  let lookup name =
    if name = "a1" then Some (make_entry name) else None in
  match restore cp ~agent_lookup:lookup ~base_config:cfg with
  | Error (Error.Config (InvalidConfig { detail; _ })) ->
      Util.string_contains ~needle:"a2" detail
  | _ -> false

let%test "restore: history preserved" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.history <- [make_iteration ~iteration:1 ~metric_value:(Some 0.5) ()];
  let cp = of_state state in
  let lookup name =
    if name = "a1" || name = "a2" then Some (make_entry name) else None in
  match restore cp ~agent_lookup:lookup ~base_config:cfg with
  | Ok restored -> List.length restored.history = 1
  | Error _ -> false

(* --- Additional checkpoint tests --- *)

let%test "config_snapshot_to_json None convergence fields" =
  let snap = {
    entry_names = ["x"];
    mode = Swarm_types.Pipeline_mode;
    max_parallel = 1;
    prompt = "p";
    timeout_sec = None;
    convergence_target = None;
    convergence_max_iterations = None;
    convergence_patience = None;
  } in
  let json = config_snapshot_to_json snap in
  let open Yojson.Safe.Util in
  json |> member "timeout_sec" = `Null
  && json |> member "convergence_target" = `Null
  && json |> member "convergence_max_iterations" = `Null
  && json |> member "convergence_patience" = `Null

let%test "config_snapshot_to_json Some convergence fields" =
  let snap = {
    entry_names = [];
    mode = Swarm_types.Supervisor;
    max_parallel = 4;
    prompt = "go";
    timeout_sec = Some 60.0;
    convergence_target = Some 0.95;
    convergence_max_iterations = Some 100;
    convergence_patience = Some 5;
  } in
  let json = config_snapshot_to_json snap in
  let open Yojson.Safe.Util in
  json |> member "timeout_sec" |> to_float = 60.0
  && json |> member "convergence_target" |> to_float = 0.95

let%test "iteration_record_to_json with agent_results" =
  let rec_ = make_iteration ~iteration:2
    ~agent_results:[("w1", Swarm_types.Idle); ("w2", Swarm_types.Idle)]
    ~elapsed:3.0 ~timestamp:500.0 () in
  let json = iteration_record_to_json rec_ in
  let open Yojson.Safe.Util in
  let agents = json |> member "agent_results" |> to_list in
  List.length agents = 2

let%test "iteration_record_of_json preserves agent names" =
  let json = `Assoc [
    ("iteration", `Int 1);
    ("metric_value", `Null);
    ("agent_results", `List [
      `Assoc [("name", `String "worker1"); ("status", `String "Idle")];
    ]);
    ("elapsed", `Float 1.0);
    ("timestamp", `Float 200.0);
    ("trace_refs", `List []);
  ] in
  let rec_ = iteration_record_of_json ~version:checkpoint_version json in
  rec_.iteration = 1
  && List.length rec_.agent_results = 1
  && fst (List.hd rec_.agent_results) = "worker1"

let%test "to_json best_metric None" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.best_metric <- None;
  let cp = of_state state in
  let json = to_json cp in
  let open Yojson.Safe.Util in
  json |> member "best_metric" = `Null

let%test "to_json best_metric Some" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.best_metric <- Some 0.99;
  let cp = of_state state in
  let json = to_json cp in
  let open Yojson.Safe.Util in
  json |> member "best_metric" |> to_float = 0.99

let%test "of_state patience_counter from state" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.patience_counter <- 7;
  let cp = of_state state in
  cp.patience_counter = 7

let%test "of_state best_iteration from state" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.best_iteration <- 3;
  let cp = of_state state in
  cp.best_iteration = 3

let%test "restore: best_iteration restored" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  state.best_iteration <- 4;
  let cp = of_state state in
  let lookup name =
    if name = "a1" || name = "a2" then Some (make_entry name) else None in
  match restore cp ~agent_lookup:lookup ~base_config:cfg with
  | Ok restored -> restored.best_iteration = 4
  | Error _ -> false

let%test "snapshot_of_config: Supervisor mode preserved" =
  let cfg = make_config ~mode:Swarm_types.Supervisor () in
  let snap = snapshot_of_config cfg in
  snap.mode = Swarm_types.Supervisor

let%test "snapshot_of_config: Pipeline mode preserved" =
  let cfg = make_config ~mode:Swarm_types.Pipeline_mode () in
  let snap = snapshot_of_config cfg in
  snap.mode = Swarm_types.Pipeline_mode

let%test "to_json history empty" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  let cp = of_state state in
  let json = to_json cp in
  let open Yojson.Safe.Util in
  json |> member "history" |> to_list = []

let%test "to_json created_at is positive" =
  let cfg = make_config () in
  let state = Swarm_types.create_state cfg in
  let cp = of_state state in
  let json = to_json cp in
  let open Yojson.Safe.Util in
  json |> member "created_at" |> to_float > 0.0

(* --- Version 2 roundtrip tests: agent_status variants --- *)

let%test "roundtrip: agent_status Idle" =
  let status = Swarm_types.Idle in
  let json = Swarm_types.agent_status_to_yojson status in
  match Swarm_types.agent_status_of_yojson json with
  | Ok restored -> restored = status
  | Error _ -> false

let%test "roundtrip: agent_status Working" =
  let status = Swarm_types.Working in
  let json = Swarm_types.agent_status_to_yojson status in
  match Swarm_types.agent_status_of_yojson json with
  | Ok restored -> restored = status
  | Error _ -> false

let%test "roundtrip: agent_status Done_ok" =
  let telemetry = { Swarm_types.trace_ref = None; usage = None; turn_count = 3 } in
  let status = Swarm_types.Done_ok { elapsed = 1.5; text = "success"; telemetry } in
  let json = Swarm_types.agent_status_to_yojson status in
  match Swarm_types.agent_status_of_yojson json with
  | Ok (Done_ok { elapsed; text; telemetry = t }) ->
      elapsed = 1.5 && text = "success" && t.turn_count = 3
  | _ -> false

let%test "roundtrip: agent_status Done_error" =
  let telemetry = { Swarm_types.trace_ref = None; usage = None; turn_count = 2 } in
  let status = Swarm_types.Done_error { elapsed = 0.5; error = "failed"; telemetry } in
  let json = Swarm_types.agent_status_to_yojson status in
  match Swarm_types.agent_status_of_yojson json with
  | Ok (Done_error { elapsed; error; telemetry = t }) ->
      elapsed = 0.5 && error = "failed" && t.turn_count = 2
  | _ -> false

(* --- Version 2 roundtrip tests: trace_refs --- *)

let%test "roundtrip: iteration_record with trace_refs" =
  let run_ref = {
    Raw_trace.worker_run_id = "run123";
    path = "/tmp/trace.jsonl";
    start_seq = 1;
    end_seq = 10;
    agent_name = "test_agent";
    session_id = Some "session456";
  } in
  let rec_ = make_iteration ~iteration:1 ~metric_value:(Some 0.8)
    ~agent_results:[("a1", Swarm_types.Idle)] ~elapsed:1.0 ~timestamp:100.0 () in
  let rec_with_traces = { rec_ with trace_refs = [run_ref] } in
  let json = iteration_record_to_json rec_with_traces in
  let restored = iteration_record_of_json ~version:checkpoint_version json in
  List.length restored.trace_refs = 1
  && (List.hd restored.trace_refs).worker_run_id = "run123"
  && (List.hd restored.trace_refs).agent_name = "test_agent"

(* --- Version 2 roundtrip tests: Done_ok with full telemetry --- *)

let%test "roundtrip: iteration_record with Done_ok status and usage" =
  let usage = {
    Types.total_input_tokens = 100;
    total_output_tokens = 50;
    total_cache_creation_input_tokens = 0;
    total_cache_read_input_tokens = 20;
    api_calls = 1;
    estimated_cost_usd = 0.05;
  } in
  let telemetry = { Swarm_types.trace_ref = None; usage = Some usage; turn_count = 2 } in
  let status = Swarm_types.Done_ok { elapsed = 2.5; text = "completed"; telemetry } in
  let rec_ = make_iteration ~iteration:5 ~metric_value:(Some 0.9)
    ~agent_results:[("agent1", status)] ~elapsed:3.0 ~timestamp:200.0 () in
  let json = iteration_record_to_json rec_ in
  let restored = iteration_record_of_json ~version:checkpoint_version json in
  match List.assoc_opt "agent1" restored.agent_results with
  | Some (Done_ok { elapsed; text; telemetry = t }) ->
      elapsed = 2.5 && text = "completed" && t.turn_count = 2
      && (match t.usage with
          | Some u -> u.total_input_tokens = 100 && u.total_output_tokens = 50
          | None -> false)
  | _ -> false

(* --- Version 1 compatibility tests --- *)

let%test "version 1 compatibility: agent_results restored as Idle" =
  let json = `Assoc [
    ("iteration", `Int 1);
    ("metric_value", `Float 0.5);
    ("agent_results", `List [
      `Assoc [("name", `String "w1"); ("status", `String "Done_ok { elapsed = 1.5; text = \"ok\"; telemetry = {...} }")];
    ]);
    ("elapsed", `Float 1.0);
    ("timestamp", `Float 100.0);
  ] in
  let restored = iteration_record_of_json ~version:1 json in
  match List.assoc_opt "w1" restored.agent_results with
  | Some Idle -> true
  | _ -> false

let%test "version 1 compatibility: trace_refs restored as empty" =
  let json = `Assoc [
    ("iteration", `Int 1);
    ("metric_value", `Null);
    ("agent_results", `List []);
    ("elapsed", `Float 1.0);
    ("timestamp", `Float 100.0);
  ] in
  let restored = iteration_record_of_json ~version:1 json in
  restored.trace_refs = []

