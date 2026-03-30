(** Swarm plan cache — store and reuse proven convergence templates.

    @since 0.99.0 *)

open Swarm_types

let schema_version = 1

(* ── Types ──────────────────────────────────────────────────────── *)

type agent_score = {
  name: string;
  role: agent_role;
  success_rate: float;
  avg_elapsed: float;
  score: float;
}

type quality_signal = {
  final_metric: float;
  convergence_iteration: int;
  total_iterations: int;
  total_elapsed: float;
  total_tokens: int;
}

type template = {
  version: int;
  structural_key: string;
  prompt_class: string;
  config_snapshot: Swarm_checkpoint.config_snapshot;
  quality: quality_signal;
  agent_scores: agent_score list;
  created_at: float;
  use_count: int;
}

type warm_start_hint = {
  suggested_max_iterations: int;
  suggested_patience: int;
  agent_order: string list;
  source_template_key: string;
}

type cache_backend = {
  get: key:string -> template option;
  set: key:string -> template -> unit;
  list_keys: unit -> string list;
}

(* ── Fingerprinting ─────────────────────────────────────────────── *)

let structural_fingerprint (config : swarm_config) =
  let agents =
    config.entries
    |> List.map (fun (e : agent_entry) ->
      Printf.sprintf "%s:%s" e.name (show_agent_role e.role))
    |> List.sort String.compare
  in
  let conv_parts = match config.convergence with
    | None -> ["no_convergence"]
    | Some c ->
      [ Printf.sprintf "target=%.6f" c.target;
        Printf.sprintf "max_iter=%d" c.max_iterations;
        Printf.sprintf "patience=%d" c.patience;
        (match c.aggregate with
         | Best_score -> "agg=best"
         | Average_score -> "agg=avg"
         | Majority_vote -> "agg=majority"
         | Custom_agg _ -> "agg=custom") ]
  in
  let canonical = Printf.sprintf "agents=[%s];mode=%s;conv=[%s]"
    (String.concat "," agents)
    (show_orchestration_mode config.mode)
    (String.concat "," conv_parts)
  in
  Digest.string canonical |> Digest.to_hex

let prompt_class prompt =
  let lower = String.lowercase_ascii (String.trim prompt) in
  let prefix =
    if String.length lower <= 100 then lower
    else String.sub lower 0 100
  in
  let len_tag = Printf.sprintf "|len=%d" (String.length lower) in
  Digest.string (prefix ^ len_tag) |> Digest.to_hex

(* ── Scoring ────────────────────────────────────────────────────── *)

let score_agents ~entries ~history =
  let role_of name =
    match List.find_opt (fun (e : agent_entry) -> e.name = name) entries with
    | Some e -> e.role
    | None -> Custom_role "unknown"
  in
  (* Collect per-agent stats from all iterations *)
  let tbl = Hashtbl.create 16 in
  List.iter (fun (record : iteration_record) ->
    List.iter (fun (name, status) ->
      let (ok, total, elapsed_sum) =
        match Hashtbl.find_opt tbl name with
        | Some v -> v
        | None -> (0, 0, 0.0)
      in
      let ok', elapsed' = match status with
        | Done_ok { elapsed; _ } -> (ok + 1, elapsed_sum +. elapsed)
        | Done_error { elapsed; _ } -> (ok, elapsed_sum +. elapsed)
        | Idle | Working -> (ok, elapsed_sum)
      in
      Hashtbl.replace tbl name (ok', total + 1, elapsed')
    ) record.agent_results
  ) history;
  (* Compute scores *)
  let agent_names =
    entries |> List.map (fun (e : agent_entry) -> e.name)
  in
  List.map (fun name ->
    let (ok, total, elapsed_sum) =
      match Hashtbl.find_opt tbl name with
      | Some v -> v
      | None -> (0, 0, 0.0)
    in
    let success_rate =
      if total = 0 then 0.0
      else float_of_int ok /. float_of_int total
    in
    let avg_elapsed =
      if total = 0 then 0.0
      else elapsed_sum /. float_of_int total
    in
    let speed_factor = 1.0 /. (1.0 +. avg_elapsed) in
    let score = success_rate *. 0.7 +. speed_factor *. 0.3 in
    { name; role = role_of name; success_rate; avg_elapsed; score }
  ) agent_names

(* ── Template creation ──────────────────────────────────────────── *)

let find_convergence_iteration ~target history =
  let rec scan = function
    | [] -> List.length history  (* fallback: last iteration *)
    | record :: rest ->
      (match record.metric_value with
       | Some v when v >= target -> record.iteration
       | _ -> scan rest)
  in
  scan history

let template_of_state (state : swarm_state) =
  if not state.converged then None
  else
    let config = state.config in
    let key = structural_fingerprint config in
    let history = List.rev state.history in  (* history is stored reversed *)
    let conv_iter = match config.convergence with
      | Some c -> find_convergence_iteration ~target:c.target history
      | None -> 0
    in
    let total_tokens =
      List.fold_left (fun acc (record : iteration_record) ->
        List.fold_left (fun a (_name, status) ->
          match status with
          | Done_ok { telemetry; _ } | Done_error { telemetry; _ } ->
            (match telemetry.usage with
             | Some u ->
               a + u.Agent_sdk.Types.total_input_tokens
                 + u.Agent_sdk.Types.total_output_tokens
             | None -> a)
          | Idle | Working -> a
        ) acc record.agent_results
      ) 0 history
    in
    let total_elapsed =
      match List.rev history with
      | last :: _ -> last.elapsed
      | [] -> 0.0
    in
    let quality = {
      final_metric =
        (match state.best_metric with Some v -> v | None -> 0.0);
      convergence_iteration = conv_iter;
      total_iterations = state.current_iteration;
      total_elapsed;
      total_tokens;
    } in
    Some {
      version = schema_version;
      structural_key = key;
      prompt_class = prompt_class config.prompt;
      config_snapshot = Swarm_checkpoint.snapshot_of_config config;
      quality;
      agent_scores = score_agents ~entries:config.entries ~history;
      created_at = Unix.gettimeofday ();
      use_count = 0;
    }

(* ── JSON serialization ─────────────────────────────────────────── *)

let agent_score_to_json (s : agent_score) : Yojson.Safe.t =
  `Assoc [
    ("name", `String s.name);
    ("role", `String (show_agent_role s.role));
    ("success_rate", `Float s.success_rate);
    ("avg_elapsed", `Float s.avg_elapsed);
    ("score", `Float s.score);
  ]

let agent_score_of_json (json : Yojson.Safe.t) : (agent_score, string) result =
  let open Yojson.Safe.Util in
  try
    let role_str = json |> member "role" |> to_string in
    let role = match role_str with
      | "Swarm_types.Discover" | "Discover" -> Discover
      | "Swarm_types.Verify" | "Verify" -> Verify
      | "Swarm_types.Execute" | "Execute" -> Execute
      | "Swarm_types.Summarize" | "Summarize" -> Summarize
      | s ->
        (* Custom_role: strip prefix if present *)
        let prefix = "(Swarm_types.Custom_role " in
        if String.length s > String.length prefix
           && String.sub s 0 (String.length prefix) = prefix then
          Custom_role (String.sub s (String.length prefix)
                         (String.length s - String.length prefix - 1))
        else Custom_role s
    in
    Ok {
      name = json |> member "name" |> to_string;
      role;
      success_rate = json |> member "success_rate" |> to_float;
      avg_elapsed = json |> member "avg_elapsed" |> to_float;
      score = json |> member "score" |> to_float;
    }
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg

let quality_signal_to_json (q : quality_signal) : Yojson.Safe.t =
  `Assoc [
    ("final_metric", `Float q.final_metric);
    ("convergence_iteration", `Int q.convergence_iteration);
    ("total_iterations", `Int q.total_iterations);
    ("total_elapsed", `Float q.total_elapsed);
    ("total_tokens", `Int q.total_tokens);
  ]

let quality_signal_of_json (json : Yojson.Safe.t)
    : (quality_signal, string) result =
  let open Yojson.Safe.Util in
  try
    Ok {
      final_metric = json |> member "final_metric" |> to_float;
      convergence_iteration =
        json |> member "convergence_iteration" |> to_int;
      total_iterations = json |> member "total_iterations" |> to_int;
      total_elapsed = json |> member "total_elapsed" |> to_float;
      total_tokens = json |> member "total_tokens" |> to_int;
    }
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg

(* Reuse config_snapshot serialization from swarm_checkpoint *)
let config_snapshot_to_json (cs : Swarm_checkpoint.config_snapshot)
    : Yojson.Safe.t =
  `Assoc [
    ("entry_names",
     `List (List.map (fun n -> `String n) cs.entry_names));
    ("mode", `String (show_orchestration_mode cs.mode));
    ("max_parallel", `Int cs.max_parallel);
    ("prompt", `String cs.prompt);
    ("timeout_sec",
     match cs.timeout_sec with Some v -> `Float v | None -> `Null);
    ("convergence_target",
     match cs.convergence_target with Some v -> `Float v | None -> `Null);
    ("convergence_max_iterations",
     match cs.convergence_max_iterations with
     | Some v -> `Int v | None -> `Null);
    ("convergence_patience",
     match cs.convergence_patience with
     | Some v -> `Int v | None -> `Null);
  ]

let config_snapshot_of_json (json : Yojson.Safe.t)
    : (Swarm_checkpoint.config_snapshot, string) result =
  let open Yojson.Safe.Util in
  try
    let mode_str = json |> member "mode" |> to_string in
    let mode = match mode_str with
      | "Swarm_types.Decentralized" | "Decentralized" -> Decentralized
      | "Swarm_types.Supervisor" | "Supervisor" -> Supervisor
      | "Swarm_types.Pipeline_mode" | "Pipeline_mode" -> Pipeline_mode
      | s -> failwith (Printf.sprintf "unknown mode: %s" s)
    in
    Ok {
      Swarm_checkpoint.entry_names =
        json |> member "entry_names" |> to_list
        |> List.map to_string;
      mode;
      max_parallel = json |> member "max_parallel" |> to_int;
      prompt = json |> member "prompt" |> to_string;
      timeout_sec = json |> member "timeout_sec" |> to_float_option;
      convergence_target =
        json |> member "convergence_target" |> to_float_option;
      convergence_max_iterations =
        json |> member "convergence_max_iterations" |> to_int_option;
      convergence_patience =
        json |> member "convergence_patience" |> to_int_option;
    }
  with
  | Failure msg -> Error msg
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg

let template_to_json (t : template) : Yojson.Safe.t =
  `Assoc [
    ("v", `Int t.version);
    ("structural_key", `String t.structural_key);
    ("prompt_class", `String t.prompt_class);
    ("config_snapshot", config_snapshot_to_json t.config_snapshot);
    ("quality", quality_signal_to_json t.quality);
    ("agent_scores",
     `List (List.map agent_score_to_json t.agent_scores));
    ("created_at", `Float t.created_at);
    ("use_count", `Int t.use_count);
  ]

let template_of_json (json : Yojson.Safe.t) : (template, string) result =
  let open Yojson.Safe.Util in
  try
    let v = json |> member "v" |> to_int in
    if v <> schema_version then
      Error (Printf.sprintf "unsupported plan cache version: %d" v)
    else
      let quality_json = json |> member "quality" in
      match quality_signal_of_json quality_json with
      | Error e -> Error e
      | Ok quality ->
        let cs_json = json |> member "config_snapshot" in
        match config_snapshot_of_json cs_json with
        | Error e -> Error e
        | Ok config_snapshot ->
          let scores_json = json |> member "agent_scores" |> to_list in
          let rec parse_scores acc = function
            | [] -> Ok (List.rev acc)
            | j :: rest ->
              match agent_score_of_json j with
              | Error e -> Error e
              | Ok s -> parse_scores (s :: acc) rest
          in
          match parse_scores [] scores_json with
          | Error e -> Error e
          | Ok agent_scores ->
            Ok {
              version = v;
              structural_key =
                json |> member "structural_key" |> to_string;
              prompt_class = json |> member "prompt_class" |> to_string;
              config_snapshot;
              quality;
              agent_scores;
              created_at = json |> member "created_at" |> to_float;
              use_count = json |> member "use_count" |> to_int;
            }
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg

(* ── Warm-start ─────────────────────────────────────────────────── *)

let hints_of_template (t : template) =
  let conv_iter = t.quality.convergence_iteration in
  let suggested_max =
    max (int_of_float (Float.round (float_of_int conv_iter *. 1.5)))
        (conv_iter + 2)
  in
  let suggested_patience = conv_iter + 1 in
  let agent_order =
    t.agent_scores
    |> List.sort (fun a b -> Float.compare b.score a.score)
    |> List.map (fun s -> s.name)
  in
  {
    suggested_max_iterations = suggested_max;
    suggested_patience;
    agent_order;
    source_template_key = t.structural_key;
  }

let reorder_entries entries order =
  (* Build position map from order *)
  let pos_of name =
    let rec find i = function
      | [] -> List.length order  (* unknown names go last *)
      | n :: _ when n = name -> i
      | _ :: rest -> find (i + 1) rest
    in
    find 0 order
  in
  List.sort (fun (a : agent_entry) (b : agent_entry) ->
    Int.compare (pos_of a.name) (pos_of b.name)
  ) entries

let apply_hints (config : swarm_config) (hint : warm_start_hint) =
  let convergence = match config.convergence with
    | None -> None
    | Some c ->
      Some {
        c with
        max_iterations =
          min c.max_iterations hint.suggested_max_iterations;
        patience =
          min c.patience hint.suggested_patience;
      }
  in
  let entries = match config.mode with
    | Decentralized -> reorder_entries config.entries hint.agent_order
    | Supervisor | Pipeline_mode -> config.entries
  in
  { config with convergence; entries }

(* ── Cache operations ───────────────────────────────────────────── *)

let record backend (state : swarm_state) =
  match template_of_state state with
  | None -> ()
  | Some tmpl ->
    backend.set ~key:tmpl.structural_key tmpl

let lookup backend (config : swarm_config) =
  let key = structural_fingerprint config in
  backend.get ~key

let lookup_and_apply backend config =
  match lookup backend config with
  | None -> config
  | Some tmpl ->
    let updated = { tmpl with use_count = tmpl.use_count + 1 } in
    backend.set ~key:tmpl.structural_key updated;
    apply_hints config (hints_of_template tmpl)

(* ── Integration ────────────────────────────────────────────────── *)

let make_recording_callbacks backend ?(base = no_callbacks) () =
  let on_converged = Some (fun state ->
    record backend state;
    match base.on_converged with
    | Some f -> f state
    | None -> ()
  ) in
  { base with on_converged }

(* ── Filesystem backend ─────────────────────────────────────────── *)

let fs_backend ~cache_dir =
  (* Ensure directory exists *)
  (try Unix.mkdir cache_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let path_of key = Filename.concat cache_dir (key ^ ".json") in
  {
    get = (fun ~key ->
      let path = path_of key in
      if Sys.file_exists path then
        try
          let ic = open_in path in
          let content = In_channel.input_all ic in
          close_in ic;
          match Yojson.Safe.from_string content with
          | json ->
            (match template_of_json json with
             | Ok t -> Some t
             | Error _ -> None)
          | exception _ -> None
        with _ -> None
      else None);
    set = (fun ~key tmpl ->
      let path = path_of key in
      let tmp_path = path ^ ".tmp" in
      let json = template_to_json tmpl in
      let oc = open_out tmp_path in
      output_string oc (Yojson.Safe.pretty_to_string json);
      close_out oc;
      Sys.rename tmp_path path);
    list_keys = (fun () ->
      try
        let entries = Sys.readdir cache_dir in
        Array.to_list entries
        |> List.filter_map (fun name ->
          if Filename.check_suffix name ".json" then
            Some (Filename.chop_suffix name ".json")
          else None)
      with _ -> []);
  }

(* ── Inline tests ───────────────────────────────────────────────── *)

[@@@coverage off]

let%test "structural_fingerprint: deterministic" =
  let config : swarm_config = {
    entries = [
      { name = "a"; run = (fun ~sw:_ _ -> assert false);
        role = Discover; get_telemetry = None; extensions = [] };
      { name = "b"; run = (fun ~sw:_ _ -> assert false);
        role = Verify; get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 0.0); target = 0.9;
      max_iterations = 10; patience = 3; aggregate = Best_score };
    max_parallel = 4; prompt = "test"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let k1 = structural_fingerprint config in
  let k2 = structural_fingerprint config in
  k1 = k2

let%test "structural_fingerprint: prompt independent" =
  let make_config prompt : swarm_config = {
    entries = [
      { name = "x"; run = (fun ~sw:_ _ -> assert false);
        role = Execute; get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 0.0); target = 0.8;
      max_iterations = 5; patience = 2; aggregate = Best_score };
    max_parallel = 2; prompt; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  structural_fingerprint (make_config "hello")
  = structural_fingerprint (make_config "world")

let%test "structural_fingerprint: mode sensitive" =
  let make_config mode : swarm_config = {
    entries = [
      { name = "x"; run = (fun ~sw:_ _ -> assert false);
        role = Execute; get_telemetry = None; extensions = [] };
    ];
    mode;
    convergence = None;
    max_parallel = 2; prompt = "test"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  structural_fingerprint (make_config Decentralized)
  <> structural_fingerprint (make_config Pipeline_mode)

let%test "prompt_class: normalization" =
  prompt_class "  Hello World  " = prompt_class "hello world"

let%test "prompt_class: length sensitive" =
  prompt_class "short" <> prompt_class (String.make 200 'x')

let%test "score_agents: empty history" =
  let entries = [
    { name = "a"; run = (fun ~sw:_ _ -> assert false);
      role = Discover; get_telemetry = None; extensions = [] };
  ] in
  let scores = score_agents ~entries ~history:[] in
  List.length scores = 1
  && (List.hd scores).success_rate = 0.0

let%test "score_agents: all success" =
  let entries = [
    { name = "a"; run = (fun ~sw:_ _ -> assert false);
      role = Discover; get_telemetry = None; extensions = [] };
  ] in
  let history = [{
    iteration = 0; metric_value = Some 0.9; elapsed = 1.0;
    timestamp = 0.0; trace_refs = [];
    agent_results = [
      ("a", Done_ok { elapsed = 0.5; text = "ok";
                      telemetry = empty_telemetry })
    ];
  }] in
  let scores = score_agents ~entries ~history in
  (List.hd scores).success_rate = 1.0

let%test "template_of_state: unconverged returns None" =
  let config : swarm_config = {
    entries = []; mode = Decentralized; convergence = None;
    max_parallel = 1; prompt = "x"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let state = create_state config in
  template_of_state state = None

let%test "template JSON roundtrip" =
  let tmpl = {
    version = schema_version;
    structural_key = "abc123";
    prompt_class = "def456";
    config_snapshot = {
      Swarm_checkpoint.entry_names = ["a"; "b"];
      mode = Decentralized; max_parallel = 4; prompt = "test";
      timeout_sec = None; convergence_target = Some 0.9;
      convergence_max_iterations = Some 10;
      convergence_patience = Some 3;
    };
    quality = {
      final_metric = 0.95; convergence_iteration = 3;
      total_iterations = 5; total_elapsed = 10.0;
      total_tokens = 1000;
    };
    agent_scores = [
      { name = "a"; role = Discover; success_rate = 1.0;
        avg_elapsed = 0.5; score = 0.85 };
      { name = "b"; role = Verify; success_rate = 0.8;
        avg_elapsed = 1.0; score = 0.71 };
    ];
    created_at = 1000.0;
    use_count = 0;
  } in
  let json = template_to_json tmpl in
  match template_of_json json with
  | Ok restored ->
    restored.structural_key = tmpl.structural_key
    && restored.quality.final_metric = tmpl.quality.final_metric
    && restored.quality.convergence_iteration = tmpl.quality.convergence_iteration
    && List.length restored.agent_scores = 2
    && restored.use_count = 0
  | Error _ -> false

let%test "hints: iterations tighter than original" =
  let tmpl = {
    version = schema_version; structural_key = "k"; prompt_class = "p";
    config_snapshot = {
      Swarm_checkpoint.entry_names = ["a"]; mode = Decentralized;
      max_parallel = 1; prompt = "t"; timeout_sec = None;
      convergence_target = Some 0.9;
      convergence_max_iterations = Some 10;
      convergence_patience = Some 5;
    };
    quality = {
      final_metric = 0.95; convergence_iteration = 3;
      total_iterations = 3; total_elapsed = 5.0; total_tokens = 500;
    };
    agent_scores = [
      { name = "a"; role = Discover; success_rate = 1.0;
        avg_elapsed = 1.0; score = 0.85 }
    ];
    created_at = 0.0; use_count = 0;
  } in
  let hints = hints_of_template tmpl in
  (* ceil(3 * 1.5) = 5, max(5, 3+2) = 5 *)
  hints.suggested_max_iterations = 5

let%test "apply_hints: preserves prompt and mode" =
  let config : swarm_config = {
    entries = [
      { name = "a"; run = (fun ~sw:_ _ -> assert false);
        role = Discover; get_telemetry = None; extensions = [] };
    ];
    mode = Pipeline_mode;
    convergence = Some {
      metric = Callback (fun () -> 0.0); target = 0.9;
      max_iterations = 20; patience = 10; aggregate = Best_score };
    max_parallel = 4; prompt = "my prompt"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let hint = {
    suggested_max_iterations = 5; suggested_patience = 3;
    agent_order = ["a"]; source_template_key = "k";
  } in
  let result = apply_hints config hint in
  result.prompt = "my prompt"
  && result.mode = Pipeline_mode

let%test "apply_hints: does not reorder Pipeline entries" =
  let config : swarm_config = {
    entries = [
      { name = "first"; run = (fun ~sw:_ _ -> assert false);
        role = Discover; get_telemetry = None; extensions = [] };
      { name = "second"; run = (fun ~sw:_ _ -> assert false);
        role = Verify; get_telemetry = None; extensions = [] };
    ];
    mode = Pipeline_mode;
    convergence = Some {
      metric = Callback (fun () -> 0.0); target = 0.9;
      max_iterations = 20; patience = 10; aggregate = Best_score };
    max_parallel = 4; prompt = "test"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let hint = {
    suggested_max_iterations = 5; suggested_patience = 3;
    agent_order = ["second"; "first"];
    source_template_key = "k";
  } in
  let result = apply_hints config hint in
  let names = List.map (fun (e : agent_entry) -> e.name) result.entries in
  names = ["first"; "second"]

let%test "apply_hints: reorders Decentralized entries" =
  let config : swarm_config = {
    entries = [
      { name = "low"; run = (fun ~sw:_ _ -> assert false);
        role = Discover; get_telemetry = None; extensions = [] };
      { name = "high"; run = (fun ~sw:_ _ -> assert false);
        role = Verify; get_telemetry = None; extensions = [] };
    ];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 0.0); target = 0.9;
      max_iterations = 20; patience = 10; aggregate = Best_score };
    max_parallel = 4; prompt = "test"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let hint = {
    suggested_max_iterations = 5; suggested_patience = 3;
    agent_order = ["high"; "low"];
    source_template_key = "k";
  } in
  let result = apply_hints config hint in
  let names = List.map (fun (e : agent_entry) -> e.name) result.entries in
  names = ["high"; "low"]

let%test "apply_hints: max_iterations clamped to original" =
  let config : swarm_config = {
    entries = [];
    mode = Decentralized;
    convergence = Some {
      metric = Callback (fun () -> 0.0); target = 0.9;
      max_iterations = 3; patience = 2; aggregate = Best_score };
    max_parallel = 1; prompt = "test"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let hint = {
    suggested_max_iterations = 10; suggested_patience = 5;
    agent_order = []; source_template_key = "k";
  } in
  let result = apply_hints config hint in
  match result.convergence with
  | Some c -> c.max_iterations = 3 && c.patience = 2
  | None -> false

let%test "make_recording_callbacks: composes with base" =
  let called = ref false in
  let base : swarm_callbacks = {
    no_callbacks with
    on_converged = Some (fun _ -> called := true)
  } in
  let store_called = ref false in
  let backend = {
    get = (fun ~key:_ -> None);
    set = (fun ~key:_ _ -> store_called := true);
    list_keys = (fun () -> []);
  } in
  let cbs = make_recording_callbacks backend ~base () in
  (* Simulate converged state *)
  let config : swarm_config = {
    entries = []; mode = Decentralized; convergence = None;
    max_parallel = 1; prompt = "x"; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false;
  } in
  let state = { (create_state config) with converged = true } in
  (match cbs.on_converged with Some f -> f state | None -> ());
  (* Both base and recording should have been called *)
  !called && !store_called
