(** Tests for swarm plan cache — template storage and warm-start. *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm
open Swarm_types

(* ── Helpers ──────────────────────────────────────────────────────── *)

let float_eps = 1e-6
let check_float msg expected actual =
  check bool msg true (Float.abs (expected -. actual) < float_eps)

let mock_entry name role : agent_entry =
  { name;
    run = (fun ~sw:_ _prompt ->
      Ok { Types.id = "m"; model = "m"; stop_reason = EndTurn;
           content = [Text "ok"]; usage = None; telemetry = None });
    role; get_telemetry = None; extensions = [] }

let base_config ?(entries=[mock_entry "a" Discover])
    ?(mode=Decentralized) ?(convergence=None) ?(prompt="test") () : swarm_config =
  { entries; mode; convergence;
    max_parallel = 4; prompt; timeout_sec = None;
    budget = no_budget; max_agent_retries = 0; collaboration_context = None;
    resource_check = None; max_concurrent_agents = None;
    enable_streaming = false }

let make_convergence ?(target=0.9) ?(max_iter=10) ?(patience=3) () : convergence_config =
  { metric = Callback (fun () -> 0.0); target;
    max_iterations = max_iter; patience; aggregate = Best_score }

let make_iteration ?(iter=0) ?(metric=None) ?(elapsed=1.0) results : iteration_record =
  { iteration = iter; metric_value = metric;
    agent_results = results; elapsed;
    timestamp = Unix.gettimeofday (); trace_refs = [] }

let in_memory_backend () =
  let tbl = Hashtbl.create 16 in
  Swarm_plan_cache.({
    get = (fun ~key -> Hashtbl.find_opt tbl key);
    set = (fun ~key tmpl -> Hashtbl.replace tbl key tmpl);
    list_keys = (fun () ->
      Hashtbl.fold (fun k _ acc -> k :: acc) tbl []);
  })

let set_config_snapshot_mode mode json =
  match json with
  | `Assoc fields ->
    `Assoc
      (List.map
         (function
           | ("config_snapshot", `Assoc config_fields) ->
             ("config_snapshot",
              `Assoc
                (List.map
                   (function
                     | ("mode", _) -> ("mode", `String mode)
                     | field -> field)
                   config_fields))
           | field -> field)
         fields)
  | _ -> fail "expected template JSON object"

let sample_template () =
  let config =
    base_config
      ~entries:[mock_entry "a" Discover; mock_entry "b" Verify]
      ~convergence:(Some (make_convergence ())) ()
  in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.95;
  state.best_iteration <- 2;
  state.current_iteration <- 3;
  state.history <- [
    make_iteration ~iter:0 ~metric:(Some 0.95)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry });
       ("b", Done_ok { elapsed = 1.0; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  match Swarm_plan_cache.template_of_state state with
  | None -> fail "expected template"
  | Some tmpl -> tmpl

(* ── Fingerprinting ───────────────────────────────────────────────── *)

let test_fingerprint_deterministic () =
  let config = base_config
    ~convergence:(Some (make_convergence ())) () in
  let k1 = Swarm_plan_cache.structural_fingerprint config in
  let k2 = Swarm_plan_cache.structural_fingerprint config in
  check string "same config same key" k1 k2

let test_fingerprint_prompt_independent () =
  let c1 = base_config ~prompt:"hello" () in
  let c2 = base_config ~prompt:"world" () in
  check string "different prompts same key"
    (Swarm_plan_cache.structural_fingerprint c1)
    (Swarm_plan_cache.structural_fingerprint c2)

let test_fingerprint_mode_sensitive () =
  let c1 = base_config ~mode:Decentralized () in
  let c2 = base_config ~mode:Pipeline_mode () in
  let k1 = Swarm_plan_cache.structural_fingerprint c1 in
  let k2 = Swarm_plan_cache.structural_fingerprint c2 in
  check bool "different modes different keys" true (k1 <> k2)

let test_fingerprint_agent_sensitive () =
  let c1 = base_config ~entries:[mock_entry "a" Discover] () in
  let c2 = base_config ~entries:[mock_entry "b" Discover] () in
  let k1 = Swarm_plan_cache.structural_fingerprint c1 in
  let k2 = Swarm_plan_cache.structural_fingerprint c2 in
  check bool "different agents different keys" true (k1 <> k2)

let test_fingerprint_role_sensitive () =
  let c1 = base_config ~entries:[mock_entry "a" Discover] () in
  let c2 = base_config ~entries:[mock_entry "a" Verify] () in
  let k1 = Swarm_plan_cache.structural_fingerprint c1 in
  let k2 = Swarm_plan_cache.structural_fingerprint c2 in
  check bool "different roles different keys" true (k1 <> k2)

let test_fingerprint_convergence_sensitive () =
  let c1 = base_config ~convergence:(Some (make_convergence ~target:0.9 ())) () in
  let c2 = base_config ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let k1 = Swarm_plan_cache.structural_fingerprint c1 in
  let k2 = Swarm_plan_cache.structural_fingerprint c2 in
  check bool "different convergence targets different keys" true (k1 <> k2)

(* ── Prompt class ─────────────────────────────────────────────────── *)

let test_prompt_class_normalization () =
  check string "case and whitespace normalized"
    (Swarm_plan_cache.prompt_class "  Hello World  ")
    (Swarm_plan_cache.prompt_class "hello world")

let test_prompt_class_length_sensitive () =
  let short = Swarm_plan_cache.prompt_class "short" in
  let long = Swarm_plan_cache.prompt_class (String.make 200 'x') in
  check bool "different lengths different classes" true (short <> long)

(* ── Scoring ──────────────────────────────────────────────────────── *)

let test_score_all_success () =
  let entries = [mock_entry "a" Discover] in
  let history = [
    make_iteration [("a", Done_ok { elapsed = 0.5; text = "ok";
                                    telemetry = empty_telemetry })];
    make_iteration ~iter:1 [("a", Done_ok { elapsed = 0.3; text = "ok";
                                            telemetry = empty_telemetry })];
  ] in
  let scores = Swarm_plan_cache.score_agents ~entries ~history in
  check int "one score" 1 (List.length scores);
  let s = List.hd scores in
  check_float "success rate 1.0" 1.0 s.success_rate;
  check bool "score positive" true (s.score > 0.0)

let test_score_mixed () =
  let entries = [mock_entry "a" Discover; mock_entry "b" Verify] in
  let history = [
    make_iteration [
      ("a", Done_ok { elapsed = 1.0; text = "ok";
                      telemetry = empty_telemetry });
      ("b", Done_error { elapsed = 2.0; error = "fail";
                         telemetry = empty_telemetry });
    ];
  ] in
  let scores = Swarm_plan_cache.score_agents ~entries ~history in
  let sa = List.find (fun (s : Swarm_plan_cache.agent_score) -> s.name = "a") scores in
  let sb = List.find (fun (s : Swarm_plan_cache.agent_score) -> s.name = "b") scores in
  check_float "a success rate" 1.0 sa.success_rate;
  check_float "b success rate" 0.0 sb.success_rate;
  check bool "a scores higher" true (sa.score > sb.score)

let test_score_empty_history () =
  let entries = [mock_entry "a" Discover] in
  let scores = Swarm_plan_cache.score_agents ~entries ~history:[] in
  check int "one score" 1 (List.length scores);
  check_float "zero success" 0.0 (List.hd scores).success_rate

(* ── Template ─────────────────────────────────────────────────────── *)

let test_template_from_unconverged () =
  let config = base_config () in
  let state = create_state config in
  check bool "unconverged returns None" true
    (Swarm_plan_cache.template_of_state state = None)

let test_template_from_converged () =
  let config = base_config
    ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.9;
  state.best_iteration <- 2;
  state.current_iteration <- 3;
  state.history <- [
    make_iteration ~iter:2 ~metric:(Some 0.9)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry })];
    make_iteration ~iter:1 ~metric:(Some 0.7)
      [("a", Done_ok { elapsed = 0.6; text = "ok";
                        telemetry = empty_telemetry })];
    make_iteration ~iter:0 ~metric:(Some 0.5)
      [("a", Done_ok { elapsed = 0.8; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  match Swarm_plan_cache.template_of_state state with
  | None -> fail "expected Some template"
  | Some tmpl ->
    check_float "final metric" 0.9 tmpl.quality.final_metric;
    check int "total iterations" 3 tmpl.quality.total_iterations;
    check int "one agent score" 1 (List.length tmpl.agent_scores)

let test_template_json_roundtrip () =
  let tmpl = sample_template () in
  let json = Swarm_plan_cache.template_to_json tmpl in
  (match Swarm_plan_cache.template_of_json json with
   | Error e -> fail (Printf.sprintf "roundtrip failed: %s" e)
   | Ok restored ->
     check string "structural key" tmpl.structural_key restored.structural_key;
     check_float "final metric"
       tmpl.quality.final_metric restored.quality.final_metric;
     check int "total iterations"
       tmpl.quality.total_iterations restored.quality.total_iterations;
     check_float "total elapsed"
       tmpl.quality.total_elapsed restored.quality.total_elapsed;
     check int "total tokens"
       tmpl.quality.total_tokens restored.quality.total_tokens;
     check int "agent scores count"
       (List.length tmpl.agent_scores) (List.length restored.agent_scores);
     let sa = List.hd tmpl.agent_scores in
     let ra = List.hd restored.agent_scores in
     check string "agent name" sa.name ra.name;
     check_float "agent avg_elapsed" sa.avg_elapsed ra.avg_elapsed;
     check_float "agent score" sa.score ra.score)

let test_template_json_accepts_legacy_mode_names () =
  let json =
    sample_template ()
    |> Swarm_plan_cache.template_to_json
    |> set_config_snapshot_mode "Swarm_types.Supervisor"
  in
  match Swarm_plan_cache.template_of_json json with
  | Error e -> fail (Printf.sprintf "legacy mode parse failed: %s" e)
  | Ok restored ->
    (match restored.config_snapshot.mode with
     | Supervisor -> ()
     | _ -> fail "expected supervisor mode")

let test_template_json_rejects_unknown_mode () =
  let json =
    sample_template ()
    |> Swarm_plan_cache.template_to_json
    |> set_config_snapshot_mode "Totally_new_mode"
  in
  match Swarm_plan_cache.template_of_json json with
  | Ok _ -> fail "expected mode parse failure"
  | Error e ->
    check string "unknown mode error"
      "unknown mode: Totally_new_mode" e

let test_custom_role_json_roundtrip () =
  let score : Swarm_plan_cache.agent_score = {
    name = "custom_agent"; role = Custom_role "reviewer";
    success_rate = 0.75; avg_elapsed = 2.0; score = 0.675 } in
  let json = Swarm_plan_cache.agent_score_to_json score in
  match Swarm_plan_cache.agent_score_of_json json with
  | Error e -> fail (Printf.sprintf "custom role roundtrip: %s" e)
  | Ok restored ->
    check string "name" score.name restored.name;
    check_float "success_rate" score.success_rate restored.success_rate;
    (match restored.role with
     | Custom_role s -> check string "custom role value" "reviewer" s
     | _ -> fail "expected Custom_role")

(* ── Warm-start ───────────────────────────────────────────────────── *)

let test_hints_iterations () =
  let config = base_config
    ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.9;
  state.current_iteration <- 3;
  state.history <- [
    make_iteration ~iter:0 ~metric:(Some 0.85)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  match Swarm_plan_cache.template_of_state state with
  | None -> fail "expected template"
  | Some tmpl ->
    let hints = Swarm_plan_cache.hints_of_template tmpl in
    check bool "max_iterations > 0" true (hints.suggested_max_iterations > 0);
    check bool "patience > 0" true (hints.suggested_patience > 0)

let test_apply_preserves_prompt () =
  let config = base_config ~prompt:"keep this"
    ~convergence:(Some (make_convergence ())) () in
  let hint : Swarm_plan_cache.warm_start_hint = {
    suggested_max_iterations = 5; suggested_patience = 2;
    agent_order = ["a"]; source_template_key = "k" } in
  let result = Swarm_plan_cache.apply_hints config hint in
  check string "prompt preserved" "keep this" result.prompt

let test_apply_preserves_mode () =
  let config = base_config ~mode:Pipeline_mode
    ~convergence:(Some (make_convergence ())) () in
  let hint : Swarm_plan_cache.warm_start_hint = {
    suggested_max_iterations = 5; suggested_patience = 2;
    agent_order = ["a"]; source_template_key = "k" } in
  let result = Swarm_plan_cache.apply_hints config hint in
  (match result.mode with Pipeline_mode -> () | _ -> fail "mode changed")

let test_apply_clamps_to_original () =
  let config = base_config
    ~convergence:(Some (make_convergence ~max_iter:3 ~patience:2 ())) () in
  let hint : Swarm_plan_cache.warm_start_hint = {
    suggested_max_iterations = 100; suggested_patience = 50;
    agent_order = []; source_template_key = "k" } in
  let result = Swarm_plan_cache.apply_hints config hint in
  match result.convergence with
  | None -> fail "convergence lost"
  | Some c ->
    check int "clamped max_iterations" 3 c.max_iterations;
    check int "clamped patience" 2 c.patience

let test_apply_reorder_decentralized () =
  let config = base_config
    ~entries:[mock_entry "low" Discover; mock_entry "high" Verify]
    ~mode:Decentralized () in
  let hint : Swarm_plan_cache.warm_start_hint = {
    suggested_max_iterations = 10; suggested_patience = 5;
    agent_order = ["high"; "low"]; source_template_key = "k" } in
  let result = Swarm_plan_cache.apply_hints config hint in
  let names = List.map (fun (e : agent_entry) -> e.name) result.entries in
  check (list string) "reordered" ["high"; "low"] names

let test_apply_no_reorder_pipeline () =
  let config = base_config
    ~entries:[mock_entry "first" Discover; mock_entry "second" Verify]
    ~mode:Pipeline_mode () in
  let hint : Swarm_plan_cache.warm_start_hint = {
    suggested_max_iterations = 10; suggested_patience = 5;
    agent_order = ["second"; "first"]; source_template_key = "k" } in
  let result = Swarm_plan_cache.apply_hints config hint in
  let names = List.map (fun (e : agent_entry) -> e.name) result.entries in
  check (list string) "preserved order" ["first"; "second"] names

(* ── Cache ops ────────────────────────────────────────────────────── *)

let test_record_and_lookup () =
  let backend = in_memory_backend () in
  let config = base_config
    ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.9;
  state.current_iteration <- 2;
  state.history <- [
    make_iteration ~iter:0 ~metric:(Some 0.9)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  Swarm_plan_cache.record backend state;
  match Swarm_plan_cache.lookup backend config with
  | None -> fail "expected hit"
  | Some tmpl ->
    check_float "metric matches" 0.9 tmpl.quality.final_metric

let test_lookup_miss () =
  let backend = in_memory_backend () in
  let config = base_config () in
  check bool "miss returns None" true
    (Swarm_plan_cache.lookup backend config = None)

let test_lookup_and_apply_increments_use_count () =
  let backend = in_memory_backend () in
  let config = base_config
    ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.9;
  state.current_iteration <- 2;
  state.history <- [
    make_iteration ~iter:0 ~metric:(Some 0.9)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  Swarm_plan_cache.record backend state;
  let _ = Swarm_plan_cache.lookup_and_apply backend config in
  match Swarm_plan_cache.lookup backend config with
  | None -> fail "expected hit after apply"
  | Some tmpl -> check int "use_count" 1 tmpl.use_count

(* ── Integration ──────────────────────────────────────────────────── *)

let test_recording_callbacks () =
  let backend = in_memory_backend () in
  let cbs = Swarm_plan_cache.make_recording_callbacks backend () in
  let config = base_config
    ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.9;
  state.current_iteration <- 1;
  state.history <- [
    make_iteration ~iter:0 ~metric:(Some 0.9)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  (match cbs.on_converged with Some f -> f state | None -> ());
  check bool "template stored" true
    (Swarm_plan_cache.lookup backend config <> None)

let test_recording_callbacks_compose () =
  let backend = in_memory_backend () in
  let base_called = ref false in
  let base : swarm_callbacks = {
    no_callbacks with on_converged = Some (fun _ -> base_called := true) } in
  let cbs = Swarm_plan_cache.make_recording_callbacks backend ~base () in
  let config = base_config () in
  let state = { (create_state config) with converged = true } in
  (match cbs.on_converged with Some f -> f state | None -> ());
  check bool "base on_converged called" true !base_called

let test_fs_backend_roundtrip () =
  let dir = Filename.concat
    (Filename.get_temp_dir_name ()) "oas_plan_cache_test" in
  (try Unix.mkdir dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let backend = Swarm_plan_cache.fs_backend ~cache_dir:dir in
  let config = base_config
    ~convergence:(Some (make_convergence ~target:0.8 ())) () in
  let state = create_state config in
  state.converged <- true;
  state.best_metric <- Some 0.9;
  state.current_iteration <- 1;
  state.history <- [
    make_iteration ~iter:0 ~metric:(Some 0.9)
      [("a", Done_ok { elapsed = 0.5; text = "ok";
                        telemetry = empty_telemetry })];
  ];
  Swarm_plan_cache.record backend state;
  match Swarm_plan_cache.lookup backend config with
  | None -> fail "fs backend roundtrip failed"
  | Some tmpl ->
    check_float "metric from fs" 0.9 tmpl.quality.final_metric;
    (* Cleanup *)
    let key = Swarm_plan_cache.structural_fingerprint config in
    Sys.remove (Filename.concat dir (key ^ ".json"));
    (try Unix.rmdir dir with _ -> ())

(* ── Test runner ──────────────────────────────────────────────────── *)

let () =
  run "swarm_plan_cache" [
    "fingerprint", [
      test_case "deterministic" `Quick test_fingerprint_deterministic;
      test_case "prompt independent" `Quick test_fingerprint_prompt_independent;
      test_case "mode sensitive" `Quick test_fingerprint_mode_sensitive;
      test_case "agent sensitive" `Quick test_fingerprint_agent_sensitive;
      test_case "role sensitive" `Quick test_fingerprint_role_sensitive;
      test_case "convergence sensitive" `Quick test_fingerprint_convergence_sensitive;
    ];
    "prompt_class", [
      test_case "normalization" `Quick test_prompt_class_normalization;
      test_case "length sensitive" `Quick test_prompt_class_length_sensitive;
    ];
    "scoring", [
      test_case "all success" `Quick test_score_all_success;
      test_case "mixed results" `Quick test_score_mixed;
      test_case "empty history" `Quick test_score_empty_history;
    ];
    "template", [
      test_case "from unconverged" `Quick test_template_from_unconverged;
      test_case "from converged" `Quick test_template_from_converged;
      test_case "json roundtrip" `Quick test_template_json_roundtrip;
      test_case "accepts legacy mode names" `Quick
        test_template_json_accepts_legacy_mode_names;
      test_case "rejects unknown mode" `Quick
        test_template_json_rejects_unknown_mode;
      test_case "custom role roundtrip" `Quick test_custom_role_json_roundtrip;
    ];
    "warm_start", [
      test_case "hints iterations" `Quick test_hints_iterations;
      test_case "preserves prompt" `Quick test_apply_preserves_prompt;
      test_case "preserves mode" `Quick test_apply_preserves_mode;
      test_case "clamps to original" `Quick test_apply_clamps_to_original;
      test_case "reorder decentralized" `Quick test_apply_reorder_decentralized;
      test_case "no reorder pipeline" `Quick test_apply_no_reorder_pipeline;
    ];
    "cache_ops", [
      test_case "record and lookup" `Quick test_record_and_lookup;
      test_case "lookup miss" `Quick test_lookup_miss;
      test_case "use_count increment" `Quick test_lookup_and_apply_increments_use_count;
    ];
    "integration", [
      test_case "recording callbacks" `Quick test_recording_callbacks;
      test_case "callbacks compose" `Quick test_recording_callbacks_compose;
      test_case "fs backend roundtrip" `Quick test_fs_backend_roundtrip;
    ];
  ]
