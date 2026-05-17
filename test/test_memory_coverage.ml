(** Extended coverage tests for Memory, Memory_tools, and Memory_access.

    Targets uncovered paths via PUBLIC API only:
    - Memory.ml: tier fallback chains (scratchpad->working->backend/ctx),
      recall_exact for Long_term with/without backend,
      Episodic/Procedural recall returning None (no fallback),
      scratchpad_entries, episode store/recall roundtrip with various outcomes,
      procedure store/matching (exercises string_contains, compute_confidence),
      record_success/failure on missing IDs, boost on missing,
      all_episodes enumeration via recall_episodes
    - Memory_tools.ml: parse field edge cases (bad types), tier variants,
      ACL-backed tools, episode with explicit id, outcome variants
    - Memory_access.ml: recall_exact, forget, recall_episodes with grant,
      permission_includes ReadWrite edge, revoke/memory accessor *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────── *)

let json_s s = `String s
let json_i i = `Int i

let execute_ok_json tool input =
  match Tool.execute tool input with
  | Ok { content } -> Yojson.Safe.from_string content
  | Error { message; _ } -> Alcotest.fail ("expected Ok: " ^ message)
;;

let execute_error tool input =
  match Tool.execute tool input with
  | Ok { content } -> Alcotest.fail ("expected Error: " ^ content)
  | Error { message; _ } -> message
;;

(* ── Memory: recall Episodic/Procedural returns None ──── *)

let test_recall_episodic_returns_none () =
  let mem = Memory.create () in
  Alcotest.(check bool)
    "episodic miss no fallback"
    true
    (Memory.recall mem ~tier:Episodic "nonexistent" = None)
;;

let test_recall_procedural_returns_none () =
  let mem = Memory.create () in
  Alcotest.(check bool)
    "procedural miss no fallback"
    true
    (Memory.recall mem ~tier:Procedural "nonexistent" = None)
;;

(* ── Memory: recall_exact Long_term with backend ──────── *)

let test_recall_exact_long_term_with_backend () =
  let store = Hashtbl.create 4 in
  let backend : Memory.long_term_backend =
    { persist =
        (fun ~key value ->
          Hashtbl.replace store key value;
          Ok ())
    ; retrieve = (fun ~key -> Hashtbl.find_opt store key)
    ; remove =
        (fun ~key ->
          Hashtbl.remove store key;
          Ok ())
    ; batch_persist =
        (fun pairs ->
          List.iter (fun (k, v) -> Hashtbl.replace store k v) pairs;
          Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  Hashtbl.replace store "only_backend" (json_s "from_store");
  match Memory.recall_exact mem ~tier:Long_term "only_backend" with
  | Some (`String "from_store") -> ()
  | _ -> Alcotest.fail "backend should return value"
;;

let test_recall_exact_long_term_no_backend () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Long_term "local_lt" (json_s "v"));
  match Memory.recall_exact mem ~tier:Long_term "local_lt" with
  | Some (`String "v") -> ()
  | _ -> Alcotest.fail "should use local ctx"
;;

(* ── Memory: scratchpad recall chain to long_term ─────── *)

let test_scratchpad_recall_chain_with_backend () =
  let store = Hashtbl.create 4 in
  let backend : Memory.long_term_backend =
    { persist =
        (fun ~key value ->
          Hashtbl.replace store key value;
          Ok ())
    ; retrieve = (fun ~key -> Hashtbl.find_opt store key)
    ; remove =
        (fun ~key ->
          Hashtbl.remove store key;
          Ok ())
    ; batch_persist = (fun _ -> Ok ())
    ; query = (fun ~prefix:_ ~limit:_ -> [])
    }
  in
  let mem = Memory.create ~long_term:backend () in
  Hashtbl.replace store "deep" (json_i 99);
  match Memory.recall mem ~tier:Scratchpad "deep" with
  | Some (`Int 99) -> ()
  | _ -> Alcotest.fail "scratchpad should chain to long_term backend"
;;

let test_scratchpad_recall_chain_no_backend () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Long_term "deep" (json_i 42));
  match Memory.recall mem ~tier:Scratchpad "deep" with
  | Some (`Int 42) -> ()
  | _ -> Alcotest.fail "scratchpad should chain to local long_term ctx"
;;

(* ── Memory: Long_term recall with/without backend ────── *)

let test_long_term_recall_no_backend_none () =
  let mem = Memory.create () in
  Alcotest.(check bool)
    "None without backend"
    true
    (Memory.recall mem ~tier:Long_term "ghost" = None)
;;

(* ── Memory: scratchpad_entries ───────────────────────── *)

let test_scratchpad_entries () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Scratchpad "a" (json_i 1));
  ignore (Memory.store mem ~tier:Scratchpad "b" (json_i 2));
  ignore (Memory.store mem ~tier:Working "c" (json_i 3));
  let entries = Memory.scratchpad_entries mem in
  Alcotest.(check int) "2 scratchpad entries" 2 (List.length entries)
;;

(* ── Memory: episode store/recall with all outcome types ── *)

let test_episode_success_outcome () =
  let mem = Memory.create () in
  let ep : Memory.episode =
    { id = "e-success"
    ; timestamp = 100.0
    ; participants = [ "alice" ]
    ; action = "deploy"
    ; outcome = Success "deployed"
    ; salience = 0.8
    ; metadata = [ "env", `String "prod" ]
    }
  in
  Memory.store_episode mem ep;
  match Memory.recall_episode mem "e-success" with
  | Some found ->
    Alcotest.(check string) "id" "e-success" found.id;
    (match found.outcome with
     | Memory.Success "deployed" -> ()
     | _ -> Alcotest.fail "wrong outcome")
  | None -> Alcotest.fail "not found"
;;

let test_episode_failure_outcome () =
  let mem = Memory.create () in
  let ep : Memory.episode =
    { id = "e-fail"
    ; timestamp = 100.0
    ; participants = []
    ; action = "deploy"
    ; outcome = Failure "timeout"
    ; salience = 0.9
    ; metadata = []
    }
  in
  Memory.store_episode mem ep;
  match Memory.recall_episode mem "e-fail" with
  | Some found ->
    (match found.outcome with
     | Memory.Failure "timeout" -> ()
     | _ -> Alcotest.fail "wrong outcome")
  | None -> Alcotest.fail "not found"
;;

let test_episode_neutral_outcome () =
  let mem = Memory.create () in
  let ep : Memory.episode =
    { id = "e-neutral"
    ; timestamp = 100.0
    ; participants = []
    ; action = "idle"
    ; outcome = Neutral
    ; salience = 0.5
    ; metadata = []
    }
  in
  Memory.store_episode mem ep;
  match Memory.recall_episode mem "e-neutral" with
  | Some found ->
    (match found.outcome with
     | Memory.Neutral -> ()
     | _ -> Alcotest.fail "wrong outcome")
  | None -> Alcotest.fail "not found"
;;

(* ── Memory: episode with metadata ────────────────────── *)

let test_episode_with_metadata () =
  let mem = Memory.create () in
  Memory.store_episode
    mem
    { id = "e-meta"
    ; timestamp = 100.0
    ; participants = []
    ; action = "test"
    ; outcome = Neutral
    ; salience = 0.5
    ; metadata = [ "key", `Int 42; "tag", `String "test" ]
    };
  let count = Memory.episode_count mem in
  Alcotest.(check int) "1 episode" 1 count
;;

(* ── Memory: recall_episodes enumeration ──────────────── *)

let test_recall_episodes_multiple () =
  let mem = Memory.create () in
  let now = 200.0 in
  for i = 0 to 4 do
    Memory.store_episode
      mem
      { id = Printf.sprintf "e%d" i
      ; timestamp = Float.of_int ((i * 10) + 100)
      ; participants = []
      ; action = "a"
      ; outcome = Neutral
      ; salience = 0.9
      ; metadata = []
      }
  done;
  let episodes = Memory.recall_episodes mem ~now ~min_salience:0.01 () in
  Alcotest.(check int) "5 episodes" 5 (List.length episodes)
;;

(* ── Memory: procedure matching exercises string_contains ── *)

let test_procedure_matching_empty_pattern () =
  let mem = Memory.create () in
  Memory.store_procedure
    mem
    { id = "p1"
    ; pattern = "anything"
    ; action = "do"
    ; success_count = 1
    ; failure_count = 0
    ; confidence = 1.0
    ; last_used = 0.0
    ; metadata = []
    };
  (* Empty pattern should match everything (string_contains ~needle:"") *)
  let results = Memory.matching_procedures mem ~pattern:"" () in
  Alcotest.(check int) "matches all" 1 (List.length results)
;;

let test_procedure_matching_no_match () =
  let mem = Memory.create () in
  Memory.store_procedure
    mem
    { id = "p1"
    ; pattern = "deploy"
    ; action = "rollback"
    ; success_count = 1
    ; failure_count = 0
    ; confidence = 1.0
    ; last_used = 0.0
    ; metadata = []
    };
  let results = Memory.matching_procedures mem ~pattern:"zzzzz" () in
  Alcotest.(check int) "no match" 0 (List.length results)
;;

(* ── Memory: record_success/failure on missing IDs ────── *)

let test_record_success_missing () =
  let mem = Memory.create () in
  Memory.record_success mem "ghost";
  Alcotest.(check int) "still 0" 0 (Memory.procedure_count mem)
;;

let test_record_failure_missing () =
  let mem = Memory.create () in
  Memory.record_failure mem "ghost";
  Alcotest.(check int) "still 0" 0 (Memory.procedure_count mem)
;;

(* ── Memory: boost_salience on missing ────────────────── *)

let test_boost_salience_missing () =
  let mem = Memory.create () in
  Memory.boost_salience mem "ghost" 0.5;
  Alcotest.(check int) "no episode created" 0 (Memory.episode_count mem)
;;

(* ── Memory: procedure with zero total (compute_confidence 0/0) ── *)

let test_procedure_zero_total () =
  let mem = Memory.create () in
  Memory.store_procedure
    mem
    { id = "p0"
    ; pattern = "test"
    ; action = "nothing"
    ; success_count = 0
    ; failure_count = 0
    ; confidence = 0.0
    ; last_used = 0.0
    ; metadata = []
    };
  match Memory.best_procedure mem ~pattern:"test" with
  | Some p -> Alcotest.(check (float 0.01)) "confidence 0" 0.0 p.confidence
  | None -> Alcotest.fail "not found"
;;

(* ── Memory_tools: tier variants ──────────────────────── *)

let test_tier_long_term_dash () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  let json =
    execute_ok_json
      tool
      (`Assoc
          [ "tier", `String "long-term"; "key", `String "k"; "value_json", `String "1" ])
  in
  Alcotest.(check string)
    "tier"
    "long_term"
    Yojson.Safe.Util.(json |> member "tier" |> to_string)
;;

let test_tier_invalid_string () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  let msg =
    execute_error
      tool
      (`Assoc [ "tier", `String "bogus"; "key", `String "k"; "value_json", `String "1" ])
  in
  Alcotest.(check bool)
    "mentions invalid"
    true
    (Util.string_contains ~needle:"invalid" msg)
;;

let test_tier_non_string () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  let msg =
    execute_error
      tool
      (`Assoc [ "tier", `Int 42; "key", `String "k"; "value_json", `String "1" ])
  in
  Alcotest.(check bool) "mentions string" true (Util.string_contains ~needle:"string" msg)
;;

(* ── Memory_tools: recall with exact=true ─────────────── *)

let test_recall_exact_flag () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "only_work" (json_s "here"));
  let tool = Memory_tools.recall mem in
  let json =
    execute_ok_json
      tool
      (`Assoc
          [ "key", `String "only_work"
          ; "tier", `String "scratchpad"
          ; "exact", `Bool true
          ])
  in
  Alcotest.(check bool)
    "not found"
    false
    Yojson.Safe.Util.(json |> member "found" |> to_bool)
;;

(* ── Memory_tools: remember_episode with explicit id ──── *)

let test_remember_episode_explicit_id () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let json =
    execute_ok_json
      tool
      (`Assoc [ "id", `String "my-custom-id"; "action", `String "test action" ])
  in
  Alcotest.(check string)
    "custom id"
    "my-custom-id"
    Yojson.Safe.Util.(json |> member "id" |> to_string)
;;

(* ── Memory_tools: episode outcome variants ───────────── *)

let test_remember_episode_failure_outcome () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let _ =
    execute_ok_json
      tool
      (`Assoc
          [ "action", `String "deploy"
          ; "outcome", `String "failure"
          ; "detail", `String "crashed"
          ])
  in
  ()
;;

let test_remember_episode_neutral_outcome () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let _ =
    execute_ok_json
      tool
      (`Assoc [ "action", `String "idle"; "outcome", `String "neutral" ])
  in
  ()
;;

let test_remember_episode_invalid_outcome () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let msg =
    execute_error tool (`Assoc [ "action", `String "deploy"; "outcome", `String "bogus" ])
  in
  Alcotest.(check bool)
    "mentions outcome"
    true
    (Util.string_contains ~needle:"outcome" msg)
;;

let test_remember_episode_outcome_non_string () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let msg =
    execute_error tool (`Assoc [ "action", `String "deploy"; "outcome", `Int 42 ])
  in
  Alcotest.(check bool) "mentions string" true (Util.string_contains ~needle:"string" msg)
;;

(* ── Memory_tools: parse_string_list_field errors ─────── *)

let test_remember_episode_bad_participants () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let msg =
    execute_error
      tool
      (`Assoc [ "action", `String "deploy"; "participants", `List [ `Int 1 ] ])
  in
  Alcotest.(check bool) "mentions array" true (Util.string_contains ~needle:"array" msg)
;;

let test_remember_episode_participants_non_list () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let msg =
    execute_error
      tool
      (`Assoc [ "action", `String "deploy"; "participants", `String "not-a-list" ])
  in
  Alcotest.(check bool) "mentions array" true (Util.string_contains ~needle:"array" msg)
;;

(* ── Memory_tools: parse_metadata_field error ─────────── *)

let test_remember_episode_bad_metadata () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let msg =
    execute_error
      tool
      (`Assoc [ "action", `String "deploy"; "metadata", `String "not-object" ])
  in
  Alcotest.(check bool) "mentions object" true (Util.string_contains ~needle:"object" msg)
;;

(* ── Memory_tools: salience as int and bad type ───────── *)

let test_remember_episode_salience_int () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let _ =
    execute_ok_json tool (`Assoc [ "action", `String "test"; "salience", `Int 1 ])
  in
  ()
;;

let test_remember_episode_salience_bad () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let msg =
    execute_error tool (`Assoc [ "action", `String "test"; "salience", `String "high" ])
  in
  Alcotest.(check bool) "mentions number" true (Util.string_contains ~needle:"number" msg)
;;

(* ── Memory_tools: missing key ────────────────────────── *)

let test_remember_missing_key () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  let msg = execute_error tool (`Assoc [ "value_json", `String "1" ]) in
  Alcotest.(check bool) "mentions key" true (Util.string_contains ~needle:"key" msg)
;;

(* ── Memory_tools: bad exact field type ───────────────── *)

let test_recall_bad_exact_type () =
  let mem = Memory.create () in
  let tool = Memory_tools.recall mem in
  let msg = execute_error tool (`Assoc [ "key", `String "k"; "exact", `String "true" ]) in
  Alcotest.(check bool)
    "mentions boolean"
    true
    (Util.string_contains ~needle:"boolean" msg)
;;

(* ── Memory_tools: find_procedure not found ───────────── *)

let test_find_procedure_not_found () =
  let mem = Memory.create () in
  let tool = Memory_tools.find_procedure mem in
  let json = execute_ok_json tool (`Assoc [ "pattern", `String "nothing" ]) in
  Alcotest.(check bool)
    "not found"
    false
    Yojson.Safe.Util.(json |> member "found" |> to_bool)
;;

(* ── Memory_tools: with metadata ──────────────────────── *)

let test_remember_episode_with_metadata () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let _ =
    execute_ok_json
      tool
      (`Assoc
          [ "action", `String "deploy"
          ; "metadata", `Assoc [ "env", `String "prod" ]
          ; "salience", `Float 0.9
          ])
  in
  ()
;;

(* ── Memory_access: recall_exact ──────────────────────── *)

let test_acl_recall_exact () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = ReadWrite };
  ignore (Memory.store mem ~tier:Working "k" (json_s "v"));
  match Memory_access.recall_exact acl ~agent:"alice" ~tier:Working "k" with
  | Ok (Some (`String "v")) -> ()
  | _ -> Alcotest.fail "should recall exact"
;;

let test_acl_recall_exact_denied () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  match Memory_access.recall_exact acl ~agent:"alice" ~tier:Working "k" with
  | Error (Denied _) -> ()
  | _ -> Alcotest.fail "should be denied"
;;

(* ── Memory_access: forget ────────────────────────────── *)

let test_acl_forget () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = ReadWrite };
  ignore (Memory.store mem ~tier:Working "k" (json_s "v"));
  (match Memory_access.forget acl ~agent:"alice" ~tier:Working "k" with
   | Ok () -> ()
   | Error _ -> Alcotest.fail "should forget");
  Alcotest.(check bool) "forgotten" true (Memory.recall_exact mem ~tier:Working "k" = None)
;;

let test_acl_forget_denied () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  match Memory_access.forget acl ~agent:"alice" ~tier:Working "k" with
  | Error (Denied _) -> ()
  | _ -> Alcotest.fail "should be denied"
;;

(* ── Memory_access: recall_episodes with grant ────────── *)

let test_acl_recall_episodes_allowed () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Episodic; key_pattern = "*"; permission = ReadWrite };
  Memory.store_episode
    mem
    { id = "e1"
    ; timestamp = 100.0
    ; participants = []
    ; action = "test"
    ; outcome = Neutral
    ; salience = 0.9
    ; metadata = []
    };
  match Memory_access.recall_episodes acl ~agent:"alice" ~now:200.0 () with
  | Ok episodes -> Alcotest.(check int) "1 episode" 1 (List.length episodes)
  | Error _ -> Alcotest.fail "should be allowed"
;;

(* ── Memory_access: memory accessor ───────────────────── *)

let test_acl_memory_accessor () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  let underlying = Memory_access.memory acl in
  ignore (Memory.store underlying ~tier:Working "direct" (json_s "val"));
  match Memory.recall_exact underlying ~tier:Working "direct" with
  | Some (`String "val") -> ()
  | _ -> Alcotest.fail "should access underlying memory"
;;

(* ── Memory_access: permission edge cases ─────────────── *)

let test_permission_readwrite_needed () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = Read };
  Alcotest.(check bool)
    "ReadWrite not satisfied by Read"
    false
    (Memory_access.check acl ~agent:"alice" ~tier:Working ~key:"k" ReadWrite)
;;

let test_permission_readwrite_grant () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = ReadWrite };
  Alcotest.(check bool)
    "ReadWrite satisfied"
    true
    (Memory_access.check acl ~agent:"alice" ~tier:Working ~key:"k" ReadWrite)
;;

(* ── Memory_access: error string all tiers ────────────── *)

let test_access_error_all_tiers () =
  let tiers = [ Memory.Scratchpad; Working; Episodic; Procedural; Long_term ] in
  List.iter
    (fun tier ->
       let err =
         Memory_access.Denied { agent_name = "bob"; tier; key = "k"; needed = Read }
       in
       let s = Memory_access.access_error_to_string err in
       Alcotest.(check bool) "non-empty" true (String.length s > 0))
    tiers
;;

(* ── Memory_access: revoke edge cases ─────────────────── *)

let test_acl_revoke_other_tiers_remain () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = ReadWrite };
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Scratchpad; key_pattern = "*"; permission = Read };
  Memory_access.revoke acl ~agent_name:"alice" ~tier:Working;
  let policies = Memory_access.policies_for acl "alice" in
  Alcotest.(check int) "1 remaining" 1 (List.length policies)
;;

let test_acl_revoke_none () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.revoke acl ~agent_name:"nobody" ~tier:Working;
  Alcotest.(check int) "still 0" 0 (List.length (Memory_access.policies_for acl "nobody"))
;;

(* ── Memory_tools: ACL-backed tools ───────────────────── *)

let test_recall_acl_allowed () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Working; key_pattern = "*"; permission = ReadWrite };
  ignore (Memory.store mem ~tier:Working "k" (json_s "v"));
  let tool = Memory_tools.recall_acl acl ~agent_name:"alice" in
  let json = execute_ok_json tool (`Assoc [ "key", `String "k" ]) in
  Alcotest.(check bool) "found" true Yojson.Safe.Util.(json |> member "found" |> to_bool)
;;

let test_recall_acl_denied () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  let tool = Memory_tools.recall_acl acl ~agent_name:"bob" in
  let msg = execute_error tool (`Assoc [ "key", `String "k" ]) in
  Alcotest.(check bool) "denied" true (Util.string_contains ~needle:"Access denied" msg)
;;

let test_remember_episode_acl_allowed () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Episodic; key_pattern = "*"; permission = ReadWrite };
  let tool = Memory_tools.remember_episode_acl acl ~agent_name:"alice" in
  let json = execute_ok_json tool (`Assoc [ "action", `String "test" ]) in
  Alcotest.(check bool) "ok" true Yojson.Safe.Util.(json |> member "ok" |> to_bool)
;;

let test_remember_episode_acl_denied () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  let tool = Memory_tools.remember_episode_acl acl ~agent_name:"bob" in
  let msg = execute_error tool (`Assoc [ "action", `String "test" ]) in
  Alcotest.(check bool) "denied" true (Util.string_contains ~needle:"Access denied" msg)
;;

let test_find_procedure_acl_allowed () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  Memory_access.grant
    acl
    { agent_name = "alice"; tier = Procedural; key_pattern = "*"; permission = ReadWrite };
  Memory.store_procedure
    mem
    { id = "pr1"
    ; pattern = "deploy"
    ; action = "retry"
    ; success_count = 5
    ; failure_count = 0
    ; confidence = 1.0
    ; last_used = 0.0
    ; metadata = []
    };
  let tool = Memory_tools.find_procedure_acl acl ~agent_name:"alice" in
  let json = execute_ok_json tool (`Assoc [ "pattern", `String "deploy" ]) in
  Alcotest.(check bool) "found" true Yojson.Safe.Util.(json |> member "found" |> to_bool)
;;

let test_find_procedure_acl_denied () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  let tool = Memory_tools.find_procedure_acl acl ~agent_name:"bob" in
  let msg = execute_error tool (`Assoc [ "pattern", `String "deploy" ]) in
  Alcotest.(check bool) "denied" true (Util.string_contains ~needle:"Access denied" msg)
;;

(* ── Test runner ───────────────────────────────────────── *)

let () =
  Alcotest.run
    "Memory_coverage"
    [ ( "recall_no_fallback"
      , [ Alcotest.test_case "episodic miss" `Quick test_recall_episodic_returns_none
        ; Alcotest.test_case "procedural miss" `Quick test_recall_procedural_returns_none
        ] )
    ; ( "recall_exact_long_term"
      , [ Alcotest.test_case
            "with backend"
            `Quick
            test_recall_exact_long_term_with_backend
        ; Alcotest.test_case "no backend" `Quick test_recall_exact_long_term_no_backend
        ] )
    ; ( "scratchpad_chain"
      , [ Alcotest.test_case
            "with backend"
            `Quick
            test_scratchpad_recall_chain_with_backend
        ; Alcotest.test_case "no backend" `Quick test_scratchpad_recall_chain_no_backend
        ] )
    ; ( "long_term_recall"
      , [ Alcotest.test_case
            "no backend none"
            `Quick
            test_long_term_recall_no_backend_none
        ] )
    ; ( "entries"
      , [ Alcotest.test_case "scratchpad_entries" `Quick test_scratchpad_entries ] )
    ; ( "episodic"
      , [ Alcotest.test_case "success outcome" `Quick test_episode_success_outcome
        ; Alcotest.test_case "failure outcome" `Quick test_episode_failure_outcome
        ; Alcotest.test_case "neutral outcome" `Quick test_episode_neutral_outcome
        ; Alcotest.test_case "with metadata" `Quick test_episode_with_metadata
        ; Alcotest.test_case "recall multiple" `Quick test_recall_episodes_multiple
        ] )
    ; ( "procedural"
      , [ Alcotest.test_case "empty pattern" `Quick test_procedure_matching_empty_pattern
        ; Alcotest.test_case "no match" `Quick test_procedure_matching_no_match
        ; Alcotest.test_case "zero total" `Quick test_procedure_zero_total
        ] )
    ; ( "missing_ops"
      , [ Alcotest.test_case "record_success missing" `Quick test_record_success_missing
        ; Alcotest.test_case "record_failure missing" `Quick test_record_failure_missing
        ; Alcotest.test_case "boost missing" `Quick test_boost_salience_missing
        ] )
    ; ( "tools_tier"
      , [ Alcotest.test_case "long-term dash" `Quick test_tier_long_term_dash
        ; Alcotest.test_case "invalid string" `Quick test_tier_invalid_string
        ; Alcotest.test_case "non-string" `Quick test_tier_non_string
        ] )
    ; ( "tools_recall"
      , [ Alcotest.test_case "exact flag" `Quick test_recall_exact_flag
        ; Alcotest.test_case "bad exact type" `Quick test_recall_bad_exact_type
        ; Alcotest.test_case "missing key" `Quick test_remember_missing_key
        ; Alcotest.test_case "not found" `Quick test_find_procedure_not_found
        ] )
    ; ( "tools_episode"
      , [ Alcotest.test_case "explicit id" `Quick test_remember_episode_explicit_id
        ; Alcotest.test_case
            "failure outcome"
            `Quick
            test_remember_episode_failure_outcome
        ; Alcotest.test_case
            "neutral outcome"
            `Quick
            test_remember_episode_neutral_outcome
        ; Alcotest.test_case
            "invalid outcome"
            `Quick
            test_remember_episode_invalid_outcome
        ; Alcotest.test_case
            "outcome non-string"
            `Quick
            test_remember_episode_outcome_non_string
        ; Alcotest.test_case
            "bad participants"
            `Quick
            test_remember_episode_bad_participants
        ; Alcotest.test_case
            "participants non-list"
            `Quick
            test_remember_episode_participants_non_list
        ; Alcotest.test_case "bad metadata" `Quick test_remember_episode_bad_metadata
        ; Alcotest.test_case "with metadata" `Quick test_remember_episode_with_metadata
        ; Alcotest.test_case "salience int" `Quick test_remember_episode_salience_int
        ; Alcotest.test_case "salience bad" `Quick test_remember_episode_salience_bad
        ] )
    ; ( "acl_exact"
      , [ Alcotest.test_case "allowed" `Quick test_acl_recall_exact
        ; Alcotest.test_case "denied" `Quick test_acl_recall_exact_denied
        ] )
    ; ( "acl_forget"
      , [ Alcotest.test_case "allowed" `Quick test_acl_forget
        ; Alcotest.test_case "denied" `Quick test_acl_forget_denied
        ] )
    ; ( "acl_episodes"
      , [ Alcotest.test_case "allowed" `Quick test_acl_recall_episodes_allowed ] )
    ; ( "acl_permission"
      , [ Alcotest.test_case "ReadWrite not Read" `Quick test_permission_readwrite_needed
        ; Alcotest.test_case "ReadWrite grant" `Quick test_permission_readwrite_grant
        ; Alcotest.test_case "error all tiers" `Quick test_access_error_all_tiers
        ; Alcotest.test_case "memory accessor" `Quick test_acl_memory_accessor
        ; Alcotest.test_case
            "revoke other remain"
            `Quick
            test_acl_revoke_other_tiers_remain
        ; Alcotest.test_case "revoke none" `Quick test_acl_revoke_none
        ] )
    ; ( "acl_tools"
      , [ Alcotest.test_case "recall allowed" `Quick test_recall_acl_allowed
        ; Alcotest.test_case "recall denied" `Quick test_recall_acl_denied
        ; Alcotest.test_case "episode allowed" `Quick test_remember_episode_acl_allowed
        ; Alcotest.test_case "episode denied" `Quick test_remember_episode_acl_denied
        ; Alcotest.test_case "procedure allowed" `Quick test_find_procedure_acl_allowed
        ; Alcotest.test_case "procedure denied" `Quick test_find_procedure_acl_denied
        ] )
    ]
;;
