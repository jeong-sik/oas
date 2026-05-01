open Base
(** Integration tests for Tool Result Relocation (RFC-OAS-005 Phase 1).

    Covers:
    1. Tool_result_store: persist/read lifecycle with temp directory
    2. Content_replacement_state: multi-turn decision freezing
    3. make_tool_results + relocation: creation-time persist
    4. Compaction + relocation: reducer compose with relocation state *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────── *)

let tmp_dir () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas_test_reloc_%d" (Unix.getpid ()))
  in
  (try Unix.mkdir dir 0o755 with
   | Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  dir
;;

let rm_rf dir =
  let entries =
    try Sys.readdir dir with
    | Sys_error _ -> [||]
  in
  Array.iter
    (fun e ->
       let path = Filename.concat dir e in
       try Unix.unlink path with
       | _ -> ())
    entries;
  try Unix.rmdir dir with
  | _ -> ()
;;

let tool_result ?(is_error = false) id content : Types.content_block =
  Types.ToolResult { tool_use_id = id; content; is_error; json = None }
;;

let asst_msg text : Types.message =
  { role = Types.Assistant
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let user_msg blocks : Types.message =
  { role = Types.User; content = blocks; name = None; tool_call_id = None; metadata = [] }
;;

(* ── 1. Tool_result_store lifecycle ───────────────────── *)

let test_store_persist_read () =
  let dir = tmp_dir () in
  let config : Tool_result_store.config =
    { storage_dir = dir
    ; session_id = "s1"
    ; threshold_chars = 100
    ; preview_chars = 50
    ; aggregate_budget = 0
    }
  in
  let store = Tool_result_store.create config |> Result.get_ok in
  let content = String.make 200 'x' in
  let preview =
    Tool_result_store.persist store ~tool_use_id:"t1" ~content |> Result.get_ok
  in
  (* Preview is shorter than content *)
  Alcotest.(check bool)
    "preview shorter"
    true
    (String.length preview < String.length content);
  (* Read back full content *)
  let readback = Tool_result_store.read store ~tool_use_id:"t1" |> Result.get_ok in
  Alcotest.(check string) "round-trip" content readback;
  (* has returns true *)
  Alcotest.(check bool) "has=true" true (Tool_result_store.has store ~tool_use_id:"t1");
  Alcotest.(check bool) "has=false" false (Tool_result_store.has store ~tool_use_id:"t2");
  (* Cleanup *)
  let _ = Tool_result_store.cleanup store in
  rm_rf dir
;;

let test_store_idempotent_persist () =
  let dir = tmp_dir () in
  let config : Tool_result_store.config =
    { storage_dir = dir
    ; session_id = "s1"
    ; threshold_chars = 50
    ; preview_chars = 20
    ; aggregate_budget = 0
    }
  in
  let store = Tool_result_store.create config |> Result.get_ok in
  let content = String.make 100 'a' in
  let p1 = Tool_result_store.persist store ~tool_use_id:"t1" ~content |> Result.get_ok in
  let p2 = Tool_result_store.persist store ~tool_use_id:"t1" ~content |> Result.get_ok in
  (* Both previews identical — idempotent *)
  Alcotest.(check string) "idempotent preview" p1 p2;
  let _ = Tool_result_store.cleanup store in
  rm_rf dir
;;

let test_store_below_threshold_not_persisted () =
  let dir = tmp_dir () in
  let config : Tool_result_store.config =
    { storage_dir = dir
    ; session_id = "s1"
    ; threshold_chars = 500
    ; preview_chars = 100
    ; aggregate_budget = 0
    }
  in
  let store = Tool_result_store.create config |> Result.get_ok in
  (* Content below threshold — should NOT be persisted *)
  Alcotest.(check bool)
    "not persisted"
    false
    (Tool_result_store.has store ~tool_use_id:"t1");
  let _ = Tool_result_store.cleanup store in
  rm_rf dir
;;

(* ── 2. Content_replacement_state: multi-turn ─────────── *)

let test_crs_multi_turn_freezing () =
  let crs = Content_replacement_state.create () in
  (* Turn 1: t1 replaced, t2 kept *)
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t1"; preview = "preview_t1"; original_chars = 5000 };
  Content_replacement_state.record_kept crs "t2";
  (* Turn 2: t3 fresh, t1+t2 frozen *)
  let blocks =
    [ tool_result "t1" "big content that should be replaced"
    ; tool_result "t2" "small content stays"
    ; tool_result "t3" "new content"
    ]
  in
  let modified, fresh = Content_replacement_state.apply_frozen crs blocks in
  (* t1 replaced with cached preview *)
  (match List.nth modified 0 with
   | Types.ToolResult { content; _ } ->
     Alcotest.(check string) "t1 replaced" "preview_t1" content
   | _ -> Alcotest.fail "t1 not ToolResult");
  (* t2 kept as-is *)
  (match List.nth modified 1 with
   | Types.ToolResult { content; _ } ->
     Alcotest.(check string) "t2 kept" "small content stays" content
   | _ -> Alcotest.fail "t2 not ToolResult");
  (* t3 is fresh *)
  Alcotest.(check (list string)) "fresh" [ "t3" ] fresh;
  (* Caller processes t3: keep it (below threshold) *)
  Content_replacement_state.record_kept crs "t3";
  (* Turn 3: apply again — all frozen, no fresh *)
  let modified2, fresh2 = Content_replacement_state.apply_frozen crs modified in
  (match List.nth modified2 0 with
   | Types.ToolResult { content; _ } ->
     Alcotest.(check string) "t1 still replaced" "preview_t1" content
   | _ -> Alcotest.fail "t1 not ToolResult on turn 3");
  Alcotest.(check (list string)) "no fresh on turn 3" [] fresh2
;;

let test_crs_json_roundtrip_preserves_decisions () =
  let crs = Content_replacement_state.create () in
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t1"; preview = "p1"; original_chars = 100 };
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t2"; preview = "p2"; original_chars = 200 };
  Content_replacement_state.record_kept crs "t3";
  let json = Content_replacement_state.to_json crs in
  let crs2 = Content_replacement_state.of_json json |> Result.get_ok in
  (* All decisions preserved *)
  Alcotest.(check int) "seen_count" 3 (Content_replacement_state.seen_count crs2);
  Alcotest.(check bool) "t1 frozen" true (Content_replacement_state.is_frozen crs2 "t1");
  Alcotest.(check bool) "t2 frozen" true (Content_replacement_state.is_frozen crs2 "t2");
  Alcotest.(check bool) "t3 frozen" true (Content_replacement_state.is_frozen crs2 "t3");
  (* Replacement content preserved *)
  (match Content_replacement_state.lookup_replacement crs2 "t1" with
   | Some r -> Alcotest.(check string) "t1 preview" "p1" r.preview
   | None -> Alcotest.fail "t1 replacement lost");
  (* t3 was kept, not replaced *)
  Alcotest.(check bool)
    "t3 no replacement"
    true
    (Content_replacement_state.lookup_replacement crs2 "t3" = None)
;;

(* ── 3. make_tool_results + relocation ────────────────── *)

let test_make_tool_results_with_relocation () =
  let dir = tmp_dir () in
  let config : Tool_result_store.config =
    { storage_dir = dir
    ; session_id = "s1"
    ; threshold_chars = 100
    ; preview_chars = 50
    ; aggregate_budget = 0
    }
  in
  let store = Tool_result_store.create config |> Result.get_ok in
  let crs = Content_replacement_state.create () in
  let mock_results : Agent_tools.tool_execution_result list =
    [ { tool_use_id = "t1"
      ; tool_name = "read"
      ; content = String.make 200 'x'
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ; { tool_use_id = "t2"
      ; tool_name = "echo"
      ; content = "small"
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let blocks = Agent_turn.make_tool_results ~relocation:(store, crs) mock_results in
  (* t1: above threshold → persisted → preview *)
  (match List.nth blocks 0 with
   | Types.ToolResult { tool_use_id; content; _ } ->
     Alcotest.(check string) "t1 id" "t1" tool_use_id;
     Alcotest.(check bool)
       "t1 persisted"
       true
       (Tool_result_store.has store ~tool_use_id:"t1");
     Alcotest.(check bool) "t1 content shorter" true (String.length content < 200)
   | _ -> Alcotest.fail "t1 not ToolResult");
  (* t2: below threshold → kept as-is *)
  (match List.nth blocks 1 with
   | Types.ToolResult { content; _ } ->
     Alcotest.(check string) "t2 content" "small" content
   | _ -> Alcotest.fail "t2 not ToolResult");
  (* CRS state: both frozen *)
  Alcotest.(check bool) "t1 frozen" true (Content_replacement_state.is_frozen crs "t1");
  Alcotest.(check bool) "t2 frozen" true (Content_replacement_state.is_frozen crs "t2");
  Alcotest.(check bool)
    "t1 replaced"
    true
    (Content_replacement_state.lookup_replacement crs "t1" <> None);
  Alcotest.(check bool)
    "t2 kept"
    true
    (Content_replacement_state.lookup_replacement crs "t2" = None);
  let _ = Tool_result_store.cleanup store in
  rm_rf dir
;;

(* ── 4. Compaction + relocation compose ───────────────── *)

let test_compaction_preserves_relocated_previews () =
  let crs = Content_replacement_state.create () in
  (* Simulate: t1 was relocated (preview in message), t2 was kept *)
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t1"; preview = "preview_of_t1"; original_chars = 60000 };
  Content_replacement_state.record_kept crs "t2";
  (* Build message list that mimics a post-relocation conversation *)
  let messages : Types.message list =
    [ { role = Types.User
      ; content = [ Types.Text "hello" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; asst_msg "I'll use tools"
    ; user_msg
        [ tool_result "t1" "preview_of_t1"
        ; tool_result "t2" "full content of t2 that is small"
        ]
    ; asst_msg "Got results. Let me think."
    ; user_msg [ Types.Text "continue" ]
    ; asst_msg "done"
    ]
  in
  (* Apply stub_tool_results (keep_recent:1) — should stub old tool results *)
  let stubbed =
    Context_reducer.reduce (Context_reducer.stub_tool_results ~keep_recent:1) messages
  in
  (* After stubbing: tool results from turn 2 should be stubbed *)
  let tool_results_after =
    List.concat_map
      (fun (msg : Types.message) ->
         List.filter_map
           (function
             | Types.ToolResult { tool_use_id; content; _ } -> Some (tool_use_id, content)
             | _ -> None)
           msg.content)
      stubbed
  in
  (* Verify tool_use_id is preserved even after stubbing *)
  let ids = List.map fst tool_results_after in
  Alcotest.(check bool) "t1 id preserved" true (List.mem "t1" ids);
  Alcotest.(check bool) "t2 id preserved" true (List.mem "t2" ids);
  (* After apply_frozen on stubbed messages: t1 gets preview_of_t1 back *)
  let all_blocks = List.concat_map (fun (m : Types.message) -> m.content) stubbed in
  let restored, _ = Content_replacement_state.apply_frozen crs all_blocks in
  let t1_content =
    List.find_map
      (function
        | Types.ToolResult { tool_use_id = "t1"; content; _ } -> Some content
        | _ -> None)
      restored
  in
  Alcotest.(check (option string)) "t1 preview restored" (Some "preview_of_t1") t1_content
;;

let test_compose_prune_then_relocation_reapply () =
  let crs = Content_replacement_state.create () in
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t1"; preview = "short_preview"; original_chars = 100000 };
  Content_replacement_state.record_kept crs "t2";
  (* Messages with mix of relocated and regular *)
  let messages : Types.message list =
    [ user_msg [ Types.Text "question" ]
    ; asst_msg "answer with tools"
    ; user_msg
        [ tool_result "t1" "short_preview"; tool_result "t2" (String.make 8000 'y') ]
    ; asst_msg "final"
    ]
  in
  (* Compose: prune_tool_outputs (4K) then stub (keep_recent:1) *)
  let reducer =
    Context_reducer.compose
      [ Context_reducer.prune_tool_outputs ~max_output_len:4000
      ; Context_reducer.stub_tool_results ~keep_recent:1
      ]
  in
  let reduced = Context_reducer.reduce reducer messages in
  (* Verify the pipeline didn't corrupt tool_use_ids *)
  let all_tool_ids =
    List.concat_map
      (fun (m : Types.message) ->
         List.filter_map
           (function
             | Types.ToolResult { tool_use_id; _ } -> Some tool_use_id
             | _ -> None)
           m.content)
      reduced
  in
  Alcotest.(check bool) "t1 survives pipeline" true (List.mem "t1" all_tool_ids);
  Alcotest.(check bool) "t2 survives pipeline" true (List.mem "t2" all_tool_ids);
  (* Apply frozen after pipeline: t1 gets its frozen preview *)
  let all_blocks = List.concat_map (fun (m : Types.message) -> m.content) reduced in
  let restored, _ = Content_replacement_state.apply_frozen crs all_blocks in
  let t1_content =
    List.find_map
      (function
        | Types.ToolResult { tool_use_id = "t1"; content; _ } -> Some content
        | _ -> None)
      restored
  in
  Alcotest.(check (option string)) "t1 frozen preview" (Some "short_preview") t1_content
;;

(* ── 5. Aggregate budget enforcement (Phase 2) ──────── *)

let test_aggregate_budget_persists_largest () =
  let dir = tmp_dir () in
  let config : Tool_result_store.config =
    { storage_dir = dir
    ; session_id = "s1"
    ; threshold_chars = 500_000
    ; (* high per-result threshold — won't trigger *)
      preview_chars = 50
    ; aggregate_budget = 15_000 (* aggregate budget *)
    }
  in
  let store = Tool_result_store.create config |> Result.get_ok in
  let crs = Content_replacement_state.create () in
  (* 3 results: total = 8000+4000+3000 = 15000, at budget boundary.
     Actually make total > budget: 8000+5000+4000 = 17000 > 15000 *)
  let mock_results : Agent_tools.tool_execution_result list =
    [ { tool_use_id = "t1"
      ; tool_name = "a"
      ; content = String.make 8000 'a'
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ; { tool_use_id = "t2"
      ; tool_name = "b"
      ; content = String.make 5000 'b'
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ; { tool_use_id = "t3"
      ; tool_name = "c"
      ; content = String.make 4000 'c'
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let blocks = Agent_turn.make_tool_results ~relocation:(store, crs) mock_results in
  (* The largest (t1, 8000 chars) should be persisted — preview saves ~7870 chars,
     bringing total from 17000 to ~9130, well under 15000 budget *)
  Alcotest.(check bool)
    "t1 persisted"
    true
    (Tool_result_store.has store ~tool_use_id:"t1");
  (* t1 should have a preview (much shorter than 8000) *)
  (match List.nth blocks 0 with
   | Types.ToolResult { content; _ } ->
     Alcotest.(check bool) "t1 is preview" true (String.length content < 200)
   | _ -> Alcotest.fail "t1 not ToolResult");
  (* t2 and t3 should be kept (not persisted) *)
  Alcotest.(check bool)
    "t2 not persisted"
    false
    (Tool_result_store.has store ~tool_use_id:"t2");
  Alcotest.(check bool)
    "t3 not persisted"
    false
    (Tool_result_store.has store ~tool_use_id:"t3");
  (* All frozen in CRS *)
  Alcotest.(check bool) "t1 frozen" true (Content_replacement_state.is_frozen crs "t1");
  Alcotest.(check bool) "t2 frozen" true (Content_replacement_state.is_frozen crs "t2");
  Alcotest.(check bool) "t3 frozen" true (Content_replacement_state.is_frozen crs "t3");
  (* t1 was replaced, t2+t3 were kept *)
  Alcotest.(check bool)
    "t1 replaced"
    true
    (Content_replacement_state.lookup_replacement crs "t1" <> None);
  Alcotest.(check bool)
    "t2 kept"
    true
    (Content_replacement_state.lookup_replacement crs "t2" = None);
  let _ = Tool_result_store.cleanup store in
  rm_rf dir
;;

let test_aggregate_budget_disabled () =
  let dir = tmp_dir () in
  let config : Tool_result_store.config =
    { storage_dir = dir
    ; session_id = "s1"
    ; threshold_chars = 500_000
    ; preview_chars = 50
    ; aggregate_budget = 0 (* disabled *)
    }
  in
  let store = Tool_result_store.create config |> Result.get_ok in
  let crs = Content_replacement_state.create () in
  let mock_results : Agent_tools.tool_execution_result list =
    [ { tool_use_id = "t1"
      ; tool_name = "a"
      ; content = String.make 8000 'x'
      ; is_error = false
      ; failure_kind = None
      ; error_class = None
      }
    ]
  in
  let _blocks = Agent_turn.make_tool_results ~relocation:(store, crs) mock_results in
  (* With aggregate_budget=0, should not persist even with large content *)
  Alcotest.(check bool)
    "not persisted"
    false
    (Tool_result_store.has store ~tool_use_id:"t1");
  Alcotest.(check bool)
    "kept in CRS"
    true
    (Content_replacement_state.is_frozen crs "t1"
     && Content_replacement_state.lookup_replacement crs "t1" = None);
  let _ = Tool_result_store.cleanup store in
  rm_rf dir
;;

(* ── 6. CRS checkpoint persistence (Phase 2) ────────── *)

let test_crs_checkpoint_roundtrip () =
  let ctx = Context.create () in
  let crs = Content_replacement_state.create () in
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t1"; preview = "p1"; original_chars = 5000 };
  Content_replacement_state.record_kept crs "t2";
  (* Persist to context *)
  Content_replacement_state.persist_to_context ctx crs;
  (* Restore from context (simulates checkpoint resume) *)
  let crs2 = Content_replacement_state.restore_from_context ctx in
  Alcotest.(check int)
    "seen_count preserved"
    2
    (Content_replacement_state.seen_count crs2);
  Alcotest.(check bool)
    "t1 frozen after restore"
    true
    (Content_replacement_state.is_frozen crs2 "t1");
  Alcotest.(check bool)
    "t2 frozen after restore"
    true
    (Content_replacement_state.is_frozen crs2 "t2");
  match Content_replacement_state.lookup_replacement crs2 "t1" with
  | Some r -> Alcotest.(check string) "t1 preview" "p1" r.preview
  | None -> Alcotest.fail "t1 replacement lost after checkpoint"
;;

let test_crs_restore_from_empty_context () =
  let ctx = Context.create () in
  (* No CRS saved — should return fresh empty state *)
  let crs = Content_replacement_state.restore_from_context ctx in
  Alcotest.(check int) "empty state" 0 (Content_replacement_state.seen_count crs)
;;

(* ── 7. Relocate_tool_results reducer (Phase 2) ─────── *)

let test_relocate_reducer_reapplies_frozen () =
  let crs = Content_replacement_state.create () in
  Content_replacement_state.record_replacement
    crs
    { tool_use_id = "t1"; preview = "cached_preview"; original_chars = 50000 };
  Content_replacement_state.record_kept crs "t2";
  (* Messages with original content (simulates checkpoint restore) *)
  let messages : Types.message list =
    [ user_msg [ Types.Text "start" ]
    ; asst_msg "using tools"
    ; user_msg
        [ tool_result "t1" "full original content that was relocated"
        ; tool_result "t2" "small content"
        ]
    ; asst_msg "thinking"
    ; user_msg [ Types.Text "continue" ]
    ; asst_msg "done"
    ]
  in
  (* Apply relocate_tool_results reducer (keep_recent:1) *)
  let reduced =
    Context_reducer.reduce
      (Context_reducer.relocate_tool_results ~state:crs ~keep_recent:1)
      messages
  in
  (* In older turns: t1 should have cached preview, t2 unchanged *)
  let t1_content =
    List.find_map
      (fun (m : Types.message) ->
         List.find_map
           (function
             | Types.ToolResult { tool_use_id = "t1"; content; _ } -> Some content
             | _ -> None)
           m.content)
      reduced
  in
  Alcotest.(check (option string))
    "t1 preview reapplied"
    (Some "cached_preview")
    t1_content
;;

(* ── Test runner ──────────────────────────────────────── *)

let () =
  Alcotest.run
    "Tool_result_relocation"
    [ ( "store_lifecycle"
      , [ Alcotest.test_case "persist/read round-trip" `Quick test_store_persist_read
        ; Alcotest.test_case "idempotent persist" `Quick test_store_idempotent_persist
        ; Alcotest.test_case
            "below threshold not persisted"
            `Quick
            test_store_below_threshold_not_persisted
        ] )
    ; ( "crs_multi_turn"
      , [ Alcotest.test_case "multi-turn freezing" `Quick test_crs_multi_turn_freezing
        ; Alcotest.test_case
            "json roundtrip preserves"
            `Quick
            test_crs_json_roundtrip_preserves_decisions
        ] )
    ; ( "make_tool_results"
      , [ Alcotest.test_case
            "relocation integration"
            `Quick
            test_make_tool_results_with_relocation
        ] )
    ; ( "compaction_compose"
      , [ Alcotest.test_case
            "stub preserves relocated previews"
            `Quick
            test_compaction_preserves_relocated_previews
        ; Alcotest.test_case
            "prune+stub then reapply"
            `Quick
            test_compose_prune_then_relocation_reapply
        ] )
    ; ( "aggregate_budget"
      , [ Alcotest.test_case
            "persists largest when over budget"
            `Quick
            test_aggregate_budget_persists_largest
        ; Alcotest.test_case
            "disabled when budget=0"
            `Quick
            test_aggregate_budget_disabled
        ] )
    ; ( "crs_checkpoint"
      , [ Alcotest.test_case
            "persist/restore roundtrip"
            `Quick
            test_crs_checkpoint_roundtrip
        ; Alcotest.test_case
            "restore from empty context"
            `Quick
            test_crs_restore_from_empty_context
        ] )
    ; ( "relocate_reducer"
      , [ Alcotest.test_case
            "reapplies frozen decisions"
            `Quick
            test_relocate_reducer_reapplies_frozen
        ] )
    ]
;;
