(** Tests for Tool_op -- algebraic tool set operations. *)

open Alcotest
open Agent_sdk
open Tool_op

let sl = list string

let check_set_eq msg expected actual =
  let sort = List.sort String.compare in
  check sl msg (sort expected) (sort actual)

let base = ["a"; "b"; "c"]

(* ── apply ────────────────────────────────────────────────── *)

let test_apply_keep_all () =
  check sl "identity" base (Tool_op.apply Keep_all base)

let test_apply_clear_all () =
  check sl "empty" [] (Tool_op.apply Clear_all base)

let test_apply_add () =
  check sl "union" ["a"; "b"; "c"; "d"]
    (Tool_op.apply (Add ["d"]) base)

let test_apply_add_duplicate () =
  check sl "no dup" ["a"; "b"; "c"]
    (Tool_op.apply (Add ["a"]) base)

let test_apply_add_empty () =
  check sl "identity" base
    (Tool_op.apply (Add []) base)

let test_apply_remove () =
  check sl "diff" ["a"; "c"]
    (Tool_op.apply (Remove ["b"]) base)

let test_apply_remove_absent () =
  check sl "no change" base
    (Tool_op.apply (Remove ["x"]) base)

let test_apply_remove_empty () =
  check sl "identity" base
    (Tool_op.apply (Remove []) base)

let test_apply_replace_with () =
  check sl "replaced" ["x"; "y"]
    (Tool_op.apply (Replace_with ["x"; "y"]) base)

let test_apply_replace_with_empty () =
  check sl "empty" []
    (Tool_op.apply (Replace_with []) base)

let test_apply_intersect () =
  check sl "intersection" ["a"; "c"]
    (Tool_op.apply (Intersect_with ["a"; "c"; "z"]) base)

let test_apply_intersect_empty () =
  check sl "empty" []
    (Tool_op.apply (Intersect_with []) base)

let test_apply_intersect_disjoint () =
  check sl "empty" []
    (Tool_op.apply (Intersect_with ["x"; "y"]) base)

let test_apply_seq () =
  check sl "sequential" ["a"; "c"; "d"]
    (Tool_op.apply (Seq [Remove ["b"]; Add ["d"]]) base)

let test_apply_seq_empty () =
  check sl "identity" base
    (Tool_op.apply (Seq []) base)

let test_apply_add_to_empty () =
  check sl "add to empty" ["x"]
    (Tool_op.apply (Add ["x"]) [])

let test_apply_remove_from_empty () =
  check sl "remove from empty" []
    (Tool_op.apply (Remove ["x"]) [])

let test_apply_dedup_current () =
  check sl "deduped input" ["a"; "b"]
    (Tool_op.apply Keep_all ["a"; "b"; "a"])

let test_apply_remove_dedup_current () =
  check sl "deduped" ["a"]
    (Tool_op.apply (Remove ["b"]) ["a"; "b"; "a"])

(* ── apply_to_tool_set ────────────────────────────────────── *)

let make_tool name =
  Tool.create ~name ~description:name ~parameters:[] (fun _ ->
    Ok { Types.content = name })

let test_apply_to_tool_set_remove () =
  let ts = Tool_set.of_list [make_tool "a"; make_tool "b"; make_tool "c"] in
  let result = Tool_op.apply_to_tool_set (Remove ["b"]) ts in
  check_set_eq "removed" ["a"; "c"] (Tool_set.names result)

let test_apply_to_tool_set_add_existing () =
  let ts = Tool_set.of_list [make_tool "a"; make_tool "b"] in
  let result = Tool_op.apply_to_tool_set (Add ["c"]) ts in
  (* "c" not in original Tool_set, so Add has no effect *)
  check_set_eq "unchanged" ["a"; "b"] (Tool_set.names result)

let test_apply_to_tool_set_intersect () =
  let ts = Tool_set.of_list [make_tool "a"; make_tool "b"; make_tool "c"] in
  let result = Tool_op.apply_to_tool_set (Intersect_with ["a"; "c"]) ts in
  check_set_eq "intersected" ["a"; "c"] (Tool_set.names result)

(* ── compose ──────────────────────────────────────────────── *)

let test_compose_empty () =
  check bool "Keep_all" true
    (Tool_op.equal (Tool_op.compose []) Keep_all)

let test_compose_single () =
  let op = Add ["x"] in
  check bool "unwrap" true
    (Tool_op.equal (Tool_op.compose [op]) op)

let test_compose_flatten () =
  let result = Tool_op.compose [Seq [Add ["a"]; Add ["b"]]; Remove ["c"]] in
  match result with
  | Seq [Add _; Add _; Remove _] -> ()
  | _ -> fail (Printf.sprintf "expected flat Seq, got %s"
    (Yojson.Safe.to_string (Tool_op.to_yojson result)))

let test_compose_identity_elimination () =
  let op = Remove ["x"] in
  let result = Tool_op.compose [Keep_all; op; Keep_all] in
  check bool "unwrap" true (Tool_op.equal result op)

let test_compose_all_identity () =
  let result = Tool_op.compose [Add []; Remove []] in
  check bool "Keep_all" true (Tool_op.equal result Keep_all)

let test_compose_deep_flatten () =
  let result = Tool_op.compose [Seq [Seq [Add ["a"]]]; Remove ["b"]] in
  match result with
  | Seq [Add _; Remove _] -> ()
  | _ -> fail (Printf.sprintf "expected deep flatten, got %s"
    (Yojson.Safe.to_string (Tool_op.to_yojson result)))

(* ── to_tool_filter ───────────────────────────────────────── *)

let test_to_tool_filter () =
  let filter = Tool_op.to_tool_filter (Remove ["b"]) ["a"; "b"; "c"] in
  match filter with
  | Guardrails.AllowList names ->
    check_set_eq "filtered" ["a"; "c"] names
  | _ -> fail "expected AllowList"

let test_to_tool_filter_identity () =
  let filter = Tool_op.to_tool_filter Keep_all ["a"; "b"] in
  match filter with
  | Guardrails.AllowList names ->
    check_set_eq "identity" ["a"; "b"] names
  | _ -> fail "expected AllowList"

(* ── is_identity ──────────────────────────────────────────── *)

let test_is_identity_keep_all () =
  check bool "Keep_all" true (Tool_op.is_identity Keep_all)

let test_is_identity_add_empty () =
  check bool "Add []" true (Tool_op.is_identity (Add []))

let test_is_identity_remove_empty () =
  check bool "Remove []" true (Tool_op.is_identity (Remove []))

let test_is_identity_add_nonempty () =
  check bool "Add [x]" false (Tool_op.is_identity (Add ["x"]))

let test_is_identity_seq_recursive () =
  check bool "Seq of identities" true
    (Tool_op.is_identity (Seq [Keep_all; Add []; Remove []]))

(* ── is_destructive ───────────────────────────────────────── *)

let test_is_destructive_clear () =
  check bool "Clear_all" true (Tool_op.is_destructive Clear_all)

let test_is_destructive_add () =
  check bool "Add" false (Tool_op.is_destructive (Add ["x"]))

let test_is_destructive_replace () =
  check bool "Replace_with" true (Tool_op.is_destructive (Replace_with ["x"]))

let test_is_destructive_intersect () =
  check bool "Intersect_with" true (Tool_op.is_destructive (Intersect_with ["x"]))

let test_is_destructive_seq () =
  check bool "Seq with Clear_all" true
    (Tool_op.is_destructive (Seq [Add ["x"]; Clear_all]))

let test_is_destructive_seq_ok () =
  check bool "Seq all non-destructive" false
    (Tool_op.is_destructive (Seq [Add ["x"]; Remove ["y"]]))

(* ── equal ────────────────────────────────────────────────── *)

let test_equal_add_order () =
  check bool "normalized" true
    (Tool_op.equal (Add ["a"; "b"]) (Add ["b"; "a"]))

let test_equal_structural () =
  check bool "different structure" false
    (Tool_op.equal (Add ["a"]) (Seq [Add ["a"]]))

let test_equal_keep_all () =
  check bool "same" true (Tool_op.equal Keep_all Keep_all)

let test_equal_different_variant () =
  check bool "different" false
    (Tool_op.equal (Add ["a"]) (Remove ["a"]))

(* ── serialization roundtrip ──────────────────────────────── *)

let test_yojson_roundtrip_keep_all () =
  match Tool_op.of_yojson (Tool_op.to_yojson Keep_all) with
  | Ok op -> check bool "roundtrip" true (Tool_op.equal op Keep_all)
  | Error e -> fail e

let test_yojson_roundtrip_seq () =
  let original = Seq [Add ["a"]; Remove ["b"]; Clear_all] in
  match Tool_op.of_yojson (Tool_op.to_yojson original) with
  | Ok op -> check bool "roundtrip" true (Tool_op.equal op original)
  | Error e -> fail e

let test_yojson_invalid () =
  match Tool_op.of_yojson (`String "bad") with
  | Error _ -> ()
  | Ok _ -> fail "expected error"

(* ── QCheck properties ────────────────────────────────────── *)

let gen_name = QCheck.Gen.(
  map (fun i -> Printf.sprintf "tool_%d" i) (int_range 0 20))

let gen_names = QCheck.Gen.(list_size (int_range 0 5) gen_name)

let gen_tool_set = QCheck.Gen.(
  map (fun names ->
    List.sort_uniq String.compare names
  ) (list_size (int_range 0 8) gen_name))

let gen_simple_op = QCheck.Gen.(
  oneof [
    return Keep_all;
    return Clear_all;
    map (fun ns -> Add ns) gen_names;
    map (fun ns -> Remove ns) gen_names;
    map (fun ns -> Replace_with ns) gen_names;
    map (fun ns -> Intersect_with ns) gen_names;
  ])

let arb_simple_op = QCheck.make gen_simple_op
  ~print:(fun op -> Yojson.Safe.to_string (Tool_op.to_yojson op))

let arb_tool_set = QCheck.make gen_tool_set
  ~print:(fun xs -> String.concat ", " xs)

let prop_keep_all_identity =
  QCheck.Test.make ~name:"Keep_all is identity" ~count:100
    arb_tool_set
    (fun xs -> Tool_op.apply Keep_all xs = List.sort_uniq String.compare xs
               || Tool_op.apply Keep_all xs = xs
               (* dedup may reorder, but membership is preserved *)
               || List.sort String.compare (Tool_op.apply Keep_all xs)
                  = List.sort String.compare (List.sort_uniq String.compare xs))

let prop_add_superset =
  QCheck.Test.make ~name:"Add preserves existing" ~count:100
    (QCheck.pair arb_tool_set (QCheck.make gen_names))
    (fun (xs, names) ->
      let result = Tool_op.apply (Add names) xs in
      List.for_all (fun x -> List.mem x result) (List.sort_uniq String.compare xs))

let prop_remove_subset =
  QCheck.Test.make ~name:"Remove only removes" ~count:100
    (QCheck.pair arb_tool_set (QCheck.make gen_names))
    (fun (xs, names) ->
      let result = Tool_op.apply (Remove names) xs in
      List.for_all (fun x -> List.mem x xs) result)

let prop_compose_homomorphism =
  QCheck.Test.make ~name:"compose [a;b] = apply b (apply a)" ~count:100
    (QCheck.triple arb_simple_op arb_simple_op arb_tool_set)
    (fun (a, b, xs) ->
      let via_seq = Tool_op.apply (Seq [a; b]) xs in
      let via_chain = Tool_op.apply b (Tool_op.apply a xs) in
      List.sort String.compare via_seq = List.sort String.compare via_chain)

let prop_yojson_roundtrip =
  QCheck.Test.make ~name:"to_yojson >> of_yojson roundtrip" ~count:100
    arb_simple_op
    (fun op ->
      match Tool_op.of_yojson (Tool_op.to_yojson op) with
      | Ok rt -> Tool_op.equal op rt
      | Error _ -> false)

(* ── hook integration ─────────────────────────────────────── *)

(** Demonstrates the primary use case: hook-based tool gating. *)
let test_hook_grant_revoke () =
  let all_tools = ["read"; "write"; "search"; "deploy"; "shell"] in
  (* Turn 1: remove dangerous tools *)
  let op1 = Tool_op.compose [Remove ["shell"; "deploy"]] in
  let filter1 = Tool_op.to_tool_filter op1 all_tools in
  (match filter1 with
   | Guardrails.AllowList names ->
     check_set_eq "turn 1: safe only" ["read"; "write"; "search"] names
   | _ -> fail "expected AllowList");
  (* Turn 5: grant deploy back *)
  let current_after_1 = Tool_op.apply op1 all_tools in
  let op5 = Add ["deploy"] in
  let filter5 = Tool_op.to_tool_filter op5 current_after_1 in
  (match filter5 with
   | Guardrails.AllowList names ->
     check_set_eq "turn 5: +deploy" ["read"; "write"; "search"; "deploy"] names
   | _ -> fail "expected AllowList")

(** Demonstrates last-turn safety restriction. *)
let test_hook_last_turn_restrict () =
  let current = ["read"; "write"; "search"; "shell"; "deploy"] in
  let safe_tools = ["read"; "search"] in
  let op = Intersect_with safe_tools in
  let filter = Tool_op.to_tool_filter op current in
  match filter with
  | Guardrails.AllowList names ->
    check_set_eq "last turn: safe only" ["read"; "search"] names
  | _ -> fail "expected AllowList"

(** Demonstrates compose for multi-stage tool selection. *)
let test_hook_compose_stages () =
  let all_tools = ["a"; "b"; "c"; "d"; "e"] in
  let op = Tool_op.compose [
    Replace_with ["a"; "b"; "c"];  (* start with subset *)
    Remove ["b"];                   (* remove one *)
    Add ["d"];                      (* add one back *)
  ] in
  let result = Tool_op.apply op all_tools in
  check_set_eq "composed" ["a"; "c"; "d"] result

(* ── runner ───────────────────────────────────────────────── *)

let () =
  run "Tool_op"
    [
      ( "apply",
        [
          test_case "Keep_all" `Quick test_apply_keep_all;
          test_case "Clear_all" `Quick test_apply_clear_all;
          test_case "Add" `Quick test_apply_add;
          test_case "Add duplicate" `Quick test_apply_add_duplicate;
          test_case "Add empty" `Quick test_apply_add_empty;
          test_case "Remove" `Quick test_apply_remove;
          test_case "Remove absent" `Quick test_apply_remove_absent;
          test_case "Remove empty" `Quick test_apply_remove_empty;
          test_case "Replace_with" `Quick test_apply_replace_with;
          test_case "Replace_with empty" `Quick test_apply_replace_with_empty;
          test_case "Intersect_with" `Quick test_apply_intersect;
          test_case "Intersect_with empty" `Quick test_apply_intersect_empty;
          test_case "Intersect_with disjoint" `Quick test_apply_intersect_disjoint;
          test_case "Seq" `Quick test_apply_seq;
          test_case "Seq empty" `Quick test_apply_seq_empty;
          test_case "Add to empty" `Quick test_apply_add_to_empty;
          test_case "Remove from empty" `Quick test_apply_remove_from_empty;
          test_case "dedup current" `Quick test_apply_dedup_current;
          test_case "Remove dedup current" `Quick test_apply_remove_dedup_current;
        ] );
      ( "apply_to_tool_set",
        [
          test_case "Remove" `Quick test_apply_to_tool_set_remove;
          test_case "Add existing only" `Quick test_apply_to_tool_set_add_existing;
          test_case "Intersect" `Quick test_apply_to_tool_set_intersect;
        ] );
      ( "compose",
        [
          test_case "empty" `Quick test_compose_empty;
          test_case "single" `Quick test_compose_single;
          test_case "flatten" `Quick test_compose_flatten;
          test_case "identity elimination" `Quick test_compose_identity_elimination;
          test_case "all identity" `Quick test_compose_all_identity;
          test_case "deep flatten" `Quick test_compose_deep_flatten;
        ] );
      ( "to_tool_filter",
        [
          test_case "Remove" `Quick test_to_tool_filter;
          test_case "identity" `Quick test_to_tool_filter_identity;
        ] );
      ( "is_identity",
        [
          test_case "Keep_all" `Quick test_is_identity_keep_all;
          test_case "Add []" `Quick test_is_identity_add_empty;
          test_case "Remove []" `Quick test_is_identity_remove_empty;
          test_case "Add nonempty" `Quick test_is_identity_add_nonempty;
          test_case "Seq recursive" `Quick test_is_identity_seq_recursive;
        ] );
      ( "is_destructive",
        [
          test_case "Clear_all" `Quick test_is_destructive_clear;
          test_case "Add" `Quick test_is_destructive_add;
          test_case "Replace_with" `Quick test_is_destructive_replace;
          test_case "Intersect_with" `Quick test_is_destructive_intersect;
          test_case "Seq with destructive" `Quick test_is_destructive_seq;
          test_case "Seq all safe" `Quick test_is_destructive_seq_ok;
        ] );
      ( "equal",
        [
          test_case "Add order" `Quick test_equal_add_order;
          test_case "structural" `Quick test_equal_structural;
          test_case "Keep_all" `Quick test_equal_keep_all;
          test_case "different variant" `Quick test_equal_different_variant;
        ] );
      ( "hook_integration",
        [
          test_case "grant/revoke" `Quick test_hook_grant_revoke;
          test_case "last turn restrict" `Quick test_hook_last_turn_restrict;
          test_case "compose stages" `Quick test_hook_compose_stages;
        ] );
      ( "serialization",
        [
          test_case "roundtrip Keep_all" `Quick test_yojson_roundtrip_keep_all;
          test_case "roundtrip Seq" `Quick test_yojson_roundtrip_seq;
          test_case "invalid input" `Quick test_yojson_invalid;
        ] );
      ( "properties",
        List.map QCheck_alcotest.to_alcotest [
          prop_keep_all_identity;
          prop_add_superset;
          prop_remove_subset;
          prop_compose_homomorphism;
          prop_yojson_roundtrip;
        ] );
    ]
