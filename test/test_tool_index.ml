(** Tests for Tool_index — BM25-based tool retrieval. *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let entry name desc =
  Tool_index.{ name; description = desc; group = None }

let grouped name desc group =
  Tool_index.{ name; description = desc; group = Some group }

(* ── Construction ─────────────────────────────────── *)

let test_empty_index () =
  let idx = Tool_index.build [] in
  check int "empty size" 0 (Tool_index.size idx);
  check int "empty vocab" 0 (Tool_index.vocabulary_size idx);
  check (list pass) "empty query" [] (Tool_index.retrieve idx "anything")

let test_of_tools () =
  let tool = Tool.create ~name:"read_file" ~description:"Read file contents"
    ~parameters:[] (fun _ -> Ok { Types.content = "ok" }) in
  let idx = Tool_index.of_tools [tool] in
  check int "size 1" 1 (Tool_index.size idx);
  check bool "vocab > 0" true (Tool_index.vocabulary_size idx > 0)

(* ── Retrieval ────────────────────────────────────── *)

let test_single_tool_match () =
  let idx = Tool_index.build [
    entry "read_file" "Read contents of a file from disk";
  ] in
  let results = Tool_index.retrieve idx "read file" in
  check bool "has result" true (List.length results > 0);
  let (name, score) = List.hd results in
  check string "correct tool" "read_file" name;
  check bool "positive score" true (score > 0.0)

let test_ranking_order () =
  let idx = Tool_index.build [
    entry "write_file" "Write content to a file";
    entry "read_file" "Read contents of a file from disk";
    entry "delete_file" "Delete a file from disk";
  ] in
  let results = Tool_index.retrieve_names idx "read file contents" in
  check bool "read_file is first" true
    (match results with "read_file" :: _ -> true | _ -> false)

let test_irrelevant_query () =
  let idx = Tool_index.build [
    entry "git_commit" "Create a git commit with message";
  ] in
  let results = Tool_index.retrieve idx "quantum physics entanglement" in
  (* Should return results (BM25 may still match on common words)
     but with low scores *)
  let high_scoring = List.filter (fun (_, s) -> s > 1.0) results in
  check int "no high-scoring matches" 0 (List.length high_scoring)

let test_top_k_limit () =
  let config = Tool_index.{ default_config with top_k = 2 } in
  let idx = Tool_index.build ~config [
    entry "tool_a" "first tool for testing";
    entry "tool_b" "second tool for testing";
    entry "tool_c" "third tool for testing";
    entry "tool_d" "fourth tool for testing";
  ] in
  let results = Tool_index.retrieve idx "tool testing" in
  check bool "at most 2" true (List.length results <= 2)

(* ── Group co-retrieval ───────────────────────────── *)

let test_group_retrieval () =
  let idx = Tool_index.build [
    grouped "git_commit" "Create a git commit" "git";
    grouped "git_push" "Push commits to remote" "git";
    grouped "git_status" "Show working tree status" "git";
    entry "read_file" "Read a file from disk";
  ] in
  let results = Tool_index.retrieve_names idx "commit changes" in
  (* git_commit should match, and git_push + git_status should be co-retrieved *)
  check bool "git_commit present" true (List.mem "git_commit" results);
  check bool "git_push co-retrieved" true (List.mem "git_push" results);
  check bool "git_status co-retrieved" true (List.mem "git_status" results)

(* ── Confidence gate ──────────────────────────────── *)

let test_confident_true () =
  let idx = Tool_index.build [
    entry "search" "Search for files and directories";
  ] in
  check bool "confident on match" true
    (Tool_index.confident idx "search files" ~threshold:0.01)

let test_confident_false () =
  let idx = Tool_index.build [
    entry "search" "Search for files";
  ] in
  check bool "not confident on unrelated" false
    (Tool_index.confident idx "zxcvbn asdfgh" ~threshold:10.0)

let test_confident_empty_index () =
  let idx = Tool_index.build [] in
  check bool "not confident on empty" false
    (Tool_index.confident idx "anything" ~threshold:0.0)

(* ── Min score filtering ──────────────────────────── *)

let test_min_score_filter () =
  let config = Tool_index.{ default_config with min_score = 100.0 } in
  let idx = Tool_index.build ~config [
    entry "tool" "a simple tool";
  ] in
  let results = Tool_index.retrieve idx "tool" in
  check int "filtered by min_score" 0 (List.length results)

(* ── Edge cases ───────────────────────────────────── *)

let test_empty_query () =
  let idx = Tool_index.build [entry "tool" "a tool"] in
  check (list pass) "empty query" [] (Tool_index.retrieve idx "")

let test_single_char_query () =
  let idx = Tool_index.build [entry "tool" "a tool"] in
  (* Single char tokenizes to empty (< 2 chars) *)
  check (list pass) "single char" [] (Tool_index.retrieve idx "a")

let test_duplicate_names () =
  let idx = Tool_index.build [
    entry "tool" "first description";
    entry "tool" "second description";
  ] in
  check int "both indexed" 2 (Tool_index.size idx)

(* ── Suite ────────────────────────────────────────── *)

let () =
  run "Tool_index" [
    "construction", [
      test_case "empty index" `Quick test_empty_index;
      test_case "of_tools" `Quick test_of_tools;
    ];
    "retrieval", [
      test_case "single match" `Quick test_single_tool_match;
      test_case "ranking order" `Quick test_ranking_order;
      test_case "irrelevant query" `Quick test_irrelevant_query;
      test_case "top_k limit" `Quick test_top_k_limit;
    ];
    "groups", [
      test_case "group co-retrieval" `Quick test_group_retrieval;
    ];
    "confidence", [
      test_case "confident true" `Quick test_confident_true;
      test_case "confident false" `Quick test_confident_false;
      test_case "confident empty" `Quick test_confident_empty_index;
    ];
    "min_score", [
      test_case "filtering" `Quick test_min_score_filter;
    ];
    "edge_cases", [
      test_case "empty query" `Quick test_empty_query;
      test_case "single char" `Quick test_single_char_query;
      test_case "duplicate names" `Quick test_duplicate_names;
    ];
  ]
