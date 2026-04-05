(** Tests for Tool_selector — 2-stage tool routing. *)

open Alcotest
open Agent_sdk

(* ── Helpers ──────────────────────────────────────── *)

let make_tool name desc =
  Tool.create ~name ~description:desc
    ~parameters:[] (fun _ -> Ok { Types.content = "ok" })

let tools_20 =
  List.init 20 (fun i ->
    make_tool (Printf.sprintf "tool_%02d" i)
      (Printf.sprintf "Description for tool number %d" i))

let tools_5 =
  [ make_tool "read_file" "Read file contents from disk";
    make_tool "write_file" "Write content to a file on disk";
    make_tool "search" "Search codebase for a pattern";
    make_tool "git_commit" "Create a git commit with message";
    make_tool "broadcast" "Send message to all agents" ]

let tool_names tools =
  List.map (fun (t : Tool.t) -> t.schema.name) tools

(* ── All strategy ────────────────────────────────── *)

let test_all_returns_everything () =
  let result = Tool_selector.select
    ~strategy:All ~context:"anything" ~tools:tools_5 in
  check int "all 5 returned" 5 (List.length result);
  check (list string) "same names"
    (tool_names tools_5) (tool_names result)

let test_all_empty () =
  let result = Tool_selector.select
    ~strategy:All ~context:"query" ~tools:[] in
  check int "empty in, empty out" 0 (List.length result)

(* ── TopK_bm25 strategy ─────────────────────────── *)

let test_bm25_narrows () =
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 3; always_include = [];
                       confidence_threshold = None; fallback_tools = [] })
    ~context:"read file" ~tools:tools_5 in
  check bool "at most 3" true (List.length result <= 3);
  check bool "at least 1" true (List.length result >= 1)

let test_bm25_always_include () =
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 2; always_include = ["broadcast"];
                       confidence_threshold = None; fallback_tools = [] })
    ~context:"read file" ~tools:tools_5 in
  let names = tool_names result in
  check bool "broadcast included" true (List.mem "broadcast" names);
  check bool "at least 2" true (List.length result >= 2)

let test_bm25_empty_tools () =
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 3; always_include = [];
                       confidence_threshold = None; fallback_tools = [] })
    ~context:"query" ~tools:[] in
  check int "empty" 0 (List.length result)

let test_bm25_k_larger_than_tools () =
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 100; always_include = [];
                       confidence_threshold = None; fallback_tools = [] })
    ~context:"tool" ~tools:tools_5 in
  check bool "returns at most all tools" true (List.length result <= 5)

let test_bm25_always_include_dedup () =
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 5;
      always_include = ["read_file"; "read_file"];
      confidence_threshold = None; fallback_tools = [] })
    ~context:"read file" ~tools:tools_5 in
  let names = tool_names result in
  let unique = List.sort_uniq String.compare names in
  check int "no duplicates" (List.length names) (List.length unique)

(* ── auto strategy ───────────────────────────────── *)

let test_auto_small () =
  let strategy = Tool_selector.auto ~tools:tools_5 in
  match strategy with
  | All -> ()
  | _ -> fail "expected All for 5 tools"

let test_auto_large () =
  let strategy = Tool_selector.auto ~tools:tools_20 in
  match strategy with
  | TopK_bm25 { k = 5; always_include = [];
               confidence_threshold = None; fallback_tools = [] } -> ()
  | TopK_bm25 { k; _ } ->
    fail (Printf.sprintf "expected k=5, got k=%d" k)
  | _ -> fail "expected TopK_bm25 for 20 tools"

let test_auto_boundary () =
  let tools_15 = List.init 15 (fun i ->
    make_tool (Printf.sprintf "t%d" i) "desc") in
  let strategy = Tool_selector.auto ~tools:tools_15 in
  match strategy with
  | All -> ()
  | _ -> fail "expected All for exactly 15 tools"

let test_auto_16 () =
  let tools_16 = List.init 16 (fun i ->
    make_tool (Printf.sprintf "t%d" i) "desc") in
  let strategy = Tool_selector.auto ~tools:tools_16 in
  match strategy with
  | TopK_bm25 _ -> ()
  | _ -> fail "expected TopK_bm25 for 16 tools"

(* ── select_names ────────────────────────────────── *)

let test_select_names () =
  let names = Tool_selector.select_names
    ~strategy:All ~context:"q" ~tools:tools_5 in
  check (list string) "all names"
    (tool_names tools_5) names

(* ── TopK_llm ───────────────────────────────────── *)

let mock_rerank ~k =
  fun ~context:_ ~candidates ->
    List.filteri (fun i _ -> i < k) (List.map fst candidates)

let test_topk_llm_with_mock () =
  let result = Tool_selector.select
    ~strategy:(TopK_llm { k = 2; bm25_prefilter_n = 5;
      always_include = []; confidence_threshold = 0.0;
      rerank_fn = mock_rerank ~k:2 })
    ~context:"read file" ~tools:tools_5 in
  check bool "at most 2" true (List.length result <= 2);
  check bool "at least 1" true (List.length result >= 1)

let test_topk_llm_fallback_on_failure () =
  let failing_rerank ~context:_ ~candidates:_ =
    failwith "LLM unavailable" in
  let result = Tool_selector.select
    ~strategy:(TopK_llm { k = 3; bm25_prefilter_n = 5;
      always_include = []; confidence_threshold = 0.0;
      rerank_fn = failing_rerank })
    ~context:"read file" ~tools:tools_5 in
  check bool "at least 1 (BM25 fallback)" true (List.length result >= 1)

let test_topk_llm_always_include () =
  let result = Tool_selector.select
    ~strategy:(TopK_llm { k = 2; bm25_prefilter_n = 5;
      always_include = ["broadcast"]; confidence_threshold = 0.0;
      rerank_fn = mock_rerank ~k:1 })
    ~context:"read file" ~tools:tools_5 in
  let names = tool_names result in
  check bool "broadcast always included" true (List.mem "broadcast" names)

let test_topk_llm_low_confidence_skips_llm () =
  let call_count = ref 0 in
  let counting_rerank ~context:_ ~candidates =
    incr call_count;
    List.map fst candidates
  in
  let _result = Tool_selector.select
    ~strategy:(TopK_llm { k = 3; bm25_prefilter_n = 5;
      always_include = []; confidence_threshold = 999.0;
      rerank_fn = counting_rerank })
    ~context:"completely unrelated quantum physics" ~tools:tools_5 in
  check int "rerank not called" 0 !call_count

let test_topk_llm_empty_tools () =
  let result = Tool_selector.select
    ~strategy:(TopK_llm { k = 3; bm25_prefilter_n = 5;
      always_include = []; confidence_threshold = 0.0;
      rerank_fn = mock_rerank ~k:3 })
    ~context:"q" ~tools:[] in
  check int "empty" 0 (List.length result)

let test_topk_llm_invalid_names_dropped () =
  let bad_rerank ~context:_ ~candidates:_ =
    ["nonexistent_tool"; "also_fake"] in
  let result = Tool_selector.select
    ~strategy:(TopK_llm { k = 3; bm25_prefilter_n = 5;
      always_include = []; confidence_threshold = 0.0;
      rerank_fn = bad_rerank })
    ~context:"read file" ~tools:tools_5 in
  (* Invalid names dropped, only always_include survives (empty here) *)
  check int "no tools from invalid rerank" 0 (List.length result)

(* ── Categorical stubs ──────────────────────────── *)

let test_categorical_llm_not_implemented () =
  match Tool_selector.select
    ~strategy:(Categorical { groups = []; classifier = `Llm;
                             always_include = [] })
    ~context:"q" ~tools:tools_5
  with
  | exception Failure _ -> ()
  | _ -> fail "expected Failure for Categorical `Llm"

(* ── Confidence threshold ───────────────────────── *)

let test_bm25_confidence_fallback () =
  (* With a very high threshold, BM25 top score will be below it,
     so fallback_tools should be included *)
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 2; always_include = [];
      confidence_threshold = Some 999.0;
      fallback_tools = ["broadcast"; "git_commit"] })
    ~context:"completely unrelated quantum physics" ~tools:tools_5 in
  let names = tool_names result in
  check bool "broadcast in fallback" true (List.mem "broadcast" names);
  check bool "git_commit in fallback" true (List.mem "git_commit" names)

let test_bm25_confidence_above () =
  (* With threshold=0.0, BM25 top score will be above it,
     so fallback_tools should NOT be included *)
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 2; always_include = [];
      confidence_threshold = Some 0.0;
      fallback_tools = ["broadcast"] })
    ~context:"read file" ~tools:tools_5 in
  check bool "at most 2" true (List.length result <= 2)

let test_bm25_confidence_none_disables () =
  (* With threshold=None, fallback is disabled regardless *)
  let result = Tool_selector.select
    ~strategy:(TopK_bm25 { k = 1; always_include = [];
      confidence_threshold = None;
      fallback_tools = ["broadcast"; "git_commit"; "search"] })
    ~context:"completely unrelated" ~tools:tools_5 in
  check bool "at most 1 (no fallback)" true (List.length result <= 1)

(* ── Categorical BM25 ───────────────────────────── *)

let test_categorical_bm25 () =
  let groups = [
    ("file_ops", ["read_file"; "write_file"]);
    ("git_ops", ["git_commit"]);
    ("comm", ["broadcast"]);
  ] in
  let result = Tool_selector.select
    ~strategy:(Categorical { groups; classifier = `Bm25;
                             always_include = [] })
    ~context:"read a file" ~tools:tools_5 in
  check bool "at least 1 result" true (List.length result >= 1)

(* ── Runner ──────────────────────────────────────── *)

let () =
  run "Tool_selector" [
    "all", [
      test_case "returns everything" `Quick test_all_returns_everything;
      test_case "empty tools" `Quick test_all_empty;
    ];
    "topk_bm25", [
      test_case "narrows to k" `Quick test_bm25_narrows;
      test_case "always_include present" `Quick test_bm25_always_include;
      test_case "empty tools" `Quick test_bm25_empty_tools;
      test_case "k larger than tools" `Quick test_bm25_k_larger_than_tools;
      test_case "always_include dedup" `Quick test_bm25_always_include_dedup;
    ];
    "auto", [
      test_case "small set -> All" `Quick test_auto_small;
      test_case "large set -> TopK_bm25" `Quick test_auto_large;
      test_case "boundary 15 -> All" `Quick test_auto_boundary;
      test_case "16 -> TopK_bm25" `Quick test_auto_16;
    ];
    "select_names", [
      test_case "returns names" `Quick test_select_names;
    ];
    "confidence", [
      test_case "fallback on low confidence" `Quick test_bm25_confidence_fallback;
      test_case "no fallback above threshold" `Quick test_bm25_confidence_above;
      test_case "None disables fallback" `Quick test_bm25_confidence_none_disables;
    ];
    "topk_llm", [
      test_case "mock rerank selects" `Quick test_topk_llm_with_mock;
      test_case "fallback on failure" `Quick test_topk_llm_fallback_on_failure;
      test_case "always_include" `Quick test_topk_llm_always_include;
      test_case "low confidence skips LLM" `Quick test_topk_llm_low_confidence_skips_llm;
      test_case "empty tools" `Quick test_topk_llm_empty_tools;
      test_case "invalid names dropped" `Quick test_topk_llm_invalid_names_dropped;
    ];
    "stubs", [
      test_case "Categorical Llm raises" `Quick test_categorical_llm_not_implemented;
    ];
    "categorical_bm25", [
      test_case "file query matches file_ops" `Quick test_categorical_bm25;
    ];
  ]
