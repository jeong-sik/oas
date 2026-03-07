(** Tests for guardrails.ml — tool filtering and execution limits *)

open Alcotest
open Agent_sdk
open Types

let make_tool name =
  Tool.create ~name ~description:"test" ~parameters:[] (fun _input -> Ok "ok")

let test_default () =
  let g = Guardrails.default in
  check bool "default filter is AllowAll" true
    (g.tool_filter = Guardrails.AllowAll);
  check bool "default limit is None" true
    (g.max_tool_calls_per_turn = None)

let test_allow_all () =
  let g = Guardrails.default in
  let tools = [make_tool "a"; make_tool "b"; make_tool "c"] in
  let filtered = Guardrails.filter_tools g tools in
  check int "all tools pass" 3 (List.length filtered)

let test_allow_list () =
  let g = { Guardrails.default with
    tool_filter = Guardrails.AllowList ["a"; "c"] } in
  let tools = [make_tool "a"; make_tool "b"; make_tool "c"] in
  let filtered = Guardrails.filter_tools g tools in
  let names = List.map (fun (t: Tool.t) -> t.schema.name) filtered in
  check (list string) "only allowed" ["a"; "c"] names

let test_deny_list () =
  let g = { Guardrails.default with
    tool_filter = Guardrails.DenyList ["b"] } in
  let tools = [make_tool "a"; make_tool "b"; make_tool "c"] in
  let filtered = Guardrails.filter_tools g tools in
  let names = List.map (fun (t: Tool.t) -> t.schema.name) filtered in
  check (list string) "b denied" ["a"; "c"] names

let test_custom_filter () =
  let g = { Guardrails.default with
    tool_filter = Guardrails.Custom (fun schema ->
      String.length schema.name > 1) } in
  let tools = [make_tool "a"; make_tool "ab"; make_tool "abc"] in
  let filtered = Guardrails.filter_tools g tools in
  let names = List.map (fun (t: Tool.t) -> t.schema.name) filtered in
  check (list string) "custom filter" ["ab"; "abc"] names

let test_exceeds_limit_none () =
  let g = Guardrails.default in
  check bool "no limit" false (Guardrails.exceeds_limit g 100)

let test_exceeds_limit_under () =
  let g = { Guardrails.default with max_tool_calls_per_turn = Some 5 } in
  check bool "under limit" false (Guardrails.exceeds_limit g 3)

let test_exceeds_limit_equal () =
  let g = { Guardrails.default with max_tool_calls_per_turn = Some 5 } in
  check bool "at limit" false (Guardrails.exceeds_limit g 5)

let test_exceeds_limit_over () =
  let g = { Guardrails.default with max_tool_calls_per_turn = Some 5 } in
  check bool "over limit" true (Guardrails.exceeds_limit g 6)

let () =
  run "Guardrails" [
    "default", [
      test_case "default values" `Quick test_default;
    ];
    "filter", [
      test_case "AllowAll" `Quick test_allow_all;
      test_case "AllowList" `Quick test_allow_list;
      test_case "DenyList" `Quick test_deny_list;
      test_case "Custom" `Quick test_custom_filter;
    ];
    "limits", [
      test_case "no limit" `Quick test_exceeds_limit_none;
      test_case "under limit" `Quick test_exceeds_limit_under;
      test_case "at limit" `Quick test_exceeds_limit_equal;
      test_case "over limit" `Quick test_exceeds_limit_over;
    ];
  ]
