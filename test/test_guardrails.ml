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
  check bool "default limit is None" true (g.max_tool_calls_per_turn = None);
  check bool "default permission mode" true (g.permission_mode = Guardrails.Default)

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

let test_permission_default_read_only () =
  let tool = make_tool "read_file" in
  check (of_pp Guardrails.pp_permission_outcome) "read-only auto allowed"
    Guardrails.Authorized
    (Guardrails.permission_for_tool Guardrails.default tool.schema)

let test_permission_default_command_requires_confirmation () =
  let tool =
    Tool.create ~kind:Command ~name:"bash" ~description:"run command" ~parameters:[]
      (fun _ -> Ok "ok")
  in
  check (of_pp Guardrails.pp_permission_outcome) "command requires confirmation"
    (Guardrails.Requires_confirmation "tool needs explicit approval in default mode")
    (Guardrails.permission_for_tool Guardrails.default tool.schema)

let test_permission_plan_rejects_edits () =
  let tool =
    Tool.create ~kind:File_edit ~name:"edit" ~description:"edit file" ~parameters:[]
      (fun _ -> Ok "ok")
  in
  let guardrails = { Guardrails.default with permission_mode = Guardrails.Plan } in
  check bool "plan rejects edit"
    true
    (match Guardrails.permission_for_tool guardrails tool.schema with
     | Guardrails.Rejected _ -> true
     | _ -> false)

let test_output_limit_truncates () =
  let guardrails = { Guardrails.default with max_output_chars = Some 5 } in
  let output, truncated = Guardrails.apply_output_limit guardrails "123456789" in
  check bool "output truncated" true truncated;
  check bool "suffix added" true (String.ends_with ~suffix:"[truncated by guardrails]" output)

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
      test_case "permission read-only" `Quick test_permission_default_read_only;
      test_case "permission command confirm" `Quick
        test_permission_default_command_requires_confirmation;
      test_case "permission plan rejects edits" `Quick test_permission_plan_rejects_edits;
      test_case "output limit truncates" `Quick test_output_limit_truncates;
    ];
  ]
