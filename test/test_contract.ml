open Base
(** Tests for Contract module — runtime contract helpers. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────── *)

let check_string = Alcotest.(check string)
let check_bool = Alcotest.(check bool)
let check_int = Alcotest.(check int)

(* ── empty ────────────────────────────────────────────── *)

let test_empty_contract () =
  let c = Contract.empty in
  check_bool "runtime_awareness is None" true (c.runtime_awareness = None);
  check_bool "trigger is None" true (c.trigger = None);
  check_int "instruction_layers is empty" 0 (List.length c.instruction_layers);
  check_int "skills is empty" 0 (List.length c.skills);
  check_bool "tool_grants is None" true (c.tool_grants = None);
  check_bool "mcp_tool_allowlist is None" true (c.mcp_tool_allowlist = None)
;;

let test_is_empty_on_empty () =
  check_bool "empty is empty" true (Contract.is_empty Contract.empty)
;;

let test_is_empty_with_awareness () =
  let c = Contract.with_runtime_awareness "test" Contract.empty in
  check_bool "not empty with awareness" false (Contract.is_empty c)
;;

(* ── with_runtime_awareness ───────────────────────────── *)

let test_runtime_awareness_set () =
  let c = Contract.with_runtime_awareness "aware" Contract.empty in
  check_bool "awareness set" true (c.runtime_awareness = Some "aware")
;;

let test_runtime_awareness_empty_string () =
  let c = Contract.with_runtime_awareness "" Contract.empty in
  check_bool "empty string -> None" true (c.runtime_awareness = None)
;;

let test_runtime_awareness_whitespace () =
  let c = Contract.with_runtime_awareness "   " Contract.empty in
  check_bool "whitespace -> None" true (c.runtime_awareness = None)
;;

let test_runtime_awareness_trimmed () =
  let c = Contract.with_runtime_awareness "  hello  " Contract.empty in
  check_bool "trimmed" true (c.runtime_awareness = Some "hello")
;;

(* ── with_trigger ─────────────────────────────────────── *)

let test_trigger_basic () =
  let c = Contract.with_trigger "heartbeat" Contract.empty in
  match c.trigger with
  | Some t ->
    check_string "kind" "heartbeat" t.kind;
    check_bool "source None" true (t.source = None);
    check_bool "reason None" true (t.reason = None);
    check_bool "payload None" true (t.payload = None)
  | None -> Alcotest.fail "expected trigger"
;;

let test_trigger_with_options () =
  let c =
    Contract.with_trigger
      ~source:"cron"
      ~reason:"scheduled"
      ~payload:(`String "data")
      "heartbeat"
      Contract.empty
  in
  match c.trigger with
  | Some t ->
    check_string "kind" "heartbeat" t.kind;
    check_bool "source" true (t.source = Some "cron");
    check_bool "reason" true (t.reason = Some "scheduled");
    check_bool "payload" true (t.payload = Some (`String "data"))
  | None -> Alcotest.fail "expected trigger"
;;

let test_trigger_empty_kind_ignored () =
  let c = Contract.with_trigger "" Contract.empty in
  check_bool "empty kind -> no trigger" true (c.trigger = None)
;;

let test_trigger_whitespace_source_trimmed () =
  let c = Contract.with_trigger ~source:"  " "test" Contract.empty in
  match c.trigger with
  | Some t -> check_bool "source trimmed to None" true (t.source = None)
  | None -> Alcotest.fail "expected trigger"
;;

(* ── add_instruction_layer ────────────────────────────── *)

let test_instruction_layer () =
  let c = Contract.add_instruction_layer ~label:"L1" "content1" Contract.empty in
  check_int "one layer" 1 (List.length c.instruction_layers);
  let layer = List.hd c.instruction_layers in
  check_bool "label" true (layer.label = Some "L1");
  check_string "content" "content1" layer.content
;;

let test_instruction_layer_no_label () =
  let c = Contract.add_instruction_layer "content" Contract.empty in
  let layer = List.hd c.instruction_layers in
  check_bool "no label" true (layer.label = None)
;;

let test_instruction_layer_empty_content () =
  let c = Contract.add_instruction_layer "" Contract.empty in
  check_int "empty content -> no layer" 0 (List.length c.instruction_layers)
;;

let test_instruction_layer_stacks () =
  let c =
    Contract.empty
    |> Contract.add_instruction_layer "first"
    |> Contract.add_instruction_layer "second"
  in
  check_int "two layers" 2 (List.length c.instruction_layers)
;;

(* ── normalize_names ──────────────────────────────────── *)

let test_with_tool_grants () =
  let c = Contract.with_tool_grants [ "a"; "b"; "a"; "  c  "; "" ] Contract.empty in
  match c.tool_grants with
  | Some names ->
    check_int "deduped + trimmed" 3 (List.length names);
    check_bool "contains a" true (List.mem "a" names);
    check_bool "contains b" true (List.mem "b" names);
    check_bool "contains c" true (List.mem "c" names)
  | None -> Alcotest.fail "expected tool_grants"
;;

let test_with_mcp_allowlist () =
  let c = Contract.with_mcp_tool_allowlist [ "x"; "y" ] Contract.empty in
  match c.mcp_tool_allowlist with
  | Some names -> check_int "2 names" 2 (List.length names)
  | None -> Alcotest.fail "expected allowlist"
;;

(* ── merge ────────────────────────────────────────────── *)

let test_merge_right_wins_awareness () =
  let left = Contract.with_runtime_awareness "left" Contract.empty in
  let right = Contract.with_runtime_awareness "right" Contract.empty in
  let merged = Contract.merge left right in
  check_bool "right wins" true (merged.runtime_awareness = Some "right")
;;

let test_merge_left_when_right_none () =
  let left = Contract.with_runtime_awareness "left" Contract.empty in
  let merged = Contract.merge left Contract.empty in
  check_bool "left preserved" true (merged.runtime_awareness = Some "left")
;;

let test_merge_instruction_layers_concat () =
  let left = Contract.add_instruction_layer "a" Contract.empty in
  let right = Contract.add_instruction_layer "b" Contract.empty in
  let merged = Contract.merge left right in
  check_int "layers concatenated" 2 (List.length merged.instruction_layers)
;;

let test_merge_tool_grants_right_wins () =
  let left = Contract.with_tool_grants [ "a" ] Contract.empty in
  let right = Contract.with_tool_grants [ "b" ] Contract.empty in
  let merged = Contract.merge left right in
  match merged.tool_grants with
  | Some [ "b" ] -> ()
  | _ -> Alcotest.fail "right tool_grants should win"
;;

(* ── to_json / compose_system_prompt ──────────────────── *)

let test_to_json_empty () =
  let json = Contract.to_json Contract.empty in
  let open Yojson.Safe.Util in
  check_bool "runtime_awareness null" true (json |> member "runtime_awareness" = `Null);
  check_bool "trigger null" true (json |> member "trigger" = `Null)
;;

let test_to_json_with_trigger () =
  let c = Contract.with_trigger ~source:"s" "test" Contract.empty in
  let json = Contract.to_json c in
  let trigger_json = Yojson.Safe.Util.member "trigger" json in
  let kind = Yojson.Safe.Util.(member "kind" trigger_json |> to_string) in
  check_string "kind" "test" kind
;;

let test_compose_system_prompt_empty () =
  let result = Contract.compose_system_prompt Contract.empty in
  check_bool "empty contract -> None" true (result = None)
;;

let test_compose_system_prompt_with_awareness () =
  let c = Contract.with_runtime_awareness "hello" Contract.empty in
  match Contract.compose_system_prompt c with
  | Some s -> check_bool "contains awareness" true (String.length s > 0)
  | None -> Alcotest.fail "expected prompt"
;;

let test_compose_system_prompt_with_base () =
  let c = Contract.with_runtime_awareness "extra" Contract.empty in
  match Contract.compose_system_prompt ~base:"base prompt" c with
  | Some s ->
    check_bool "contains base" true (Util.string_contains ~needle:"base prompt" s);
    check_bool "contains awareness" true (Util.string_contains ~needle:"extra" s)
  | None -> Alcotest.fail "expected prompt"
;;

let test_compose_system_prompt_with_trigger () =
  let c =
    Contract.with_trigger ~source:"cron" ~reason:"timer" "heartbeat" Contract.empty
  in
  match Contract.compose_system_prompt c with
  | Some s ->
    check_bool "contains trigger" true (Util.string_contains ~needle:"heartbeat" s);
    check_bool "contains source" true (Util.string_contains ~needle:"cron" s)
  | None -> Alcotest.fail "expected prompt"
;;

(* ── filter_tools ─────────────────────────────────────── *)

let test_filter_tools_none_grants () =
  let tools =
    [ Tool.create ~name:"a" ~description:"" ~parameters:[] (fun _ ->
        Ok { Types.content = "" })
    ; Tool.create ~name:"b" ~description:"" ~parameters:[] (fun _ ->
        Ok { Types.content = "" })
    ]
  in
  let filtered = Contract.filter_tools Contract.empty tools in
  check_int "no filter -> all tools" 2 (List.length filtered)
;;

let test_filter_tools_with_grants () =
  let tools =
    [ Tool.create ~name:"a" ~description:"" ~parameters:[] (fun _ ->
        Ok { Types.content = "" })
    ; Tool.create ~name:"b" ~description:"" ~parameters:[] (fun _ ->
        Ok { Types.content = "" })
    ; Tool.create ~name:"c" ~description:"" ~parameters:[] (fun _ ->
        Ok { Types.content = "" })
    ]
  in
  let c = Contract.with_tool_grants [ "a"; "c" ] Contract.empty in
  let filtered = Contract.filter_tools c tools in
  check_int "filtered to 2" 2 (List.length filtered)
;;

(* ── context_with_contract ────────────────────────────── *)

let test_context_with_contract_empty () =
  let result = Contract.context_with_contract Contract.empty in
  check_bool "empty contract -> None" true (result = None)
;;

let test_context_with_contract_non_empty () =
  let c = Contract.with_runtime_awareness "test" Contract.empty in
  match Contract.context_with_contract c with
  | Some ctx ->
    let value = Context.get ctx Contract.context_key in
    check_bool "has value" true (value <> None)
  | None -> Alcotest.fail "expected context"
;;

let test_context_with_contract_preserves_identity () =
  let original = Context.create () in
  Context.set original "user_data" (`String "hello");
  let c = Contract.with_runtime_awareness "test" Contract.empty in
  match Contract.context_with_contract ~context:original c with
  | Some ctx ->
    (* returned context IS the original, not a copy *)
    Context.set ctx "injected" (`String "world");
    let from_original = Context.get original "injected" in
    check_bool
      "write to returned ctx visible in original"
      true
      (from_original = Some (`String "world"));
    let from_ctx = Context.get ctx "user_data" in
    check_bool "original data in returned ctx" true (from_ctx = Some (`String "hello"))
  | None -> Alcotest.fail "expected context"
;;

(* ── Suite ────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "contract"
    [ ( "empty"
      , [ Alcotest.test_case "empty contract" `Quick test_empty_contract
        ; Alcotest.test_case "is_empty on empty" `Quick test_is_empty_on_empty
        ; Alcotest.test_case "is_empty with awareness" `Quick test_is_empty_with_awareness
        ] )
    ; ( "runtime_awareness"
      , [ Alcotest.test_case "set awareness" `Quick test_runtime_awareness_set
        ; Alcotest.test_case "empty string" `Quick test_runtime_awareness_empty_string
        ; Alcotest.test_case "whitespace" `Quick test_runtime_awareness_whitespace
        ; Alcotest.test_case "trimmed" `Quick test_runtime_awareness_trimmed
        ] )
    ; ( "trigger"
      , [ Alcotest.test_case "basic" `Quick test_trigger_basic
        ; Alcotest.test_case "with options" `Quick test_trigger_with_options
        ; Alcotest.test_case "empty kind ignored" `Quick test_trigger_empty_kind_ignored
        ; Alcotest.test_case
            "whitespace source trimmed"
            `Quick
            test_trigger_whitespace_source_trimmed
        ] )
    ; ( "instruction_layer"
      , [ Alcotest.test_case "with label" `Quick test_instruction_layer
        ; Alcotest.test_case "no label" `Quick test_instruction_layer_no_label
        ; Alcotest.test_case "empty content" `Quick test_instruction_layer_empty_content
        ; Alcotest.test_case "stacks" `Quick test_instruction_layer_stacks
        ] )
    ; ( "tool_grants"
      , [ Alcotest.test_case "with_tool_grants" `Quick test_with_tool_grants
        ; Alcotest.test_case "with_mcp_allowlist" `Quick test_with_mcp_allowlist
        ] )
    ; ( "merge"
      , [ Alcotest.test_case "right wins awareness" `Quick test_merge_right_wins_awareness
        ; Alcotest.test_case "left when right None" `Quick test_merge_left_when_right_none
        ; Alcotest.test_case "layers concat" `Quick test_merge_instruction_layers_concat
        ; Alcotest.test_case
            "tool_grants right wins"
            `Quick
            test_merge_tool_grants_right_wins
        ] )
    ; ( "json"
      , [ Alcotest.test_case "to_json empty" `Quick test_to_json_empty
        ; Alcotest.test_case "to_json with trigger" `Quick test_to_json_with_trigger
        ] )
    ; ( "compose_system_prompt"
      , [ Alcotest.test_case "empty" `Quick test_compose_system_prompt_empty
        ; Alcotest.test_case
            "with awareness"
            `Quick
            test_compose_system_prompt_with_awareness
        ; Alcotest.test_case "with base" `Quick test_compose_system_prompt_with_base
        ; Alcotest.test_case "with trigger" `Quick test_compose_system_prompt_with_trigger
        ] )
    ; ( "filter"
      , [ Alcotest.test_case "no grants" `Quick test_filter_tools_none_grants
        ; Alcotest.test_case "with grants" `Quick test_filter_tools_with_grants
        ] )
    ; ( "context"
      , [ Alcotest.test_case "empty contract" `Quick test_context_with_contract_empty
        ; Alcotest.test_case
            "non-empty contract"
            `Quick
            test_context_with_contract_non_empty
        ; Alcotest.test_case
            "preserves identity"
            `Quick
            test_context_with_contract_preserves_identity
        ] )
    ]
;;
