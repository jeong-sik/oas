open Base
open Alcotest
open Agent_sdk

let execute_ok_json tool input =
  match Tool.execute tool input with
  | Ok { content } -> Yojson.Safe.from_string content
  | Error { message; _ } -> fail ("expected Ok, got error: " ^ message)
;;

let execute_error tool input =
  match Tool.execute tool input with
  | Ok { content } -> fail ("expected Error, got: " ^ content)
  | Error { message; _ } -> message
;;

let test_all_exposes_expected_names () =
  let mem = Memory.create () in
  let names =
    Memory_tools.all mem |> List.map (fun (tool : Tool.t) -> tool.schema.name)
  in
  check
    (list string)
    "tool names"
    [ "memory_remember"
    ; "memory_recall"
    ; "memory_remember_episode"
    ; "memory_find_procedure"
    ]
    names
;;

let test_remember_defaults_to_working () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  let json =
    execute_ok_json tool (`Assoc [ "key", `String "count"; "value_json", `String "42" ])
  in
  check bool "ok" true Yojson.Safe.Util.(json |> member "ok" |> to_bool);
  check string "tier" "working" Yojson.Safe.Util.(json |> member "tier" |> to_string);
  check
    (option int)
    "stored value"
    (Some 42)
    (match Memory.recall mem ~tier:Working "count" with
     | Some (`Int value) -> Some value
     | _ -> None)
;;

let test_remember_falls_back_to_raw_string () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  ignore
    (execute_ok_json
       tool
       (`Assoc [ "key", `String "note"; "value_json", `String "hello world" ]));
  check
    string
    "raw string"
    "hello world"
    (match Memory.recall mem ~tier:Working "note" with
     | Some (`String value) -> value
     | _ -> fail "expected raw string")
;;

let test_recall_found_and_missing () =
  let mem = Memory.create () in
  ignore (Memory.store mem ~tier:Working "cfg" (`Assoc [ "enabled", `Bool true ]));
  let tool = Memory_tools.recall mem in
  let found = execute_ok_json tool (`Assoc [ "key", `String "cfg" ]) in
  check bool "found" true Yojson.Safe.Util.(found |> member "found" |> to_bool);
  check
    bool
    "value.enabled"
    true
    Yojson.Safe.Util.(found |> member "value" |> member "enabled" |> to_bool);
  let missing = execute_ok_json tool (`Assoc [ "key", `String "missing" ]) in
  check bool "missing" false Yojson.Safe.Util.(missing |> member "found" |> to_bool)
;;

let test_invalid_generic_tier_is_rejected () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember mem in
  let message =
    execute_error
      tool
      (`Assoc
          [ "tier", `String "episodic"; "key", `String "k"; "value_json", `String "1" ])
  in
  check
    bool
    "mentions generic tools"
    true
    (Util.string_contains ~needle:"generic memory tools" message)
;;

let test_remember_episode_generates_id () =
  let mem = Memory.create () in
  let tool = Memory_tools.remember_episode mem in
  let json =
    execute_ok_json
      tool
      (`Assoc
          [ "action", `String "deploy"
          ; "participants", `List [ `String "alice" ]
          ; "outcome", `String "success"
          ; "detail", `String "green"
          ])
  in
  let episode_id = Yojson.Safe.Util.(json |> member "id" |> to_string) in
  check bool "generated prefix" true (Util.string_contains ~needle:"ep_" episode_id);
  match Memory.recall_episode mem episode_id with
  | Some episode ->
    check string "action" "deploy" episode.action;
    check (list string) "participants" [ "alice" ] episode.participants
  | None -> fail "episode not stored"
;;

let test_find_procedure_respects_threshold_and_touch () =
  let mem = Memory.create () in
  Memory.store_procedure
    mem
    { id = "pr_low"
    ; pattern = "deploy"
    ; action = "retry blindly"
    ; success_count = 1
    ; failure_count = 4
    ; confidence = 0.2
    ; last_used = 10.0
    ; metadata = []
    };
  Memory.store_procedure
    mem
    { id = "pr_high"
    ; pattern = "deploy"
    ; action = "rollback first"
    ; success_count = 4
    ; failure_count = 1
    ; confidence = 0.8
    ; last_used = 10.0
    ; metadata = []
    };
  let tool = Memory_tools.find_procedure mem in
  let json =
    execute_ok_json
      tool
      (`Assoc
          [ "pattern", `String "deploy"
          ; "min_confidence", `Float 0.5
          ; "touch", `Bool true
          ])
  in
  check bool "found" true Yojson.Safe.Util.(json |> member "found" |> to_bool);
  check
    string
    "selected id"
    "pr_high"
    Yojson.Safe.Util.(json |> member "procedure" |> member "id" |> to_string);
  match Memory.best_procedure mem ~pattern:"deploy" with
  | Some procedure -> check bool "last_used touched" true (procedure.last_used > 10.0)
  | None -> fail "procedure missing after touch"
;;

let test_remember_acl_denied_by_default () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  let tool = Memory_tools.remember_acl acl ~agent_name:"alice" in
  let message =
    execute_error
      tool
      (`Assoc [ "key", `String "secret"; "value_json", `String {|{"v":1}|} ])
  in
  check bool "denied" true (Util.string_contains ~needle:"Access denied" message)
;;

let test_all_acl_reuses_same_tool_surface () =
  let mem = Memory.create () in
  let acl = Memory_access.create mem in
  let names =
    Memory_tools.all_acl acl ~agent_name:"alice"
    |> List.map (fun (tool : Tool.t) -> tool.schema.name)
  in
  check
    (list string)
    "acl tool names"
    [ "memory_remember"
    ; "memory_recall"
    ; "memory_remember_episode"
    ; "memory_find_procedure"
    ]
    names
;;

let () =
  run
    "Memory_tools"
    [ ( "surface"
      , [ test_case "all exposes expected names" `Quick test_all_exposes_expected_names
        ; test_case
            "all_acl reuses same tool names"
            `Quick
            test_all_acl_reuses_same_tool_surface
        ] )
    ; ( "remember"
      , [ test_case "defaults to working" `Quick test_remember_defaults_to_working
        ; test_case "raw string fallback" `Quick test_remember_falls_back_to_raw_string
        ; test_case "invalid generic tier" `Quick test_invalid_generic_tier_is_rejected
        ; test_case "acl denied by default" `Quick test_remember_acl_denied_by_default
        ] )
    ; "recall", [ test_case "found and missing" `Quick test_recall_found_and_missing ]
    ; "episodic", [ test_case "generated id" `Quick test_remember_episode_generates_id ]
    ; ( "procedural"
      , [ test_case
            "threshold and touch"
            `Quick
            test_find_procedure_respects_threshold_and_touch
        ] )
    ]
;;
