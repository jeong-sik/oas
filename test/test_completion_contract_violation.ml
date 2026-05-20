(** Tests for Completion_contract.violation_detail and satisfying_tools.

    Structured violation details with tool suggestions — callers can extract
    which tools were called, why each was rejected, and which tools would
    satisfy the contract. *)

open Agent_sdk
module Lp = Llm_provider.Types

let noop_handler _ = Ok { Types.content = "ok" }

let make_response ~content : Lp.api_response =
  { id = "test"
  ; model = "test-model"
  ; stop_reason = Lp.EndTurn
  ; content
  ; usage = None
  ; telemetry = None
  }
;;

let make_tool name =
  Tool.create ~name ~description:("desc:" ^ name) ~parameters:[] noop_handler
;;

let read_only_tool name =
  let descriptor =
    Tool.
      { kind = None
      ; mutation_class = None
      ; concurrency_class = None
      ; permission = Some ReadOnly
      ; evidence_role = None
      ; shell = None
      ; notes = []
      ; examples = []
      }
  in
  Tool.create ~descriptor ~name ~description:("desc:" ^ name) ~parameters:[] noop_handler
;;

(* --- violation_detail_to_string --- *)

let test_to_string_with_satisfying_tools () =
  let detail =
    Completion_contract.
      { called_tools = [ "search" ]
      ; satisfying_tools = [ "keeper_bash"; "keeper_write" ]
      ; rejection_reasons = [ "search", "read-only and cannot satisfy" ]
      }
  in
  let s = Completion_contract.violation_detail_to_string detail in
  let has_satisfying =
    try
      ignore (Str.search_forward (Str.regexp_string "Satisfying tools") s 0);
      true
    with
    | Not_found -> false
  in
  let has_keeper =
    try
      ignore (Str.search_forward (Str.regexp_string "keeper_bash") s 0);
      true
    with
    | Not_found -> false
  in
  Alcotest.(check bool) "contains Satisfying tools" true has_satisfying;
  Alcotest.(check bool) "contains keeper_bash" true has_keeper
;;

let test_to_string_without_satisfying_tools () =
  let detail =
    Completion_contract.
      { called_tools = [ "search" ]; satisfying_tools = []; rejection_reasons = [] }
  in
  let s = Completion_contract.violation_detail_to_string detail in
  let has_blocker =
    try
      ignore
        (Str.search_forward
           (Str.regexp_string "No currently visible tool can satisfy")
           s
           0);
      true
    with
    | Not_found -> false
  in
  Alcotest.(check bool) "contains blocker suggestion" true has_blocker
;;

(* --- validate_response satisfying_tools parameter --- *)

let test_backward_compat_no_satisfying_tools () =
  let response = make_response ~content:[ Lp.Text "hello" ] in
  match
    Completion_contract.validate_response
      ~contract:Completion_contract.Require_tool_use
      response
  with
  | Error msg ->
    let has_suggestion =
      try
        ignore (Str.search_forward (Str.regexp_string "Satisfying tools") msg 0);
        true
      with
      | Not_found -> false
    in
    Alcotest.(check bool) "no suggestion when param omitted" false has_suggestion;
    Alcotest.(check bool) "error message non-trivial" true (String.length msg > 10)
  | Ok () -> Alcotest.fail "expected Error"
;;

let test_satisfying_tools_appended_to_error () =
  let response = make_response ~content:[ Lp.Text "hello" ] in
  match
    Completion_contract.validate_response
      ~satisfying_tools:[ "keeper_bash"; "keeper_write" ]
      ~contract:Completion_contract.Require_tool_use
      response
  with
  | Error msg ->
    let has_suggestion =
      try
        ignore
          (Str.search_forward
             (Str.regexp_string "Satisfying tools for this contract:")
             msg
             0);
        true
      with
      | Not_found -> false
    in
    let has_keeper =
      try
        ignore (Str.search_forward (Str.regexp_string "keeper_bash") msg 0);
        true
      with
      | Not_found -> false
    in
    Alcotest.(check bool) "has suggestion line" true has_suggestion;
    Alcotest.(check bool) "has keeper_bash" true has_keeper
  | Ok () -> Alcotest.fail "expected Error"
;;

let test_satisfying_tools_appended_for_specific_tool () =
  let response =
    make_response
      ~content:[ Lp.ToolUse { id = "c1"; name = "search"; input = `Assoc [] } ]
  in
  match
    Completion_contract.validate_response
      ~satisfying_tools:[ "calculator" ]
      ~contract:(Completion_contract.Require_specific_tool "calculator")
      response
  with
  | Error msg ->
    let has_suggestion =
      try
        ignore
          (Str.search_forward
             (Str.regexp_string "Satisfying tools for this contract:")
             msg
             0);
        true
      with
      | Not_found -> false
    in
    Alcotest.(check bool) "has suggestion" true has_suggestion
  | Ok () -> Alcotest.fail "expected Error for wrong tool"
;;

(* --- violation_detail_of_response --- *)

let test_detail_on_failure_no_calls () =
  let response = make_response ~content:[ Lp.Text "hello" ] in
  match
    Completion_contract.violation_detail_of_response
      ~satisfying_tools:[ "keeper_bash" ]
      ~contract:Completion_contract.Require_tool_use
      response
  with
  | Error detail ->
    Alcotest.(check int) "called_tools empty" 0 (List.length detail.called_tools);
    Alcotest.(check int) "satisfying_tools has 1" 1 (List.length detail.satisfying_tools);
    Alcotest.(check int)
      "rejection_reasons empty"
      0
      (List.length detail.rejection_reasons)
  | Ok () -> Alcotest.fail "expected Error"
;;

let test_detail_with_rejected_calls () =
  let tool = read_only_tool "search" in
  let response =
    make_response
      ~content:[ Lp.ToolUse { id = "c1"; name = "search"; input = `Assoc [] } ]
  in
  match
    Completion_contract.violation_detail_of_response
      ~tools:[ tool ]
      ~required_tool_satisfaction:Completion_contract.effectful_tool_satisfies
      ~satisfying_tools:[ "keeper_bash" ]
      ~contract:Completion_contract.Require_tool_use
      response
  with
  | Error detail ->
    Alcotest.(check int) "called_tools has 1" 1 (List.length detail.called_tools);
    Alcotest.(check int) "satisfying_tools has 1" 1 (List.length detail.satisfying_tools);
    (match detail.rejection_reasons with
     | [ ("search", reason) ] ->
       let has_readonly =
         try
           ignore (Str.search_forward (Str.regexp_string "read-only") reason 0);
           true
         with
         | Not_found -> false
       in
       Alcotest.(check bool) "reason mentions read-only" true has_readonly
     | _ -> Alcotest.fail "expected single rejection reason for 'search'")
  | Ok () -> Alcotest.fail "expected Error"
;;

let test_detail_ok_when_satisfied () =
  let response =
    make_response
      ~content:[ Lp.ToolUse { id = "c1"; name = "calculator"; input = `Assoc [] } ]
  in
  match
    Completion_contract.violation_detail_of_response
      ~satisfying_tools:[ "keeper_bash" ]
      ~contract:Completion_contract.Require_tool_use
      response
  with
  | Ok () -> () (* pass *)
  | Error _ -> Alcotest.fail "expected Ok for satisfied contract"
;;

let test_detail_specific_tool_no_calls () =
  let response = make_response ~content:[ Lp.Text "hello" ] in
  match
    Completion_contract.violation_detail_of_response
      ~satisfying_tools:[ "calculator" ]
      ~contract:(Completion_contract.Require_specific_tool "calculator")
      response
  with
  | Error detail ->
    Alcotest.(check int) "called_tools empty" 0 (List.length detail.called_tools);
    Alcotest.(check int)
      "satisfying has calculator"
      1
      (List.length detail.satisfying_tools)
  | Ok () -> Alcotest.fail "expected Error"
;;

let test_detail_allow_text_always_ok () =
  let response = make_response ~content:[ Lp.Text "hello" ] in
  match
    Completion_contract.violation_detail_of_response
      ~satisfying_tools:[ "keeper_bash" ]
      ~contract:Completion_contract.Allow_text_or_tool
      response
  with
  | Ok () -> () (* pass *)
  | Error _ -> Alcotest.fail "Allow_text_or_tool should always pass"
;;

let test_detail_no_tool_use_violation () =
  let response =
    make_response
      ~content:[ Lp.ToolUse { id = "c1"; name = "search"; input = `Assoc [] } ]
  in
  match
    Completion_contract.violation_detail_of_response
      ~contract:Completion_contract.Require_no_tool_use
      response
  with
  | Error detail ->
    Alcotest.(check int) "called has search" 1 (List.length detail.called_tools);
    Alcotest.(check int) "no satisfying tools" 0 (List.length detail.satisfying_tools)
  | Ok () -> Alcotest.fail "expected Error for tool use when no-tool required"
;;

let () =
  Alcotest.run
    "completion_contract.violation_detail"
    [ ( "to_string"
      , [ Alcotest.test_case
            "with satisfying tools"
            `Quick
            test_to_string_with_satisfying_tools
        ; Alcotest.test_case
            "without satisfying tools (blocker suggestion)"
            `Quick
            test_to_string_without_satisfying_tools
        ] )
    ; ( "validate_response satisfying_tools param"
      , [ Alcotest.test_case
            "backward compat: no satisfying_tools still works"
            `Quick
            test_backward_compat_no_satisfying_tools
        ; Alcotest.test_case
            "satisfying_tools appended to Require_tool_use error"
            `Quick
            test_satisfying_tools_appended_to_error
        ; Alcotest.test_case
            "satisfying_tools appended to Require_specific_tool error"
            `Quick
            test_satisfying_tools_appended_for_specific_tool
        ] )
    ; ( "violation_detail_of_response"
      , [ Alcotest.test_case
            "returns structured detail on failure (no calls)"
            `Quick
            test_detail_on_failure_no_calls
        ; Alcotest.test_case
            "with rejected calls returns reasons"
            `Quick
            test_detail_with_rejected_calls
        ; Alcotest.test_case
            "returns Ok when contract satisfied"
            `Quick
            test_detail_ok_when_satisfied
        ; Alcotest.test_case
            "Require_specific_tool with no calls"
            `Quick
            test_detail_specific_tool_no_calls
        ; Alcotest.test_case
            "Allow_text_or_tool always Ok"
            `Quick
            test_detail_allow_text_always_ok
        ; Alcotest.test_case
            "Require_no_tool_use violation"
            `Quick
            test_detail_no_tool_use_violation
        ] )
    ]
;;
