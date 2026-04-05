(** Unit tests for Tool_middleware — reusable validation/coercion primitives. *)

open Agent_sdk

(* ── Helpers ──────────────────────────────────────────────── *)

let make_schema ?(required = []) (props : (string * string) list)
  : Yojson.Safe.t =
  let prop_entries =
    List.map (fun (name, type_str) ->
      (name, `Assoc [("type", `String type_str)])
    ) props
  in
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc prop_entries);
    ("required", `List (List.map (fun s -> `String s) required));
  ]

let tool_schema name json_schema : Types.tool_schema =
  Tool_middleware.tool_schema_of_json ~name json_schema

let string_contains ~sub s =
  let len = String.length s
  and sub_len = String.length sub in
  let rec loop i =
    i + sub_len <= len &&
    (String.sub s i sub_len = sub || loop (i + 1))
  in
  sub_len = 0 || loop 0

(* ── validate_and_coerce ─────────────────────────────────── *)

let test_pass_no_params () =
  let schema : Types.tool_schema =
    { name = "noop"; description = ""; parameters = [] }
  in
  match Tool_middleware.validate_and_coerce ~tool_name:"noop" ~schema `Null with
  | Tool_middleware.Pass -> ()
  | _ -> Alcotest.fail "empty params should Pass"

let test_pass_correct_types () =
  let schema = tool_schema "test" (make_schema [("name", "string")]) in
  let args = `Assoc [("name", `String "alice")] in
  match Tool_middleware.validate_and_coerce ~tool_name:"test" ~schema args with
  | Tool_middleware.Pass -> ()
  | Tool_middleware.Proceed _ -> Alcotest.fail "no coercion needed"
  | Tool_middleware.Reject r -> Alcotest.fail r.message

let test_proceed_coercion () =
  let schema = tool_schema "test"
    (make_schema [("count", "integer")]) in
  let args = `Assoc [("count", `String "42")] in
  match Tool_middleware.validate_and_coerce ~tool_name:"test" ~schema args with
  | Tool_middleware.Proceed coerced ->
    let v = Yojson.Safe.Util.member "count" coerced in
    Alcotest.(check int) "coerced to 42" 42
      (match v with `Int i -> i | _ -> -1)
  | Tool_middleware.Pass -> Alcotest.fail "expected Proceed (coercion)"
  | Tool_middleware.Reject r -> Alcotest.fail r.message

let test_proceed_bool_coercion () =
  let schema = tool_schema "test"
    (make_schema [("flag", "boolean")]) in
  let args = `Assoc [("flag", `String "true")] in
  match Tool_middleware.validate_and_coerce ~tool_name:"test" ~schema args with
  | Tool_middleware.Proceed coerced ->
    let v = Yojson.Safe.Util.member "flag" coerced in
    Alcotest.(check bool) "coerced to true" true
      (match v with `Bool b -> b | _ -> false)
  | Tool_middleware.Pass -> Alcotest.fail "expected Proceed (coercion)"
  | Tool_middleware.Reject r -> Alcotest.fail r.message

let test_reject_invalid () =
  let schema = tool_schema "test"
    (make_schema ~required:["count"] [("count", "integer")]) in
  let args = `Assoc [("count", `String "not_a_number")] in
  match Tool_middleware.validate_and_coerce ~tool_name:"test" ~schema args with
  | Tool_middleware.Reject r ->
    Alcotest.(check bool) "is_error" true r.is_error;
    Alcotest.(check bool) "has message" true (String.length r.message > 0)
  | _ -> Alcotest.fail "expected Reject for non-coercible string"

let test_reject_missing_required () =
  let schema = tool_schema "test"
    (make_schema ~required:["name"] [("name", "string")]) in
  let args = `Assoc [] in
  match Tool_middleware.validate_and_coerce ~tool_name:"test" ~schema args with
  | Tool_middleware.Reject _ -> ()
  | _ -> Alcotest.fail "expected Reject for missing required field"

(* ── tool_schema_of_json ─────────────────────────────────── *)

let test_schema_of_json () =
  let json = make_schema ~required:["room"]
    [("room", "string"); ("count", "integer")] in
  let schema = Tool_middleware.tool_schema_of_json ~name:"test_tool" json in
  Alcotest.(check string) "name" "test_tool" schema.name;
  Alcotest.(check string) "description" "" schema.description;
  Alcotest.(check int) "param count" 2 (List.length schema.parameters);
  let room_param = List.find
    (fun (p : Types.tool_param) -> p.name = "room") schema.parameters in
  Alcotest.(check bool) "room required" true room_param.required;
  Alcotest.(check bool) "room is string" true
    (room_param.param_type = Types.String)

let test_schema_of_json_empty () =
  let json = `Assoc [] in
  let schema = Tool_middleware.tool_schema_of_json ~name:"empty" json in
  Alcotest.(check int) "no params" 0 (List.length schema.parameters)

(* ── make_validation_hook ────────────────────────────────── *)

let test_hook_unknown_tool () =
  let hook = Tool_middleware.make_validation_hook
    ~lookup:(fun _ -> None) in
  match hook ~name:"unknown" ~args:`Null with
  | Tool_middleware.Pass -> ()
  | _ -> Alcotest.fail "unknown tool should Pass"

let test_hook_valid_tool () =
  let schema = tool_schema "known"
    (make_schema [("name", "string")]) in
  let hook = Tool_middleware.make_validation_hook
    ~lookup:(fun n -> if n = "known" then Some schema else None) in
  let args = `Assoc [("name", `String "alice")] in
  match hook ~name:"known" ~args with
  | Tool_middleware.Pass -> ()
  | Tool_middleware.Proceed _ -> Alcotest.fail "no coercion needed"
  | Tool_middleware.Reject r -> Alcotest.fail r.message

let test_hook_coercion () =
  let schema = tool_schema "coerce_me"
    (make_schema [("n", "integer")]) in
  let hook = Tool_middleware.make_validation_hook
    ~lookup:(fun n -> if n = "coerce_me" then Some schema else None) in
  let args = `Assoc [("n", `String "7")] in
  match hook ~name:"coerce_me" ~args with
  | Tool_middleware.Proceed coerced ->
    let v = Yojson.Safe.Util.member "n" coerced in
    Alcotest.(check int) "coerced" 7
      (match v with `Int i -> i | _ -> -1)
  | _ -> Alcotest.fail "expected Proceed"

let test_hook_rejection () =
  let schema = tool_schema "strict"
    (make_schema ~required:["x"] [("x", "integer")]) in
  let hook = Tool_middleware.make_validation_hook
    ~lookup:(fun n -> if n = "strict" then Some schema else None) in
  let args = `Assoc [("x", `String "abc")] in
  match hook ~name:"strict" ~args with
  | Tool_middleware.Reject _ -> ()
  | _ -> Alcotest.fail "expected Reject"

(* ── heal_tool_call ─────────────────────────────────────── *)

let int_schema = tool_schema "calc"
  (make_schema ~required:["n"] [("n", "integer")])

let mock_response ?(tool_name = "calc") ?(id = "fix1") input =
  Ok { Types.id = "m1"; model = "mock"; stop_reason = StopToolUse;
       content = [ToolUse { id; name = tool_name; input }];
       usage = None }

let mock_llm_fixes _msgs =
  mock_response (`Assoc [("n", `Int 42)])

let mock_llm_text_only _msgs =
  Ok { Types.id = "m1"; model = "mock"; stop_reason = EndTurn;
       content = [Text "I cannot fix this"]; usage = None }

let mock_llm_fails _msgs =
  Error (Error.Internal "network timeout")

let test_heal_valid_first_try () =
  let args = `Assoc [("n", `Int 7)] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_fails () with
  | Ok r ->
    Alcotest.(check int) "attempts" 1 r.attempts;
    Alcotest.(check bool) "not healed" false r.healed;
    Alcotest.(check string) "final tool_use_id" "tu1" r.final_tool_use_id
  | Error _ -> Alcotest.fail "should succeed without calling LLM"

let test_heal_coerced_first_try () =
  let args = `Assoc [("n", `String "7")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_fails () with
  | Ok r ->
    Alcotest.(check int) "attempts" 1 r.attempts;
    Alcotest.(check bool) "not healed" false r.healed;
    Alcotest.(check string) "final tool_use_id" "tu1" r.final_tool_use_id;
    let v = Yojson.Safe.Util.member "n" r.value in
    Alcotest.(check int) "coerced" 7 (match v with `Int i -> i | _ -> -1)
  | Error _ -> Alcotest.fail "coercion should succeed"

let test_heal_retry_fixes () =
  let args = `Assoc [("n", `String "bad")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_fixes () with
  | Ok r ->
    Alcotest.(check int) "attempts" 2 r.attempts;
    Alcotest.(check bool) "healed" true r.healed;
    Alcotest.(check string) "final tool_use_id" "fix1" r.final_tool_use_id;
    let v = Yojson.Safe.Util.member "n" r.value in
    Alcotest.(check int) "fixed" 42 (match v with `Int i -> i | _ -> -1)
  | Error _ -> Alcotest.fail "should heal after 1 retry"

let test_heal_exhausted () =
  let always_bad _msgs = mock_response (`Assoc [("n", `String "still_bad")]) in
  let args = `Assoc [("n", `String "bad")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:always_bad
    ~max_retries:2 () with
  | Error (Tool_middleware.Exhausted { attempts; limit; _ }) ->
    Alcotest.(check int) "attempts" 3 attempts;
    Alcotest.(check int) "limit" 2 limit
  | Error _ -> Alcotest.fail "wrong error type"
  | Ok _ -> Alcotest.fail "should exhaust retries"

let test_heal_llm_no_tool_call () =
  let args = `Assoc [("n", `String "bad")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_text_only () with
  | Error (Tool_middleware.Exhausted { last_error; _ }) ->
    Alcotest.(check bool) "mentions tool name" true
      (string_contains ~sub:"calc" last_error);
    Alcotest.(check bool) "mentions missing tool call" true
      (string_contains ~sub:"no" last_error &&
       string_contains ~sub:"tool call" last_error)
  | Error _ -> Alcotest.fail "wrong error type"
  | Ok _ -> Alcotest.fail "should fail when LLM returns no tool call"

let test_heal_llm_error () =
  let args = `Assoc [("n", `String "bad")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_fails () with
  | Error (Tool_middleware.Llm_error _) -> ()
  | Error _ -> Alcotest.fail "wrong error type"
  | Ok _ -> Alcotest.fail "should fail on LLM error"

let test_heal_on_retry_called () =
  let count = ref 0 in
  let on_retry ~attempt:_ ~error:_ = incr count in
  let always_bad _msgs = mock_response (`Assoc [("n", `String "x")]) in
  let args = `Assoc [("n", `String "bad")] in
  ignore (Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:always_bad
    ~max_retries:2 ~on_retry ());
  Alcotest.(check int) "on_retry called" 2 !count

let test_heal_max_retries_zero () =
  let args = `Assoc [("n", `String "bad")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_fixes
    ~max_retries:0 () with
  | Error (Tool_middleware.Exhausted { attempts; _ }) ->
    Alcotest.(check int) "attempts" 1 attempts
  | _ -> Alcotest.fail "max_retries=0 should exhaust immediately"

let test_heal_negative_max_retries_clamped () =
  let args = `Assoc [("n", `String "bad")] in
  match Tool_middleware.heal_tool_call ~tool_name:"calc" ~schema:int_schema
    ~tool_use_id:"tu1" ~args ~prior_messages:[] ~llm:mock_llm_fixes
    ~max_retries:(-3) () with
  | Error (Tool_middleware.Exhausted { attempts; limit; _ }) ->
    Alcotest.(check int) "attempts" 1 attempts;
    Alcotest.(check int) "limit" 0 limit
  | _ -> Alcotest.fail "negative max_retries should clamp to zero"

(* ── Runner ──────────────────────────────────────────────── *)

let () =
  Alcotest.run "Tool_middleware" [
    ("validate_and_coerce", [
      Alcotest.test_case "pass: no params" `Quick test_pass_no_params;
      Alcotest.test_case "pass: correct types" `Quick test_pass_correct_types;
      Alcotest.test_case "proceed: int coercion" `Quick test_proceed_coercion;
      Alcotest.test_case "proceed: bool coercion" `Quick test_proceed_bool_coercion;
      Alcotest.test_case "reject: invalid" `Quick test_reject_invalid;
      Alcotest.test_case "reject: missing required" `Quick test_reject_missing_required;
    ]);
    ("tool_schema_of_json", [
      Alcotest.test_case "basic conversion" `Quick test_schema_of_json;
      Alcotest.test_case "empty schema" `Quick test_schema_of_json_empty;
    ]);
    ("make_validation_hook", [
      Alcotest.test_case "unknown tool -> Pass" `Quick test_hook_unknown_tool;
      Alcotest.test_case "valid tool -> Pass" `Quick test_hook_valid_tool;
      Alcotest.test_case "coercion -> Proceed" `Quick test_hook_coercion;
      Alcotest.test_case "invalid -> Reject" `Quick test_hook_rejection;
    ]);
    ("heal_tool_call", [
      Alcotest.test_case "valid first try" `Quick test_heal_valid_first_try;
      Alcotest.test_case "coerced first try" `Quick test_heal_coerced_first_try;
      Alcotest.test_case "retry fixes" `Quick test_heal_retry_fixes;
      Alcotest.test_case "exhausted" `Quick test_heal_exhausted;
      Alcotest.test_case "LLM returns no tool call" `Quick test_heal_llm_no_tool_call;
       Alcotest.test_case "LLM error" `Quick test_heal_llm_error;
       Alcotest.test_case "on_retry callback" `Quick test_heal_on_retry_called;
       Alcotest.test_case "max_retries=0" `Quick test_heal_max_retries_zero;
       Alcotest.test_case "negative max_retries clamps to 0" `Quick
         test_heal_negative_max_retries_clamped;
     ]);
   ]
