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
  ]
