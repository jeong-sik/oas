(** Tests for tool.ml -- handler execution, context passing, schema generation *)

open Alcotest
open Agent_sdk

let test_simple_handler_ok () =
  let tool = Tool.create
    ~name:"echo"
    ~description:"Echo input"
    ~parameters:[{
      Types.name = "msg"; description = "Message";
      param_type = Types.String; required = true;
    }]
    (fun input ->
      let open Yojson.Safe.Util in
      Ok (input |> member "msg" |> to_string))
  in
  let actual = Tool.execute tool (`Assoc [("msg", `String "hello")]) in
  check (result string string) "returns Ok" (Ok "hello") actual

let test_simple_handler_error () =
  let tool = Tool.create
    ~name:"fail"
    ~description:"Always fails"
    ~parameters:[]
    (fun _input -> Error "intentional error")
  in
  let actual = Tool.execute tool `Null in
  check (result string string) "returns Error" (Error "intentional error") actual

let test_context_handler_receives_context () =
  let tool = Tool.create_with_context
    ~name:"stateful"
    ~description:"Read from context"
    ~parameters:[]
    (fun ctx _input ->
      match Context.get ctx "key" with
      | Some (`String v) -> Ok v
      | _ -> Error "key not found")
  in
  let ctx = Context.create () in
  Context.set ctx "key" (`String "ctx_value");
  let actual = Tool.execute ~context:ctx tool `Null in
  check (result string string) "reads context" (Ok "ctx_value") actual

let test_context_handler_writes_context () =
  let tool = Tool.create_with_context
    ~name:"writer"
    ~description:"Write to context"
    ~parameters:[]
    (fun ctx _input ->
      Context.set ctx "written" (`Int 42);
      Ok "done")
  in
  let ctx = Context.create () in
  let _result = Tool.execute ~context:ctx tool `Null in
  check bool "context was written" true
    (Context.get ctx "written" = Some (`Int 42))

let test_context_handler_default_context () =
  let tool = Tool.create_with_context
    ~name:"noctx"
    ~description:"No explicit context"
    ~parameters:[]
    (fun _ctx _input -> Ok "works")
  in
  let actual = Tool.execute tool `Null in
  check (result string string) "default context" (Ok "works") actual

let test_schema_to_json_structure () =
  let tool = Tool.create
    ~name:"calc"
    ~description:"Calculate"
    ~parameters:[
      { Types.name = "expr"; description = "Expression";
        param_type = Types.String; required = true };
      { Types.name = "precision"; description = "Decimal places";
        param_type = Types.Integer; required = false };
    ]
    (fun _input -> Ok "")
  in
  let json = Tool.schema_to_json tool in
  let open Yojson.Safe.Util in
  check string "name" "calc" (json |> member "name" |> to_string);
  check string "description" "Calculate" (json |> member "description" |> to_string);
  let schema = json |> member "input_schema" in
  check string "schema type" "object" (schema |> member "type" |> to_string);
  let props = schema |> member "properties" in
  let expr_prop = props |> member "expr" in
  check string "expr type" "string" (expr_prop |> member "type" |> to_string);
  let prec_prop = props |> member "precision" in
  check string "precision type" "integer" (prec_prop |> member "type" |> to_string);
  let required = schema |> member "required" |> to_list |> List.map to_string in
  check (list string) "required" ["expr"] required

let test_schema_param_types () =
  let params = [
    { Types.name = "s"; description = ""; param_type = Types.String; required = false };
    { Types.name = "i"; description = ""; param_type = Types.Integer; required = false };
    { Types.name = "n"; description = ""; param_type = Types.Number; required = false };
    { Types.name = "b"; description = ""; param_type = Types.Boolean; required = false };
    { Types.name = "a"; description = ""; param_type = Types.Array; required = false };
    { Types.name = "o"; description = ""; param_type = Types.Object; required = false };
  ] in
  let tool = Tool.create ~name:"types" ~description:"" ~parameters:params
    (fun _input -> Ok "") in
  let json = Tool.schema_to_json tool in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  check string "string" "string" (props |> member "s" |> member "type" |> to_string);
  check string "integer" "integer" (props |> member "i" |> member "type" |> to_string);
  check string "number" "number" (props |> member "n" |> member "type" |> to_string);
  check string "boolean" "boolean" (props |> member "b" |> member "type" |> to_string);
  check string "array" "array" (props |> member "a" |> member "type" |> to_string);
  check string "object" "object" (props |> member "o" |> member "type" |> to_string)

let test_descriptor_preserved_and_not_in_schema () =
  let tool =
    Tool.create
      ~descriptor:
        {
          Tool.kind = Some "shell";
          shell =
            Some
              {
                Tool.single_command_only = true;
                shell_metacharacters_allowed = false;
                workdir_policy = Some Tool.Recommended;
              };
          notes = [ "Use explicit workdir." ];
        }
      ~name:"shell_exec"
      ~description:"Run a constrained shell command"
      ~parameters:
        [
          {
            Types.name = "command";
            description = "Command";
            param_type = Types.String;
            required = true;
          };
        ]
      (fun _ -> Ok "ok")
  in
  let descriptor = Tool.descriptor tool in
  check bool "descriptor present" true (Option.is_some descriptor);
  let json = Tool.schema_to_json tool in
  let open Yojson.Safe.Util in
  check bool "descriptor not in wire schema" true
    (json |> member "descriptor" = `Null)

let () =
  run "Tool" [
    "simple_handler", [
      test_case "ok result" `Quick test_simple_handler_ok;
      test_case "error result" `Quick test_simple_handler_error;
    ];
    "context_handler", [
      test_case "receives context" `Quick test_context_handler_receives_context;
      test_case "writes context" `Quick test_context_handler_writes_context;
      test_case "default context" `Quick test_context_handler_default_context;
    ];
    "schema", [
      test_case "json structure" `Quick test_schema_to_json_structure;
      test_case "param types" `Quick test_schema_param_types;
      test_case "descriptor preserved" `Quick
        test_descriptor_preserved_and_not_in_schema;
    ];
  ]
