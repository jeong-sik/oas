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
      Ok { Types.content = input |> member "msg" |> to_string })
  in
  let actual = Tool.execute tool (`Assoc [("msg", `String "hello")]) in
  match actual with
  | Ok { content } -> check string "returns Ok" "hello" content
  | Error _ -> fail "expected Ok"

let test_simple_handler_error () =
  let tool = Tool.create
    ~name:"fail"
    ~description:"Always fails"
    ~parameters:[]
    (fun _input -> Error { Types.message = "intentional error"; recoverable = true })
  in
  let actual = Tool.execute tool `Null in
  match actual with
  | Error { message; _ } -> check string "returns Error" "intentional error" message
  | Ok _ -> fail "expected Error"

let test_context_handler_receives_context () =
  let tool = Tool.create_with_context
    ~name:"stateful"
    ~description:"Read from context"
    ~parameters:[]
    (fun ctx _input ->
      match Context.get ctx "key" with
      | Some (`String v) -> Ok { Types.content = v }
      | _ -> Error { Types.message = "key not found"; recoverable = true })
  in
  let ctx = Context.create () in
  Context.set ctx "key" (`String "ctx_value");
  let actual = Tool.execute ~context:ctx tool `Null in
  match actual with
  | Ok { content } -> check string "reads context" "ctx_value" content
  | Error _ -> fail "expected Ok"

let test_context_handler_writes_context () =
  let tool = Tool.create_with_context
    ~name:"writer"
    ~description:"Write to context"
    ~parameters:[]
    (fun ctx _input ->
      Context.set ctx "written" (`Int 42);
      Ok { Types.content = "done" })
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
    (fun _ctx _input -> Ok { Types.content = "works" })
  in
  let actual = Tool.execute tool `Null in
  match actual with
  | Ok { content } -> check string "default context" "works" content
  | Error _ -> fail "expected Ok"

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
    (fun _input -> Ok { Types.content = "" })
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
    (fun _input -> Ok { Types.content = "" }) in
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
          mutation_class = None;
          concurrency_class = Some Tool.Exclusive_external;
          permission = Some Tool.Destructive;
          shell =
            Some
              {
                Tool.single_command_only = true;
                shell_metacharacters_allowed = false;
                chaining_allowed = false;
                redirection_allowed = false;
                pipes_allowed = false;
                workdir_policy = Some Tool.Recommended;
              };
          notes = [ "Use explicit workdir." ];
          examples = [ "python3 check.py" ];
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
      (fun _ -> Ok { Types.content = "ok" })
  in
  let descriptor = Tool.descriptor tool in
  check bool "descriptor present" true (Option.is_some descriptor);
  let descriptor_json = Tool.descriptor_to_yojson descriptor in
  let json = Tool.schema_to_json tool in
  let open Yojson.Safe.Util in
  check bool "descriptor not in wire schema" true
    (json |> member "descriptor" = `Null);
  check string "descriptor json has concurrency_class"
    (Yojson.Safe.to_string (`List [ `String "Exclusive_external" ]))
    (Yojson.Safe.to_string (descriptor_json |> member "concurrency_class"));
  check bool "descriptor json has shell" true
    (descriptor_json |> member "shell" <> `Null);
  check bool "descriptor json has examples" true
    (descriptor_json |> member "examples" <> `Null)

(* ── Phase 4: descriptor yojson, workdir_policy ────────────────── *)

let test_workdir_policy_yojson_roundtrip () =
  let variants = [
    (Tool.Required, "required");
    (Tool.Recommended, "recommended");
    (Tool.None_expected, "none_expected");
  ] in
  List.iter (fun (v, expected_str) ->
    let json = Tool.workdir_policy_to_yojson v in
    match Tool.workdir_policy_of_yojson json with
    | Ok decoded ->
      check string "roundtrip" (Tool.show_workdir_policy v) (Tool.show_workdir_policy decoded)
    | Error msg -> fail (Printf.sprintf "workdir_policy roundtrip %s: %s" expected_str msg)
  ) variants

let test_shell_constraints_yojson_roundtrip () =
  let value : Tool.shell_constraints = {
    single_command_only = true;
    shell_metacharacters_allowed = false;
    chaining_allowed = false;
    redirection_allowed = true;
    pipes_allowed = true;
    workdir_policy = Some Tool.Required;
  } in
  let json = Tool.shell_constraints_to_yojson value in
  match Tool.shell_constraints_of_yojson json with
  | Ok decoded ->
    check string "shell roundtrip"
      (Tool.show_shell_constraints value)
      (Tool.show_shell_constraints decoded)
  | Error msg -> fail ("shell_constraints roundtrip: " ^ msg)

let test_descriptor_to_yojson_none () =
  let json = Tool.descriptor_to_yojson None in
  check string "null" (Yojson.Safe.to_string `Null) (Yojson.Safe.to_string json)

let test_concurrency_class_yojson_roundtrip () =
  let variants = [
    Tool.Parallel_read;
    Tool.Sequential_workspace;
    Tool.Exclusive_external;
  ] in
  List.iter (fun value ->
    let json = Tool.concurrency_class_to_yojson value in
    match Tool.concurrency_class_of_yojson json with
    | Ok decoded ->
        check string "concurrency roundtrip"
          (Tool.show_concurrency_class value)
          (Tool.show_concurrency_class decoded)
    | Error msg ->
        fail ("concurrency_class roundtrip: " ^ msg)
  ) variants

let test_create_rejects_inconsistent_descriptor () =
  check_raises
    "invalid descriptor"
    (Invalid_argument
       "Tool.create: descriptor mismatch: mutation_class=read_only requires concurrency_class=parallel_read")
    (fun () ->
       ignore
         (Tool.create
            ~descriptor:
              {
                Tool.kind = None;
                mutation_class = Some "read_only";
                concurrency_class = Some Tool.Sequential_workspace;
                permission = None;
                shell = None;
                notes = [];
                examples = [];
              }
            ~name:"bad"
            ~description:"bad"
            ~parameters:[]
            (fun _ -> Ok { Types.content = "ok" })))

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
    "yojson_roundtrip", [
      test_case "workdir_policy" `Quick test_workdir_policy_yojson_roundtrip;
      test_case "shell_constraints" `Quick test_shell_constraints_yojson_roundtrip;
      test_case "concurrency_class" `Quick test_concurrency_class_yojson_roundtrip;
      test_case "descriptor None" `Quick test_descriptor_to_yojson_none;
    ];
    "validation", [
      test_case "create rejects inconsistent descriptor" `Quick
        test_create_rejects_inconsistent_descriptor;
    ];
    "with_defaults", [
      test_case "injects missing args" `Quick (fun () ->
        let tool = Tool.create
          ~name:"greet" ~description:"Greet"
          ~parameters:[{
            Types.name = "name"; description = "Name";
            param_type = Types.String; required = true;
          }]
          (fun input ->
            let open Yojson.Safe.Util in
            Ok { Types.content = input |> member "name" |> to_string })
        in
        let wrapped = Tool.with_defaults
          [("name", `String "default_user")] tool in
        match Tool.execute wrapped (`Assoc []) with
        | Ok { content } -> check string "default injected" "default_user" content
        | Error _ -> fail "expected Ok"
      );
      test_case "preserves explicit args" `Quick (fun () ->
        let tool = Tool.create
          ~name:"greet" ~description:"Greet"
          ~parameters:[]
          (fun input ->
            let open Yojson.Safe.Util in
            Ok { Types.content = input |> member "name" |> to_string })
        in
        let wrapped = Tool.with_defaults
          [("name", `String "default_user")] tool in
        match Tool.execute wrapped (`Assoc [("name", `String "alice")]) with
        | Ok { content } -> check string "explicit preserved" "alice" content
        | Error _ -> fail "expected Ok"
      );
      test_case "works with context handler" `Quick (fun () ->
        let tool = Tool.create_with_context
          ~name:"ctx_greet" ~description:"Greet with context"
          ~parameters:[]
          (fun _ctx input ->
            let open Yojson.Safe.Util in
            Ok { Types.content = input |> member "agent" |> to_string })
        in
        let wrapped = Tool.with_defaults
          [("agent", `String "worker-1")] tool in
        let ctx = Context.create () in
        match Tool.execute ~context:ctx wrapped (`Assoc []) with
        | Ok { content } -> check string "default in ctx handler" "worker-1" content
        | Error _ -> fail "expected Ok"
      );
    ];
    "builtin_descriptor", [
      test_case "read is Parallel_read" `Quick (fun () ->
        match Mode_enforcer.builtin_descriptor "read" with
        | None -> fail "expected Some descriptor for read"
        | Some d ->
          check (option string) "mutation_class" (Some "read_only") d.Tool.mutation_class;
          check bool "concurrency is Parallel_read" true
            (d.Tool.concurrency_class = Some Tool.Parallel_read);
          check bool "permission is ReadOnly" true
            (d.Tool.permission = Some Tool.ReadOnly)
      );
      test_case "write is Sequential_workspace" `Quick (fun () ->
        match Mode_enforcer.builtin_descriptor "write" with
        | None -> fail "expected Some descriptor for write"
        | Some d ->
          check (option string) "mutation_class" (Some "local_mutation") d.Tool.mutation_class;
          check bool "concurrency is Sequential_workspace" true
            (d.Tool.concurrency_class = Some Tool.Sequential_workspace);
          check bool "permission is Write" true
            (d.Tool.permission = Some Tool.Write)
      );
      test_case "bash is Exclusive_external with shell constraints" `Quick (fun () ->
        match Mode_enforcer.builtin_descriptor "bash" with
        | None -> fail "expected Some descriptor for bash"
        | Some d ->
          check (option string) "mutation_class" (Some "external_effect") d.Tool.mutation_class;
          check bool "concurrency is Exclusive_external" true
            (d.Tool.concurrency_class = Some Tool.Exclusive_external);
          check bool "permission is Destructive" true
            (d.Tool.permission = Some Tool.Destructive);
          check bool "has shell constraints" true (Option.is_some d.Tool.shell)
      );
      test_case "unknown tool returns None" `Quick (fun () ->
        check bool "no descriptor" true
          (Mode_enforcer.builtin_descriptor "nonexistent_tool_xyz" = None)
      );
      test_case "case insensitive" `Quick (fun () ->
        check bool "READ returns Some" true
          (Option.is_some (Mode_enforcer.builtin_descriptor "READ"))
      );
    ];
  ]
