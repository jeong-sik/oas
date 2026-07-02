(** Tests for tool.ml -- handler execution, context passing, schema generation *)

open Alcotest
open Agent_sdk

let contains_substring haystack needle =
  let hlen = String.length haystack
  and nlen = String.length needle in
  let rec scan i =
    if i + nlen > hlen
    then false
    else if String.sub haystack i nlen = needle
    then true
    else scan (i + 1)
  in
  if nlen = 0 then true else scan 0
;;

let sorted_strings xs = List.sort_uniq String.compare xs

let test_simple_handler_ok () =
  let tool =
    Tool.create
      ~name:"echo"
      ~description:"Echo input"
      ~parameters:
        [ { Types.name = "msg"
          ; description = "Message"
          ; param_type = Types.String
          ; required = true
          }
        ]
      (fun input ->
         let open Yojson.Safe.Util in
         Ok { Types.content = input |> member "msg" |> to_string; _meta = None })
  in
  let actual = Tool.execute tool (`Assoc [ "msg", `String "hello" ]) in
  match actual with
  | Ok { content; _meta = _ } -> check string "returns Ok" "hello" content
  | Error _ -> fail "expected Ok"
;;

let test_simple_handler_error () =
  let tool =
    Tool.create ~name:"fail" ~description:"Always fails" ~parameters:[] (fun _input ->
      Error
        { Types.message = "intentional error"; recoverable = true; error_class = None })
  in
  let actual = Tool.execute tool `Null in
  match actual with
  | Error { message; _ } -> check string "returns Error" "intentional error" message
  | Ok _ -> fail "expected Error"
;;

let test_context_handler_receives_context () =
  let tool =
    Tool.create_with_context
      ~name:"stateful"
      ~description:"Read from context"
      ~parameters:[]
      (fun ctx _input ->
         match Context.get ctx "key" with
         | Some (`String v) -> Ok { Types.content = v; _meta = None }
         | _ ->
           Error
             { Types.message = "key not found"; recoverable = true; error_class = None })
  in
  let ctx = Context.create_sync () in
  Context.set ctx "key" (`String "ctx_value");
  let actual = Tool.execute ~context:ctx tool `Null in
  match actual with
  | Ok { content; _meta = _ } -> check string "reads context" "ctx_value" content
  | Error _ -> fail "expected Ok"
;;

let test_context_handler_writes_context () =
  let tool =
    Tool.create_with_context
      ~name:"writer"
      ~description:"Write to context"
      ~parameters:[]
      (fun ctx _input ->
         Context.set ctx "written" (`Int 42);
         Ok { Types.content = "done"; _meta = None })
  in
  let ctx = Context.create_sync () in
  let _result = Tool.execute ~context:ctx tool `Null in
  check bool "context was written" true (Context.get ctx "written" = Some (`Int 42))
;;

let test_context_handler_requires_context () =
  let tool =
    Tool.create_with_context
      ~name:"noctx"
      ~description:"No explicit context"
      ~parameters:[]
      (fun _ctx _input -> Ok { Types.content = "works"; _meta = None })
  in
  let actual = Tool.execute tool `Null in
  match actual with
  | Error { message; recoverable; error_class } ->
    check string "error message" "context-aware tool requires explicit context" message;
    check bool "not recoverable" false recoverable;
    check
      bool
      "deterministic"
      true
      (match error_class with
       | Some Types.Deterministic -> true
       | _ -> false)
  | Ok _ -> fail "expected missing-context error"
;;

let test_schema_to_json_structure () =
  let tool =
    Tool.create
      ~name:"calc"
      ~description:"Calculate"
      ~parameters:
        [ { Types.name = "expr"
          ; description = "Expression"
          ; param_type = Types.String
          ; required = true
          }
        ; { Types.name = "precision"
          ; description = "Decimal places"
          ; param_type = Types.Integer
          ; required = false
          }
        ]
      (fun _input -> Ok { Types.content = ""; _meta = None })
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
  check (list string) "required" [ "expr" ] required
;;

let test_schema_param_types () =
  let params =
    [ { Types.name = "s"; description = ""; param_type = Types.String; required = false }
    ; { Types.name = "i"; description = ""; param_type = Types.Integer; required = false }
    ; { Types.name = "n"; description = ""; param_type = Types.Number; required = false }
    ; { Types.name = "b"; description = ""; param_type = Types.Boolean; required = false }
    ; { Types.name = "a"; description = ""; param_type = Types.Array; required = false }
    ; { Types.name = "o"; description = ""; param_type = Types.Object; required = false }
    ]
  in
  let tool =
    Tool.create ~name:"types" ~description:"" ~parameters:params (fun _input ->
      Ok { Types.content = ""; _meta = None })
  in
  let json = Tool.schema_to_json tool in
  let open Yojson.Safe.Util in
  let props = json |> member "input_schema" |> member "properties" in
  check string "string" "string" (props |> member "s" |> member "type" |> to_string);
  check string "integer" "integer" (props |> member "i" |> member "type" |> to_string);
  check string "number" "number" (props |> member "n" |> member "type" |> to_string);
  check string "boolean" "boolean" (props |> member "b" |> member "type" |> to_string);
  check string "array" "array" (props |> member "a" |> member "type" |> to_string);
  check string "object" "object" (props |> member "o" |> member "type" |> to_string)
;;

let test_descriptor_preserved_and_not_in_schema () =
  let tool =
    Tool.create
      ~descriptor:
        { Tool.kind = Some "shell"
        ; mutation_class = None
        ; concurrency_class = Some Tool.Exclusive_external
        ; permission = Some Tool.Destructive
        ; evidence_role = None
        ; shell =
            Some
              { Tool.single_command_only = true
              ; shell_metacharacters_allowed = false
              ; chaining_allowed = false
              ; redirection_allowed = false
              ; pipes_allowed = false
              ; workdir_policy = Some Tool.Recommended
              }
        ; notes = [ "Use explicit workdir." ]
        ; examples = [ "python3 check.py" ]
        }
      ~name:"shell_exec"
      ~description:"Run a constrained shell command"
      ~parameters:
        [ { Types.name = "command"
          ; description = "Command"
          ; param_type = Types.String
          ; required = true
          }
        ]
      (fun _ -> Ok { Types.content = "ok"; _meta = None })
  in
  let descriptor = Tool.descriptor tool in
  check bool "descriptor present" true (Option.is_some descriptor);
  let descriptor_json = Tool.descriptor_to_yojson descriptor in
  let json = Tool.schema_to_json tool in
  let open Yojson.Safe.Util in
  check bool "descriptor not in wire schema" true (json |> member "descriptor" = `Null);
  check
    string
    "descriptor json has concurrency_class"
    (Yojson.Safe.to_string (`List [ `String "Exclusive_external" ]))
    (Yojson.Safe.to_string (descriptor_json |> member "concurrency_class"));
  check bool "descriptor json has shell" true (descriptor_json |> member "shell" <> `Null);
  check
    bool
    "descriptor json has examples"
    true
    (descriptor_json |> member "examples" <> `Null)
;;

(* ── Phase 4: descriptor yojson, workdir_policy ────────────────── *)

let test_workdir_policy_yojson_roundtrip () =
  let variants =
    [ Tool.Required, "required"
    ; Tool.Recommended, "recommended"
    ; Tool.None_expected, "none_expected"
    ]
  in
  List.iter
    (fun (v, expected_str) ->
       let json = Tool.workdir_policy_to_yojson v in
       match Tool.workdir_policy_of_yojson json with
       | Ok decoded ->
         check
           string
           "roundtrip"
           (Tool.show_workdir_policy v)
           (Tool.show_workdir_policy decoded)
       | Error msg ->
         fail (Printf.sprintf "workdir_policy roundtrip %s: %s" expected_str msg))
    variants
;;

let test_shell_constraints_yojson_roundtrip () =
  let value : Tool.shell_constraints =
    { single_command_only = true
    ; shell_metacharacters_allowed = false
    ; chaining_allowed = false
    ; redirection_allowed = true
    ; pipes_allowed = true
    ; workdir_policy = Some Tool.Required
    }
  in
  let json = Tool.shell_constraints_to_yojson value in
  match Tool.shell_constraints_of_yojson json with
  | Ok decoded ->
    check
      string
      "shell roundtrip"
      (Tool.show_shell_constraints value)
      (Tool.show_shell_constraints decoded)
  | Error msg -> fail ("shell_constraints roundtrip: " ^ msg)
;;

let test_descriptor_to_yojson_none () =
  let json = Tool.descriptor_to_yojson None in
  check string "null" (Yojson.Safe.to_string `Null) (Yojson.Safe.to_string json)
;;

let test_concurrency_class_yojson_roundtrip () =
  let variants =
    [ Tool.Parallel_read; Tool.Sequential_workspace; Tool.Exclusive_external ]
  in
  List.iter
    (fun value ->
       let json = Tool.concurrency_class_to_yojson value in
       match Tool.concurrency_class_of_yojson json with
       | Ok decoded ->
         check
           string
           "concurrency roundtrip"
           (Tool.show_concurrency_class value)
           (Tool.show_concurrency_class decoded)
       | Error msg -> fail ("concurrency_class roundtrip: " ^ msg))
    variants
;;

let test_mutation_class_yojson_roundtrip () =
  let variants =
    [ Tool.Read_only, "read_only"
    ; Tool.Workspace, "workspace"
    ; Tool.Workspace_mutating, "workspace_mutating"
    ; Tool.Local_mutation, "local_mutation"
    ; Tool.External, "external"
    ; Tool.External_effect, "external_effect"
    ]
  in
  List.iter
    (fun (value, expected) ->
       let json = Tool.mutation_class_to_yojson value in
       check string "canonical json" expected (Yojson.Safe.Util.to_string json);
       match Tool.mutation_class_of_yojson json with
       | Ok decoded ->
         check
           string
           "roundtrip"
           (Tool.show_mutation_class value)
           (Tool.show_mutation_class decoded)
       | Error msg -> fail ("mutation_class roundtrip: " ^ msg))
    variants
;;

let test_mutation_class_of_yojson_accepts_legacy_constructor_names () =
  let cases =
    [ "Read_only", Tool.Read_only
    ; "Workspace", Tool.Workspace
    ; "Workspace_mutating", Tool.Workspace_mutating
    ; "Local_mutation", Tool.Local_mutation
    ; "External", Tool.External
    ; "External_effect", Tool.External_effect
    ]
  in
  List.iter
    (fun (json_string, expected) ->
       match Tool.mutation_class_of_yojson (`String json_string) with
       | Ok decoded ->
         check
           string
           "legacy constructor"
           (Tool.show_mutation_class expected)
           (Tool.show_mutation_class decoded)
       | Error msg -> fail ("legacy mutation_class: " ^ msg))
    cases
;;

let test_mutation_class_expected_concurrency_class () =
  let check_mapping mutation_class expected =
    check
      (option string)
      (Tool.mutation_class_to_string mutation_class)
      expected
      (Option.map
         Tool.concurrency_class_name
         (Tool.expected_concurrency_class_of_mutation_class mutation_class))
  in
  check_mapping Tool.Read_only (Some "parallel_read");
  check_mapping Tool.Workspace (Some "sequential_workspace");
  check_mapping Tool.Workspace_mutating (Some "sequential_workspace");
  check_mapping Tool.Local_mutation (Some "sequential_workspace");
  check_mapping Tool.External (Some "exclusive_external");
  check_mapping Tool.External_effect (Some "exclusive_external");
  check
    (list string)
    "known mutation classes"
    (sorted_strings
       [ "read_only"
       ; "workspace"
       ; "workspace_mutating"
       ; "local_mutation"
       ; "external"
       ; "external_effect"
       ])
    (sorted_strings Tool.known_mutation_classes)
;;

let test_create_rejects_inconsistent_descriptor () =
  check_raises
    "invalid descriptor"
    (Invalid_argument
       "Tool.create: descriptor mismatch: mutation_class=read_only requires \
        concurrency_class=parallel_read")
    (fun () ->
       ignore
         (Tool.create
            ~descriptor:
              { Tool.kind = None
              ; mutation_class = Some Tool.Read_only
              ; concurrency_class = Some Tool.Sequential_workspace
              ; permission = None
              ; evidence_role = None
              ; shell = None
              ; notes = []
              ; examples = []
              }
            ~name:"bad"
            ~description:"bad"
            ~parameters:[]
            (fun _ -> Ok { Types.content = "ok"; _meta = None })))
;;

let test_create_rejects_workspace_mismatch () =
  check_raises
    "workspace mismatch"
    (Invalid_argument
       "Tool.create: descriptor mismatch: mutation_class=workspace requires \
        concurrency_class=sequential_workspace")
    (fun () ->
       ignore
         (Tool.create
            ~descriptor:
              { Tool.kind = None
              ; mutation_class = Some Tool.Workspace
              ; concurrency_class = Some Tool.Exclusive_external
              ; permission = None
              ; evidence_role = None
              ; shell = None
              ; notes = []
              ; examples = []
              }
            ~name:"bad"
            ~description:"bad"
            ~parameters:[]
            (fun _ -> Ok { Types.content = "ok"; _meta = None })))
;;

let test_create_rejects_external_mismatch () =
  check_raises
    "external mismatch"
    (Invalid_argument
       "Tool.create: descriptor mismatch: mutation_class=external requires \
        concurrency_class=exclusive_external")
    (fun () ->
       ignore
         (Tool.create
            ~descriptor:
              { Tool.kind = None
              ; mutation_class = Some Tool.External
              ; concurrency_class = Some Tool.Parallel_read
              ; permission = None
              ; evidence_role = None
              ; shell = None
              ; notes = []
              ; examples = []
              }
            ~name:"bad"
            ~description:"bad"
            ~parameters:[]
            (fun _ -> Ok { Types.content = "ok"; _meta = None })))
;;

let test_mutation_class_of_yojson_rejects_unknown () =
  let expected_classes = Tool.known_mutation_classes in
  match Tool.mutation_class_of_yojson (`String "nonexistent") with
  | Ok _ -> fail "expected unknown mutation_class error"
  | Error msg ->
    check
      bool
      "message mentions unknown mutation_class"
      true
      (contains_substring msg "unknown mutation_class: nonexistent");
    List.iter
      (fun cls -> check bool ("known class: " ^ cls) true (List.mem cls expected_classes))
      expected_classes
;;

let () =
  run
    "Tool"
    [ ( "simple_handler"
      , [ test_case "ok result" `Quick test_simple_handler_ok
        ; test_case "error result" `Quick test_simple_handler_error
        ] )
    ; ( "context_handler"
      , [ test_case "receives context" `Quick test_context_handler_receives_context
        ; test_case "writes context" `Quick test_context_handler_writes_context
        ; test_case "requires context" `Quick test_context_handler_requires_context
        ] )
    ; ( "schema"
      , [ test_case "json structure" `Quick test_schema_to_json_structure
        ; test_case "param types" `Quick test_schema_param_types
        ; test_case
            "descriptor preserved"
            `Quick
            test_descriptor_preserved_and_not_in_schema
        ] )
    ; ( "yojson_roundtrip"
      , [ test_case "workdir_policy" `Quick test_workdir_policy_yojson_roundtrip
        ; test_case "shell_constraints" `Quick test_shell_constraints_yojson_roundtrip
        ; test_case "concurrency_class" `Quick test_concurrency_class_yojson_roundtrip
        ; test_case "mutation_class" `Quick test_mutation_class_yojson_roundtrip
        ; test_case
            "mutation_class legacy constructors"
            `Quick
            test_mutation_class_of_yojson_accepts_legacy_constructor_names
        ; test_case "descriptor None" `Quick test_descriptor_to_yojson_none
        ] )
    ; ( "validation"
      , [ test_case
            "mutation class expected concurrency"
            `Quick
            test_mutation_class_expected_concurrency_class
        ; test_case
            "create rejects inconsistent descriptor"
            `Quick
            test_create_rejects_inconsistent_descriptor
        ; test_case
            "create rejects workspace mismatch"
            `Quick
            test_create_rejects_workspace_mismatch
        ; test_case
            "create rejects external mismatch"
            `Quick
            test_create_rejects_external_mismatch
        ; test_case
            "mutation_class_of_yojson rejects unknown"
            `Quick
            test_mutation_class_of_yojson_rejects_unknown
        ] )
    ; ( "with_defaults"
      , [ test_case "injects missing args" `Quick (fun () ->
            let tool =
              Tool.create
                ~name:"greet"
                ~description:"Greet"
                ~parameters:
                  [ { Types.name = "name"
                    ; description = "Name"
                    ; param_type = Types.String
                    ; required = true
                    }
                  ]
                (fun input ->
                   let open Yojson.Safe.Util in
                   Ok
                     { Types.content = input |> member "name" |> to_string; _meta = None })
            in
            let wrapped = Tool.with_defaults [ "name", `String "default_user" ] tool in
            match Tool.execute wrapped (`Assoc []) with
            | Ok { content; _meta = _ } ->
              check string "default injected" "default_user" content
            | Error _ -> fail "expected Ok")
        ; test_case "preserves explicit args" `Quick (fun () ->
            let tool =
              Tool.create ~name:"greet" ~description:"Greet" ~parameters:[] (fun input ->
                let open Yojson.Safe.Util in
                Ok { Types.content = input |> member "name" |> to_string; _meta = None })
            in
            let wrapped = Tool.with_defaults [ "name", `String "default_user" ] tool in
            match Tool.execute wrapped (`Assoc [ "name", `String "alice" ]) with
            | Ok { content; _meta = _ } ->
              check string "explicit preserved" "alice" content
            | Error _ -> fail "expected Ok")
        ; test_case "works with context handler" `Quick (fun () ->
            let tool =
              Tool.create_with_context
                ~name:"ctx_greet"
                ~description:"Greet with context"
                ~parameters:[]
                (fun _ctx input ->
                   let open Yojson.Safe.Util in
                   Ok
                     { Types.content = input |> member "agent" |> to_string
                     ; _meta = None
                     })
            in
            let wrapped = Tool.with_defaults [ "agent", `String "worker-1" ] tool in
            let ctx = Context.create_sync () in
            match Tool.execute ~context:ctx wrapped (`Assoc []) with
            | Ok { content; _meta = _ } ->
              check string "default in ctx handler" "worker-1" content
            | Error _ -> fail "expected Ok")
        ] )
      (* RFC-OAS-011 OAS-E PR-5: removed the "builtin_descriptor" test group.
         It exercised Mode_enforcer.builtin_descriptor — the CDAL boundary
         API that RFC-OAS-009 v2 PR-B/C unwired and PR-6 will delete. The
         tests were already obsolete after RFC-OAS-009 v2 (descriptor is
         now consumer-supplied, not builtin-derived). *)
    ]
;;
