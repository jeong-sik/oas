(** Tests for tool.ml -- handler execution, context passing, schema generation *)

open Alcotest
open Agent_sdk

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

let test_invocation_handler_receives_exact_id () =
  let tool =
    Tool.create_with_invocation
      ~name:"invocation"
      ~description:"Read invocation"
      ~parameters:[]
      (fun invocation _input ->
         Ok
           { Types.content =
               Printf.sprintf
                 "%s:%d:%d"
                 (Tool.Invocation.tool_use_id invocation)
                 (Tool.Invocation.turn invocation)
                 (Tool.Invocation.planned_index invocation)
           ; _meta = None
           })
  in
  let invocation =
    Tool.Invocation.create ~tool_use_id:"provider-call-17" ~turn:4 ~planned_index:2
  in
  match Tool.execute ~invocation tool `Null with
  | Ok { content; _meta = _ } ->
    check string "exact occurrence" "provider-call-17:4:2" content
  | Error _ -> fail "expected invocation-aware tool to run"
;;

let test_invocation_handler_requires_invocation () =
  let tool =
    Tool.create_with_invocation
      ~name:"invocation"
      ~description:"Read invocation"
      ~parameters:[]
      (fun _invocation _input -> Ok { Types.content = "unexpected"; _meta = None })
  in
  match Tool.execute tool `Null with
  | Error { message; recoverable; error_class } ->
    check
      string
      "error message"
      "invocation-aware tool requires explicit invocation"
      message;
    check bool "not recoverable" false recoverable;
    check
      bool
      "deterministic"
      true
      (match error_class with
       | Some Types.Deterministic -> true
       | _ -> false)
  | Ok _ -> fail "expected missing-invocation error"
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
      ~descriptor:{ Tool.execution_mode = Concurrent }
      ~name:"shell_exec"
      ~description:"Run a command"
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
    "descriptor json has execution_mode"
    "concurrent"
    (descriptor_json |> member "execution_mode" |> to_string);
  check int "one structural field" 1 (descriptor_json |> to_assoc |> List.length)
;;

let test_descriptor_to_yojson_none () =
  let json = Tool.descriptor_to_yojson None in
  check string "null" (Yojson.Safe.to_string `Null) (Yojson.Safe.to_string json)
;;

let test_execution_mode_yojson_roundtrip () =
  let variants = [ Tool.Concurrent; Tool.Serial ] in
  List.iter
    (fun value ->
       let json = Tool.execution_mode_to_yojson value in
       match Tool.execution_mode_of_yojson json with
       | Ok decoded ->
         check
           string
           "execution mode roundtrip"
           (Tool.show_execution_mode value)
           (Tool.show_execution_mode decoded)
       | Error msg -> fail ("execution_mode roundtrip: " ^ msg))
    variants
;;

let test_missing_descriptor_defaults_to_serial () =
  let tool =
    Tool.create ~name:"plain" ~description:"" ~parameters:[] (fun _ ->
      Ok { Types.content = "ok"; _meta = None })
  in
  check
    string
    "serial"
    (Tool.show_execution_mode Tool.Serial)
    (Tool.show_execution_mode (Tool.execution_mode tool))
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
    ; ( "invocation_handler"
      , [ test_case "receives exact id" `Quick test_invocation_handler_receives_exact_id
        ; test_case
            "requires invocation"
            `Quick
            test_invocation_handler_requires_invocation
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
      , [ test_case "execution_mode" `Quick test_execution_mode_yojson_roundtrip
        ; test_case "descriptor None" `Quick test_descriptor_to_yojson_none
        ; test_case
            "missing descriptor is serial"
            `Quick
            test_missing_descriptor_defaults_to_serial
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
        ; test_case "works with invocation handler" `Quick (fun () ->
            let tool =
              Tool.create_with_invocation
                ~name:"invocation_greet"
                ~description:"Greet from invocation"
                ~parameters:[]
                (fun invocation input ->
                   let open Yojson.Safe.Util in
                   Ok
                     { Types.content =
                         Tool.Invocation.tool_use_id invocation
                         ^ ":"
                         ^ (input |> member "name" |> to_string)
                     ; _meta = None
                     })
            in
            let wrapped = Tool.with_defaults [ "name", `String "default" ] tool in
            let invocation =
              Tool.Invocation.create ~tool_use_id:"call-1" ~turn:3 ~planned_index:1
            in
            match Tool.execute ~invocation wrapped (`Assoc []) with
            | Ok { content; _meta = _ } ->
              check string "invocation and default preserved" "call-1:default" content
            | Error _ -> fail "expected Ok")
        ] )
    ]
;;
