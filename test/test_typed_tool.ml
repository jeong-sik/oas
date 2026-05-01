open Base
(** Tests for Typed_tool — compile-time type safety + runtime correctness. *)

open Agent_sdk

(* ── Test fixtures ──────────────────────────────────────── *)

type greet_input =
  { name : string
  ; shout : bool
  }

type greet_output = { greeting : string }

let greet_params : Types.tool_param list =
  [ { name = "name"; description = "Who to greet"; param_type = String; required = true }
  ; { name = "shout"
    ; description = "Uppercase output"
    ; param_type = Boolean
    ; required = false
    }
  ]
;;

let parse_greet (json : Yojson.Safe.t) : (greet_input, string) result =
  let open Yojson.Safe.Util in
  try
    let name = json |> member "name" |> to_string in
    let shout =
      try json |> member "shout" |> to_bool with
      | _ -> false
    in
    Ok { name; shout }
  with
  | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
;;

let handle_greet (input : greet_input) : (greet_output, string) result =
  if input.name = ""
  then Error "name cannot be empty"
  else (
    let greeting = "Hello, " ^ input.name in
    let greeting = if input.shout then String.uppercase_ascii greeting else greeting in
    Ok { greeting })
;;

let encode_greet (output : greet_output) : Yojson.Safe.t =
  `Assoc [ "greeting", `String output.greeting ]
;;

let greet_tool =
  Typed_tool.create
    ~name:"greet"
    ~description:"Greet someone"
    ~params:greet_params
    ~parse:parse_greet
    ~handler:handle_greet
    ~encode:encode_greet
    ()
;;

(* ── Context-aware fixture ──────────────────────────────── *)

let greet_ctx_tool =
  Typed_tool.create_with_context
    ~name:"greet_ctx"
    ~description:"Greet with context"
    ~params:greet_params
    ~parse:parse_greet
    ~handler:(fun ctx input ->
      let prefix =
        match Context.get ctx "prefix" with
        | Some (`String s) -> s ^ " "
        | _ -> ""
      in
      Ok { greeting = prefix ^ "Hello, " ^ input.name })
    ~encode:encode_greet
    ()
;;

(* ── Tests ──────────────────────────────────────────────── *)

let test_execute_success () =
  let input = `Assoc [ "name", `String "Vincent"; "shout", `Bool false ] in
  match Typed_tool.execute greet_tool input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let greeting = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "greeting" "Hello, Vincent" greeting
  | Error e -> Alcotest.fail e.message
;;

let test_execute_shout () =
  let input = `Assoc [ "name", `String "Vincent"; "shout", `Bool true ] in
  match Typed_tool.execute greet_tool input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let greeting = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "uppercase" "HELLO, VINCENT" greeting
  | Error e -> Alcotest.fail e.message
;;

let test_execute_parse_error () =
  let input = `Assoc [ "name", `Int 42 ] in
  match Typed_tool.execute greet_tool input with
  | Ok _ -> Alcotest.fail "expected parse error"
  | Error e -> Alcotest.(check bool) "recoverable" true e.recoverable
;;

let test_execute_handler_error () =
  let input = `Assoc [ "name", `String "" ] in
  match Typed_tool.execute greet_tool input with
  | Ok _ -> Alcotest.fail "expected handler error"
  | Error e ->
    Alcotest.(check bool) "not recoverable" false e.recoverable;
    Alcotest.(check bool)
      "contains 'empty'"
      true
      (try
         let _ = Str.search_forward (Str.regexp_string "empty") e.message 0 in
         true
       with
       | Not_found -> false)
;;

let test_execute_parsed_success () =
  let input = `Assoc [ "name", `String "OCaml" ] in
  match Typed_tool.execute_parsed greet_tool input with
  | Ok (parsed_input, Ok output) ->
    Alcotest.(check string) "parsed name" "OCaml" parsed_input.name;
    Alcotest.(check string) "output" "Hello, OCaml" output.greeting
  | Ok (_, Error e) -> Alcotest.fail ("handler error: " ^ e)
  | Error e -> Alcotest.fail ("parse error: " ^ e)
;;

let test_execute_parsed_parse_error () =
  let input = `String "not an object" in
  match Typed_tool.execute_parsed greet_tool input with
  | Error _ -> () (* expected *)
  | Ok _ -> Alcotest.fail "expected parse error"
;;

let test_to_untyped_bridge () =
  let untyped = Typed_tool.to_untyped greet_tool in
  let input = `Assoc [ "name", `String "Bridge" ] in
  match Tool.execute untyped input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let greeting = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "bridge works" "Hello, Bridge" greeting
  | Error e -> Alcotest.fail e.message
;;

let test_to_untyped_preserves_schema () =
  let untyped = Typed_tool.to_untyped greet_tool in
  Alcotest.(check string) "name preserved" "greet" untyped.schema.name;
  Alcotest.(check int) "params count" 2 (List.length untyped.schema.parameters)
;;

let test_schema_extraction () =
  let schema = Typed_tool.schema greet_tool in
  Alcotest.(check string) "name" "greet" schema.name;
  Alcotest.(check string) "desc" "Greet someone" schema.description
;;

let test_name_extraction () =
  Alcotest.(check string) "name" "greet" (Typed_tool.name greet_tool)
;;

let test_descriptor_none () =
  Alcotest.(check bool) "no descriptor" true (Typed_tool.descriptor greet_tool = None)
;;

let test_descriptor_some () =
  let tool =
    Typed_tool.create
      ~name:"safe_read"
      ~description:"Read-only tool"
      ~params:[]
      ~parse:(fun _ -> Ok ())
      ~handler:(fun () -> Ok ())
      ~encode:(fun () -> `Null)
      ~descriptor:
        { kind = Some "read"
        ; mutation_class = Some "read_only"
        ; concurrency_class = Some Tool.Parallel_read
        ; permission = Some Tool.ReadOnly
        ; shell = None
        ; notes = []
        ; examples = []
        }
      ()
  in
  match Typed_tool.descriptor tool with
  | Some d -> Alcotest.(check bool) "read_only" true (d.permission = Some Tool.ReadOnly)
  | None -> Alcotest.fail "expected descriptor"
;;

let test_context_handler () =
  let ctx = Context.create () in
  Context.set ctx "prefix" (`String "Dear");
  let input = `Assoc [ "name", `String "Admin" ] in
  match Typed_tool.execute ~context:ctx greet_ctx_tool input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let greeting = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "with prefix" "Dear Hello, Admin" greeting
  | Error e -> Alcotest.fail e.message
;;

let test_context_handler_no_context () =
  let input = `Assoc [ "name", `String "Admin" ] in
  match Typed_tool.execute greet_ctx_tool input with
  | Ok _ -> Alcotest.fail "expected error when context missing"
  | Error e -> Alcotest.(check bool) "not recoverable" false e.recoverable
;;

let test_to_untyped_context_bridge () =
  let untyped = Typed_tool.to_untyped greet_ctx_tool in
  let ctx = Context.create () in
  Context.set ctx "prefix" (`String "Hey");
  let input = `Assoc [ "name", `String "World" ] in
  match Tool.execute ~context:ctx untyped input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let greeting = Yojson.Safe.Util.(json |> member "greeting" |> to_string) in
    Alcotest.(check string) "ctx bridge" "Hey Hello, World" greeting
  | Error e -> Alcotest.fail e.message
;;

let test_null_input_parse () =
  match Typed_tool.execute greet_tool `Null with
  | Error e -> Alcotest.(check bool) "recoverable" true e.recoverable
  | Ok _ -> Alcotest.fail "expected error on null input"
;;

(* ── Test runner ────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Typed_tool"
    [ ( "execute"
      , [ Alcotest.test_case "success" `Quick test_execute_success
        ; Alcotest.test_case "shout mode" `Quick test_execute_shout
        ; Alcotest.test_case "parse error is recoverable" `Quick test_execute_parse_error
        ; Alcotest.test_case
            "handler error is not recoverable"
            `Quick
            test_execute_handler_error
        ; Alcotest.test_case "null input" `Quick test_null_input_parse
        ] )
    ; ( "execute_parsed"
      , [ Alcotest.test_case
            "success with intermediate"
            `Quick
            test_execute_parsed_success
        ; Alcotest.test_case "parse error" `Quick test_execute_parsed_parse_error
        ] )
    ; ( "to_untyped"
      , [ Alcotest.test_case "bridge execution" `Quick test_to_untyped_bridge
        ; Alcotest.test_case "preserves schema" `Quick test_to_untyped_preserves_schema
        ; Alcotest.test_case "context bridge" `Quick test_to_untyped_context_bridge
        ] )
    ; ( "introspection"
      , [ Alcotest.test_case "schema" `Quick test_schema_extraction
        ; Alcotest.test_case "name" `Quick test_name_extraction
        ; Alcotest.test_case "descriptor none" `Quick test_descriptor_none
        ; Alcotest.test_case "descriptor some" `Quick test_descriptor_some
        ] )
    ; ( "context_handler"
      , [ Alcotest.test_case "with context" `Quick test_context_handler
        ; Alcotest.test_case "without context" `Quick test_context_handler_no_context
        ] )
    ]
;;
