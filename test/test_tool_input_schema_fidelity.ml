(** Regression tests for authoritative tool input schemas.

    A caller-supplied JSON Schema used to be converted to [Types.tool_param
    list] and then rebuilt by {!Types.params_to_input_schema}, which keeps only
    type, description and required. Everything else — [minimum], [maximum],
    [default], [enum], nested properties — was destroyed before the tool
    definition reached the provider. These tests pin the authoritative schema
    to the wire. *)

open Alcotest
open Agent_sdk

(* The production case that surfaced the loss: a byte cap the model must stay
   inside. Named so the expectations below cannot drift from the fixture. *)
let max_bytes_minimum = 1
let max_bytes_maximum = 65536
let max_bytes_default = 65536
let cursor_enum = [ "start"; "end" ]

let rich_input_schema : Yojson.Safe.t =
  `Assoc
    [ "type", `String "object"
    ; ( "properties"
      , `Assoc
          [ ( "max_bytes"
            , `Assoc
                [ "type", `String "integer"
                ; "description", `String "Maximum bytes to read"
                ; "minimum", `Int max_bytes_minimum
                ; "maximum", `Int max_bytes_maximum
                ; "default", `Int max_bytes_default
                ] )
          ; ( "cursor"
            , `Assoc
                [ "type", `String "string"
                ; "description", `String "Where to read from"
                ; "enum", `List (List.map (fun value -> `String value) cursor_enum)
                ] )
          ; ( "window"
            , `Assoc
                [ "type", `String "object"
                ; "description", `String "Nested window bounds"
                ; ( "properties"
                  , `Assoc
                      [ "offset", `Assoc [ "type", `String "integer" ]
                      ; "length", `Assoc [ "type", `String "integer" ]
                      ] )
                ] )
          ] )
    ; "required", `List [ `String "max_bytes" ]
    ]
;;

let noop_handler _input = Ok { Types.content = ""; _meta = None }

let json =
  testable
    (fun formatter value ->
       Format.pp_print_string formatter (Yojson.Safe.to_string value))
    ( = )
;;

let tool_from_rich_schema () : Tool.t =
  match
    Mcp.tool_of_input_schema_result
      ~name:"read_file"
      ~description:"Read a file"
      ~input_schema:rich_input_schema
      noop_handler
  with
  | Ok tool -> tool
  | Error detail -> failf "tool_of_input_schema_result: %s" detail
;;

let member name value = Yojson.Safe.Util.member name value

(* ── Wire fidelity ────────────────────────────────────────── *)

let test_authoritative_schema_reaches_the_wire () =
  let tool = tool_from_rich_schema () in
  let emitted = Tool.schema_to_json tool |> member "input_schema" in
  check json "input_schema emitted verbatim" rich_input_schema emitted;
  let properties = emitted |> member "properties" in
  let max_bytes = properties |> member "max_bytes" in
  check
    int
    "minimum survives"
    max_bytes_minimum
    (max_bytes |> member "minimum" |> Yojson.Safe.Util.to_int);
  check
    int
    "maximum survives"
    max_bytes_maximum
    (max_bytes |> member "maximum" |> Yojson.Safe.Util.to_int);
  check
    int
    "default survives"
    max_bytes_default
    (max_bytes |> member "default" |> Yojson.Safe.Util.to_int);
  check
    (list string)
    "enum survives"
    cursor_enum
    (properties
     |> member "cursor"
     |> member "enum"
     |> Yojson.Safe.Util.to_list
     |> List.map Yojson.Safe.Util.to_string);
  check
    (list string)
    "nested properties survive"
    [ "length"; "offset" ]
    (properties
     |> member "window"
     |> member "properties"
     |> Yojson.Safe.Util.to_assoc
     |> List.map fst
     |> List.sort String.compare)
;;

let test_derived_schema_unchanged_without_input_schema () =
  let parameters =
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
  in
  let tool = Tool.create ~name:"calc" ~description:"Calculate" ~parameters noop_handler in
  (* Pinned against a literal rather than against
     [Types.params_to_input_schema parameters]. In the [None] branch
     [schema_to_json] IS that function, so comparing the two puts the same
     call on both sides of the check and any regression inside it cancels
     out — dropping every parameter description passed that way. *)
  check
    json
    "derived from parameters when no authoritative schema"
    (`Assoc
        [ "type", `String "object"
        ; ( "properties"
          , `Assoc
              [ ( "expr"
                , `Assoc [ "type", `String "string"; "description", `String "Expression" ]
                )
              ; ( "precision"
                , `Assoc
                    [ "type", `String "integer"; "description", `String "Decimal places" ]
                )
              ] )
        ; "required", `List [ `String "expr" ]
        ])
    (Tool.schema_to_json tool |> member "input_schema")
;;

(* ── parameters / input_schema invariant ──────────────────── *)

let check_parameters_derived_from_schema label (schema : Types.tool_schema) =
  check
    (option json)
    (label ^ ": authoritative schema kept")
    (Some rich_input_schema)
    schema.input_schema;
  match schema.input_schema with
  | None -> failf "%s: expected an authoritative schema" label
  | Some source ->
    check
      bool
      (label ^ ": parameters equal json_schema_to_params input_schema")
      true
      (schema.parameters = Mcp.json_schema_to_params source)
;;

let test_tool_parameters_are_derived_from_the_schema () =
  let tool = tool_from_rich_schema () in
  check_parameters_derived_from_schema "Mcp.tool_of_input_schema_result" tool.schema
;;

let test_middleware_schema_keeps_its_source () =
  match
    Tool_middleware.tool_schema_of_json_result
      ~name:"read_file"
      ~description:"Read a file"
      rich_input_schema
  with
  | Error detail -> failf "tool_schema_of_json_result: %s" detail
  | Ok schema ->
    check_parameters_derived_from_schema
      "Tool_middleware.tool_schema_of_json_result"
      schema
;;

let test_invalid_schema_names_the_failing_property () =
  let unsupported_type_schema : Yojson.Safe.t =
    `Assoc
      [ "type", `String "object"
      ; "properties", `Assoc [ "count", `Assoc [ "type", `String "decimal" ] ]
      ]
  in
  match
    Mcp.tool_of_input_schema_result
      ~name:"broken"
      ~description:"Broken"
      ~input_schema:unsupported_type_schema
      noop_handler
  with
  | Ok _ -> fail "expected the unsupported type to be rejected"
  | Error detail ->
    check
      bool
      "names the tool and the offending type"
      true
      (Util.contains_substring_ci ~haystack:detail ~needle:"broken"
       && Util.contains_substring_ci ~haystack:detail ~needle:"decimal")
;;

(* ── tool_schema JSON round-trips ─────────────────────────── *)

let sample_parameters =
  [ { Types.name = "max_bytes"
    ; description = "Maximum bytes to read"
    ; param_type = Types.Integer
    ; required = true
    }
  ]
;;

let check_manual_roundtrip label (schema : Types.tool_schema) =
  match Types.tool_schema_of_json (Types.tool_schema_to_json schema) with
  | Error detail -> failf "%s: tool_schema_of_json: %s" label detail
  | Ok decoded -> check bool (label ^ ": record preserved") true (decoded = schema)
;;

let check_derived_roundtrip label (schema : Types.tool_schema) =
  match Types.tool_schema_of_yojson (Types.tool_schema_to_yojson schema) with
  | Error detail -> failf "%s: tool_schema_of_yojson: %s" label detail
  | Ok decoded -> check bool (label ^ ": record preserved") true (decoded = schema)
;;

let test_tool_schema_json_roundtrip () =
  let without =
    { Types.name = "read_file"
    ; description = "Read a file"
    ; parameters = sample_parameters
    ; strict = None
    ; input_schema = None
    }
  in
  let with_schema = { without with Types.input_schema = Some rich_input_schema } in
  check
    bool
    "input_schema omitted when None"
    false
    (List.mem_assoc
       "input_schema"
       (Yojson.Safe.Util.to_assoc (Types.tool_schema_to_json without)));
  check
    json
    "input_schema emitted verbatim when Some"
    rich_input_schema
    (Types.tool_schema_to_json with_schema |> member "input_schema");
  check_manual_roundtrip "manual without input_schema" without;
  check_manual_roundtrip "manual with input_schema" with_schema;
  check_derived_roundtrip "derived without input_schema" without;
  check_derived_roundtrip "derived with input_schema" with_schema
;;

let test_tool_schema_of_json_rejects_non_object () =
  match Types.tool_schema_of_json (`List []) with
  | Ok _ -> fail "expected a non-object tool_schema to be rejected"
  | Error detail ->
    check string "reports the shape" "tool_schema must be a JSON object" detail
;;

let () =
  run
    "tool_input_schema_fidelity"
    [ ( "wire fidelity"
      , [ test_case
            "authoritative schema reaches the wire"
            `Quick
            test_authoritative_schema_reaches_the_wire
        ; test_case
            "derived schema unchanged without input_schema"
            `Quick
            test_derived_schema_unchanged_without_input_schema
        ] )
    ; ( "invariant"
      , [ test_case
            "tool parameters derived from the schema"
            `Quick
            test_tool_parameters_are_derived_from_the_schema
        ; test_case
            "middleware schema keeps its source"
            `Quick
            test_middleware_schema_keeps_its_source
        ; test_case
            "invalid schema names the failing property"
            `Quick
            test_invalid_schema_names_the_failing_property
        ] )
    ; ( "round-trip"
      , [ test_case "tool_schema json round-trip" `Quick test_tool_schema_json_roundtrip
        ; test_case
            "tool_schema_of_json rejects non-object"
            `Quick
            test_tool_schema_of_json_rejects_non_object
        ] )
    ]
;;
