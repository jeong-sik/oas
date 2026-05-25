(** Tests for Tool_schema_gen — combinator-based schema derivation. *)

open Agent_sdk

(* ── Two-field schema (broadcast-like) ──────────────────── *)

let broadcast_schema =
  Tool_schema_gen.(
    two
      (string_field "message" ~required:true ~desc:"Content" ())
      (string_field "format" ~required:false ~desc:"Output format" ()))
;;

let test_to_params () =
  let params = Tool_schema_gen.to_params broadcast_schema in
  Alcotest.(check int) "2 params" 2 (List.length params);
  let p0 = List.nth params 0 in
  Alcotest.(check string) "name" "message" p0.name;
  Alcotest.(check bool) "required" true p0.required
;;

let format_errors errs = Tool_input_validation.format_errors ~tool_name:"test" errs

let test_parse_valid () =
  let json = `Assoc [ "message", `String "hello"; "format", `String "compact" ] in
  match Tool_schema_gen.parse broadcast_schema json with
  | Ok (msg, fmt) ->
    Alcotest.(check string) "message" "hello" msg;
    Alcotest.(check string) "format" "compact" fmt
  | Error errs -> Alcotest.fail (format_errors errs)
;;

let test_parse_optional_missing () =
  let json = `Assoc [ "message", `String "hello" ] in
  match Tool_schema_gen.parse broadcast_schema json with
  | Ok (msg, fmt) ->
    Alcotest.(check string) "message" "hello" msg;
    Alcotest.(check string) "default format" "" fmt
  | Error errs -> Alcotest.fail (format_errors errs)
;;

let test_parse_required_missing () =
  let json = `Assoc [ "format", `String "compact" ] in
  match Tool_schema_gen.parse broadcast_schema json with
  | Ok _ -> Alcotest.fail "expected error"
  | Error errs ->
    Alcotest.(check bool) "at least one error" true (List.length errs > 0);
    let e = List.hd errs in
    Alcotest.(check string) "path" "message" e.Tool_input_validation.path;
    Alcotest.(check string) "actual" "missing" e.Tool_input_validation.actual
;;

let test_to_json_schema () =
  let schema_json = Tool_schema_gen.to_json_schema broadcast_schema in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "object" (schema_json |> member "type" |> to_string);
  let props = schema_json |> member "properties" in
  Alcotest.(check bool) "has message" true (props |> member "message" <> `Null);
  let req = schema_json |> member "required" |> to_list in
  Alcotest.(check int) "1 required" 1 (List.length req)
;;

(* ── Single-field schema ────────────────────────────────── *)

let int_schema = Tool_schema_gen.(one (int_field "count" ~required:true ~desc:"Count" ()))

let test_int_parse () =
  match Tool_schema_gen.parse int_schema (`Assoc [ "count", `Int 42 ]) with
  | Ok n -> Alcotest.(check int) "count" 42 n
  | Error errs -> Alcotest.fail (format_errors errs)
;;

let test_int_coercion () =
  match Tool_schema_gen.parse int_schema (`Assoc [ "count", `String "7" ]) with
  | Ok n -> Alcotest.(check int) "coerced" 7 n
  | Error errs -> Alcotest.fail (format_errors errs)
;;

(* ── Three-field schema ─────────────────────────────────── *)

let triple =
  Tool_schema_gen.(
    three
      (string_field "name" ~required:true ~desc:"Name" ())
      (int_field "age" ~required:true ~desc:"Age" ())
      (bool_field "active" ~required:false ~desc:"Active" ()))
;;

let test_triple_parse () =
  let json = `Assoc [ "name", `String "Alice"; "age", `Int 30; "active", `Bool true ] in
  match Tool_schema_gen.parse triple json with
  | Ok (name, age, active) ->
    Alcotest.(check string) "name" "Alice" name;
    Alcotest.(check int) "age" 30 age;
    Alcotest.(check bool) "active" true active
  | Error errs -> Alcotest.fail (format_errors errs)
;;

let test_triple_params () =
  Alcotest.(check int) "3 params" 3 (List.length (Tool_schema_gen.to_params triple))
;;

(* ── Four-field schema and coercion/error accumulation ──── *)

let four_fields =
  Tool_schema_gen.(
    four
      (string_field "label" ~required:true ~desc:"Label" ())
      (int_field "count" ~required:true ~desc:"Count" ())
      (float_field "ratio" ~required:true ~desc:"Ratio" ())
      (bool_field "enabled" ~required:true ~desc:"Enabled" ()))
;;

let test_four_field_coercion () =
  let json =
    `Assoc
      [ "label", `Bool true
      ; "count", `Intlit "9"
      ; "ratio", `String "2.5"
      ; "enabled", `String "false"
      ]
  in
  match Tool_schema_gen.parse four_fields json with
  | Ok (label, count, ratio, enabled) ->
    Alcotest.(check string) "label coerced" "true" label;
    Alcotest.(check int) "count coerced" 9 count;
    Alcotest.(check (float 0.0001)) "ratio coerced" 2.5 ratio;
    Alcotest.(check bool) "enabled coerced" false enabled
  | Error errs -> Alcotest.fail (format_errors errs)
;;

let test_four_field_error_accumulation () =
  let json = `Assoc [ "label", `List []; "count", `Float 1.5; "ratio", `Bool false ] in
  match Tool_schema_gen.parse four_fields json with
  | Ok _ -> Alcotest.fail "expected field errors"
  | Error errs ->
    let paths = List.map (fun e -> e.Tool_input_validation.path) errs in
    Alcotest.(check int) "all four errors" 4 (List.length errs);
    Alcotest.(check bool) "label error" true (List.mem "label" paths);
    Alcotest.(check bool) "count error" true (List.mem "count" paths);
    Alcotest.(check bool) "ratio error" true (List.mem "ratio" paths);
    Alcotest.(check bool) "enabled missing" true (List.mem "enabled" paths)
;;

let optional_defaults =
  Tool_schema_gen.(
    two
      (float_field "ratio" ~required:false ~desc:"Ratio" ~default:0.25 ())
      (bool_field "enabled" ~required:false ~desc:"Enabled" ~default:true ()))
;;

let test_optional_custom_defaults () =
  match Tool_schema_gen.parse optional_defaults (`Assoc []) with
  | Ok (ratio, enabled) ->
    Alcotest.(check (float 0.0001)) "ratio default" 0.25 ratio;
    Alcotest.(check bool) "enabled default" true enabled
  | Error errs -> Alcotest.fail (format_errors errs)
;;

(* ── Integration: schema_gen + Typed_tool ───────────────── *)

let test_typed_tool_integration () =
  let params = Tool_schema_gen.to_params broadcast_schema in
  let parse json =
    Tool_schema_gen.parse broadcast_schema json
    |> Result.map_error (fun errs ->
      Tool_input_validation.format_errors ~tool_name:"gen_broadcast" errs)
  in
  let tool =
    Typed_tool.create
      ~name:"gen_broadcast"
      ~description:"Generated schema"
      ~params
      ~parse
      ~handler:(fun (msg, _fmt) ->
        if msg = "" then Error "empty" else Ok ("sent: " ^ msg))
      ~encode:(fun s -> `Assoc [ "result", `String s ])
      ()
  in
  let input = `Assoc [ "message", `String "typed-gen" ] in
  match Typed_tool.execute tool input with
  | Ok { content } ->
    let json = Yojson.Safe.from_string content in
    let r = Yojson.Safe.Util.(json |> member "result" |> to_string) in
    Alcotest.(check string) "result" "sent: typed-gen" r
  | Error e -> Alcotest.fail e.message
;;

(* ── Runner ─────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "Tool_schema_gen"
    [ ( "two_field"
      , [ Alcotest.test_case "to_params" `Quick test_to_params
        ; Alcotest.test_case "parse valid" `Quick test_parse_valid
        ; Alcotest.test_case "optional missing" `Quick test_parse_optional_missing
        ; Alcotest.test_case "required missing" `Quick test_parse_required_missing
        ; Alcotest.test_case "json schema" `Quick test_to_json_schema
        ] )
    ; ( "single_field"
      , [ Alcotest.test_case "int parse" `Quick test_int_parse
        ; Alcotest.test_case "int coercion" `Quick test_int_coercion
        ] )
    ; ( "three_field"
      , [ Alcotest.test_case "parse" `Quick test_triple_parse
        ; Alcotest.test_case "params count" `Quick test_triple_params
        ] )
    ; ( "four_field"
      , [ Alcotest.test_case "coercion" `Quick test_four_field_coercion
        ; Alcotest.test_case
            "error accumulation"
            `Quick
            test_four_field_error_accumulation
        ; Alcotest.test_case "optional defaults" `Quick test_optional_custom_defaults
        ] )
    ; ( "integration"
      , [ Alcotest.test_case "schema_gen + Typed_tool" `Quick test_typed_tool_integration
        ] )
    ]
;;
