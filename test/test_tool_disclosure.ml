(** Tests for Tool.disclosure_level and schema_to_json_with_disclosure. *)

open Alcotest
open Agent_sdk

let make_tool name =
  Tool.create
    ~name
    ~description:("Description of " ^ name)
    ~parameters:
      [ { Types.name = "query"
        ; description = "A query string"
        ; param_type = Types.String
        ; required = true
        }
      ; { Types.name = "limit"
        ; description = "Max results"
        ; param_type = Types.Integer
        ; required = false
        }
      ]
    (fun _ -> Ok { Types.content = "ok" })
;;

let json_keys = function
  | `Assoc fields -> List.map fst fields
  | _ -> []
;;

let has_key key json = List.mem key (json_keys json)

let test_full_schema_byte_identical_to_legacy () =
  let tool = make_tool "search" in
  let legacy = Tool.schema_to_json tool |> Yojson.Safe.to_string in
  let via_disclosure =
    Tool.schema_to_json_with_disclosure Tool.Full_schema tool |> Yojson.Safe.to_string
  in
  check string "Full_schema = legacy schema_to_json" legacy via_disclosure
;;

let test_minimal_index_omits_input_schema () =
  let tool = make_tool "search" in
  let json = Tool.schema_to_json_with_disclosure Tool.Minimal_index tool in
  check bool "no input_schema key" false (has_key "input_schema" json);
  check bool "has name key" true (has_key "name" json);
  check bool "has description key" true (has_key "description" json)
;;

let test_minimal_index_preserves_name_and_description () =
  let tool = make_tool "search" in
  match Tool.schema_to_json_with_disclosure Tool.Minimal_index tool with
  | `Assoc fields ->
    let name = List.assoc "name" fields in
    let desc = List.assoc "description" fields in
    check string "name field" "\"search\"" (Yojson.Safe.to_string name);
    check
      string
      "description field"
      "\"Description of search\""
      (Yojson.Safe.to_string desc)
  | _ -> fail "expected JSON object"
;;

let test_hybrid_full_names_selects_full () =
  let tool_a = make_tool "a" in
  let tool_b = make_tool "b" in
  let level = Tool.Hybrid { full_names = [ "a" ] } in
  let json_a = Tool.schema_to_json_with_disclosure level tool_a in
  let json_b = Tool.schema_to_json_with_disclosure level tool_b in
  check bool "a has input_schema (full)" true (has_key "input_schema" json_a);
  check bool "b lacks input_schema (minimal)" false (has_key "input_schema" json_b)
;;

let test_hybrid_empty_full_names_acts_as_minimal () =
  let tool = make_tool "search" in
  let hybrid_json =
    Tool.schema_to_json_with_disclosure (Tool.Hybrid { full_names = [] }) tool
    |> Yojson.Safe.to_string
  in
  let minimal_json =
    Tool.schema_to_json_with_disclosure Tool.Minimal_index tool |> Yojson.Safe.to_string
  in
  check string "Hybrid [] = Minimal_index" minimal_json hybrid_json
;;

let test_hybrid_all_full_names_acts_as_full () =
  let tool = make_tool "search" in
  let hybrid_json =
    Tool.schema_to_json_with_disclosure (Tool.Hybrid { full_names = [ "search" ] }) tool
    |> Yojson.Safe.to_string
  in
  let full_json =
    Tool.schema_to_json_with_disclosure Tool.Full_schema tool |> Yojson.Safe.to_string
  in
  check string "Hybrid [tool_name] = Full_schema" full_json hybrid_json
;;

let test_byte_size_monotonic_full_geq_minimal () =
  let tool = make_tool "search" in
  let full_len =
    String.length
      (Yojson.Safe.to_string (Tool.schema_to_json_with_disclosure Tool.Full_schema tool))
  in
  let min_len =
    String.length
      (Yojson.Safe.to_string
         (Tool.schema_to_json_with_disclosure Tool.Minimal_index tool))
  in
  check bool "Full size >= Minimal size" true (full_len >= min_len);
  check bool "Minimal is strictly smaller (params exist)" true (min_len < full_len)
;;

let test_hybrid_size_bounded_by_full_and_minimal () =
  let tool = make_tool "search" in
  let full_len =
    String.length
      (Yojson.Safe.to_string (Tool.schema_to_json_with_disclosure Tool.Full_schema tool))
  in
  let min_len =
    String.length
      (Yojson.Safe.to_string
         (Tool.schema_to_json_with_disclosure Tool.Minimal_index tool))
  in
  let hybrid_in_len =
    String.length
      (Yojson.Safe.to_string
         (Tool.schema_to_json_with_disclosure
            (Tool.Hybrid { full_names = [ "search" ] })
            tool))
  in
  let hybrid_out_len =
    String.length
      (Yojson.Safe.to_string
         (Tool.schema_to_json_with_disclosure
            (Tool.Hybrid { full_names = [ "other" ] })
            tool))
  in
  check int "Hybrid included = Full size" full_len hybrid_in_len;
  check int "Hybrid excluded = Minimal size" min_len hybrid_out_len
;;

let () =
  run
    "Tool.disclosure"
    [ ( "schema_to_json_with_disclosure"
      , [ test_case
            "Full_schema byte-identical to legacy schema_to_json"
            `Quick
            test_full_schema_byte_identical_to_legacy
        ; test_case
            "Minimal_index omits input_schema"
            `Quick
            test_minimal_index_omits_input_schema
        ; test_case
            "Minimal_index preserves name and description"
            `Quick
            test_minimal_index_preserves_name_and_description
        ; test_case
            "Hybrid full_names selects which tools get full"
            `Quick
            test_hybrid_full_names_selects_full
        ; test_case
            "Hybrid full_names=[] degenerates to Minimal_index"
            `Quick
            test_hybrid_empty_full_names_acts_as_minimal
        ; test_case
            "Hybrid full_names covering all tools degenerates to Full_schema"
            `Quick
            test_hybrid_all_full_names_acts_as_full
        ; test_case
            "Byte size: Full > Minimal when params exist"
            `Quick
            test_byte_size_monotonic_full_geq_minimal
        ; test_case
            "Hybrid byte size bounded by Full and Minimal per tool"
            `Quick
            test_hybrid_size_bounded_by_full_and_minimal
        ] )
    ]
;;
