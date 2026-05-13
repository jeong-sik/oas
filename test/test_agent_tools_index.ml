open Alcotest
open Agent_sdk

let make_tool ?(content = "ok") name =
  Tool.create
    ~name
    ~description:("desc:" ^ name ^ ":" ^ content)
    ~parameters:[]
    (fun _ -> Ok { Types.content })
;;

let tool_description = function
  | None -> None
  | Some (tool : Tool.t) -> Some tool.schema.description
;;

let legacy_find tools name =
  List.find_opt (fun (tool : Tool.t) -> String.equal tool.schema.name name) tools
;;

let check_lookup_matches_legacy tools name =
  let index = Agent_tools.build_index tools in
  check
    (option string)
    ("lookup " ^ name)
    (tool_description (legacy_find tools name))
    (tool_description (Agent_tools.find_in_index index name))
;;

let test_exact_lookup_matches_list_find () =
  let tools = [ make_tool "alpha"; make_tool "beta"; make_tool "gamma" ] in
  List.iter (check_lookup_matches_legacy tools) [ "alpha"; "beta"; "gamma"; "missing" ]
;;

let test_duplicate_exact_name_preserves_first_match () =
  let tools = [ make_tool ~content:"first" "dup"; make_tool ~content:"second" "dup" ] in
  let index = Agent_tools.build_index tools in
  match Agent_tools.find_in_index index "dup" with
  | None -> fail "expected dup"
  | Some tool ->
    check string "first duplicate wins" "desc:dup:first" tool.schema.description
;;

let test_tool_id_normalized_lookup_for_builtin () =
  let tools = [ make_tool "READ_FILE" ] in
  let index = Agent_tools.build_index tools in
  match Agent_tools.find_in_index index "read_file" with
  | None -> fail "expected normalized read_file"
  | Some tool -> check string "normalized lookup" "READ_FILE" tool.schema.name
;;

let () =
  run
    "Agent_tools_index"
    [ ( "lookup"
      , [ test_case
            "exact lookup matches list find"
            `Quick
            test_exact_lookup_matches_list_find
        ; test_case
            "duplicate exact name preserves first match"
            `Quick
            test_duplicate_exact_name_preserves_first_match
        ; test_case
            "tool_id normalized lookup for builtin"
            `Quick
            test_tool_id_normalized_lookup_for_builtin
        ] )
    ]
;;
