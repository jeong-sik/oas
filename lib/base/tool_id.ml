type t =
  | Search
  | List_dir
  | Find_file
  | Read_file
  | Find_symbol
  | Get_symbols_overview
  | Find_referencing_symbols
  | Search_for_pattern
  | Notebook_read
  | Read_console_messages
  | Read_network_requests
  | Get_page_text
  | Read_page
  | Tabs_context_mcp
  | Task_list
  | Task_get
  | Task_output
  | Create_text_file
  | Replace_content
  | Rename_symbol
  | Insert_after_symbol
  | Insert_before_symbol
  | Replace_symbol_body
  | Notebook_edit
  | Task_create
  | Task_update
  | Task_stop
  | Team_create
  | Team_delete
  | Ask_user_question
  | Navigate
  | Computer
  | Find
  | Form_input
  | Javascript_tool
  | Tabs_create_mcp
  | Upload_image
  | Execute_shell_command
  | Mcp of
      { server : string
      ; tool : string
      }
  | User of string
[@@deriving show]

(* Variant constructors carry only strings and immutable record fields, so
   structural equality via polymorphic [=] is well-defined and equivalent to
   a hand-rolled match. *)
let equal = ( = )

let to_string = function
  | Search -> "search"
  | List_dir -> "list_dir"
  | Find_file -> "find_file"
  | Read_file -> "read_file"
  | Find_symbol -> "find_symbol"
  | Get_symbols_overview -> "get_symbols_overview"
  | Find_referencing_symbols -> "find_referencing_symbols"
  | Search_for_pattern -> "search_for_pattern"
  | Notebook_read -> "notebook_read"
  | Read_console_messages -> "read_console_messages"
  | Read_network_requests -> "read_network_requests"
  | Get_page_text -> "get_page_text"
  | Read_page -> "read_page"
  | Tabs_context_mcp -> "tabs_context_mcp"
  | Task_list -> "task_list"
  | Task_get -> "task_get"
  | Task_output -> "task_output"
  | Create_text_file -> "create_text_file"
  | Replace_content -> "replace_content"
  | Rename_symbol -> "rename_symbol"
  | Insert_after_symbol -> "insert_after_symbol"
  | Insert_before_symbol -> "insert_before_symbol"
  | Replace_symbol_body -> "replace_symbol_body"
  | Notebook_edit -> "notebook_edit"
  | Task_create -> "task_create"
  | Task_update -> "task_update"
  | Task_stop -> "task_stop"
  | Team_create -> "team_create"
  | Team_delete -> "team_delete"
  | Ask_user_question -> "ask_user_question"
  | Navigate -> "navigate"
  | Computer -> "computer"
  | Find -> "find"
  | Form_input -> "form_input"
  | Javascript_tool -> "javascript_tool"
  | Tabs_create_mcp -> "tabs_create_mcp"
  | Upload_image -> "upload_image"
  | Execute_shell_command -> "execute_shell_command"
  | Mcp { server; tool } -> "mcp__" ^ server ^ "__" ^ tool
  | User name -> name
;;

let all_builtins =
  [ Search
  ; List_dir
  ; Find_file
  ; Read_file
  ; Find_symbol
  ; Get_symbols_overview
  ; Find_referencing_symbols
  ; Search_for_pattern
  ; Notebook_read
  ; Read_console_messages
  ; Read_network_requests
  ; Get_page_text
  ; Read_page
  ; Tabs_context_mcp
  ; Task_list
  ; Task_get
  ; Task_output
  ; Create_text_file
  ; Replace_content
  ; Rename_symbol
  ; Insert_after_symbol
  ; Insert_before_symbol
  ; Replace_symbol_body
  ; Notebook_edit
  ; Task_create
  ; Task_update
  ; Task_stop
  ; Team_create
  ; Team_delete
  ; Ask_user_question
  ; Navigate
  ; Computer
  ; Find
  ; Form_input
  ; Javascript_tool
  ; Tabs_create_mcp
  ; Upload_image
  ; Execute_shell_command
  ]
;;

let mcp_prefix = "mcp__"
let mcp_separator = "__"

(* Parse "mcp__<server>__<tool>". Returns [None] when prefix missing,
   server or tool empty, or no separator between server and tool. *)
let parse_mcp s =
  let plen = String.length mcp_prefix in
  let n = String.length s in
  if n <= plen || not (String.equal (String.sub s 0 plen) mcp_prefix)
  then None
  else (
    let rest = String.sub s plen (n - plen) in
    let rlen = String.length rest in
    let rec find_sep i =
      if i + 1 >= rlen
      then None
      else if Char.equal rest.[i] '_' && Char.equal rest.[i + 1] '_'
      then Some i
      else find_sep (i + 1)
    in
    match find_sep 0 with
    | None -> None
    | Some i ->
      let server = String.sub rest 0 i in
      let tool =
        String.sub
          rest
          (i + String.length mcp_separator)
          (rlen - i - String.length mcp_separator)
      in
      if String.equal server "" || String.equal tool ""
      then None
      else Some (Mcp { server; tool }))
;;

let of_string raw =
  let s = String.lowercase_ascii raw in
  match s with
  | "search" -> Search
  | "list_dir" -> List_dir
  | "find_file" -> Find_file
  | "read_file" -> Read_file
  | "find_symbol" -> Find_symbol
  | "get_symbols_overview" -> Get_symbols_overview
  | "find_referencing_symbols" -> Find_referencing_symbols
  | "search_for_pattern" -> Search_for_pattern
  | "notebook_read" -> Notebook_read
  | "read_console_messages" -> Read_console_messages
  | "read_network_requests" -> Read_network_requests
  | "get_page_text" -> Get_page_text
  | "read_page" -> Read_page
  | "tabs_context_mcp" -> Tabs_context_mcp
  | "task_list" -> Task_list
  | "task_get" -> Task_get
  | "task_output" -> Task_output
  | "create_text_file" -> Create_text_file
  | "replace_content" -> Replace_content
  | "rename_symbol" -> Rename_symbol
  | "insert_after_symbol" -> Insert_after_symbol
  | "insert_before_symbol" -> Insert_before_symbol
  | "replace_symbol_body" -> Replace_symbol_body
  | "notebook_edit" -> Notebook_edit
  | "task_create" -> Task_create
  | "task_update" -> Task_update
  | "task_stop" -> Task_stop
  | "team_create" -> Team_create
  | "team_delete" -> Team_delete
  | "ask_user_question" -> Ask_user_question
  | "navigate" -> Navigate
  | "computer" -> Computer
  | "find" -> Find
  | "form_input" -> Form_input
  | "javascript_tool" -> Javascript_tool
  | "tabs_create_mcp" -> Tabs_create_mcp
  | "upload_image" -> Upload_image
  | "execute_shell_command" -> Execute_shell_command
  | other ->
    (match parse_mcp other with
     | Some m -> m
     | None -> User other)
;;

let%test "all_builtins_round_trip" =
  List.for_all (fun b -> equal (of_string (to_string b)) b) all_builtins
;;

let%test "of_string_lowercases_builtin" = equal (of_string "READ_FILE") Read_file

let%test "of_string_retired_native_tool_names_are_user_tools" =
  List.for_all
    (fun name -> equal (of_string name) (User (String.lowercase_ascii name)))
    [ "Read"; "Glob"; "Grep"; "Write"; "Edit"; "Bash" ]
;;

let%test "of_string_user_lowercases" = equal (of_string "MyTool") (User "mytool")

let%test "of_string_mcp_parses" =
  equal
    (of_string "mcp__agent_llm_a__read_file")
    (Mcp { server = "agent_llm_a"; tool = "read_file" })
;;

let%test "of_string_mcp_lowercases" =
  equal (of_string "MCP__Server__Tool") (Mcp { server = "server"; tool = "tool" })
;;

let%test "of_string_malformed_mcp_falls_back_to_user" =
  equal (of_string "mcp__only_server") (User "mcp__only_server")
;;

let%test "of_string_empty_mcp_server_falls_back" =
  equal (of_string "mcp____tool") (User "mcp____tool")
;;

let%test "to_string_mcp_round_trips" =
  let m = Mcp { server = "abc"; tool = "xyz" } in
  equal (of_string (to_string m)) m
;;

let%test "no_duplicate_in_all_builtins" =
  let strs = List.map to_string all_builtins in
  let unique = List.sort_uniq String.compare strs in
  List.length strs = List.length unique
;;
