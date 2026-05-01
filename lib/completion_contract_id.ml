open Base
type t =
  | Allow_text_or_tool
  | Require_tool_use
  | Require_specific_tool of string
  | Require_no_tool_use

let to_string = function
  | Allow_text_or_tool -> "allow_text_or_tool"
  | Require_tool_use -> "require_tool_use"
  | Require_specific_tool name -> Printf.sprintf "require_specific_tool(%s)" name
  | Require_no_tool_use -> "require_no_tool_use"
;;
