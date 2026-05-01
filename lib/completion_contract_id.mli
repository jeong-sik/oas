open Base
type t =
  | Allow_text_or_tool
  | Require_tool_use
  | Require_specific_tool of string
  | Require_no_tool_use

val to_string : t -> string
