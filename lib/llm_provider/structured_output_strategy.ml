type tool_selection =
  | Forced_named
  | Forced_any
  | Model_choice

type t =
  | Native_json_schema
  | Tool_call of tool_selection
  | Json_mode_with_prompt_schema

type unsupported = No_structured_output_path

let unsupported_to_string = function
  | No_structured_output_path ->
    "the model declares no schema-constrained output, no tools, and no JSON output mode, \
     so there is no wire on which a structured-output request can be expressed"
;;

let tool_selection_to_string = function
  | Forced_named -> "forced named tool"
  | Forced_any -> "forced any tool"
  | Model_choice -> "single tool, model chooses"
;;

let to_string = function
  | Native_json_schema -> "native json_schema"
  | Tool_call selection ->
    Printf.sprintf "tool_call (%s)" (tool_selection_to_string selection)
  | Json_mode_with_prompt_schema -> "json_mode with prompt schema"
;;

(* Named selection is preferred over "any" even though the request carries a
   single tool: naming it is what a provider validates against, and it keeps
   the request honest if a caller later adds tools of their own. *)
let tool_selection_of_capabilities (caps : Capabilities.capabilities) =
  if caps.supports_named_tool_choice
  then Forced_named
  else if caps.supports_required_tool_choice
  then Forced_any
  else Model_choice
;;

let select ~(capabilities : Capabilities.capabilities) =
  if capabilities.supports_structured_output
  then Ok Native_json_schema
  else if capabilities.supports_tools
  then Ok (Tool_call (tool_selection_of_capabilities capabilities))
  else if capabilities.supports_response_format_json
  then Ok Json_mode_with_prompt_schema
  else Error No_structured_output_path
;;

let tool_choice_of_selection = function
  | Forced_named -> None (* the caller supplies the tool name *)
  | Forced_any -> Some Types.Any
  | Model_choice -> None
;;

[@@@coverage off]

let%test "native wins when the model declares schema-constrained output" =
  select
    ~capabilities:
      { Capabilities.default_capabilities with supports_structured_output = true }
  = Ok Native_json_schema
;;

let%test "tools carry the schema when there is no native field" =
  (* Exactly the GLM / Cohere shape: tools yes, named tool_choice no. *)
  select
    ~capabilities:
      { Capabilities.default_capabilities with
        supports_structured_output = false
      ; supports_tools = true
      ; supports_named_tool_choice = false
      ; supports_required_tool_choice = false
      ; supports_response_format_json = true
      }
  = Ok (Tool_call Model_choice)
;;

let%test "a model that can name a tool gets the named selection" =
  select
    ~capabilities:
      { Capabilities.default_capabilities with
        supports_tools = true
      ; supports_named_tool_choice = true
      }
  = Ok (Tool_call Forced_named)
;;

let%test "required-but-not-named falls to any, not to prompt-only" =
  select
    ~capabilities:
      { Capabilities.default_capabilities with
        supports_tools = true
      ; supports_named_tool_choice = false
      ; supports_required_tool_choice = true
      }
  = Ok (Tool_call Forced_any)
;;

let%test "json mode is the last resort, not the second choice" =
  (* A model with both tools and json mode must take the tool path: json mode
     guarantees syntax only, tools guarantee shape. *)
  select
    ~capabilities:
      { Capabilities.default_capabilities with
        supports_tools = true
      ; supports_response_format_json = true
      }
  <> Ok Json_mode_with_prompt_schema
;;

let%test "json mode is chosen when tools are absent" =
  select
    ~capabilities:
      { Capabilities.default_capabilities with
        supports_tools = false
      ; supports_response_format_json = true
      }
  = Ok Json_mode_with_prompt_schema
;;

let%test "a model with no output path errors instead of pretending" =
  select ~capabilities:Capabilities.default_capabilities = Error No_structured_output_path
;;
