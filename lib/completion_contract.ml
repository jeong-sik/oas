open Types

type t =
  | Allow_text_or_tool
  | Require_tool_use

let to_string = function
  | Allow_text_or_tool -> "allow_text_or_tool"
  | Require_tool_use -> "require_tool_use"

let of_tool_choice = function
  | Some (Any | Tool _) -> Require_tool_use
  | Some Auto | Some None_ | None -> Allow_text_or_tool

let validate_response ~(contract : t) (response : api_response) :
    (unit, string) result =
  match contract with
  | Allow_text_or_tool -> Ok ()
  | Require_tool_use ->
    if List.exists (function ToolUse _ -> true | _ -> false) response.content
    then Ok ()
    else
      Error
        "required tool contract unsatisfied: tool_choice requested tool use, \
         but the model returned no ToolUse block"

let validator ~(contract : t) (response : api_response) =
  validate_response ~contract response

let accept_on_exhaustion ~(contract : t) =
  match contract with
  | Allow_text_or_tool -> false
  | Require_tool_use -> false

let%test "of_tool_choice requires tools for Any" =
  match of_tool_choice (Some Any) with
  | Require_tool_use -> true
  | Allow_text_or_tool -> false

let%test "of_tool_choice allows text for None and Auto and explicit none" =
  of_tool_choice None = Allow_text_or_tool
  && of_tool_choice (Some Auto) = Allow_text_or_tool
  && of_tool_choice (Some None_) = Allow_text_or_tool

let%test "of_tool_choice requires tools for explicit Tool" =
  of_tool_choice (Some (Tool "calculator")) = Require_tool_use

let%test "validate_response allows text for Allow_text_or_tool" =
  validate_response
    ~contract:Allow_text_or_tool
    { id = "r";
      model = "m";
      stop_reason = EndTurn;
      content = [ Text "hello" ];
      usage = None;
      telemetry = None;
    }
  = Ok ()

let%test "validate_response accepts ToolUse for Require_tool_use" =
  validate_response
    ~contract:Require_tool_use
    { id = "r";
      model = "m";
      stop_reason = StopToolUse;
      content =
        [ ToolUse
            { id = "call-1";
              name = "calculator";
              input = `Assoc [];
            }
        ];
      usage = None;
      telemetry = None;
    }
  = Ok ()

let%test "validate_response rejects text-only for Require_tool_use" =
  match
    validate_response
      ~contract:Require_tool_use
      { id = "r";
        model = "m";
        stop_reason = EndTurn;
        content = [ Text "hello" ];
        usage = None;
        telemetry = None;
      }
  with
  | Error msg ->
    String.contains msg 't'
    && String.length msg > 10
  | Ok () -> false
