open Types

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

let of_tool_choice ?(supports_tool_choice = true) = function
  | Some Any ->
    if supports_tool_choice then Require_tool_use
    else Allow_text_or_tool
  | Some (Tool name) ->
    if supports_tool_choice then Require_specific_tool name
    else Allow_text_or_tool
  | Some None_ ->
    if supports_tool_choice then Require_no_tool_use
    else Allow_text_or_tool
  | Some Auto | None -> Allow_text_or_tool

let tool_use_names (response : api_response) =
  List.filter_map
    (function
      | ToolUse { name; _ } -> Some name
      | _ -> None)
    response.content

let validate_response ~(contract : t) (response : api_response) :
    (unit, string) result =
  match contract with
  | Allow_text_or_tool -> Ok ()
  | Require_tool_use ->
    if tool_use_names response <> []
    then Ok ()
    else
      Error
        "required tool contract unsatisfied: tool_choice requested tool use, \
         but the model returned no ToolUse block"
  | Require_specific_tool name ->
    (match tool_use_names response with
     | [] ->
       Error
         (Printf.sprintf
            "required tool contract unsatisfied: tool_choice requested tool \
             '%s', but the model returned no ToolUse block"
            name)
     | tool_names when List.mem name tool_names -> Ok ()
     | tool_names ->
       Error
         (Printf.sprintf
            "required tool contract unsatisfied: tool_choice requested tool \
             '%s', but the model called [%s]"
            name
            (String.concat ", " tool_names)))
  | Require_no_tool_use ->
    (match tool_use_names response with
     | [] -> Ok ()
     | tool_names ->
       Error
         (Printf.sprintf
            "required no-tool contract unsatisfied: tool_choice disabled tools, \
             but the model called [%s]"
            (String.concat ", " tool_names)))

let validator ~(contract : t) (response : api_response) =
  validate_response ~contract response

let accept_on_exhaustion ~(contract : t) =
  match contract with
  | Allow_text_or_tool -> false
  | Require_tool_use -> false
  | Require_specific_tool _ -> false
  | Require_no_tool_use -> false

let resolve_accept ?accept_reason ~(accept : api_response -> bool) =
  match accept_reason with
  | Some validator -> validator
  | None ->
    fun response ->
      if accept response
      then Ok ()
      else Error "response rejected by accept validator"

let%test "of_tool_choice requires tools for Any" =
  match of_tool_choice (Some Any) with
  | Require_tool_use -> true
  | Allow_text_or_tool | Require_specific_tool _ | Require_no_tool_use -> false

let%test "of_tool_choice allows text for None and Auto" =
  of_tool_choice None = Allow_text_or_tool
  && of_tool_choice (Some Auto) = Allow_text_or_tool

let%test "of_tool_choice requires tools for explicit Tool" =
  of_tool_choice (Some (Tool "calculator")) = Require_specific_tool "calculator"

let%test "of_tool_choice requires no tool use for None_" =
  of_tool_choice (Some None_) = Require_no_tool_use

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

let%test "validate_response accepts matching ToolUse for Require_specific_tool" =
  validate_response
    ~contract:(Require_specific_tool "calculator")
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

let%test "validate_response rejects different ToolUse for Require_specific_tool" =
  match
    validate_response
      ~contract:(Require_specific_tool "calculator")
      { id = "r";
        model = "m";
        stop_reason = StopToolUse;
        content =
          [ ToolUse
              { id = "call-1";
                name = "search";
                input = `Assoc [];
              }
          ];
        usage = None;
        telemetry = None;
      }
  with
  | Error msg -> String.contains msg 'c' && String.contains msg 's'
  | Ok () -> false

let%test "validate_response rejects ToolUse for Require_no_tool_use" =
  match
    validate_response
      ~contract:Require_no_tool_use
      { id = "r";
        model = "m";
        stop_reason = StopToolUse;
        content =
          [ ToolUse
              { id = "call-1";
                name = "search";
                input = `Assoc [];
              }
          ];
        usage = None;
        telemetry = None;
      }
  with
  | Error msg -> String.contains msg 'n' && String.contains msg 's'
  | Ok () -> false

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
