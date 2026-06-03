(** Sub-agent delegation and handoff.
    Inspired by Anthropic SDK's AgentDefinition + Task tool
    and Google ADK's Root/Sub-agent pattern.

    To avoid circular dependency with Agent, handoff defines types and
    a tool constructor. Actual delegation is done by Agent.run_with_handoffs
    which intercepts ToolUse blocks matching "transfer_to_*" names. *)

(** Handoff target definition *)
type handoff_target =
  { name : string
  ; description : string
  ; config : Types.agent_config
  ; tools : Tool.t list
  }

(** Result of a handoff execution *)
type handoff_result =
  { target_name : string
  ; response : Types.api_response
  }

(** Delegate function type -- provided by Agent at runtime.
    Takes switch, target definition, and user prompt.
    Returns the sub-agent's API response or an error. *)
type delegate_fn =
  sw:Eio.Switch.t
  -> handoff_target
  -> string
  -> (Types.api_response, Error.sdk_error) result

(** Prefix used to identify handoff tools in ToolUse blocks *)
let handoff_prefix = "transfer_to_"

(** Extract the handoff target name from a tool name, if it is a handoff tool.

    Returns [Some suffix] when [name] starts with [handoff_prefix]; the suffix
    may be empty, so the bare prefix ["transfer_to_"] yields [Some ""]. Returns
    [None] otherwise.

    Folding the prefix check and the suffix extraction into one function makes
    this total: there is no input for which it raises [Invalid_argument]. The
    earlier split — [is_handoff_tool] (guard) followed by a separate
    [target_name_of_tool] that called [String.sub] assuming the guard held —
    re-scanned the prefix and raised on undersized input if ever called without
    the guard (parse, don't validate). *)
let target_name_of_tool name =
  let prefix_len = String.length handoff_prefix in
  if String.length name >= prefix_len && String.sub name 0 prefix_len = handoff_prefix
  then Some (String.sub name prefix_len (String.length name - prefix_len))
  else None
;;

let%test "target_name_of_tool: extracts suffix after prefix" =
  target_name_of_tool "transfer_to_researcher" = Some "researcher"
;;

let%test "target_name_of_tool: bare prefix yields empty suffix" =
  (* Preserves the prior [is_handoff_tool] [>=] semantics: [name] of exactly
     the prefix length still matches, with an empty target name. *)
  target_name_of_tool "transfer_to_" = Some ""
;;

let%test "target_name_of_tool: non-handoff name is None" =
  target_name_of_tool "get_weather" = None
;;

let%test "target_name_of_tool: undersized input is None, never raises" =
  (* ["x"] is shorter than the 12-char prefix; the previous partial
     implementation raised [Invalid_argument] on this input. *)
  target_name_of_tool "x" = None
;;

let%test "target_name_of_tool: total across the prefix-length boundary" =
  (* The only prior failure mode was [String.sub] with a negative length when
     [String.length name < prefix_len]. Sweep lengths spanning the prefix
     boundary plus near-miss prefixes and assert no input raises. *)
  let raises s =
    try
      ignore (target_name_of_tool s);
      false
    with
    | _ -> true
  in
  let samples =
    List.init 25 (fun n -> String.make n 'a')
    @ [ ""; handoff_prefix; handoff_prefix ^ "y"; "transfer_to"; "transfer_t" ]
  in
  not (List.exists raises samples)
;;

(** Create a handoff tool visible to the LLM.
    The handler is a stub -- actual delegation is intercepted
    by the agent runner before this handler is called. *)
let make_handoff_tool (target : handoff_target) : Tool.t =
  let handler _input : Types.tool_result =
    Error
      { message = "Handoff tools are intercepted by the agent runner"
      ; recoverable = false
      ; error_class = None
      }
  in
  Tool.create
    ~name:(Printf.sprintf "%s%s" handoff_prefix target.name)
    ~description:(Printf.sprintf "Hand off to %s: %s" target.name target.description)
    ~parameters:
      [ { Types.name = "prompt"
        ; description = "Instructions for the sub-agent"
        ; param_type = Types.String
        ; required = true
        }
      ]
    handler
;;
