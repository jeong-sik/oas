open Types

let _log = Log.create ~module_name:"completion_contract" ()

type t = Completion_contract_id.t =
  | Allow_text_or_tool
  | Require_tool_use
  | Require_specific_tool of string
  | Require_no_tool_use

let to_string = Completion_contract_id.to_string

type resolved =
  { requested : t
  ; effective : t
  ; relaxed : bool
  }

type tool_call =
  { name : string
  ; input : Yojson.Safe.t
  ; tool : Tool.t option
  }

type required_tool_satisfaction = tool_call -> (unit, string) result

type violation_detail = Completion_contract_violation_detail.t =
  { called_tools : string list
  ; satisfying_tools : string list
  ; rejection_reasons : (string * string) list (** [(tool_name, reason)] *)
  }
[@@deriving show]

let violation_detail_to_string (detail : violation_detail) : string =
  let called =
    match detail.called_tools with
    | [] -> ""
    | tools -> Printf.sprintf "model called [%s]" (String.concat ", " tools)
  in
  let rejections =
    match detail.rejection_reasons with
    | [] -> ""
    | reasons ->
      Printf.sprintf
        " (%s)"
        (String.concat
           "; "
           (List.map (fun (name, reason) -> Printf.sprintf "%s: %s" name reason) reasons))
  in
  let suggestion =
    match detail.satisfying_tools with
    | [] ->
      "\nNo currently visible tool can satisfy this contract -- emit a blocker instead."
    | tools ->
      Printf.sprintf
        "\nSatisfying tools for this contract: [%s]"
        (String.concat ", " tools)
  in
  called ^ rejections ^ suggestion
;;

let any_tool_call_satisfies (_call : tool_call) = Ok ()

let effectful_tool_satisfies (call : tool_call) =
  match call.tool with
  | None ->
    Error
      (Printf.sprintf
         "tool '%s' has no registered descriptor for strict required-tool validation"
         call.name)
  | Some tool ->
    (match Tool.permission tool with
     | Some Tool.ReadOnly ->
       Error
         (Printf.sprintf
            "tool '%s' is read-only and cannot satisfy a required-tool contract"
            call.name)
     | Some (Tool.Write | Tool.Destructive) -> Ok ()
     | None ->
       Error
         (Printf.sprintf
            "tool '%s' has no permission metadata for strict required-tool validation"
            call.name))
;;

let requested_of_tool_choice choice =
  match choice with
  | Some Any -> Require_tool_use
  | Some (Tool name) -> Require_specific_tool name
  | Some None_ -> Require_no_tool_use
  | Some Auto | None -> Allow_text_or_tool
;;

let of_tool_choice ?(supports_tool_choice = true) choice =
  let requested = requested_of_tool_choice choice in
  match requested with
  | Allow_text_or_tool -> Allow_text_or_tool
  | contract when supports_tool_choice -> contract
  | contract ->
    Log.debug
      _log
      "tool_choice contract relaxed (provider does not support tool_choice)"
      [ Log.S ("requested", to_string contract)
      ; Log.S ("effective", "allow_text_or_tool")
      ];
    Allow_text_or_tool
;;

let resolve_tool_choice_contract ?(supports_tool_choice = true) choice =
  let requested = requested_of_tool_choice choice in
  let effective = of_tool_choice ~supports_tool_choice choice in
  { requested; effective; relaxed = requested <> effective }
;;

let tool_use_names (response : api_response) =
  List.filter_map
    (function
      | ToolUse { name; _ } -> Some name
      | _ -> None)
    response.content
;;

type tool_lookup_index = (string, Tool.t) Hashtbl.t

let add_first tbl key value = if not (Hashtbl.mem tbl key) then Hashtbl.add tbl key value

let build_tool_lookup_index tools =
  let index = Hashtbl.create (max 16 (List.length tools * 2)) in
  List.iter (fun (tool : Tool.t) -> add_first index tool.schema.name tool) tools;
  index
;;

let tool_lookup index name = Hashtbl.find_opt index name

let tool_use_calls ~(tools : Tool.t list) (response : api_response) =
  (* Build the lookup index lazily: text-only responses (the common case under
     a large tool catalog) skip the O(|tools|) hash construction entirely. *)
  let tool_index = lazy (build_tool_lookup_index tools) in
  List.filter_map
    (function
      | ToolUse { name; input; _ } ->
        Some { name; input; tool = tool_lookup (Lazy.force tool_index) name }
      | _ -> None)
    response.content
;;

let satisfaction_errors ~required_tool_satisfaction calls =
  List.filter_map
    (fun call ->
       match required_tool_satisfaction call with
       | Ok () -> None
       | Error reason -> Some (Printf.sprintf "%s: %s" call.name reason))
    calls
;;

let any_satisfying_call ~required_tool_satisfaction calls =
  List.exists (fun call -> Result.is_ok (required_tool_satisfaction call)) calls
;;

let format_suggestion ?(satisfying_tools = []) () =
  match satisfying_tools with
  | [] -> ""
  | tools ->
    Printf.sprintf "\nSatisfying tools for this contract: [%s]" (String.concat ", " tools)
;;

let unsatisfied_calls_message calls errors =
  match errors with
  | [] ->
    Printf.sprintf
      "required tool contract unsatisfied: model called [%s], but no call satisfied the \
       required-tool predicate"
      (String.concat ", " (List.map (fun call -> call.name) calls))
  | _ ->
    Printf.sprintf
      "required tool contract unsatisfied: model called [%s], but no call satisfied the \
       required-tool predicate (%s)"
      (String.concat ", " (List.map (fun call -> call.name) calls))
      (String.concat "; " errors)
;;

(* A response whose stop_reason signals the model was cut off mid-turn rather
   than cleanly deciding not to call a tool. Surfacing a
   [CompletionContractViolation] in that case is misleading — the caller can
   continue the turn (or raise max_tokens) instead. Observed on Anthropic
   Haiku 4.5 where extended thinking consumes the 8192 output budget before
   a ToolUse block emits: Anthropic returns [pause_turn] which currently
   parses to [Unknown "pause_turn"] since it is not yet a first-class
   [stop_reason] variant. *)
let stop_reason_is_resumable (sr : stop_reason) : bool =
  match sr with
  | MaxTokens -> true
  | Unknown "pause_turn" -> true
  | EndTurn | StopToolUse | StopSequence | Unknown _ -> false
;;

let validate_response
      ?(tools = [])
      ?(required_tool_satisfaction = any_tool_call_satisfies)
      ?(satisfying_tools = [])
      ~(contract : t)
      (response : api_response)
  : (unit, string) result
  =
  let suggestion = format_suggestion ~satisfying_tools () in
  match contract with
  | Allow_text_or_tool -> Ok ()
  | Require_tool_use ->
    let calls = tool_use_calls ~tools response in
    if calls <> [] && any_satisfying_call ~required_tool_satisfaction calls
    then Ok ()
    else if calls <> []
    then
      Error
        (unsatisfied_calls_message
           calls
           (satisfaction_errors ~required_tool_satisfaction calls)
         ^ suggestion)
    else if stop_reason_is_resumable response.stop_reason
    then Ok ()
    else
      Error
        ("required tool contract unsatisfied: tool_choice requested tool use, but the \
          model returned no ToolUse block"
         ^ suggestion)
  | Require_specific_tool name ->
    let calls = tool_use_calls ~tools response in
    (match calls with
     | [] when stop_reason_is_resumable response.stop_reason -> Ok ()
     | [] ->
       Error
         (Printf.sprintf
            "required tool contract unsatisfied: tool_choice requested tool '%s', but \
             the model returned no ToolUse block"
            name
          ^ suggestion)
     | calls ->
       let matching = List.filter (fun call -> String.equal call.name name) calls in
       if matching <> [] && any_satisfying_call ~required_tool_satisfaction matching
       then Ok ()
       else if matching <> []
       then
         Error
           (Printf.sprintf
              "required tool contract unsatisfied: tool_choice requested tool '%s', but \
               matching calls did not satisfy the required-tool predicate (%s)"
              name
              (String.concat
                 "; "
                 (satisfaction_errors ~required_tool_satisfaction matching))
            ^ suggestion)
       else (
         let tool_names = List.map (fun call -> call.name) calls in
         Error
           (Printf.sprintf
              "required tool contract unsatisfied: tool_choice requested tool '%s', but \
               the model called [%s]"
              name
              (String.concat ", " tool_names)
            ^ suggestion)))
  | Require_no_tool_use ->
    (match tool_use_names response with
     | [] -> Ok ()
     | tool_names ->
       Error
         (Printf.sprintf
            "required no-tool contract unsatisfied: tool_choice disabled tools, but the \
             model called [%s]"
            (String.concat ", " tool_names)))
;;

let validator ~(contract : t) (response : api_response) =
  validate_response ~contract response
;;

(** Extract structured violation details from a failed validation.

    Returns [Ok ()] when the contract is satisfied, or [Error violation_detail]
    with the called tools, rejection reasons, and suggested satisfying tools. *)
let violation_detail_of_response
      ?(tools = [])
      ?(required_tool_satisfaction = any_tool_call_satisfies)
      ?(satisfying_tools = [])
      ~(contract : t)
      (response : api_response)
  : (unit, violation_detail) result
  =
  let make_detail ?(rejection_reasons = []) called_tool_names =
    Error { called_tools = called_tool_names; satisfying_tools; rejection_reasons }
  in
  match contract with
  | Allow_text_or_tool -> Ok ()
  | Require_tool_use ->
    let calls = tool_use_calls ~tools response in
    if calls <> [] && any_satisfying_call ~required_tool_satisfaction calls
    then Ok ()
    else if calls <> []
    then (
      let reasons =
        List.filter_map
          (fun call ->
             match required_tool_satisfaction call with
             | Ok () -> None
             | Error reason -> Some (call.name, reason))
          calls
      in
      make_detail ~rejection_reasons:reasons (List.map (fun c -> c.name) calls))
    else if stop_reason_is_resumable response.stop_reason
    then Ok ()
    else make_detail []
  | Require_specific_tool name ->
    let calls = tool_use_calls ~tools response in
    (match calls with
     | [] when stop_reason_is_resumable response.stop_reason -> Ok ()
     | [] -> make_detail []
     | calls ->
       let matching = List.filter (fun call -> String.equal call.name name) calls in
       if matching <> [] && any_satisfying_call ~required_tool_satisfaction matching
       then Ok ()
       else if matching <> []
       then (
         let reasons =
           List.filter_map
             (fun call ->
                match required_tool_satisfaction call with
                | Ok () -> None
                | Error reason -> Some (call.name, reason))
             matching
         in
         make_detail ~rejection_reasons:reasons [ name ])
       else make_detail (List.map (fun c -> c.name) calls))
  | Require_no_tool_use ->
    (match tool_use_names response with
     | [] -> Ok ()
     | tool_names -> make_detail tool_names)
;;

let validate_response_with_detail
      ?(tools = [])
      ?(required_tool_satisfaction = any_tool_call_satisfies)
      ?(satisfying_tools = [])
      ~(contract : t)
      (response : api_response)
  =
  match
    validate_response
      ~tools
      ~required_tool_satisfaction
      ~satisfying_tools
      ~contract
      response
  with
  | Ok () -> Ok ()
  | Error reason ->
    let violation_detail =
      match
        violation_detail_of_response
          ~tools
          ~required_tool_satisfaction
          ~satisfying_tools
          ~contract
          response
      with
      | Ok () -> None
      | Error detail -> Some detail
    in
    Error (reason, violation_detail)
;;

let accept_on_exhaustion ~(contract : t) =
  match contract with
  | Allow_text_or_tool -> true
  | Require_tool_use -> true
  | Require_specific_tool _ -> true
  | Require_no_tool_use -> true
;;

let resolve_accept ?accept_reason ~(accept : api_response -> bool) =
  match accept_reason with
  | Some validator -> validator
  | None ->
    fun response ->
      if accept response then Ok () else Error "response rejected by accept validator"
;;

let%test "of_tool_choice requires tools for Any" =
  match of_tool_choice (Some Any) with
  | Require_tool_use -> true
  | Allow_text_or_tool | Require_specific_tool _ | Require_no_tool_use -> false
;;

let%test "of_tool_choice allows text for None and Auto" =
  of_tool_choice None = Allow_text_or_tool
  && of_tool_choice (Some Auto) = Allow_text_or_tool
;;

let%test "of_tool_choice requires tools for explicit Tool" =
  of_tool_choice (Some (Tool "calculator")) = Require_specific_tool "calculator"
;;

let%test "of_tool_choice requires no tool use for None_" =
  of_tool_choice (Some None_) = Require_no_tool_use
;;

let%test "validate_response allows text for Allow_text_or_tool" =
  validate_response
    ~contract:Allow_text_or_tool
    { id = "r"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text "hello" ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

let%test "validate_response accepts ToolUse for Require_tool_use" =
  validate_response
    ~contract:Require_tool_use
    { id = "r"
    ; model = "m"
    ; stop_reason = StopToolUse
    ; content = [ ToolUse { id = "call-1"; name = "calculator"; input = `Assoc [] } ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

let%test "validate_response accepts matching ToolUse for Require_specific_tool" =
  validate_response
    ~contract:(Require_specific_tool "calculator")
    { id = "r"
    ; model = "m"
    ; stop_reason = StopToolUse
    ; content = [ ToolUse { id = "call-1"; name = "calculator"; input = `Assoc [] } ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

let%test "validate_response preserves first-match tool descriptor semantics" =
  let descriptor perm =
    Tool.
      { kind = None
      ; mutation_class = None
      ; concurrency_class = None
      ; permission = Some perm
      ; evidence_role = None
      ; shell = None
      ; notes = []
      ; examples = []
      }
  in
  let tool permission content =
    Tool.create
      ~descriptor:(descriptor permission)
      ~name:"status"
      ~description:content
      ~parameters:[]
      (fun _ -> Ok { content })
  in
  match
    validate_response
      ~tools:[ tool Tool.ReadOnly "read-only first"; tool Tool.Write "write second" ]
      ~required_tool_satisfaction:effectful_tool_satisfies
      ~contract:Require_tool_use
      { id = "r"
      ; model = "m"
      ; stop_reason = StopToolUse
      ; content = [ ToolUse { id = "call-1"; name = "status"; input = `Assoc [] } ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error msg ->
    (try
       ignore (Str.search_forward (Str.regexp_string "read-only") msg 0);
       true
     with
     | Not_found -> false)
  | Ok () -> false
;;

let%test "validate_response rejects different ToolUse for Require_specific_tool" =
  match
    validate_response
      ~contract:(Require_specific_tool "calculator")
      { id = "r"
      ; model = "m"
      ; stop_reason = StopToolUse
      ; content = [ ToolUse { id = "call-1"; name = "search"; input = `Assoc [] } ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error msg -> String.contains msg 'c' && String.contains msg 's'
  | Ok () -> false
;;

let%test "validate_response rejects ToolUse for Require_no_tool_use" =
  match
    validate_response
      ~contract:Require_no_tool_use
      { id = "r"
      ; model = "m"
      ; stop_reason = StopToolUse
      ; content = [ ToolUse { id = "call-1"; name = "search"; input = `Assoc [] } ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error msg -> String.contains msg 'n' && String.contains msg 's'
  | Ok () -> false
;;

let%test "validate_response rejects text-only for Require_tool_use" =
  match
    validate_response
      ~contract:Require_tool_use
      { id = "r"
      ; model = "m"
      ; stop_reason = EndTurn
      ; content = [ Text "hello" ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error msg -> String.contains msg 't' && String.length msg > 10
  | Ok () -> false
;;

(* --- supports_tool_choice=false tests --- *)

let%test "of_tool_choice relaxes Any when supports_tool_choice=false" =
  of_tool_choice ~supports_tool_choice:false (Some Any) = Allow_text_or_tool
;;

let%test "of_tool_choice relaxes Tool when supports_tool_choice=false" =
  of_tool_choice ~supports_tool_choice:false (Some (Tool "voice")) = Allow_text_or_tool
;;

let%test "of_tool_choice relaxes None_ when supports_tool_choice=false" =
  of_tool_choice ~supports_tool_choice:false (Some None_) = Allow_text_or_tool
;;

let%test "of_tool_choice keeps Auto as Allow_text_or_tool regardless" =
  of_tool_choice ~supports_tool_choice:false (Some Auto) = Allow_text_or_tool
  && of_tool_choice ~supports_tool_choice:true (Some Auto) = Allow_text_or_tool
;;

let%test "of_tool_choice enforces Any when supports_tool_choice=true (default)" =
  of_tool_choice (Some Any) = Require_tool_use
  && of_tool_choice ~supports_tool_choice:true (Some Any) = Require_tool_use
;;

let%test "text-only response passes when contract relaxed for unsupported provider" =
  let contract = of_tool_choice ~supports_tool_choice:false (Some Any) in
  validate_response
    ~contract
    { id = "r"
    ; model = "m"
    ; stop_reason = EndTurn
    ; content = [ Text "hello" ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

(* --- stop_reason resumability tests (extended-thinking / budget cut-off) --- *)

let%test "Require_tool_use accepts no-ToolUse response when stop_reason is MaxTokens" =
  validate_response
    ~contract:Require_tool_use
    { id = "r"
    ; model = "claude-haiku-4-5-20251001"
    ; stop_reason = MaxTokens
    ; content = [ Text "partial thinking output" ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

let%test
    "Require_tool_use accepts no-ToolUse response when stop_reason is Unknown pause_turn"
  =
  validate_response
    ~contract:Require_tool_use
    { id = "r"
    ; model = "claude-haiku-4-5-20251001"
    ; stop_reason = Unknown "pause_turn"
    ; content = [ Text "thinking..." ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

let%test "Require_tool_use still rejects no-ToolUse response when stop_reason is EndTurn" =
  match
    validate_response
      ~contract:Require_tool_use
      { id = "r"
      ; model = "m"
      ; stop_reason = EndTurn
      ; content = [ Text "I decline to call tools" ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error _ -> true
  | Ok () -> false
;;

let%test
    "Require_tool_use still rejects no-ToolUse response when stop_reason is Unknown \
     refusal"
  =
  match
    validate_response
      ~contract:Require_tool_use
      { id = "r"
      ; model = "m"
      ; stop_reason = Unknown "refusal"
      ; content = [ Text "..." ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error _ -> true
  | Ok () -> false
;;

let%test "Require_specific_tool accepts no-ToolUse response when stop_reason is MaxTokens"
  =
  validate_response
    ~contract:(Require_specific_tool "calculator")
    { id = "r"
    ; model = "m"
    ; stop_reason = MaxTokens
    ; content = [ Text "partial" ]
    ; usage = None
    ; telemetry = None
    }
  = Ok ()
;;

let%test
    "Require_specific_tool still rejects wrong-tool response when stop_reason is \
     MaxTokens"
  =
  match
    validate_response
      ~contract:(Require_specific_tool "calculator")
      { id = "r"
      ; model = "m"
      ; stop_reason = MaxTokens
      ; content = [ ToolUse { id = "call-1"; name = "search"; input = `Assoc [] } ]
      ; usage = None
      ; telemetry = None
      }
  with
  | Error _ -> true
  | Ok () -> false
;;

let%test "stop_reason_is_resumable classification" =
  stop_reason_is_resumable MaxTokens
  && stop_reason_is_resumable (Unknown "pause_turn")
  && (not (stop_reason_is_resumable EndTurn))
  && (not (stop_reason_is_resumable StopToolUse))
  && (not (stop_reason_is_resumable StopSequence))
  && (not (stop_reason_is_resumable (Unknown "refusal")))
  && not (stop_reason_is_resumable (Unknown "anything-else"))
;;
