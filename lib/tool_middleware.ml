(** Tool call middleware — reusable validation, coercion, and dispatch primitives.

    @since 0.101.0 *)

(* ── Pre-hook action type ─────────────────────────────────── *)

type pre_hook_action =
  | Pass
  | Proceed of Yojson.Safe.t
  | Reject of
      { is_error : bool
      ; message : string
      }

(* ── Validation convenience ───────────────────────────────── *)

let validate_and_coerce ~tool_name ~(schema : Types.tool_schema) args =
  if schema.parameters = []
  then Pass
  else (
    match Tool_input_validation.validate schema args with
    | Tool_input_validation.Valid coerced ->
      if Yojson.Safe.equal coerced args then Pass else Proceed coerced
    | Tool_input_validation.Invalid errors ->
      let message = Tool_input_validation.format_errors_inline ~tool_name ~args errors in
      Reject { is_error = true; message })
;;

(* -- Descriptor shell constraints --------------------------------------- *)

let extract_shell_command_arg = function
  | `Assoc fields ->
    List.find_map
      (fun name ->
         match List.assoc_opt name fields with
         | Some (`String value) -> Some value
         | _ -> None)
      [ "command"; "cmd" ]
  | _ -> None
;;

let has_dangerous_ampersand cmd =
  let len = String.length cmd in
  let rec loop i =
    if i >= len
    then false
    else if cmd.[i] <> '&'
    then loop (i + 1)
    else if i > 0 && cmd.[i - 1] = '>'
    then loop (i + 1)
    else true
  in
  loop 0
;;

let has_chaining cmd =
  Util.string_contains ~needle:"&&" cmd
  || Util.string_contains ~needle:"||" cmd
  || String.exists
       (function
         | ';' | '\n' | '\r' -> true
         | _ -> false)
       cmd
  || has_dangerous_ampersand cmd
;;

let has_redirection cmd =
  String.exists
    (function
      | '<' | '>' -> true
      | _ -> false)
    cmd
;;

let has_command_substitution cmd =
  Util.string_contains ~needle:"$(" cmd
  || Util.string_contains ~needle:"`" cmd
  || Util.string_contains ~needle:"<(" cmd
  || Util.string_contains ~needle:">(" cmd
;;

let validate_shell_constraints ~tool_name ~(descriptor : Tool.descriptor) args =
  match descriptor.shell, extract_shell_command_arg args with
  | None, _ | Some _, None -> Pass
  | Some shell, Some command ->
    let command = String.trim command in
    let reject reason =
      Reject
        { is_error = true
        ; message =
            Printf.sprintf "Tool '%s' shell constraint violation: %s" tool_name reason
        }
    in
    if (shell.single_command_only || not shell.chaining_allowed) && has_chaining command
    then reject "command chaining is not allowed"
    else if (not shell.pipes_allowed) && String.contains command '|'
    then reject "pipes are not allowed"
    else if (not shell.redirection_allowed) && has_redirection command
    then reject "redirection is not allowed"
    else if (not shell.shell_metacharacters_allowed) && has_command_substitution command
    then reject "command substitution is not allowed"
    else Pass
;;

(* ── Schema conversion ────────────────────────────────────── *)

let tool_schema_of_json_result ~name ?(description = "") json_schema =
  match Mcp_schema.json_schema_to_params_result json_schema with
  | Ok parameters -> Ok { Types.name; description; parameters; strict = None }
  | Error detail -> Error detail
;;

let tool_schema_of_json ~name ?(description = "") json_schema : Types.tool_schema =
  match tool_schema_of_json_result ~name ~description json_schema with
  | Ok schema -> schema
  | Error detail -> invalid_arg detail
;;

(* ── Hook factory ─────────────────────────────────────────── *)

let make_validation_hook ~lookup =
  fun ~name ~args ->
  match lookup name with
  | None -> Pass
  | Some schema -> validate_and_coerce ~tool_name:name ~schema args
;;

(* ── Self-Healing Retry Loop ─────────────────────────────── *)

type healing_result =
  { value : Yojson.Safe.t
  ; attempts : int
  ; healed : bool
  }

type healing_failure =
  | Exhausted of
      { attempts : int
      ; limit : int
      ; last_error : string
      }
  | Llm_error of Error.sdk_error

type llm_callback = Types.message list -> (Types.api_response, Error.sdk_error) result

(** Extract the first ToolUse block matching [tool_name]. *)
let extract_tool_args ~tool_name (content : Types.content_block list) =
  List.find_map
    (function
      | Types.ToolUse { name; input; id } when name = tool_name -> Some (id, input)
      | _ -> None)
    content
;;

let heal_tool_call
      ~tool_name
      ~schema
      ~tool_use_id
      ~args
      ~prior_messages
      ~llm
      ?(max_retries = 3)
      ?on_retry
      ()
  =
  let rec loop attempt current_args current_id messages =
    (* Correction_pipeline.run validates internally — Fixed means already valid.
       Only fall through to validate_and_coerce on Still_invalid. *)
    let det_result = Correction_pipeline.run ~schema current_args in
    match det_result with
    | Correction_pipeline.Fixed { corrected; corrections } ->
      Ok
        { value = corrected
        ; attempts = attempt + 1
        ; healed = attempt > 0 || corrections <> []
        }
    | Correction_pipeline.Still_invalid { errors = _; attempted = _ } ->
      (match validate_and_coerce ~tool_name ~schema current_args with
       | Pass -> Ok { value = current_args; attempts = attempt + 1; healed = attempt > 0 }
       | Proceed coerced -> Ok { value = coerced; attempts = attempt + 1; healed = true }
       | Reject { message; _ } ->
         if attempt >= max_retries
         then
           Error
             (Exhausted
                { attempts = attempt + 1; limit = max_retries; last_error = message })
         else (
           (match on_retry with
            | Some cb -> cb ~attempt:(attempt + 1) ~error:message
            | None -> ());
           (* Reuse det_result from above — no duplicate pipeline run *)
           let enriched_message =
             match det_result with
             | Correction_pipeline.Still_invalid { errors; attempted } ->
               Correction_pipeline.build_nondet_feedback
                 ~tool_name
                 ~args:current_args
                 ~still_invalid:errors
                 ~attempted
             | _ -> message
           in
           let error_feedback : Types.message =
             { role = Tool
             ; content =
                 [ ToolResult
                     { tool_use_id = current_id
                     ; content =
                         Printf.sprintf
                           "Validation failed (attempt %d/%d):\n\
                            %s\n\
                            Fix the parameters and call the tool again."
                           (attempt + 1)
                           (max_retries + 1)
                           enriched_message
                     ; is_error = true
                     ; json = None
                     ; content_blocks = None
                     }
                 ]
             ; name = None
             ; tool_call_id = None
             ; metadata = []
             }
           in
           let retry_messages = messages @ [ error_feedback ] in
           match llm retry_messages with
           | Error sdk_err -> Error (Llm_error sdk_err)
           | Ok (response : Types.api_response) ->
             (match extract_tool_args ~tool_name response.content with
              | None ->
                Error
                  (Exhausted
                     { attempts = attempt + 1
                     ; limit = max_retries
                     ; last_error =
                         Printf.sprintf
                           "LLM response contained no '%s' tool call"
                           tool_name
                     })
              | Some (new_id, new_args) ->
                let assistant_msg : Types.message =
                  { role = Assistant
                  ; content = response.content
                  ; name = None
                  ; tool_call_id = None
                  ; metadata = []
                  }
                in
                loop (attempt + 1) new_args new_id (retry_messages @ [ assistant_msg ]))))
  in
  let initial_assistant : Types.message =
    { role = Assistant
    ; content = [ ToolUse { id = tool_use_id; name = tool_name; input = args } ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  loop 0 args tool_use_id (prior_messages @ [ initial_assistant ])
;;
