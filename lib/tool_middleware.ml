(** Tool call middleware — reusable validation, coercion, and dispatch primitives.

    @since 0.101.0 *)

(* ── Pre-hook action type ─────────────────────────────────── *)

type pre_hook_action =
  | Pass
  | Proceed of Yojson.Safe.t
  | Reject of { is_error: bool; message: string }

(* ── Validation convenience ───────────────────────────────── *)

let validate_and_coerce ~tool_name ~(schema : Types.tool_schema) args =
  if schema.parameters = [] then Pass
  else
    match Tool_input_validation.validate schema args with
    | Tool_input_validation.Valid coerced ->
      if Yojson.Safe.equal coerced args then Pass
      else Proceed coerced
    | Tool_input_validation.Invalid errors ->
      let message =
        Tool_input_validation.format_errors_inline ~tool_name ~args errors
      in
      Reject { is_error = true; message }

(* ── Schema conversion ────────────────────────────────────── *)

let tool_schema_of_json ~name ?(description = "") json_schema : Types.tool_schema =
  let parameters = Mcp_schema.json_schema_to_params json_schema in
  { name; description; parameters }

(* ── Hook factory ─────────────────────────────────────────── *)

let make_validation_hook ~lookup =
  fun ~name ~args ->
    match lookup name with
    | None -> Pass
    | Some schema -> validate_and_coerce ~tool_name:name ~schema args

(* ── Self-Healing Retry Loop ─────────────────────────────── *)

type healing_result = {
  value: Yojson.Safe.t;
  attempts: int;
  healed: bool;
}

type healing_failure =
  | Exhausted of { attempts: int; limit: int; last_error: string }
  | Llm_error of Error.sdk_error

type llm_callback =
  Types.message list -> (Types.api_response, Error.sdk_error) result

(** Extract the first ToolUse block matching [tool_name]. *)
let extract_tool_args ~tool_name (content : Types.content_block list) =
  List.find_map (function
    | Types.ToolUse { name; input; id } when name = tool_name ->
      Some (id, input)
    | _ -> None)
    content

let heal_tool_call ~tool_name ~schema ~tool_use_id ~args
    ~prior_messages ~llm ?(max_retries = 3) ?on_retry () =
  let rec loop attempt current_args current_id messages =
    (* Phase 2: Run deterministic correction pipeline BEFORE validation.
       This exhausts all det corrections before any NonDet LLM retry. *)
    let effective_args = match attempt with
      | 0 -> (
        match Correction_pipeline.run ~schema current_args with
        | Correction_pipeline.Fixed { corrected; _ } -> corrected
        | Correction_pipeline.Still_invalid _ -> current_args)
      | _ -> current_args  (* retries already have LLM-corrected args *)
    in
    match validate_and_coerce ~tool_name ~schema effective_args with
    | Pass ->
      Ok { value = effective_args; attempts = attempt + 1;
           healed = attempt > 0 || not (Yojson.Safe.equal effective_args current_args) }
    | Proceed coerced ->
      Ok { value = coerced; attempts = attempt + 1;
           healed = attempt > 0 || not (Yojson.Safe.equal coerced current_args) }
    | Reject { message; _ } ->
      if attempt >= max_retries then
        Error (Exhausted {
          attempts = attempt + 1; limit = max_retries;
          last_error = message })
      else begin
        (match on_retry with
         | Some cb -> cb ~attempt:(attempt + 1) ~error:message
         | None -> ());
        (* Enrich error feedback with correction pipeline context *)
        let enriched_message = match Correction_pipeline.run ~schema current_args with
          | Correction_pipeline.Still_invalid { errors; attempted } ->
            Correction_pipeline.build_nondet_feedback
              ~tool_name ~args:current_args ~still_invalid:errors ~attempted
          | _ -> message
        in
        let error_feedback : Types.message =
          { role = User;
            content = [
              ToolResult {
                tool_use_id = current_id;
                content = Printf.sprintf
                  "Validation failed (attempt %d/%d):\n%s\nFix the parameters and call the tool again."
                  (attempt + 1) (max_retries + 1) enriched_message;
                is_error = true;
                json = None;
              }];
            name = None; tool_call_id = None }
        in
        let retry_messages = messages @ [error_feedback] in
        match llm retry_messages with
        | Error sdk_err -> Error (Llm_error sdk_err)
        | Ok (response : Types.api_response) -> begin
          match extract_tool_args ~tool_name response.content with
          | None ->
            Error (Exhausted {
              attempts = attempt + 1; limit = max_retries;
              last_error =
                Printf.sprintf "LLM response contained no '%s' tool call"
                  tool_name })
          | Some (new_id, new_args) ->
            let assistant_msg : Types.message =
              { role = Assistant; content = response.content;
                name = None; tool_call_id = None }
            in
            loop (attempt + 1) new_args new_id
              (retry_messages @ [assistant_msg])
        end
      end
  in
  let initial_assistant : Types.message =
    { role = Assistant;
      content = [
        ToolUse { id = tool_use_id; name = tool_name; input = args }];
      name = None; tool_call_id = None }
  in
  loop 0 args tool_use_id (prior_messages @ [initial_assistant])
