(** Handoff message helpers — pure functions for handoff detection and message manipulation.

    These functions operate only on Types.message lists and have no dependency
    on Agent.t, making them safe to compile independently. *)

open Types

(** Find the most recent assistant message and extract the first handoff ToolUse.
    Returns (tool_use_id, target_name, prompt) or None. *)
let find_handoff_in_messages messages =
  let last_assistant =
    List.rev messages |> List.find_opt (fun message -> message.role = Assistant)
  in
  match last_assistant with
  | None -> None
  | Some assistant_message ->
      List.fold_left (fun acc block ->
        match acc with
        | Some _ -> acc
        | None ->
            match block with
            | ToolUse { id; name; input } when Handoff.is_handoff_tool name ->
                let target_name = Handoff.target_name_of_tool name in
                let prompt =
                  match input with
                  | `Assoc pairs ->
                      (match List.assoc_opt "prompt" pairs with
                       | Some (`String s) -> s
                       | _ -> "Continue the conversation.")
                  | _ -> "Continue the conversation."
                in
                Some (id, target_name, prompt)
            | _ -> None
      ) None assistant_message.content

(** Replace the tool result emitted for a specific ToolUse id in the most recent
    user tool_result message. This lets handoff interception overwrite the
    sentinel handler output with the delegated agent response. *)
let replace_tool_result messages ~tool_id ~content ~is_error =
  let replace_in_content blocks =
    List.map (function
      | ToolResult { tool_use_id = id; _ } when id = tool_id -> ToolResult { tool_use_id = id; content; is_error }
      | block -> block
    ) blocks
  in
  (* Walk from the end (reversed) to find the most recent matching ToolResult.
     acc accumulates skipped elements in original order. *)
  let rec rewrite acc = function
    | [] ->
        acc @ [{ role = User; content = [ToolResult { tool_use_id = tool_id; content; is_error }] }]
    | ((message : message) :: rest) ->
        let has_tool_result =
          List.exists (function
            | ToolResult { tool_use_id = id; _ } when id = tool_id -> true
            | _ -> false
          ) message.content
        in
        if message.role = User && has_tool_result then
          List.rev_append rest ({ message with content = replace_in_content message.content } :: acc)
        else
          rewrite (message :: acc) rest
  in
  rewrite [] (List.rev messages)
