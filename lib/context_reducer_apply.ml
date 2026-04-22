open Types

let group_into_turns = Context_reducer_turns.group_into_turns
let estimate_message_tokens = Context_reducer_estimate.estimate_message_tokens
let estimate_block_tokens = Context_reducer_estimate.estimate_block_tokens

let apply_prune_tool_outputs ~max_output_len messages =
  List.map
    (fun (msg : message) ->
      let content =
        List.map
          (fun block ->
            match block with
            | ToolResult { tool_use_id; content; is_error; _ }
              when String.length content > max_output_len ->
              let truncated = String.sub content 0 max_output_len in
              let marker =
                Printf.sprintf "\n[truncated: %d chars]" (String.length content)
              in
              ToolResult
                { tool_use_id; content = truncated ^ marker; is_error; json = None }
            | other -> other)
          msg.content
      in
      { msg with content })
    messages

let truncate_json_strings ~max_arg_len (json : Yojson.Safe.t) : Yojson.Safe.t =
  let changed = ref false in
  let rec walk = function
    | `String s when String.length s > max_arg_len ->
      changed := true;
      let prefix =
        if max_arg_len >= 20 then String.sub s 0 20
        else String.sub s 0 (min max_arg_len (String.length s))
      in
      `String (Printf.sprintf "%s...(truncated %d chars)" prefix (String.length s))
    | `Assoc pairs ->
      `Assoc (List.map (fun (k, v) -> (k, walk v)) pairs)
    | `List items ->
      `List (List.map walk items)
    | other -> other
  in
  let result = walk json in
  if !changed then result else json

let apply_prune_tool_args ~max_arg_len ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map
          (fun (msg : message) ->
            if msg.role <> Assistant then msg
            else
              let content =
                List.map
                  (fun block ->
                    match block with
                    | ToolUse { id; name; input } ->
                      let truncated = truncate_json_strings ~max_arg_len input in
                      if truncated == input then block
                      else ToolUse { id; name; input = truncated }
                    | other -> other)
                  msg.content
              in
              { msg with content })
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

let apply_repair_dangling_tool_calls messages =
  let result_ids =
    List.fold_left
      (fun acc (msg : message) ->
        List.fold_left
          (fun acc block ->
            match block with
            | ToolResult { tool_use_id; _ } -> tool_use_id :: acc
            | _ -> acc)
          acc msg.content)
      [] messages
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (msg : message) :: rest ->
      let orphan_ids =
        List.filter_map
          (fun block ->
            match block with
            | ToolUse { id; _ } when not (List.mem id result_ids) -> Some id
            | _ -> None)
          msg.content
      in
      if orphan_ids = [] then
        aux (msg :: acc) rest
      else
        let repairs =
          List.map
            (fun id ->
              {
                role = User;
                content =
                  [
                    ToolResult
                      {
                        tool_use_id = id;
                        content = "Tool call cancelled before completion.";
                        is_error = true;
                        json = None;
                      };
                  ];
                name = None;
                tool_call_id = None; metadata = [];
              })
            orphan_ids
        in
        aux (List.rev_append repairs (msg :: acc)) rest
  in
  aux [] messages

let apply_repair_orphaned_tool_results messages =
  let use_ids =
    List.fold_left
      (fun acc (msg : message) ->
        List.fold_left
          (fun acc block ->
            match block with
            | ToolUse { id; _ } -> id :: acc
            | _ -> acc)
          acc msg.content)
      [] messages
  in
  List.filter_map
    (fun (msg : message) ->
      let content =
        List.filter
          (fun block ->
            match block with
            | ToolResult { tool_use_id; _ } -> List.mem tool_use_id use_ids
            | _ -> true)
          msg.content
      in
      if content = [] then None else Some { msg with content })
    messages

let apply_merge_contiguous messages =
  let rec aux acc = function
    | [] -> List.rev acc
    | msg :: rest ->
      (match acc with
       | prev :: acc_rest
         when prev.role = msg.role
              && not
                   (List.exists
                      (function ToolResult _ -> true | _ -> false)
                      msg.content)
              && not
                   (List.exists
                      (function ToolResult _ -> true | _ -> false)
                      prev.content) ->
         let merged = { prev with content = prev.content @ msg.content } in
         aux (merged :: acc_rest) rest
       | _ -> aux (msg :: acc) rest)
  in
  aux [] messages

let apply_drop_thinking messages =
  let total = List.length messages in
  List.filter_map
    (fun (i, (msg : message)) ->
      if i >= total - 2 then Some msg
      else
        let content =
          List.filter
            (fun block ->
              match block with
              | Thinking _ | RedactedThinking _ -> false
              | _ -> true)
            msg.content
        in
        if content = [] then None else Some { msg with content })
    (List.mapi (fun i msg -> (i, msg)) messages)

let apply_prune_by_role ~drop_roles messages =
  let should_drop (msg : message) =
    List.exists (fun r -> r = msg.role) drop_roles
    && not (List.exists (function ToolResult _ -> true | _ -> false) msg.content)
    && not (List.exists (function ToolUse _ -> true | _ -> false) msg.content)
  in
  List.filter (fun msg -> not (should_drop msg)) messages

let apply_clear_tool_results ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map
          (fun (msg : message) ->
            let content =
              List.map
                (fun block ->
                  match block with
                  | ToolResult { tool_use_id; content; is_error; _ }
                    when String.length content > 50 ->
                    let summary =
                      if is_error then "[tool error result cleared]"
                      else
                        Printf.sprintf
                          "[tool result cleared: %d chars]"
                          (String.length content)
                    in
                    ToolResult { tool_use_id; content = summary; is_error; json = None }
                  | other -> other)
                msg.content
            in
            { msg with content })
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

let apply_stub_tool_results ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let tool_names = Hashtbl.create 32 in
    List.iter
      (fun (msg : message) ->
        List.iter
          (function
            | ToolUse { id; name; _ } -> Hashtbl.replace tool_names id name
            | _ -> ())
          msg.content)
      messages;
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map
          (fun (msg : message) ->
            let content =
              List.map
                (fun block ->
                  match block with
                  | ToolResult { tool_use_id; content; is_error; _ }
                    when String.length content > 50 ->
                    let tool_name =
                      match Hashtbl.find_opt tool_names tool_use_id with
                      | Some n -> n
                      | None -> "unknown"
                    in
                    let line_count =
                      1
                      + String.fold_left
                          (fun acc c -> if c = '\n' then acc + 1 else acc)
                          0 content
                    in
                    let status = if is_error then "error" else "ok" in
                    let stub =
                      Printf.sprintf "[tool: %s, %d lines, %s]" tool_name line_count status
                    in
                    ToolResult { tool_use_id; content = stub; is_error; json = None }
                  | other -> other)
                msg.content
            in
            { msg with content })
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed

let apply_cap_message_tokens ~max_tokens ~keep_recent messages =
  if max_tokens <= 0 then messages
  else
    let turns = group_into_turns messages in
    let total = List.length turns in
    if total <= keep_recent then messages
    else
      let front_budget = max_tokens * 6 / 10 in
      let back_budget = max_tokens * 3 / 10 in
      let is_pair_block = function
        | ToolUse _ | ToolResult _ -> true
        | _ -> false
      in
      let cap_message (msg : message) =
        let msg_tokens = estimate_message_tokens msg in
        if msg_tokens <= max_tokens then msg
        else
          let blocks = Array.of_list msg.content in
          let n_blocks = Array.length blocks in
          if n_blocks <= 1 then msg
          else
            let block_tokens = Array.map estimate_block_tokens blocks in
            let keep = Array.make n_blocks false in
            let mandatory_tokens = ref 0 in
            Array.iteri
              (fun i b ->
                if is_pair_block b then begin
                  keep.(i) <- true;
                  mandatory_tokens := !mandatory_tokens + block_tokens.(i)
                end)
              blocks;
            if !mandatory_tokens >= max_tokens then msg
            else begin
              let budget_remaining = max_tokens - !mandatory_tokens in
              let front_budget' = min front_budget (budget_remaining * 6 / 10) in
              let back_budget' = min back_budget (budget_remaining * 3 / 10) in
              let front_used = ref 0 in
              let i = ref 0 in
              let stop_front = ref false in
              while not !stop_front && !i < n_blocks do
                if keep.(!i) then incr i
                else if !front_used + block_tokens.(!i) <= front_budget' then begin
                  keep.(!i) <- true;
                  front_used := !front_used + block_tokens.(!i);
                  incr i
                end else
                  stop_front := true
              done;
              let back_used = ref 0 in
              let j = ref (n_blocks - 1) in
              let stop_back = ref false in
              while not !stop_back && !j >= 0 do
                if keep.(!j) then decr j
                else if !back_used + block_tokens.(!j) <= back_budget' then begin
                  keep.(!j) <- true;
                  back_used := !back_used + block_tokens.(!j);
                  decr j
                end else
                  stop_back := true
              done;
              let n_dropped = ref 0 in
              let dropped_tokens = ref 0 in
              let first_drop = ref (-1) in
              for idx = 0 to n_blocks - 1 do
                if not keep.(idx) then begin
                  incr n_dropped;
                  dropped_tokens := !dropped_tokens + block_tokens.(idx);
                  if !first_drop = -1 then first_drop := idx
                end
              done;
              if !n_dropped = 0 then msg
              else begin
                let marker =
                  Text
                    (Printf.sprintf
                       "[truncated: %d blocks, ~%d tokens removed]"
                       !n_dropped !dropped_tokens)
                in
                let out = ref [] in
                Array.iteri
                  (fun idx b ->
                    if keep.(idx) then out := b :: !out
                    else if idx = !first_drop then out := marker :: !out)
                  blocks;
                { msg with content = List.rev !out }
              end
            end
      in
      let process_turn i turn =
        if i >= total - keep_recent then turn
        else List.map cap_message turn
      in
      let processed = List.mapi process_turn turns in
      List.concat processed

let apply_summarize_old ~keep_recent ~summarizer messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let old_turns = List.filteri (fun i _ -> i < total - keep_recent) turns in
    let recent_turns = List.filteri (fun i _ -> i >= total - keep_recent) turns in
    let old_messages = List.concat old_turns in
    let summary_text = summarizer old_messages in
    let summary_msg =
      { role = User; content = [Text summary_text]; name = None; tool_call_id = None ; metadata = []}
    in
    summary_msg :: List.concat recent_turns

let apply_relocate_tool_results ~state ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent then messages
  else
    let process_turn i turn =
      if i >= total - keep_recent then turn
      else
        List.map
          (fun (msg : message) ->
            let content, _fresh =
              Content_replacement_state.apply_frozen state msg.content
            in
            { msg with content })
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed
