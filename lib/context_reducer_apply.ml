open Types
module Provider_replay = Llm_provider.Provider_replay

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
              | ToolResult
                  { tool_use_id; content; is_error; failure_kind; error_class; _ }
                when String.length content > max_output_len ->
                let truncated = String.sub content 0 max_output_len in
                let marker =
                  Printf.sprintf "\n[truncated: %d chars]" (String.length content)
                in
                ToolResult
                  { tool_use_id
                  ; content = truncated ^ marker
                  ; is_error
                  ; failure_kind
                  ; error_class
                  ; json = None
                  ; content_blocks = None
                  }
              | other -> other)
           msg.content
       in
       { msg with content })
    messages
;;

let truncate_json_strings ~max_arg_len (json : Yojson.Safe.t) : Yojson.Safe.t =
  let changed = ref false in
  let rec walk = function
    | `String s when String.length s > max_arg_len ->
      changed := true;
      let prefix =
        if max_arg_len >= 20
        then String.sub s 0 20
        else String.sub s 0 (min max_arg_len (String.length s))
      in
      `String (Printf.sprintf "%s...(truncated %d chars)" prefix (String.length s))
    | `Assoc pairs -> `Assoc (List.map (fun (k, v) -> k, walk v) pairs)
    | `List items -> `List (List.map walk items)
    | other -> other
  in
  let result = walk json in
  if !changed then result else json
;;

let apply_prune_tool_args ~max_arg_len ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent
  then messages
  else (
    let process_turn i turn =
      if i >= total - keep_recent
      then turn
      else
        List.map
          (fun (msg : message) ->
             if msg.role <> Assistant
             then msg
             else (
               let content =
                 List.map
                   (fun block ->
                      match block with
                      | ToolUse { id; name; input } ->
                        let truncated = truncate_json_strings ~max_arg_len input in
                        if truncated == input
                        then block
                        else ToolUse { id; name; input = truncated }
                      | other -> other)
                   msg.content
               in
               { msg with content }))
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed)
;;

let tool_use_ids (msg : message) =
  List.filter_map
    (function
      | ToolUse { id; _ } -> Some id
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ -> None)
    msg.content
;;

let tool_result_ids (msg : message) =
  List.filter_map
    (function
      | ToolResult { tool_use_id; _ } -> Some tool_use_id
      | Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolUse _
      | Image _
      | Document _
      | Audio _ -> None)
    msg.content
;;

let has_tool_result msg = tool_result_ids msg <> []
let has_tool_use msg = tool_use_ids msg <> []

let has_reasoning_block (msg : message) =
  List.exists
    (function
      | Thinking _ | ReasoningDetails _ | RedactedThinking _ -> true
      | Text _ | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ -> false)
    msg.content
;;

let is_provider_replay_carrier = function
  | RedactedThinking data ->
    (match Provider_replay.decode data with
     | Provider_replay.Replay _ | Provider_replay.Malformed_replay _ -> true
     | Provider_replay.Not_replay -> false)
  | Text _
  | Thinking _
  | ReasoningDetails _
  | ToolUse _
  | ToolResult _
  | Image _
  | Document _
  | Audio _ -> false
;;

type dangling_repair_report = { synthesized_tool_results : int }

let synthetic_tool_result_message id =
  { role = Tool
  ; content =
      [ ToolResult
          { tool_use_id = id
          ; content =
              "OAS context reducer synthesized this error result because the original \
               tool call had no matching ToolResult."
          ; is_error = true
          ; failure_kind = None
          ; error_class = None
          ; json = None
          ; content_blocks = None
          }
      ]
  ; name = None
  ; tool_call_id = None
  ; metadata =
      [ "oas.synthetic_tool_result", `Bool true
      ; "oas.synthetic_reason", `String "dangling_tool_use"
      ; "oas.tool_use_id", `String id
      ]
  }
;;

let split_tool_result_span messages =
  let rec loop span = function
    | msg :: rest when has_tool_result msg -> loop (msg :: span) rest
    | rest -> List.rev span, rest
  in
  loop [] messages
;;

let apply_repair_dangling_tool_calls_with_report messages =
  let synthesized_tool_results = ref 0 in
  let rec aux acc = function
    | [] -> List.rev acc, { synthesized_tool_results = !synthesized_tool_results }
    | (msg : message) :: rest ->
      let use_ids = if msg.role = Assistant then tool_use_ids msg else [] in
      if use_ids = []
      then aux (msg :: acc) rest
      else (
        let result_span, tail = split_tool_result_span rest in
        let result_ids = List.concat_map tool_result_ids result_span in
        let orphan_ids = List.filter (fun id -> not (List.mem id result_ids)) use_ids in
        synthesized_tool_results := !synthesized_tool_results + List.length orphan_ids;
        let repairs = List.map synthetic_tool_result_message orphan_ids in
        let segment = (msg :: result_span) @ repairs in
        aux (List.rev_append segment acc) tail)
  in
  aux [] messages
;;

let apply_repair_dangling_tool_calls messages =
  fst (apply_repair_dangling_tool_calls_with_report messages)
;;

let apply_repair_orphaned_tool_results messages =
  let filter_tool_results allowed seen (msg : message) =
    let seen_ref = ref seen in
    let content =
      List.filter
        (function
          | ToolResult { tool_use_id; _ } ->
            let keep =
              List.mem tool_use_id allowed && not (List.mem tool_use_id !seen_ref)
            in
            if keep then seen_ref := tool_use_id :: !seen_ref;
            keep
          | Text _
          | Thinking _
          | ReasoningDetails _
          | RedactedThinking _
          | ToolUse _
          | Image _
          | Document _
          | Audio _ -> true)
        msg.content
    in
    let msg = if content = [] then None else Some { msg with content } in
    msg, !seen_ref
  in
  let filter_result_span allowed span =
    let filtered, _seen =
      List.fold_left
        (fun (acc, seen) msg ->
           let msg, seen = filter_tool_results allowed seen msg in
           match msg with
           | Some msg -> msg :: acc, seen
           | None -> acc, seen)
        ([], [])
        span
    in
    List.rev filtered
  in
  let rec aux acc = function
    | [] -> List.rev acc
    | (msg : message) :: rest ->
      let use_ids = if msg.role = Assistant then tool_use_ids msg else [] in
      if use_ids = []
      then (
        let msg, _seen = filter_tool_results [] [] msg in
        let acc =
          match msg with
          | Some msg -> msg :: acc
          | None -> acc
        in
        aux acc rest)
      else (
        let span, tail = split_tool_result_span rest in
        let filtered_span = filter_result_span use_ids span in
        aux (List.rev_append filtered_span (msg :: acc)) tail)
  in
  aux [] messages
;;

let apply_merge_contiguous messages =
  let rec aux acc = function
    | [] -> List.rev acc
    | msg :: rest ->
      (match acc with
       | prev :: acc_rest
         when prev.role = msg.role
              && (not
                    (List.exists
                       (fun (block : content_block) ->
                          match block with
                          | ToolResult _ -> true
                          | Text _
                          | Thinking _
                          | ReasoningDetails _
                          | RedactedThinking _
                          | ToolUse _
                          | Image _
                          | Document _
                          | Audio _ -> false)
                       msg.content))
              && not
                   (List.exists
                      (fun (block : content_block) ->
                         match block with
                         | ToolResult _ -> true
                         | Text _
                         | Thinking _
                         | ReasoningDetails _
                         | RedactedThinking _
                         | ToolUse _
                         | Image _
                         | Document _
                         | Audio _ -> false)
                      prev.content) ->
         let merged = { prev with content = prev.content @ msg.content } in
         aux (merged :: acc_rest) rest
       | _ -> aux (msg :: acc) rest)
  in
  aux [] messages
;;

let apply_drop_thinking messages =
  let preserves_tool_reasoning msg =
    msg.role = Assistant && has_tool_use msg && has_reasoning_block msg
  in
  let keep_after_drop_thinking ~preserve_reasoning (block : content_block) =
    match block with
    (* Tool-call reasoning can be required by thinking-capable providers when
       the assistant tool-call message is replayed in later rounds. Plain
       assistant reasoning is context weight and should not survive
       [drop_thinking]. *)
    | Thinking _ | ReasoningDetails _ | RedactedThinking _ -> preserve_reasoning
    | Text _ | ToolUse _ | ToolResult _ | Image _ | Document _ | Audio _ -> true
  in
  let filter_content ~preserve_reasoning content =
    let rec loop acc = function
      | (RedactedThinking data as carrier) :: target :: rest ->
        (match Provider_replay.decode data with
         | Provider_replay.Replay { retention = Provider_replay.Exact_next_block; _ }
         | Provider_replay.Malformed_replay _ -> loop (target :: carrier :: acc) rest
         | Provider_replay.Not_replay ->
           let acc = if preserve_reasoning then carrier :: acc else acc in
           loop acc (target :: rest))
      | [ (RedactedThinking _ as carrier) ] when is_provider_replay_carrier carrier ->
        List.rev (carrier :: acc)
      | block :: rest ->
        let acc =
          if keep_after_drop_thinking ~preserve_reasoning block then block :: acc else acc
        in
        loop acc rest
      | [] -> List.rev acc
    in
    loop [] content
  in
  List.filter_map
    (fun (msg : message) ->
       let preserve_reasoning = preserves_tool_reasoning msg in
       let content = filter_content ~preserve_reasoning msg.content in
       if content = [] then None else Some { msg with content })
    messages
;;

let apply_prune_by_role ~drop_roles messages =
  let should_drop (msg : message) =
    List.exists (fun r -> r = msg.role) drop_roles
    && (not
          (List.exists
             (fun (block : content_block) ->
                match block with
                | ToolResult _ -> true
                | Text _
                | Thinking _
                | ReasoningDetails _
                | RedactedThinking _
                | ToolUse _
                | Image _
                | Document _
                | Audio _ -> false)
             msg.content))
    && not
         (List.exists
            (fun (block : content_block) ->
               match block with
               | ToolUse _ -> true
               | Text _
               | Thinking _
               | ReasoningDetails _
               | RedactedThinking _
               | ToolResult _
               | Image _
               | Document _
               | Audio _ -> false)
            msg.content)
  in
  List.filter (fun msg -> not (should_drop msg)) messages
;;

let apply_clear_tool_results ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent
  then messages
  else (
    let process_turn i turn =
      if i >= total - keep_recent
      then turn
      else
        List.map
          (fun (msg : message) ->
             let content =
               List.map
                 (fun block ->
                    match block with
                    | ToolResult
                        { tool_use_id; content; is_error; failure_kind; error_class; _ }
                      when String.length content > 50 ->
                      let summary =
                        if is_error
                        then "[tool error result cleared]"
                        else
                          Printf.sprintf
                            "[tool result cleared: %d chars]"
                            (String.length content)
                      in
                      ToolResult
                        { tool_use_id
                        ; content = summary
                        ; is_error
                        ; failure_kind
                        ; error_class
                        ; json = None
                        ; content_blocks = None
                        }
                    | other -> other)
                 msg.content
             in
             { msg with content })
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed)
;;

let apply_stub_tool_results ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent
  then messages
  else (
    let tool_names = Hashtbl.create 32 in
    let record_tool_name id name =
      match Hashtbl.find_opt tool_names id with
      | None -> Hashtbl.add tool_names id name
      | Some existing when String.equal existing name -> ()
      | Some _ -> Hashtbl.replace tool_names id "ambiguous_tool_use_id"
    in
    List.iter
      (fun (msg : message) ->
         List.iter
           (function
             | ToolUse { id; name; _ } -> record_tool_name id name
             | _ -> ())
           msg.content)
      messages;
    let process_turn i turn =
      if i >= total - keep_recent
      then turn
      else
        List.map
          (fun (msg : message) ->
             let content =
               List.map
                 (fun block ->
                    match block with
                    | ToolResult
                        { tool_use_id; content; is_error; failure_kind; error_class; _ }
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
                            0
                            content
                      in
                      let status = if is_error then "error" else "ok" in
                      let stub =
                        Printf.sprintf
                          "[tool: %s, %d lines, %s]"
                          tool_name
                          line_count
                          status
                      in
                      ToolResult
                        { tool_use_id
                        ; content = stub
                        ; is_error
                        ; failure_kind
                        ; error_class
                        ; json = None
                        ; content_blocks = None
                        }
                    | other -> other)
                 msg.content
             in
             { msg with content })
          turn
    in
    let processed = List.mapi process_turn turns in
    List.concat processed)
;;

let apply_cap_message_tokens ?cache ~max_tokens ~keep_recent messages =
  if max_tokens <= 0
  then messages
  else (
    let turns = group_into_turns messages in
    let total = List.length turns in
    if total <= keep_recent
    then messages
    else (
      let front_budget = max_tokens * 6 / 10 in
      let back_budget = max_tokens * 3 / 10 in
      let is_pair_block (block : content_block) =
        match block with
        | ToolUse _ | ToolResult _ -> true
        | Text _
        | Thinking _
        | ReasoningDetails _
        | RedactedThinking _
        | Image _
        | Document _
        | Audio _ -> false
      in
      let cap_message (msg : message) =
        let msg_tokens = estimate_message_tokens ?cache msg in
        if msg_tokens <= max_tokens
        then msg
        else (
          let blocks = Array.of_list msg.content in
          let n_blocks = Array.length blocks in
          if n_blocks <= 1
          then msg
          else (
            let block_tokens = Array.map (estimate_block_tokens ?cache) blocks in
            let keep = Array.make n_blocks false in
            let mandatory_tokens = ref 0 in
            let mark_mandatory i =
              if not keep.(i)
              then (
                keep.(i) <- true;
                mandatory_tokens := !mandatory_tokens + block_tokens.(i))
            in
            Array.iteri
              (fun i b ->
                 if is_pair_block b then mark_mandatory i;
                 if is_provider_replay_carrier b
                 then (
                   mark_mandatory i;
                   if i + 1 < n_blocks then mark_mandatory (i + 1)))
              blocks;
            if !mandatory_tokens >= max_tokens
            then msg
            else (
              let budget_remaining = max_tokens - !mandatory_tokens in
              let front_budget' = min front_budget (budget_remaining * 6 / 10) in
              let back_budget' = min back_budget (budget_remaining * 3 / 10) in
              let front_used = ref 0 in
              let i = ref 0 in
              let stop_front = ref false in
              while (not !stop_front) && !i < n_blocks do
                if keep.(!i)
                then incr i
                else if !front_used + block_tokens.(!i) <= front_budget'
                then (
                  keep.(!i) <- true;
                  front_used := !front_used + block_tokens.(!i);
                  incr i)
                else stop_front := true
              done;
              let back_used = ref 0 in
              let j = ref (n_blocks - 1) in
              let stop_back = ref false in
              while (not !stop_back) && !j >= 0 do
                if keep.(!j)
                then decr j
                else if !back_used + block_tokens.(!j) <= back_budget'
                then (
                  keep.(!j) <- true;
                  back_used := !back_used + block_tokens.(!j);
                  decr j)
                else stop_back := true
              done;
              let n_dropped = ref 0 in
              let dropped_tokens = ref 0 in
              let first_drop = ref (-1) in
              for idx = 0 to n_blocks - 1 do
                if not keep.(idx)
                then (
                  incr n_dropped;
                  dropped_tokens := !dropped_tokens + block_tokens.(idx);
                  if !first_drop = -1 then first_drop := idx)
              done;
              if !n_dropped = 0
              then msg
              else (
                let marker =
                  Text
                    (Printf.sprintf
                       "[truncated: %d blocks, ~%d tokens removed]"
                       !n_dropped
                       !dropped_tokens)
                in
                let out = ref [] in
                Array.iteri
                  (fun idx b ->
                     if keep.(idx)
                     then out := b :: !out
                     else if idx = !first_drop
                     then out := marker :: !out)
                  blocks;
                { msg with content = List.rev !out }))))
      in
      let process_turn i turn =
        if i >= total - keep_recent then turn else List.map cap_message turn
      in
      let processed = List.mapi process_turn turns in
      List.concat processed))
;;

let apply_summarize_old ~keep_recent ~summarizer messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent
  then messages
  else (
    let old_turns = List.filteri (fun i _ -> i < total - keep_recent) turns in
    let recent_turns = List.filteri (fun i _ -> i >= total - keep_recent) turns in
    let old_messages = List.concat old_turns in
    let fallback_summary exn =
      let reason = Printexc.to_string exn in
      Printf.sprintf
        "[Summary unavailable: summarizer failed: %s]\n[Preserved %d recent turns]"
        reason
        keep_recent
    in
    let summary_text =
      try summarizer old_messages with
      | Eio.Cancel.Cancelled _ as e -> raise e
      | exn -> fallback_summary exn
    in
    let summary_msg =
      { role = User
      ; content = [ Text summary_text ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    in
    summary_msg :: List.concat recent_turns)
;;

let apply_relocate_tool_results ~state ~keep_recent messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= keep_recent
  then messages
  else (
    let process_turn i turn =
      if i >= total - keep_recent
      then turn
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
    List.concat processed)
;;

let apply_cache_alignment ?cache ~size messages =
  if size <= 0 then invalid_arg "apply_cache_alignment: size must be a positive integer";
  let total_tokens =
    List.fold_left (fun acc msg -> acc + estimate_message_tokens ?cache msg) 0 messages
  in
  let remainder = total_tokens mod size in
  if remainder = 0 || total_tokens = 0
  then messages
  else (
    let padding_needed = size - remainder in
    let padding_block =
      Text (Printf.sprintf "\n<!-- [system_padding: %d] -->\n" padding_needed)
    in
    match messages with
    | [] -> []
    | last_msg :: rest ->
      let new_content = last_msg.content @ [ padding_block ] in
      { last_msg with content = new_content } :: rest)
;;

let%test "apply_cache_alignment rejects non-positive size" =
  try
    ignore (apply_cache_alignment ~size:0 []);
    false
  with
  | Invalid_argument _ -> true
;;

let%test "apply_cache_alignment adds padding when not aligned" =
  let msg =
    { role = Assistant
    ; content = [ Text "hello world" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  match apply_cache_alignment ~size:100 [ msg ] with
  | [ aligned ] -> List.length aligned.content > List.length msg.content
  | _ -> false
;;
