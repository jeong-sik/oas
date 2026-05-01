open Types

let estimate_char_tokens = Llm_provider.Text_estimate.estimate_char_tokens

let estimate_block_tokens = function
  | Text s -> estimate_char_tokens s
  | Thinking { content; _ } -> estimate_char_tokens content
  | RedactedThinking _ -> 50
  | ToolUse { name; input; _ } ->
    let input_str = Yojson.Safe.to_string input in
    estimate_char_tokens (name ^ input_str)
  | ToolResult { content; json; _ } ->
    let base = estimate_char_tokens content in
    (match json with
     | Some j -> base + estimate_char_tokens (Yojson.Safe.to_string j)
     | None -> base)
  | Image { data; _ } -> min ((String.length data * 3 / 4 / 750) + 1) 1600
  | Document { data; _ } -> min ((String.length data * 3 / 4 / 500) + 1) 3000
  | Audio { data; _ } -> min ((String.length data * 3 / 4 / 320) + 1) 5000
;;

let estimate_message_tokens (msg : message) : int =
  List.fold_left (fun acc block -> acc + estimate_block_tokens block) 0 msg.content
;;

let estimate_next_turn_overhead
      ?(system_prompt = "")
      ?(tools : Yojson.Safe.t list = [])
      ?(output_reserve = 4096)
      ()
  : int
  =
  let system_tokens =
    if String.length system_prompt = 0 then 0 else estimate_char_tokens system_prompt
  in
  let tool_tokens =
    List.fold_left
      (fun acc tool -> acc + estimate_char_tokens (Yojson.Safe.to_string tool))
      0
      tools
  in
  let framing = 100 in
  system_tokens + tool_tokens + output_reserve + framing
;;
