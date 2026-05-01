open Base
open Types

let group_into_turns (messages : message list) : message list list =
  let rec aux current_turn acc = function
    | [] ->
      if current_turn = [] then List.rev acc else List.rev (List.rev current_turn :: acc)
    | msg :: rest ->
      if msg.role = User && current_turn <> []
      then (
        let has_tool_result =
          List.exists
            (function
              | ToolResult _ -> true
              | _ -> false)
            msg.content
        in
        if has_tool_result
        then aux (msg :: current_turn) acc rest
        else aux [ msg ] (List.rev current_turn :: acc) rest)
      else aux (msg :: current_turn) acc rest
  in
  aux [] [] messages
;;

let apply_keep_last_n n messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= n
  then messages
  else (
    let kept = List.filteri (fun i _ -> i >= total - n) turns in
    List.concat kept)
;;

let apply_token_budget budget messages =
  let turns = group_into_turns messages in
  let reversed = List.rev turns in
  let rec take_turns acc remaining = function
    | [] -> acc
    | turn :: rest ->
      let turn_tokens =
        List.fold_left
          (fun sum msg -> sum + Context_reducer_estimate.estimate_message_tokens msg)
          0
          turn
      in
      if remaining >= turn_tokens
      then take_turns (turn :: acc) (remaining - turn_tokens) rest
      else acc
  in
  let kept = take_turns [] budget reversed in
  match kept, reversed with
  | [], most_recent :: _ -> most_recent
  | _ -> List.concat kept
;;

let apply_keep_first_and_last ~first_n ~last_n messages =
  let turns = group_into_turns messages in
  let total = List.length turns in
  if total <= first_n + last_n
  then messages
  else (
    let first_turns = List.filteri (fun i _ -> i < first_n) turns in
    let last_turns = List.filteri (fun i _ -> i >= total - last_n) turns in
    List.concat (first_turns @ last_turns))
;;
