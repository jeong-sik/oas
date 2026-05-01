(** Succession — cross-agent context relay for multi-generation execution. *)

open Types

let msg_text (m : Types.message) =
  m.content
  |> List.filter_map (function
    | Types.Text s -> Some s
    | _ -> None)
  |> String.concat "\n"
;;

(* ================================================================ *)
(* Metrics                                                          *)
(* ================================================================ *)

type metrics =
  { total_turns : int
  ; total_tokens_used : int
  ; total_cost_usd : float
  ; tasks_completed : int
  ; errors_encountered : int
  ; elapsed_seconds : float
  }

let empty_metrics =
  { total_turns = 0
  ; total_tokens_used = 0
  ; total_cost_usd = 0.0
  ; tasks_completed = 0
  ; errors_encountered = 0
  ; elapsed_seconds = 0.0
  }
;;

let merge_metrics a b =
  { total_turns = a.total_turns + b.total_turns
  ; total_tokens_used = a.total_tokens_used + b.total_tokens_used
  ; total_cost_usd = a.total_cost_usd +. b.total_cost_usd
  ; tasks_completed = a.tasks_completed + b.tasks_completed
  ; errors_encountered = a.errors_encountered + b.errors_encountered
  ; elapsed_seconds = a.elapsed_seconds +. b.elapsed_seconds
  }
;;

(* ================================================================ *)
(* DNA type                                                         *)
(* ================================================================ *)

type dna =
  { generation : int
  ; trace_id : string
  ; goal : string
  ; progress_summary : string
  ; compressed_context : string
  ; pending_actions : string list
  ; key_decisions : string list
  ; memory_refs : string list
  ; warnings : string list
  ; metrics : metrics
  }

(* ================================================================ *)
(* Helpers                                                          *)
(* ================================================================ *)

let clip = Util.clip

(* ================================================================ *)
(* Progress extraction from messages                                *)
(* ================================================================ *)

let build_progress_summary (msgs : message list) : string =
  let assistant_msgs = List.filter (fun (m : message) -> m.role = Assistant) msgs in
  let recent =
    match List.length assistant_msgs with
    | n when n > 5 ->
      let start = n - 5 in
      List.filteri (fun i _ -> i >= start) assistant_msgs
    | _ -> assistant_msgs
  in
  let parts = List.map (fun (m : message) -> clip (msg_text m) 200) recent in
  String.concat "\n" parts
;;

let extract_pending_actions (msgs : message list) : string list =
  match List.rev msgs with
  | [] -> []
  | (last : message) :: _ when last.role = User -> [ clip (msg_text last) 200 ]
  | _ -> []
;;

let extract_key_decisions (msgs : message list) : string list =
  let decision_markers =
    [ "decided"
    ; "chosen"
    ; "selected"
    ; "using"
    ; "approach:"
    ; "strategy:"
    ; "will use"
    ; "going with"
    ]
  in
  List.filter_map
    (fun (m : message) ->
       if m.role <> Assistant
       then None
       else (
         let text = msg_text m in
         let lower = String.lowercase_ascii text in
         let has_marker =
           List.exists
             (fun marker ->
                let rec find pos =
                  if pos + String.length marker > String.length lower
                  then false
                  else if String.sub lower pos (String.length marker) = marker
                  then true
                  else find (pos + 1)
                in
                find 0)
             decision_markers
         in
         if has_marker then Some (clip text 150) else None))
    msgs
;;

(* ================================================================ *)
(* DNA Extraction                                                   *)
(* ================================================================ *)

let extract_dna
      ~(messages : message list)
      ~(goal : string)
      ~(generation : int)
      ~(trace_id : string)
      ?(metrics = empty_metrics)
      ?(warnings = [])
      ()
  : dna
  =
  let progress_summary = build_progress_summary messages in
  let pending_actions = extract_pending_actions messages in
  let key_decisions = extract_key_decisions messages in
  let compressed_context =
    let recent =
      match List.length messages with
      | n when n > 10 -> List.filteri (fun i _ -> i >= n - 10) messages
      | _ -> messages
    in
    recent
    |> List.map (fun (m : message) ->
      let role_str =
        match m.role with
        | User -> "U"
        | Assistant -> "A"
        | System -> "S"
        | Tool -> "T"
      in
      Printf.sprintf "%s: %s" role_str (clip (msg_text m) 100))
    |> String.concat "\n"
  in
  { generation
  ; trace_id
  ; goal
  ; progress_summary
  ; compressed_context
  ; pending_actions
  ; key_decisions
  ; memory_refs = []
  ; warnings
  ; metrics
  }
;;

(* ================================================================ *)
(* Hydration                                                        *)
(* ================================================================ *)

let build_successor_system_prompt (dna : dna) : string =
  let buf = Buffer.create 2048 in
  Buffer.add_string
    buf
    (Printf.sprintf "You are continuing a task from generation %d.\n\n" dna.generation);
  Buffer.add_string buf (Printf.sprintf "Goal: %s\n\n" dna.goal);
  if dna.progress_summary <> ""
  then
    Buffer.add_string buf (Printf.sprintf "Progress so far:\n%s\n\n" dna.progress_summary);
  if dna.pending_actions <> []
  then (
    Buffer.add_string buf "Pending actions:\n";
    List.iter
      (fun a -> Buffer.add_string buf (Printf.sprintf "- %s\n" a))
      dna.pending_actions;
    Buffer.add_string buf "\n");
  if dna.key_decisions <> []
  then (
    Buffer.add_string buf "Key decisions made:\n";
    List.iter
      (fun d -> Buffer.add_string buf (Printf.sprintf "- %s\n" d))
      dna.key_decisions;
    Buffer.add_string buf "\n");
  if dna.warnings <> []
  then (
    Buffer.add_string buf "Warnings:\n";
    List.iter (fun w -> Buffer.add_string buf (Printf.sprintf "- %s\n" w)) dna.warnings;
    Buffer.add_string buf "\n");
  Buffer.add_string buf "Continue the task from where the previous agent left off.";
  Buffer.contents buf
;;

let hydrate_messages (dna : dna) : message list =
  let context_msg =
    if dna.compressed_context <> ""
    then
      [ text_message
          System
          (Printf.sprintf "Previous conversation context:\n%s" dna.compressed_context)
      ]
    else []
  in
  let goal_msg = [ text_message User (Printf.sprintf "Continue: %s" dna.goal) ] in
  context_msg @ goal_msg
;;

(* ================================================================ *)
(* Cross-model normalization                                        *)
(* ================================================================ *)

let normalize_for_model (msgs : message list) ~(target_model : string) : message list =
  let _ = target_model in
  (* Strip thinking blocks for all models *)
  let strip_thinking (m : message) : message =
    let content =
      List.filter
        (function
          | Thinking _ | RedactedThinking _ -> false
          | _ -> true)
        m.content
    in
    { m with content }
  in
  (* Repair dangling tool calls: ensure every ToolUse has a matching ToolResult *)
  let has_tool_result msgs tool_use_id =
    List.exists
      (fun (m : message) ->
         List.exists
           (function
             | ToolResult { tool_use_id = id; _ } -> String.equal id tool_use_id
             | _ -> false)
           m.content)
      msgs
  in
  let repair_tool_calls msgs =
    let rec process acc = function
      | [] -> List.rev acc
      | (m : message) :: rest ->
        let dangling =
          List.filter_map
            (function
              | ToolUse { id; name; _ } ->
                if has_tool_result rest id then None else Some (id, name)
              | _ -> None)
            m.content
        in
        let repairs =
          List.map
            (fun (id, name) ->
               { role = Tool
               ; content =
                   [ ToolResult
                       { tool_use_id = id
                       ; content =
                           Printf.sprintf "[truncated: %s result unavailable]" name
                       ; is_error = true
                       ; json = None
                       }
                   ]
               ; name = None
               ; tool_call_id = None
               ; metadata = []
               })
            dangling
        in
        process (List.rev_append repairs (m :: acc)) rest
    in
    process [] msgs
  in
  msgs |> List.map strip_thinking |> repair_tool_calls
;;

(* ================================================================ *)
(* Serialization                                                    *)
(* ================================================================ *)

let str_list_to_json = Util.json_of_string_list

let str_list_of_json json =
  let open Yojson.Safe.Util in
  try
    to_list json
    |> List.filter_map (fun v ->
      try Some (to_string v) with
      | Yojson.Safe.Util.Type_error _ -> None)
  with
  | Yojson.Safe.Util.Type_error _ -> []
;;

let metrics_to_json m : Yojson.Safe.t =
  `Assoc
    [ "total_turns", `Int m.total_turns
    ; "total_tokens_used", `Int m.total_tokens_used
    ; "total_cost_usd", `Float m.total_cost_usd
    ; "tasks_completed", `Int m.tasks_completed
    ; "errors_encountered", `Int m.errors_encountered
    ; "elapsed_seconds", `Float m.elapsed_seconds
    ]
;;

let metrics_of_json json =
  let open Yojson.Safe.Util in
  { total_turns = Util.json_member_int "total_turns" json
  ; total_tokens_used = Util.json_member_int "total_tokens_used" json
  ; total_cost_usd =
      json |> member "total_cost_usd" |> to_float_option |> Option.value ~default:0.0
  ; tasks_completed = Util.json_member_int "tasks_completed" json
  ; errors_encountered = Util.json_member_int "errors_encountered" json
  ; elapsed_seconds =
      json |> member "elapsed_seconds" |> to_float_option |> Option.value ~default:0.0
  }
;;

let dna_to_json dna : Yojson.Safe.t =
  `Assoc
    [ "generation", `Int dna.generation
    ; "trace_id", `String dna.trace_id
    ; "goal", `String dna.goal
    ; "progress_summary", `String dna.progress_summary
    ; "compressed_context", `String dna.compressed_context
    ; "pending_actions", str_list_to_json dna.pending_actions
    ; "key_decisions", str_list_to_json dna.key_decisions
    ; "memory_refs", str_list_to_json dna.memory_refs
    ; "warnings", str_list_to_json dna.warnings
    ; "metrics", metrics_to_json dna.metrics
    ]
;;

let dna_of_json json =
  let open Yojson.Safe.Util in
  try
    Ok
      { generation = json |> member "generation" |> to_int
      ; trace_id = json |> member "trace_id" |> to_string
      ; goal = json |> member "goal" |> to_string
      ; progress_summary = json |> member "progress_summary" |> to_string
      ; compressed_context = json |> member "compressed_context" |> to_string
      ; pending_actions = str_list_of_json (json |> member "pending_actions")
      ; key_decisions = str_list_of_json (json |> member "key_decisions")
      ; memory_refs = str_list_of_json (json |> member "memory_refs")
      ; warnings = str_list_of_json (json |> member "warnings")
      ; metrics = metrics_of_json (json |> member "metrics")
      }
  with
  | Eio.Cancel.Cancelled _ as e -> raise e
  | exn -> Error (Printf.sprintf "DNA parse error: %s" (Printexc.to_string exn))
;;
