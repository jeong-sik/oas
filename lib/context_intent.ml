(** Reusable query intent classification for OAS consumers. *)

open Types

type intent =
  | Conversational
  | Task_command
  | Status_check
  | Knowledge_query
  | Coordination
[@@deriving yojson, show]

type retrieval_depth =
  | Skip
  | Light
  | Full
[@@deriving yojson, show]

type classification = {
  intent : intent;
  depth : retrieval_depth;
  confidence : float;
  rationale : string option;
}
[@@deriving yojson, show]

let intent_to_string = function
  | Conversational -> "conversational"
  | Task_command -> "task_command"
  | Status_check -> "status_check"
  | Knowledge_query -> "knowledge_query"
  | Coordination -> "coordination"

let normalize_label raw =
  raw
  |> String.trim
  |> String.lowercase_ascii
  |> String.map (function
       | '-' | ' ' -> '_'
       | c -> c)

let intent_of_string raw =
  match normalize_label raw with
  | "conversational" | "conversation" -> Ok Conversational
  | "task_command" | "task" | "command" -> Ok Task_command
  | "status_check" | "status" | "progress" -> Ok Status_check
  | "knowledge_query" | "knowledge" | "query" | "question" -> Ok Knowledge_query
  | "coordination" | "coordinate" | "transfer" -> Ok Coordination
  | other ->
      Error
        (Printf.sprintf
           "unknown intent '%s' (expected conversational, task_command, status_check, knowledge_query, coordination)"
           other)

let depth_for_intent = function
  | Conversational | Task_command -> Skip
  | Status_check | Coordination -> Light
  | Knowledge_query -> Full

let clamp_unit value =
  if value < 0.0 then 0.0 else if value > 1.0 then 1.0 else value

let starts_with_any ~prefixes text =
  List.exists
    (fun prefix ->
      let lp = String.length prefix in
      String.length text >= lp && String.sub text 0 lp = prefix)
    prefixes

let matched_keywords keywords text =
  List.filter
    (fun keyword -> Util.contains_substring_ci ~haystack:text ~needle:keyword)
    keywords

let string_of_signals intent signals =
  match signals with
  | [] -> intent_to_string intent ^ ": default fallback"
  | _ ->
      Printf.sprintf "%s: matched %s" (intent_to_string intent)
        (String.concat ", " signals)

let heuristic_classify query =
  let text = String.trim query in
  let lowered = String.lowercase_ascii text in
  let len = String.length lowered in
  let short = len > 0 && len <= 48 in
  let asks_question =
    Util.string_contains ~needle:"?" lowered
    || starts_with_any ~prefixes:
         [
           "what";
           "why";
           "how";
           "when";
           "where";
           "which";
           "who";
           "can you";
           "could you";
           "would you";
           "tell me";
           "show me";
           "explain";
         ]
         lowered
  in
  let starts_with_action =
    starts_with_any ~prefixes:
      [
        "fix";
        "add";
        "remove";
        "update";
        "refactor";
        "write";
        "create";
        "open";
        "close";
        "merge";
        "review";
        "run";
        "implement";
        "build";
        "test";
        "check";
      ]
      lowered
  in
  let conversational_hits =
    matched_keywords
      [ "hello"; "hi"; "hey"; "thanks"; "thank you"; "good morning"; "good evening" ]
      lowered
  in
  let task_hits =
    matched_keywords
      [
        "fix";
        "add";
        "remove";
        "update";
        "refactor";
        "write";
        "create";
        "open";
        "close";
        "merge";
        "review";
        "run";
        "implement";
        "build";
        "test";
      ]
      lowered
  in
  let status_hits =
    matched_keywords
      [ "status"; "progress"; "remaining"; "pending"; "blocked"; "current"; "state"; "what changed"; "what's open"; "open issue" ]
      lowered
  in
  let knowledge_hits =
    matched_keywords
      [ "explain"; "how"; "why"; "what is"; "docs"; "document"; "search"; "find"; "lookup"; "understand"; "compare"; "analyze" ]
      lowered
  in
  let coordination_hits =
    matched_keywords
      [ "assign"; "route"; "transfer"; "notify"; "group"; "actor"; "monitor"; "coordinate"; "sync"; "reserve"; "parallel" ]
      lowered
  in
  let conversational_score =
    (float_of_int (List.length conversational_hits) *. 0.45)
    +. if short && not asks_question then 0.20 else 0.0
  in
  let task_score =
    (float_of_int (List.length task_hits) *. 0.40)
    +. if starts_with_action then 0.25 else 0.0
    +. if not asks_question && len > 0 then 0.05 else 0.0
  in
  let status_score =
    (float_of_int (List.length status_hits) *. 0.50)
    +. if asks_question then 0.05 else 0.0
  in
  let knowledge_score =
    (float_of_int (List.length knowledge_hits) *. 0.35)
    +. if asks_question then 0.25 else 0.0
    +. if starts_with_any
         ~prefixes:[ "what"; "why"; "how"; "when"; "where"; "which"; "who" ]
         lowered
      then 0.15
      else 0.0
  in
  let coordination_score =
    (float_of_int (List.length coordination_hits) *. 0.50)
    +. if starts_with_any
         ~prefixes:[ "assign"; "route"; "transfer"; "coordinate"; "sync"; "reserve" ]
         lowered
      then 0.20
      else 0.0
  in
  let scored =
    [
      (Coordination, coordination_score, coordination_hits);
      (Status_check, status_score, status_hits);
      (Task_command, task_score, task_hits);
      (Knowledge_query, knowledge_score, knowledge_hits);
      (Conversational, conversational_score, conversational_hits);
    ]
  in
  let total_score =
    List.fold_left (fun acc (_, score, _) -> acc +. score) 0.0 scored
  in
  let intent, score, signals =
    if total_score <= 0.0 then
      if asks_question then (Knowledge_query, 0.55, [ "question_fallback" ])
      else if short then (Conversational, 0.60, [ "short_message_fallback" ])
      else (Task_command, 0.55, [ "imperative_fallback" ])
    else
      List.fold_left
        (fun ((_, best_score, _) as best) candidate ->
          let _, candidate_score, _ = candidate in
          if Float.compare candidate_score best_score > 0 then candidate else best)
        (Conversational, neg_infinity, [])
        scored
  in
  let confidence =
    if total_score <= 0.0 then score
    else clamp_unit (max 0.50 (score /. total_score))
  in
  {
    intent;
    depth = depth_for_intent intent;
    confidence;
    rationale = Some (string_of_signals intent signals);
  }

let parse_model_json json =
  let open Yojson.Safe.Util in
  match json |> member "intent" |> to_string_option with
  | None -> Error "missing required field 'intent'"
  | Some intent_raw -> (
      match intent_of_string intent_raw with
      | Error _ as err -> err
      | Ok intent -> (
          match json |> member "confidence" |> to_float_option with
          | None -> Error "missing required field 'confidence'"
          | Some confidence when confidence < 0.0 || confidence > 1.0 ->
              Error "confidence must be between 0.0 and 1.0"
          | Some confidence ->
              let rationale = json |> member "rationale" |> to_string_option in
              Ok
                {
                  intent;
                  depth = depth_for_intent intent;
                  confidence;
                  rationale;
                }))

let schema =
  {
    Structured.name = "classify_context_intent";
    description =
      "Classify a user query into exactly one normalized intent for context routing.";
    params =
      [
        {
          name = "intent";
          description =
            "One of: conversational, task_command, status_check, knowledge_query, coordination.";
          param_type = String;
          required = true;
        };
        {
          name = "confidence";
          description = "Confidence score from 0.0 to 1.0.";
          param_type = Number;
          required = true;
        };
        {
          name = "rationale";
          description = "Optional short explanation for the classification.";
          param_type = String;
          required = false;
        };
      ];
    parse = parse_model_json;
  }

let prompt_for_query query =
  Printf.sprintf
    "Classify the following user query into exactly one intent category.\n\n\
     Categories:\n\
     - conversational: casual chat, greeting, thanks, or social exchange.\n\
     - task_command: a direct request to do, change, run, create, fix, review, or update something.\n\
     - status_check: asks about current state, progress, remaining work, or what is open/blocked.\n\
     - knowledge_query: asks for explanation, lookup, facts, docs, or analysis.\n\
     - coordination: asks to assign, route, transfer, sync, notify, or coordinate across actors.\n\n\
     Return only the tool input.\n\
     Set confidence to a number between 0.0 and 1.0.\n\n\
     Query:\n%s"
    query

let classify_model ~sw ~net ?base_url ?provider ?clock ~config ?(max_retries = 2) query =
  Structured.extract_with_retry ~sw ~net ?base_url ?provider ?clock ~config
    ~schema ~max_retries (prompt_for_query query)

let classify_hybrid ~sw ~net ?base_url ?provider ?clock ~config
    ?(max_retries = 2) query =
  match
    classify_model ~sw ~net ?base_url ?provider ?clock ~config ~max_retries
      query
  with
  | Ok result -> Ok result.value
  | Error _ -> Ok (heuristic_classify query)
