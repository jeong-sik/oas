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

type classification =
  { intent : intent
  ; depth : retrieval_depth
  ; confidence : float
  ; rationale : string option
  }
[@@deriving yojson, show]

let intent_to_string = function
  | Conversational -> "conversational"
  | Task_command -> "task_command"
  | Status_check -> "status_check"
  | Knowledge_query -> "knowledge_query"
  | Coordination -> "coordination"
;;

let normalize_label raw =
  raw
  |> String.trim
  |> String.lowercase_ascii
  |> String.map (function
    | '-' | ' ' -> '_'
    | c -> c)
;;

let intent_of_string raw =
  match normalize_label raw with
  | "conversational" | "conversation" -> Ok Conversational
  | "task_command" | "task" | "command" -> Ok Task_command
  | "status_check" | "status" | "progress" -> Ok Status_check
  | "knowledge_query" | "knowledge" | "query" | "question" -> Ok Knowledge_query
  | "coordination" | "coordinate" | "transfer" | "handoff" -> Ok Coordination
  | other ->
    Error
      (Printf.sprintf
         "unknown intent '%s' (expected conversational, task_command, status_check, \
          knowledge_query, coordination)"
         other)
;;

let depth_for_intent = function
  | Conversational | Task_command -> Skip
  | Status_check | Coordination -> Light
  | Knowledge_query -> Full
;;

let parse_model_json json =
  let open Yojson.Safe.Util in
  match json |> member "intent" |> to_string_option with
  | None -> Error "missing required field 'intent'"
  | Some intent_raw ->
    (match intent_of_string intent_raw with
     | Error _ as err -> err
     | Ok intent ->
       (match json |> member "confidence" |> to_float_option with
        | None -> Error "missing required field 'confidence'"
        | Some confidence when confidence < 0.0 || confidence > 1.0 ->
          Error "confidence must be between 0.0 and 1.0"
        | Some confidence ->
          let rationale = json |> member "rationale" |> to_string_option in
          Ok { intent; depth = depth_for_intent intent; confidence; rationale }))
;;

let schema =
  { Structured.name = "classify_context_intent"
  ; description =
      "Classify a user query into exactly one normalized intent for context routing."
  ; params =
      [ { name = "intent"
        ; description =
            "One of: conversational, task_command, status_check, knowledge_query, \
             coordination."
        ; param_type = String
        ; required = true
        }
      ; { name = "confidence"
        ; description = "Confidence score from 0.0 to 1.0."
        ; param_type = Number
        ; required = true
        }
      ; { name = "rationale"
        ; description = "Optional short explanation for the classification."
        ; param_type = String
        ; required = false
        }
      ]
  ; parse = parse_model_json
  }
;;

let prompt_for_query query =
  Printf.sprintf
    "Classify the following user query into exactly one intent category.\n\n\
     Categories:\n\
     - conversational: casual chat, greeting, thanks, or social exchange.\n\
     - task_command: a direct request to do, change, run, create, fix, review, or update \
     something.\n\
     - status_check: asks about current state, progress, remaining work, or what is \
     open/blocked.\n\
     - knowledge_query: asks for explanation, lookup, facts, docs, or analysis.\n\
     - coordination: asks to assign, route, transfer, sync, notify, or coordinate across \
     actors.\n\n\
     Return only the tool input.\n\
     Set confidence to a number between 0.0 and 1.0.\n\n\
     Query:\n\
     %s"
    query
;;

let classify_model ~sw ~net ?base_url ?provider ?clock ~config ?(max_retries = 2) query =
  Structured.extract_with_retry
    ~sw
    ~net
    ?base_url
    ?provider
    ?clock
    ~config
    ~schema
    ~max_retries
    (prompt_for_query query)
;;

let classify_hybrid
      ~sw
      ~net
      ?base_url
      ?provider
      ?clock
      ~config
      ?(max_retries = 2)
      ~fallback
      query
  =
  match classify_model ~sw ~net ?base_url ?provider ?clock ~config ~max_retries query with
  | Ok result -> Ok result.value
  | Error _ -> Ok (fallback query)
;;
