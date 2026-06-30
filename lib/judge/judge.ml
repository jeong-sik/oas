(** LLM-based evaluation and scoring.

    Provides a single-turn judgment call against a single provider.
    The LLM receives a system prompt and context, and returns
    a structured JSON response parsed into a {!judgment} record.

    @since 0.78.0
    @since 0.142.0 Single-provider API. *)

open Llm_provider

(* ── Types ──────────────────────────────────────────────── *)

type risk_level =
  | Low
  | Medium
  | High
  | Critical
[@@deriving yojson, show]

type judgment =
  { score : float
  ; confidence : float
  ; risk : risk_level
  ; summary : string
  ; evidence : string list
  ; recommended_action : string option
  }
[@@deriving yojson, show]

type judge_config =
  { system_prompt : string
  ; temperature : float
  ; max_tokens : int
  ; output_schema : Yojson.Safe.t option
  }
[@@deriving show]

let default_config () =
  { system_prompt =
      "You are a precise evaluator. Analyze the given context and return a JSON object \
       with: score (0.0-1.0), confidence (0.0-1.0), risk (low/medium/high/critical), \
       summary (string), evidence (string array), recommended_action (string or null)."
  ; temperature = 0.2
  ; max_tokens = 2048
  ; output_schema = None
  }
;;

(* ── Risk derivation ────────────────────────────────────── *)

let risk_of_score score =
  if score < 0.3
  then Low
  else if score < 0.6
  then Medium
  else if score < 0.8
  then High
  else Critical
;;

(* ── JSON parsing helpers ───────────────────────────────── *)

let risk_level_of_string = function
  | "low" -> Ok Low
  | "medium" -> Ok Medium
  | "high" -> Ok High
  | "critical" -> Ok Critical
  | value -> Error (Printf.sprintf "unsupported risk level: %S" value)
;;

let parse_unit_float ~field value =
  match value with
  | `Float v ->
    if Float.is_nan v || v < 0.0 || v > 1.0
    then Error (Printf.sprintf "%s must be a number between 0.0 and 1.0" field)
    else Ok v
  | `Int v ->
    let v = float_of_int v in
    if v < 0.0 || v > 1.0
    then Error (Printf.sprintf "%s must be a number between 0.0 and 1.0" field)
    else Ok v
  | _ -> Error (Printf.sprintf "%s must be a number" field)
;;

let required_member name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (Printf.sprintf "missing required field: %s" name)
;;

let parse_required_string ~field value =
  match value with
  | `String value -> Ok value
  | _ -> Error (Printf.sprintf "%s must be a string" field)
;;

let parse_evidence = function
  | None | Some `Null -> Ok []
  | Some (`List items) ->
    List.fold_right
      (fun item acc ->
         match item, acc with
         | `String value, Ok values -> Ok (value :: values)
         | _, Ok _ -> Error "evidence must contain only strings"
         | _, (Error _ as error) -> error)
      items
      (Ok [])
  | Some _ -> Error "evidence must be an array of strings"
;;

let parse_recommended_action = function
  | None | Some `Null -> Ok None
  | Some (`String value) -> Ok (Some value)
  | Some _ -> Error "recommended_action must be a string or null"
;;

let strip_exact_json_fence text =
  let text = String.trim text in
  let len = String.length text in
  if len >= 6 && String.sub text 0 3 = "```"
  then (
    let closing = len - 3 in
    if String.sub text closing 3 <> "```"
    then Error "JSON fence is not closed at the end of the response"
    else (
      let body = String.sub text 3 (closing - 3) in
      match String.index_opt body '\n' with
      | None -> Ok (String.trim body)
      | Some first_newline ->
        let fence_label = String.sub body 0 first_newline |> String.trim in
        let payload =
          String.sub body (first_newline + 1) (String.length body - first_newline - 1)
          |> String.trim
        in
        if fence_label = "" || String.lowercase_ascii fence_label = "json"
        then Ok payload
        else Error (Printf.sprintf "unsupported JSON fence label: %S" fence_label)))
  else Ok text
;;

let parse_judgment text =
  let ( let* ) = Result.bind in
  try
    let* json_str = strip_exact_json_fence text in
    let json = Yojson.Safe.from_string json_str in
    let* fields =
      match json with
      | `Assoc fields -> Ok fields
      | _ -> Error "judgment response must be a JSON object"
    in
    let* score_json = required_member "score" fields in
    let* score = parse_unit_float ~field:"score" score_json in
    let* confidence =
      let* confidence_json = required_member "confidence" fields in
      parse_unit_float ~field:"confidence" confidence_json
    in
    let* risk =
      let* risk_json = required_member "risk" fields in
      let* value = parse_required_string ~field:"risk" risk_json in
      risk_level_of_string (String.lowercase_ascii value)
    in
    let* summary_json = required_member "summary" fields in
    let* summary = parse_required_string ~field:"summary" summary_json in
    let* evidence = parse_evidence (List.assoc_opt "evidence" fields) in
    let* recommended_action =
      parse_recommended_action (List.assoc_opt "recommended_action" fields)
    in
    Ok { score; confidence; risk; summary; evidence; recommended_action }
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "JSON parse error: %s" msg)
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Printf.sprintf "JSON type error: %s" msg)
;;

(* ── LLM call (single provider) ─────────────────────────── *)

let provider_config_for_judge ~(provider : Provider_config.t) ~(config : judge_config) =
  let response_format =
    match config.output_schema with
    | Some schema -> Types.JsonSchema schema
    | None -> provider.response_format
  in
  let output_schema =
    Provider_config.output_schema_of_response_format
      ?override:config.output_schema
      response_format
  in
  { provider with
    Provider_config.temperature = Some config.temperature
  ; max_tokens = Some config.max_tokens
  ; response_format
  ; output_schema
  }
;;

let judge ~sw ~net ~provider ~config ~context () =
  let messages : Types.message list =
    [ { role = System
      ; content = [ Text config.system_prompt ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = User
      ; content = [ Text context ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let provider_cfg = provider_config_for_judge ~provider ~config in
  match Complete.complete ~sw ~net ~config:provider_cfg ~messages ~tools:[] () with
  | Error err ->
    let msg =
      match err with
      | Http_client.HttpError { code; body } ->
        Printf.sprintf
          "HTTP %d: %s"
          code
          (if String.length body > 200 then String.sub body 0 200 ^ "..." else body)
      | Http_client.AcceptRejected { reason } -> reason
      | Http_client.NetworkError { message; _ } -> message
      | Http_client.TimeoutError { message; _ } -> message
      | Http_client.ProviderTerminal { message; _ } -> message
      | Http_client.ProviderFailure { kind; message } ->
        Http_client.provider_failure_to_string ~kind ~message
    in
    Error (Printf.sprintf "Judge LLM call failed: %s" msg)
  | Ok response ->
    let text = Types.text_of_response response in
    if String.length text = 0
    then Error "Judge LLM returned empty response"
    else (
      match parse_judgment text with
      | Ok j -> Ok j
      | Error parse_err ->
        Error (Printf.sprintf "Judge LLM returned invalid JSON: %s" parse_err))
;;
