(** Response_harness — deterministic extraction layer for LLM outputs.

    Composes {!Lenient_json} and {!Structured} into a unified pipeline:
    1. Tool-use schema extraction (deterministic, preferred)
    2. Lenient text extraction (recovery-based, fallback)
    3. Telemetry tracking (success/failure/recovery counts)

    Follows the Samchon harness pattern: parse -> validate -> feedback -> retry.
    Schema-based extraction constrains through absence (invalid states are
    unrepresentable) rather than prohibition (prompting "don't do X").

    @since 0.100.4 *)

open Types

(* ── Telemetry ──────────────────────────────────────────────── *)

(** Atomic counters for domain-safe telemetry (OCaml 5 multicore). *)
type telemetry_counters =
  { parse_success : int Atomic.t
  ; parse_failure : int Atomic.t
  ; recovery_applied : int Atomic.t
  ; tool_use_extraction : int Atomic.t
  ; text_extraction : int Atomic.t
  }

let global_telemetry =
  { parse_success = Atomic.make 0
  ; parse_failure = Atomic.make 0
  ; recovery_applied = Atomic.make 0
  ; tool_use_extraction = Atomic.make 0
  ; text_extraction = Atomic.make 0
  }
;;

let bump c = Atomic.incr c

type telemetry_snapshot =
  { parse_success : int
  ; parse_failure : int
  ; recovery_applied : int
  ; tool_use_extraction : int
  ; text_extraction : int
  ; success_rate : float
  }

let snapshot () : telemetry_snapshot =
  let ps = Atomic.get global_telemetry.parse_success in
  let pf = Atomic.get global_telemetry.parse_failure in
  let total = ps + pf in
  let rate = if total = 0 then 1.0 else Float.of_int ps /. Float.of_int total in
  { parse_success = ps
  ; parse_failure = pf
  ; recovery_applied = Atomic.get global_telemetry.recovery_applied
  ; tool_use_extraction = Atomic.get global_telemetry.tool_use_extraction
  ; text_extraction = Atomic.get global_telemetry.text_extraction
  ; success_rate = rate
  }
;;

let reset_telemetry () =
  Atomic.set global_telemetry.parse_success 0;
  Atomic.set global_telemetry.parse_failure 0;
  Atomic.set global_telemetry.recovery_applied 0;
  Atomic.set global_telemetry.tool_use_extraction 0;
  Atomic.set global_telemetry.text_extraction 0
;;

(* ── Metric schema (tool_use-based extraction) ────────────── *)

(** Tool schema for metric extraction via tool_use.
    Use with [Structured.extract] or [Structured.extract_with_retry]
    to force the model to report metrics as structured tool calls
    instead of free-form XML tags. *)
let metric_schema ?(metric_name = "score") () : Metric_contract.metric Structured.schema =
  { name = "report_metric"
  ; description =
      Printf.sprintf
        "Report the evaluation metric. Use this tool to report the %s value."
        metric_name
  ; params =
      [ { name = "name"
        ; param_type = String
        ; description = "Metric name"
        ; required = true
        }
      ; { name = "value"
        ; param_type = Number
        ; description = "Metric value (finite decimal)"
        ; required = true
        }
      ]
  ; parse =
      (fun json ->
        let open Yojson.Safe.Util in
        try
          let name = json |> member "name" |> to_string in
          let value = json |> member "value" |> to_float in
          match classify_float value with
          | FP_normal | FP_subnormal | FP_zero -> Ok { Metric_contract.name; value }
          | FP_infinite | FP_nan ->
            Error (Printf.sprintf "metric value must be finite: %g" value)
        with
        | Type_error (msg, _) -> Error (Printf.sprintf "metric schema type error: %s" msg)
        | Failure msg -> Error (Printf.sprintf "metric schema parse error: %s" msg))
  }
;;

(* ── Lenient score extraction from text ───────────────────── *)

(** Extract the first float from a text string using lenient recovery.
    Handles common LLM output patterns:
    - Plain float: "0.75"
    - Score prefix: "Score: 0.75"
    - With explanation: "0.75 - The response was good"
    - Percentage: "75%" (converted to 0.75)

    Returns the first valid finite float found, or None. *)
let extract_first_float (text : string) : float option =
  let len = String.length text in
  let rec scan i =
    if i >= len
    then None
    else (
      let c = text.[i] in
      if
        (c >= '0' && c <= '9')
        || (c = '.' && i + 1 < len && text.[i + 1] >= '0' && text.[i + 1] <= '9')
      then (
        (* Found start of a potential number *)
        let j = ref i in
        let has_dot = ref (c = '.') in
        while
          !j + 1 < len
          &&
          let next = text.[!j + 1] in
          (next >= '0' && next <= '9')
          || (next = '.'
              && (not !has_dot)
              &&
              (has_dot := true;
               true))
        do
          incr j
        done;
        let num_str = String.sub text i (!j - i + 1) in
        match float_of_string_opt num_str with
        | Some f when Float.is_finite f ->
          (* Check if followed by % — convert to fraction *)
          let value = if !j + 1 < len && text.[!j + 1] = '%' then f /. 100.0 else f in
          Some value
        | _ -> scan (!j + 1))
      else if
        c = '-'
        && i + 1 < len
        && ((text.[i + 1] >= '0' && text.[i + 1] <= '9')
            || (text.[i + 1] = '.'
                && i + 2 < len
                && text.[i + 2] >= '0'
                && text.[i + 2] <= '9'))
      then (
        (* Negative number: parse from the minus sign *)
        let j = ref (i + 1) in
        let has_dot = ref (text.[i + 1] = '.') in
        while
          !j + 1 < len
          &&
          let next = text.[!j + 1] in
          (next >= '0' && next <= '9')
          || (next = '.'
              && (not !has_dot)
              &&
              (has_dot := true;
               true))
        do
          incr j
        done;
        let num_str = String.sub text i (!j - i + 1) in
        match float_of_string_opt num_str with
        | Some f when Float.is_finite f ->
          let value = if !j + 1 < len && text.[!j + 1] = '%' then f /. 100.0 else f in
          Some value
        | _ -> scan (!j + 1))
      else scan (i + 1))
  in
  scan 0
;;

(** Extract a score (0.0-1.0) from model response text.
    More robust than regex: handles percentages, score prefixes,
    inline explanations. Clamps to [0.0, 1.0] range.

    Replaces: [Str.regexp {|[0-9]+\.[0-9]+|}] patterns. *)
let extract_score_from_text (text : string) : float option =
  match extract_first_float text with
  | Some f ->
    let clamped = Float.min 1.0 (Float.max 0.0 f) in
    bump global_telemetry.parse_success;
    bump global_telemetry.text_extraction;
    Some clamped
  | None ->
    bump global_telemetry.parse_failure;
    None
;;

(* ── Metric extraction from text (lenient) ────────────────── *)

(** Parse a metric from text using the existing Metric_contract parser,
    with lenient preprocessing applied first.
    Falls back to Metric_contract.parse on preprocessed text. *)
let parse_metric_from_text ?expected_name (text : string)
  : (Metric_contract.metric, string) result
  =
  (* Try direct parse first *)
  match Metric_contract.parse ?expected_name text with
  | Ok _ as ok ->
    bump global_telemetry.parse_success;
    bump global_telemetry.text_extraction;
    ok
  | Error _ ->
    (* Apply lenient recovery: strip fences, trim *)
    let cleaned = text |> Llm_provider.Lenient_json.strip_markdown_fence |> String.trim in
    (match Metric_contract.parse ?expected_name cleaned with
     | Ok _ as ok ->
       bump global_telemetry.parse_success;
       bump global_telemetry.recovery_applied;
       bump global_telemetry.text_extraction;
       ok
     | Error _ as err ->
       bump global_telemetry.parse_failure;
       err)
;;

(* ── Tool-use metric extraction from response ─────────────── *)

(** Extract a metric from an API response's content blocks via tool_use.
    Looks for a [report_metric] tool call and parses it with {!metric_schema}.
    This is the preferred extraction path — fully deterministic. *)
let extract_metric_from_response ?(metric_name = "score") (content : content_block list)
  : (Metric_contract.metric, string) result
  =
  let schema = metric_schema ~metric_name () in
  match Structured.extract_tool_input ~schema content with
  | Ok metric ->
    bump global_telemetry.parse_success;
    bump global_telemetry.tool_use_extraction;
    Ok metric
  | Error e ->
    bump global_telemetry.parse_failure;
    Error (Error.to_string e)
;;

(* ── Inline tests ──────────────────────────────────────────── *)

[@@@coverage off]

let%test "extract_first_float: plain float" = extract_first_float "0.75" = Some 0.75

let%test "extract_first_float: with prefix" =
  extract_first_float "Score: 0.85" = Some 0.85
;;

let%test "extract_first_float: percentage" = extract_first_float "75%" = Some 0.75
let%test "extract_first_float: integer" = extract_first_float "1" = Some 1.0

let%test "extract_first_float: embedded in text" =
  extract_first_float "The result is 0.42 which is good" = Some 0.42
;;

let%test "extract_first_float: no number" = extract_first_float "no numbers here" = None

let%test "extract_score_from_text: clamps to range" =
  reset_telemetry ();
  extract_score_from_text "2.5" = Some 1.0
;;

let%test "extract_score_from_text: tracks telemetry" =
  reset_telemetry ();
  ignore (extract_score_from_text "0.5");
  let t = snapshot () in
  t.parse_success = 1 && t.text_extraction = 1
;;

let%test "metric_schema parses valid input" =
  let schema = metric_schema () in
  let json = `Assoc [ "name", `String "score"; "value", `Float 0.8 ] in
  match schema.parse json with
  | Ok m -> m.name = "score" && m.value = 0.8
  | Error _ -> false
;;

let%test "metric_schema rejects nan" =
  let schema = metric_schema () in
  let json = `Assoc [ "name", `String "score"; "value", `Float Float.nan ] in
  match schema.parse json with
  | Error _ -> true
  | Ok _ -> false
;;
