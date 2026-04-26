(** LLM-based evaluation and scoring.

    Provides a single-turn judgment call against a single provider.
    The LLM receives a system prompt and context, and returns
    a structured JSON response parsed into a {!judgment} record.

    @since 0.78.0
    @since 0.142.0 Single-provider API. *)

open Llm_provider

(* ── Types ──────────────────────────────────────────────── *)

type risk_level = Low | Medium | High | Critical
[@@deriving yojson, show]

type judgment = {
  score: float;
  confidence: float;
  risk: risk_level;
  summary: string;
  evidence: string list;
  recommended_action: string option;
}
[@@deriving yojson, show]

type judge_config = {
  system_prompt: string;
  temperature: float;
  max_tokens: int;
  output_schema: Yojson.Safe.t option;
}
[@@deriving show]

let default_config () = {
  system_prompt = "You are a precise evaluator. Analyze the given context and return a JSON object with: score (0.0-1.0), confidence (0.0-1.0), risk (low/medium/high/critical), summary (string), evidence (string array), recommended_action (string or null).";
  temperature = 0.2;
  max_tokens = 2048;
  output_schema = None;
}

(* ── Risk derivation ────────────────────────────────────── *)

let risk_of_score score =
  if score < 0.3 then Low
  else if score < 0.6 then Medium
  else if score < 0.8 then High
  else Critical

(* ── JSON parsing helpers ───────────────────────────────── *)

let risk_level_of_string = function
  | "low" -> Low
  | "medium" -> Medium
  | "high" -> High
  | "critical" -> Critical
  | _ -> Low  (* conservative default *)

let clamp_01 v =
  Float.max 0.0 (Float.min 1.0 v)

(** Extract the first JSON object from a string that may contain
    markdown fencing or surrounding prose. *)
let extract_json_substring text =
  match String.index_opt text '{' with
  | None -> None
  | Some start ->
    let len = String.length text in
    let depth = ref 0 in
    let in_string = ref false in
    let escape = ref false in
    let found = ref None in
    let i = ref start in
    while !i < len && Option.is_none !found do
      let c = text.[!i] in
      if !escape then
        escape := false
      else if c = '\\' && !in_string then
        escape := true
      else if c = '"' then
        in_string := not !in_string
      else if not !in_string then begin
        if c = '{' then incr depth
        else if c = '}' then begin
          decr depth;
          if !depth = 0 then
            found := Some (String.sub text start (!i - start + 1))
        end
      end;
      incr i
    done;
    !found

let parse_judgment text =
  let json_str = match extract_json_substring text with
    | Some s -> s
    | None -> text
  in
  try
    let json = Yojson.Safe.from_string json_str in
    let open Yojson.Safe.Util in
    let score = clamp_01 (json |> member "score" |> to_float) in
    let confidence = clamp_01
        (try json |> member "confidence" |> to_float
         with Type_error _ -> 0.5) in
    let risk =
      try json |> member "risk" |> to_string |> String.lowercase_ascii |> risk_level_of_string
      with Type_error _ -> risk_of_score score
    in
    let summary =
      try json |> member "summary" |> to_string
      with Type_error _ -> "No summary provided"
    in
    let evidence =
      try json |> member "evidence" |> to_list |> List.filter_map (fun j ->
        try Some (to_string j) with Type_error _ -> None)
      with Type_error _ -> []
    in
    let recommended_action =
      try
        match json |> member "recommended_action" with
        | `Null -> None
        | j -> Some (to_string j)
      with Type_error _ -> None
    in
    Ok { score; confidence; risk; summary; evidence; recommended_action }
  with
  | Yojson.Json_error msg ->
    Error (Printf.sprintf "JSON parse error: %s" msg)
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error (Printf.sprintf "JSON type error: %s" msg)

(* ── LLM call (single provider) ─────────────────────────── *)

let judge ~sw ~net ~provider ~config ~context () =
  let messages : Types.message list = [
    { role = System; content = [Text config.system_prompt]; name = None; tool_call_id = None ; metadata = []};
    { role = User; content = [Text context]; name = None; tool_call_id = None ; metadata = []};
  ] in
  (* Override provider sampling with judge_config (deterministic evaluation). *)
  let provider_cfg = {
    provider with
    Provider_config.temperature = Some config.temperature;
    max_tokens = Some config.max_tokens;
    output_schema = config.output_schema;
  } in
  match
    Complete.complete ~sw ~net ~config:provider_cfg ~messages ~tools:[] ()
  with
  | Error err ->
    let msg = match err with
      | Http_client.HttpError { code; body } ->
        Printf.sprintf "HTTP %d: %s" code
          (if String.length body > 200
           then String.sub body 0 200 ^ "..."
           else body)
      | Http_client.AcceptRejected { reason } -> reason
      | Http_client.NetworkError { message; _ } -> message
      | Http_client.CliTransportRequired { kind } ->
        Printf.sprintf "CLI transport required for %s" kind
      | Http_client.ProviderTerminal { message; _ } -> message
    in
    Error (Printf.sprintf "Judge LLM call failed: %s" msg)
  | Ok response ->
    let text = Types.text_of_response response in
    if String.length text = 0 then
      Error "Judge LLM returned empty response"
    else
      match parse_judgment text with
      | Ok j -> Ok j
      | Error _parse_err ->
        (* Fallback: create low-confidence judgment from raw text *)
        Ok {
          score = 0.5;
          confidence = 0.1;
          risk = Medium;
          summary = (if String.length text > 200
                     then String.sub text 0 200 ^ "..."
                     else text);
          evidence = [];
          recommended_action = Some "Manual review recommended (structured parse failed)";
        }
