(** Ollama native API request building and response parsing.

    Uses [/api/chat] endpoint with [think] parameter for thinking control,
    and [options] object for sampling parameters.

    @since 0.113.0 *)

open Types

(* ── Request building ────────────────────────────────── *)

(** Build Ollama native [/api/chat] request body.
    Key differences from OpenAI compat:
    - [think] parameter (boolean) instead of [chat_template_kwargs]
    - Sampling params go inside [options] object
    - [num_predict] instead of [max_tokens]
    - No [tool_choice] support *)
let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =

  let provider_messages =
    (match config.system_prompt with
     | Some s when not (Api_common.string_is_blank s) ->
         [`Assoc [("role", `String "system"); ("content", `String (Utf8_sanitize.sanitize s))]]
     | _ -> [])
    @ List.concat_map Backend_openai_serialize.openai_messages_of_message messages
  in

  let body =
    [ ("model", `String config.model_id);
      ("messages", `List provider_messages) ]
  in

  (* think: false by default for Ollama to prevent thinking models from
     consuming all tokens in reasoning. Only enable when explicitly requested. *)
  let think = match config.enable_thinking with
    | Some true -> true
    | _ -> false
  in
  let body = ("think", `Bool think) :: body in

  (* Ollama defaults to stream=true, so always send explicit value *)
  let body = ("stream", `Bool stream) :: body in

  let body = match tools with
    | [] -> body
    | ts ->
        ("tools", `List (List.map Backend_openai_serialize.build_openai_tool_json ts)) :: body
  in

  (* Sampling parameters go inside "options" object *)
  let options = ref [] in
  (match config.max_tokens with
   | n -> options := ("num_predict", `Int n) :: !options);
  (match config.temperature with
   | Some t -> options := ("temperature", `Float t) :: !options
   | None -> ());
  (match config.top_p with
   | Some p -> options := ("top_p", `Float p) :: !options
   | None -> ());
  (match config.top_k with
   | Some k -> options := ("top_k", `Int k) :: !options
   | None -> ());
  (match config.min_p with
   | Some p -> options := ("min_p", `Float p) :: !options
   | None -> ());
  let body = ("options", `Assoc !options) :: body in

  Yojson.Safe.to_string (`Assoc body)

(* ── Response parsing ────────────────────────────────── *)

let parse_ollama_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in

  match json |> member "error" with
  | `String s -> Error s
  | `Assoc _ as err -> Error (Yojson.Safe.to_string err)
  | _ ->

  let message = json |> member "message" in

  let text_content =
    match message |> member "content" with
    | `String s -> s
    | _ -> ""
  in

  let tool_blocks =
    match message |> member "tool_calls" with
    | `List calls ->
        List.filter_map
          (fun tc ->
            try
              let fn = tc |> member "function" in
              (* Ollama native API returns arguments as JSON object, not string *)
              let input = match fn |> member "arguments" with
                | `Assoc _ as obj -> obj
                | `String s -> Api_common.json_of_string_or_raw s
                | other -> other
              in
              Some
                (ToolUse
                   { id = tc |> member "id" |> to_string_option
                          |> Option.value ~default:(Printf.sprintf "ollama-%d" (Hashtbl.hash tc));
                     name = fn |> member "name" |> to_string;
                     input })
            with Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ | Yojson.Json_error _ -> None)
          calls
    | _ -> []
  in

  let thinking_blocks =
    match message |> member "thinking" with
    | `String s when not (Api_common.string_is_blank s) ->
        [Thinking { thinking_type = "thinking"; content = s }]
    | _ -> []
  in

  let done_reason =
    json |> member "done_reason" |> to_string_option |> Option.value ~default:"stop"
  in

  let stop_reason =
    match String.lowercase_ascii done_reason with
    | "tool_calls" when tool_blocks <> [] -> StopToolUse
    | "length" -> MaxTokens
    | "stop" -> EndTurn
    | _other when tool_blocks <> [] -> StopToolUse
    | other -> Unknown other
  in

  let input_tokens =
    json |> member "prompt_eval_count" |> to_int_option |> Option.value ~default:0
  in
  let output_tokens =
    json |> member "eval_count" |> to_int_option |> Option.value ~default:0
  in

  let usage =
    if input_tokens = 0 && output_tokens = 0 then None
    else Some {
      input_tokens;
      output_tokens;
      cache_creation_input_tokens = 0;
      cache_read_input_tokens = 0;
      cost_usd = None;
    }
  in

  let telemetry =
    let system_fingerprint =
      json |> member "system_fingerprint" |> to_string_option
    in
    (* Ollama native timings: durations in nanoseconds *)
    let prompt_eval_duration =
      json |> member "prompt_eval_duration" |> to_int_option
    in
    let eval_duration =
      json |> member "eval_duration" |> to_int_option
    in
    let eval_count =
      json |> member "eval_count" |> to_int_option
    in
    let prompt_eval_count =
      json |> member "prompt_eval_count" |> to_int_option
    in
    let timings =
      if prompt_eval_duration = None && eval_duration = None then None
      else
        let ns_to_ms = function Some n -> Some (float_of_int n /. 1e6) | None -> None in
        let per_second n_opt ms_opt = match n_opt, ms_opt with
          | Some n, Some ms when ms > 0.0 -> Some (float_of_int n /. (ms /. 1000.0))
          | _ -> None
        in
        let prompt_ms = ns_to_ms prompt_eval_duration in
        let predicted_ms = ns_to_ms eval_duration in
        Some {
          Types.prompt_n = prompt_eval_count;
          prompt_ms;
          prompt_per_second = per_second prompt_eval_count prompt_ms;
          predicted_n = eval_count;
          predicted_ms;
          predicted_per_second = per_second eval_count predicted_ms;
          cache_n = None;
        }
    in
    let reasoning_tokens = match thinking_blocks with
      | [Thinking { content; _ }] -> Some (max 1 (String.length content / 4))
      | _ -> None
    in
    Some { Types.system_fingerprint; timings; reasoning_tokens; request_latency_ms = 0;
           provider_kind = Some "ollama"; reasoning_effort = None }
  in

  Ok {
    id = json |> member "model" |> to_string_option |> Option.value ~default:"ollama";
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    stop_reason;
    content = thinking_blocks @ (if Api_common.string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
    usage;
    telemetry;
  }

[@@@coverage off]
(* === Inline tests === *)

let%test "build_request sets think=false by default" =
  let config = Provider_config.make ~kind:Ollama ~model_id:"qwen3.5:9b"
    ~base_url:"http://localhost:11434" ~max_tokens:4096 () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  Yojson.Safe.Util.(json |> member "think" |> to_bool) = false

let%test "build_request sets think=true when enabled" =
  let config = Provider_config.make ~kind:Ollama ~model_id:"qwen3.5:9b"
    ~base_url:"http://localhost:11434" ~max_tokens:4096
    ~enable_thinking:true () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  Yojson.Safe.Util.(json |> member "think" |> to_bool) = true

let%test "build_request puts sampling in options" =
  let config = Provider_config.make ~kind:Ollama ~model_id:"m"
    ~base_url:"http://localhost:11434" ~max_tokens:2048
    ~temperature:0.7 ~min_p:0.05 () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  let opts = Yojson.Safe.Util.(json |> member "options") in
  Yojson.Safe.Util.(opts |> member "num_predict" |> to_int) = 2048
  && Yojson.Safe.Util.(opts |> member "temperature" |> to_float) = 0.7
  && Yojson.Safe.Util.(opts |> member "min_p" |> to_float) = 0.05

let%test "build_request always sends stream field" =
  let config = Provider_config.make ~kind:Ollama ~model_id:"m"
    ~base_url:"http://localhost:11434" ~max_tokens:100 () in
  let body = build_request ~config ~messages:[] () in
  let json = Yojson.Safe.from_string body in
  Yojson.Safe.Util.(json |> member "stream" |> to_bool) = false

let%test "parse_ollama_response basic text" =
  let json = {|{"model":"qwen3.5:9b","message":{"role":"assistant","content":"Hello"},"done":true,"done_reason":"stop","prompt_eval_count":10,"eval_count":5,"eval_duration":100000000}|} in
  match parse_ollama_response json with
  | Ok resp ->
      resp.model = "qwen3.5:9b"
      && resp.stop_reason = EndTurn
      && resp.content = [Text "Hello"]
      && (match resp.telemetry with
          | Some t -> t.provider_kind = Some "ollama"
                      && t.timings <> None
          | None -> false)
  | Error _ -> false

let%test "parse_ollama_response with thinking" =
  let json = {|{"model":"m","message":{"role":"assistant","content":"Hi","thinking":"Let me think..."},"done":true,"done_reason":"stop","eval_count":10}|} in
  match parse_ollama_response json with
  | Ok resp ->
      List.length resp.content = 2
      && (match List.hd resp.content with
          | Thinking { content; _ } -> content = "Let me think..."
          | _ -> false)
      && (match resp.telemetry with
          | Some t -> t.reasoning_tokens <> None
          | None -> false)
  | Error _ -> false

let%test "parse_ollama_response with tool_calls" =
  let json = {|{"model":"m","message":{"role":"assistant","content":"","tool_calls":[{"function":{"name":"get_weather","arguments":{"city":"Seoul"}}}]},"done":true,"done_reason":"tool_calls"}|} in
  match parse_ollama_response json with
  | Ok resp ->
      resp.stop_reason = StopToolUse
      && (match resp.content with
          | [ToolUse { name; _ }] -> name = "get_weather"
          | _ -> false)
  | Error _ -> false

let%test "parse_ollama_response error" =
  let json = {|{"error":"model not found"}|} in
  match parse_ollama_response json with
  | Error msg -> msg = "model not found"
  | Ok _ -> false
