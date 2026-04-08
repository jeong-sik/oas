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

  match message with
  | `Null -> Error "Invalid Ollama response: message field is null"
  | `Assoc _ ->
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
                  let arguments =
                    fn |> member "arguments" |> to_string_option |> Option.value ~default:"{}"
                  in
                  Some
                    (ToolUse
                       { id = tc |> member "id" |> to_string_option |> Option.value ~default:"ollama-call";
                         name = fn |> member "name" |> to_string;
                         input = Api_common.json_of_string_or_raw arguments })
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
        let system_fingerprint = None in
        let timings = None in
        let reasoning_tokens = None in
        Some { Types.system_fingerprint; timings; reasoning_tokens; request_latency_ms = 0;
                provider_kind = None; reasoning_effort = None }
      in

      Ok {
        id = json |> member "model" |> to_string_option |> Option.value ~default:"ollama";
        model = json |> member "model" |> to_string_option |> Option.value ~default:"";
        stop_reason;
        content = thinking_blocks @ (if Api_common.string_is_blank text_content then [] else [Text text_content]) @ tool_blocks;
        usage;
        telemetry;
      }
  | _ -> Error "Invalid Ollama response: message field is missing or invalid type"
