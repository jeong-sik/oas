(** Ollama API request building and response parsing *)

open Types

let build_ollama_chat_body ~provider_config ~config ~messages ?tools () =
  let base =
    Yojson.Safe.from_string
      (Api_openai.build_openai_body ~provider_config ~config ~messages ?tools ())
  in
  match base with
  | `Assoc fields ->
      Yojson.Safe.to_string
        (`Assoc
          (("stream", `Bool false)
           :: ("think", `Bool false)
           :: ("options", `Assoc [("num_predict", `Int config.config.max_tokens)])
           :: fields))
  | other ->
      raise (Api_openai.Openai_api_error
        (Printf.sprintf "build_ollama_chat_body: unexpected JSON shape: %s"
          (Yojson.Safe.to_string other)))

let build_ollama_generate_body ~config ~messages () =
  let prompt =
    let system =
      match config.config.system_prompt with
      | Some s when not (Api_common.string_is_blank s) -> [ "[System] " ^ s ]
      | _ -> []
    in
    let rendered =
      messages
      |> List.concat_map (fun msg ->
             match msg.role with
             | User | Tool ->
                 let text = Api_common.text_blocks_to_string msg.content in
                 if Api_common.string_is_blank text then [] else [ text ]
             | Assistant ->
                 let text = Api_common.text_blocks_to_string msg.content in
                 if Api_common.string_is_blank text then [] else [ "[Assistant] " ^ text ]
             | System ->
                 let text = Api_common.text_blocks_to_string msg.content in
                 if Api_common.string_is_blank text then [] else [ "[System] " ^ text ])
    in
    String.concat "\n" (system @ rendered)
  in
  Yojson.Safe.to_string
    (`Assoc
      [
        ("model", `String (model_to_string config.config.model));
        ("prompt", `String prompt);
        ("stream", `Bool false);
        ("think", `Bool false);
        ("options", `Assoc [("num_predict", `Int config.config.max_tokens)]);
      ])

let parse_ollama_chat_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let msg = json |> member "message" in
  let content = msg |> member "content" |> to_string_option |> Option.value ~default:"" in
  let tool_blocks =
    match msg |> member "tool_calls" with
    | `List calls ->
        List.filter_map
          (fun tc ->
            try
              let fn = tc |> member "function" in
              let arguments =
                match fn |> member "arguments" with
                | `String s -> s
                | `Null -> "{}"
                | other -> Yojson.Safe.to_string other
              in
              Some
                (ToolUse
                   { id = tc |> member "id" |> to_string_option |> Option.value ~default:"function_call";
                     name = fn |> member "name" |> to_string;
                     input = Api_common.json_of_string_or_raw arguments })
            with Yojson.Safe.Util.Type_error _ | Yojson.Safe.Util.Undefined _ | Yojson.Json_error _ -> None)
          calls
    | _ -> []
  in
  let usage =
    Some
      {
        input_tokens =
          json |> member "prompt_eval_count" |> to_int_option |> Option.value ~default:0;
        output_tokens =
          json |> member "eval_count" |> to_int_option |> Option.value ~default:0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
      }
  in
  {
    id = json |> member "created_at" |> to_string_option |> Option.value ~default:"";
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    stop_reason = if tool_blocks <> [] then StopToolUse else EndTurn;
    content = (if Api_common.string_is_blank content then [] else [Text content]) @ tool_blocks;
    usage;
  }

let parse_ollama_generate_response json_str =
  let open Yojson.Safe.Util in
  let json = Yojson.Safe.from_string json_str in
  let content = json |> member "response" |> to_string_option |> Option.value ~default:"" in
  let usage =
    Some
      {
        input_tokens =
          json |> member "prompt_eval_count" |> to_int_option |> Option.value ~default:0;
        output_tokens =
          json |> member "eval_count" |> to_int_option |> Option.value ~default:0;
        cache_creation_input_tokens = 0;
        cache_read_input_tokens = 0;
      }
  in
  {
    id = json |> member "created_at" |> to_string_option |> Option.value ~default:"";
    model = json |> member "model" |> to_string_option |> Option.value ~default:"";
    stop_reason = EndTurn;
    content = if Api_common.string_is_blank content then [] else [Text content];
    usage;
  }
