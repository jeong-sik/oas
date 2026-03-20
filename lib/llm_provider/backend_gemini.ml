(** Gemini native API request building and response parsing.

    Wire format: contents/parts, systemInstruction, thinkingConfig,
    functionDeclarations.  Ref: ai.google.dev/api/generate-content

    @since 0.72.0 *)

open Types

exception Gemini_api_error of string

(* ── Helpers ────────────────────────────────────────── *)

let gemini_role_of_oas = function
  | User | System | Tool -> "user"
  | Assistant -> "model"

(** Build a tool_use_id -> tool_name lookup table from message history.
    Gemini's functionResponse requires the function NAME, but OAS
    ToolResult only carries the tool_use_id (a UUID). *)
let build_tool_id_to_name (messages : message list) : (string, string) Hashtbl.t =
  let tbl = Hashtbl.create 8 in
  List.iter (fun (msg : message) ->
    List.iter (function
      | ToolUse { id; name; _ } -> Hashtbl.replace tbl id name
      | _ -> ()
    ) msg.content
  ) messages;
  tbl

(* ── Content block -> Gemini part ───────────────────── *)

let part_of_content_block id_to_name = function
  | Text s ->
      Some (`Assoc [("text", `String s)])
  | Thinking { content; _ } ->
      Some (`Assoc [("thought", `Bool true); ("text", `String content)])
  | Image { media_type; data; _ } ->
      Some (`Assoc [
        ("inlineData", `Assoc [
          ("mimeType", `String media_type);
          ("data", `String data);
        ])
      ])
  | Audio { media_type; data; _ } ->
      Some (`Assoc [
        ("inlineData", `Assoc [
          ("mimeType", `String media_type);
          ("data", `String data);
        ])
      ])
  | Document { media_type; data; _ } ->
      Some (`Assoc [
        ("inlineData", `Assoc [
          ("mimeType", `String media_type);
          ("data", `String data);
        ])
      ])
  | ToolUse { name; input; _ } ->
      Some (`Assoc [
        ("functionCall", `Assoc [
          ("name", `String name);
          ("args", input);
        ])
      ])
  | ToolResult { tool_use_id; content; _ } ->
      let name = match Hashtbl.find_opt id_to_name tool_use_id with
        | Some n -> n
        | None -> tool_use_id
      in
      Some (`Assoc [
        ("functionResponse", `Assoc [
          ("name", `String name);
          ("response", `Assoc [("result", `String content)]);
        ])
      ])
  | RedactedThinking _ -> None

(* ── Message list -> (contents, systemInstruction option) ── *)

let contents_of_messages (messages : message list) =
  let id_to_name = build_tool_id_to_name messages in
  let system_parts = ref [] in
  let contents = ref [] in
  List.iter (fun (msg : message) ->
    match msg.role with
    | System ->
        let parts = List.filter_map (part_of_content_block id_to_name) msg.content in
        system_parts := !system_parts @ parts
    | User | Assistant | Tool ->
        let parts = List.filter_map (part_of_content_block id_to_name) msg.content in
        if parts <> [] then
          contents := `Assoc [
            ("role", `String (gemini_role_of_oas msg.role));
            ("parts", `List parts);
          ] :: !contents
  ) messages;
  let system_instruction = match !system_parts with
    | [] -> None
    | parts -> Some (`Assoc [("parts", `List parts)])
  in
  (List.rev !contents, system_instruction)

(* ── Tool schema -> Gemini functionDeclarations ─────── *)

let build_function_declaration = function
  | `Assoc fields ->
      let name = match List.assoc_opt "name" fields with
        | Some (`String s) -> s | _ -> "tool"
      in
      let description = match List.assoc_opt "description" fields with
        | Some (`String s) -> s | _ -> ""
      in
      let parameters = match List.assoc_opt "input_schema" fields with
        | Some schema -> schema
        | None ->
            (match List.assoc_opt "parameters" fields with
             | Some schema -> schema
             | None -> `Assoc [])
      in
      `Assoc [
        ("name", `String name);
        ("description", `String description);
        ("parameters", parameters);
      ]
  | other -> other

(* ── Build request body ─────────────────────────────── *)

let build_request ?(stream=false) ~(config : Provider_config.t)
    ~(messages : message list) ?(tools : Yojson.Safe.t list = []) () =
  ignore stream;  (* Gemini streaming is URL-based, not body-based *)
  let (contents, system_instruction) = contents_of_messages messages in
  (* Prepend system_prompt from config if present *)
  let system_instruction = match config.system_prompt, system_instruction with
    | Some s, None when not (Api_common.string_is_blank s) ->
        Some (`Assoc [("parts", `List [`Assoc [("text", `String s)]])])
    | Some s, Some (`Assoc fields) when not (Api_common.string_is_blank s) ->
        let existing_parts = match List.assoc_opt "parts" fields with
          | Some (`List ps) -> ps | _ -> []
        in
        let config_part = `Assoc [("text", `String s)] in
        Some (`Assoc [("parts", `List (config_part :: existing_parts))])
    | _, si -> si
  in
  let body = [("contents", `List contents)] in
  let body = match system_instruction with
    | Some si -> ("systemInstruction", si) :: body
    | None -> body
  in
  (* generationConfig *)
  let gen_config = ref [] in
  gen_config := ("maxOutputTokens", `Int config.max_tokens) :: !gen_config;
  (match config.temperature with
   | Some t -> gen_config := ("temperature", `Float t) :: !gen_config
   | None -> ());
  (match config.top_p with
   | Some p -> gen_config := ("topP", `Float p) :: !gen_config
   | None -> ());
  (match config.top_k with
   | Some k -> gen_config := ("topK", `Int k) :: !gen_config
   | None -> ());
  (* Thinking config *)
  (match config.enable_thinking with
   | Some true ->
       let budget = match config.thinking_budget with
         | Some b -> b | None -> 10000 in
       gen_config :=
         ("thinkingConfig", `Assoc [
           ("thinkingBudget", `Int budget);
           ("includeThoughts", `Bool true);
         ]) :: !gen_config
   | _ -> ());
  (* JSON mode *)
  if config.response_format_json then
    gen_config := ("responseMimeType", `String "application/json") :: !gen_config;
  let body = ("generationConfig", `Assoc !gen_config) :: body in
  (* Tools *)
  let body = match tools with
    | [] -> body
    | ts ->
        let func_decls = List.map build_function_declaration ts in
        ("tools", `List [
          `Assoc [("functionDeclarations", `List func_decls)]
        ]) :: body
  in
  (* Tool config (tool_choice) *)
  let body = match config.tool_choice with
    | Some Auto ->
        ("toolConfig", `Assoc [
          ("functionCallingConfig", `Assoc [("mode", `String "AUTO")])
        ]) :: body
    | Some Any ->
        ("toolConfig", `Assoc [
          ("functionCallingConfig", `Assoc [("mode", `String "ANY")])
        ]) :: body
    | Some None_ ->
        ("toolConfig", `Assoc [
          ("functionCallingConfig", `Assoc [("mode", `String "NONE")])
        ]) :: body
    | Some (Tool name) ->
        ("toolConfig", `Assoc [
          ("functionCallingConfig", `Assoc [
            ("mode", `String "ANY");
            ("allowedFunctionNames", `List [`String name]);
          ])
        ]) :: body
    | None -> body
  in
  Yojson.Safe.to_string (`Assoc body)

(* ── Parse response ─────────────────────────────────── *)

let parse_response json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Null | `Assoc [] ->
      let candidates = json |> member "candidates" in
      let candidate = match candidates with
        | `List (c :: _) -> c
        | _ -> json  (* fallback for unexpected shapes *)
      in
      let content_obj = candidate |> member "content" in
      let parts = match content_obj |> member "parts" with
        | `List ps -> ps | _ -> []
      in
      let content = List.filter_map (fun part ->
        match part |> member "text" with
        | `String s ->
            let is_thought =
              part |> member "thought" |> to_bool_option
              |> Option.value ~default:false
            in
            if is_thought then
              Some (Thinking { thinking_type = "thinking"; content = s })
            else
              Some (Text s)
        | _ ->
            match part |> member "functionCall" with
            | `Assoc _ as fc ->
                let name = fc |> member "name" |> to_string in
                let args = fc |> member "args" in
                let id = Printf.sprintf "call_%s_%d"
                    name (Hashtbl.hash (Yojson.Safe.to_string args))
                in
                Some (ToolUse { id; name; input = args })
            | _ -> None
      ) parts in
      let finish_reason =
        candidate |> member "finishReason" |> to_string_option
        |> Option.value ~default:"STOP"
      in
      let has_tool_use =
        List.exists (function ToolUse _ -> true | _ -> false) content
      in
      let stop_reason =
        if has_tool_use then StopToolUse
        else match String.uppercase_ascii finish_reason with
          | "STOP" -> EndTurn
          | "MAX_TOKENS" -> MaxTokens
          | "SAFETY" -> Unknown "safety"
          | "RECITATION" -> Unknown "recitation"
          | other -> Unknown other
      in
      let usage =
        let um = json |> member "usageMetadata" in
        if um = `Null then None
        else
          Some {
            input_tokens =
              um |> member "promptTokenCount" |> to_int_option
              |> Option.value ~default:0;
            output_tokens =
              um |> member "candidatesTokenCount" |> to_int_option
              |> Option.value ~default:0;
            cache_creation_input_tokens = 0;
            cache_read_input_tokens =
              um |> member "cachedContentTokenCount" |> to_int_option
              |> Option.value ~default:0;
          }
      in
      let model_str =
        json |> member "modelVersion" |> to_string_option
        |> Option.value ~default:""
      in
      { id = ""; model = model_str; stop_reason; content; usage }
  | err ->
      let msg =
        err |> member "message" |> to_string_option
        |> Option.value ~default:"Unknown Gemini API error"
      in
      raise (Gemini_api_error msg)
