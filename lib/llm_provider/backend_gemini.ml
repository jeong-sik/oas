(** Gemini native API request building and response parsing.

    Wire format: contents/parts, systemInstruction, thinkingConfig,
    functionDeclarations.  Ref: ai.google.dev/api/generate-content

    @since 0.72.0 *)

open Types

type request_artifact = string Request_artifact_internal.t

let request_payload = Request_artifact_internal.payload
let request_output_token_receipt = Request_artifact_internal.output_token_receipt

exception Gemini_api_error of string

(* ── Helpers ────────────────────────────────────────── *)

(* The Gemini API has no parallel-function-call disable. [functionCallingConfig]
   supports only [mode] (AUTO/ANY/VALIDATED/NONE) and [allowedFunctionNames] --
   there is no equivalent of OpenAI [parallel_tool_calls:false] or Anthropic
   [tool_choice.disable_parallel_tool_use]. A caller's [disable_parallel_tool_use]
   therefore cannot be honored on the wire and is dropped; we surface that
   asymmetry once per model rather than ignoring it silently. Verified 2026-06-03
   against ai.google.dev/gemini-api/docs/function-calling.
   Stored as an atomic list so the check works with or without an Eio scheduler. *)
let parallel_disable_warned : string list Atomic.t = Atomic.make []

let warn_parallel_disable_unsupported ~model_id =
  let warned = Atomic.get parallel_disable_warned in
  if not (List.mem model_id warned)
  then (
    let rec mark () =
      let old = Atomic.get parallel_disable_warned in
      if List.mem model_id old
      then ()
      else if Atomic.compare_and_set parallel_disable_warned old (model_id :: old)
      then
        Diag.warn
          "backend_gemini"
          "disable_parallel_tool_use requested for model %s but the Gemini API has no \
           parallel-disable option (functionCallingConfig supports only mode and \
           allowedFunctionNames); ignoring."
          model_id
      else mark ()
    in
    mark ())
;;

let thinking_level_of_budget ~supports_minimal = function
  | Some n when n <= 0 -> if supports_minimal then "minimal" else "low"
  | Some n ->
    (match Reasoning_effort.of_budget n with
     | Some Reasoning_effort.Low -> "low"
     | Some Reasoning_effort.Medium -> "medium"
     | Some Reasoning_effort.High | Some Reasoning_effort.XHigh -> "high"
     | Some (Reasoning_effort.None_ | Reasoning_effort.Minimal) | None ->
       if supports_minimal then "minimal" else "low")
  | None -> "high"
;;

let thinking_config_of_config (config : Provider_config.t) =
  match Capabilities.gemini_thinking_control_of_id config.model_id with
  | Capabilities.Gemini_thinking_level { supports_minimal } ->
    (match config.enable_thinking with
     | Some false ->
       let level = if supports_minimal then "minimal" else "low" in
       Some (`Assoc [ "thinkingLevel", `String level ])
     | Some true ->
       let level = thinking_level_of_budget ~supports_minimal config.thinking_budget in
       Some (`Assoc [ "thinkingLevel", `String level; "includeThoughts", `Bool true ])
     | None -> None)
  | Capabilities.Gemini_thinking_budget | Capabilities.Gemini_unknown_thinking_control ->
    (match config.enable_thinking with
     | Some false -> Some (`Assoc [ "thinkingBudget", `Int 0 ])
     | Some true ->
       let budget =
         match config.thinking_budget with
         | Some b -> b
         | None -> Constants.Thinking.gemini_budget ()
       in
       Some (`Assoc [ "thinkingBudget", `Int budget; "includeThoughts", `Bool true ])
     | None -> None)
;;

let gemini_role_of_oas = function
  | User | System | Tool -> "user"
  | Assistant -> "model"
;;

let gemini_thought_signature_kind = "gemini_thought_signature"
let gemini_part_thought_signature_kind = "gemini_part_thought_signature"

type gemini_part_signature_target =
  | Gemini_text_part
  | Gemini_thought_part

let gemini_part_signature_target_to_string = function
  | Gemini_text_part -> "text"
  | Gemini_thought_part -> "thought"
;;

let gemini_part_signature_target_of_string = function
  | "text" -> Some Gemini_text_part
  | "thought" -> Some Gemini_thought_part
  | _ -> None
;;

let string_field_opt key = function
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`String s) when not (Api_common.string_is_blank s) -> Some s
     | Some _ | None -> None)
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let gemini_thought_signature_payload ~tool_use_id ~thought_signature =
  Yojson.Safe.to_string
    (`Assoc
        [ "provider", `String "gemini"
        ; "kind", `String gemini_thought_signature_kind
        ; "tool_use_id", `String tool_use_id
        ; "thoughtSignature", `String thought_signature
        ])
;;

let gemini_thought_signature_carrier ~tool_use_id ~thought_signature =
  RedactedThinking (gemini_thought_signature_payload ~tool_use_id ~thought_signature)
;;

let gemini_part_thought_signature_payload ~target ~thought_signature =
  Provider_replay.encode_exact_next_block
    ~payload:
      (`Assoc
          [ "provider", `String "gemini"
          ; "kind", `String gemini_part_thought_signature_kind
          ; "target", `String (gemini_part_signature_target_to_string target)
          ; "thoughtSignature", `String thought_signature
          ])
;;

let gemini_part_thought_signature_carrier ~target ~thought_signature =
  RedactedThinking (gemini_part_thought_signature_payload ~target ~thought_signature)
;;

let exact_string_field_opt key = function
  | `Assoc fields ->
    (match List.assoc_opt key fields with
     | Some (`String s) -> Some s
     | Some _ | None -> None)
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null -> None
;;

let gemini_thought_signature_of_redacted data =
  try
    let json = Yojson.Safe.from_string data in
    match string_field_opt "provider" json, string_field_opt "kind" json with
    | Some "gemini", Some kind when String.equal kind gemini_thought_signature_kind ->
      (match
         ( string_field_opt "tool_use_id" json
         , match exact_string_field_opt "thoughtSignature" json with
           | Some s -> Some s
           | None -> exact_string_field_opt "thought_signature" json )
       with
       | Some tool_use_id, Some thought_signature -> Some (tool_use_id, thought_signature)
       | _ -> None)
    | _ -> None
  with
  | Yojson.Json_error _ -> None
;;

type gemini_part_signature_decode =
  | Not_gemini_part_signature
  | Decoded_gemini_part_signature of gemini_part_signature_target * string
  | Malformed_gemini_part_signature

let decode_gemini_part_thought_signature data =
  match Provider_replay.decode data with
  | Provider_replay.Not_replay -> Not_gemini_part_signature
  | Provider_replay.Malformed_replay _ -> Malformed_gemini_part_signature
  | Provider_replay.Replay
      { retention = Provider_replay.Exact_next_block; payload = json } ->
    (match string_field_opt "provider" json, string_field_opt "kind" json with
     | Some "gemini", Some kind when String.equal kind gemini_part_thought_signature_kind
       ->
       (match
          ( Option.bind
              (string_field_opt "target" json)
              gemini_part_signature_target_of_string
          , exact_string_field_opt "thoughtSignature" json )
        with
        | Some target, Some thought_signature ->
          Decoded_gemini_part_signature (target, thought_signature)
        | _ -> Malformed_gemini_part_signature)
     | _ -> Malformed_gemini_part_signature)
;;

let gemini_tool_signatures_of_blocks blocks =
  let tbl = Hashtbl.create 8 in
  List.iter
    (function
      | RedactedThinking data ->
        (match gemini_thought_signature_of_redacted data with
         | Some (tool_use_id, signature) -> Hashtbl.replace tbl tool_use_id signature
         | None -> ())
      | Text _
      | Thinking _
      | ReasoningDetails _
      | ToolUse _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ -> ())
    blocks;
  tbl
;;

(** Build a tool_use_id -> tool_name lookup table from message history.
    Gemini's functionResponse requires the function NAME, but OAS
    ToolResult only carries the tool_use_id (a UUID). *)
let build_tool_id_to_name (messages : message list) : (string, string) Hashtbl.t =
  let tbl = Hashtbl.create 8 in
  List.iter
    (fun (msg : message) ->
       List.iter
         (function
           | ToolUse { id; name; _ } -> Hashtbl.replace tbl id name
           | _ -> ())
         msg.content)
    messages;
  tbl
;;

(* ── Content block -> Gemini part ───────────────────── *)

let inline_data_part ~block ~media_type ~data source_type =
  let data = Api_common.base64_media_payload ~backend:"gemini" ~block ~data source_type in
  Some
    (`Assoc
        [ "inlineData", `Assoc [ "mimeType", `String media_type; "data", `String data ] ])
;;

let part_of_content_block id_to_name tool_signatures = function
  | Text s -> Some (`Assoc [ "text", `String (Utf8_sanitize.sanitize s) ])
  | Thinking { content; _ } ->
    Some
      (`Assoc [ "thought", `Bool true; "text", `String (Utf8_sanitize.sanitize content) ])
  | ReasoningDetails _ -> None
  | Image { media_type; data; source_type } ->
    inline_data_part ~block:"image" ~media_type ~data source_type
  | Audio { media_type; data; source_type } ->
    inline_data_part ~block:"audio" ~media_type ~data source_type
  | Document { media_type; data; source_type } ->
    inline_data_part ~block:"document" ~media_type ~data source_type
  | ToolUse { id; name; input } ->
    let fields =
      [ "functionCall", `Assoc [ "id", `String id; "name", `String name; "args", input ] ]
    in
    let fields =
      match Hashtbl.find_opt tool_signatures id with
      | Some signature -> ("thoughtSignature", `String signature) :: fields
      | None -> fields
    in
    Some (`Assoc fields)
  | ToolResult { tool_use_id; content; _ } ->
    let name =
      match Hashtbl.find_opt id_to_name tool_use_id with
      | Some n -> n
      | None ->
        Diag.warn
          "backend_gemini"
          "ToolResult tool_use_id '%s' has no matching ToolUse in %d-entry lookup table; \
           using UUID as functionResponse name (Gemini API requires name). This usually \
           means the ToolUse block was in a conversation turn that was compacted or \
           trimmed."
          tool_use_id
          (Hashtbl.length id_to_name);
        tool_use_id
    in
    Some
      (`Assoc
          [ ( "functionResponse"
            , `Assoc
                [ "id", `String tool_use_id
                ; "name", `String name
                ; ( "response"
                  , `Assoc [ "result", `String (Utf8_sanitize.sanitize content) ] )
                ] )
          ])
  | RedactedThinking _ -> None
;;

let signature_target_of_content_block = function
  | Text _ -> Some Gemini_text_part
  | Thinking _ -> Some Gemini_thought_part
  | ReasoningDetails _
  | RedactedThinking _
  | ToolUse _
  | ToolResult _
  | Image _
  | Document _
  | Audio _ -> None
;;

let same_signature_target left right =
  match left, right with
  | Gemini_text_part, Gemini_text_part | Gemini_thought_part, Gemini_thought_part -> true
  | Gemini_text_part, Gemini_thought_part | Gemini_thought_part, Gemini_text_part -> false
;;

let attach_thought_signature thought_signature = function
  | `Assoc fields -> `Assoc (("thoughtSignature", `String thought_signature) :: fields)
  | `List _ | `String _ | `Int _ | `Intlit _ | `Float _ | `Bool _ | `Null ->
    raise
      (Gemini_api_error
         "Gemini part serializer produced a non-object for a thoughtSignature target")
;;

let parts_of_content_blocks id_to_name tool_signatures blocks =
  (* Gemini requires an opaque [thoughtSignature] to be replayed on the exact
     model part that carried it. OAS represents that otherwise-unmodeled field
     as a [RedactedThinking] block immediately before its target. Adjacency is
     the structural identity: if a reducer breaks it, fail the request rather
     than attaching the signature to a different part or silently dropping it. *)
  let malformed_carrier expected actual =
    let actual =
      match actual with
      | Some target -> gemini_part_signature_target_to_string target
      | None -> "unsupported"
    in
    raise
      (Gemini_api_error
         (Printf.sprintf
            "Gemini thoughtSignature carrier targets %s but its adjacent content block \
             is %s"
            (gemini_part_signature_target_to_string expected)
            actual))
  in
  let rec loop acc = function
    | RedactedThinking data :: target_block :: rest ->
      (match decode_gemini_part_thought_signature data with
       | Not_gemini_part_signature -> loop acc (target_block :: rest)
       | Malformed_gemini_part_signature ->
         raise
           (Gemini_api_error
              "Malformed Gemini thoughtSignature carrier in conversation history")
       | Decoded_gemini_part_signature (expected_target, thought_signature) ->
         let actual_target = signature_target_of_content_block target_block in
         (match actual_target with
          | Some actual_target when same_signature_target expected_target actual_target ->
            (match part_of_content_block id_to_name tool_signatures target_block with
             | Some part ->
               loop (attach_thought_signature thought_signature part :: acc) rest
             | None -> malformed_carrier expected_target (Some actual_target))
          | Some _ | None -> malformed_carrier expected_target actual_target))
    | [ RedactedThinking data ] ->
      (match decode_gemini_part_thought_signature data with
       | Decoded_gemini_part_signature (expected_target, _) ->
         malformed_carrier expected_target None
       | Malformed_gemini_part_signature ->
         raise
           (Gemini_api_error
              "Malformed Gemini thoughtSignature carrier in conversation history")
       | Not_gemini_part_signature -> List.rev acc)
    | block :: rest ->
      let acc =
        match part_of_content_block id_to_name tool_signatures block with
        | Some part -> part :: acc
        | None -> acc
      in
      loop acc rest
    | [] -> List.rev acc
  in
  loop [] blocks
;;

(* ── Message list -> (contents, systemInstruction option) ── *)

let contents_of_messages (messages : message list) =
  let messages =
    messages
    |> Tool_message_pairs.close_for_provider_request
    |> Api_common.merge_tool_result_followup_user_messages
  in
  let id_to_name = build_tool_id_to_name messages in
  let system_parts = ref [] in
  let contents = ref [] in
  List.iter
    (fun (msg : message) ->
       let tool_signatures = gemini_tool_signatures_of_blocks msg.content in
       match msg.role with
       | System ->
         let parts = parts_of_content_blocks id_to_name tool_signatures msg.content in
         system_parts := !system_parts @ parts
       | User | Assistant | Tool ->
         let parts = parts_of_content_blocks id_to_name tool_signatures msg.content in
         if parts <> []
         then
           contents
           := `Assoc
                [ "role", `String (gemini_role_of_oas msg.role); "parts", `List parts ]
              :: !contents)
    messages;
  let system_instruction =
    match !system_parts with
    | [] -> None
    | parts -> Some (`Assoc [ "parts", `List parts ])
  in
  List.rev !contents, system_instruction
;;

(* ── Tool schema -> Gemini functionDeclarations ─────── *)

let build_function_declaration = function
  | `Assoc fields ->
    let name =
      match List.assoc_opt "name" fields with
      | Some (`String s) -> s
      | _ -> "tool"
    in
    let description =
      match List.assoc_opt "description" fields with
      | Some (`String s) -> s
      | _ -> ""
    in
    let parameters =
      match List.assoc_opt "input_schema" fields with
      | Some schema -> schema
      | None ->
        (match List.assoc_opt "parameters" fields with
         | Some schema -> schema
         | None -> `Assoc [])
    in
    `Assoc
      [ "name", `String name
      ; "description", `String description
      ; "parameters", parameters
      ]
  | other -> other
;;

(* ── Build request body ─────────────────────────────── *)

let build_request_artifact
      ?(stream = false)
      ~(config : Provider_config.t)
      ~(messages : message list)
      ?(tools : Yojson.Safe.t list = [])
      ()
  =
  ignore stream;
  (* Gemini streaming is URL-based, not body-based *)
  let output_token_receipt =
    Backend_openai_request.output_token_receipt
      ~envelope:Types.Gemini_generation_config_max_output_tokens
      config
  in
  let contents, system_instruction = contents_of_messages messages in
  (* Prepend system_prompt from config if present *)
  let system_instruction =
    match config.system_prompt, system_instruction with
    | Some s, None when not (Api_common.string_is_blank s) ->
      let s = Utf8_sanitize.sanitize s in
      Some (`Assoc [ "parts", `List [ `Assoc [ "text", `String s ] ] ])
    | Some s, Some (`Assoc fields) when not (Api_common.string_is_blank s) ->
      let s = Utf8_sanitize.sanitize s in
      let existing_parts =
        match List.assoc_opt "parts" fields with
        | Some (`List ps) -> ps
        | _ -> []
      in
      let config_part = `Assoc [ "text", `String s ] in
      Some (`Assoc [ "parts", `List (config_part :: existing_parts) ])
    | _, si -> si
  in
  let body = [ "contents", `List contents ] in
  let body =
    match system_instruction with
    | Some si -> ("systemInstruction", si) :: body
    | None -> body
  in
  (* generationConfig *)
  let gen_config = ref [] in
  (* Shared budget policy (caller override clamped to catalog ceiling,
     omitted when both are unknown) — [maxOutputTokens] is optional on
     generateContent, and omission lets Gemini apply the model's own
     limit instead of an invented 16384. *)
  (match Types.output_token_receipt_effective output_token_receipt with
   | Some mt -> gen_config := ("maxOutputTokens", `Int mt) :: !gen_config
   | None -> ());
  (match config.temperature with
   | Some t -> gen_config := ("temperature", `Float t) :: !gen_config
   | None -> ());
  (match config.top_p with
   | Some p -> gen_config := ("topP", `Float p) :: !gen_config
   | None -> ());
  (match config.top_k with
   | Some k -> gen_config := ("topK", `Int k) :: !gen_config
   | None -> ());
  (* Seed — Gemini API supports seed in generationConfig *)
  (let caps =
     match Capabilities.for_model_id config.model_id with
     | Some c -> c
     | None -> Capabilities.default_capabilities
   in
   if caps.supports_seed
   then (
     let seed =
       match config.seed with
       | Some n -> n
       | None ->
         (match Constants.Deterministic.seed_of_env () with
          | Some n -> n
          | None -> Constants.Deterministic.default_seed)
     in
     gen_config := ("seed", `Int seed) :: !gen_config));
  (* Gemini 3+ uses [thinkingLevel]; Gemini 2.5 uses [thinkingBudget]. *)
  (match thinking_config_of_config config with
   | Some thinking_config ->
     gen_config := ("thinkingConfig", thinking_config) :: !gen_config
   | None -> ());
  let structured_schema =
    match config.output_schema, config.response_format with
    | Some schema, _ -> Some schema
    | None, Types.JsonSchema schema -> Some schema
    | None, Types.JsonMode | None, Types.Off -> None
  in
  (* JSON mode / native structured output *)
  (match structured_schema with
   | Some schema ->
     gen_config
     := ("responseJsonSchema", schema)
        :: ("responseMimeType", `String "application/json")
        :: !gen_config
   | None when config.response_format = Types.JsonMode ->
     gen_config := ("responseMimeType", `String "application/json") :: !gen_config
   | None -> ());
  let body = ("generationConfig", `Assoc !gen_config) :: body in
  (* Tools *)
  let body =
    match tools with
    | [] -> body
    | ts ->
      if config.disable_parallel_tool_use
      then warn_parallel_disable_unsupported ~model_id:config.model_id;
      let func_decls = List.map build_function_declaration ts in
      ("tools", `List [ `Assoc [ "functionDeclarations", `List func_decls ] ]) :: body
  in
  (* Tool config (tool_choice) *)
  let body =
    match config.tool_choice with
    | Some Auto ->
      ("toolConfig", `Assoc [ "functionCallingConfig", `Assoc [ "mode", `String "AUTO" ] ])
      :: body
    | Some Any ->
      ("toolConfig", `Assoc [ "functionCallingConfig", `Assoc [ "mode", `String "ANY" ] ])
      :: body
    | Some None_ ->
      ("toolConfig", `Assoc [ "functionCallingConfig", `Assoc [ "mode", `String "NONE" ] ])
      :: body
    | Some (Tool name) ->
      ( "toolConfig"
      , `Assoc
          [ ( "functionCallingConfig"
            , `Assoc
                [ "mode", `String "ANY"; "allowedFunctionNames", `List [ `String name ] ]
            )
          ] )
      :: body
    | None -> body
  in
  Request_artifact_internal.create
    ~payload:(Yojson.Safe.to_string (`Assoc body))
    ~output_token_receipt
;;

let build_request ?stream ~config ~messages ?tools () =
  build_request_artifact ?stream ~config ~messages ?tools () |> request_payload
;;

(* ── Parse response ─────────────────────────────────── *)

let parse_response json =
  let open Yojson.Safe.Util in
  match json |> member "error" with
  | `Null | `Assoc [] ->
    let candidates = json |> member "candidates" in
    let candidate =
      match candidates with
      | `List (c :: _) -> c
      | _ -> json (* fallback for unexpected shapes *)
    in
    let content_obj = candidate |> member "content" in
    let parts =
      match content_obj |> member "parts" with
      | `List ps -> ps
      | _ -> []
    in
    let content =
      List.concat_map
        (fun part ->
           let part_thought_signature =
             part |> member "thoughtSignature" |> to_string_option
           in
           match part |> member "text" with
           | `String s ->
             let is_thought = Cli_common_json.member_bool "thought" part in
             let target, block =
               if is_thought
               then Gemini_thought_part, Thinking { signature = None; content = s }
               else Gemini_text_part, Text s
             in
             (match part_thought_signature with
              | Some thought_signature ->
                [ gemini_part_thought_signature_carrier ~target ~thought_signature
                ; block
                ]
              | None -> [ block ])
           | _ ->
             (match part |> member "functionCall" with
              | `Assoc _ as fc ->
                let name = fc |> member "name" |> to_string in
                let args = fc |> member "args" in
                let id =
                  match string_field_opt "id" fc with
                  | Some id -> id
                  | None -> Api_common.fresh_tool_use_id ()
                in
                let tool_use = ToolUse { id; name; input = args } in
                (match part |> member "thoughtSignature" |> to_string_option with
                 | Some thought_signature
                   when not (Api_common.string_is_blank thought_signature) ->
                   [ gemini_thought_signature_carrier ~tool_use_id:id ~thought_signature
                   ; tool_use
                   ]
                 | Some _ | None -> [ tool_use ])
              | _ -> []))
        parts
    in
    let finish_reason =
      candidate
      |> member "finishReason"
      |> to_string_option
      |> Option.value ~default:"STOP"
    in
    let has_tool_use =
      (* N-of-M followup to PR #1519 / #1521 — same content_block
         catch-all that was closed in tool_use_recovery.ml and
         context_reducer_apply.ml. The Gemini backend's stop-reason
         inference uses the same shape and was missed in those sweeps. *)
      List.exists
        (fun (block : Types.content_block) ->
           match block with
           | ToolUse _ -> true
           | Text _
           | Thinking _
           | ReasoningDetails _
           | RedactedThinking _
           | ToolResult _
           | Image _
           | Document _
           | Audio _ -> false)
        content
    in
    let stop_reason =
      if has_tool_use
      then StopToolUse
      else (
        match String.uppercase_ascii finish_reason with
        | "STOP" -> EndTurn
        | "MAX_TOKENS" -> MaxTokens
        | "SAFETY" -> Refusal
        | "RECITATION" -> Refusal
        | other -> Unknown other)
    in
    let usage =
      let um = json |> member "usageMetadata" in
      if um = `Null
      then None
      else
        Some
          { input_tokens = Cli_common_json.member_int "promptTokenCount" um
          ; output_tokens = Cli_common_json.member_int "candidatesTokenCount" um
          ; cache_creation_input_tokens = 0
          ; cache_read_input_tokens =
              Cli_common_json.member_int "cachedContentTokenCount" um
          ; cost_usd = None
          }
    in
    let model_str = Cli_common_json.member_str "modelVersion" json in
    { id = ""; model = model_str; stop_reason; content; usage; telemetry = None }
  | err ->
    let msg =
      err
      |> member "message"
      |> to_string_option
      |> Option.value ~default:"Unknown Gemini API error"
    in
    raise (Gemini_api_error msg)
;;
