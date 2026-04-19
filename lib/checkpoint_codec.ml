open Types
open Checkpoint_types

let checkpoint_version = Checkpoint_types.checkpoint_version

let usage_to_json u =
  `Assoc [
    ("total_input_tokens", `Int u.total_input_tokens);
    ("total_output_tokens", `Int u.total_output_tokens);
    ("total_cache_creation_input_tokens", `Int u.total_cache_creation_input_tokens);
    ("total_cache_read_input_tokens", `Int u.total_cache_read_input_tokens);
    ("api_calls", `Int u.api_calls);
    ("estimated_cost_usd", `Float u.estimated_cost_usd);
  ]

let usage_of_json json =
  let open Yojson.Safe.Util in
  {
    total_input_tokens = json |> member "total_input_tokens" |> to_int;
    total_output_tokens = json |> member "total_output_tokens" |> to_int;
    total_cache_creation_input_tokens =
      json |> member "total_cache_creation_input_tokens" |> to_int_option
      |> Option.value ~default:0;
    total_cache_read_input_tokens =
      json |> member "total_cache_read_input_tokens" |> to_int_option
      |> Option.value ~default:0;
    api_calls =
      json |> member "api_calls" |> to_int_option
      |> Option.value ~default:0;
    estimated_cost_usd =
      (match json |> member "estimated_cost_usd" with
       | `Float f -> f
       | `Int i -> Float.of_int i
       | _ -> 0.0);
  }

let tool_schema_to_json = Types.tool_schema_to_json

let map_str_err r =
  Result.map_error (fun s ->
    Error.Serialization (UnknownVariant { type_name = "param_type"; value = s })) r

let tool_schema_of_json json = map_str_err (Types.tool_schema_of_json json)

let result_all items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok item :: rest -> loop (item :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] items

let content_block_of_json_strict json =
  try
    match Api.content_block_of_json json with
    | Some block -> Ok block
    | None ->
        let open Yojson.Safe.Util in
        let block_type =
          json |> member "type" |> to_string_option |> Option.value ~default:"<missing>"
        in
        Error
          (Error.Serialization
             (JsonParseError
                {
                  detail =
                    Printf.sprintf "Unknown content block type: %s" block_type;
                }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Invalid content block: %s" msg }))
  | Yojson.Json_error msg ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Invalid content block: %s" msg }))
  | Failure msg ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Invalid content block: %s" msg }))

let message_of_json json =
  let open Yojson.Safe.Util in
  let role_str = json |> member "role" |> to_string in
  let role =
    match role_str with
    | "assistant" -> Ok Assistant
    | "user" -> Ok User
    | other ->
        Error
          (Error.Serialization
             (UnknownVariant { type_name = "role"; value = other }))
  in
  let content =
    json |> member "content" |> to_list
    |> List.map content_block_of_json_strict
    |> result_all
  in
  match role, content with
  | Ok role, Ok content -> Ok { role; content; name = None; tool_call_id = None }
  | Error e, _ -> Error e
  | _, Error e -> Error e

let checkpoint_to_json cp =
  `Assoc
    [
      ("version", `Int cp.version);
      ("session_id", `String cp.session_id);
      ("agent_name", `String cp.agent_name);
      ("model", model_to_yojson cp.model);
      ("system_prompt",
       (match cp.system_prompt with Some s -> `String s | None -> `Null));
      ("messages", `List (List.map Api.message_to_json cp.messages));
      ("usage", usage_to_json cp.usage);
      ("turn_count", `Int cp.turn_count);
      ("created_at", `Float cp.created_at);
      ("tools", `List (List.map tool_schema_to_json cp.tools));
      ("tool_choice",
       (match cp.tool_choice with
        | Some tc -> tool_choice_to_json tc
        | None -> `Null));
      ( "temperature",
        Option.value ~default:`Null
          (Option.map (fun v -> `Float v) cp.temperature) );
      ("top_p", Option.value ~default:`Null (Option.map (fun v -> `Float v) cp.top_p));
      ("top_k", Option.value ~default:`Null (Option.map (fun v -> `Int v) cp.top_k));
      ("min_p", Option.value ~default:`Null (Option.map (fun v -> `Float v) cp.min_p));
      ( "enable_thinking",
        Option.value ~default:`Null
          (Option.map (fun v -> `Bool v) cp.enable_thinking) );
      ("response_format_json", `Bool cp.response_format_json);
      ( "thinking_budget",
        Option.value ~default:`Null
          (Option.map (fun v -> `Int v) cp.thinking_budget) );
      ("disable_parallel_tool_use", `Bool cp.disable_parallel_tool_use);
      ("cache_system_prompt", `Bool cp.cache_system_prompt);
      ( "max_input_tokens",
        Option.value ~default:`Null
          (Option.map (fun v -> `Int v) cp.max_input_tokens) );
      ( "max_total_tokens",
        Option.value ~default:`Null
          (Option.map (fun v -> `Int v) cp.max_total_tokens) );
      ("context", Context.to_json cp.context);
      ("mcp_sessions", Mcp_session.info_list_to_json cp.mcp_sessions);
      ("working_context", Option.value ~default:`Null cp.working_context);
    ]

let checkpoint_hash cp =
  Digest.string (Yojson.Safe.to_string (checkpoint_to_json cp)) |> Digest.to_hex

let context_diff_to_json (diff : Context.diff) =
  let kv_pairs name pairs =
    ( name,
      `List
        (List.map
           (fun (key, value) -> `Assoc [ ("key", `String key); ("value", value) ])
           pairs) )
  in
  `Assoc
    [
      kv_pairs "added" diff.added;
      ("removed", `List (List.map (fun key -> `String key) diff.removed));
      kv_pairs "changed" diff.changed;
    ]

let context_diff_of_json json =
  let open Yojson.Safe.Util in
  let parse_kvs name =
    json |> member name |> to_list
    |> List.map (fun item ->
           try Ok (item |> member "key" |> to_string, item |> member "value")
           with
           | Type_error (msg, _) ->
               Error
                 (Error.Serialization
                    (JsonParseError
                       {
                         detail =
                           Printf.sprintf
                             "Checkpoint.delta context diff %s: %s" name msg;
                       })))
    |> result_all
  in
  let parse_removed () =
    try Ok (json |> member "removed" |> to_list |> List.map to_string)
    with
    | Type_error (msg, _) ->
        Error
          (Error.Serialization
             (JsonParseError
                {
                  detail =
                    Printf.sprintf
                      "Checkpoint.delta context diff removed: %s" msg;
                }))
  in
  match parse_kvs "added", parse_kvs "changed", parse_removed () with
  | Ok added, Ok changed, Ok removed ->
      Ok { Context.added; removed; changed }
  | Error e, _, _ -> Error e
  | _, Error e, _ -> Error e
  | _, _, Error e -> Error e

let delta_to_json (delta : delta) =
  let op_to_json = function
    | Replace_identity patch ->
        `Assoc
          [
            ("kind", `String "replace_identity");
            ("session_id", `String patch.session_id);
            ("agent_name", `String patch.agent_name);
            ("model", model_to_yojson patch.model);
            ("created_at", `Float patch.created_at);
          ]
    | Replace_system_prompt prompt ->
        `Assoc
          [
            ("kind", `String "replace_system_prompt");
            ("system_prompt",
             match prompt with Some value -> `String value | None -> `Null);
          ]
    | Splice_messages splice ->
        `Assoc
          [
            ("kind", `String "splice_messages");
            ("start_index", `Int splice.start_index);
            ("delete_count", `Int splice.delete_count);
            ("insert", `List (List.map Api.message_to_json splice.insert));
          ]
    | Replace_usage usage ->
        `Assoc [ ("kind", `String "replace_usage"); ("usage", usage_to_json usage) ]
    | Replace_turn_count turn_count ->
        `Assoc [ ("kind", `String "replace_turn_count"); ("turn_count", `Int turn_count) ]
    | Replace_tools tools ->
        `Assoc
          [
            ("kind", `String "replace_tools");
            ("tools", `List (List.map tool_schema_to_json tools));
          ]
    | Replace_tool_choice tool_choice ->
        `Assoc
          [
            ("kind", `String "replace_tool_choice");
            ("tool_choice",
             match tool_choice with
             | Some choice -> tool_choice_to_json choice
             | None -> `Null);
          ]
    | Replace_sampling patch ->
        `Assoc
          [
            ("kind", `String "replace_sampling");
            ("temperature",
             Option.value ~default:`Null
               (Option.map (fun value -> `Float value) patch.temperature));
            ("top_p",
             Option.value ~default:`Null
               (Option.map (fun value -> `Float value) patch.top_p));
            ("top_k",
             Option.value ~default:`Null
               (Option.map (fun value -> `Int value) patch.top_k));
            ("min_p",
             Option.value ~default:`Null
               (Option.map (fun value -> `Float value) patch.min_p));
            ("enable_thinking",
             Option.value ~default:`Null
               (Option.map (fun value -> `Bool value) patch.enable_thinking));
            ("thinking_budget",
             Option.value ~default:`Null
               (Option.map (fun value -> `Int value) patch.thinking_budget));
          ]
    | Replace_limits patch ->
        `Assoc
          [
            ("kind", `String "replace_limits");
            ("disable_parallel_tool_use", `Bool patch.disable_parallel_tool_use);
            ("response_format_json", `Bool patch.response_format_json);
            ("cache_system_prompt", `Bool patch.cache_system_prompt);
            ("max_input_tokens",
             Option.value ~default:`Null
               (Option.map (fun value -> `Int value) patch.max_input_tokens));
            ("max_total_tokens",
             Option.value ~default:`Null
               (Option.map (fun value -> `Int value) patch.max_total_tokens));
          ]
    | Patch_context diff ->
        `Assoc [ ("kind", `String "patch_context"); ("diff", context_diff_to_json diff) ]
    | Replace_mcp_sessions sessions ->
        `Assoc
          [
            ("kind", `String "replace_mcp_sessions");
            ("mcp_sessions", Mcp_session.info_list_to_json sessions);
          ]
    | Replace_working_context working_context ->
        `Assoc
          [
            ("kind", `String "replace_working_context");
            ("working_context", Option.value ~default:`Null working_context);
          ]
  in
  `Assoc
    [
      ("delta_version", `Int delta.delta_version);
      ("base_checkpoint_version", `Int delta.base_checkpoint_version);
      ("base_checkpoint_hash", `String delta.base_checkpoint_hash);
      ("result_checkpoint_hash", `String delta.result_checkpoint_hash);
      ("created_at", `Float delta.created_at);
      ("operations", `List (List.map op_to_json delta.operations));
    ]

let delta_of_json json =
  let open Yojson.Safe.Util in
  let op_of_json op_json =
    let kind = op_json |> member "kind" |> to_string in
    match kind with
    | "replace_identity" ->
        let model =
          model_of_yojson (op_json |> member "model")
          |> Result.map_error (fun e ->
                 Error.Serialization (JsonParseError { detail = e }))
        in
        Result.map
          (fun model ->
            Replace_identity
              {
                session_id = op_json |> member "session_id" |> to_string;
                agent_name = op_json |> member "agent_name" |> to_string;
                model;
                created_at = op_json |> member "created_at" |> to_float;
              })
          model
    | "replace_system_prompt" ->
        Ok
          (Replace_system_prompt
             (op_json |> member "system_prompt" |> to_string_option))
    | "splice_messages" ->
        let insert =
          op_json |> member "insert" |> to_list |> List.map message_of_json |> result_all
        in
        Result.map
          (fun insert ->
            Splice_messages
              {
                start_index = op_json |> member "start_index" |> to_int;
                delete_count = op_json |> member "delete_count" |> to_int;
                insert;
              })
          insert
    | "replace_usage" ->
        Ok (Replace_usage (usage_of_json (op_json |> member "usage")))
    | "replace_turn_count" ->
        Ok (Replace_turn_count (op_json |> member "turn_count" |> to_int))
    | "replace_tools" ->
        let tools =
          op_json |> member "tools" |> to_list |> List.map tool_schema_of_json |> result_all
        in
        Result.map (fun tools -> Replace_tools tools) tools
    | "replace_tool_choice" ->
        (match op_json |> member "tool_choice" with
         | `Null -> Ok (Replace_tool_choice None)
         | value ->
             tool_choice_of_json value
             |> Result.map (fun value -> Replace_tool_choice (Some value)))
    | "replace_sampling" ->
        Ok
          (Replace_sampling
             {
               temperature = op_json |> member "temperature" |> to_float_option;
               top_p = op_json |> member "top_p" |> to_float_option;
               top_k = op_json |> member "top_k" |> to_int_option;
               min_p = op_json |> member "min_p" |> to_float_option;
               enable_thinking = op_json |> member "enable_thinking" |> to_bool_option;
               thinking_budget = op_json |> member "thinking_budget" |> to_int_option;
             })
    | "replace_limits" ->
        Ok
          (Replace_limits
             {
               disable_parallel_tool_use =
                 op_json |> member "disable_parallel_tool_use" |> to_bool;
               response_format_json =
                 op_json |> member "response_format_json" |> to_bool;
               cache_system_prompt =
                 op_json |> member "cache_system_prompt" |> to_bool;
               max_input_tokens =
                 op_json |> member "max_input_tokens" |> to_int_option;
               max_total_tokens =
                 op_json |> member "max_total_tokens" |> to_int_option;
             })
    | "patch_context" ->
        context_diff_of_json (op_json |> member "diff")
        |> Result.map (fun diff -> Patch_context diff)
    | "replace_mcp_sessions" ->
        Mcp_session.info_list_of_json (op_json |> member "mcp_sessions")
        |> Result.map (fun sessions -> Replace_mcp_sessions sessions)
    | "replace_working_context" ->
        Ok
          (Replace_working_context
             (match op_json |> member "working_context" with
              | `Null -> None
              | value -> Some value))
    | other ->
        Error
          (Error.Serialization
             (UnknownVariant
                { type_name = "Checkpoint.delta_op"; value = other }))
  in
  try
    let operations =
      json |> member "operations" |> to_list |> List.map op_of_json |> result_all
    in
    Result.map
      (fun operations ->
        {
          delta_version = json |> member "delta_version" |> to_int;
          base_checkpoint_version =
            json |> member "base_checkpoint_version" |> to_int;
          base_checkpoint_hash =
            json |> member "base_checkpoint_hash" |> to_string;
          result_checkpoint_hash =
            json |> member "result_checkpoint_hash" |> to_string;
          created_at = json |> member "created_at" |> to_float;
          operations;
        })
      operations
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Checkpoint.delta_of_json: %s" msg }))
  | Yojson.Json_error msg ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Checkpoint.delta_of_json: %s" msg }))

let to_json = checkpoint_to_json

let of_json json =
  try
    let open Yojson.Safe.Util in
    let version = json |> member "version" |> to_int in
    if version <> checkpoint_version && version <> 3 && version <> 2 && version <> 1 then
      Error
        (Error.Serialization
           (VersionMismatch { expected = checkpoint_version; got = version }))
    else
      let tool_choice =
        match json |> member "tool_choice" with
        | `Null -> Ok None
        | tc -> Result.map Option.some (tool_choice_of_json tc)
      in
      match tool_choice with
      | Error e -> Error e
      | Ok tool_choice ->
          let model =
            model_of_yojson (json |> member "model")
            |> Result.map_error (fun e ->
                   Error.Serialization (JsonParseError { detail = e }))
          in
          let messages =
            json |> member "messages" |> to_list |> List.map message_of_json |> result_all
          in
          let tools =
            json |> member "tools" |> to_list |> List.map tool_schema_of_json |> result_all
          in
          let context =
            match json |> member "context" with
            | `Null -> Ok (Context.create ())
            | `Assoc _ as value -> Ok (Context.of_json value)
            | _ ->
                Error
                  (Error.Serialization
                     (JsonParseError
                        {
                          detail =
                            "Checkpoint.of_json: context must be a JSON object or null";
                        }))
          in
          let mcp_sessions =
            match json |> member "mcp_sessions" with
            | `Null -> Ok []
            | `List _ as lst -> Mcp_session.info_list_of_json lst
            | _ ->
                Error
                  (Error.Serialization
                     (JsonParseError
                        {
                          detail =
                            "Checkpoint.of_json: mcp_sessions must be a JSON array or null";
                        }))
          in
          let working_context =
            match json |> member "working_context" with
            | `Null -> None
            | v -> Some v
          in
          (match model, messages, tools, context, mcp_sessions with
           | Ok model, Ok messages, Ok tools, Ok context, Ok mcp_sessions ->
               Ok
                 {
                   version = checkpoint_version;
                   session_id = json |> member "session_id" |> to_string;
                   agent_name = json |> member "agent_name" |> to_string;
                   model;
                   system_prompt = json |> member "system_prompt" |> to_string_option;
                   messages;
                   usage = json |> member "usage" |> usage_of_json;
                   turn_count = json |> member "turn_count" |> to_int;
                   created_at = json |> member "created_at" |> to_float;
                   tools;
                   tool_choice;
                   disable_parallel_tool_use =
                     json |> member "disable_parallel_tool_use" |> to_bool_option
                     |> Option.value ~default:false;
                   temperature = json |> member "temperature" |> to_float_option;
                   top_p = json |> member "top_p" |> to_float_option;
                   top_k = json |> member "top_k" |> to_int_option;
                   min_p = json |> member "min_p" |> to_float_option;
                   enable_thinking = json |> member "enable_thinking" |> to_bool_option;
                   response_format_json =
                     json |> member "response_format_json" |> to_bool_option
                     |> Option.value ~default:false;
                   thinking_budget = json |> member "thinking_budget" |> to_int_option;
                   cache_system_prompt =
                     json |> member "cache_system_prompt" |> to_bool_option
                     |> Option.value ~default:false;
                   max_input_tokens = json |> member "max_input_tokens" |> to_int_option;
                   max_total_tokens = json |> member "max_total_tokens" |> to_int_option;
                   context;
                   mcp_sessions;
                   working_context;
                 }
           | Error e, _, _, _, _ -> Error e
           | _, Error e, _, _, _ -> Error e
           | _, _, Error e, _, _ -> Error e
           | _, _, _, Error e, _ -> Error e
           | _, _, _, _, Error e -> Error e)
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Checkpoint.of_json: %s" msg }))
  | Yojson.Json_error msg ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Checkpoint.of_json: %s" msg }))
  | Failure msg ->
      Error
        (Error.Serialization
           (JsonParseError
              { detail = Printf.sprintf "Checkpoint.of_json: %s" msg }))

let to_string cp =
  to_json cp |> Yojson.Safe.to_string

let of_string s =
  try
    let json = Yojson.Safe.from_string s in
    of_json json
  with Yojson.Json_error msg ->
    Error
      (Error.Serialization
         (JsonParseError
            { detail = Printf.sprintf "Invalid JSON: %s" msg }))
