(** Agent state checkpoint — versioned JSON serialization.

    Captures the full conversation state (messages, usage, config) as a
    pure value for callers that want to persist and later restore agent
    state.  This module only handles serialization; file I/O and resume
    orchestration are left to the caller. *)

open Types

let checkpoint_version = 4

type t = {
  version: int;
  session_id: string;
  agent_name: string;
  model: model;
  system_prompt: string option;
  messages: message list;
  usage: usage_stats;
  turn_count: int;
  created_at: float;
  tools: tool_schema list;
  tool_choice: tool_choice option;
  disable_parallel_tool_use: bool;
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  response_format_json: bool;
  thinking_budget: int option;
  cache_system_prompt: bool;
  max_input_tokens: int option;
  max_total_tokens: int option;
  context: Context.t;
  mcp_sessions: Mcp_session.info list;
  working_context: Yojson.Safe.t option;
}

type message_splice = {
  start_index: int;
  delete_count: int;
  insert: message list;
}

type identity_patch = {
  session_id: string;
  agent_name: string;
  model: model;
  created_at: float;
}

type sampling_patch = {
  temperature: float option;
  top_p: float option;
  top_k: int option;
  min_p: float option;
  enable_thinking: bool option;
  thinking_budget: int option;
}

type limits_patch = {
  disable_parallel_tool_use: bool;
  response_format_json: bool;
  cache_system_prompt: bool;
  max_input_tokens: int option;
  max_total_tokens: int option;
}

type delta_op =
  | Replace_identity of identity_patch
  | Replace_system_prompt of string option
  | Splice_messages of message_splice
  | Replace_usage of usage_stats
  | Replace_turn_count of int
  | Replace_tools of tool_schema list
  | Replace_tool_choice of tool_choice option
  | Replace_sampling of sampling_patch
  | Replace_limits of limits_patch
  | Patch_context of Context.diff
  | Replace_mcp_sessions of Mcp_session.info list
  | Replace_working_context of Yojson.Safe.t option

type delta = {
  delta_version: int;
  base_checkpoint_version: int;
  base_checkpoint_hash: string;
  result_checkpoint_hash: string;
  created_at: float;
  operations: delta_op list;
}

type delta_restore_mode =
  | Delta_applied
  | Full_restore

type delta_restore_result = {
  checkpoint: t;
  mode: delta_restore_mode;
}

(* ── Serialization helpers ──────────────────────────────────────── *)

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

let result_all items =
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | Ok item :: rest -> loop (item :: acc) rest
    | Error e :: _ -> Error e
  in
  loop [] items

let tool_param_to_json (p : tool_param) =
  `Assoc [
    ("name", `String p.name);
    ("description", `String p.description);
    ("param_type", `String (param_type_to_string p.param_type));
    ("required", `Bool p.required);
  ]

let tool_param_of_json json =
  let open Yojson.Safe.Util in
  let type_str = json |> member "param_type" |> to_string in
  let param_type =
    match type_str with
    | "string" -> Ok String
    | "integer" -> Ok Integer
    | "number" -> Ok Number
    | "boolean" -> Ok Boolean
    | "array" -> Ok Array
    | "object" -> Ok Object
    | other -> Error (Error.Serialization (UnknownVariant { type_name = "param_type"; value = other }))
  in
  Result.map
    (fun param_type ->
      {
        name = json |> member "name" |> to_string;
        description = json |> member "description" |> to_string;
        param_type;
        required = json |> member "required" |> to_bool;
      })
    param_type

let tool_schema_to_json (s : tool_schema) =
  `Assoc [
    ("name", `String s.name);
    ("description", `String s.description);
    ("parameters", `List (List.map tool_param_to_json s.parameters));
  ]

let tool_schema_of_json json =
  let open Yojson.Safe.Util in
  let parameters =
    json |> member "parameters" |> to_list |> List.map tool_param_of_json |> result_all
  in
  Result.map
    (fun parameters ->
      {
        name = json |> member "name" |> to_string;
        description = json |> member "description" |> to_string;
        parameters;
      })
    parameters

let content_block_of_json_strict json =
  try
    match Api.content_block_of_json json with
    | Some block -> Ok block
    | None ->
        let open Yojson.Safe.Util in
        let block_type =
          json |> member "type" |> to_string_option |> Option.value ~default:"<missing>"
        in
        Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Unknown content block type: %s" block_type }))
  with
  | Yojson.Safe.Util.Type_error (msg, _) ->
      Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Invalid content block: %s" msg }))
  | Yojson.Json_error msg ->
      Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Invalid content block: %s" msg }))
  | Failure msg ->
      Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Invalid content block: %s" msg }))

let message_of_json json =
  let open Yojson.Safe.Util in
  let role_str = json |> member "role" |> to_string in
  let role =
    match role_str with
    | "assistant" -> Ok Assistant
    | "user" -> Ok User
    | other -> Error (Error.Serialization (UnknownVariant { type_name = "role"; value = other }))
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

let context_diff_is_empty diff =
  diff.Context.added = [] && diff.removed = [] && diff.changed = []

let list_sub lst start len =
  if len <= 0 then []
  else
    let rec drop n rest =
      if n <= 0 then rest
      else
        match rest with
        | [] -> []
        | _ :: tail -> drop (n - 1) tail
    in
    let rec take n rest acc =
      if n <= 0 then List.rev acc
      else
        match rest with
        | [] -> List.rev acc
        | item :: tail -> take (n - 1) tail (item :: acc)
    in
    take len (drop start lst) []

let common_prefix_len left right =
  let rec loop n = function
    | x :: xs, y :: ys when x = y -> loop (n + 1) (xs, ys)
    | _ -> n
  in
  loop 0 (left, right)

let common_suffix_len left right prefix_len =
  let left_arr = Array.of_list left in
  let right_arr = Array.of_list right in
  let max_len =
    min (Array.length left_arr - prefix_len) (Array.length right_arr - prefix_len)
  in
  let rec loop n =
    if n >= max_len then n
    else
      let left_idx = Array.length left_arr - 1 - n in
      let right_idx = Array.length right_arr - 1 - n in
      if left_arr.(left_idx) = right_arr.(right_idx) then loop (n + 1) else n
  in
  loop 0

let compute_message_splice before after =
  let prefix_len = common_prefix_len before after in
  let suffix_len = common_suffix_len before after prefix_len in
  let before_mid_len = List.length before - prefix_len - suffix_len in
  let after_mid_len = List.length after - prefix_len - suffix_len in
  {
    start_index = prefix_len;
    delete_count = max 0 before_mid_len;
    insert = list_sub after prefix_len (max 0 after_mid_len);
  }

let apply_message_splice messages splice =
  let total = List.length messages in
  if splice.start_index < 0 || splice.delete_count < 0
     || splice.start_index > total
     || splice.start_index + splice.delete_count > total then
    Error
      (Error.Io
         (ValidationFailed
            {
              detail =
                Printf.sprintf
                  "Invalid message splice: start=%d delete=%d total=%d"
                  splice.start_index splice.delete_count total;
            }))
  else
    let prefix = list_sub messages 0 splice.start_index in
    let suffix =
      list_sub messages
        (splice.start_index + splice.delete_count)
        (total - splice.start_index - splice.delete_count)
    in
    Ok (prefix @ splice.insert @ suffix)

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
    Ok
      {
        Context.added;
        removed;
        changed;
      }
  | Error e, _, _ -> Error e
  | _, Error e, _ -> Error e
  | _, _, Error e -> Error e

let sampling_patch_changed (before : t) (after : t) =
  before.temperature <> after.temperature
  || before.top_p <> after.top_p
  || before.top_k <> after.top_k
  || before.min_p <> after.min_p
  || before.enable_thinking <> after.enable_thinking
  || before.thinking_budget <> after.thinking_budget

let limits_patch_changed (before : t) (after : t) =
  before.disable_parallel_tool_use <> after.disable_parallel_tool_use
  || before.response_format_json <> after.response_format_json
  || before.cache_system_prompt <> after.cache_system_prompt
  || before.max_input_tokens <> after.max_input_tokens
  || before.max_total_tokens <> after.max_total_tokens

let delta_version = 1

let delta_metrics_names =
  ( "checkpoint_delta_apply_total",
    "checkpoint_delta_apply_failures_total",
    "checkpoint_delta_size_bytes",
    "checkpoint_full_restore_fallback_total" )

let is_truthy = function
  | Some ("1" | "true" | "TRUE" | "yes" | "on") -> true
  | _ -> false

let delta_enabled () =
  (* Prefer OAS_DELTA_CHECKPOINT; fall back to deprecated MASC_DELTA_CHECKPOINT
     for backward compatibility during the migration window. *)
  is_truthy (Sys.getenv_opt "OAS_DELTA_CHECKPOINT")
  || is_truthy (Sys.getenv_opt "MASC_DELTA_CHECKPOINT")

let register_delta_metrics metrics =
  let apply_total_name, apply_failures_name, size_name, fallback_name =
    delta_metrics_names
  in
  ( Metrics.counter metrics ~name:apply_total_name ~unit_:"1",
    Metrics.counter metrics ~name:apply_failures_name ~unit_:"1",
    Metrics.histogram metrics ~name:size_name
      ~buckets:[ 128.; 512.; 1024.; 4096.; 16384.; 65536. ],
    Metrics.counter metrics ~name:fallback_name ~unit_:"1" )

let delta_failure_rate_exceeded metrics =
  let apply_total, apply_failures, _, _ = register_delta_metrics metrics in
  let total = Metrics.counter_value apply_total () in
  if total = 0 then false
  else
    let failures = Metrics.counter_value apply_failures () in
    (float_of_int failures /. float_of_int total) > 0.05

let apply_context_patch ctx diff =
  let next = Context.copy ctx in
  List.iter (fun key -> Context.delete next key) diff.Context.removed;
  List.iter (fun (key, value) -> Context.set next key value) diff.Context.added;
  List.iter (fun (key, value) -> Context.set next key value) diff.Context.changed;
  next

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
        |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e }))
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

let compute_delta (before : t) (after : t) =
  let operations = ref [] in
  let push op = operations := op :: !operations in
  if
    before.session_id <> after.session_id
    || before.agent_name <> after.agent_name
    || before.model <> after.model
    || before.created_at <> after.created_at
  then
    push
      (Replace_identity
         {
           session_id = after.session_id;
           agent_name = after.agent_name;
           model = after.model;
           created_at = after.created_at;
         });
  if before.system_prompt <> after.system_prompt then
    push (Replace_system_prompt after.system_prompt);
  let message_splice = compute_message_splice before.messages after.messages in
  if message_splice.delete_count <> 0 || message_splice.insert <> [] then
    push (Splice_messages message_splice);
  if before.usage <> after.usage then push (Replace_usage after.usage);
  if before.turn_count <> after.turn_count then
    push (Replace_turn_count after.turn_count);
  if before.tools <> after.tools then push (Replace_tools after.tools);
  if before.tool_choice <> after.tool_choice then
    push (Replace_tool_choice after.tool_choice);
  if sampling_patch_changed before after then
    push
      (Replace_sampling
         {
           temperature = after.temperature;
           top_p = after.top_p;
           top_k = after.top_k;
           min_p = after.min_p;
           enable_thinking = after.enable_thinking;
           thinking_budget = after.thinking_budget;
         });
  if limits_patch_changed before after then
    push
      (Replace_limits
         {
           disable_parallel_tool_use = after.disable_parallel_tool_use;
           response_format_json = after.response_format_json;
           cache_system_prompt = after.cache_system_prompt;
           max_input_tokens = after.max_input_tokens;
           max_total_tokens = after.max_total_tokens;
         });
  let context_diff = Context.diff before.context after.context in
  if not (context_diff_is_empty context_diff) then push (Patch_context context_diff);
  if before.mcp_sessions <> after.mcp_sessions then
    push (Replace_mcp_sessions after.mcp_sessions);
  if before.working_context <> after.working_context then
    push (Replace_working_context after.working_context);
  {
    delta_version;
    base_checkpoint_version = checkpoint_version;
    base_checkpoint_hash = checkpoint_hash before;
    result_checkpoint_hash = checkpoint_hash after;
    created_at = after.created_at;
    operations = List.rev !operations;
  }

let apply_delta base delta =
  if delta.delta_version <> delta_version then
    Error
      (Error.Serialization
         (VersionMismatch { expected = delta_version; got = delta.delta_version }))
  else if delta.base_checkpoint_version <> checkpoint_version then
    Error
      (Error.Serialization
         (VersionMismatch
            {
              expected = checkpoint_version;
              got = delta.base_checkpoint_version;
            }))
  else if checkpoint_hash base <> delta.base_checkpoint_hash then
    Error
      (Error.Io
         (ValidationFailed
            { detail = "Checkpoint.delta base hash mismatch" }))
  else
    let apply_op (checkpoint : t) (op : delta_op) =
      match op with
      | Replace_identity (patch : identity_patch) ->
        Ok
          {
            checkpoint with
            session_id = patch.session_id;
            agent_name = patch.agent_name;
            model = patch.model;
            created_at = patch.created_at;
          }
      | Replace_system_prompt system_prompt ->
        Ok { checkpoint with system_prompt }
      | Splice_messages splice ->
        apply_message_splice checkpoint.messages splice
        |> Result.map (fun messages -> { checkpoint with messages })
      | Replace_usage usage -> Ok { checkpoint with usage }
      | Replace_turn_count turn_count -> Ok { checkpoint with turn_count }
      | Replace_tools tools -> Ok { checkpoint with tools }
      | Replace_tool_choice tool_choice -> Ok { checkpoint with tool_choice }
      | Replace_sampling (patch : sampling_patch) ->
        Ok
          {
            checkpoint with
            temperature = patch.temperature;
            top_p = patch.top_p;
            top_k = patch.top_k;
            min_p = patch.min_p;
            enable_thinking = patch.enable_thinking;
            thinking_budget = patch.thinking_budget;
          }
      | Replace_limits (patch : limits_patch) ->
        Ok
          {
            checkpoint with
            disable_parallel_tool_use = patch.disable_parallel_tool_use;
            response_format_json = patch.response_format_json;
            cache_system_prompt = patch.cache_system_prompt;
            max_input_tokens = patch.max_input_tokens;
            max_total_tokens = patch.max_total_tokens;
          }
      | Patch_context diff ->
        Ok { checkpoint with context = apply_context_patch checkpoint.context diff }
      | Replace_mcp_sessions mcp_sessions ->
        Ok { checkpoint with mcp_sessions }
      | Replace_working_context working_context ->
        Ok { checkpoint with working_context }
    in
    let initial = { base with context = Context.copy base.context } in
    let result =
      List.fold_left
        (fun acc op -> Result.bind acc (fun checkpoint -> apply_op checkpoint op))
        (Ok initial) delta.operations
    in
    Result.bind result (fun checkpoint ->
        if checkpoint_hash checkpoint <> delta.result_checkpoint_hash then
          Error
            (Error.Io
               (ValidationFailed
                  { detail = "Checkpoint.delta result hash mismatch" }))
        else Ok checkpoint)

let restore_with_delta_fallback ?metrics ~base ~delta ~full_checkpoint () =
  let fallback () =
    Option.iter
      (fun metrics ->
        let _, _, _, fallback_total = register_delta_metrics metrics in
        Metrics.incr fallback_total 1)
      metrics;
    Ok { checkpoint = full_checkpoint; mode = Full_restore }
  in
  if not (delta_enabled ()) then
    fallback ()
  else if
    match metrics with
    | Some metrics -> delta_failure_rate_exceeded metrics
    | None -> false
  then
    fallback ()
  else (
    Option.iter
      (fun metrics ->
        let apply_total, _, size_histogram, _ = register_delta_metrics metrics in
        Metrics.incr apply_total 1;
        let payload_len = String.length (Yojson.Safe.to_string (delta_to_json delta)) in
        Metrics.observe size_histogram (float_of_int payload_len))
      metrics;
    match apply_delta base delta with
    | Ok checkpoint -> Ok { checkpoint; mode = Delta_applied }
    | Error _ ->
      Option.iter
        (fun metrics ->
          let _, apply_failures, _, fallback_total = register_delta_metrics metrics in
          Metrics.incr apply_failures 1;
          Metrics.incr fallback_total 1)
        metrics;
      Ok { checkpoint = full_checkpoint; mode = Full_restore })

(* ── Public API ─────────────────────────────────────────────────── *)

let to_json = checkpoint_to_json

let of_json json =
  try
    let open Yojson.Safe.Util in
    let version = json |> member "version" |> to_int in
    if version <> checkpoint_version && version <> 3 && version <> 2 && version <> 1 then
      Error (Error.Serialization (VersionMismatch { expected = checkpoint_version; got = version }))
    else
      let tool_choice =
        match json |> member "tool_choice" with
        | `Null -> Ok None
        | tc -> Result.map Option.some (tool_choice_of_json tc)
      in
      match tool_choice with
      | Error e -> Error e
      | Ok tool_choice ->
        let model = model_of_yojson (json |> member "model")
          |> Result.map_error (fun e -> Error.Serialization (JsonParseError { detail = e })) in
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
                      { detail = "Checkpoint.of_json: context must be a JSON object or null" }))
        in
        let mcp_sessions =
          match json |> member "mcp_sessions" with
          | `Null -> Ok []  (* v1 backward compatibility *)
          | `List _ as lst -> Mcp_session.info_list_of_json lst
          | _ -> Error (Error.Serialization (JsonParseError { detail = "Checkpoint.of_json: mcp_sessions must be a JSON array or null" }))
        in
        let working_context =
          match json |> member "working_context" with
          | `Null -> None
          | v -> Some v
        in
        (match model, messages, tools, context, mcp_sessions with
         | Ok model, Ok messages, Ok tools, Ok context, Ok mcp_sessions ->
             Ok {
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
                 json |> member "response_format_json" |> to_bool_option |> Option.value ~default:false;
               thinking_budget = json |> member "thinking_budget" |> to_int_option;
               cache_system_prompt =
                 json |> member "cache_system_prompt" |> to_bool_option |> Option.value ~default:false;
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
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Checkpoint.of_json: %s" msg }))
  | Yojson.Json_error msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Checkpoint.of_json: %s" msg }))
  | Failure msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Checkpoint.of_json: %s" msg }))

let to_string cp =
  to_json cp |> Yojson.Safe.to_string

let of_string s =
  try
    let json = Yojson.Safe.from_string s in
    of_json json
  with Yojson.Json_error msg ->
    Error (Error.Serialization (JsonParseError { detail = Printf.sprintf "Invalid JSON: %s" msg }))

let message_count cp = List.length cp.messages

let token_usage cp = cp.usage
