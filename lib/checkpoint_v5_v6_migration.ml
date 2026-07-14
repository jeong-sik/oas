(** Finite persistence migration from the released checkpoint-v5/v6 JSON
    schemas to the exact checkpoint-v8 JSON schema. This module does not widen
    the current checkpoint domain: only documents emitted by the v5/v6
    serializers are accepted. The shared validators also enforce the exact
    nested v8 persistence schema before the current decoder runs. *)

open Result_syntax

let target_version = 8

type schema =
  | Released_v5_pre_preserve_capped
  | Released_v5_preserve_capped
  | Released_v5_preserve_unbounded
  | Released_v6
  | Current_v8

let source_version_supported = function
  | 5 | 6 -> true
  | _ -> false
;;

let schema_version = function
  | Released_v5_pre_preserve_capped
  | Released_v5_preserve_capped
  | Released_v5_preserve_unbounded -> 5
  | Released_v6 -> 6
  | Current_v8 -> target_version
;;

let schema_scope = function
  | Released_v5_pre_preserve_capped -> "Checkpoint v5 pre-preserve capped"
  | Released_v5_preserve_capped -> "Checkpoint v5 preserve capped"
  | Released_v5_preserve_unbounded -> "Checkpoint v5 preserve unbounded"
  | Released_v6 -> "Checkpoint v6"
  | Current_v8 -> "Checkpoint v8"
;;

let is_released_v5 = function
  | Released_v5_pre_preserve_capped
  | Released_v5_preserve_capped
  | Released_v5_preserve_unbounded -> true
  | Released_v6 | Current_v8 -> false
;;

let json_errorf format =
  Printf.ksprintf
    (fun detail -> Error (Error.Serialization (JsonParseError { detail })))
    format
;;

let result_all items =
  List.fold_left
    (fun acc item ->
       let* values = acc in
       let* value = item in
       Ok (value :: values))
    (Ok [])
    items
  |> Result.map List.rev
;;

let duplicate_names names =
  names
  |> List.sort String.compare
  |> List.fold_left
       (fun (previous, duplicates) name ->
          match previous with
          | Some previous when String.equal previous name -> Some name, name :: duplicates
          | Some _ | None -> Some name, duplicates)
       (None, [])
  |> snd
  |> List.sort_uniq String.compare
;;

let object_fields_without_duplicates ~scope = function
  | `Assoc fields ->
    let duplicates = duplicate_names (List.map fst fields) in
    if duplicates = []
    then Ok fields
    else json_errorf "%s duplicates fields [%s]" scope (String.concat "," duplicates)
  | _ -> json_errorf "%s must be a JSON object" scope
;;

let validate_object_shape ~scope ~required ~optional = function
  | `Assoc fields ->
    let names = List.map fst fields in
    let duplicates = duplicate_names names in
    let missing = List.filter (fun name -> not (List.mem name names)) required in
    let expected = required @ optional in
    let unknown = List.filter (fun name -> not (List.mem name expected)) names in
    if duplicates = [] && missing = [] && unknown = []
    then Ok fields
    else
      json_errorf
        "%s schema mismatch (missing=[%s], unknown=[%s], duplicate=[%s])"
        scope
        (String.concat "," missing)
        (String.concat "," unknown)
        (String.concat "," duplicates)
  | _ -> json_errorf "%s must be a JSON object" scope
;;

let required_field ~scope name fields =
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> json_errorf "%s is missing field %s" scope name
;;

let replace_field name value fields =
  List.map
    (fun (field_name, field_value) ->
       if String.equal name field_name then field_name, value else field_name, field_value)
    fields
;;

let validate_string ~scope = function
  | `String _ -> Ok ()
  | _ -> json_errorf "%s must be a string" scope
;;

let validate_bool ~scope = function
  | `Bool _ -> Ok ()
  | _ -> json_errorf "%s must be a boolean" scope
;;

let validate_int ~scope = function
  | `Int _ -> Ok ()
  | _ -> json_errorf "%s must be an integer" scope
;;

let validate_float ~scope = function
  | `Float _ -> Ok ()
  | _ -> json_errorf "%s must be a float" scope
;;

let validate_optional ~scope validate = function
  | `Null -> Ok ()
  | value -> validate ~scope value
;;

let validate_string_value ~scope ~allowed = function
  | `String value when List.mem value allowed -> Ok ()
  | `String value -> json_errorf "%s has unsupported value %S" scope value
  | _ -> json_errorf "%s must be a string" scope
;;

let validate_list ~scope validate = function
  | `List values ->
    values
    |> List.mapi (fun index value ->
      validate ~scope:(Printf.sprintf "%s[%d]" scope index) value)
    |> result_all
    |> Result.map (fun _ -> ())
  | _ -> json_errorf "%s must be an array" scope
;;

let validate_unique_object ~scope = function
  | `Assoc fields ->
    let duplicates = duplicate_names (List.map fst fields) in
    if duplicates = []
    then Ok ()
    else json_errorf "%s duplicates fields [%s]" scope (String.concat "," duplicates)
  | _ -> json_errorf "%s must be a JSON object" scope
;;

let validate_env_pair ~scope json =
  let* fields =
    validate_object_shape ~scope ~required:[ "key"; "value" ] ~optional:[] json
  in
  let* key = required_field ~scope "key" fields in
  let* value = required_field ~scope "value" fields in
  let* () = validate_string ~scope:(scope ^ ".key") key in
  validate_string ~scope:(scope ^ ".value") value
;;

let validate_tool_param ~scope json =
  let* fields =
    validate_object_shape
      ~scope
      ~required:[ "name"; "description"; "param_type"; "required" ]
      ~optional:[]
      json
  in
  let* name = required_field ~scope "name" fields in
  let* description = required_field ~scope "description" fields in
  let* param_type = required_field ~scope "param_type" fields in
  let* required = required_field ~scope "required" fields in
  let* () = validate_string ~scope:(scope ^ ".name") name in
  let* () = validate_string ~scope:(scope ^ ".description") description in
  let* () =
    validate_string_value
      ~scope:(scope ^ ".param_type")
      ~allowed:[ "string"; "integer"; "number"; "boolean"; "array"; "object" ]
      param_type
  in
  validate_bool ~scope:(scope ^ ".required") required
;;

let validate_tool_schema ~scope json =
  let* fields =
    validate_object_shape
      ~scope
      ~required:[ "name"; "description"; "parameters" ]
      ~optional:[ "strict" ]
      json
  in
  let* name = required_field ~scope "name" fields in
  let* description = required_field ~scope "description" fields in
  let* parameters = required_field ~scope "parameters" fields in
  let* () = validate_string ~scope:(scope ^ ".name") name in
  let* () = validate_string ~scope:(scope ^ ".description") description in
  let* () = validate_list ~scope:(scope ^ ".parameters") validate_tool_param parameters in
  match List.assoc_opt "strict" fields with
  | None -> Ok ()
  | Some strict -> validate_bool ~scope:(scope ^ ".strict") strict
;;

let validate_tool_choice ~scope = function
  | `Null -> Ok ()
  | `Assoc fields as json ->
    let type_values =
      List.filter_map
        (fun (name, value) -> if String.equal name "type" then Some value else None)
        fields
    in
    (match type_values with
     | [ `String "auto" ] | [ `String "any" ] | [ `String "none" ] ->
       validate_object_shape ~scope ~required:[ "type" ] ~optional:[] json
       |> Result.map (fun _ -> ())
     | [ `String "tool" ] ->
       let* fields =
         validate_object_shape ~scope ~required:[ "type"; "name" ] ~optional:[] json
       in
       let* name = required_field ~scope "name" fields in
       validate_string ~scope:(scope ^ ".name") name
     | [ `String value ] -> json_errorf "%s has unsupported type %S" scope value
     | [ _ ] -> json_errorf "%s.type must be a string" scope
     | [] -> json_errorf "%s is missing field type" scope
     | _ -> json_errorf "%s duplicates field type" scope)
  | _ -> json_errorf "%s must be null or a JSON object" scope
;;

let validate_response_format ~scope = function
  | `Assoc fields as json ->
    let type_values =
      List.filter_map
        (fun (name, value) -> if String.equal name "type" then Some value else None)
        fields
    in
    (match type_values with
     | [ `String "off" ] | [ `String "json_mode" ] ->
       validate_object_shape ~scope ~required:[ "type" ] ~optional:[] json
       |> Result.map (fun _ -> ())
     | [ `String "json_schema" ] ->
       let* fields =
         validate_object_shape ~scope ~required:[ "type"; "schema" ] ~optional:[] json
       in
       let* schema = required_field ~scope "schema" fields in
       (match schema with
        | `Null -> json_errorf "%s.schema must not be null" scope
        | _ -> Ok ())
     | [ `String value ] -> json_errorf "%s has unsupported type %S" scope value
     | [ _ ] -> json_errorf "%s.type must be a string" scope
     | [] -> json_errorf "%s is missing field type" scope
     | _ -> json_errorf "%s duplicates field type" scope)
  | _ -> json_errorf "%s must be a JSON object" scope
;;

let released_checkpoint_common_fields =
  [ "version"
  ; "session_id"
  ; "agent_name"
  ; "model"
  ; "system_prompt"
  ; "messages"
  ; "usage"
  ; "turn_count"
  ; "created_at"
  ; "tools"
  ; "tool_choice"
  ; "temperature"
  ; "top_p"
  ; "top_k"
  ; "min_p"
  ; "enable_thinking"
  ; "response_format"
  ; "thinking_budget"
  ; "disable_parallel_tool_use"
  ; "cache_system_prompt"
  ; "context"
  ; "mcp_sessions"
  ; "working_context"
  ]
;;

let preserve_thinking_field = "preserve_thinking"
let max_input_tokens_field = "max_input_tokens"
let max_total_tokens_field = "max_total_tokens"

let checkpoint_fields = function
  | Released_v5_pre_preserve_capped ->
    max_input_tokens_field :: max_total_tokens_field :: released_checkpoint_common_fields
  | Released_v5_preserve_capped ->
    preserve_thinking_field
    :: max_input_tokens_field
    :: max_total_tokens_field
    :: released_checkpoint_common_fields
  | Released_v5_preserve_unbounded | Released_v6 ->
    preserve_thinking_field :: released_checkpoint_common_fields
  | Current_v8 ->
    "reasoning_effort" :: preserve_thinking_field :: released_checkpoint_common_fields
;;

let current_checkpoint_fields = checkpoint_fields Current_v8

let schema_of_source_fields ~version fields =
  match version with
  | 5 ->
    (match
       ( List.mem_assoc preserve_thinking_field fields
       , List.mem_assoc max_input_tokens_field fields
       , List.mem_assoc max_total_tokens_field fields )
     with
     | false, true, true -> Ok Released_v5_pre_preserve_capped
     | true, true, true -> Ok Released_v5_preserve_capped
     | true, false, false -> Ok Released_v5_preserve_unbounded
     | false, false, false ->
       json_errorf
         "Checkpoint v5 does not match a released shape: preserve_thinking and both \
          token cap fields are absent"
     | _, has_max_input_tokens, has_max_total_tokens ->
       json_errorf
         "Checkpoint v5 has a partial released token-cap shape (max_input_tokens=%b, \
          max_total_tokens=%b)"
         has_max_input_tokens
         has_max_total_tokens)
  | 6 -> Ok Released_v6
  | version ->
    Error
      (Error.Serialization (VersionMismatch { expected = target_version; got = version }))
;;

let released_usage_common_fields =
  [ "total_input_tokens"
  ; "total_output_tokens"
  ; "total_cache_creation_input_tokens"
  ; "total_cache_read_input_tokens"
  ; "api_calls"
  ; "estimated_cost_usd"
  ]
;;

let current_usage_fields = "pricing_gap" :: released_usage_common_fields

let validate_usage_numbers ~scope fields =
  let* total_input_tokens = required_field ~scope "total_input_tokens" fields in
  let* total_output_tokens = required_field ~scope "total_output_tokens" fields in
  let* total_cache_creation_input_tokens =
    required_field ~scope "total_cache_creation_input_tokens" fields
  in
  let* total_cache_read_input_tokens =
    required_field ~scope "total_cache_read_input_tokens" fields
  in
  let* api_calls = required_field ~scope "api_calls" fields in
  let* estimated_cost_usd = required_field ~scope "estimated_cost_usd" fields in
  let* () = validate_int ~scope:(scope ^ ".total_input_tokens") total_input_tokens in
  let* () = validate_int ~scope:(scope ^ ".total_output_tokens") total_output_tokens in
  let* () =
    validate_int
      ~scope:(scope ^ ".total_cache_creation_input_tokens")
      total_cache_creation_input_tokens
  in
  let* () =
    validate_int
      ~scope:(scope ^ ".total_cache_read_input_tokens")
      total_cache_read_input_tokens
  in
  let* () = validate_int ~scope:(scope ^ ".api_calls") api_calls in
  validate_float ~scope:(scope ^ ".estimated_cost_usd") estimated_cost_usd
;;

type released_usage_shape =
  | Without_unpriced_model
  | With_unpriced_model

let migrate_usage ~schema json =
  let scope = schema_scope schema ^ " usage" in
  let allows_missing_unpriced_model =
    match schema with
    | Released_v5_pre_preserve_capped -> true
    | Released_v5_preserve_capped
    | Released_v5_preserve_unbounded
    | Released_v6
    | Current_v8 -> false
  in
  let* fields =
    if allows_missing_unpriced_model
    then
      validate_object_shape
        ~scope
        ~required:released_usage_common_fields
        ~optional:[ "unpriced_model" ]
        json
    else
      validate_object_shape
        ~scope
        ~required:("unpriced_model" :: released_usage_common_fields)
        ~optional:[]
        json
  in
  let* () = validate_usage_numbers ~scope fields in
  let usage_shape, unpriced_model =
    match List.assoc_opt "unpriced_model" fields with
    | None -> Without_unpriced_model, `Null
    | Some unpriced_model -> With_unpriced_model, unpriced_model
  in
  let* pricing_gap =
    match usage_shape, unpriced_model with
    | Without_unpriced_model, `Null -> Ok `Null
    | With_unpriced_model, `Null -> Ok `Null
    | With_unpriced_model, `String "<unknown>" ->
      Ok (`Assoc [ "kind", `String "model_identity_unavailable" ])
    | With_unpriced_model, `String "" ->
      json_errorf "%s unpriced_model must not be empty" scope
    | With_unpriced_model, `String model_id ->
      Ok (`Assoc [ "kind", `String "pricing_unavailable"; "model_id", `String model_id ])
    | With_unpriced_model, _ ->
      json_errorf "%s unpriced_model must be string or null" scope
    | Without_unpriced_model, _ -> json_errorf "%s internal usage-shape mismatch" scope
  in
  Ok
    ( `Assoc (("pricing_gap", pricing_gap) :: List.remove_assoc "unpriced_model" fields)
    , usage_shape )
;;

let validate_pricing_gap ~scope = function
  | `Null -> Ok ()
  | `Assoc fields as json ->
    let kinds =
      List.filter_map
        (fun (name, value) -> if String.equal name "kind" then Some value else None)
        fields
    in
    (match kinds with
     | [ `String "model_identity_unavailable" ] ->
       validate_object_shape ~scope ~required:[ "kind" ] ~optional:[] json
       |> Result.map (fun _ -> ())
     | [ `String "pricing_unavailable" ] ->
       let* fields =
         validate_object_shape ~scope ~required:[ "kind"; "model_id" ] ~optional:[] json
       in
       let* model_id = required_field ~scope "model_id" fields in
       (match model_id with
        | `String "" -> json_errorf "%s.model_id must not be empty" scope
        | `String _ -> Ok ()
        | _ -> json_errorf "%s.model_id must be a string" scope)
     | [ `String value ] -> json_errorf "%s has unsupported kind %S" scope value
     | [ _ ] -> json_errorf "%s.kind must be a string" scope
     | [] -> json_errorf "%s is missing field kind" scope
     | _ -> json_errorf "%s duplicates field kind" scope)
  | _ -> json_errorf "%s must be null or a JSON object" scope
;;

let validate_current_usage json =
  let scope = "Checkpoint v8 usage" in
  let* fields =
    validate_object_shape ~scope ~required:current_usage_fields ~optional:[] json
  in
  let* () = validate_usage_numbers ~scope fields in
  let* pricing_gap = required_field ~scope "pricing_gap" fields in
  validate_pricing_gap ~scope:(scope ^ ".pricing_gap") pricing_gap
;;

let released_v6_failure_kind ~scope = function
  | Types.Validation_error | Types.Recoverable_tool_error | Types.Non_retryable_tool_error
    -> Ok ()
  | Types.Reported_tool_error | Types.Unattributed_tool_error ->
    json_errorf "%s.failure_kind is not a released v6 value" scope
;;

let current_failure_kind _ = Ok ()

let rec normalize_tool_result ~schema ~scope json =
  let optional_provenance_fields =
    if is_released_v5 schema then [] else [ "failure_kind"; "error_class" ]
  in
  let* fields =
    validate_object_shape
      ~scope
      ~required:[ "type"; "tool_use_id"; "content"; "is_error" ]
      ~optional:optional_provenance_fields
      json
  in
  let* type_value = required_field ~scope "type" fields in
  let* tool_use_id = required_field ~scope "tool_use_id" fields in
  let* content = required_field ~scope "content" fields in
  let* is_error = required_field ~scope "is_error" fields in
  let* () =
    match type_value with
    | `String "tool_result" -> Ok ()
    | `String value -> json_errorf "%s.type must be tool_result, got %S" scope value
    | _ -> json_errorf "%s.type must be a string" scope
  in
  let* () = validate_string ~scope:(scope ^ ".tool_use_id") tool_use_id in
  let* migrated_content =
    match content with
    | `String _ -> Ok content
    | `List blocks ->
      let+ migrated_blocks =
        blocks
        |> List.mapi (fun index block ->
          normalize_content_block
            ~schema
            ~scope:(Printf.sprintf "%s.content[%d]" scope index)
            block)
        |> result_all
      in
      `List migrated_blocks
    | _ -> json_errorf "%s.content must be a string or an array" scope
  in
  let fields = replace_field "content" migrated_content fields in
  let failure_kind = List.assoc_opt "failure_kind" fields in
  let error_class = List.assoc_opt "error_class" fields in
  let* () =
    match failure_kind with
    | None -> Ok ()
    | Some value ->
      (match Types.tool_failure_kind_of_yojson value with
       | Ok failure_kind ->
         (match schema with
          | Released_v5_pre_preserve_capped
          | Released_v5_preserve_capped
          | Released_v5_preserve_unbounded ->
            json_errorf "%s.failure_kind is not a released v5 field" scope
          | Released_v6 -> released_v6_failure_kind ~scope failure_kind
          | Current_v8 -> current_failure_kind failure_kind)
       | Error _ -> json_errorf "%s.failure_kind is not a supported value" scope)
  in
  let* () =
    match error_class with
    | None -> Ok ()
    | Some value ->
      (match Types.tool_error_class_of_yojson value with
       | Ok (Types.Transient | Types.Deterministic | Types.Unknown) -> Ok ()
       | Error _ -> json_errorf "%s.error_class is not a supported value" scope)
  in
  match is_error, failure_kind, error_class with
  | `Bool true, None, None ->
    (match schema with
     | Released_v5_pre_preserve_capped
     | Released_v5_preserve_capped
     | Released_v5_preserve_unbounded
     | Released_v6 ->
       Ok
         (`Assoc
             (fields
              @ [ ( "failure_kind"
                  , Types.tool_failure_kind_to_yojson Types.Unattributed_tool_error )
                ]))
     | Current_v8 -> json_errorf "%s failure is missing failure_kind provenance" scope)
  | `Bool true, Some _, _ -> Ok (`Assoc fields)
  | `Bool true, None, Some _ ->
    json_errorf "%s has error_class without failure_kind" scope
  | `Bool false, None, None -> Ok (`Assoc fields)
  | `Bool false, Some _, _ | `Bool false, None, Some _ ->
    json_errorf "%s marks success but contains failure provenance" scope
  | _, _, _ -> json_errorf "%s is_error must be boolean" scope

and normalize_content_block ~schema ~scope json =
  match json with
  | `Assoc fields ->
    let type_values =
      List.filter_map
        (fun (name, value) -> if String.equal name "type" then Some value else None)
        fields
    in
    (match type_values with
     | [ `String "tool_result" ] -> normalize_tool_result ~schema ~scope json
     | [ `String "text" ] ->
       let* fields =
         validate_object_shape ~scope ~required:[ "type"; "text" ] ~optional:[] json
       in
       let* text = required_field ~scope "text" fields in
       let+ () = validate_string ~scope:(scope ^ ".text") text in
       json
     | [ `String "thinking" ] ->
       let* fields =
         validate_object_shape
           ~scope
           ~required:[ "type"; "thinking" ]
           ~optional:[ "signature" ]
           json
       in
       let* thinking = required_field ~scope "thinking" fields in
       let* () = validate_string ~scope:(scope ^ ".thinking") thinking in
       let* () =
         match List.assoc_opt "signature" fields with
         | None -> Ok ()
         | Some signature -> validate_string ~scope:(scope ^ ".signature") signature
       in
       Ok json
     | [ `String "reasoning_details" ] ->
       let* fields =
         validate_object_shape
           ~scope
           ~required:[ "type"; "details" ]
           ~optional:[ "reasoning_content" ]
           json
       in
       let* details = required_field ~scope "details" fields in
       let* () =
         validate_list ~scope:(scope ^ ".details") validate_unique_object details
       in
       let* () =
         match List.assoc_opt "reasoning_content" fields with
         | None -> Ok ()
         | Some (`String _) -> Ok ()
         | Some _ -> json_errorf "%s.reasoning_content must be a string" scope
       in
       Ok json
     | [ `String "redacted_thinking" ] ->
       let* fields =
         validate_object_shape ~scope ~required:[ "type"; "data" ] ~optional:[] json
       in
       let* data = required_field ~scope "data" fields in
       let+ () = validate_string ~scope:(scope ^ ".data") data in
       json
     | [ `String "tool_use" ] ->
       let* fields =
         validate_object_shape
           ~scope
           ~required:[ "type"; "id"; "name"; "input" ]
           ~optional:[]
           json
       in
       let* id = required_field ~scope "id" fields in
       let* name = required_field ~scope "name" fields in
       let* () = validate_string ~scope:(scope ^ ".id") id in
       let+ () = validate_string ~scope:(scope ^ ".name") name in
       json
     | [ `String ("image" | "document" | "audio") ] ->
       let* fields =
         validate_object_shape ~scope ~required:[ "type"; "source" ] ~optional:[] json
       in
       let* source = required_field ~scope "source" fields in
       let* source_fields =
         validate_object_shape
           ~scope:(scope ^ ".source")
           ~required:[ "type"; "media_type"; "data" ]
           ~optional:[]
           source
       in
       let* source_type =
         required_field ~scope:(scope ^ ".source") "type" source_fields
       in
       let* media_type =
         required_field ~scope:(scope ^ ".source") "media_type" source_fields
       in
       let* data = required_field ~scope:(scope ^ ".source") "data" source_fields in
       let* () =
         validate_string_value
           ~scope:(scope ^ ".source.type")
           ~allowed:[ "base64"; "url"; "file_id" ]
           source_type
       in
       let* () = validate_string ~scope:(scope ^ ".source.media_type") media_type in
       let+ () = validate_string ~scope:(scope ^ ".source.data") data in
       json
     | [ `String value ] -> json_errorf "%s has unsupported type %S" scope value
     | [ _ ] -> json_errorf "%s type must be a string" scope
     | [] -> json_errorf "%s is missing field type" scope
     | _ -> json_errorf "%s duplicates field type" scope)
  | _ -> json_errorf "%s must be a JSON object" scope
;;

let normalize_message ~schema index json =
  let scope = Printf.sprintf "%s message[%d]" (schema_scope schema) index in
  let* fields =
    validate_object_shape
      ~scope
      ~required:[ "role"; "content" ]
      ~optional:[ "name"; "tool_call_id"; "metadata" ]
      json
  in
  let* role = required_field ~scope "role" fields in
  let* content = required_field ~scope "content" fields in
  let* () =
    validate_string_value
      ~scope:(scope ^ ".role")
      ~allowed:[ "system"; "user"; "assistant"; "tool" ]
      role
  in
  let* () =
    match List.assoc_opt "name" fields with
    | None -> Ok ()
    | Some name -> validate_string ~scope:(scope ^ ".name") name
  in
  let* () =
    match List.assoc_opt "tool_call_id" fields with
    | None -> Ok ()
    | Some tool_call_id -> validate_string ~scope:(scope ^ ".tool_call_id") tool_call_id
  in
  let* () =
    match List.assoc_opt "metadata" fields with
    | None -> Ok ()
    | Some (`Assoc []) ->
      json_errorf "%s.metadata must be omitted when it has no fields" scope
    | Some metadata -> validate_unique_object ~scope:(scope ^ ".metadata") metadata
  in
  match content with
  | `List blocks ->
    let* migrated_blocks =
      blocks
      |> List.mapi (fun block_index block ->
        normalize_content_block
          ~schema
          ~scope:(Printf.sprintf "%s content[%d]" scope block_index)
          block)
      |> result_all
    in
    Ok (`Assoc (replace_field "content" (`List migrated_blocks) fields))
  | _ -> json_errorf "%s content must be an array" scope
;;

let normalize_messages ~schema = function
  | `List messages -> messages |> List.mapi (normalize_message ~schema) |> result_all
  | _ -> json_errorf "%s messages must be an array" (schema_scope schema)
;;

let mcp_session_common_fields =
  [ "server_name"; "command"; "args"; "env"; "tool_schemas"; "transport_kind" ]
;;

let mcp_session_http_fields =
  "http_base_url" :: "http_headers" :: mcp_session_common_fields
;;

let mcp_session_policy_fields = "env_policy" :: mcp_session_http_fields

type transport_kind =
  | Stdio
  | Http

type env_policy =
  | Inherit
  | Minimal
  | Explicit

type mcp_session_shape =
  | Pre_http_fields
  | Http_fields_without_policy
  | Http_fields_with_policy

type checkpoint_usage_shape =
  | Released_usage of released_usage_shape
  | Current_usage

let transport_kind_of_json ~scope = function
  | `String "stdio" -> Ok Stdio
  | `String "http" -> Ok Http
  | `String value -> json_errorf "%s has unsupported value %S" scope value
  | _ -> json_errorf "%s must be a string" scope
;;

let env_policy_of_json ~scope = function
  | `String "inherit" -> Ok Inherit
  | `String "minimal" -> Ok Minimal
  | `String "explicit" -> Ok Explicit
  | `String value -> json_errorf "%s has unsupported value %S" scope value
  | _ -> json_errorf "%s must be a string" scope
;;

let mcp_session_shape ~schema ~usage_shape ~scope fields =
  match schema, usage_shape with
  | Released_v5_pre_preserve_capped, Released_usage Without_unpriced_model ->
    (match
       List.mem_assoc "http_base_url" fields, List.mem_assoc "http_headers" fields
     with
     | false, false -> Ok Pre_http_fields
     | true, true -> Ok Http_fields_without_policy
     | has_http_base_url, has_http_headers ->
       json_errorf
         "%s has a partial released HTTP field shape (http_base_url=%b, http_headers=%b)"
         scope
         has_http_base_url
         has_http_headers)
  | Released_v5_pre_preserve_capped, Released_usage With_unpriced_model
  | Released_v5_preserve_capped, Released_usage With_unpriced_model ->
    Ok Http_fields_without_policy
  | Released_v5_preserve_unbounded, Released_usage With_unpriced_model
  | Released_v6, Released_usage With_unpriced_model -> Ok Http_fields_with_policy
  | Current_v8, Current_usage -> Ok Http_fields_without_policy
  | ( ( Released_v5_pre_preserve_capped
      | Released_v5_preserve_capped
      | Released_v5_preserve_unbounded
      | Released_v6
      | Current_v8 )
    , (Released_usage _ | Current_usage) ) ->
    json_errorf "%s has an inconsistent checkpoint and usage release shape" scope
;;

let fields_for_mcp_session_shape = function
  | Pre_http_fields -> mcp_session_common_fields
  | Http_fields_without_policy -> mcp_session_http_fields
  | Http_fields_with_policy -> mcp_session_policy_fields
;;

let normalize_mcp_session ~schema ~mcp_shape index json =
  let scope = Printf.sprintf "%s mcp_sessions[%d]" (schema_scope schema) index in
  let required = fields_for_mcp_session_shape mcp_shape in
  let* fields = validate_object_shape ~scope ~required ~optional:[] json in
  let* server_name = required_field ~scope "server_name" fields in
  let* command = required_field ~scope "command" fields in
  let* args = required_field ~scope "args" fields in
  let* env = required_field ~scope "env" fields in
  let* tool_schemas = required_field ~scope "tool_schemas" fields in
  let* transport_kind = required_field ~scope "transport_kind" fields in
  let* () = validate_string ~scope:(scope ^ ".server_name") server_name in
  let* () = validate_string ~scope:(scope ^ ".command") command in
  let* () = validate_list ~scope:(scope ^ ".args") validate_string args in
  let* () = validate_list ~scope:(scope ^ ".env") validate_env_pair env in
  let* () =
    validate_list ~scope:(scope ^ ".tool_schemas") validate_tool_schema tool_schemas
  in
  let* transport_kind =
    transport_kind_of_json ~scope:(scope ^ ".transport_kind") transport_kind
  in
  let* fields, http_base_url =
    match mcp_shape with
    | Pre_http_fields ->
      (match transport_kind with
       | Stdio ->
         Ok (("http_base_url", `Null) :: ("http_headers", `List []) :: fields, `Null)
       | Http ->
         json_errorf
           "%s cannot migrate a pre-HTTP-field HTTP session without inventing its \
            reconnect URL"
           scope)
    | Http_fields_without_policy | Http_fields_with_policy ->
      let* http_base_url = required_field ~scope "http_base_url" fields in
      let* http_headers = required_field ~scope "http_headers" fields in
      let* () =
        validate_optional ~scope:(scope ^ ".http_base_url") validate_string http_base_url
      in
      let* () =
        validate_list ~scope:(scope ^ ".http_headers") validate_env_pair http_headers
      in
      Ok (fields, http_base_url)
  in
  let* () =
    match transport_kind, http_base_url with
    | Http, `Null -> json_errorf "%s HTTP transport requires http_base_url" scope
    | (Http | Stdio), _ -> Ok ()
  in
  match mcp_shape with
  | Pre_http_fields | Http_fields_without_policy -> Ok (`Assoc fields)
  | Http_fields_with_policy ->
    let* env_policy_json = required_field ~scope "env_policy" fields in
    let* env_policy = env_policy_of_json ~scope:(scope ^ ".env_policy") env_policy_json in
    (match transport_kind, env_policy with
     | Http, (Inherit | Minimal | Explicit) ->
       (* HTTP reconnect never consults subprocess environment policy. Removing
          this released-only field cannot widen process credentials. *)
       Ok (`Assoc (List.remove_assoc "env_policy" fields))
     | Stdio, Inherit -> Ok (`Assoc (List.remove_assoc "env_policy" fields))
     | Stdio, (Minimal | Explicit) ->
       json_errorf
         "%s cannot migrate stdio env_policy %s without widening the saved subprocess \
          environment"
         scope
         (Yojson.Safe.to_string env_policy_json))
;;

let normalize_mcp_sessions ~schema ~usage_shape = function
  | `List [] -> Ok []
  | `List (first_session :: _ as sessions) ->
    let first_scope = Printf.sprintf "%s mcp_sessions[0]" (schema_scope schema) in
    let* first_fields =
      object_fields_without_duplicates ~scope:first_scope first_session
    in
    let* mcp_shape =
      mcp_session_shape ~schema ~usage_shape ~scope:first_scope first_fields
    in
    sessions |> List.mapi (normalize_mcp_session ~schema ~mcp_shape) |> result_all
  | _ -> json_errorf "%s mcp_sessions must be an array" (schema_scope schema)
;;

let normalize_released_checkpoint_fields ~schema fields =
  let scope = schema_scope schema in
  let* () =
    match schema with
    | Released_v5_pre_preserve_capped | Released_v5_preserve_capped ->
      let* max_input_tokens = required_field ~scope max_input_tokens_field fields in
      let* max_total_tokens = required_field ~scope max_total_tokens_field fields in
      let* () =
        validate_optional
          ~scope:(scope ^ "." ^ max_input_tokens_field)
          validate_int
          max_input_tokens
      in
      validate_optional
        ~scope:(scope ^ "." ^ max_total_tokens_field)
        validate_int
        max_total_tokens
    | Released_v5_preserve_unbounded | Released_v6 -> Ok ()
    | Current_v8 -> json_errorf "%s is not a released checkpoint" scope
  in
  let fields =
    fields
    |> List.remove_assoc max_input_tokens_field
    |> List.remove_assoc max_total_tokens_field
  in
  match schema with
  | Released_v5_pre_preserve_capped -> Ok ((preserve_thinking_field, `Null) :: fields)
  | Released_v5_preserve_capped | Released_v5_preserve_unbounded | Released_v6 ->
    Ok fields
  | Current_v8 -> json_errorf "%s is not a released checkpoint" scope
;;

let validate_common_checkpoint_fields ~scope fields =
  let* session_id = required_field ~scope "session_id" fields in
  let* agent_name = required_field ~scope "agent_name" fields in
  let* model = required_field ~scope "model" fields in
  let* system_prompt = required_field ~scope "system_prompt" fields in
  let* turn_count = required_field ~scope "turn_count" fields in
  let* created_at = required_field ~scope "created_at" fields in
  let* tools = required_field ~scope "tools" fields in
  let* tool_choice = required_field ~scope "tool_choice" fields in
  let* temperature = required_field ~scope "temperature" fields in
  let* top_p = required_field ~scope "top_p" fields in
  let* top_k = required_field ~scope "top_k" fields in
  let* min_p = required_field ~scope "min_p" fields in
  let* enable_thinking = required_field ~scope "enable_thinking" fields in
  let* preserve_thinking = required_field ~scope "preserve_thinking" fields in
  let* response_format = required_field ~scope "response_format" fields in
  let* thinking_budget = required_field ~scope "thinking_budget" fields in
  let* disable_parallel_tool_use =
    required_field ~scope "disable_parallel_tool_use" fields
  in
  let* cache_system_prompt = required_field ~scope "cache_system_prompt" fields in
  let* context = required_field ~scope "context" fields in
  let* () = validate_string ~scope:(scope ^ ".session_id") session_id in
  let* () = validate_string ~scope:(scope ^ ".agent_name") agent_name in
  let* () = validate_string ~scope:(scope ^ ".model") model in
  let* () =
    validate_optional ~scope:(scope ^ ".system_prompt") validate_string system_prompt
  in
  let* () = validate_int ~scope:(scope ^ ".turn_count") turn_count in
  let* () = validate_float ~scope:(scope ^ ".created_at") created_at in
  let* () = validate_list ~scope:(scope ^ ".tools") validate_tool_schema tools in
  let* () = validate_tool_choice ~scope:(scope ^ ".tool_choice") tool_choice in
  let* () =
    validate_optional ~scope:(scope ^ ".temperature") validate_float temperature
  in
  let* () = validate_optional ~scope:(scope ^ ".top_p") validate_float top_p in
  let* () = validate_optional ~scope:(scope ^ ".top_k") validate_int top_k in
  let* () = validate_optional ~scope:(scope ^ ".min_p") validate_float min_p in
  let* () =
    validate_optional ~scope:(scope ^ ".enable_thinking") validate_bool enable_thinking
  in
  let* () =
    validate_optional
      ~scope:(scope ^ ".preserve_thinking")
      validate_bool
      preserve_thinking
  in
  let* () =
    validate_response_format ~scope:(scope ^ ".response_format") response_format
  in
  let* () =
    validate_optional ~scope:(scope ^ ".thinking_budget") validate_int thinking_budget
  in
  let* () =
    validate_bool ~scope:(scope ^ ".disable_parallel_tool_use") disable_parallel_tool_use
  in
  let* () = validate_bool ~scope:(scope ^ ".cache_system_prompt") cache_system_prompt in
  validate_unique_object ~scope:(scope ^ ".context") context
;;

let to_v8_json json =
  let bootstrap_scope = "Checkpoint v5/v6" in
  let* source_fields = object_fields_without_duplicates ~scope:bootstrap_scope json in
  let* version = required_field ~scope:bootstrap_scope "version" source_fields in
  let* schema =
    match version with
    | `Int version -> schema_of_source_fields ~version source_fields
    | _ -> json_errorf "%s version must be an integer" bootstrap_scope
  in
  let scope = schema_scope schema in
  let* source_fields =
    validate_object_shape ~scope ~required:(checkpoint_fields schema) ~optional:[] json
  in
  let* fields = normalize_released_checkpoint_fields ~schema source_fields in
  let* () = validate_common_checkpoint_fields ~scope fields in
  let* usage_json = required_field ~scope "usage" fields in
  let* usage, released_usage_shape = migrate_usage ~schema usage_json in
  let* messages_json = required_field ~scope "messages" fields in
  let* messages = normalize_messages ~schema messages_json in
  let* mcp_sessions_json = required_field ~scope "mcp_sessions" fields in
  let* mcp_sessions =
    normalize_mcp_sessions
      ~schema
      ~usage_shape:(Released_usage released_usage_shape)
      mcp_sessions_json
  in
  let migrated =
    fields
    |> replace_field "version" (`Int target_version)
    |> replace_field "usage" usage
    |> replace_field "messages" (`List messages)
    |> replace_field "mcp_sessions" (`List mcp_sessions)
  in
  Ok (`Assoc (("reasoning_effort", `Null) :: migrated))
;;

let validate_v8_json json =
  let schema = Current_v8 in
  let scope = schema_scope schema in
  let* fields =
    validate_object_shape ~scope ~required:current_checkpoint_fields ~optional:[] json
  in
  let* version = required_field ~scope "version" fields in
  let* () =
    match version with
    | `Int version when version = target_version -> Ok ()
    | `Int version ->
      json_errorf "%s has version %d, expected %d" scope version target_version
    | _ -> json_errorf "%s version must be an integer" scope
  in
  let* () = validate_common_checkpoint_fields ~scope fields in
  let* usage = required_field ~scope "usage" fields in
  let* () = validate_current_usage usage in
  let* messages = required_field ~scope "messages" fields in
  let* _ = normalize_messages ~schema messages in
  let* mcp_sessions = required_field ~scope "mcp_sessions" fields in
  let* _ = normalize_mcp_sessions ~schema ~usage_shape:Current_usage mcp_sessions in
  let* reasoning_effort = required_field ~scope "reasoning_effort" fields in
  validate_optional
    ~scope:(scope ^ ".reasoning_effort")
    (validate_string_value ~allowed:Llm_provider.Reasoning_effort.all_wire_values)
    reasoning_effort
;;
