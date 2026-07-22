module Sha256 = Digestif.SHA256

type fingerprint = Fingerprint of string

type output_admission_error =
  | Explicit_capability_snapshot_required
  | Contradictory_output_state
  | Unsupported_output_contract of
      { provider_kind : Provider_config.provider_kind
      ; model_id : string
      ; response_format : Types.response_format
      }
  | Unsupported_exact_cross_feature
  | Global_admission_not_allowed
  | Invalid_connect_timeout of float
  | Invalid_body_timeout of float
  | Caller_supplied_framing_header_not_allowed of string
  | Provider_request_rejected of Http_client.http_error
  | Request_serialization_rejected of Http_client.http_error

type json_validation_provenance =
  | Json_syntax_validated
  | Provider_schema_requested_client_validation_required

type normalized_output =
  | Text_output of string
  | Json_output of
      { value : Yojson.Safe.t
      ; validation : json_validation_provenance
      }

type output_normalization_error =
  | Incomplete_structured_response of Types.stop_reason
  | Missing_structured_text
  | Ambiguous_structured_text of int
  | Unexpected_structured_content
  | Invalid_json of string

type frozen_wire_request =
  { response_codec : Provider_http_codec.t
  ; provider_kind : Provider_config.provider_kind
  ; url : string
  ; headers : (string * string) list
  ; body : string
  ; body_sha256 : string
  ; connect_timeout_s : float option
  ; body_timeout_s : float option
  }

type t =
  { response_format : Types.response_format
  ; wire : frozen_wire_request
  ; fingerprint : fingerprint
  }

let fingerprint_to_string (Fingerprint value) = value
let sha256 value = Sha256.(to_hex (digest_string value))

let rec canonical_json = function
  | `Assoc fields ->
    `Assoc
      (fields
       |> List.map (fun (name, value) -> name, canonical_json value)
       |> List.sort (fun (left, _) (right, _) -> String.compare left right))
  | `List values -> `List (List.map canonical_json values)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as scalar -> scalar
;;

let canonical_response_format = function
  | Types.JsonSchema schema -> Types.JsonSchema (canonical_json schema)
  | (Types.Off | Types.JsonMode) as response_format -> response_format
;;

let response_format_state_is_consistent
      (config : Provider_config.t)
      (response_format : Types.response_format)
  =
  match response_format, config.output_schema with
  | Types.Off, None | Types.JsonMode, None -> true
  | Types.JsonSchema schema, Some duplicate ->
    canonical_json schema = canonical_json duplicate
  | Types.Off, Some _ | Types.JsonMode, Some _ | Types.JsonSchema _, None -> false
;;

let json_mode_has_wire_serializer = function
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope -> true
  | Provider_config.Anthropic | Provider_config.Kimi -> false
;;

let contract_is_supported
      (config : Provider_config.t)
      (capabilities : Capabilities.capabilities)
  =
  match config.response_format with
  | Types.Off -> true
  | Types.JsonMode ->
    capabilities.supports_response_format_json
    && json_mode_has_wire_serializer config.kind
  | Types.JsonSchema _ -> capabilities.supports_structured_output
;;

let timeout_is_valid = function
  | None -> true
  | Some seconds -> Float.is_finite seconds && seconds > 0.0
;;

let content_uses_exact_cross_feature = function
  | Types.ToolUse _
  | Types.ToolResult _
  | Types.Thinking _
  | Types.ReasoningDetails _
  | Types.RedactedThinking _ -> true
  | Types.Text _ | Types.Image _ | Types.Document _ | Types.Audio _ -> false
;;

let request_uses_exact_cross_feature (request : Llm_transport.completion_request) =
  let config = request.config in
  request.tools <> []
  || config.tool_stream
  || config.disable_parallel_tool_use
  || (match config.tool_choice with
      | None | Some Types.None_ -> false
      | Some _ -> true)
  || Option.is_some config.enable_thinking
  || Option.is_some config.preserve_thinking
  || Option.is_some config.thinking_budget
  || Option.is_some config.reasoning_effort
  || Option.is_some config.clear_thinking
  || List.exists
       (fun (message : Types.message) ->
          List.exists content_uses_exact_cross_feature message.content)
       request.messages
;;

let header_name_equal left right =
  String.equal (String.lowercase_ascii left) (String.lowercase_ascii right)
;;

let forbidden_framing_headers =
  [ "connection"
  ; "content-length"
  ; "expect"
  ; "keep-alive"
  ; "proxy-authenticate"
  ; "proxy-authorization"
  ; "proxy-connection"
  ; "te"
  ; "trailer"
  ; "transfer-encoding"
  ; "upgrade"
  ]
;;

let caller_supplied_framing_header headers =
  List.find_map
    (fun (name, _) -> forbidden_framing_headers |> List.find_opt (header_name_equal name))
    headers
;;

let add_part buffer value =
  Buffer.add_string buffer (string_of_int (String.length value));
  Buffer.add_char buffer ':';
  Buffer.add_string buffer value
;;

let option_float = function
  | None -> "none"
  | Some value -> Printf.sprintf "some:%.17g" value
;;

let plan_fingerprint
      ~(config : Provider_config.t)
      ~(capabilities : Capabilities.capabilities)
      ~(wire : frozen_wire_request)
      ~(fit : Prepared_completion_request.context_fit)
  =
  let material = Buffer.create 512 in
  List.iter
    (add_part material)
    [ "oas-exact-output-plan-v2"
    ; Provider_http_codec.fingerprint_tag wire.response_codec
    ; Provider_config.string_of_provider_kind wire.provider_kind
    ; Option.value config.provider_id ~default:""
    ; config.model_id
    ; wire.url
    ; wire.body_sha256
    ; Yojson.Safe.to_string (Types.response_format_to_json config.response_format)
    ; (if capabilities.supports_response_format_json then "1" else "0")
    ; (if capabilities.supports_structured_output then "1" else "0")
    ; string_of_int fit.input_tokens
    ; string_of_int fit.reserved_output_tokens
    ; string_of_int fit.max_context_tokens
    ; option_float wire.connect_timeout_s
    ; option_float wire.body_timeout_s
    ; string_of_int (List.length wire.headers)
    ];
  List.iter
    (fun (name, value) ->
       add_part material name;
       add_part material value)
    wire.headers;
  Fingerprint (sha256 (Buffer.contents material))
;;

let freeze_config_response_format (config : Provider_config.t) response_format =
  let output_schema =
    match response_format with
    | Types.JsonSchema schema -> Some schema
    | Types.Off | Types.JsonMode -> None
  in
  { config with response_format; output_schema }
;;

let request_url (config : Provider_config.t) =
  match config.kind with
  | Provider_config.Gemini -> Complete_sampling.gemini_url ~config ~stream:false
  | Provider_config.Anthropic
  | Provider_config.Kimi
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Glm
  | Provider_config.DashScope -> config.base_url ^ config.request_path
;;

let admit admitted =
  let prepared = Prepared_completion_request.admitted_request admitted in
  let request = Prepared_completion_request.request prepared in
  let original_config = request.config in
  match original_config.model_capabilities_override with
  | None -> Error Explicit_capability_snapshot_required
  | Some capabilities ->
    let response_format = canonical_response_format original_config.response_format in
    let config = freeze_config_response_format original_config response_format in
    let auth_headers = Provider_config.auth_headers_for_config config in
    if not (response_format_state_is_consistent original_config response_format)
    then Error Contradictory_output_state
    else if request_uses_exact_cross_feature request
    then Error Unsupported_exact_cross_feature
    else if Option.is_some config.max_concurrent_requests
    then Error Global_admission_not_allowed
    else if not (timeout_is_valid config.connect_timeout_s)
    then Error (Invalid_connect_timeout (Option.get config.connect_timeout_s))
    else if not (timeout_is_valid request.body_timeout_s)
    then Error (Invalid_body_timeout (Option.get request.body_timeout_s))
    else if not (contract_is_supported config capabilities)
    then
      Error
        (Unsupported_output_contract
           { provider_kind = config.kind; model_id = config.model_id; response_format })
    else if
      Option.is_some (caller_supplied_framing_header (config.headers @ auth_headers))
    then
      Error
        (Caller_supplied_framing_header_not_allowed
           (Option.get (caller_supplied_framing_header (config.headers @ auth_headers))))
    else (
      match Complete_common.validate_all config with
      | Error error -> Error (Provider_request_rejected error)
      | Ok () ->
        (match
           Complete_common.serialize_http_request
             ~stream:false
             ~config
             ~messages:request.messages
             ~tools:request.tools
         with
         | Error error -> Error (Request_serialization_rejected error)
         | Ok (response_codec, body) ->
           let body_sha256 = sha256 body in
           let headers =
             config.headers
             @ auth_headers
             @ [ "Content-Length", string_of_int (String.length body) ]
           in
           let wire =
             { response_codec
             ; provider_kind = config.kind
             ; url = request_url config
             ; headers
             ; body
             ; body_sha256
             ; connect_timeout_s = config.connect_timeout_s
             ; body_timeout_s = request.body_timeout_s
             }
           in
           let fit = Prepared_completion_request.admitted_fit admitted in
           let fingerprint = plan_fingerprint ~config ~capabilities ~wire ~fit in
           Ok { response_format; wire; fingerprint }))
;;

let fingerprint plan = plan.fingerprint
let response_format plan = plan.response_format
let request_body_sha256 plan = plan.wire.body_sha256
let request_url plan = plan.wire.url
let request_headers plan = plan.wire.headers
let request_body plan = plan.wire.body
let response_codec plan = plan.wire.response_codec
let provider_kind plan = plan.wire.provider_kind
let connect_timeout_s plan = plan.wire.connect_timeout_s
let body_timeout_s plan = plan.wire.body_timeout_s

let verify_frozen_request plan =
  String.equal plan.wire.body_sha256 (sha256 plan.wire.body)
;;

let structured_text content =
  let rec loop texts = function
    | [] ->
      (match List.rev texts with
       | [] -> Error Missing_structured_text
       | [ text ] -> Ok text
       | values -> Error (Ambiguous_structured_text (List.length values)))
    | Types.Text text :: rest -> loop (text :: texts) rest
    | (Types.Thinking _ | Types.ReasoningDetails _ | Types.RedactedThinking _) :: rest ->
      loop texts rest
    | ( Types.ToolUse _
      | Types.ToolResult _
      | Types.Image _
      | Types.Document _
      | Types.Audio _ )
      :: _ -> Error Unexpected_structured_content
  in
  loop [] content
;;

let stop_is_terminal = function
  | Types.EndTurn | Types.StopSequence -> true
  | Types.StopToolUse
  | Types.MaxTokens
  | Types.Refusal
  | Types.ContentFilter
  | Types.RepetitionTruncation
  | Types.PauseTurn
  | Types.Compaction
  | Types.ContextWindowExceeded
  | Types.UnmatchedToolCalls
  | Types.Unknown _ -> false
;;

let normalize_response response_format (response : Types.api_response) =
  if not (stop_is_terminal response.stop_reason)
  then Error (Incomplete_structured_response response.stop_reason)
  else (
    match structured_text response.content with
    | Error _ as error -> error
    | Ok text ->
      (match response_format with
       | Types.Off -> Ok (Text_output text)
       | (Types.JsonMode | Types.JsonSchema _) as response_format ->
         (try
            let value = Yojson.Safe.from_string text in
            let validation =
              match response_format with
              | Types.JsonMode -> Json_syntax_validated
              | Types.JsonSchema _ -> Provider_schema_requested_client_validation_required
              | Types.Off -> assert false
            in
            Ok (Json_output { value; validation })
          with
          | Yojson.Json_error detail -> Error (Invalid_json detail))))
;;

let normalize plan response = normalize_response plan.response_format response

let%test "JsonMode records syntax-only validation provenance" =
  let response : Types.api_response =
    { id = "json-mode"
    ; model = "fixture"
    ; stop_reason = Types.EndTurn
    ; content = [ Types.Text {|{"accepted":true}|} ]
    ; usage = None
    ; telemetry = None
    }
  in
  match normalize_response Types.JsonMode response with
  | Ok
      (Json_output
         { value = `Assoc [ ("accepted", `Bool true) ]
         ; validation = Json_syntax_validated
         }) -> true
  | Ok _ | Error _ -> false
;;

let%test "canonical fingerprint is sensitive to the frozen response codec" =
  let config =
    Provider_config.make
      ~kind:Provider_config.Anthropic
      ~model_id:"fingerprint-fixture"
      ~base_url:"https://example.test"
      ~request_path:"/v1/messages"
      ~max_tokens:16
      ~response_format:Types.Off
      ()
  in
  let capabilities = Capabilities.anthropic_capabilities in
  let fit : Prepared_completion_request.context_fit =
    { input_tokens = 1; reserved_output_tokens = 16; max_context_tokens = 128 }
  in
  let wire response_codec =
    { response_codec
    ; provider_kind = Provider_config.Anthropic
    ; url = "https://example.test/v1/messages"
    ; headers = [ "Content-Length", "2" ]
    ; body = "{}"
    ; body_sha256 = sha256 "{}"
    ; connect_timeout_s = None
    ; body_timeout_s = None
    }
  in
  let fingerprint response_codec =
    plan_fingerprint ~config ~capabilities ~wire:(wire response_codec) ~fit
    |> fingerprint_to_string
  in
  let anthropic_codec = Provider_http_codec.of_config config in
  let openai_codec =
    Provider_http_codec.of_config
      { config with
        kind = Provider_config.OpenAI_compat
      ; request_path = "/v1/chat/completions"
      }
  in
  fingerprint anthropic_codec <> fingerprint openai_codec
;;
