(** API dispatch — re-exports provider modules and routes create_message *)

module Retry = Llm_provider.Retry
open Types

type response_accept = Types.api_response -> (unit, string) result

type create_message_error =
  | Retry_error of Retry.api_error
  | Attributed_retry_error of
      { retry_error : Retry.api_error
      ; http_error : Llm_provider.Http_client.http_error
      }
  | Completion_error of Llm_provider.Http_client.http_error

let attributed_retry_error http_error retry_error =
  Attributed_retry_error { retry_error; http_error }
;;

let create_message_error_of_http_error = function
  | Llm_provider.Http_client.HttpError { code; body; retry_after_header } as http_error ->
    attributed_retry_error
      http_error
      (Retry.classify_error ~retry_after_header ~status:code ~body)
  | Llm_provider.Http_client.NetworkError
      { message; kind = Llm_provider.Http_client.Timeout } as http_error ->
    attributed_retry_error http_error (Retry.Timeout { message; phase = None })
  | Llm_provider.Http_client.NetworkError { message; kind } as http_error ->
    attributed_retry_error http_error (Retry.NetworkError { message; kind })
  | Llm_provider.Http_client.TimeoutError { message; phase } as http_error ->
    attributed_retry_error http_error (Retry.Timeout { message; phase = Some phase })
  | Llm_provider.Http_client.AcceptRejected { reason } as http_error ->
    attributed_retry_error
      http_error
      (Retry.InvalidRequest
         { message = "Response rejected: " ^ reason; reason = Unknown_invalid_request })
  | Llm_provider.Http_client.ProviderTerminal { message; _ } as http_error ->
    attributed_retry_error
      http_error
      (Retry.InvalidRequest { message; reason = Unknown_invalid_request })
  | Llm_provider.Http_client.ProviderFailure
      { kind = Llm_provider.Http_client.Empty_completion { stop_reason }; message } as
    http_error ->
    (* The #2621 empty-completion overflow rule lives in one place now:
       [Retry.overflow_of_empty_completion]. A ContextWindowExceeded empty turn
       is a context-overflow contract (only compaction makes progress, not
       retry/rotate); every other stop_reason stays an opaque completion error.
       This site's wrappers — [attributed_retry_error] / [Completion_error] —
       are preserved exactly. *)
    (match Retry.overflow_of_empty_completion ~stop_reason ~message with
     | Some overflow -> attributed_retry_error http_error overflow
     | None -> Completion_error http_error)
  | Llm_provider.Http_client.ProviderFailure { kind; message } as http_error ->
    attributed_retry_error
      http_error
      (Retry.InvalidRequest
         { message = Llm_provider.Http_client.provider_failure_to_string ~kind ~message
         ; reason = Unknown_invalid_request
         })
;;

let detailed_error_of_create_message_error ~binding = function
  | Retry_error err ->
    Provider_failure_attribution.of_response_parse_error ~binding (Error.Api err)
  | Attributed_retry_error { retry_error; http_error } ->
    { (Provider_failure_attribution.of_http_error ~binding http_error) with
      error = Error.Api retry_error
    }
  | Completion_error err -> Provider_failure_attribution.of_http_error ~binding err
;;

(* Re-export Api_common *)
let default_base_url = Api_common.default_base_url
let api_version = Api_common.api_version
let max_response_body = Api_common.max_response_body
let string_is_blank = Api_common.string_is_blank
let text_blocks_to_string = Api_common.text_blocks_to_string
let json_of_string_or_raw = Api_common.json_of_string_or_raw
let content_block_to_json = Api_common.content_block_to_json
let content_block_of_json = Api_common.content_block_of_json
let message_to_json = Api_common.message_to_json
let make_https = Api_common.make_https

(* Re-export Api_anthropic *)
let parse_response = Api_anthropic.parse_response

let build_body_assoc ~config ~messages ?tools ~stream () =
  Api_anthropic.build_body_assoc ~config ~messages ?tools ~stream ()
;;

let build_body_assoc_result_for_resolved_config =
  Api_anthropic.build_body_assoc_result_for_resolved_config
;;

(* Re-export Api_openai *)
let openai_messages_of_message = Api_openai.openai_messages_of_message
let openai_content_parts_of_blocks = Api_openai.openai_content_parts_of_blocks
let build_openai_body_result = Api_openai.build_openai_body_result

let build_openai_body_result_for_resolved_config =
  Api_openai.build_openai_body_result_for_resolved_config
;;

let build_openai_body = Api_openai.build_openai_body

let parse_openai_response_result =
  Llm_provider.Backend_openai_parse.parse_openai_response_result
;;

(* Transport latency patch. Parser layers leave request_latency_ms unknown
   because they only see the JSON response body; only the transport layer
   can measure request latency. *)
let project_custom_provider_history
      ~(provider_config : Llm_provider.Provider_config.t)
      messages
  =
  match
    Llm_provider.Reasoning_history_projection.project_for_provider_config
      ~assistant_has_payload:(fun content -> content <> [])
      ~reasoning_block_supported:(function
        | Types.Thinking _ | Types.ReasoningDetails _ | Types.RedactedThinking _ -> false
        | Types.Text _
        | Types.ToolUse _
        | Types.ToolResult _
        | Types.Image _
        | Types.Document _
        | Types.Audio _ -> false)
      provider_config
      messages
  with
  | Error error -> Error (Llm_provider.Reasoning_history_projection.error_to_string error)
  | Ok projection ->
    Llm_provider.Reasoning_history_projection.observe
      ~component:"api_custom_provider"
      projection;
    Ok projection.messages
;;

let ensure_nonempty_response resp =
  Llm_provider.Complete_common.ensure_nonempty_completion (Ok resp)
  |> Result.map_error (fun err -> Completion_error err)
;;

let parse_openai_completion body_str =
  match parse_openai_response_result body_str with
  | Ok resp -> ensure_nonempty_response resp
  | Error (Llm_provider.Backend_openai_parse.Provider_error message) ->
    Error
      (Retry_error
         (Retry.InvalidRequest { message; reason = Retry.Unknown_invalid_request }))
  | Error (Llm_provider.Backend_openai_parse.Empty_completion empty) ->
    Error
      (Completion_error
         (Llm_provider.Http_client.empty_completion_error ~stop_reason:empty.stop_reason))
;;

(** Send a non-streaming message to the explicitly selected provider.
    A wall-clock timeout is applied only when both [clock] and
    [request_timeout_s] are supplied. *)
let create_message_detailed
      ~sw
      ~net
      ~provider
      ?clock
      ?request_timeout_s
      ~config
      ~messages
      ?tools
      ?slot_id
      ()
  =
  let ( let* ) = Result.bind in
  let configuration_error field detail =
    Provider_failure_attribution.of_provider_configuration_error
      (Error.Config (InvalidConfig { field; detail }))
  in
  let timeout_mode =
    match clock, request_timeout_s with
    | None, Some _ ->
      Error
        (configuration_error
           "request_timeout_s"
           "request_timeout_s requires an Eio clock")
    | Some _, Some timeout_s when timeout_s <= 0.0 ->
      Error
        (configuration_error
           "request_timeout_s"
           "request_timeout_s must be greater than zero")
    | Some clock, Some timeout_s -> Ok (Some (clock, timeout_s))
    | Some _, None | None, None -> Ok None
  in
  match timeout_mode with
  | Error error -> Error error
  | Ok timeout_mode ->
    (match Provider.resolve provider with
     | Error error ->
       Error (Provider_failure_attribution.of_provider_configuration_error error)
     | Ok (base_url, api_key, header_list) ->
       let provider_cfg = provider in
       let provider_id = Provider_runtime_binding.provider_id_of_config provider_cfg in
       let model_spec = Provider.model_spec_of_config provider_cfg in
       let* binding =
         Binding_identity.of_resolved_provider
           ~transport:Binding_identity.Http
           ~provider:provider_cfg
           ~base_url
           ~request_path:model_spec.request_path
           ~api_key
         |> Result.map_error (configuration_error "model_id")
       in
       let kind = model_spec.request_kind in
       let path = model_spec.request_path in
       let response_provider_config_result =
         Provider.provider_config_of_agent ~state:config ~base_url (Some provider_cfg)
       in
       let request_handler_result =
         match response_provider_config_result with
         | Error _ as error -> error
         | Ok response_provider_config ->
           let response_provider_config =
             { response_provider_config with base_url; request_path = path }
           in
           (match kind with
            | Provider.Anthropic_messages -> Ok (`Anthropic, response_provider_config)
            | Provider.Openai_chat_completions -> Ok (`Openai, response_provider_config)
            | Provider.Custom name ->
              (match Provider.find_provider name with
               | Some impl -> Ok (`Custom impl, response_provider_config)
               | None ->
                 Error
                   (Error.Config
                      (Error.InvalidConfig
                         { field = "provider"
                         ; detail =
                             Printf.sprintf "Custom provider '%s' is not registered" name
                         }))))
       in
       (match request_handler_result with
        | Error error ->
          Error (Provider_failure_attribution.of_runtime_binding_error ~binding error)
        | Ok (request_handler, response_provider_config) ->
          let body_result =
            match request_handler with
            | `Anthropic ->
              build_body_assoc_result_for_resolved_config
                ~resolved_config:response_provider_config
                ~cache_extended_ttl:config.config.cache_extended_ttl
                ~messages
                ?tools
                ~stream:false
                ()
              |> Result.map (fun body -> Yojson.Safe.to_string (`Assoc body))
            | `Openai ->
              build_openai_body_result_for_resolved_config
                ~resolved_config:response_provider_config
                ~messages
                ?tools
                ?slot_id
                ()
            | `Custom impl ->
              (match
                 project_custom_provider_history
                   ~provider_config:response_provider_config
                   messages
               with
               | Error _ as error -> error
               | Ok messages ->
                 (try Ok (impl.build_body ~config ~messages ?tools ()) with
                  | Invalid_argument reason -> Error reason))
          in
          (match body_result with
           | Error reason ->
             let error =
               Error.Api
                 (Retry.InvalidRequest
                    { message = "Request rejected: " ^ reason
                    ; reason = Retry.Unknown_invalid_request
                    })
             in
             Error
               (Provider_failure_attribution.of_request_validation_error ~binding error)
           | Ok body_str ->
             let url = base_url ^ path in
             let provider_kind = response_provider_config.kind in
             let do_http_call () =
               (* Merge auth headers at request time via Provider_config so that
         [header_list] (from [Provider.resolve]) never carries sensitive tokens. *)
               let auth_hdrs =
                 Llm_provider.Provider_config.auth_headers_for_kind_and_key
                   ~kind:provider_kind
                   ~api_key
               in
               match
                 let clock = Option.map fst timeout_mode in
                 let timeout_s = Option.map snd timeout_mode in
                 Llm_provider.Http_client.post_sync
                   ?clock
                   ?timeout_s
                   ~sw
                   ~net
                   ~url
                   ~headers:(header_list @ auth_hdrs)
                   ~body:body_str
                   ()
               with
               | Ok (200, body_str) -> `Ok body_str
               | Ok (code, body_str) -> `HttpError (code, body_str)
               | Error err -> `TransportError (create_message_error_of_http_error err)
             in
             let do_request () =
               let latency_counter =
                 Llm_provider.Complete_common.start_latency_counter ()
               in
               let measured_latency_ms () =
                 Llm_provider.Complete_common.latency_ms_int latency_counter
               in
               let call_result =
                 match timeout_mode with
                 | None -> do_http_call ()
                 | Some (clock, request_timeout_s) ->
                   (try
                      Eio.Time.with_timeout_exn clock request_timeout_s do_http_call
                    with
                    | Eio.Time.Timeout ->
                      let message =
                        Printf.sprintf
                          "HTTP request exceeded %.1fs wall-clock timeout"
                          request_timeout_s
                      in
                      let http_error =
                        Llm_provider.Http_client.TimeoutError
                          { message; phase = Llm_provider.Http_client.Wall_clock }
                      in
                      `TransportError
                        (attributed_retry_error
                           http_error
                           (Retry.Timeout { message; phase = None })))
               in
               try
                 match call_result with
                 | `Ok body_str ->
                   let lat = measured_latency_ms () in
                   let raw_resp_result =
                     match request_handler with
                     | `Anthropic ->
                       parse_response (Yojson.Safe.from_string body_str)
                       |> ensure_nonempty_response
                     | `Openai ->
                       (* Reasoning stays typed as Thinking in the parsed response; it
                 is no longer promoted to a Text answer block (which caused the
                 #2236 CoT re-injection loop). Display surfacing is a read-side
                 projection concern, decoupled from parsing. *)
                       parse_openai_completion body_str
                     | `Custom impl ->
                       impl.parse_response body_str |> ensure_nonempty_response
                   in
                   Result.map
                     (fun resp ->
                        Llm_provider.Pricing.annotate_response_cost ~provider_id resp
                        |> fun response ->
                        Llm_provider.Complete_common.patch_telemetry
                          response
                          ~config:response_provider_config
                          lat)
                     raw_resp_result
                 | `HttpError (code, body_str) ->
                   Error
                     (create_message_error_of_http_error
                        (Llm_provider.Http_client.HttpError
                           { code; body = body_str; retry_after_header = None }))
                 | `TransportError err -> Error err
               with
               | Eio.Io _ as exn ->
                 Error
                   (create_message_error_of_http_error
                      (Llm_provider.Http_client.NetworkError
                         { message = Printexc.to_string exn; kind = Unknown }))
               | Unix.Unix_error _ as exn ->
                 Error
                   (create_message_error_of_http_error
                      (Llm_provider.Http_client.NetworkError
                         { message = Printexc.to_string exn; kind = Unknown }))
               (* Backend_gemini.Gemini_api_error and Backend_glm.Glm_api_error
       are intentionally NOT caught here: this function only
       dispatches [Anthropic_messages | Openai_chat_completions |
       Custom] (see the match on [kind] above), so the Gemini/Glm
       response parsers are never invoked on this path and those
       exceptions cannot reach here. They are caught at their real
       live site in [Llm_provider.Complete] — see
       lib/llm_provider/complete.ml:271,274. *)
               | Failure msg ->
                 Error
                   (Retry_error (Retry.NetworkError { message = msg; kind = Unknown }))
               | Yojson.Json_error msg ->
                 Error
                   (Retry_error
                      (Retry.InvalidRequest
                         { message = "JSON parse error: " ^ msg
                         ; reason = Retry.Json_parse_error
                         }))
               | Yojson.Safe.Util.Type_error (msg, _) ->
                 Error
                   (Retry_error
                      (Retry.InvalidRequest
                         { message = "JSON type error: " ^ msg
                         ; reason = Retry.Json_parse_error
                         }))
               | Yojson.Safe.Util.Undefined (msg, _) ->
                 Error
                   (Retry_error
                      (Retry.InvalidRequest
                         { message = "JSON undefined field error: " ^ msg
                         ; reason = Retry.Json_parse_error
                         }))
             in
             Result.map_error
               (detailed_error_of_create_message_error ~binding)
               (do_request ()))))
;;

let create_message
      ~sw
      ~net
      ~provider
      ?clock
      ?request_timeout_s
      ~config
      ~messages
      ?tools
      ?slot_id
      ()
  =
  create_message_detailed
    ~sw
    ~net
    ~provider
    ?clock
    ?request_timeout_s
    ~config
    ~messages
    ?tools
    ?slot_id
    ()
  |> Result.map_error (fun detailed -> detailed.Provider_failure_attribution.error)
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "re-exported default_base_url is non-empty" = String.length default_base_url > 0
let%test "re-exported api_version is non-empty" = String.length api_version > 0

let%test "Retry.Timeout classifies as retryable" =
  Retry.is_retryable
    (Retry.Timeout
       { message = "HTTP request exceeded 60.0s wall-clock timeout"; phase = None })
;;

let%test "empty completion with ContextWindowExceeded types as ContextOverflow" =
  match
    create_message_error_of_http_error
      (Llm_provider.Http_client.ProviderFailure
         { kind =
             Llm_provider.Http_client.Empty_completion
               { stop_reason = Llm_provider.Types.ContextWindowExceeded }
         ; message = "provider returned an empty assistant turn"
         })
  with
  | Attributed_retry_error { retry_error = Retry.ContextOverflow { limit = None; _ }; _ }
    -> true
  | _ -> false
;;

let%test "empty completion with other stop_reason stays a completion error" =
  match
    create_message_error_of_http_error
      (Llm_provider.Http_client.ProviderFailure
         { kind =
             Llm_provider.Http_client.Empty_completion
               { stop_reason = Llm_provider.Types.EndTurn }
         ; message = "provider returned an empty assistant turn"
         })
  with
  | Completion_error _ -> true
  | _ -> false
;;

let%test "re-exported max_response_body is positive" = max_response_body > 0
let%test "string_is_blank true for empty" = string_is_blank "" = true
let%test "string_is_blank true for spaces" = string_is_blank "   " = true
let%test "string_is_blank false for content" = string_is_blank "hello" = false

let json_shape_mismatch = function
  | `Assoc _ | `Bool _ | `Float _ | `Int _ | `Intlit _ | `List _ | `Null | `String _ ->
    false
;;

let%test "json_of_string_or_raw valid json" =
  match json_of_string_or_raw "{\"key\":\"val\"}" with
  | `Assoc [ ("key", `String "val") ] -> true
  | other -> json_shape_mismatch other
;;

let%test "json_of_string_or_raw invalid json returns raw assoc" =
  match json_of_string_or_raw "not json" with
  | `Assoc [ ("raw", `String "not json") ] -> true
  | other -> json_shape_mismatch other
;;

let%test "content_block_to_json text block" =
  let json = content_block_to_json (Types.Text "hello") in
  let open Yojson.Safe.Util in
  json |> member "type" |> to_string = "text"
  && json |> member "text" |> to_string = "hello"
;;

let%test "content_block_of_json text block" =
  let json = `Assoc [ "type", `String "text"; "text", `String "hi" ] in
  match content_block_of_json json with
  | Some (Types.Text "hi") -> true
  | None
  | Some
      ( Types.Text _
      | Types.Thinking _
      | Types.ReasoningDetails _
      | Types.RedactedThinking _
      | Types.Image _
      | Types.Document _
      | Types.Audio _
      | Types.ToolUse _
      | Types.ToolResult _ ) -> false
;;

let%test "message_to_json user message" =
  let msg : Types.message =
    { role = User
    ; content = [ Types.Text "test" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let json = message_to_json msg in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "user"
;;

let%test "text_blocks_to_string joins text" =
  let blocks = [ Types.Text "a"; Types.Text "b" ] in
  let result = text_blocks_to_string blocks in
  String.length result > 0
;;

(* --- Additional coverage tests for api.ml --- *)

let%test "string_is_blank tab only" = string_is_blank "\t" = true
let%test "string_is_blank newline only" = string_is_blank "\n" = true
let%test "string_is_blank mixed whitespace" = string_is_blank " \t\n " = true
let%test "string_is_blank single char" = string_is_blank "x" = false

let%test "json_of_string_or_raw empty string" =
  match json_of_string_or_raw "" with
  | `Assoc [ ("raw", `String "") ] -> true
  | other -> json_shape_mismatch other
;;

let%test "json_of_string_or_raw integer string" =
  match json_of_string_or_raw "42" with
  | `Int 42 -> true
  | other -> json_shape_mismatch other
;;

let%test "json_of_string_or_raw array" =
  match json_of_string_or_raw "[1,2,3]" with
  | `List _ -> true
  | other -> json_shape_mismatch other
;;

let%test "json_of_string_or_raw null" =
  match json_of_string_or_raw "null" with
  | `Null -> true
  | other -> json_shape_mismatch other
;;

let%test "content_block_to_json tool_use block" =
  let block = Types.ToolUse { id = "t1"; name = "fn"; input = `Assoc [ "x", `Int 1 ] } in
  let json = content_block_to_json block in
  let open Yojson.Safe.Util in
  json |> member "type" |> to_string = "tool_use"
;;

let%test "content_block_of_json tool_use block" =
  let json =
    `Assoc
      [ "type", `String "tool_use"
      ; "id", `String "t1"
      ; "name", `String "fn"
      ; "input", `Assoc [ "x", `Int 1 ]
      ]
  in
  match content_block_of_json json with
  | Some (Types.ToolUse { id = "t1"; name = "fn"; _ }) -> true
  | None
  | Some
      ( Types.Text _
      | Types.Thinking _
      | Types.ReasoningDetails _
      | Types.RedactedThinking _
      | Types.Image _
      | Types.Document _
      | Types.Audio _
      | Types.ToolUse _
      | Types.ToolResult _ ) -> false
;;

let%test "content_block_of_json tool_result" =
  let json =
    `Assoc
      [ "type", `String "tool_result"
      ; "tool_use_id", `String "t1"
      ; "content", `String "ok"
      ]
  in
  match content_block_of_json json with
  | Some (Types.ToolResult { tool_use_id = "t1"; content = "ok"; _ }) -> true
  | None
  | Some
      ( Types.Text _
      | Types.Thinking _
      | Types.ReasoningDetails _
      | Types.RedactedThinking _
      | Types.Image _
      | Types.Document _
      | Types.Audio _
      | Types.ToolUse _
      | Types.ToolResult _ ) -> false
;;

let%test "content_block_of_json unknown type" =
  let json = `Assoc [ "type", `String "unknown_type" ] in
  content_block_of_json json = None
;;

let%test "message_to_json assistant message" =
  let msg : Types.message =
    { role = Assistant
    ; content = [ Types.Text "response" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let json = message_to_json msg in
  let open Yojson.Safe.Util in
  json |> member "role" |> to_string = "assistant"
;;

let%test "text_blocks_to_string empty" = text_blocks_to_string [] = ""

let%test "text_blocks_to_string single" =
  let result = text_blocks_to_string [ Types.Text "only" ] in
  String.length result > 0
;;

let%test "text_blocks_to_string non-text blocks ignored" =
  let blocks =
    [ Types.ToolUse { id = "t"; name = "f"; input = `Null }; Types.Text "visible" ]
  in
  let result = text_blocks_to_string blocks in
  String.length result > 0
;;
