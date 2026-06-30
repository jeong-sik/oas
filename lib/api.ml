(** API dispatch — re-exports provider modules and routes create_message *)

module Retry = Llm_provider.Retry
open Types

type response_accept = Types.api_response -> (unit, string) result

let retry_error_of_http_error = function
  | Llm_provider.Http_client.HttpError { code; body } ->
    Retry.classify_error ~status:code ~body
  | Llm_provider.Http_client.NetworkError
      { message; kind = Llm_provider.Http_client.Timeout } ->
    Retry.Timeout { message; phase = None }
  | Llm_provider.Http_client.NetworkError { message; kind } ->
    Retry.NetworkError { message; kind }
  | Llm_provider.Http_client.TimeoutError { message; phase } ->
    Retry.Timeout { message; phase = Some phase }
  | Llm_provider.Http_client.AcceptRejected { reason } ->
    Retry.InvalidRequest
      { message = "Response rejected: " ^ reason; reason = Unknown_invalid_request }
  | Llm_provider.Http_client.ProviderTerminal { message; _ } ->
    Retry.InvalidRequest { message; reason = Unknown_invalid_request }
  | Llm_provider.Http_client.ProviderFailure { kind; message } ->
    Retry.InvalidRequest
      { message = Llm_provider.Http_client.provider_failure_to_string ~kind ~message
      ; reason = Unknown_invalid_request
      }
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

(* Re-export Api_openai *)
let openai_messages_of_message = Api_openai.openai_messages_of_message
let openai_content_parts_of_blocks = Api_openai.openai_content_parts_of_blocks
let build_openai_body_result = Api_openai.build_openai_body_result
let build_openai_body = Api_openai.build_openai_body

let parse_openai_response_result =
  Llm_provider.Backend_openai_parse.parse_openai_response_result
;;

(* Transport latency patch. Parser layers leave request_latency_ms unknown
   because they only see the JSON response body; only the transport layer
   can measure request latency. *)
let patch_latency (resp : Types.api_response) (latency_ms : int option)
  : Types.api_response
  =
  let telemetry =
    match resp.telemetry with
    | Some t -> Some { t with Llm_provider.Types.request_latency_ms = latency_ms }
    | None ->
      let default = Llm_provider.Types.default_inference_telemetry in
      Some { default with request_latency_ms = latency_ms }
  in
  { resp with telemetry }
;;

(** Send a non-streaming message to the API, dispatching by provider.
    When [clock] is supplied the HTTP request is wrapped in
    [Eio.Time.with_timeout_exn] using [request_timeout_s] (default
    [Api_common.default_request_timeout_s]); the resulting
    [Eio.Time.Timeout] is mapped to [Retry.Timeout] so [Retry.with_retry]
    can retry or surface the failure. Without a clock no timeout is
    applied (preserves backward-compatible call sites that run outside
    an Eio domain). *)
let create_message
      ~sw
      ~net
      ?(base_url = default_base_url)
      ?provider
      ?clock
      ?retry_config
      ?request_timeout_s
      ~config
      ~messages
      ?tools
      ?slot_id
      ()
  =
  let request_timeout_s =
    match request_timeout_s with
    | Some v -> v
    | None -> Api_common.default_request_timeout_s
  in
  let resolve_result =
    match provider with
    | Some p ->
      (match Provider.resolve p with
       | Ok (url, api_key, headers) -> Ok (p, url, api_key, headers)
       | Error e -> Error e)
    | None ->
      Error
        (Error.Config
           (MissingEnvVar
              { var_name =
                  "provider (Api.create_message no longer falls back to \
                   ANTHROPIC_API_KEY; pass an explicit provider)"
              }))
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, api_key, header_list) ->
    let model_spec = Provider.model_spec_of_config provider_cfg in
    let kind = model_spec.request_kind in
    let path = model_spec.request_path in
    let body_result =
      match kind with
      | Provider.Anthropic_messages ->
        Ok
          (Yojson.Safe.to_string
             (`Assoc (build_body_assoc ~config ~messages ?tools ~stream:false ())))
      | Provider.Openai_chat_completions ->
        Api_openai.build_openai_body_result
          ~provider_config:provider_cfg
          ~config
          ~messages
          ?tools
          ?slot_id
          ()
      | Provider.Custom name ->
        (match Provider.find_provider name with
         | Some impl -> Ok (impl.build_body ~config ~messages ?tools ())
         | None -> Ok (Yojson.Safe.to_string (`Assoc [])))
    in
    (match body_result with
     | Error reason ->
       Error
         (Error.Api
            (Retry.InvalidRequest
               { message = "Request rejected: " ^ reason
               ; reason = Retry.Unknown_invalid_request
               }))
     | Ok body_str ->
       let url = base_url ^ path in
       let provider_kind_of_request_kind = function
         | Provider.Anthropic_messages -> Llm_provider.Provider_config.Anthropic
         | Provider.Openai_chat_completions -> Llm_provider.Provider_config.OpenAI_compat
         | Provider.Custom _ -> Llm_provider.Provider_config.OpenAI_compat
       in
       let do_http_call () =
         (* Merge auth headers at request time via Provider_config so that
         [header_list] (from [Provider.resolve]) never carries sensitive tokens. *)
         let auth_hdrs =
           Llm_provider.Provider_config.auth_headers_for_kind_and_key
             ~kind:(provider_kind_of_request_kind kind)
             ~api_key
         in
         match
           Llm_provider.Http_client.post_sync
             ?clock
             ~timeout_s:request_timeout_s
             ~sw
             ~net
             ~url
             ~headers:(header_list @ auth_hdrs)
             ~body:body_str
             ()
         with
         | Ok (200, body_str) -> `Ok body_str
         | Ok (code, body_str) -> `HttpError (code, body_str)
         | Error err -> `TransportError (retry_error_of_http_error err)
       in
       let do_request () =
         let latency_counter = Llm_provider.Complete_common.start_latency_counter () in
         let measured_latency_ms () =
           Llm_provider.Complete_common.latency_ms_int latency_counter
         in
         try
           let call_result =
             match clock with
             | Some clk -> Eio.Time.with_timeout_exn clk request_timeout_s do_http_call
             | None -> do_http_call ()
           in
           match call_result with
           | `Ok body_str ->
             let lat = measured_latency_ms () in
             let raw_resp_result =
               match kind with
               | Provider.Anthropic_messages ->
                 Ok (parse_response (Yojson.Safe.from_string body_str))
               | Provider.Openai_chat_completions ->
                 (* Reasoning stays typed as Thinking in the parsed response; it
                 is no longer promoted to a Text answer block (which caused the
                 #2236 CoT re-injection loop). Display surfacing is a read-side
                 projection concern, decoupled from parsing. *)
                 parse_openai_response_result body_str
               | Provider.Custom name ->
                 (match Provider.find_provider name with
                  | Some impl -> Ok (impl.parse_response body_str)
                  | None -> parse_openai_response_result body_str)
             in
             (match raw_resp_result with
              | Ok resp ->
                Ok
                  (Llm_provider.Pricing.annotate_response_cost resp
                   |> fun r -> patch_latency r lat)
              | Error msg ->
                Error
                  (Retry.InvalidRequest
                     { message = msg; reason = Retry.Unknown_invalid_request }))
           | `HttpError (code, body_str) ->
             Error (Retry.classify_error ~status:code ~body:body_str)
           | `TransportError err -> Error err
         with
         | Eio.Time.Timeout ->
           Error
             (Retry.Timeout
                { message =
                    Printf.sprintf
                      "HTTP request exceeded %.1fs wall-clock timeout"
                      request_timeout_s
                ; phase = None
                })
         | Eio.Io _ as exn ->
           Error (Retry.NetworkError { message = Printexc.to_string exn; kind = Unknown })
         | Unix.Unix_error _ as exn ->
           Error (Retry.NetworkError { message = Printexc.to_string exn; kind = Unknown })
         (* Backend_gemini.Gemini_api_error and Backend_glm.Glm_api_error
       are intentionally NOT caught here: this function only
       dispatches [Anthropic_messages | Openai_chat_completions |
       Custom] (see the match on [kind] above), so the Gemini/Glm
       response parsers are never invoked on this path and those
       exceptions cannot reach here. They are caught at their real
       live site in [Llm_provider.Complete] — see
       lib/llm_provider/complete.ml:271,274. *)
         | Failure msg -> Error (Retry.NetworkError { message = msg; kind = Unknown })
         | Yojson.Json_error msg ->
           Error
             (Retry.InvalidRequest
                { message = "JSON parse error: " ^ msg; reason = Retry.Json_parse_error })
         | Yojson.Safe.Util.Type_error (msg, _) ->
           Error
             (Retry.InvalidRequest
                { message = "JSON type error: " ^ msg; reason = Retry.Json_parse_error })
         | Yojson.Safe.Util.Undefined (msg, _) ->
           Error
             (Retry.InvalidRequest
                { message = "JSON undefined field error: " ^ msg
                ; reason = Retry.Json_parse_error
                })
       in
       (match clock with
        | Some clock ->
          (match Retry.with_retry ~clock ?config:retry_config do_request with
           | Ok _ as success -> success
           | Error err -> Error (Error.Api err))
        | None ->
          (match do_request () with
           | Ok _ as success -> success
           | Error err -> Error (Error.Api err))))
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "re-exported default_base_url is non-empty" = String.length default_base_url > 0
let%test "re-exported api_version is non-empty" = String.length api_version > 0

let%test "default_request_timeout_s is positive" =
  Api_common.default_request_timeout_s > 0.0
;;

let%test "default_request_timeout_s is bounded to a reasonable ceiling" =
  (* Guards against accidental "set to a big number to mask a stall"
     regressions; 10 minutes is already well past any healthy LLM turn. *)
  Api_common.default_request_timeout_s <= 600.0
;;

let%test "Retry.Timeout classifies as retryable" =
  Retry.is_retryable
    (Retry.Timeout
       { message = "HTTP request exceeded 60.0s wall-clock timeout"; phase = None })
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

(* --- patch_latency tests --- *)

let%test "patch_latency creates telemetry when None with measured ms" =
  let resp : Types.api_response =
    { id = "r1"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let patched = patch_latency resp (Some 500) in
  match patched.telemetry with
  | Some t -> t.Llm_provider.Types.request_latency_ms = Some 500
  | None -> false
;;

let%test "patch_latency overwrites existing request_latency_ms" =
  let telemetry : Llm_provider.Types.inference_telemetry =
    { Llm_provider.Types.default_inference_telemetry with
      system_fingerprint = Some "fp"
    ; reasoning_tokens = Some 10
    ; request_latency_ms = None (* parser cannot observe transport latency *)
    ; provider_kind = Some Llm_provider.Provider_config.Anthropic
    ; canonical_model_id = Some "claude-4-sonnet"
    }
  in
  let resp : Types.api_response =
    { id = "r2"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = Some telemetry
    }
  in
  let patched = patch_latency resp (Some 1234) in
  match patched.telemetry with
  | Some t ->
    t.request_latency_ms = Some 1234
    && t.system_fingerprint = Some "fp" (* preserved *)
    && t.reasoning_tokens = Some 10 (* preserved *)
    && t.canonical_model_id = Some "claude-4-sonnet" (* preserved *)
  | None -> false
;;

let%test "patch_latency zero latency still patches" =
  let resp : Types.api_response =
    { id = "r3"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let patched = patch_latency resp (Some 0) in
  (* Even 0 gets wrapped in Some — not a no-op. Caller decides semantics. *)
  match patched.telemetry with
  | Some t -> t.request_latency_ms = Some 0
  | None -> false
;;

let%test "patch_latency preserves unknown latency" =
  let resp : Types.api_response =
    { id = "r4"
    ; model = "m"
    ; stop_reason = Types.EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let patched = patch_latency resp None in
  match patched.telemetry with
  | Some t -> t.request_latency_ms = None
  | None -> false
;;
