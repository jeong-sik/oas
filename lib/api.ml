(** API dispatch — re-exports provider modules and routes create_message *)

module Retry = Llm_provider.Retry
open Types
open Result_syntax

type response_accept = Types.api_response -> (unit, string) result

type create_message_error =
  | Retry_error of Retry.api_error
  | Completion_error of Llm_provider.Http_client.http_error

let create_message_error_of_http_error = function
  | Llm_provider.Http_client.HttpError { code; body } ->
    Retry_error (Retry.classify_error ~status:code ~body)
  | Llm_provider.Http_client.NetworkError
      { message; kind = Llm_provider.Http_client.Timeout } ->
    Retry_error (Retry.Timeout { message; phase = None })
  | Llm_provider.Http_client.NetworkError { message; kind } ->
    Retry_error (Retry.NetworkError { message; kind })
  | Llm_provider.Http_client.TimeoutError { message; phase } ->
    Retry_error (Retry.Timeout { message; phase = Some phase })
  | Llm_provider.Http_client.AcceptRejected { reason } ->
    Retry_error
      (Retry.InvalidRequest
         { message = "Response rejected: " ^ reason; reason = Unknown_invalid_request })
  | Llm_provider.Http_client.ProviderTerminal { message; _ } ->
    Retry_error (Retry.InvalidRequest { message; reason = Unknown_invalid_request })
  | Llm_provider.Http_client.ProviderFailure
      { kind = Llm_provider.Http_client.Empty_completion _; _ } as err ->
    Completion_error err
  | Llm_provider.Http_client.ProviderFailure { kind; message } ->
    Retry_error
      (Retry.InvalidRequest
         { message = Llm_provider.Http_client.provider_failure_to_string ~kind ~message
         ; reason = Unknown_invalid_request
         })
;;

let retry_error_of_create_message_error = function
  | Retry_error err -> Some err
  | Completion_error err -> Llm_provider.Retry_classify.classify_retry_error err
;;

let sdk_error_of_create_message_error = function
  | Retry_error err -> Error.Api err
  | Completion_error err -> Http_error_sdk.of_http_error err
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

let build_body_artifact ~config ~messages ?tools ~stream () =
  Api_anthropic.build_body_artifact ~config ~messages ?tools ~stream ()
;;

(* Re-export Api_openai *)
let openai_messages_of_message = Api_openai.openai_messages_of_message
let openai_content_parts_of_blocks = Api_openai.openai_content_parts_of_blocks
let build_openai_body_result = Api_openai.build_openai_body_result
let build_openai_body_artifact_result = Api_openai.build_openai_body_artifact_result
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

let parse_openai_responses_completion body_str =
  match Llm_provider.Backend_openai_responses.parse_response_result body_str with
  | Ok resp -> ensure_nonempty_response resp
  | Error message ->
    Error
      (Retry_error
         (Retry.InvalidRequest { message; reason = Retry.Unknown_invalid_request }))
;;

type request_plan =
  | Anthropic_request of Llm_provider.Provider_config.t
  | Openai_request of Llm_provider.Provider_config.t
  | Custom_request of Provider.provider_impl

let resolve_request_plan ~state ~base_url (provider_cfg : Provider.config) =
  match Provider.request_kind provider_cfg.provider with
  | Provider.Custom name ->
    (match Provider.find_provider name with
     | Some impl -> Ok (Custom_request impl)
     | None ->
       Error
         (Error.Config
            (InvalidConfig
               { field = "provider"
               ; detail = Printf.sprintf "Custom provider %S is no longer registered" name
               })))
  | Provider.Anthropic_messages ->
    Result.map
      (fun config -> Anthropic_request config)
      (Provider.provider_config_of_agent ~state ~base_url (Some provider_cfg))
  | Provider.Openai_chat_completions ->
    Result.map
      (fun config -> Openai_request config)
      (Provider.provider_config_of_agent ~state ~base_url (Some provider_cfg))
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
      ?on_output_token_receipt
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
    let* request_plan = resolve_request_plan ~state:config ~base_url provider_cfg in
    let path =
      match request_plan with
      | Anthropic_request config | Openai_request config -> config.request_path
      | Custom_request impl -> impl.request_path
    in
    let body_result =
      match request_plan with
      | Anthropic_request wire_config ->
        let artifact =
          Llm_provider.Backend_anthropic.build_request_with_receipt
            ~config:wire_config
            ~messages
            ?tools
            ()
        in
        Ok
          ( Llm_provider.Provider_request_artifact.payload artifact
          , Some (Llm_provider.Provider_request_artifact.output_token_receipt artifact) )
      | Openai_request _wire_config ->
        Result.map
          (fun artifact ->
             ( Llm_provider.Provider_request_artifact.payload artifact
             , Some (Llm_provider.Provider_request_artifact.output_token_receipt artifact)
             ))
          (Api_openai.build_openai_body_artifact_result
             ~provider_config:provider_cfg
             ~config
             ~messages
             ?tools
             ?slot_id
             ())
      | Custom_request impl -> Ok (impl.build_body ~config ~messages ?tools (), None)
    in
    (match body_result with
     | Error reason ->
       Error
         (Error.Api
            (Retry.InvalidRequest
               { message = "Request rejected: " ^ reason
               ; reason = Retry.Unknown_invalid_request
               }))
     | Ok (body_str, output_token_receipt) ->
       Option.iter
         (Llm_provider.Complete_common.emit_output_token_receipt on_output_token_receipt)
         output_token_receipt;
       let url = base_url ^ path in
       let do_http_call () =
         (* Merge auth headers at request time via Provider_config so that
         [header_list] (from [Provider.resolve]) never carries sensitive tokens. *)
         let auth_hdrs =
           match request_plan with
           | Anthropic_request config | Openai_request config ->
             Llm_provider.Provider_config.auth_headers_for_kind_and_key
               ~kind:config.kind
               ~api_key
           | Custom_request _ -> []
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
         | Error err -> `TransportError (create_message_error_of_http_error err)
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
               match request_plan with
               | Anthropic_request _ ->
                 Llm_provider.Backend_anthropic.parse_response
                   (Yojson.Safe.from_string body_str)
                 |> ensure_nonempty_response
               | Openai_request wire_config ->
                 (* Reasoning stays typed as Thinking in the parsed response; it
                 is no longer promoted to a Text answer block (which caused the
                 #2236 CoT re-injection loop). Display surfacing is a read-side
                 projection concern, decoupled from parsing. *)
                 if
                   Llm_provider.Provider_config.request_path_targets_responses_api
                     wire_config.request_path
                 then parse_openai_responses_completion body_str
                 else parse_openai_completion body_str
               | Custom_request impl ->
                 impl.parse_response body_str |> ensure_nonempty_response
             in
             Result.map
               (fun resp ->
                  let response = Llm_provider.Pricing.annotate_response_cost resp in
                  patch_latency response lat)
               raw_resp_result
           | `HttpError (code, body_str) ->
             Error (Retry_error (Retry.classify_error ~status:code ~body:body_str))
           | `TransportError err -> Error err
         with
         | Eio.Time.Timeout ->
           Error
             (Retry_error
                (Retry.Timeout
                   { message =
                       Printf.sprintf
                         "HTTP request exceeded %.1fs wall-clock timeout"
                         request_timeout_s
                   ; phase = None
                   }))
         | Eio.Io _ as exn ->
           Error
             (Retry_error
                (Retry.NetworkError { message = Printexc.to_string exn; kind = Unknown }))
         | Unix.Unix_error _ as exn ->
           Error
             (Retry_error
                (Retry.NetworkError { message = Printexc.to_string exn; kind = Unknown }))
         (* Backend_gemini.Gemini_api_error and Backend_glm.Glm_api_error
       are intentionally NOT caught here: this function only
       dispatches [Anthropic_messages | Openai_chat_completions |
       Custom] (see the match on [kind] above), so the Gemini/Glm
       response parsers are never invoked on this path and those
       exceptions cannot reach here. They are caught at their real
       live site in [Llm_provider.Complete] — see
       lib/llm_provider/complete.ml:271,274. *)
         | Failure msg ->
           Error (Retry_error (Retry.NetworkError { message = msg; kind = Unknown }))
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
       let result =
         match clock with
         | Some clock ->
           Retry.with_retry_map_error
             ~clock
             ?config:retry_config
             ~classify:retry_error_of_create_message_error
             do_request
         | None -> do_request ()
       in
       Result.map_error sdk_error_of_create_message_error result)
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
