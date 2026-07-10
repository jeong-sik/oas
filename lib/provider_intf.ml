(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types.
    Each LLM backend should satisfy one of these.

    {b Compile-time guarantee}: attempting to pass a non-streaming
    provider as STREAMING_PROVIDER produces a type error. *)

module Http_client = Llm_provider.Http_client
module Retry = Llm_provider.Retry

type response_error =
  | Retry_error of Retry.api_error
  | Completion_error of Http_client.http_error

let sdk_error_of_http_error = function
  | Http_client.ProviderFailure { kind = Http_client.Empty_completion _; _ } as err ->
    Http_error_sdk.of_http_error err
  | Http_client.HttpError { code; body } ->
    Error.Api (Retry.classify_error ~status:code ~body)
  | Http_client.NetworkError { message; kind = Http_client.Timeout } ->
    Error.Api (Retry.Timeout { message; phase = None })
  | Http_client.NetworkError { message; kind } ->
    Error.Api (Retry.NetworkError { message; kind })
  | Http_client.TimeoutError { message; phase } ->
    Error.Api (Retry.Timeout { message; phase = Some phase })
  | Http_client.AcceptRejected { reason } ->
    Error.Api
      (Retry.InvalidRequest
         { message = "Response rejected: " ^ reason; reason = Unknown_invalid_request })
  | Http_client.ProviderTerminal { message; _ } ->
    Error.Api (Retry.InvalidRequest { message; reason = Unknown_invalid_request })
  | Http_client.ProviderFailure { kind; message } ->
    Error.Api
      (Retry.InvalidRequest
         { message = Http_client.provider_failure_to_string ~kind ~message
         ; reason = Unknown_invalid_request
         })
;;

let sdk_error_of_response_error = function
  | Retry_error err -> Error.Api err
  | Completion_error err -> sdk_error_of_http_error err
;;

let ensure_nonempty_response resp =
  Llm_provider.Complete_common.ensure_nonempty_completion (Ok resp)
  |> Result.map_error (fun err -> Completion_error err)
;;

let parse_openai_response_result body_str =
  try
    match Llm_provider.Backend_openai_parse.parse_openai_response_result body_str with
    | Ok resp -> ensure_nonempty_response resp
    | Error (Llm_provider.Backend_openai_parse.Provider_error message) ->
      Error
        (Retry_error
           (Retry.InvalidRequest { message; reason = Retry.Unknown_invalid_request }))
    | Error (Llm_provider.Backend_openai_parse.Empty_completion empty) ->
      Error
        (Completion_error
           (Http_client.empty_completion_error ~stop_reason:empty.stop_reason))
  with
  | Yojson.Json_error msg ->
    Error
      (Retry_error
         (Retry.InvalidRequest
            { message = "JSON parse error: " ^ msg; reason = Retry.Json_parse_error }))
  | Yojson.Safe.Util.Type_error (msg, _) ->
    Error
      (Retry_error
         (Retry.InvalidRequest
            { message = "JSON type error: " ^ msg; reason = Retry.Json_parse_error }))
  | Yojson.Safe.Util.Undefined (msg, _) ->
    Error
      (Retry_error
         (Retry.InvalidRequest
            { message = "JSON undefined field error: " ^ msg
            ; reason = Retry.Json_parse_error
            }))
;;

(** Synchronous provider: can send a message and get a response. *)
module type PROVIDER = sig
  type t

  val create_message
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> unit
    -> (Types.api_response, Error.sdk_error) result
end

(** Streaming provider: extends PROVIDER with SSE streaming. *)
module type STREAMING_PROVIDER = sig
  include PROVIDER

  val create_message_stream
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> on_event:(Types.sse_event -> unit)
    -> unit
    -> (Types.api_response, Error.sdk_error) result
end

(** First-class packed provider. *)
type provider_module = (module PROVIDER)

type streaming_provider_module = (module STREAMING_PROVIDER)

(** Runtime dispatch: resolve a provider config to a first-class module.
    Returns an error if provider configuration or credentials cannot be
    resolved. *)
let of_config (provider_cfg : Provider.config) : (provider_module, Error.sdk_error) result
  =
  match Provider.resolve provider_cfg with
  | Error err -> Error err
  | Ok (base_url, api_key, headers) ->
    let spec = Provider.model_spec_of_config provider_cfg in
    let module P = struct
      type t = unit

      let create_message ~sw ~net ~config ~messages ?tools () =
        let kind = spec.request_kind in
        let path = spec.request_path in
        let body_result =
          match kind with
          | Provider.Anthropic_messages ->
            Ok
              (Yojson.Safe.to_string
                 (`Assoc
                     (Api_anthropic.build_body_assoc
                        ~config
                        ~messages
                        ?tools
                        ~stream:false
                        ())))
          | Provider.Openai_chat_completions ->
            Api_openai.build_openai_body_result
              ~provider_config:provider_cfg
              ~config
              ~messages
              ?tools
              ()
          | Provider.Custom name ->
            (match Provider.find_provider name with
             | Some impl -> Ok (impl.build_body ~config ~messages ?tools ())
             | None -> Ok (Yojson.Safe.to_string (`Assoc [])))
        in
        match body_result with
        | Error reason ->
          Error
            (Error.Api
               (Retry.InvalidRequest
                  { message = "Request rejected: " ^ reason
                  ; reason = Retry.Unknown_invalid_request
                  }))
        | Ok body_str ->
          let url = base_url ^ path in
          (* Merge auth headers at request time so that [headers] (from
         [Provider.resolve]) never carries sensitive tokens. *)
          let auth_hdrs =
            if api_key = ""
            then []
            else (
              match kind with
              | Provider.Anthropic_messages -> [ "x-api-key", api_key ]
              | Provider.Openai_chat_completions | Provider.Custom _ ->
                [ "Authorization", "Bearer " ^ api_key ])
          in
          (match
             Http_client.post_sync
               ~sw
               ~net
               ~url
               ~headers:(headers @ auth_hdrs)
               ~body:body_str
               ()
           with
           | Ok (200, body_str) ->
             let result =
               match kind with
               | Provider.Anthropic_messages ->
                 Api_anthropic.parse_response (Yojson.Safe.from_string body_str)
                 |> ensure_nonempty_response
               | Provider.Openai_chat_completions -> parse_openai_response_result body_str
               | Provider.Custom name ->
                 (match Provider.find_provider name with
                  | Some impl -> impl.parse_response body_str |> ensure_nonempty_response
                  | None -> parse_openai_response_result body_str)
             in
             Result.map_error sdk_error_of_response_error result
           | Ok (code, body_str) ->
             Error
               (sdk_error_of_http_error (Http_client.HttpError { code; body = body_str }))
           | Error err -> Error (sdk_error_of_http_error err))
      ;;
    end
    in
    Ok (module P : PROVIDER)
;;

(** Check if a provider config supports native streaming. *)
let supports_streaming (provider_cfg : Provider.config) : bool =
  let caps = Provider.capabilities_for_config provider_cfg in
  caps.supports_native_streaming
;;

(** Resolve to a streaming provider if supported.
    Returns [Ok (Some _)] when native streaming is supported, [Ok None]
    otherwise (caller should fall back to sync + synthetic). Returns an
    error if provider configuration or credentials cannot be resolved. *)
let of_config_streaming (provider_cfg : Provider.config)
  : (streaming_provider_module option, Error.sdk_error) result
  =
  if not (supports_streaming provider_cfg)
  then Ok None
  else (
    match of_config provider_cfg with
    | Error err -> Error err
    | Ok base_module ->
      let module Base = (val base_module : PROVIDER) in
      let base_url =
        match Provider.resolve provider_cfg with
        | Ok (url, _, _) -> url
        | Error _ -> ""
      in
      let module SP = struct
        include Base

        let create_message_stream
              ~sw
              ~net
              ?clock
              ?idle_timeout
              ~config
              ~messages
              ?tools
              ~on_event
              ()
          =
          Streaming.create_message_stream
            ~sw
            ~net
            ?clock
            ?idle_timeout
            ~base_url
            ~provider:provider_cfg
            ~config
            ~messages
            ?tools
            ~on_event
            ()
        ;;
      end
      in
      Ok (Some (module SP : STREAMING_PROVIDER)))
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "supports_streaming Anthropic" =
  let cfg : Provider.config =
    { provider = Provider.Anthropic
    ; model_id = "claude-3-5-sonnet-20241022"
    ; api_key_env = "ANTHROPIC_API_KEY"
    }
  in
  supports_streaming cfg = true
;;

let%test "supports_streaming OpenAICompat" =
  let cfg : Provider.config =
    { provider =
        Provider.OpenAICompat
          { base_url = Llm_provider.Constants.Endpoints.default_url_localhost
          ; auth_header = None
          ; path = "/v1/chat/completions"
          ; static_token = None
          }
    ; model_id = "dashscope-3.5"
    ; api_key_env = ""
    }
  in
  (* OpenAI-compat providers support streaming *)
  supports_streaming cfg
;;

let%test "of_config returns a provider_module" =
  match of_config (Provider.local_llm ()) with
  | Ok _ -> true
  | Error _ -> false
;;

let%test "of_config propagates resolve errors" =
  let cfg : Provider.config =
    { provider = Provider.Anthropic
    ; model_id = "claude-3-5-sonnet-20241022"
    ; api_key_env = "OAS_PROVIDER_INTF_NONEXISTENT_KEY"
    }
  in
  match of_config cfg with
  | Error (Error.Config (MissingEnvVar _)) -> true
  | Ok _ -> false
  | Error _ -> false
;;

let%test "of_config_streaming Local returns Some" =
  match of_config_streaming (Provider.local_llm ()) with
  | Ok (Some _) -> true
  | _ -> false
;;
