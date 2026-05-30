(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types.
    Each LLM backend should satisfy one of these.

    {b Compile-time guarantee}: attempting to pass a non-streaming
    provider as STREAMING_PROVIDER produces a type error. *)

module Http_client = Llm_provider.Http_client
module Retry = Llm_provider.Retry

let retry_error_of_http_error = function
  | Http_client.HttpError { code; body } -> Retry.classify_error ~status:code ~body
  | Http_client.NetworkError { message; kind = Http_client.Timeout } ->
    Retry.Timeout { message }
  | Http_client.NetworkError { message; kind } -> Retry.NetworkError { message; kind }
  | Http_client.TimeoutError { message; _ } -> Retry.Timeout { message }
  | Http_client.AcceptRejected { reason } ->
    Retry.InvalidRequest { message = "Response rejected: " ^ reason }
  | Http_client.ProviderTerminal { message; _ } -> Retry.InvalidRequest { message }
  | Http_client.ProviderFailure { kind; message } ->
    Retry.InvalidRequest
      { message = Http_client.provider_failure_to_string ~kind ~message }
;;

let parse_provider_d_response_result body_str =
  try Llm_provider.Backend_provider_d_parse.parse_provider_d_response_result body_str with
  | Yojson.Json_error msg | Yojson.Safe.Util.Type_error (msg, _) -> Error msg
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
    Returns [Some (module STREAMING_PROVIDER)] if native streaming is
    supported, [None] otherwise (caller should fall back to sync + synthetic). *)
let of_config (provider_cfg : Provider.config) : provider_module =
  let spec = Provider.model_spec_of_config provider_cfg in
  let base_url, api_key, headers =
    match Provider.resolve provider_cfg with
    | Ok (url, key, hdrs) -> url, key, hdrs
    | Error _ -> "", "", []
  in
  let module P = struct
    type t = unit

    let create_message ~sw ~net ~config ~messages ?tools () =
      let kind = spec.request_kind in
      let path = spec.request_path in
      let body_str =
        match kind with
        | Provider.Anthropic_messages ->
          Yojson.Safe.to_string
            (`Assoc
                (Api_provider_a.build_body_assoc
                   ~config
                   ~messages
                   ?tools
                   ~stream:false
                   ()))
        | Provider.Openai_chat_completions ->
          Api_provider_d.build_provider_d_body
            ~provider_config:provider_cfg
            ~config
            ~messages
            ?tools
            ()
        | Provider.Custom name ->
          (match Provider.find_provider name with
           | Some impl -> impl.build_body ~config ~messages ?tools ()
           | None -> Yojson.Safe.to_string (`Assoc []))
      in
      let url = base_url ^ path in
      (* Merge auth headers at request time so that [headers] (from
         [Provider.resolve]) never carries sensitive tokens. *)
      let auth_hdrs =
        if api_key = "" then []
        else match kind with
          | Provider.Anthropic_messages -> [ "x-api-key", api_key ]
          | Provider.Openai_chat_completions | Provider.Custom _ ->
            [ "Authorization", "Bearer " ^ api_key ]
      in
      match Http_client.post_sync ~sw ~net ~url ~headers:(headers @ auth_hdrs) ~body:body_str () with
      | Ok (200, body_str) ->
        (match kind with
         | Provider.Anthropic_messages ->
           Ok (Api_provider_a.parse_response (Yojson.Safe.from_string body_str))
         | Provider.Openai_chat_completions ->
           (match parse_provider_d_response_result body_str with
            | Ok resp -> Ok resp
            | Error msg -> Error (Error.Api (Retry.InvalidRequest { message = msg })))
         | Provider.Custom name ->
           (match Provider.find_provider name with
            | Some impl -> Ok (impl.parse_response body_str)
            | None ->
              (match parse_provider_d_response_result body_str with
               | Ok resp -> Ok resp
               | Error msg -> Error (Error.Api (Retry.InvalidRequest { message = msg })))))
      | Ok (code, body_str) ->
        Error (Error.Api (Retry.classify_error ~status:code ~body:body_str))
      | Error err -> Error (Error.Api (retry_error_of_http_error err))
    ;;
  end
  in
  (module P : PROVIDER)
;;

(** Check if a provider config supports native streaming. *)
let supports_streaming (provider_cfg : Provider.config) : bool =
  let caps = Provider.capabilities_for_config provider_cfg in
  caps.supports_native_streaming
;;

(** Resolve to a streaming provider if supported.
    Returns [Some] for Anthropic and Provider_d-compatible providers. *)
let of_config_streaming (provider_cfg : Provider.config)
  : streaming_provider_module option
  =
  if not (supports_streaming provider_cfg)
  then None
  else (
    let base_module = of_config provider_cfg in
    let module Base = (val base_module : PROVIDER) in
    let base_url =
      match Provider.resolve provider_cfg with
      | Ok (url, _, _) -> url
      | Error _ -> ""
    in
    let module SP = struct
      include Base

      let create_message_stream ~sw ~net ~config ~messages ?tools ~on_event () =
        Streaming.create_message_stream
          ~sw
          ~net
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
    Some (module SP : STREAMING_PROVIDER))
;;

[@@@coverage off]
(* === Inline tests === *)

let%test "supports_streaming Anthropic" =
  let cfg : Provider.config =
    { provider = Provider.Anthropic
    ; model_id = "agent_llm_a-3-5-sonnet-20241022"
    ; api_key_env = "PROVIDER_A_API_KEY"
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
    ; model_id = "provider_h-3.5"
    ; api_key_env = ""
    }
  in
  (* Provider_d-compat providers support streaming *)
  supports_streaming cfg
;;

let%test "of_config returns a provider_module" =
  let cfg : Provider.config =
    { provider = Provider.Anthropic
    ; model_id = "agent_llm_a-3-5-sonnet-20241022"
    ; api_key_env = "PROVIDER_A_API_KEY"
    }
  in
  let _m = of_config cfg in
  (* Just verify it doesn't raise *)
  true
;;

let%test "of_config_streaming Anthropic returns Some" =
  let cfg : Provider.config =
    { provider = Provider.Anthropic
    ; model_id = "agent_llm_a-3-5-sonnet-20241022"
    ; api_key_env = "PROVIDER_A_API_KEY"
    }
  in
  match of_config_streaming cfg with
  | Some _ -> true
  | None -> false
;;
