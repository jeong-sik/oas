(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types.
    Each LLM backend should satisfy one of these.

    {b Compile-time guarantee}: attempting to pass a non-streaming
    provider as STREAMING_PROVIDER produces a type error. *)

(** Synchronous provider: can send a message and get a response. *)
module type PROVIDER = sig
  type t

  val create_message :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    unit ->
    (Types.api_response, Error.sdk_error) result
end

(** Streaming provider: extends PROVIDER with SSE streaming. *)
module type STREAMING_PROVIDER = sig
  include PROVIDER

  val create_message_stream :
    sw:Eio.Switch.t ->
    net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t ->
    config:Types.agent_state ->
    messages:Types.message list ->
    ?tools:Yojson.Safe.t list ->
    on_event:(Types.sse_event -> unit) ->
    unit ->
    (Types.api_response, Error.sdk_error) result
end

(** First-class packed provider. *)
type provider_module = (module PROVIDER)
type streaming_provider_module = (module STREAMING_PROVIDER)

(** Runtime dispatch: resolve a provider config to a first-class module.
    Returns [Some (module STREAMING_PROVIDER)] if native streaming is
    supported, [None] otherwise (caller should fall back to sync + synthetic). *)
let of_config (provider_cfg : Provider.config) : provider_module =
  let spec = Provider.model_spec_of_config provider_cfg in
  let base_url, headers =
    match Provider.resolve provider_cfg with
    | Ok (url, _key, hdrs) -> (url, hdrs)
    | Error _ -> ("", [])
  in
  let module P = struct
    type t = unit

    let create_message ~sw ~net ~config ~messages ?tools () =
      let kind = spec.request_kind in
      let path = spec.request_path in
      let body_str =
        match kind with
        | Provider.Anthropic_messages ->
          Yojson.Safe.to_string (`Assoc (Api_anthropic.build_body_assoc ~config ~messages ?tools ~stream:false ()))
        | Provider.Openai_chat_completions ->
          Api_openai.build_openai_body ~provider_config:provider_cfg ~config ~messages ?tools ()
        | Provider.Custom name ->
          (match Provider.find_provider name with
           | Some impl -> impl.build_body ~config ~messages ?tools ()
           | None -> Yojson.Safe.to_string (`Assoc []))
      in
      let uri = Uri.of_string (base_url ^ path) in
      let https = Api_common.make_https () in
      let client = Cohttp_eio.Client.make ~https net in
      let hdr_list = headers in
      let hdr = Http.Header.of_list hdr_list in
      try
        let resp, body =
          Cohttp_eio.Client.post ~sw client ~headers:hdr
            ~body:(Cohttp_eio.Body.of_string body_str) uri
        in
        match Cohttp.Response.status resp with
        | `OK ->
          let body_str = Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body body |> take_all) in
          let response =
            match kind with
            | Provider.Anthropic_messages ->
              Api_anthropic.parse_response (Yojson.Safe.from_string body_str)
            | Provider.Openai_chat_completions ->
              Api_openai.parse_openai_response body_str
            | Provider.Custom name ->
              (match Provider.find_provider name with
               | Some impl -> impl.parse_response body_str
               | None -> Api_openai.parse_openai_response body_str)
          in
          Ok response
        | status ->
          let code = Cohttp.Code.code_of_status status in
          let body_str = Eio.Buf_read.(of_flow ~max_size:Api_common.max_response_body body |> take_all) in
          Error (Error.Api (Retry.classify_error ~status:code ~body:body_str))
      with
      | Eio.Io _ as exn ->
        Error (Error.Api (Retry.NetworkError { message = Printexc.to_string exn }))
      | Unix.Unix_error _ as exn ->
        Error (Error.Api (Retry.NetworkError { message = Printexc.to_string exn }))
      | Failure msg ->
        Error (Error.Api (Retry.NetworkError { message = msg }))
  end in
  (module P : PROVIDER)

(** Check if a provider config supports native streaming. *)
let supports_streaming (provider_cfg : Provider.config) : bool =
  let caps = Provider.capabilities_for_config provider_cfg in
  caps.supports_native_streaming

(** Resolve to a streaming provider if supported.
    Returns [Some] for Anthropic and OpenAI-compatible providers. *)
let of_config_streaming (provider_cfg : Provider.config)
    : streaming_provider_module option =
  if not (supports_streaming provider_cfg) then None
  else
    let base_module = of_config provider_cfg in
    let module Base = (val base_module : PROVIDER) in
    let base_url = match Provider.resolve provider_cfg with
      | Ok (url, _, _) -> url | Error _ -> ""
    in
    let module SP = struct
      include Base

      let create_message_stream ~sw ~net ~config ~messages ?tools
          ~on_event () =
        Streaming.create_message_stream ~sw ~net ~base_url
          ~provider:provider_cfg ~config ~messages ?tools ~on_event ()
    end in
    Some (module SP : STREAMING_PROVIDER)
