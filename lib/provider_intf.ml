(** Provider interface module types.

    Defines PROVIDER and STREAMING_PROVIDER as first-class module types.
    Each LLM backend should satisfy one of these.

    {b Compile-time guarantee}: attempting to pass a non-streaming
    provider as STREAMING_PROVIDER produces a type error. *)

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

module type DETAILED_PROVIDER = sig
  include PROVIDER

  val create_message_detailed
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> unit
    -> (Types.api_response, Provider_failure_attribution.detailed_error) result
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

module type DETAILED_STREAMING_PROVIDER = sig
  include STREAMING_PROVIDER

  val create_message_detailed
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> unit
    -> (Types.api_response, Provider_failure_attribution.detailed_error) result

  val create_message_stream_detailed
    :  sw:Eio.Switch.t
    -> net:[ `Generic | `Unix ] Eio.Net.ty Eio.Resource.t
    -> ?clock:_ Eio.Time.clock
    -> ?idle_timeout:float
    -> config:Types.agent_state
    -> messages:Types.message list
    -> ?tools:Yojson.Safe.t list
    -> on_event:(Types.sse_event -> unit)
    -> unit
    -> (Types.api_response, Provider_failure_attribution.detailed_error) result
end

(** First-class packed provider. *)
type provider_module = (module PROVIDER)

type detailed_provider_module = (module DETAILED_PROVIDER)
type streaming_provider_module = (module STREAMING_PROVIDER)
type detailed_streaming_provider_module = (module DETAILED_STREAMING_PROVIDER)

(** Runtime dispatch: resolve a provider config to a first-class module.
    Returns an error if provider configuration or credentials cannot be
    resolved. *)
let of_config_detailed (provider_cfg : Provider.config)
  : (detailed_provider_module, Provider_failure_attribution.detailed_error) result
  =
  match Provider.resolve provider_cfg with
  | Error error ->
    Error (Provider_failure_attribution.of_provider_configuration_error error)
  | Ok _resolved ->
    let module P = struct
      type t = unit

      let create_message_detailed ~sw ~net ~config ~messages ?tools () =
        Api.create_message_detailed
          ~sw
          ~net
          ~provider:provider_cfg
          ~config
          ~messages
          ?tools
          ()
      ;;

      let create_message ~sw ~net ~config ~messages ?tools () =
        create_message_detailed ~sw ~net ~config ~messages ?tools ()
        |> Result.map_error (fun detailed -> detailed.Provider_failure_attribution.error)
      ;;
    end
    in
    Ok (module P : DETAILED_PROVIDER)
;;

let of_config (provider_cfg : Provider.config) : (provider_module, Error.sdk_error) result
  =
  match of_config_detailed provider_cfg with
  | Error detailed -> Error detailed.error
  | Ok detailed_module ->
    let module P = (val detailed_module : DETAILED_PROVIDER) in
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
let of_config_streaming_detailed (provider_cfg : Provider.config)
  : ( detailed_streaming_provider_module option
      , Provider_failure_attribution.detailed_error )
      result
  =
  match of_config_detailed provider_cfg with
  | Error detailed -> Error detailed
  | Ok _ when not (supports_streaming provider_cfg) -> Ok None
  | Ok base_module ->
    let module Base = (val base_module : DETAILED_PROVIDER) in
    let module SP = struct
      include Base

      let create_message_stream_detailed
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
        Streaming.create_message_stream_detailed
          ~sw
          ~net
          ?clock
          ?idle_timeout
          ~provider:provider_cfg
          ~config
          ~messages
          ?tools
          ~on_event
          ()
      ;;

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
        create_message_stream_detailed
          ~sw
          ~net
          ?clock
          ?idle_timeout
          ~config
          ~messages
          ?tools
          ~on_event
          ()
        |> Result.map_error (fun detailed -> detailed.Provider_failure_attribution.error)
      ;;
    end
    in
    Ok (Some (module SP : DETAILED_STREAMING_PROVIDER))
;;

let of_config_streaming (provider_cfg : Provider.config)
  : (streaming_provider_module option, Error.sdk_error) result
  =
  match of_config_streaming_detailed provider_cfg with
  | Error detailed -> Error detailed.error
  | Ok None -> Ok None
  | Ok (Some detailed_module) ->
    let module SP = (val detailed_module : DETAILED_STREAMING_PROVIDER) in
    Ok (Some (module SP : STREAMING_PROVIDER))
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
