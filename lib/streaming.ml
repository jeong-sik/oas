(** SSE streaming client for multi-provider LLM APIs.

    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Pure SSE event parsing and synthetic emission are delegated to
    {!Llm_provider.Streaming}. The HTTP streaming client remains here
    due to agent_state/Provider/Error coupling. *)

open Types

(* Re-export pure functions from llm_provider *)
let parse_sse_event = Llm_provider.Streaming.parse_sse_event
let emit_synthetic_events = Llm_provider.Streaming.emit_synthetic_events

(* ── Shared streaming accumulation ──────────────────────────── *)

type stream_acc = Llm_provider.Complete_stream_acc.stream_acc =
  { id : string ref
  ; model : string ref
  ; input_tokens : int ref
  ; output_tokens : int ref
  ; cache_creation : int ref
  ; cache_read : int ref
  ; stop_reason : stop_reason ref
  ; stop_reason_received : bool ref
  ; done_sentinel_seen : bool ref
  ; terminal_incomplete : bool ref
  ; sse_error : stream_error option ref
  ; block_texts : (int, Buffer.t) Hashtbl.t
  ; block_types : (int, string) Hashtbl.t
  ; block_tool_ids : (int, string) Hashtbl.t
  ; block_tool_names : (int, string) Hashtbl.t
  ; block_thinking_signatures : (int, Buffer.t) Hashtbl.t
  ; block_reasoning_details : (int, reasoning_detail list ref) Hashtbl.t
  ; block_media_types : (int, string) Hashtbl.t
  ; block_media_sources : (int, media_source_kind) Hashtbl.t
  }

let create_stream_acc = Llm_provider.Complete_stream_acc.create_stream_acc
let accumulate_event = Llm_provider.Complete_stream_acc.accumulate_event
let finalize_stream_acc = Llm_provider.Complete_stream_acc.finalize_stream_acc

(* ── HTTP error mapping ─────────────────────────────────────── *)

let map_http_error = Http_error_sdk.of_http_error

let map_stream_finalize_result = function
  | Error e -> Error (map_http_error e)
  | Ok result ->
    let result =
      Result.map_error Llm_provider.Complete_stream.http_error_of_stream_error result
      |> Llm_provider.Complete_common.ensure_nonempty_completion
    in
    (match result with
     | Ok resp -> Ok (Llm_provider.Pricing.annotate_response_cost resp)
     | Error err -> Error (map_http_error err))
;;

let%test "map_stream_finalize_result maps typed empty to provider unavailable (oas#2483)" =
  let empty_resp : api_response =
    { id = "m"
    ; model = "test"
    ; stop_reason = EndTurn
    ; content = []
    ; usage = None
    ; telemetry = None
    }
  in
  let max_tokens_resp = { empty_resp with stop_reason = MaxTokens } in
  let ok_resp = { empty_resp with content = [ Text "hi" ] } in
  let fails_closed response =
    match map_stream_finalize_result (Ok (Ok response)) with
    | Error (Error.Provider (Llm_provider.Error.ProviderUnavailable _)) -> true
    | Error _ | Ok _ -> false
  in
  (* A content-bearing completion still finalizes Ok: the guard fires only on
     the all-empty clean close, not on every stream. *)
  let nonempty_ok =
    match map_stream_finalize_result (Ok (Ok ok_resp)) with
    | Ok _ -> true
    | Error _ -> false
  in
  fails_closed empty_resp && fails_closed max_tokens_resp && nonempty_ok
;;

(** Streaming variant of create_message.
    Supports Anthropic (native SSE) and OpenAI-compatible (SSE).
    Custom providers fall back to sync + synthetic events.

    Does not accept retry_config: SSE streams deliver partial results
    incrementally; retrying mid-stream would discard data. *)
let create_message_stream
      ~sw
      ~net
      ?clock
      ?idle_timeout
      ?(base_url = Api.default_base_url)
      ?provider
      ~config
      ~messages
      ?tools
      ~on_event
      ?on_output_token_receipt
      ()
  : (api_response, Error.sdk_error) result
  =
  let resolve_result =
    match provider with
    | Some p ->
      (match Provider.resolve p with
       | Ok (url, key, _headers) -> Ok (p, url, key)
       | Error e -> Error e)
    | None ->
      (match Llm_provider.Cli_common_env.get "ANTHROPIC_API_KEY" with
       | Some key ->
         let fallback_provider : Provider.config =
           { provider = Provider.Anthropic
           ; model_id = model_to_string config.config.model
           ; api_key_env = "ANTHROPIC_API_KEY"
           }
         in
         Ok (fallback_provider, base_url, key)
       | None -> Error (Error.Config (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" })))
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, _api_key) ->
    (match Provider.request_kind provider_cfg.provider with
     | Provider.Anthropic_messages | Provider.Openai_chat_completions ->
       (match
          Provider.provider_config_of_agent ~state:config ~base_url (Some provider_cfg)
        with
        | Error _ as error -> error
        | Ok wire_config ->
          Llm_provider.Complete.complete_stream
            ~sw
            ~net
            ?clock
            ?stream_idle_timeout_s:idle_timeout
            ~config:wire_config
            ~messages
            ~tools:(Option.value tools ~default:[])
            ~on_event
            ?on_output_token_receipt
            ()
          |> Result.map_error map_http_error)
     | Provider.Custom _ ->
       (* Sync fallback: non-streaming call + synthetic events *)
       (match
          Api.create_message
            ~sw
            ~net
            ~base_url
            ~provider:provider_cfg
            ~config
            ~messages
            ?tools
            ?on_output_token_receipt
            ()
        with
        | Ok response ->
          emit_synthetic_events response on_event;
          Ok response
        | Error _ as e -> e))
;;
