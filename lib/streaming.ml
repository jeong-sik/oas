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

type stream_acc =
  { msg_id : string ref
  ; msg_model : string ref
  ; input_tokens : int ref
  ; output_tokens : int ref
  ; cache_creation : int ref
  ; cache_read : int ref
  ; stop_reason : stop_reason ref
  ; stop_reason_received : bool ref
  ; sse_error : string option ref
  ; block_texts : (int, Buffer.t) Hashtbl.t
  ; block_types : (int, string) Hashtbl.t
  ; block_tool_ids : (int, string) Hashtbl.t
  ; block_tool_names : (int, string) Hashtbl.t
  ; block_thinking_signatures : (int, Buffer.t) Hashtbl.t
  }

let create_stream_acc () =
  { msg_id = ref ""
  ; msg_model = ref ""
  ; input_tokens = ref 0
  ; output_tokens = ref 0
  ; cache_creation = ref 0
  ; cache_read = ref 0
  ; stop_reason = ref EndTurn
  ; stop_reason_received = ref false
  ; sse_error = ref None
  ; block_texts = Hashtbl.create 4
  ; block_types = Hashtbl.create 4
  ; block_tool_ids = Hashtbl.create 4
  ; block_tool_names = Hashtbl.create 4
  ; block_thinking_signatures = Hashtbl.create 4
  }
;;

let accumulate_event (acc : stream_acc) = function
  | MessageStart { id; model; usage } ->
    acc.msg_id := id;
    acc.msg_model := model;
    (match usage with
     | Some u ->
       acc.input_tokens := u.input_tokens;
       acc.cache_creation := u.cache_creation_input_tokens;
       acc.cache_read := u.cache_read_input_tokens
     | None -> ())
  | ContentBlockStart { index; content_type; tool_id; tool_name } ->
    Hashtbl.replace acc.block_types index content_type;
    Hashtbl.replace acc.block_texts index (Buffer.create 64);
    (match tool_id with
     | Some id -> Hashtbl.replace acc.block_tool_ids index id
     | None -> ());
    (match tool_name with
     | Some n -> Hashtbl.replace acc.block_tool_names index n
     | None -> ())
  | ContentBlockDelta { index; delta } ->
    let buf =
      match Hashtbl.find_opt acc.block_texts index with
      | Some b -> b
      | None ->
        let b = Buffer.create 64 in
        Hashtbl.replace acc.block_texts index b;
        b
    in
    (match delta with
     | TextDelta s | ThinkingDelta s | InputJsonDelta s -> Buffer.add_string buf s
     | ThinkingSignatureDelta s ->
       let sig_buf =
         match Hashtbl.find_opt acc.block_thinking_signatures index with
         | Some b -> b
         | None ->
           let b = Buffer.create 256 in
           Hashtbl.replace acc.block_thinking_signatures index b;
           b
       in
       Buffer.add_string sig_buf s)
  | MessageDelta { stop_reason = sr; usage } ->
    (match sr with
     | Some r ->
       acc.stop_reason := r;
       acc.stop_reason_received := true
     | None -> ());
    (match usage with
     | Some u ->
       acc.output_tokens := u.output_tokens;
       if u.cache_creation_input_tokens > 0
       then acc.cache_creation := u.cache_creation_input_tokens;
       if u.cache_read_input_tokens > 0 then acc.cache_read := u.cache_read_input_tokens
     | None -> ())
  (* WORKAROUND: this secondary streaming surface ([create_message_stream] via
     [provider_intf], used by examples + e2e) keeps its own string [sse_error]
     carrier and still collapses to [NetworkError {Unknown}] at finalize. The
     typed-carrier fix landed on the primary completion path
     ([Complete.complete_stream] / [Complete_stream_acc]). Root fix: unify this
     duplicate accumulator onto [Complete_stream_acc] (follow-on). Here we only
     destructure the enriched [SSEError] payload to keep compiling. *)
  | SSEError { message; _ } -> acc.sse_error := Some message
  | SSEParseFailed { raw; reason } ->
    let preview =
      if String.length raw > 200 then String.sub raw 0 200 ^ "...(truncated)" else raw
    in
    acc.sse_error
    := Some (Printf.sprintf "sse_parse_failed: %s | chunk: %s" reason preview)
  | SSEUnknownEventType { event_type; raw } ->
    let preview =
      if String.length raw > 200 then String.sub raw 0 200 ^ "...(truncated)" else raw
    in
    acc.sse_error
    := Some (Printf.sprintf "sse_unknown_event_type: %s | chunk: %s" event_type preview)
  | MessageStop -> acc.stop_reason_received := true
  (* StreamIncomplete drives the partial-tool drop on the primary
     [Complete_stream_acc] path; this secondary accumulator (see WORKAROUND
     above) does not assemble/drop tool blocks here, so it is a no-op pending the
     same unification. *)
  | StreamIncomplete _ -> ()
  | Ping | ContentBlockStop _ | Connected | Timeout _ -> ()
;;

let finalize_stream_acc (acc : stream_acc) =
  match !(acc.sse_error) with
  | Some msg -> Error msg
  | None when not !(acc.stop_reason_received) ->
    Error "stream_terminated_without_stop_reason"
  | None ->
    let content =
      Hashtbl.fold
        (fun index ctype items ->
           let text =
             match Hashtbl.find_opt acc.block_texts index with
             | Some buf -> Buffer.contents buf
             | None -> ""
           in
           let block =
             match ctype with
             | "text" -> Some (Text text)
             | "thinking" ->
               let thinking_type =
                 match Hashtbl.find_opt acc.block_thinking_signatures index with
                 | Some buf when Buffer.length buf > 0 -> Buffer.contents buf
                 | Some _ | None -> ""
               in
               Some (Thinking { thinking_type; content = text })
             | "redacted_thinking" ->
               (match Hashtbl.find_opt acc.block_tool_ids index with
                | Some data when data <> "" -> Some (RedactedThinking data)
                | Some _ | None -> None)
             | "tool_use" ->
               let tool_id =
                 match Hashtbl.find_opt acc.block_tool_ids index with
                 | Some id -> id
                 | None -> ""
               in
               let tool_name =
                 match Hashtbl.find_opt acc.block_tool_names index with
                 | Some name -> name
                 | None -> ""
               in
               (try
                  Some
                    (ToolUse
                       { id = tool_id
                       ; name = tool_name
                       ; input = Yojson.Safe.from_string text
                       })
                with
                | Yojson.Json_error _ -> Some (Text text))
             | _ -> None
           in
           match block with
           | Some b -> (index, b) :: items
           | None -> items)
        acc.block_types
        []
      |> List.sort (fun (a, _) (b, _) -> compare a b)
      |> List.map snd
    in
    let usage =
      if
        !(acc.input_tokens) > 0
        || !(acc.output_tokens) > 0
        || !(acc.cache_creation) > 0
        || !(acc.cache_read) > 0
      then
        Some
          { input_tokens = !(acc.input_tokens)
          ; output_tokens = !(acc.output_tokens)
          ; cache_creation_input_tokens = !(acc.cache_creation)
          ; cache_read_input_tokens = !(acc.cache_read)
          ; cost_usd = None
          }
      else None
    in
    Ok
      { id = !(acc.msg_id)
      ; model = !(acc.msg_model)
      ; stop_reason = !(acc.stop_reason)
      ; content
      ; usage
      ; telemetry = None
      }
;;

(* ── HTTP error mapping ─────────────────────────────────────── *)

let map_http_error = Http_error_sdk.of_http_error

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
      (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
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
  | Ok (provider_cfg, base_url, api_key) ->
    (match Provider.request_kind provider_cfg.provider with
     | Provider.Anthropic_messages ->
       let headers =
         [ "Content-Type", "application/json"
         ; "x-api-key", api_key
         ; "anthropic-version", Api.api_version
         ]
       in
       let body_assoc = Api.build_body_assoc ~config ~messages ?tools ~stream:true () in
       let body = Yojson.Safe.to_string (`Assoc body_assoc) in
       let url = base_url ^ "/v1/messages" in
       (match
          Llm_provider.Http_client.with_post_stream
            ?clock
            ~net
            ~url
            ~headers
            ~body
            ~f:(fun reader ->
              let acc = create_stream_acc () in
              Llm_provider.Http_client.read_sse
                ?clock
                ?idle_timeout
                ~reader
                ~on_data:(fun ~event_type data ->
                  if data <> "[DONE]"
                  then (
                    match parse_sse_event event_type data with
                    | None ->
                      let evt =
                        SSEParseFailed
                          { raw = data; reason = "anthropic_sse_chunk_parse_failure" }
                      in
                      on_event evt;
                      accumulate_event acc evt
                    | Some evt ->
                      on_event evt;
                      accumulate_event acc evt))
                ();
              if !(acc.stop_reason_received) then on_event MessageStop;
              finalize_stream_acc acc)
            ()
        with
        | Error e -> Error (map_http_error e)
        | Ok (Ok resp) -> Ok (Llm_provider.Pricing.annotate_response_cost resp)
        | Ok (Error msg) ->
          Error
            (Error.Provider
               (Llm_provider.Error.of_http_error
                  (Llm_provider.Http_client.ProviderFailure
                     { kind =
                         Llm_provider.Http_client.Provider_parse_error
                           { parser = Some "sse_stream_accumulator" }
                     ; message = Printf.sprintf "SSE stream error: %s" msg
                     }))))
     | Provider.Openai_chat_completions ->
       (* OpenAI-compatible SSE streaming. *)
       let openai_compat_kind = Llm_provider.Provider_config.OpenAI_compat in
       let auth_headers =
         Provider.auth_headers_only_for_kind ~kind:openai_compat_kind ~api_key
       in
       let headers =
         match Provider.resolve provider_cfg with
         | Ok (_, _, h) -> h @ auth_headers
         | Error _ -> [ "Content-Type", "application/json" ] @ auth_headers
       in
       let stream_path = Provider.request_path provider_cfg.provider in
       (match
          Api_openai.build_openai_body_result
            ~provider_config:provider_cfg
            ~config
            ~messages
            ?tools
            ()
        with
        | Error reason ->
          Error
            (Error.Api
               (Llm_provider.Retry.InvalidRequest
                  { message = "Request rejected: " ^ reason
                  ; reason = Llm_provider.Retry.Unknown_invalid_request
                  }))
        | Ok body ->
          let body =
            body
            (* Streaming must request both SSE chunks and usage deltas so the
               accumulator can surface final token/cost metrics. *)
            |> Llm_provider.Http_client.inject_stream_and_options
          in
          let url = base_url ^ stream_path in
          (match
             Llm_provider.Http_client.with_post_stream
               ?clock
               ~net
               ~url
               ~headers
               ~body
               ~f:(fun reader ->
                 let acc = create_stream_acc () in
                 let oai_state = Llm_provider.Streaming.create_openai_stream_state () in
                 let msg_started = ref false in
                 Llm_provider.Http_client.read_sse
                   ?clock
                   ?idle_timeout
                   ~reader
                   ~on_data:(fun ~event_type:_ data ->
                     if data = "[DONE]"
                     then ()
                     else (
                       match Llm_provider.Streaming.parse_openai_sse_chunk data with
                       | None ->
                         let evt =
                           SSEParseFailed
                             { raw = data; reason = "openai_sse_chunk_parse_failure" }
                         in
                         on_event evt;
                         accumulate_event acc evt
                       | Some chunk ->
                         if not !msg_started
                         then (
                           msg_started := true;
                           let evt =
                             MessageStart
                               { id = chunk.chunk_id
                               ; model = chunk.chunk_model
                               ; usage = None
                               }
                           in
                           on_event evt;
                           accumulate_event acc evt);
                         let evs, _tel =
                           Llm_provider.Streaming.openai_chunk_to_events oai_state chunk
                         in
                         List.iter
                           (fun evt ->
                              on_event evt;
                              accumulate_event acc evt)
                           evs))
                   ();
                 if !(acc.stop_reason_received) then on_event MessageStop;
                 finalize_stream_acc acc)
               ()
           with
           | Error e -> Error (map_http_error e)
           | Ok (Ok resp) -> Ok (Llm_provider.Pricing.annotate_response_cost resp)
           | Ok (Error msg) ->
             Error
               (Error.Provider
                  (Llm_provider.Error.of_http_error
                     (Llm_provider.Http_client.ProviderFailure
                        { kind =
                            Llm_provider.Http_client.Provider_parse_error
                              { parser = Some "sse_stream_accumulator" }
                        ; message = Printf.sprintf "SSE stream error: %s" msg
                        })))))
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
            ()
        with
        | Ok response ->
          emit_synthetic_events response on_event;
          Ok response
        | Error _ as e -> e))
;;
