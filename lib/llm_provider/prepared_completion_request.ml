type t = { request : Llm_transport.completion_request }

type measured =
  { prepared : t
  ; measurement : Count_tokens_sync.completion_request_measurement
  }

type context_fit =
  { input_tokens : int
  ; reserved_output_tokens : int
  ; max_context_tokens : int
  }

type fit_error =
  | Context_limit_unknown of { model_id : string }
  | Invalid_context_limit of
      { model_id : string
      ; max_context_tokens : int
      }
  | Output_reservation_unknown of { model_id : string }
  | Context_window_exceeded of context_fit

type admitted =
  { measured : measured
  ; fit : context_fit
  }

let prepare
      ~config
      ~messages
      ?(tools = [])
      ?(trace_context = [])
      ?capture_id
      ?stream_idle_timeout_s
      ?first_event_timeout_s
      ()
  =
  { request =
      { Llm_transport.config =
          Complete_common.config_with_trace_context config trace_context
      ; messages
      ; tools
      ; capture_id
      ; observe_wire_chunk = None
      ; stream_idle_timeout_s
      ; first_event_timeout_s
      }
  }
;;

let request prepared = prepared.request

let measure ?connection_cache ?clock ?timeout_s ~sw ~net prepared =
  let config = prepared.request.Llm_transport.config in
  let measured () =
    Count_tokens_sync.measure_completion_request
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      prepared.request
    |> Result.map (fun measurement -> { prepared; measurement })
  in
  match Complete_common.validate_all config with
  | Error (Http_client.AcceptRejected { reason }) ->
    Error (Count_tokens_sync.Invalid_completion_request reason)
  | Error error ->
    Error (Count_tokens_sync.Input_count_failed (Input_token_count.Transport error))
  | Ok () -> Provider_admission.with_admission ~config measured
;;

let measurement measured = measured.measurement

(* Pure single source for the context-token limit. Uses only the caller-owned
   config and the exact model capability -- no network -- so a pre-knowable
   limit failure is decidable before any measurement round-trip. *)
let resolve_context_limit prepared =
  let config = prepared.request.config in
  let max_context =
    match config.max_context with
    | Some _ as explicit -> explicit
    | None ->
      Option.bind
        (Provider_config.capabilities_for_config_model config)
        (fun capabilities -> capabilities.Capabilities.max_context_tokens)
  in
  match max_context with
  | None -> Error (Context_limit_unknown { model_id = config.model_id })
  | Some max_context_tokens when max_context_tokens <= 0 ->
    Error (Invalid_context_limit { model_id = config.model_id; max_context_tokens })
  | Some max_context_tokens -> Ok max_context_tokens
;;

let admit ~max_context_tokens measured =
  let request = measured.prepared.request in
  let input_tokens = measured.measurement.input_count.input_tokens in
  let reserved_output_tokens =
    Types.output_token_receipt_effective measured.measurement.output_token_receipt
  in
  match reserved_output_tokens with
  | None ->
    (* [measure_completion_request] currently returns only a required receipt.
       Keep this branch total if a future provider protocol can report an
       optional output ceiling. *)
    Error (Output_reservation_unknown { model_id = request.config.model_id })
  | Some reserved_output_tokens ->
    let fit = { input_tokens; reserved_output_tokens; max_context_tokens } in
    if
      reserved_output_tokens > max_context_tokens
      || input_tokens > max_context_tokens - reserved_output_tokens
    then Error (Context_window_exceeded fit)
    else Ok { measured; fit }
;;

let admitted_request admitted = admitted.measured.prepared
let admitted_fit admitted = admitted.fit
