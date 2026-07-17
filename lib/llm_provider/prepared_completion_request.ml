type t = { request : Llm_transport.completion_request }
type identity = t

type measurement_evidence =
  { request_identity : identity
  ; measurement : Count_tokens_sync.completion_request_measurement
  }

type measured =
  { prepared : t
  ; evidence : measurement_evidence
  }

let prepare request = { request }

let prepare_sync ~config ~messages ?(tools = []) ?(trace_context = []) () =
  prepare
    { Llm_transport.config =
        Complete_common.config_with_trace_context config trace_context
    ; messages
    ; tools
    ; capture_id = None
    ; observe_wire_chunk = None
    ; stream_idle_timeout_s = None
    }
;;

let prepare_stream
      ~config
      ~messages
      ?(tools = [])
      ?(trace_context = [])
      ?capture_id
      ?stream_idle_timeout_s
      ()
  =
  prepare
    { Llm_transport.config =
        Complete_common.config_with_trace_context config trace_context
    ; messages
    ; tools
    ; capture_id
    ; observe_wire_chunk = None
    ; stream_idle_timeout_s
    }
;;

let request prepared = prepared.request
let identity prepared = prepared
let same_identity left right = left == right

let measure ?connection_cache ?clock ?timeout_s ~sw ~net prepared =
  Count_tokens_sync.measure_completion_request
    ?connection_cache
    ?clock
    ?timeout_s
    ~sw
    ~net
    prepared.request
  |> Result.map (fun measurement ->
    { prepared; evidence = { request_identity = prepared; measurement } })
;;

let measurement_evidence measured = measured.evidence

let inline_test_request () =
  let config =
    Provider_config.make
      ~kind:Provider_config.Anthropic
      ~model_id:"prepared-identity-test"
      ~base_url:"https://example.invalid"
      ~api_key:"test-key"
      ~max_tokens:1
      ()
  in
  { Llm_transport.config
  ; messages = []
  ; tools = []
  ; capture_id = None
  ; observe_wire_chunk = None
  ; stream_idle_timeout_s = None
  }
;;

let%test "prepare retains the exact request with allocation-specific identity" =
  let raw = inline_test_request () in
  let left = prepare raw in
  let right = prepare raw in
  request left == raw
  && identity left == left
  && not (same_identity (identity left) (identity right))
;;
