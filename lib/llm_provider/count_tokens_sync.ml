let count_tokens_url (config : Provider_config.t) =
  match String.index_opt config.request_path '?' with
  | None -> config.base_url ^ config.request_path ^ "/count_tokens"
  | Some query_start ->
    let path = String.sub config.request_path 0 query_start in
    let query =
      String.sub
        config.request_path
        query_start
        (String.length config.request_path - query_start)
    in
    config.base_url ^ path ^ "/count_tokens" ^ query
;;

type 'callback_error count_dispatch_error =
  | Count_failed of Input_token_count.error
  | Count_before_dispatch_failed of 'callback_error

let count_anthropic_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw:_
      ~net
      ~(config : Provider_config.t)
      ~messages
      ?(tools = [])
      ~before_dispatch
      ()
  =
  let protocol = Input_token_count.Anthropic_messages_count_tokens in
  match config.kind with
  | Provider_config.Anthropic | Provider_config.Kimi ->
    let request_body =
      try
        Ok (Backend_anthropic.build_count_tokens_request ~config ~messages ~tools ())
      with
      | Invalid_argument reason -> Error (Http_client.AcceptRejected { reason })
    in
    let result =
      match request_body with
      | Error error ->
        Error
          (Count_failed
             (Input_token_count.Transport error))
      | Ok body ->
        let callback_failure = ref None in
        let before_http_dispatch () =
          match before_dispatch () with
          | Ok () -> Ok ()
          | Error cause ->
            callback_failure := Some cause;
            Error
              (Http_client.AcceptRejected
                 { reason = "count-token durable dispatch fence rejected" })
        in
        let transport =
          Http_client.post_sync_once_with_evidence
            ?cache:connection_cache
            ?clock
            ?body_timeout_s:timeout_s
            ~before_dispatch:before_http_dispatch
            ~net
            ~url:(count_tokens_url config)
            ~headers:
              (config.headers
               @ Provider_config.auth_headers_for_config config
               @ [ "Content-Type", "application/json"
                 ; "Content-Length", string_of_int (String.length body)
                 ])
            ~body
            ()
        in
        (match !callback_failure, transport with
         | Some cause, _ -> Error (Count_before_dispatch_failed cause)
         | None, Error transport_error ->
           let error =
             match transport_error with
             | Http_client.Before_dispatch_error error
             | Http_client.Dispatch_started_error error
             | Http_client.Response_received_error { error; _ } -> error
           in
           Error
             (Count_failed
                (Input_token_count.Transport error))
         | None, Ok (response, _) ->
           let response_body =
             if response.status >= 200 && response.status < 300
             then Ok response.body
             else
               Error
                 (Http_client.HttpError
                    { code = response.status
                    ; body = response.body
                    ; retry_after_header = response.retry_after_header
                    })
           in
           Input_token_count.decode_transport_result
             ~protocol
             ~model_id:config.model_id
             response_body
           |> Result.map_error (fun error -> Count_failed error))
    in
    result
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope ->
    Error
      (Count_failed
         (Input_token_count.Unsupported { protocol; model_id = config.model_id }))
;;

let count_anthropic
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~config
      ~messages
      ?tools
      ()
  =
  match
    count_anthropic_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~config
      ~messages
      ?tools
      ~before_dispatch:(fun () -> Ok ())
      ()
  with
  | Ok count -> Ok count
  | Error (Count_failed error) -> Error error
  | Error (Count_before_dispatch_failed ()) -> assert false
;;

type completion_request_measurement =
  { input_count : Input_token_count.count
  ; output_token_receipt : Types.output_token_receipt
  }

type completion_request_error =
  | Input_count_failed of Input_token_count.error
  | Output_token_resolution_failed of Types.required_output_token_error
  | Invalid_completion_request of string

type 'callback_error completion_request_dispatch_error =
  | Completion_request_failed of completion_request_error
  | Before_dispatch_failed of 'callback_error

type exact_completion_measurement_request =
  { protocol : Input_token_count.protocol
  ; model_id : string
  ; url : string
  ; headers : (string * string) list
  ; body : string
  ; connect_timeout_s : float option
  ; body_timeout_s : float option
  ; output_token_receipt : Types.output_token_receipt
  }

let supports_completion_request_measurement (config : Provider_config.t) =
  match config.kind with
  | Provider_config.Anthropic | Provider_config.Kimi -> true
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope -> false
;;

let freeze_exact_completion_measurement_request
      ~anthropic_thinking_control
      ~serialized_request_body
      (request : Llm_transport.completion_request)
  =
  let config = request.config in
  let protocol = Input_token_count.Anthropic_messages_count_tokens in
  if supports_completion_request_measurement config
  then (
    try
      match
        Backend_anthropic.build_request_artifact_with_thinking_control
          ~stream:false
          ~anthropic_thinking_control
          ~config
          ~messages:request.messages
          ~tools:request.tools
          ()
      with
      | Error error -> Error (Output_token_resolution_failed error)
      | Ok generation_artifact ->
        if
          not
            (String.equal
               serialized_request_body
               (Backend_anthropic.request_payload generation_artifact))
        then
          Error
            (Invalid_completion_request
               "exact measurement artifact does not own the frozen generation body")
        else
          let body =
            Backend_anthropic.build_count_tokens_request
              ~config
              ~messages:request.messages
              ~tools:request.tools
              ()
          in
          Ok
            { protocol
            ; model_id = config.model_id
            ; url = count_tokens_url config
            ; headers =
                config.headers
                @ Provider_config.auth_headers_for_config config
                @ [ "Content-Type", "application/json"
                  ; "Content-Length", string_of_int (String.length body)
                  ]
            ; body
            ; connect_timeout_s = config.connect_timeout_s
            ; body_timeout_s = request.body_timeout_s
            ; output_token_receipt =
                Backend_anthropic.request_output_token_receipt generation_artifact
            }
    with
    | Invalid_argument detail -> Error (Invalid_completion_request detail))
  else
    Error
      (Input_count_failed
         (Input_token_count.Unsupported { protocol; model_id = config.model_id }))
;;

let measure_exact_completion_request_with_before_dispatch
      ?connection_cache
      ?clock
      ~net
      ~before_dispatch
      (request : exact_completion_measurement_request)
  =
  let callback_failure = ref None in
  let before_http_dispatch () =
    match before_dispatch () with
    | Ok () -> Ok ()
    | Error cause ->
      callback_failure := Some cause;
      Error
        (Http_client.AcceptRejected
           { reason = "exact measurement durable dispatch fence rejected" })
  in
  let transport =
    Http_client.post_sync_once_with_evidence
      ?cache:connection_cache
      ?clock
      ?connect_timeout_s:request.connect_timeout_s
      ?body_timeout_s:request.body_timeout_s
      ~before_dispatch:before_http_dispatch
      ~net
      ~url:request.url
      ~headers:request.headers
      ~body:request.body
      ()
  in
  match !callback_failure, transport with
  | Some cause, _ -> Error (Before_dispatch_failed cause)
  | None, Error transport_error ->
    let error =
      match transport_error with
      | Http_client.Before_dispatch_error error
      | Http_client.Dispatch_started_error error
      | Http_client.Response_received_error { error; _ } -> error
    in
    Error
      (Completion_request_failed
         (Input_count_failed (Input_token_count.Transport error)))
  | None, Ok (response, _) ->
    let response_body =
      if response.status >= 200 && response.status < 300
      then Ok response.body
      else
        Error
          (Http_client.HttpError
             { code = response.status
             ; body = response.body
             ; retry_after_header = response.retry_after_header
             })
    in
    Input_token_count.decode_transport_result
      ~protocol:request.protocol
      ~model_id:request.model_id
      response_body
    |> Result.map_error (fun error ->
      Completion_request_failed (Input_count_failed error))
    |> Result.map (fun input_count ->
      { input_count; output_token_receipt = request.output_token_receipt })
;;

let measure_completion_request_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~before_dispatch
      (request : Llm_transport.completion_request)
  =
  let config = request.config in
  if supports_completion_request_measurement config
  then (
    let output_token_receipt =
      Backend_anthropic.required_output_token_receipt config
      |> Result.map_error (fun error ->
        Completion_request_failed (Output_token_resolution_failed error))
    in
    match output_token_receipt with
    | Error _ as error -> error
    | Ok output_token_receipt ->
      (match
         count_anthropic_with_before_dispatch
           ?connection_cache
           ?clock
           ?timeout_s
           ~sw
           ~net
           ~config
           ~messages:request.messages
           ~tools:request.tools
           ~before_dispatch
           ()
       with
       | Error (Count_failed error) ->
         Error (Completion_request_failed (Input_count_failed error))
       | Error (Count_before_dispatch_failed error) ->
         Error (Before_dispatch_failed error)
       | Ok input_count ->
         Ok
           { input_count
           ; output_token_receipt
           }))
  else
    Error
      (Completion_request_failed
         (Input_count_failed
            (Input_token_count.Unsupported
               { protocol = Input_token_count.Anthropic_messages_count_tokens
               ; model_id = config.model_id
               })))
;;

let measure_completion_request
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      request
  =
  match
    measure_completion_request_with_before_dispatch
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~before_dispatch:(fun () -> Ok ())
      request
  with
  | Ok measurement -> Ok measurement
  | Error (Completion_request_failed error) -> Error error
  | Error (Before_dispatch_failed ()) -> assert false
;;
