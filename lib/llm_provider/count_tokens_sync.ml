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

let count_anthropic
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      ~(config : Provider_config.t)
      ~messages
      ?(tools = [])
      ()
  =
  let protocol = Input_token_count.Anthropic_messages_count_tokens in
  match config.kind with
  | Provider_config.Anthropic ->
    let request_body =
      try
        Ok (Backend_anthropic.build_count_tokens_request ~config ~messages ~tools ())
      with
      | Invalid_argument reason -> Error (Http_client.AcceptRejected { reason })
    in
    let response_body =
      match request_body with
      | Error _ as error -> error
      | Ok body ->
        (match
           Http_client.post_sync
             ?cache:connection_cache
             ?clock
             ?timeout_s
             ~sw
             ~net
             ~url:(count_tokens_url config)
             ~headers:(config.headers @ Provider_config.auth_headers_for_config config)
             ~body
             ()
         with
         | Error _ as error -> error
         | Ok (code, response) when code >= 200 && code < 300 -> Ok response
         | Ok (code, body) ->
           Error (Http_client.HttpError { code; body; retry_after_header = None }))
    in
    Input_token_count.decode_transport_result
      ~protocol
      ~model_id:config.model_id
      response_body
  | Provider_config.Kimi
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope ->
    Error (Input_token_count.Unsupported { protocol; model_id = config.model_id })
;;

type completion_request_measurement =
  { input_count : Input_token_count.count
  ; output_token_receipt : Types.output_token_receipt
  }

type completion_request_error =
  | Input_count_failed of Input_token_count.error
  | Output_token_resolution_failed of Types.required_output_token_error
  | Invalid_completion_request of string

let supports_completion_request_measurement (config : Provider_config.t) =
  match config.kind with
  | Provider_config.Anthropic -> true
  | Provider_config.Kimi
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope -> false
;;

let measure_completion_request
      ?connection_cache
      ?clock
      ?timeout_s
      ~sw
      ~net
      (request : Llm_transport.completion_request)
  =
  let config = request.config in
  if supports_completion_request_measurement config
  then (
    let artifact =
      try
        Backend_anthropic.build_request_artifact
          ~config
          ~messages:request.messages
          ~tools:request.tools
          ()
        |> Result.map_error (fun error -> Output_token_resolution_failed error)
      with
      | Invalid_argument detail -> Error (Invalid_completion_request detail)
    in
    match artifact with
    | Error _ as error -> error
    | Ok artifact ->
      (match
         count_anthropic
           ?connection_cache
           ?clock
           ?timeout_s
           ~sw
           ~net
           ~config
           ~messages:request.messages
           ~tools:request.tools
           ()
       with
       | Error error -> Error (Input_count_failed error)
       | Ok input_count ->
         Ok
           { input_count
           ; output_token_receipt =
               Backend_anthropic.request_output_token_receipt artifact
           }))
  else
    Error
      (Input_count_failed
         (Input_token_count.Unsupported
            { protocol = Input_token_count.Anthropic_messages_count_tokens
            ; model_id = config.model_id
            }))
;;
