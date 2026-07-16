type protocol =
  | Anthropic_messages_count_tokens
  | Openai_responses_input_tokens
  | Gemini_count_tokens
[@@deriving show, eq]

type count =
  { input_tokens : int
  ; protocol : protocol
  ; model_id : string
  }
[@@deriving show, eq]

type error =
  | Unsupported of
      { protocol : protocol
      ; model_id : string
      }
  | Transport of Http_client.http_error
  | Invalid_response of
      { protocol : protocol
      ; model_id : string
      ; detail : string
      }

let token_field = function
  | Anthropic_messages_count_tokens | Openai_responses_input_tokens -> "input_tokens"
  | Gemini_count_tokens -> "totalTokens"
;;

let invalid protocol model_id detail =
  Error (Invalid_response { protocol; model_id; detail })
;;

let nonnegative_int ~protocol ~model_id ~field = function
  | `Int value when value >= 0 -> Ok value
  | `Int _ -> invalid protocol model_id (Printf.sprintf "%s must be non-negative" field)
  | `Intlit _ ->
    invalid
      protocol
      model_id
      (Printf.sprintf "%s is outside the supported integer range" field)
  | json ->
    invalid
      protocol
      model_id
      (Printf.sprintf
         "%s must be an integer (got %s)"
         field
         (Json_util.json_type_name json))
;;

let decode_response ~protocol ~model_id body =
  match Json_util.decode_json_with Fun.id body with
  | Error detail -> invalid protocol model_id detail
  | Ok (`Assoc fields) ->
    let field = token_field protocol in
    (match List.assoc_opt field fields with
     | None ->
       invalid protocol model_id (Printf.sprintf "missing required field %s" field)
     | Some json ->
       Result.map
         (fun input_tokens -> { input_tokens; protocol; model_id })
         (nonnegative_int ~protocol ~model_id ~field json))
  | Ok json ->
    invalid
      protocol
      model_id
      (Printf.sprintf
         "input-token count response must be an object (got %s)"
         (Json_util.json_type_name json))
;;

let decode_transport_result ~protocol ~model_id = function
  | Error error -> Error (Transport error)
  | Ok body -> decode_response ~protocol ~model_id body
;;

let count_anthropic
      ?connection_cache
      ~sw
      ~net
      ~(config : Provider_config.t)
      ~messages
      ?(tools = [])
      ()
  =
  let protocol = Anthropic_messages_count_tokens in
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
             ~sw
             ~net
             ~url:(config.base_url ^ config.request_path ^ "/count_tokens")
             ~headers:(config.headers @ Provider_config.auth_headers_for_config config)
             ~body
             ()
         with
         | Error _ as error -> error
         | Ok (code, response) when code >= 200 && code < 300 -> Ok response
         | Ok (code, body) -> Error (Http_client.HttpError { code; body }))
    in
    decode_transport_result ~protocol ~model_id:config.model_id response_body
  | Provider_config.Kimi
  | Provider_config.OpenAI_compat
  | Provider_config.Ollama
  | Provider_config.Gemini
  | Provider_config.Glm
  | Provider_config.DashScope ->
    Error (Unsupported { protocol; model_id = config.model_id })
;;
