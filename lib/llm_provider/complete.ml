(** Standalone LLM completion: build → HTTP → parse.
    @since 0.46.0 *)

let complete ~sw ~net ~(config : Provider_config.t)
    ~(messages : Types.message list) ?(tools=[]) () =
  let body_str = match config.kind with
    | Provider_config.Anthropic ->
        Backend_anthropic.build_request ~config ~messages ~tools ()
    | Provider_config.OpenAI_compat ->
        Backend_openai.build_request ~config ~messages ~tools ()
  in
  let url = config.base_url ^ config.request_path in
  match Http_client.post_sync ~sw ~net ~url
          ~headers:config.headers ~body:body_str with
  | Error _ as e -> e
  | Ok (code, body) ->
      if code >= 200 && code < 300 then
        let response = match config.kind with
          | Provider_config.Anthropic ->
              Backend_anthropic.parse_response
                (Yojson.Safe.from_string body)
          | Provider_config.OpenAI_compat ->
              Backend_openai.parse_openai_response body
        in
        Ok response
      else
        Error (Http_client.HttpError { code; body })
