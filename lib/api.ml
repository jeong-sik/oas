(** API dispatch — re-exports provider modules and routes create_message *)

open Types

(* Re-export Api_common *)
let default_base_url = Api_common.default_base_url
let api_version = Api_common.api_version
let max_response_body = Api_common.max_response_body
let string_is_blank = Api_common.string_is_blank
let text_blocks_to_string = Api_common.text_blocks_to_string
let json_of_string_or_raw = Api_common.json_of_string_or_raw
let content_block_to_json = Api_common.content_block_to_json
let content_block_of_json = Api_common.content_block_of_json
let message_to_json = Api_common.message_to_json
let make_https = Api_common.make_https

(* Re-export Api_anthropic *)
let parse_response = Api_anthropic.parse_response
let build_body_assoc = Api_anthropic.build_body_assoc

(* Re-export Api_openai *)
let openai_messages_of_message = Api_openai.openai_messages_of_message
let openai_content_parts_of_blocks = Api_openai.openai_content_parts_of_blocks
let build_openai_body = Api_openai.build_openai_body
let parse_openai_response = Api_openai.parse_openai_response

(* Re-export Api_ollama *)
let parse_ollama_chat_response = Api_ollama.parse_ollama_chat_response
let parse_ollama_generate_response = Api_ollama.parse_ollama_generate_response

(** Send a non-streaming message to the API, dispatching by provider *)
let create_message ~sw ~net ?(base_url=default_base_url) ?provider ?clock ?retry_config ~config ~messages ?tools () =
  let resolve_result = match provider with
    | Some p ->
        (match Provider.resolve p with
         | Ok (url, _key, headers) -> Ok (p, url, headers)
         | Error e -> Error e)
    | None ->
        (match Sys.getenv_opt "ANTHROPIC_API_KEY" with
         | Some key ->
             Ok
               ( Provider.
                   {
                     provider = Anthropic;
                     model_id = model_to_string config.config.model;
                     api_key_env = "ANTHROPIC_API_KEY";
                   },
                 base_url,
                 [
                   ("Content-Type", "application/json");
                   ("x-api-key", key);
                   ("anthropic-version", api_version);
                 ] )
         | None -> Error (Error.Config (MissingEnvVar { var_name = "ANTHROPIC_API_KEY" })))
  in
  match resolve_result with
  | Error e -> Error e
  | Ok (provider_cfg, base_url, header_list) ->
  let headers = Http.Header.of_list header_list in
  let kind = Provider.request_kind provider_cfg.provider in
  let path = Provider.request_path provider_cfg.provider in
  let body_str =
    match kind with
    | Provider.Anthropic_messages ->
        Yojson.Safe.to_string (`Assoc (build_body_assoc ~config ~messages ?tools ~stream:false ()))
    | Provider.Openai_chat_completions ->
        Api_openai.build_openai_body ~config ~messages ?tools ()
    | Provider.Ollama_chat ->
        Api_ollama.build_ollama_chat_body ~config ~messages ?tools ()
    | Provider.Ollama_generate ->
        Api_ollama.build_ollama_generate_body ~config ~messages ()
  in
  let uri = Uri.of_string (base_url ^ path) in

  let https = make_https () in
  let client = Cohttp_eio.Client.make ~https net in
  let do_request () =
    try
      let resp, body = Cohttp_eio.Client.post ~sw client ~headers ~body:(Cohttp_eio.Body.of_string body_str) uri in
      match Cohttp.Response.status resp with
      | `OK ->
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          let response =
            match kind with
            | Provider.Anthropic_messages ->
                parse_response (Yojson.Safe.from_string body_str)
            | Provider.Openai_chat_completions ->
                parse_openai_response body_str
            | Provider.Ollama_chat ->
                parse_ollama_chat_response body_str
            | Provider.Ollama_generate ->
                parse_ollama_generate_response body_str
          in
          Ok response
      | status ->
          let code = Cohttp.Code.code_of_status status in
          let body_str = Eio.Buf_read.(of_flow ~max_size:max_response_body body |> take_all) in
          Error (Retry.classify_error ~status:code ~body:body_str)
    with exn ->
      Error (Retry.NetworkError { message = Printexc.to_string exn })
  in
  match clock with
  | Some clock ->
      (match Retry.with_retry ~clock ?config:retry_config do_request with
       | Ok _ as success -> success
       | Error err -> Error (Error.Api err))
  | None ->
      (match do_request () with
       | Ok _ as success -> success
       | Error err -> Error (Error.Api err))
