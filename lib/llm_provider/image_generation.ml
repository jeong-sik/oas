type source =
  | Remote_url of string
  | Inline_base64 of
      { media_type : string
      ; data : string
      }

type image = { source : source }

type usage =
  { input_tokens : int
  ; output_tokens : int
  ; total_tokens : int
  }

type filter_role =
  | User
  | Assistant
  | History
  | Other of string

type content_filter =
  { role : filter_role option
  ; level : int
  }

type response =
  { created_at : int option
  ; images : image list
  ; usage : usage option
  ; content_filter : content_filter list
  }

type protocol =
  | Openai_image
  | Zai_image

let reject reason = Error (Http_client.AcceptRejected { reason })

let parse_failure message =
  Error
    (Http_client.ProviderFailure
       { kind = Http_client.Provider_parse_error { parser = Some "image_generation" }
       ; message
       })
;;

let protocol_of_config (config : Provider_config.t) =
  match config.kind with
  | Provider_config.Glm -> Ok Zai_image
  | Provider_config.OpenAI_compat -> Ok Openai_image
  | Anthropic | Kimi | Ollama | Gemini | DashScope ->
    reject
      (Printf.sprintf
         "image generation has no wire implementation for provider kind %s"
         (Provider_config.string_of_provider_kind config.kind))
;;

let validate_declared_task (config : Provider_config.t) =
  match Provider_config.capabilities_for_config_model config with
  | Some { Capabilities.task = Some Capabilities.Image_generation; _ } -> Ok ()
  | Some { task = Some task; _ } ->
    reject
      (Printf.sprintf
         "model %S declares task %S, not image_generation"
         config.model_id
         (Capability_vocab.task_to_string task))
  | Some { task = None; _ } ->
    reject (Printf.sprintf "model %S does not declare an inference task" config.model_id)
  | None ->
    reject
      (Printf.sprintf
         "model %S has no exact catalog capability declaration"
         config.model_id)
;;

let validate_request config prompt =
  if String.trim prompt = ""
  then reject "image generation prompt must not be empty"
  else if String.trim config.Provider_config.model_id = ""
  then reject "image generation model_id must not be empty"
  else (
    match validate_declared_task config with
    | Error _ as error -> error
    | Ok () -> protocol_of_config config)
;;

let request_body ~protocol ~(config : Provider_config.t) ~prompt =
  let fields = [ "model", `String config.model_id; "prompt", `String prompt ] in
  let fields =
    match protocol with
    | Zai_image -> fields
    | Openai_image -> fields @ [ "output_format", `String "png" ]
  in
  `Assoc fields |> Yojson.Safe.to_string
;;

let source_of_json json =
  let open Yojson.Safe.Util in
  match member "url" json, member "b64_json" json with
  | `String url, `Null when String.trim url <> "" -> Ok (Remote_url url)
  | `Null, `String data when String.trim data <> "" ->
    Ok (Inline_base64 { media_type = "image/png"; data })
  | `String _, `String _ -> parse_failure "image item contains both url and b64_json"
  | _ -> parse_failure "image item must contain exactly one non-empty url or b64_json"
;;

let images_of_json json =
  match Yojson.Safe.Util.member "data" json with
  | `List [] -> parse_failure "image response data is empty"
  | `List items ->
    List.fold_left
      (fun acc item ->
         match acc, source_of_json item with
         | Ok images, Ok source -> Ok ({ source } :: images)
         | (Error _ as error), _ | _, (Error _ as error) -> error)
      (Ok [])
      items
    |> Result.map List.rev
  | _ -> parse_failure "image response data must be a non-empty list"
;;

let int_field name json =
  match Yojson.Safe.Util.member name json with
  | `Int value -> Ok value
  | _ -> parse_failure (Printf.sprintf "image response usage.%s must be an integer" name)
;;

let usage_of_json json =
  match Yojson.Safe.Util.member "usage" json with
  | `Null -> Ok None
  | `Assoc _ as usage_json ->
    (match int_field "input_tokens" usage_json with
     | Error _ as error -> error
     | Ok input_tokens ->
       (match int_field "output_tokens" usage_json with
        | Error _ as error -> error
        | Ok output_tokens ->
          (match int_field "total_tokens" usage_json with
           | Error _ as error -> error
           | Ok total_tokens -> Ok (Some { input_tokens; output_tokens; total_tokens }))))
  | _ -> parse_failure "image response usage must be an object"
;;

let created_at_of_json json =
  match Yojson.Safe.Util.member "created" json with
  | `Null -> Ok None
  | `Int value -> Ok (Some value)
  | _ -> parse_failure "image response created must be an integer"
;;

let filter_role_of_string = function
  | "user" -> User
  | "assistant" -> Assistant
  | "history" -> History
  | other -> Other other
;;

let content_filter_item json =
  match Yojson.Safe.Util.member "role" json, Yojson.Safe.Util.member "level" json with
  | `String role, `Int level when String.trim role <> "" && level >= 0 && level <= 3 ->
    Ok { role = Some (filter_role_of_string role); level }
  | `Null, `Int level when level >= 0 && level <= 3 -> Ok { role = None; level }
  | _ ->
    parse_failure
      "image response content_filter item requires level in [0,3] and an optional role"
;;

let content_filter_of_json json =
  match Yojson.Safe.Util.member "content_filter" json with
  | `Null -> Ok []
  | `List items ->
    List.fold_left
      (fun acc item ->
         match acc, content_filter_item item with
         | Ok observations, Ok observation -> Ok (observation :: observations)
         | (Error _ as error), _ | _, (Error _ as error) -> error)
      (Ok [])
      items
    |> Result.map List.rev
  | _ -> parse_failure "image response content_filter must be a list"
;;

let parse_response body =
  try
    let json = Yojson.Safe.from_string body in
    match created_at_of_json json with
    | Error _ as error -> error
    | Ok created_at ->
      (match images_of_json json with
       | Error _ as error -> error
       | Ok images ->
         (match usage_of_json json with
          | Error _ as error -> error
          | Ok usage ->
            (match content_filter_of_json json with
             | Error _ as error -> error
             | Ok content_filter -> Ok { created_at; images; usage; content_filter })))
  with
  | Yojson.Json_error message -> parse_failure ("invalid image response JSON: " ^ message)
;;

let generate
      ~sw
      ~net
      ?clock
      ?timeout_s
      ?connection_cache
      ~(config : Provider_config.t)
      ~prompt
      ()
  =
  match validate_request config prompt with
  | Error _ as error -> error
  | Ok protocol ->
    let url = config.base_url ^ config.request_path in
    let headers = config.headers @ Provider_config.auth_headers_for_config config in
    let body = request_body ~protocol ~config ~prompt in
    (match
       Http_client.post_sync
         ?cache:connection_cache
         ?clock
         ?timeout_s
         ~sw
         ~net
         ~url
         ~headers
         ~body
         ()
     with
     | Error _ as error -> error
     | Ok (code, response_body) when code >= 200 && code < 300 ->
       parse_response response_body
     | Ok (code, response_body) ->
       Error (Http_client.HttpError { code; body = response_body }))
;;

let test_caps task = { Capabilities.default_capabilities with task }

let test_config kind task =
  Provider_config.make
    ~kind
    ~provider_id:"test-image"
    ~model_id:"test-image-model"
    ~base_url:"https://example.invalid"
    ~request_path:"/images/generations"
    ~model_capabilities_override:(test_caps task)
    ()
;;

let catalog_declares_image_task provider_label model_id =
  match
    Capabilities.for_provider_model_id
      ~allow_bare_fallback:false
      ~provider_label
      ~model_id
  with
  | Some { task = Some Capabilities.Image_generation; _ } -> true
  | Some _ | None -> false
;;

let%test "embedded catalog declares exact image providers" =
  catalog_declares_image_task "zai-image" "glm-image"
  && catalog_declares_image_task "openai-image" "gpt-image-2"
;;

let%test "request requires an exact image-generation task" =
  match validate_request (test_config Provider_config.Glm None) "draw" with
  | Error (Http_client.AcceptRejected _) -> true
  | Ok _ | Error _ -> false
;;

let%test "Z.AI URL response and safety observation are typed" =
  match
    parse_response
      {|{"created":7,"data":[{"url":"https://example.test/a.png"}],"content_filter":[{"level":1},{"role":"assistant","level":2}]}|}
  with
  | Ok
      { created_at = Some 7
      ; images = [ { source = Remote_url url } ]
      ; usage = None
      ; content_filter =
          [ { role = None; level = 1 }; { role = Some Assistant; level = 2 } ]
      } -> String.equal url "https://example.test/a.png"
  | Ok _ | Error _ -> false
;;

let%test "OpenAI base64 response preserves usage" =
  match
    parse_response
      {|{"created":8,"data":[{"b64_json":"aGVsbG8="}],"usage":{"input_tokens":1,"output_tokens":2,"total_tokens":3}}|}
  with
  | Ok
      { images = [ { source = Inline_base64 { media_type = "image/png"; data } } ]
      ; usage = Some { input_tokens = 1; output_tokens = 2; total_tokens = 3 }
      ; content_filter = []
      ; _
      } -> String.equal data "aGVsbG8="
  | Ok _ | Error _ -> false
;;

let%test "ambiguous image source fails closed" =
  match parse_response {|{"data":[{"url":"https://x","b64_json":"eA=="}]}|} with
  | Error (Http_client.ProviderFailure _) -> true
  | Ok _ | Error _ -> false
;;
