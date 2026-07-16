type output_format =
  | Mp3
  | Opus
  | Aac
  | Flac
  | Wav
  | Pcm

type voice =
  | Named of string
  | Custom_id of string

type response =
  { format : output_format
  ; audio : string
  }

let reject reason = Error (Http_client.AcceptRejected { reason })

let parse_failure message =
  Error
    (Http_client.ProviderFailure
       { kind = Http_client.Provider_parse_error { parser = Some "speech_generation" }
       ; message
       })
;;

let validate_declared_task (config : Provider_config.t) =
  match Provider_config.capabilities_for_config_model config with
  | Some { Capabilities.task = Some Capabilities.Speech; _ } -> Ok ()
  | Some { task = Some task; _ } ->
    reject
      (Printf.sprintf
         "model %S declares task %S, not speech"
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

let validate_non_empty field value =
  if String.trim value = ""
  then reject (Printf.sprintf "speech generation %s must not be empty" field)
  else Ok ()
;;

let validate_request (config : Provider_config.t) text voice =
  let voice_value =
    match voice with
    | Named value | Custom_id value -> value
  in
  let ( let* ) = Result.bind in
  let* () = validate_non_empty "text" text in
  let* () = validate_non_empty "model_id" config.model_id in
  let* () = validate_non_empty "voice" voice_value in
  let* () = validate_declared_task config in
  match config.kind with
  | Provider_config.OpenAI_compat -> Ok ()
  | Anthropic | Kimi | Ollama | Gemini | Glm | DashScope ->
    reject
      (Printf.sprintf
         "speech generation has no wire implementation for provider kind %s"
         (Provider_config.string_of_provider_kind config.kind))
;;

let format_to_wire = function
  | Mp3 -> "mp3"
  | Opus -> "opus"
  | Aac -> "aac"
  | Flac -> "flac"
  | Wav -> "wav"
  | Pcm -> "pcm"
;;

let request_body ~(config : Provider_config.t) ~text ~voice ~format =
  let voice_json =
    match voice with
    | Named name -> `String name
    | Custom_id id -> `Assoc [ "id", `String id ]
  in
  `Assoc
    [ "model", `String config.model_id
    ; "input", `String text
    ; "voice", voice_json
    ; "response_format", `String (format_to_wire format)
    ]
  |> Yojson.Safe.to_string
;;

let generate
      ~sw
      ~net
      ?clock
      ?timeout_s
      ?connection_cache
      ~(config : Provider_config.t)
      ~text
      ~voice
      ~format
      ()
  =
  match validate_request config text voice with
  | Error _ as error -> error
  | Ok () ->
    let url = config.base_url ^ config.request_path in
    let headers = config.headers @ Provider_config.auth_headers_for_config config in
    let body = request_body ~config ~text ~voice ~format in
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
     | Ok (code, audio) when code >= 200 && code < 300 ->
       if audio = ""
       then parse_failure "OpenAI Speech API returned an empty body"
       else Ok { format; audio }
     | Ok (code, body) -> Error (Http_client.HttpError { code; body }))
;;

let test_caps task = { Capabilities.default_capabilities with task }

let test_config task =
  Provider_config.make
    ~kind:Provider_config.OpenAI_compat
    ~provider_id:"test-speech"
    ~model_id:"test-speech-model"
    ~base_url:"https://example.invalid"
    ~request_path:"/audio/speech"
    ~model_capabilities_override:(test_caps task)
    ()
;;

let%test "speech request requires exact catalog task" =
  match validate_request (test_config None) "hello" (Named "alloy") with
  | Error (Http_client.AcceptRejected _) -> true
  | Ok _ | Error _ -> false
;;

let%test "named voice request preserves exact model and format" =
  let config = test_config (Some Capabilities.Speech) in
  let json =
    request_body ~config ~text:"hello" ~voice:(Named "alloy") ~format:Wav
    |> Yojson.Safe.from_string
  in
  let open Yojson.Safe.Util in
  member "model" json = `String "test-speech-model"
  && member "voice" json = `String "alloy"
  && member "response_format" json = `String "wav"
;;

let%test "custom voice id remains a typed object" =
  let config = test_config (Some Capabilities.Speech) in
  let json =
    request_body ~config ~text:"hello" ~voice:(Custom_id "voice-1") ~format:Mp3
    |> Yojson.Safe.from_string
  in
  Yojson.Safe.Util.member "voice" json = `Assoc [ "id", `String "voice-1" ]
;;
