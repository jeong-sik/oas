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

type source =
  | Raw_bytes of string
  | Inline_base64 of
      { media_type : string
      ; data : string
      }
  | Remote_url of string

type audio =
  { format : output_format
  ; source : source
  ; sample_rate : int option
  ; channels : int option
  }

type usage =
  { input_tokens : int option
  ; output_tokens : int option
  ; total_tokens : int option
  ; cached_tokens : int option
  ; thought_tokens : int option
  ; tool_use_tokens : int option
  }

type response =
  { provider_response_id : string option
  ; created_at_rfc3339 : string option
  ; audios : audio list
  ; usage : usage option
  }

type protocol =
  | Openai_speech
  | Gemini_interaction

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
  | Provider_config.OpenAI_compat -> Ok Openai_speech
  | Provider_config.Gemini -> Ok Gemini_interaction
  | Anthropic | Kimi | Ollama | Glm | DashScope ->
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

(* Single source for the Gemini Interactions audio MIME vocabulary
   (documented enum at ai.google.dev/api/interactions-api — Opus is
   "audio/opus", not "audio/ogg_opus"); both wire directions derive from
   this list so they cannot drift apart. *)
let gemini_supported_formats =
  [ Mp3, "audio/mp3"; Opus, "audio/opus"; Wav, "audio/wav"; Pcm, "audio/l16" ]
;;

(* Documented near-synonyms Gemini may return for a format we requested. *)
let gemini_response_aliases = [ "audio/mpeg", Mp3 ]

let gemini_media_type format =
  match List.assoc_opt format gemini_supported_formats with
  | Some media_type -> Ok media_type
  | None -> reject "Gemini Interactions TTS does not support the requested format"
;;

(* MIME types are case-insensitive and may carry parameters (RFC 2045);
   Gemini surfaces return forms like "audio/L16;codec=pcm;rate=24000".
   Compare on the case-folded type/subtype only. *)
let media_type_essence value =
  let without_parameters =
    match String.index_opt value ';' with
    | None -> value
    | Some semicolon -> String.sub value 0 semicolon
  in
  String.lowercase_ascii (String.trim without_parameters)
;;

let format_of_media_type value =
  let essence = media_type_essence value in
  let canonical =
    List.find_opt (fun (_, mime) -> String.equal mime essence) gemini_supported_formats
  in
  match canonical, List.assoc_opt essence gemini_response_aliases with
  | Some (format, _), _ | None, Some format -> Ok format
  | None, None ->
    parse_failure (Printf.sprintf "unsupported Gemini audio MIME %S" value)
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

let gemini_request_body ~(config : Provider_config.t) ~text ~voice ~format =
  match voice with
  | Custom_id _ -> reject "Gemini Interactions TTS does not accept a custom voice id"
  | Named name ->
    Result.map
      (fun media_type ->
         `Assoc
           [ "model", `String config.model_id
           ; "input", `String text
           ; "store", `Bool false
           ; ( "response_format"
             , `Assoc
                 [ "type", `String "audio"
                 ; "delivery", `String "inline"
                 ; "mime_type", `String media_type
                 ] )
           ; ( "generation_config"
             , `Assoc [ "speech_config", `List [ `Assoc [ "voice", `String name ] ] ] )
           ]
         |> Yojson.Safe.to_string)
      (gemini_media_type format)
;;

let optional_string name json =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value <> "" -> Ok (Some value)
  | _ -> parse_failure (Printf.sprintf "Gemini interaction %s must be non-empty" name)
;;

let required_string name json =
  match optional_string name json with
  | Ok (Some value) -> Ok value
  | Ok None | Error _ ->
    parse_failure (Printf.sprintf "Gemini interaction %s is required" name)
;;

let optional_int name json =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok None
  | `Int value -> Ok (Some value)
  | _ -> parse_failure (Printf.sprintf "Gemini interaction %s must be an integer" name)
;;

let usage_of_json json =
  match Yojson.Safe.Util.member "usage" json with
  | `Null -> Ok None
  | `Assoc _ as usage ->
    let ( let* ) = Result.bind in
    let* input_tokens = optional_int "total_input_tokens" usage in
    let* output_tokens = optional_int "total_output_tokens" usage in
    let* total_tokens = optional_int "total_tokens" usage in
    let* cached_tokens = optional_int "total_cached_tokens" usage in
    let* thought_tokens = optional_int "total_thought_tokens" usage in
    let* tool_use_tokens = optional_int "total_tool_use_tokens" usage in
    Ok
      (Some
         { input_tokens
         ; output_tokens
         ; total_tokens
         ; cached_tokens
         ; thought_tokens
         ; tool_use_tokens
         })
  | _ -> parse_failure "Gemini interaction usage must be an object"
;;

let audio_of_json expected json =
  let open Yojson.Safe.Util in
  let ( let* ) = Result.bind in
  let* media_type =
    match member "mime_type" json with
    | `String value when String.trim value <> "" -> Ok value
    | _ -> parse_failure "Gemini audio requires a MIME type"
  in
  let* format = format_of_media_type media_type in
  let* () =
    if format = expected
    then Ok ()
    else parse_failure "Gemini audio format differs from the requested format"
  in
  let* source =
    match member "data" json, member "uri" json with
    | `String data, `Null when String.trim data <> "" ->
      Ok (Inline_base64 { media_type; data })
    | `Null, `String uri when String.trim uri <> "" -> Ok (Remote_url uri)
    | `String _, `String _ -> parse_failure "Gemini audio contains both data and uri"
    | _ -> parse_failure "Gemini audio requires exactly one non-empty data or uri"
  in
  let* sample_rate = optional_int "sample_rate" json in
  let* channels = optional_int "channels" json in
  Ok { format; source; sample_rate; channels }
;;

let audios_of_json expected json =
  let open Yojson.Safe.Util in
  let add_content acc content =
    match member "type" content with
    | `String "audio" ->
      Result.map (fun audio -> audio :: acc) (audio_of_json expected content)
    | `String kind ->
      parse_failure (Printf.sprintf "unexpected Gemini speech content %S" kind)
    | _ -> parse_failure "Gemini speech content requires a type"
  in
  let add_step acc step =
    match member "type" step with
    | `String "thought" -> Ok acc
    | `String "model_output" ->
      (match member "content" step with
       | `List items ->
         List.fold_left
           (fun result item -> Result.bind result (fun acc -> add_content acc item))
           (Ok acc)
           items
       | _ -> parse_failure "Gemini model_output content must be a list")
    | `String kind ->
      parse_failure (Printf.sprintf "unexpected Gemini speech step %S" kind)
    | _ -> parse_failure "Gemini speech step requires a type"
  in
  match member "steps" json with
  | `List steps ->
    let result =
      List.fold_left
        (fun result step -> Result.bind result (fun acc -> add_step acc step))
        (Ok [])
        steps
    in
    Result.bind result (function
      | [] -> parse_failure "Gemini interaction returned no audio"
      | audios -> Ok (List.rev audios))
  | _ -> parse_failure "Gemini interaction steps must be a list"
;;

(* A well-formed interaction whose status is not "completed" is a
   provider-reported outcome, not a parser defect: keep it out of
   Provider_parse_error so parse-error alarms stay meaningful. *)
let gemini_status_failure status =
  Error
    (Http_client.ProviderFailure
       { kind =
           Http_client.Unknown_provider_failure
             { reason = Some (Printf.sprintf "gemini interaction status %s" status) }
       ; message =
           Printf.sprintf "Gemini interaction finished with status %S, not completed" status
       })
;;

let parse_gemini_response expected body =
  let decode json =
    let ( let* ) = Result.bind in
    let* provider_response_id = required_string "id" json in
    let* status = required_string "status" json in
    let* () = if String.equal status "completed" then Ok () else gemini_status_failure status in
    let* created_at_rfc3339 = optional_string "created" json in
    let* audios = audios_of_json expected json in
    let* usage = usage_of_json json in
    Ok
      { provider_response_id = Some provider_response_id
      ; created_at_rfc3339
      ; audios
      ; usage
      }
  in
  match Json_util.decode_json_with decode body with
  | Ok result -> result
  | Error message -> parse_failure ("invalid Gemini interaction JSON: " ^ message)
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
  | Ok protocol ->
    let body =
      match protocol with
      | Openai_speech -> Ok (request_body ~config ~text ~voice ~format)
      | Gemini_interaction -> gemini_request_body ~config ~text ~voice ~format
    in
    (match body with
     | Error _ as error -> error
     | Ok body ->
       let url = config.base_url ^ config.request_path in
       let headers = config.headers @ Provider_config.auth_headers_for_config config in
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
          (match protocol with
           | Openai_speech ->
             if response_body = ""
             then parse_failure "OpenAI Speech API returned an empty body"
             else
               Ok
                 { provider_response_id = None
                 ; created_at_rfc3339 = None
                 ; audios =
                     [ { format
                       ; source = Raw_bytes response_body
                       ; sample_rate = None
                       ; channels = None
                       }
                     ]
                 ; usage = None
                 }
           | Gemini_interaction -> parse_gemini_response format response_body)
        | Ok (code, body) -> Error (Http_client.HttpError { code; body })))
;;

let test_caps task = { Capabilities.default_capabilities with task }

let test_config ?(kind = Provider_config.OpenAI_compat) task =
  Provider_config.make
    ~kind
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

let%test "Gemini request is stateless and keeps exact audio format" =
  let config = test_config ~kind:Provider_config.Gemini (Some Capabilities.Speech) in
  match gemini_request_body ~config ~text:"hello" ~voice:(Named "Kore") ~format:Wav with
  | Error _ -> false
  | Ok body ->
    let json = Yojson.Safe.from_string body in
    let open Yojson.Safe.Util in
    member "store" json = `Bool false
    && json |> member "response_format" |> member "mime_type" = `String "audio/wav"
;;

let%test "Gemini response preserves exact audio and absent usage" =
  match
    parse_gemini_response
      Wav
      {|{"id":"ix-tts","status":"completed","created":"2026-07-16T00:00:00Z","steps":[{"type":"model_output","content":[{"type":"audio","data":"eA==","mime_type":"audio/wav","sample_rate":24000,"channels":1}]}],"usage":{}}|}
  with
  | Ok
      { provider_response_id = Some "ix-tts"
      ; audios =
          [ { format = Wav
            ; source = Inline_base64 { media_type = "audio/wav"; data = "eA==" }
            ; sample_rate = Some 24000
            ; channels = Some 1
            }
          ]
      ; usage = Some { input_tokens = None; output_tokens = None; _ }
      ; _
      } -> true
  | Ok _ | Error _ -> false
;;
