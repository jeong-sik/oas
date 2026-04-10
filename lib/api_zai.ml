(** Z.AI general API helpers for non-chat media endpoints. *)

open Llm_provider

type async_task_status =
  | Processing
  | Success
  | Fail
  | Unknown of string

type image_result = {
  url : string;
}

type image_generation_response = {
  created : int;
  data : image_result list;
}

type async_submit_response = {
  model : string;
  id : string;
  request_id : string option;
  task_status : async_task_status;
}

type media_result = {
  url : string;
  cover_image_url : string option;
}

type media_async_result = {
  model : string;
  task_status : async_task_status;
  results : media_result list;
  request_id : string option;
}

type transcription_result = {
  id : string;
  created : int;
  request_id : string option;
  model : string;
  text : string;
}

type audio_source =
  | File_path of string
  | File_base64 of string

let max_multipart_file_size = 25 * 1024 * 1024

let multipart_random = Random.State.make_self_init ()

let auth_headers ?api_key content_type =
  let api_key =
    match api_key with
    | Some key when String.trim key <> "" -> key
    | _ -> Sys.getenv_opt "ZAI_API_KEY" |> Option.value ~default:""
  in
  let base = [("Content-Type", content_type)] in
  if api_key = "" then base
  else ("Authorization", "Bearer " ^ api_key) :: base

let status_of_string = function
  | "PROCESSING" -> Processing
  | "SUCCESS" -> Success
  | "FAIL" -> Fail
  | other -> Unknown other

let read_json body =
  try Ok (Yojson.Safe.from_string body)
  with Yojson.Json_error msg ->
    Error (Http_client.HttpError { code = 400; body = "JSON parse error: " ^ msg })

let parse_guard f =
  try f ()
  with
  | Yojson.Safe.Util.Type_error (msg, _)
  | Yojson.Safe.Util.Undefined (msg, _) ->
      Error (Http_client.HttpError { code = 400; body = "JSON schema error: " ^ msg })

let parse_image_generation body =
  parse_guard (fun () ->
    let open Yojson.Safe.Util in
    match read_json body with
    | Error _ as err -> err
    | Ok json ->
        let created = json |> member "created" |> to_int_option |> Option.value ~default:0 in
        let data =
          json |> member "data" |> to_list
          |> List.filter_map (fun item ->
            item |> member "url" |> to_string_option |> Option.map (fun url -> { url }))
        in
        Ok { created; data })

let parse_async_submit body =
  parse_guard (fun () ->
    let open Yojson.Safe.Util in
    match read_json body with
    | Error _ as err -> err
    | Ok json ->
        Ok {
          model = json |> member "model" |> to_string;
          id = json |> member "id" |> to_string;
          request_id = json |> member "request_id" |> to_string_option;
          task_status =
            json |> member "task_status" |> to_string_option
            |> Option.map status_of_string
            |> Option.value ~default:(Unknown "");
        })

let parse_media_async_result body =
  parse_guard (fun () ->
    let open Yojson.Safe.Util in
    match read_json body with
    | Error _ as err -> err
    | Ok json ->
        let results =
          json |> member "video_result" |> to_list
          |> List.filter_map (fun item ->
            item |> member "url" |> to_string_option
            |> Option.map (fun url ->
              { url; cover_image_url = item |> member "cover_image_url" |> to_string_option }))
        in
        Ok {
          model = json |> member "model" |> to_string;
          task_status =
            json |> member "task_status" |> to_string_option
            |> Option.map status_of_string
            |> Option.value ~default:(Unknown "");
          results;
          request_id = json |> member "request_id" |> to_string_option;
        })

let parse_transcription body =
  parse_guard (fun () ->
    let open Yojson.Safe.Util in
    match read_json body with
    | Error _ as err -> err
    | Ok json ->
        Ok {
          id = json |> member "id" |> to_string;
          created = json |> member "created" |> to_int_option |> Option.value ~default:0;
          request_id = json |> member "request_id" |> to_string_option;
          model = json |> member "model" |> to_string;
          text = json |> member "text" |> to_string;
        })

let post_json ~sw ~net ~url ~headers body parse =
  match Http_client.post_sync ~sw ~net ~url ~headers ~body with
  | Error _ as err -> err
  | Ok (code, body) ->
      if code >= 200 && code < 300 then parse body
      else Error (Http_client.HttpError { code; body })

let encode_path_segment value =
  let buf = Buffer.create (String.length value * 3) in
  String.iter (fun ch ->
    match ch with
    | 'A' .. 'Z'
    | 'a' .. 'z'
    | '0' .. '9'
    | '-'
    | '_'
    | '.'
    | '~' -> Buffer.add_char buf ch
    | _ -> Buffer.add_string buf (Printf.sprintf "%%%02X" (Char.code ch))
  ) value;
  Buffer.contents buf

let media_async_result_url ~base_url ~id =
  let base_uri = Uri.of_string base_url in
  let encoded_id = encode_path_segment id in
  let path = Uri.path base_uri ^ "/async-result/" ^ encoded_id in
  Uri.to_string (Uri.with_path base_uri path)

let generate_image ~sw ~net
    ?(base_url = Zai_catalog.general_base_url) ?api_key ?size
    ~model ~prompt () =
  let body =
    let fields = [("model", `String model); ("prompt", `String prompt)] in
    let fields = match size with
      | Some value -> ("size", `String value) :: fields
      | None -> fields
    in
    Yojson.Safe.to_string (`Assoc fields)
  in
  post_json ~sw ~net
    ~url:(base_url ^ "/images/generations")
    ~headers:(auth_headers ?api_key "application/json")
    body parse_image_generation

let generate_image_async ~sw ~net
    ?(base_url = Zai_catalog.general_base_url) ?api_key ?size
    ~model ~prompt () =
  let body =
    let fields = [("model", `String model); ("prompt", `String prompt)] in
    let fields = match size with
      | Some value -> ("size", `String value) :: fields
      | None -> fields
    in
    Yojson.Safe.to_string (`Assoc fields)
  in
  post_json ~sw ~net
    ~url:(base_url ^ "/async/images/generations")
    ~headers:(auth_headers ?api_key "application/json")
    body parse_async_submit

let generate_video_async ~sw ~net
    ?(base_url = Zai_catalog.general_base_url) ?api_key ?image_url
    ~model ~prompt () =
  let fields = [("model", `String model); ("prompt", `String prompt)] in
  let fields = match image_url with
    | Some value -> ("image_url", `String value) :: fields
    | None -> fields
  in
  post_json ~sw ~net
    ~url:(base_url ^ "/videos/generations")
    ~headers:(auth_headers ?api_key "application/json")
    (Yojson.Safe.to_string (`Assoc fields))
    parse_async_submit

let get_media_async_result ~sw ~net
    ?(base_url = Zai_catalog.general_base_url) ?api_key ~id () =
  match Http_client.get_sync ~sw ~net
          ~url:(media_async_result_url ~base_url ~id)
          ~headers:(auth_headers ?api_key "application/json") with
  | Error _ as err -> err
  | Ok (code, body) ->
      if code >= 200 && code < 300 then parse_media_async_result body
      else Error (Http_client.HttpError { code; body })

let read_file path =
  try
    let ic = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr ic)
      (fun () ->
        let len = in_channel_length ic in
        if len > max_multipart_file_size then
          Error (Http_client.HttpError {
            code = 400;
            body =
              Printf.sprintf
                "File %S is too large for in-memory multipart upload (%d bytes > %d bytes)"
                path len max_multipart_file_size;
          })
        else
          Ok (really_input_string ic len))
  with Sys_error msg ->
    Error (Http_client.HttpError { code = 400; body = "File error: " ^ msg })

let generate_multipart_boundary () =
  Printf.sprintf "----oas-zai-boundary-%08x%08x%08x"
    (Random.State.bits multipart_random)
    (Random.State.bits multipart_random)
    (Random.State.bits multipart_random)

let multipart_body ?prompt ?language ~model ~source () =
  let boundary = generate_multipart_boundary () in
  let buf = Buffer.create 4096 in
  let add_line line =
    Buffer.add_string buf line;
    Buffer.add_string buf "\r\n"
  in
  let add_field name value =
    add_line ("--" ^ boundary);
    add_line (Printf.sprintf "Content-Disposition: form-data; name=%S" name);
    add_line "";
    add_line value
  in
  add_field "model" model;
  (match prompt with Some value -> add_field "prompt" value | None -> ());
  (match language with Some value -> add_field "language" value | None -> ());
  let source_result =
    match source with
    | File_base64 data ->
        add_field "file_base64" data;
        Ok ()
    | File_path path ->
        let filename = Filename.basename path in
        (match read_file path with
         | Error _ as err -> err
         | Ok content ->
             add_line ("--" ^ boundary);
             add_line
               (Printf.sprintf
                  "Content-Disposition: form-data; name=\"file\"; filename=%S"
                  filename);
             add_line "Content-Type: application/octet-stream";
             add_line "";
             Buffer.add_string buf content;
             Buffer.add_string buf "\r\n";
             Ok ())
  in
  match source_result with
  | Error _ as err -> err
  | Ok () ->
      add_line ("--" ^ boundary ^ "--");
      Ok (boundary, Buffer.contents buf)

let transcribe_audio ~sw ~net
    ?(base_url = Zai_catalog.general_base_url) ?api_key
    ?prompt ?language ~model ~source () =
  match multipart_body ?prompt ?language ~model ~source () with
  | Error _ as err -> err
  | Ok (boundary, body) ->
      let headers =
        auth_headers ?api_key ("multipart/form-data; boundary=" ^ boundary)
      in
      post_json ~sw ~net
        ~url:(base_url ^ "/audio/transcriptions")
        ~headers body parse_transcription

[@@@coverage off]

let%test "status_of_string parses known values" =
  status_of_string "SUCCESS" = Success

let%test "auth_headers uses explicit api key when provided" =
  let headers = auth_headers ~api_key:"secret" "application/json" in
  List.mem ("Authorization", "Bearer secret") headers
  && List.mem ("Content-Type", "application/json") headers

let%test "parse_image_generation filters entries without urls" =
  match parse_image_generation
          {|{"created":42,"data":[{"url":"https://example.com/a.png"},{"x":1}]}|} with
  | Ok result -> result.created = 42 && List.length result.data = 1
  | Error _ -> false

let%test "parse_async_submit handles basic body" =
  match parse_async_submit {|{"model":"glm-image","id":"job-1","task_status":"PROCESSING"}|} with
  | Ok result -> result.id = "job-1" && result.task_status = Processing
  | Error _ -> false

let%test "parse_async_submit keeps unknown statuses" =
  match parse_async_submit
          {|{"model":"glm-image","id":"job-1","task_status":"QUEUED","request_id":"req-1"}|} with
  | Ok result ->
      result.request_id = Some "req-1"
      && result.task_status = Unknown "QUEUED"
  | Error _ -> false

let%test "parse_media_async_result handles published schema" =
  match parse_media_async_result
          {|{"model":"glm-image","task_status":"SUCCESS","video_result":[{"url":"https://example.com/a.png","cover_image_url":"https://example.com/c.png"}]}|} with
  | Ok result ->
      result.task_status = Success
      && List.length result.results = 1
  | Error _ -> false

let%test "parse_media_async_result defaults unknown status and filters bad items" =
  match parse_media_async_result
          {|{"model":"glm-video","task_status":"QUEUED","video_result":[{"cover_image_url":"https://example.com/c.png"},{"url":"https://example.com/a.mp4"}]}|} with
  | Ok result ->
      result.task_status = Unknown "QUEUED"
      && List.length result.results = 1
      && List.hd result.results = { url = "https://example.com/a.mp4"; cover_image_url = None }
  | Error _ -> false

let%test "media_async_result_url encodes task id path segment" =
  let url =
    media_async_result_url
      ~base_url:"https://api.z.ai/api/paas/v4"
      ~id:"job/1 ?x#y"
  in
  String.contains url '%'

let%test "parse_transcription handles sync response" =
  match parse_transcription
          {|{"id":"tr-1","created":1,"model":"glm-asr-2512","text":"hello"}|} with
  | Ok result -> result.text = "hello"
  | Error _ -> false

let%test "multipart_body reports missing file path" =
  match multipart_body ~model:"glm-asr-2512" ~source:(File_path "/nonexistent/file.wav") () with
  | Error _ -> true
  | Ok _ -> false

let%test "multipart_body supports base64 source with optional fields" =
  match
    multipart_body ~model:"glm-asr-2512" ~source:(File_base64 "Zm9v")
      ~prompt:"hello" ~language:"ko" ()
  with
  | Error _ -> false
  | Ok (_boundary, body) ->
      String.contains body 'h'
      && String.contains body 'k'
      && String.contains body 'Z'
