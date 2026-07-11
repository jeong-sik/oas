(** Tests for llm_provider sub-library modules:
    complete, api_common, backend_gemini,
    capability_filter, capabilities.

    Focuses on pure functions and data construction only. *)

open Llm_provider

(* ── Helpers ──────────────────────────────────────── *)

let make_config
      ?(kind = Provider_config.OpenAI_compat)
      ?(model_id = "test-model")
      ?(base_url = "http://127.0.0.1:8085")
      ?(api_key = "")
      ?(request_path = "/v1/chat/completions")
      ?(max_tokens = 1024)
      ?temperature
      ?system_prompt
      ?enable_thinking
      ?thinking_budget
      ?tool_choice
      ?(response_format_json = false)
      ()
  : Provider_config.t
  =
  Provider_config.make
    ~kind
    ~model_id
    ~base_url
    ~api_key
    ~request_path
    ~max_tokens
    ?temperature
    ?system_prompt
    ?enable_thinking
    ?thinking_budget
    ?tool_choice
    ~response_format_json
    ()
;;

let user_msg s : Types.message =
  { role = User; content = [ Text s ]; name = None; tool_call_id = None; metadata = [] }
;;

let assistant_msg s : Types.message =
  { role = Assistant
  ; content = [ Text s ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let system_msg s : Types.message =
  { role = System; content = [ Text s ]; name = None; tool_call_id = None; metadata = [] }
;;

let gemini25_flash_model = "gemini-2.5-flash"

let gemini25_url ?api_key:_ ~stream () =
  let action = if stream then "streamGenerateContent" else "generateContent" in
  let base =
    Printf.sprintf
      "https://gen.googleapis.com/v1beta/models/%s:%s"
      gemini25_flash_model
      action
  in
  if stream then base ^ "?alt=sse" else base
;;

let mk_response
      ?(id = "r1")
      ?(model = "m1")
      ?(stop_reason = Types.EndTurn)
      ?(content = [ Types.Text "ok" ])
      ?usage
      ()
  : Types.api_response
  =
  { id; model; stop_reason; content; usage; telemetry = None }
;;

(* ═══════════════════════════════════════════════════
   1. Complete — gemini_url, is_retryable, default_retry_config
   ═══════════════════════════════════════════════════ *)

let test_gemini_url_sync_no_key () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~base_url:"https://gen.googleapis.com/v1beta"
      ~api_key:""
      ()
  in
  let url = Complete.gemini_url ~config ~stream:false in
  Alcotest.(check string) "sync no key" (gemini25_url ~stream:false ()) url
;;

let test_gemini_url_sync_with_key () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~base_url:"https://gen.googleapis.com/v1beta"
      ~api_key:"mykey"
      ()
  in
  let url = Complete.gemini_url ~config ~stream:false in
  Alcotest.(check string)
    "sync with key"
    (gemini25_url ~api_key:"mykey" ~stream:false ())
    url
;;

let test_gemini_url_stream_with_key () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~base_url:"https://gen.googleapis.com/v1beta"
      ~api_key:"mykey"
      ()
  in
  let url = Complete.gemini_url ~config ~stream:true in
  Alcotest.(check string)
    "stream with key"
    (gemini25_url ~api_key:"mykey" ~stream:true ())
    url
;;

let test_gemini_url_stream_no_key () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~base_url:"https://gen.googleapis.com/v1beta"
      ~api_key:""
      ()
  in
  let url = Complete.gemini_url ~config ~stream:true in
  Alcotest.(check string) "stream no key" (gemini25_url ~stream:true ()) url
;;

let test_is_retryable_429 () =
  Alcotest.(check bool)
    "429 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 429; body = "" }))
;;

let test_is_retryable_500 () =
  Alcotest.(check bool)
    "500 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 500; body = "" }))
;;

let test_is_retryable_502 () =
  Alcotest.(check bool)
    "502 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 502; body = "" }))
;;

let test_is_retryable_503 () =
  Alcotest.(check bool)
    "503 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 503; body = "" }))
;;

let test_is_retryable_529 () =
  Alcotest.(check bool)
    "529 retryable"
    true
    (Complete.is_retryable (Http_client.HttpError { code = 529; body = "" }))
;;

let test_is_retryable_400 () =
  Alcotest.(check bool)
    "400 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 400; body = "" }))
;;

let test_is_retryable_401 () =
  Alcotest.(check bool)
    "401 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 401; body = "" }))
;;

let test_is_retryable_403 () =
  Alcotest.(check bool)
    "403 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 403; body = "" }))
;;

let test_is_retryable_404 () =
  Alcotest.(check bool)
    "404 not retryable"
    false
    (Complete.is_retryable (Http_client.HttpError { code = 404; body = "" }))
;;

let test_is_retryable_network () =
  Alcotest.(check bool)
    "network always retryable"
    true
    (Complete.is_retryable
       (Http_client.NetworkError { message = "refused"; kind = Unknown }))
;;

let test_default_retry_config () =
  let c = Complete.default_retry_config in
  Alcotest.(check int) "max_retries" 3 c.max_retries;
  Alcotest.(check (float 0.01)) "initial_delay" 1.0 c.initial_delay_sec;
  Alcotest.(check (float 0.01)) "max_delay" 30.0 c.max_delay_sec;
  Alcotest.(check (float 0.01)) "backoff" 2.0 c.backoff_multiplier
;;

(* ═══════════════════════════════════════════════════
   2. Api_common — constants, helpers, content block JSON
   ═══════════════════════════════════════════════════ *)

let test_default_base_url () =
  Alcotest.(check string)
    "base url"
    "https://api.anthropic.com"
    Api_common.default_base_url
;;

let test_api_version () =
  Alcotest.(check string) "api version" "2023-06-01" Api_common.api_version
;;

let test_max_response_body () =
  Alcotest.(check int) "10 MB" (10 * 1024 * 1024) Api_common.max_response_body
;;

let test_max_stdio_buffer () =
  Alcotest.(check int) "16 MB" (16 * 1024 * 1024) Api_common.max_stdio_buffer
;;

let test_fresh_tool_use_id () =
  let id1 = Api_common.fresh_tool_use_id () in
  let id2 = Api_common.fresh_tool_use_id () in
  Alcotest.(check bool) "distinct allocations" true (id1 <> id2);
  Alcotest.(check bool)
    "starts with OAS namespace"
    true
    (String.starts_with ~prefix:"call_oas_" id1)
;;

let test_fresh_tool_use_id_domain_safe () =
  let domains =
    Array.init 4 (fun _ ->
      Domain.spawn (fun () -> List.init 64 (fun _ -> Api_common.fresh_tool_use_id ())))
  in
  let ids = domains |> Array.to_list |> List.concat_map Domain.join in
  let unique = Hashtbl.create (List.length ids) in
  List.iter (fun id -> Hashtbl.replace unique id ()) ids;
  Alcotest.(check int)
    "all domain allocations unique"
    (List.length ids)
    (Hashtbl.length unique)
;;

let test_string_is_blank () =
  Alcotest.(check bool) "empty" true (Api_common.string_is_blank "");
  Alcotest.(check bool) "spaces" true (Api_common.string_is_blank "   ");
  Alcotest.(check bool) "tab" true (Api_common.string_is_blank "\t");
  Alcotest.(check bool) "not blank" false (Api_common.string_is_blank "a");
  Alcotest.(check bool) "with spaces" false (Api_common.string_is_blank " a ")
;;

let test_text_blocks_to_string () =
  let blocks : Types.content_block list =
    [ Text "hello"
    ; Thinking { signature = Some "t"; content = "reason" }
    ; ToolUse { id = "t1"; name = "tool"; input = `Null }
    ; Text "world"
    ]
  in
  let result = Api_common.text_blocks_to_string blocks in
  Alcotest.(check string) "text+thinking" "hello\nreason\nworld" result
;;

let test_text_blocks_to_string_empty () =
  Alcotest.(check string) "empty" "" (Api_common.text_blocks_to_string [])
;;

let test_text_blocks_to_string_redacted () =
  let blocks : Types.content_block list = [ Text "visible"; RedactedThinking "secret" ] in
  let result = Api_common.text_blocks_to_string blocks in
  Alcotest.(check string) "redacted excluded" "visible" result
;;

let test_json_of_string_or_raw_valid () =
  let result = Api_common.json_of_string_or_raw {|{"key":"val"}|} in
  match result with
  | `Assoc [ ("key", `String "val") ] -> ()
  | _ -> Alcotest.fail "expected valid JSON parse"
;;

let test_json_of_string_or_raw_invalid () =
  let result = Api_common.json_of_string_or_raw "not json" in
  match result with
  | `Assoc [ ("raw", `String "not json") ] -> ()
  | _ -> Alcotest.fail "expected raw fallback"
;;

let test_base64_media_data_url () =
  let result =
    Api_common.base64_media_data_url
      ~backend:"test_backend"
      ~block:"image"
      ~media_type:"image/png"
      ~data:"abc"
      Types.Base64
  in
  Alcotest.(check string) "data URL" "data:image/png;base64,abc" result
;;

let test_base64_media_payload () =
  let result =
    Api_common.base64_media_payload
      ~backend:"test_backend"
      ~block:"audio"
      ~data:"abc"
      Types.Base64
  in
  Alcotest.(check string) "payload" "abc" result
;;

let test_base64_media_source_rejects_url () =
  try
    ignore
      (Api_common.base64_media_data_url
         ~backend:"test_backend"
         ~block:"image"
         ~media_type:"image/png"
         ~data:"https://example.invalid/image.png"
         Types.Url);
    Alcotest.fail "expected Url media source to fail closed"
  with
  | Invalid_argument message ->
    Alcotest.(check string)
      "message"
      "test_backend does not support image media source kind url"
      message
;;

(* content_block_to_json *)
let test_content_block_to_json_text () =
  let json = Api_common.content_block_to_json (Text "hello") in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "text" (json |> member "type" |> to_string);
  Alcotest.(check string) "text" "hello" (json |> member "text" |> to_string)
;;

let test_content_block_to_json_thinking () =
  let json =
    Api_common.content_block_to_json
      (Thinking { signature = Some "sig123"; content = "reason" })
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "thinking" (json |> member "type" |> to_string);
  Alcotest.(check string) "signature" "sig123" (json |> member "signature" |> to_string);
  Alcotest.(check string) "thinking" "reason" (json |> member "thinking" |> to_string)
;;

let test_content_block_to_json_redacted () =
  let json = Api_common.content_block_to_json (RedactedThinking "secret") in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "redacted_thinking" (json |> member "type" |> to_string);
  Alcotest.(check string) "data" "secret" (json |> member "data" |> to_string)
;;

let test_content_block_to_json_tool_use () =
  let json =
    Api_common.content_block_to_json
      (ToolUse { id = "tu1"; name = "fn1"; input = `Assoc [ "x", `Int 1 ] })
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "tool_use" (json |> member "type" |> to_string);
  Alcotest.(check string) "id" "tu1" (json |> member "id" |> to_string);
  Alcotest.(check string) "name" "fn1" (json |> member "name" |> to_string)
;;

let test_content_block_to_json_tool_result () =
  let json =
    Api_common.content_block_to_json
      (ToolResult
         { tool_use_id = "tu1"
         ; content = "done"
         ; is_error = true
         ; failure_kind = Some Validation_error
         ; error_class = Some Deterministic
         ; json = None
         ; content_blocks = None
         })
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "tool_result" (json |> member "type" |> to_string);
  Alcotest.(check bool) "is_error" true (json |> member "is_error" |> to_bool);
  Alcotest.(check bool)
    "failure_kind is SDK-only"
    true
    (json |> member "failure_kind" = `Null);
  Alcotest.(check bool)
    "error_class is SDK-only"
    true
    (json |> member "error_class" = `Null)
;;

let test_content_block_to_json_image () =
  let json =
    Api_common.content_block_to_json
      (Image { media_type = "image/png"; data = "abc"; source_type = Types.Base64 })
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "image" (json |> member "type" |> to_string);
  let source = json |> member "source" in
  Alcotest.(check string)
    "media_type"
    "image/png"
    (source |> member "media_type" |> to_string)
;;

let test_content_block_to_json_document () =
  let json =
    Api_common.content_block_to_json
      (Document
         { media_type = "application/pdf"; data = "pdf"; source_type = Types.Base64 })
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "document" (json |> member "type" |> to_string)
;;

let test_content_block_to_json_audio () =
  let json =
    Api_common.content_block_to_json
      (Audio { media_type = "audio/wav"; data = "wav"; source_type = Types.Base64 })
  in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "type" "audio" (json |> member "type" |> to_string)
;;

(* content_block_of_json roundtrip *)
let test_content_block_of_json_text () =
  let json = `Assoc [ "type", `String "text"; "text", `String "hello" ] in
  match Api_common.content_block_of_json json with
  | Some (Text "hello") -> ()
  | _ -> Alcotest.fail "text roundtrip"
;;

let test_content_block_of_json_thinking () =
  let json =
    `Assoc
      [ "type", `String "thinking"
      ; "signature", `String "sig"
      ; "thinking", `String "reason"
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (Thinking { signature = Some "sig"; content = "reason" }) -> ()
  | _ -> Alcotest.fail "thinking roundtrip"
;;

let test_content_block_of_json_redacted_thinking () =
  let json = `Assoc [ "type", `String "redacted_thinking"; "data", `String "hidden" ] in
  match Api_common.content_block_of_json json with
  | Some (RedactedThinking "hidden") -> ()
  | _ -> Alcotest.fail "redacted_thinking roundtrip"
;;

let test_content_block_of_json_tool_use () =
  let json =
    `Assoc
      [ "type", `String "tool_use"
      ; "id", `String "tu1"
      ; "name", `String "fn"
      ; "input", `Assoc [ "k", `Int 1 ]
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (ToolUse { id = "tu1"; name = "fn"; _ }) -> ()
  | _ -> Alcotest.fail "tool_use roundtrip"
;;

let test_content_block_of_json_tool_result () =
  let json =
    `Assoc
      [ "type", `String "tool_result"
      ; "tool_use_id", `String "tu1"
      ; "content", `String "done"
      ; "is_error", `Bool false
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (ToolResult { tool_use_id = "tu1"; content = "done"; is_error = false; _ }) -> ()
  | _ -> Alcotest.fail "tool_result roundtrip"
;;

let test_content_block_of_json_tool_result_no_is_error () =
  let json =
    `Assoc
      [ "type", `String "tool_result"
      ; "tool_use_id", `String "tu1"
      ; "content", `String "done"
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (ToolResult { is_error = false; _ }) -> ()
  | _ -> Alcotest.fail "tool_result default is_error"
;;

let test_content_block_of_json_image () =
  let json =
    `Assoc
      [ "type", `String "image"
      ; ( "source"
        , `Assoc
            [ "type", `String "base64"
            ; "media_type", `String "image/png"
            ; "data", `String "abc"
            ] )
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (Image { media_type = "image/png"; data = "abc"; source_type = Types.Base64 }) ->
    ()
  | _ -> Alcotest.fail "image roundtrip"
;;

let test_content_block_of_json_document () =
  let json =
    `Assoc
      [ "type", `String "document"
      ; ( "source"
        , `Assoc
            [ "type", `String "base64"
            ; "media_type", `String "application/pdf"
            ; "data", `String "pdf"
            ] )
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (Document { media_type = "application/pdf"; _ }) -> ()
  | _ -> Alcotest.fail "document roundtrip"
;;

let test_content_block_of_json_audio () =
  let json =
    `Assoc
      [ "type", `String "audio"
      ; ( "source"
        , `Assoc
            [ "type", `String "base64"
            ; "media_type", `String "audio/wav"
            ; "data", `String "wav"
            ] )
      ]
  in
  match Api_common.content_block_of_json json with
  | Some (Audio { media_type = "audio/wav"; _ }) -> ()
  | _ -> Alcotest.fail "audio roundtrip"
;;

let test_content_block_of_json_unknown () =
  let json = `Assoc [ "type", `String "video"; "url", `String "x" ] in
  Alcotest.(check bool)
    "unknown -> None"
    true
    (Api_common.content_block_of_json json = None)
;;

let test_content_block_of_json_no_type () =
  let json = `Assoc [ "text", `String "hello" ] in
  Alcotest.(check bool)
    "no type -> None"
    true
    (Api_common.content_block_of_json json = None)
;;

(* message_to_json *)
let test_message_to_json_user () =
  let msg = user_msg "hi" in
  let json = Api_common.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role" "user" (json |> member "role" |> to_string);
  match json |> member "content" with
  | `List [ _ ] -> ()
  | _ -> Alcotest.fail "expected content list"
;;

let test_message_to_json_assistant () =
  let msg = assistant_msg "hi" in
  let json = Api_common.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "role" "assistant" (json |> member "role" |> to_string)
;;

let test_message_to_json_system () =
  let msg = system_msg "prompt" in
  let json = Api_common.message_to_json msg in
  let open Yojson.Safe.Util in
  (* System maps to "user" in api_common *)
  Alcotest.(check string) "role" "user" (json |> member "role" |> to_string)
;;

let test_message_to_json_tool () =
  let msg : Types.message =
    { role = Tool
    ; content = [ Text "result" ]
    ; name = None
    ; tool_call_id = None
    ; metadata = []
    }
  in
  let json = Api_common.message_to_json msg in
  let open Yojson.Safe.Util in
  Alcotest.(check string) "tool -> user" "user" (json |> member "role" |> to_string)
;;

(* ═══════════════════════════════════════════════════
   4. Backend_gemini — build_request, parse_response,
      contents_of_messages
   ═══════════════════════════════════════════════════ *)

let test_contents_of_messages_user () =
  let msgs = [ user_msg "hello" ] in
  let contents, sys = Backend_gemini.contents_of_messages msgs in
  Alcotest.(check int) "1 content" 1 (List.length contents);
  Alcotest.(check bool) "no system" true (sys = None)
;;

let test_contents_of_messages_system () =
  let msgs = [ system_msg "you are helpful"; user_msg "hi" ] in
  let contents, sys = Backend_gemini.contents_of_messages msgs in
  Alcotest.(check int) "1 user content" 1 (List.length contents);
  Alcotest.(check bool) "has system" true (sys <> None)
;;

let test_contents_of_messages_mixed () =
  let msgs =
    [ system_msg "system prompt"; user_msg "user question"; assistant_msg "model answer" ]
  in
  let contents, sys = Backend_gemini.contents_of_messages msgs in
  Alcotest.(check int) "user + model contents" 2 (List.length contents);
  Alcotest.(check bool) "has system" true (sys <> None)
;;

let test_contents_of_messages_tool_use () =
  let msgs : Types.message list =
    [ user_msg "call this tool"
    ; { role = Assistant
      ; content =
          [ ToolUse { id = "tu1"; name = "my_fn"; input = `Assoc [ "x", `Int 1 ] } ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ; { role = Tool
      ; content =
          [ ToolResult
              { tool_use_id = "tu1"
              ; content = "result42"
              ; is_error = false
              ; failure_kind = None
              ; error_class = None
              ; json = None
              ; content_blocks = None
              }
          ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let contents, _sys = Backend_gemini.contents_of_messages msgs in
  (* Should have 3 content entries: user, assistant with functionCall, tool with functionResponse *)
  Alcotest.(check int) "3 contents" 3 (List.length contents)
;;

let test_contents_of_messages_redacted_filtered () =
  let msgs : Types.message list =
    [ { role = Assistant
      ; content = [ RedactedThinking "secret"; Text "visible" ]
      ; name = None
      ; tool_call_id = None
      ; metadata = []
      }
    ]
  in
  let contents, _sys = Backend_gemini.contents_of_messages msgs in
  Alcotest.(check int) "1 content" 1 (List.length contents);
  (* The redacted thinking should be filtered out, leaving only text *)
  let open Yojson.Safe.Util in
  match List.hd contents |> member "parts" with
  | `List parts -> Alcotest.(check int) "1 part (redacted filtered)" 1 (List.length parts)
  | _ -> Alcotest.fail "expected parts list"
;;

let test_build_request_basic () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~max_tokens:100
      ~temperature:0.5
      ()
  in
  let body_str = Backend_gemini.build_request ~config ~messages:[ user_msg "hello" ] () in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  (* Check contents exist *)
  (match json |> member "contents" with
   | `List (_ :: _) -> ()
   | _ -> Alcotest.fail "expected non-empty contents");
  (* Check generationConfig *)
  let gc = json |> member "generationConfig" in
  Alcotest.(check int) "maxOutputTokens" 100 (gc |> member "maxOutputTokens" |> to_int);
  Alcotest.(check (float 0.01)) "temperature" 0.5 (gc |> member "temperature" |> to_number)
;;

let test_build_request_with_system_prompt () =
  let config =
    make_config ~kind:Gemini ~model_id:gemini25_flash_model ~system_prompt:"be helpful" ()
  in
  let body_str = Backend_gemini.build_request ~config ~messages:[ user_msg "hello" ] () in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  match json |> member "systemInstruction" with
  | `Null -> Alcotest.fail "expected systemInstruction"
  | si ->
    let parts = si |> member "parts" in
    (match parts with
     | `List (_ :: _) -> ()
     | _ -> Alcotest.fail "expected parts in systemInstruction")
;;

let test_build_request_with_thinking () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~enable_thinking:true
      ~thinking_budget:5000
      ()
  in
  let body_str =
    Backend_gemini.build_request ~config ~messages:[ user_msg "reason about this" ] ()
  in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let gc = json |> member "generationConfig" in
  let tc = gc |> member "thinkingConfig" in
  Alcotest.(check int) "thinkingBudget" 5000 (tc |> member "thinkingBudget" |> to_int);
  Alcotest.(check bool) "includeThoughts" true (tc |> member "includeThoughts" |> to_bool)
;;

let test_build_request_with_thinking_default_budget () =
  let config =
    make_config ~kind:Gemini ~model_id:gemini25_flash_model ~enable_thinking:true ()
  in
  let body_str =
    Backend_gemini.build_request ~config ~messages:[ user_msg "reason" ] ()
  in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let gc = json |> member "generationConfig" in
  let tc = gc |> member "thinkingConfig" in
  Alcotest.(check int)
    "default thinkingBudget"
    (Constants.Thinking.gemini_budget ())
    (tc |> member "thinkingBudget" |> to_int)
;;

let with_env name value f =
  let saved = Sys.getenv_opt name in
  (match value with
   | Some v -> Unix.putenv name v
   | None -> Unix.putenv name "");
  Fun.protect
    ~finally:(fun () ->
      match saved with
      | Some v -> Unix.putenv name v
      | None -> Unix.putenv name "")
    f
;;

let getenv_from pairs name = List.assoc_opt name pairs

let test_constants_http_code_sets () =
  Alcotest.(check (list int))
    "retryable"
    [ 429; 500; 502; 503; 529 ]
    Constants.Http.retryable_codes;
  Alcotest.(check (list int))
    "cascadable"
    [ 401; 403; 429; 498; 500; 502; 503; 529 ]
    Constants.Http.cascadable_codes
;;

let test_constants_inference_profiles () =
  let check_profile label expected_temp expected_max profile =
    Alcotest.(check (float 0.001))
      (label ^ " temp")
      expected_temp
      profile.Constants.Inference_profile.temperature;
    Alcotest.(check int) (label ^ " max") expected_max profile.max_tokens;
    Alcotest.(check (option (float 0.001))) (label ^ " top_p") None profile.top_p;
    Alcotest.(check (option int)) (label ^ " top_k") None profile.top_k;
    Alcotest.(check (option (float 0.001))) (label ^ " min_p") None profile.min_p
  in
  check_profile "coordinator" 0.3 500 Constants.Inference_profile.coordinator_default;
  check_profile "cascade alias" 0.3 500 Constants.Inference_profile.cascade_default;
  Alcotest.(check bool)
    "cascade_default aliases coordinator_default"
    true
    (Constants.Inference_profile.cascade_default
     = Constants.Inference_profile.coordinator_default);
  check_profile "agent" 0.7 16_384 Constants.Inference_profile.agent_default;
  check_profile "low_variance" 0.1 2048 Constants.Inference_profile.low_variance;
  check_profile "worker" 0.2 16_384 Constants.Inference_profile.worker_default;
  check_profile "deterministic" 0.0 4096 Constants.Inference_profile.deterministic
;;

let test_constants_retry_cache_sampling_and_endpoints () =
  Alcotest.(check int) "cache ttl" 300 Constants.Cache.default_ttl_sec;
  Alcotest.(check int) "retry max" 3 Constants.Retry.max_retries;
  Alcotest.(check (float 0.001)) "retry initial" 1.0 Constants.Retry.initial_delay_sec;
  Alcotest.(check (float 0.001)) "retry max delay" 30.0 Constants.Retry.max_delay_sec;
  Alcotest.(check int) "structured max" 3 Constants.Structured_retry.max_retries;
  Alcotest.(check (float 0.001))
    "structured max delay"
    60.0
    Constants.Structured_retry.max_delay;
  Alcotest.(check (float 0.001)) "min_p" 0.05 Constants.Sampling.openai_compat_min_p;
  Alcotest.(check int) "truncate" 200 Constants.Truncation.max_error_body_length;
  Alcotest.(check int) "llama port" 8085 Constants.Endpoints.default_llama_port;
  Alcotest.(check string)
    "default url"
    "http://127.0.0.1:8085"
    Constants.Endpoints.default_url;
  Alcotest.(check string)
    "localhost url"
    "http://localhost:8085"
    Constants.Endpoints.default_url_localhost;
  Alcotest.(check int) "chars per token" 4 Constants.Token_estimation.chars_per_token
;;

let test_constants_env_helpers () =
  let getenv =
    getenv_from
      [ Constants.Env.thinking_budget_default, "4096"
      ; Constants.Env.anthropic_thinking_budget, "2048"
      ; Constants.Env.gemini_thinking_budget, "3072"
      ; Constants.Env.default_seed, "123"
      ]
  in
  Alcotest.(check int)
    "default thinking env override"
    4096
    (Constants.Thinking.default_budget_for_env ~getenv ());
  Alcotest.(check int)
    "anthropic provider override"
    2048
    (Constants.Thinking.anthropic_budget ~getenv ());
  Alcotest.(check int)
    "gemini provider override"
    3072
    (Constants.Thinking.gemini_budget ~getenv ());
  Alcotest.(check (option int))
    "seed env override"
    (Some 123)
    (Constants.Deterministic.seed_of_env ~getenv ());
  let invalid_getenv = getenv_from [ Constants.Env.default_seed, "not-an-int" ] in
  Alcotest.(check (option int))
    "seed invalid"
    None
    (Constants.Deterministic.seed_of_env ~getenv:invalid_getenv ());
  with_env "OAS_THINKING_BUDGET_DEFAULT" (Some "4096") (fun () ->
    Alcotest.(check (option int))
      "env budget valid"
      (Some 4096)
      (Constants.Thinking.env_budget Constants.Env.thinking_budget_default));
  with_env "OAS_THINKING_BUDGET_DEFAULT" (Some "0") (fun () ->
    Alcotest.(check (option int))
      "env budget zero invalid"
      None
      (Constants.Thinking.env_budget Constants.Env.thinking_budget_default));
  with_env "OAS_ANTHROPIC_THINKING_BUDGET" (Some "2048") (fun () ->
    Alcotest.(check int)
      "anthropic override"
      2048
      (Constants.Thinking.anthropic_budget ()));
  with_env "OAS_GEMINI_THINKING_BUDGET" (Some "3072") (fun () ->
    Alcotest.(check int) "gemini override" 3072 (Constants.Thinking.gemini_budget ()))
;;

let test_slot_cache_helpers () =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-slot-cache-%d" (Unix.getpid ()))
  in
  if Sys.file_exists dir then Unix.rmdir dir;
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> if Sys.file_exists dir then Unix.rmdir dir)
    (fun () ->
       let filename = Slot_cache.cache_filename ~session_id:"session-1" ~slot_id:7 in
       Alcotest.(check string) "filename" "slot-session-1-7.bin" filename;
       let path = Filename.concat dir filename in
       let oc = open_out path in
       output_string oc "cache";
       close_out oc;
       Alcotest.(check bool) "file exists" true (Sys.file_exists path);
       Slot_cache.cleanup_file ~filename ~save_dir:dir;
       Alcotest.(check bool) "file removed" false (Sys.file_exists path);
       Slot_cache.cleanup_file ~filename:"missing.bin" ~save_dir:dir)
;;

let test_llm_transport_runtime_mcp_policy_json () =
  let stdio =
    Llm_transport.Stdio_server
      { name = "local"
      ; command = "python"
      ; args = [ "-m"; "server" ]
      ; env = [ "TOKEN", "redacted" ]
      }
  in
  let http =
    Llm_transport.Http_server
      { name = "remote"
      ; url = "http://127.0.0.1:8931/mcp"
      ; headers = [ "Authorization", "Bearer token" ]
      }
  in
  Alcotest.(check string)
    "stdio name"
    "local"
    (Llm_transport.runtime_mcp_server_name stdio);
  Alcotest.(check string)
    "http name"
    "remote"
    (Llm_transport.runtime_mcp_server_name http);
  let policy =
    { Llm_transport.servers = [ stdio; http ]
    ; allowed_server_names = [ "local"; "remote" ]
    ; allowed_tool_names = [ "read"; "write" ]
    ; permission_mode = Some "ask"
    ; approval_mode = Some "manual"
    ; strict = false
    ; disable_builtin_tools = true
    }
  in
  let json = Llm_transport.runtime_mcp_policy_to_yojson policy in
  let open Yojson.Safe.Util in
  Alcotest.(check int)
    "server count"
    2
    (json |> member "servers" |> to_list |> List.length);
  Alcotest.(check string)
    "permission"
    "ask"
    (json |> member "permission_mode" |> to_string);
  Alcotest.(check string) "approval" "manual" (json |> member "approval_mode" |> to_string);
  Alcotest.(check bool) "strict" false (json |> member "strict" |> to_bool);
  Alcotest.(check bool)
    "disable builtins"
    true
    (json |> member "disable_builtin_tools" |> to_bool)
;;

let test_build_request_json_mode () =
  let config =
    make_config ~kind:Gemini ~model_id:gemini25_flash_model ~response_format_json:true ()
  in
  let body_str =
    Backend_gemini.build_request ~config ~messages:[ user_msg "json pls" ] ()
  in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let gc = json |> member "generationConfig" in
  Alcotest.(check string)
    "responseMimeType"
    "application/json"
    (gc |> member "responseMimeType" |> to_string)
;;

let test_build_request_with_tools () =
  let config = make_config ~kind:Gemini ~model_id:gemini25_flash_model () in
  let tool_schema =
    `Assoc
      [ "name", `String "get_weather"
      ; "description", `String "Get weather info"
      ; ( "input_schema"
        , `Assoc
            [ "type", `String "object"
            ; "properties", `Assoc [ "city", `Assoc [ "type", `String "string" ] ]
            ] )
      ]
  in
  let body_str =
    Backend_gemini.build_request
      ~config
      ~messages:[ user_msg "weather" ]
      ~tools:[ tool_schema ]
      ()
  in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  match json |> member "tools" with
  | `List [ t ] ->
    (match t |> member "functionDeclarations" with
     | `List [ fd ] ->
       Alcotest.(check string) "name" "get_weather" (fd |> member "name" |> to_string)
     | _ -> Alcotest.fail "expected functionDeclarations")
  | _ -> Alcotest.fail "expected tools list"
;;

let test_build_request_tool_choice_auto () =
  let config =
    make_config ~kind:Gemini ~model_id:gemini25_flash_model ~tool_choice:Auto ()
  in
  let body_str =
    Backend_gemini.build_request
      ~config
      ~messages:[ user_msg "hi" ]
      ~tools:[ `Assoc [ "name", `String "t" ] ]
      ()
  in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let tc = json |> member "toolConfig" |> member "functionCallingConfig" in
  Alcotest.(check string) "mode" "AUTO" (tc |> member "mode" |> to_string)
;;

let test_build_request_tool_choice_any () =
  let config =
    make_config ~kind:Gemini ~model_id:gemini25_flash_model ~tool_choice:Any ()
  in
  let body_str =
    Backend_gemini.build_request
      ~config
      ~messages:[ user_msg "hi" ]
      ~tools:[ `Assoc [ "name", `String "t" ] ]
      ()
  in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let tc = json |> member "toolConfig" |> member "functionCallingConfig" in
  Alcotest.(check string) "mode" "ANY" (tc |> member "mode" |> to_string)
;;

let test_build_request_tool_choice_none () =
  let config =
    make_config ~kind:Gemini ~model_id:gemini25_flash_model ~tool_choice:None_ ()
  in
  let body_str = Backend_gemini.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let tc = json |> member "toolConfig" |> member "functionCallingConfig" in
  Alcotest.(check string) "mode" "NONE" (tc |> member "mode" |> to_string)
;;

let test_build_request_tool_choice_specific () =
  let config =
    make_config
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~tool_choice:(Tool "get_weather")
      ()
  in
  let body_str = Backend_gemini.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let tc = json |> member "toolConfig" |> member "functionCallingConfig" in
  Alcotest.(check string) "mode" "ANY" (tc |> member "mode" |> to_string);
  match tc |> member "allowedFunctionNames" with
  | `List [ `String "get_weather" ] -> ()
  | _ -> Alcotest.fail "expected allowedFunctionNames"
;;

let test_build_request_top_p_top_k () =
  let config : Provider_config.t =
    Provider_config.make
      ~kind:Gemini
      ~model_id:gemini25_flash_model
      ~base_url:""
      ~top_p:0.9
      ~top_k:40
      ()
  in
  let body_str = Backend_gemini.build_request ~config ~messages:[ user_msg "hi" ] () in
  let json = Yojson.Safe.from_string body_str in
  let open Yojson.Safe.Util in
  let gc = json |> member "generationConfig" in
  Alcotest.(check (float 0.01)) "topP" 0.9 (gc |> member "topP" |> to_number);
  Alcotest.(check int) "topK" 40 (gc |> member "topK" |> to_int)
;;

(* parse_response *)
let test_parse_response_basic () =
  let json =
    Yojson.Safe.from_string
      (Printf.sprintf
         {|{
    "candidates": [{
      "content": {
        "parts": [{"text": "Hello world"}],
        "role": "model"
      },
      "finishReason": "STOP"
    }],
    "usageMetadata": {
      "promptTokenCount": 10,
      "candidatesTokenCount": 20
    },
    "modelVersion": "%s"
  }|}
         gemini25_flash_model)
  in
  let resp = Backend_gemini.parse_response json in
  Alcotest.(check string) "model" gemini25_flash_model resp.model;
  (match resp.content with
   | [ Text "Hello world" ] -> ()
   | _ -> Alcotest.fail "expected text block");
  match resp.stop_reason with
  | Types.EndTurn -> ()
  | _ -> Alcotest.fail "expected EndTurn"
;;

let test_parse_response_with_thinking () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {
        "parts": [
          {"thought": true, "text": "let me think"},
          {"text": "answer"}
        ]
      },
      "finishReason": "STOP"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  Alcotest.(check int) "2 blocks" 2 (List.length resp.content);
  match List.hd resp.content with
  | Types.Thinking { content = "let me think"; _ } -> ()
  | _ -> Alcotest.fail "expected thinking block"
;;

let test_parse_response_function_call () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {
        "parts": [{
          "functionCall": {
            "name": "get_weather",
            "args": {"city": "Seoul"}
          }
        }]
      },
      "finishReason": "STOP"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  (match resp.content with
   | [ Types.ToolUse { name = "get_weather"; _ } ] -> ()
   | _ -> Alcotest.fail "expected ToolUse");
  match resp.stop_reason with
  | Types.StopToolUse -> ()
  | _ -> Alcotest.fail "expected StopToolUse"
;;

let test_parse_response_max_tokens () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": [{"text": "partial"}]},
      "finishReason": "MAX_TOKENS"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.stop_reason with
  | Types.MaxTokens -> ()
  | _ -> Alcotest.fail "expected MaxTokens"
;;

let test_parse_response_safety () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": []},
      "finishReason": "SAFETY"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.stop_reason with
  | Types.Refusal -> ()
  | _ -> Alcotest.fail "expected Refusal"
;;

let test_parse_response_recitation () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": []},
      "finishReason": "RECITATION"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.stop_reason with
  | Types.Refusal -> ()
  | _ -> Alcotest.fail "expected Refusal"
;;

let test_parse_response_unknown_reason () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": [{"text": "x"}]},
      "finishReason": "BLOCKLIST"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.stop_reason with
  | Types.Unknown "BLOCKLIST" -> ()
  | _ -> Alcotest.fail "expected Unknown BLOCKLIST"
;;

let test_parse_response_no_finish_reason () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": [{"text": "x"}]}
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.stop_reason with
  | Types.EndTurn -> () (* default is STOP -> EndTurn *)
  | _ -> Alcotest.fail "expected EndTurn default"
;;

let test_parse_response_no_usage () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": [{"text": "x"}]},
      "finishReason": "STOP"
    }]
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  Alcotest.(check bool) "no usage" true (resp.usage = None)
;;

let test_parse_response_error () =
  let json =
    Yojson.Safe.from_string
      {|{
    "error": {"message": "model not found", "code": 404}
  }|}
  in
  try
    let _ = Backend_gemini.parse_response json in
    Alcotest.fail "expected exception"
  with
  | Backend_gemini.Gemini_api_error msg ->
    Alcotest.(check string) "error msg" "model not found" msg
;;

let test_parse_response_error_no_message () =
  let json =
    Yojson.Safe.from_string
      {|{
    "error": {"code": 500}
  }|}
  in
  try
    let _ = Backend_gemini.parse_response json in
    Alcotest.fail "expected exception"
  with
  | Backend_gemini.Gemini_api_error msg ->
    Alcotest.(check string) "default msg" "Unknown Gemini API error" msg
;;

let test_parse_response_no_candidates () =
  (* Should fallback to json itself as candidate *)
  let json =
    Yojson.Safe.from_string
      {|{
    "content": {"parts": [{"text": "fallback"}]},
    "finishReason": "STOP"
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.content with
  | [ Text "fallback" ] -> ()
  | _ -> Alcotest.fail "expected fallback parse"
;;

let test_parse_response_usage_with_cache () =
  let json =
    Yojson.Safe.from_string
      {|{
    "candidates": [{
      "content": {"parts": [{"text": "x"}]},
      "finishReason": "STOP"
    }],
    "usageMetadata": {
      "promptTokenCount": 100,
      "candidatesTokenCount": 50,
      "cachedContentTokenCount": 30
    }
  }|}
  in
  let resp = Backend_gemini.parse_response json in
  match resp.usage with
  | Some u ->
    Alcotest.(check int) "input" 100 u.input_tokens;
    Alcotest.(check int) "output" 50 u.output_tokens;
    Alcotest.(check int) "cache_read" 30 u.cache_read_input_tokens;
    Alcotest.(check int) "cache_creation" 0 u.cache_creation_input_tokens
  | None -> Alcotest.fail "expected usage"
;;

(* ═══════════════════════════════════════════════════
   5. Capability_filter — predicates, limits, combinators
   ═══════════════════════════════════════════════════ *)

let test_requires_tools () =
  let caps = Capabilities.anthropic_capabilities in
  Alcotest.(check bool) "anthropic has tools" true (Capability_filter.requires_tools caps);
  Alcotest.(check bool)
    "default no tools"
    false
    (Capability_filter.requires_tools Capabilities.default_capabilities)
;;

let test_requires_streaming () =
  Alcotest.(check bool)
    "anthropic streaming"
    true
    (Capability_filter.requires_streaming Capabilities.anthropic_capabilities);
  Alcotest.(check bool)
    "default no streaming"
    false
    (Capability_filter.requires_streaming Capabilities.default_capabilities)
;;

let test_requires_reasoning () =
  Alcotest.(check bool)
    "anthropic reasoning"
    true
    (Capability_filter.requires_reasoning Capabilities.anthropic_capabilities);
  Alcotest.(check bool)
    "default no reasoning"
    false
    (Capability_filter.requires_reasoning Capabilities.default_capabilities)
;;

let test_requires_multimodal () =
  Alcotest.(check bool)
    "anthropic multimodal"
    true
    (Capability_filter.requires_multimodal Capabilities.anthropic_capabilities)
;;

let test_requires_json_format () =
  Alcotest.(check bool)
    "openai json"
    true
    (Capability_filter.requires_json_format Capabilities.openai_compat_chat_capabilities);
  Alcotest.(check bool)
    "default no json"
    false
    (Capability_filter.requires_json_format Capabilities.default_capabilities)
;;

let test_requires_parallel_tools () =
  Alcotest.(check bool)
    "anthropic parallel tools"
    true
    (Capability_filter.requires_parallel_tools Capabilities.anthropic_capabilities)
;;

let test_requires_thinking () =
  Alcotest.(check bool)
    "anthropic thinking"
    true
    (Capability_filter.requires_thinking Capabilities.anthropic_capabilities);
  Alcotest.(check bool)
    "default no thinking"
    false
    (Capability_filter.requires_thinking Capabilities.default_capabilities)
;;

let test_requires_structured_output () =
  Alcotest.(check bool)
    "openai structured"
    true
    (Capability_filter.requires_structured_output
       Capabilities.openai_compat_chat_capabilities);
  Alcotest.(check bool)
    "default no structured"
    false
    (Capability_filter.requires_structured_output Capabilities.default_capabilities)
;;

let test_requires_caching () =
  Alcotest.(check bool)
    "anthropic caching"
    true
    (Capability_filter.requires_caching Capabilities.anthropic_capabilities);
  Alcotest.(check bool)
    "default no caching"
    false
    (Capability_filter.requires_caching Capabilities.default_capabilities)
;;

let test_requires_vision () =
  Alcotest.(check bool)
    "anthropic vision"
    true
    (Capability_filter.requires_vision Capabilities.anthropic_capabilities);
  Alcotest.(check bool)
    "default no vision"
    false
    (Capability_filter.requires_vision Capabilities.default_capabilities)
;;

let test_requires_computer_use () =
  Alcotest.(check bool)
    "anthropic computer_use"
    true
    (Capability_filter.requires_computer_use Capabilities.anthropic_capabilities);
  Alcotest.(check bool)
    "default no computer_use"
    false
    (Capability_filter.requires_computer_use Capabilities.default_capabilities)
;;

let test_requires_system_prompt () =
  Alcotest.(check bool)
    "default has system_prompt"
    true
    (Capability_filter.requires_system_prompt Capabilities.default_capabilities)
;;

let test_fits_context_none () =
  Alcotest.(check bool)
    "None -> fail closed"
    false
    (Capability_filter.fits_context ~tokens:999999 Capabilities.default_capabilities)
;;

let test_fits_context_within () =
  Alcotest.(check bool)
    "within limit"
    true
    (Capability_filter.fits_context ~tokens:100000 Capabilities.anthropic_capabilities)
;;

let test_fits_context_over () =
  Alcotest.(check bool)
    "over limit"
    false
    (Capability_filter.fits_context ~tokens:999999 Capabilities.anthropic_capabilities)
;;

let test_fits_output_none () =
  Alcotest.(check bool)
    "None -> fail closed"
    false
    (Capability_filter.fits_output ~tokens:999999 Capabilities.default_capabilities)
;;

let test_fits_output_within () =
  Alcotest.(check bool)
    "within limit"
    true
    (Capability_filter.fits_output ~tokens:1000 Capabilities.anthropic_capabilities)
;;

let test_fits_output_over () =
  Alcotest.(check bool)
    "over limit"
    false
    (Capability_filter.fits_output ~tokens:999999 Capabilities.anthropic_capabilities)
;;

let fit_result_testable =
  let pp fmt = function
    | Capability_filter.Fits -> Format.pp_print_string fmt "Fits"
    | Capability_filter.Does_not_fit -> Format.pp_print_string fmt "Does_not_fit"
    | Capability_filter.Unknown_limit -> Format.pp_print_string fmt "Unknown_limit"
  in
  let eq a b = a = b in
  Alcotest.testable pp eq
;;

let test_check_context_unknown () =
  Alcotest.(check fit_result_testable)
    "None -> Unknown_limit"
    Capability_filter.Unknown_limit
    (Capability_filter.check_context ~tokens:999999 Capabilities.default_capabilities)
;;

let test_check_context_fits () =
  Alcotest.(check fit_result_testable)
    "within -> Fits"
    Capability_filter.Fits
    (Capability_filter.check_context ~tokens:100000 Capabilities.anthropic_capabilities)
;;

let test_check_context_over () =
  Alcotest.(check fit_result_testable)
    "over -> Does_not_fit"
    Capability_filter.Does_not_fit
    (Capability_filter.check_context ~tokens:999999 Capabilities.anthropic_capabilities)
;;

let test_check_output_unknown () =
  Alcotest.(check fit_result_testable)
    "None -> Unknown_limit"
    Capability_filter.Unknown_limit
    (Capability_filter.check_output ~tokens:999999 Capabilities.default_capabilities)
;;

(* requires_code_execution helper -- not exported from Capability_filter *)
let requires_code_execution (c : Capabilities.capabilities) = c.supports_code_execution

let test_requires_all () =
  let caps = Capabilities.anthropic_capabilities in
  let pred =
    Capability_filter.requires_all
      [ Capability_filter.requires_tools; Capability_filter.requires_streaming ]
  in
  Alcotest.(check bool) "all satisfied" true (pred caps);
  let pred2 =
    Capability_filter.requires_all
      [ Capability_filter.requires_tools; requires_code_execution ]
  in
  (* anthropic does not have code_execution *)
  Alcotest.(check bool) "not all satisfied" false (pred2 caps)
;;

let test_requires_any () =
  let caps = Capabilities.default_capabilities in
  let pred =
    Capability_filter.requires_any
      [ Capability_filter.requires_tools; Capability_filter.requires_system_prompt ]
  in
  Alcotest.(check bool) "any satisfied (system_prompt)" true (pred caps);
  let pred2 =
    Capability_filter.requires_any
      [ Capability_filter.requires_tools; Capability_filter.requires_streaming ]
  in
  Alcotest.(check bool) "none satisfied" false (pred2 caps)
;;

(* ═══════════════════════════════════════════════════
   6. Capabilities — presets, for_model_id, with_context_size
   ═══════════════════════════════════════════════════ *)

let test_default_capabilities () =
  let c = Capabilities.default_capabilities in
  Alcotest.(check bool) "no tools" false c.supports_tools;
  Alcotest.(check bool) "system_prompt" true c.supports_system_prompt;
  Alcotest.(check bool) "no context limit" true (c.max_context_tokens = None)
;;

let test_anthropic_capabilities () =
  let c = Capabilities.anthropic_capabilities in
  Alcotest.(check bool) "tools" true c.supports_tools;
  Alcotest.(check bool) "tool_choice" true c.supports_tool_choice;
  Alcotest.(check bool) "named tool_choice" true c.supports_named_tool_choice;
  Alcotest.(check bool) "parallel tools" true c.supports_parallel_tool_calls;
  Alcotest.(check bool) "reasoning" true c.supports_reasoning;
  Alcotest.(check bool) "extended_thinking" true c.supports_extended_thinking;
  Alcotest.(check bool) "structured output" true c.supports_structured_output;
  Alcotest.(check bool) "multimodal" true c.supports_multimodal_inputs;
  Alcotest.(check bool) "streaming" true c.supports_native_streaming;
  Alcotest.(check bool) "caching" true c.supports_caching;
  Alcotest.(check bool) "computer_use" true c.supports_computer_use;
  Alcotest.(check (option int)) "max_context" (Some 200_000) c.max_context_tokens
;;

let test_openai_compat_chat_capabilities () =
  let c = Capabilities.openai_compat_chat_capabilities in
  Alcotest.(check bool) "tools" true c.supports_tools;
  Alcotest.(check bool) "tool_choice" true c.supports_tool_choice;
  Alcotest.(check bool) "named tool_choice" true c.supports_named_tool_choice;
  Alcotest.(check bool) "json format" true c.supports_response_format_json;
  Alcotest.(check bool) "structured" true c.supports_structured_output;
  Alcotest.(check (option int)) "max_context" (Some 128_000) c.max_context_tokens
;;

let test_openai_compat_chat_extended_capabilities () =
  let c = Capabilities.openai_compat_chat_extended_capabilities in
  Alcotest.(check bool) "reasoning" true c.supports_reasoning;
  Alcotest.(check bool) "extended_thinking" true c.supports_extended_thinking;
  Alcotest.(check bool) "top_k" true c.supports_top_k;
  Alcotest.(check bool) "min_p" true c.supports_min_p
;;

let test_gemini_capabilities () =
  let c = Capabilities.gemini_capabilities in
  Alcotest.(check bool) "audio" true c.supports_audio_input;
  Alcotest.(check bool) "video" true c.supports_video_input;
  Alcotest.(check bool) "code_execution" true c.supports_code_execution;
  (* Gemini generationConfig accepts topK — pin so capability-gated
     consumers do not silently drop it for Gemini configs. *)
  Alcotest.(check bool) "top_k" true c.supports_top_k;
  Alcotest.(check bool) "no min_p" false c.supports_min_p;
  Alcotest.(check (option int)) "max_context" (Some 1_000_000) c.max_context_tokens;
  Alcotest.(check (option int)) "max_output" (Some 65_000) c.max_output_tokens
;;

let test_glm_capabilities () =
  let c = Capabilities.glm_capabilities in
  (* Tool descriptions are sent and the model can still emit tool_use blocks. *)
  Alcotest.(check bool) "supports_tools" true c.supports_tools;
  (* GLM accepts provider-level tool_choice modes such as auto/any, but does
     not support named forced tool_choice. Keep these axes separate so callers
     reject unsupported named forcing without dropping valid auto requests. *)
  Alcotest.(check bool) "supports tool_choice modes" true c.supports_tool_choice;
  Alcotest.(check bool)
    "rejects named forced tool_choice"
    false
    c.supports_named_tool_choice;
  Alcotest.(check bool) "structured output disabled" false c.supports_structured_output;
  Alcotest.(check (option int)) "200K context" (Some 200_000) c.max_context_tokens;
  Alcotest.(check (option int)) "40960 output cap" (Some 40_960) c.max_output_tokens
;;

let test_for_model_id_claude_opus_4 () =
  match Capabilities.for_model_id "claude-opus-4-20260101" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens;
    Alcotest.(check (option int)) "128K output" (Some 128_000) c.max_output_tokens
  | None -> Alcotest.fail "expected Some for claude-opus-4"
;;

let test_for_model_id_claude_sonnet_4 () =
  match Capabilities.for_model_id "claude-sonnet-4-latest" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens;
    Alcotest.(check (option int)) "64K output" (Some 64_000) c.max_output_tokens
  | None -> Alcotest.fail "expected Some for claude-sonnet-4"
;;

let test_for_model_id_claude_haiku_4 () =
  match Capabilities.for_model_id "claude-haiku-4-2026" with
  | Some c ->
    Alcotest.(check (option int)) "200K context" (Some 200_000) c.max_context_tokens;
    Alcotest.(check (option int)) "8K output" (Some 8_192) c.max_output_tokens
  | None -> Alcotest.fail "expected Some for claude-haiku-4"
;;

let test_for_model_id_gpt5 () =
  match Capabilities.for_model_id "gpt-5-latest" with
  | Some c ->
    Alcotest.(check bool) "computer_use" true c.supports_computer_use;
    Alcotest.(check (option int)) "1050K context" (Some 1_050_000) c.max_context_tokens
  | None -> Alcotest.fail "expected Some for gpt-5"
;;

let test_for_model_id_gpt41 () =
  match Capabilities.for_model_id "gpt-4.1-mini" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens
  | None -> Alcotest.fail "expected Some for gpt-4.1"
;;

let test_for_model_id_gpt4o () =
  match Capabilities.for_model_id "gpt-latest" with
  | Some c ->
    Alcotest.(check (option int)) "128K context" (Some 128_000) c.max_context_tokens
  | None -> Alcotest.fail "expected Some for gpt"
;;

let test_for_model_id_gemini25 () =
  match Capabilities.for_model_id gemini25_flash_model with
  | Some c -> Alcotest.(check bool) "code_execution" true c.supports_code_execution
  | None -> Alcotest.fail "expected Some for legacy gemini"
;;

let test_for_model_id_gemini3 () =
  match Capabilities.for_model_id "gemini-3-pro" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected Some for gemini-3"
;;

let test_for_model_id_qwen3 () =
  match Capabilities.for_model_id "dashscope-3.5-35b" with
  | Some c ->
    Alcotest.(check bool) "tools" true c.supports_tools;
    Alcotest.(check bool) "reasoning" true c.supports_reasoning;
    Alcotest.(check bool) "top_k" true c.supports_top_k;
    Alcotest.(check bool) "min_p" true c.supports_min_p
  | None -> Alcotest.fail "expected Some for qwen3"
;;

let test_for_model_id_llama4 () =
  match Capabilities.for_model_id "llama-4-scout" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens;
    Alcotest.(check bool) "multimodal" true c.supports_multimodal_inputs
  | None -> Alcotest.fail "expected Some for llama-4"
;;

let test_for_model_id_llama4_alt () =
  match Capabilities.for_model_id "llama4-maverick" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected Some for llama4"
;;

let test_for_model_id_deepseek_v4_flash () =
  match Capabilities.for_model_id "deepseek-v4-flash" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens;
    Alcotest.(check (option int)) "384K output" (Some 384_000) c.max_output_tokens;
    Alcotest.(check bool) "tools" true c.supports_tools;
    Alcotest.(check bool) "reasoning" true c.supports_reasoning;
    Alcotest.(check bool)
      "thinking object"
      true
      (c.thinking_control_format = Capabilities.Thinking_object);
    Alcotest.(check bool) "caching" true c.supports_caching
  | None -> Alcotest.fail "expected Some for deepseek-v4-flash"
;;

let test_for_model_id_deepseek_v4_pro () =
  match Capabilities.for_model_id "deepseek-v4-pro" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens;
    Alcotest.(check (option int)) "384K output" (Some 384_000) c.max_output_tokens;
    Alcotest.(check bool) "tools" true c.supports_tools;
    Alcotest.(check bool) "reasoning" true c.supports_reasoning;
    Alcotest.(check bool)
      "thinking object"
      true
      (c.thinking_control_format = Capabilities.Thinking_object);
    Alcotest.(check bool) "caching" true c.supports_caching
  | None -> Alcotest.fail "expected Some for deepseek-v4-pro"
;;

let test_for_model_id_mistral_large () =
  match Capabilities.for_model_id "mistral-large-2025" with
  | Some c ->
    Alcotest.(check bool) "structured" true c.supports_structured_output;
    Alcotest.(check (option int)) "260K context" (Some 260_000) c.max_context_tokens
  | None -> Alcotest.fail "expected Some for mistral-large"
;;

let test_for_model_id_mistral_small () =
  match Capabilities.for_model_id "mistral-small-latest" with
  | Some c ->
    Alcotest.(check bool) "reasoning" true c.supports_reasoning;
    Alcotest.(check (option int)) "256K context" (Some 256_000) c.max_context_tokens
  | None -> Alcotest.fail "expected Some for mistral-small"
;;

let test_for_model_id_command () =
  match Capabilities.for_model_id "command-r-plus" with
  | Some c ->
    Alcotest.(check (option int)) "256K context" (Some 256_000) c.max_context_tokens;
    Alcotest.(check (option int)) "32K output" (Some 32_000) c.max_output_tokens
  | None -> Alcotest.fail "expected Some for command"
;;

let test_for_model_id_grok () =
  match Capabilities.for_model_id "grok-4.3" with
  | Some c ->
    Alcotest.(check (option int)) "1M context" (Some 1_000_000) c.max_context_tokens;
    Alcotest.(check bool) "reasoning" true c.supports_reasoning
  | None -> Alcotest.fail "expected Some for grok"
;;

let test_for_model_id_glm () =
  (* glm-4.5-flash matches current Z.AI GLM-4.5 thinking limits. *)
  (match Capabilities.for_model_id "glm-4.5-flash" with
   | Some c ->
     Alcotest.(check (option int)) "128K context" (Some 128_000) c.max_context_tokens;
     Alcotest.(check (option int)) "96K output" (Some 96_000) c.max_output_tokens;
     Alcotest.(check bool) "tools" true c.supports_tools;
     Alcotest.(check bool) "reasoning" true c.supports_reasoning;
     Alcotest.(check bool) "thinking" true c.supports_extended_thinking
   | None -> Alcotest.fail "expected Some for glm-4.5-flash");
  (* glm-5.1 should still get full capabilities *)
  match Capabilities.for_model_id "glm-5.1" with
  | Some c ->
    Alcotest.(check (option int)) "200K context" (Some 200_000) c.max_context_tokens;
    Alcotest.(check (option int)) "128K output" (Some 128_000) c.max_output_tokens;
    Alcotest.(check bool) "reasoning" true c.supports_reasoning
  | None -> Alcotest.fail "expected Some for glm-5.1"
;;

let test_for_model_id_unknown () =
  Alcotest.(check bool)
    "unknown -> None"
    true
    (Capabilities.for_model_id "totally-unknown-model" = None)
;;

let test_for_model_id_case_insensitive () =
  match Capabilities.for_model_id "Claude-Opus-4-Latest" with
  | Some _ -> ()
  | None -> Alcotest.fail "expected case-insensitive match"
;;

let test_with_context_size () =
  let c =
    Capabilities.with_context_size Capabilities.default_capabilities ~ctx_size:262144
  in
  Alcotest.(check (option int)) "set ctx" (Some 262144) c.max_context_tokens
;;

let test_with_context_size_overrides () =
  let c =
    Capabilities.with_context_size Capabilities.anthropic_capabilities ~ctx_size:1_000_000
  in
  Alcotest.(check (option int)) "overrides" (Some 1_000_000) c.max_context_tokens
;;

(* ── Complete_common.thinking_control_disable_unsatisfiable ──
   Guards the fail-loud path that rejects an unsatisfiable "disable thinking"
   request instead of dropping it silently (the librarian empty/invalid-JSON class). *)

let reasoning_no_control_caps =
  { Capabilities.openai_compat_chat_capabilities with supports_reasoning = true }
;;

let test_thinking_disable_unsatisfiable_when_reasoning_no_control () =
  let config = make_config ~model_id:"reasoner-no-control" ~enable_thinking:false () in
  Alcotest.(check bool)
    "reasoning model + No_thinking_control + disable -> unsatisfiable"
    true
    (Complete_common.thinking_control_disable_unsatisfiable
       ~caps:reasoning_no_control_caps
       config)
;;

let test_thinking_enable_is_satisfiable () =
  let config = make_config ~model_id:"reasoner-no-control" ~enable_thinking:true () in
  Alcotest.(check bool)
    "enable_thinking=true is satisfiable (model thinks by default)"
    false
    (Complete_common.thinking_control_disable_unsatisfiable
       ~caps:reasoning_no_control_caps
       config)
;;

let test_thinking_unset_is_satisfiable () =
  let config = make_config ~model_id:"reasoner-no-control" () in
  Alcotest.(check bool)
    "enable_thinking unset -> nothing to satisfy"
    false
    (Complete_common.thinking_control_disable_unsatisfiable
       ~caps:reasoning_no_control_caps
       config)
;;

let test_thinking_disable_noop_for_non_reasoning () =
  (* openai_compat_chat_capabilities has supports_reasoning = false *)
  let config = make_config ~model_id:"non-reasoner" ~enable_thinking:false () in
  Alcotest.(check bool)
    "non-reasoning model -> disable is a harmless no-op"
    false
    (Complete_common.thinking_control_disable_unsatisfiable
       ~caps:Capabilities.openai_compat_chat_capabilities
       config)
;;

let test_thinking_disable_satisfiable_with_control_format () =
  let caps =
    { Capabilities.openai_compat_chat_capabilities with
      supports_reasoning = true
    ; thinking_control_format = Capabilities.Thinking_object
    }
  in
  let config = make_config ~model_id:"reasoner-with-control" ~enable_thinking:false () in
  Alcotest.(check bool)
    "reasoning model WITH a thinking_control_format -> satisfiable"
    false
    (Complete_common.thinking_control_disable_unsatisfiable ~caps config)
;;

(* ═══════════════════════════════════════════════════
   Test runner
   ═══════════════════════════════════════════════════ *)

let () =
  Alcotest.run
    "llm_provider_cov"
    [ ( "complete.gemini_url"
      , [ Alcotest.test_case "sync no key" `Quick test_gemini_url_sync_no_key
        ; Alcotest.test_case "sync with key" `Quick test_gemini_url_sync_with_key
        ; Alcotest.test_case "stream with key" `Quick test_gemini_url_stream_with_key
        ; Alcotest.test_case "stream no key" `Quick test_gemini_url_stream_no_key
        ] )
    ; ( "complete.is_retryable"
      , [ Alcotest.test_case "429" `Quick test_is_retryable_429
        ; Alcotest.test_case "500" `Quick test_is_retryable_500
        ; Alcotest.test_case "502" `Quick test_is_retryable_502
        ; Alcotest.test_case "503" `Quick test_is_retryable_503
        ; Alcotest.test_case "529" `Quick test_is_retryable_529
        ; Alcotest.test_case "400" `Quick test_is_retryable_400
        ; Alcotest.test_case "401" `Quick test_is_retryable_401
        ; Alcotest.test_case "403" `Quick test_is_retryable_403
        ; Alcotest.test_case "404" `Quick test_is_retryable_404
        ; Alcotest.test_case "network" `Quick test_is_retryable_network
        ] )
    ; ( "complete.retry_config"
      , [ Alcotest.test_case "defaults" `Quick test_default_retry_config ] )
    ; ( "api_common.constants"
      , [ Alcotest.test_case "default_base_url" `Quick test_default_base_url
        ; Alcotest.test_case "api_version" `Quick test_api_version
        ; Alcotest.test_case "max_response_body" `Quick test_max_response_body
        ; Alcotest.test_case "max_stdio_buffer" `Quick test_max_stdio_buffer
        ] )
    ; ( "api_common.helpers"
      , [ Alcotest.test_case "fresh_tool_use_id" `Quick test_fresh_tool_use_id
        ; Alcotest.test_case
            "fresh_tool_use_id domain-safe"
            `Quick
            test_fresh_tool_use_id_domain_safe
        ; Alcotest.test_case "string_is_blank" `Quick test_string_is_blank
        ; Alcotest.test_case "text_blocks_to_string" `Quick test_text_blocks_to_string
        ; Alcotest.test_case
            "text_blocks_to_string empty"
            `Quick
            test_text_blocks_to_string_empty
        ; Alcotest.test_case
            "text_blocks_to_string redacted"
            `Quick
            test_text_blocks_to_string_redacted
        ; Alcotest.test_case
            "json_of_string_or_raw valid"
            `Quick
            test_json_of_string_or_raw_valid
        ; Alcotest.test_case
            "json_of_string_or_raw invalid"
            `Quick
            test_json_of_string_or_raw_invalid
        ; Alcotest.test_case "base64_media_data_url" `Quick test_base64_media_data_url
        ; Alcotest.test_case "base64_media_payload" `Quick test_base64_media_payload
        ; Alcotest.test_case
            "base64_media_source rejects url"
            `Quick
            test_base64_media_source_rejects_url
        ] )
    ; ( "api_common.content_block_to_json"
      , [ Alcotest.test_case "text" `Quick test_content_block_to_json_text
        ; Alcotest.test_case "thinking" `Quick test_content_block_to_json_thinking
        ; Alcotest.test_case "redacted" `Quick test_content_block_to_json_redacted
        ; Alcotest.test_case "tool_use" `Quick test_content_block_to_json_tool_use
        ; Alcotest.test_case "tool_result" `Quick test_content_block_to_json_tool_result
        ; Alcotest.test_case "image" `Quick test_content_block_to_json_image
        ; Alcotest.test_case "document" `Quick test_content_block_to_json_document
        ; Alcotest.test_case "audio" `Quick test_content_block_to_json_audio
        ] )
    ; ( "api_common.content_block_of_json"
      , [ Alcotest.test_case "text" `Quick test_content_block_of_json_text
        ; Alcotest.test_case "thinking" `Quick test_content_block_of_json_thinking
        ; Alcotest.test_case
            "redacted_thinking"
            `Quick
            test_content_block_of_json_redacted_thinking
        ; Alcotest.test_case "tool_use" `Quick test_content_block_of_json_tool_use
        ; Alcotest.test_case "tool_result" `Quick test_content_block_of_json_tool_result
        ; Alcotest.test_case
            "tool_result no is_error"
            `Quick
            test_content_block_of_json_tool_result_no_is_error
        ; Alcotest.test_case "image" `Quick test_content_block_of_json_image
        ; Alcotest.test_case "document" `Quick test_content_block_of_json_document
        ; Alcotest.test_case "audio" `Quick test_content_block_of_json_audio
        ; Alcotest.test_case "unknown" `Quick test_content_block_of_json_unknown
        ; Alcotest.test_case "no type" `Quick test_content_block_of_json_no_type
        ] )
    ; ( "api_common.message_to_json"
      , [ Alcotest.test_case "user" `Quick test_message_to_json_user
        ; Alcotest.test_case "assistant" `Quick test_message_to_json_assistant
        ; Alcotest.test_case "system" `Quick test_message_to_json_system
        ; Alcotest.test_case "tool" `Quick test_message_to_json_tool
        ] )
    ; ( "backend_gemini.contents_of_messages"
      , [ Alcotest.test_case "user" `Quick test_contents_of_messages_user
        ; Alcotest.test_case "system" `Quick test_contents_of_messages_system
        ; Alcotest.test_case "mixed" `Quick test_contents_of_messages_mixed
        ; Alcotest.test_case "tool_use" `Quick test_contents_of_messages_tool_use
        ; Alcotest.test_case
            "redacted filtered"
            `Quick
            test_contents_of_messages_redacted_filtered
        ] )
    ; ( "backend_gemini.build_request"
      , [ Alcotest.test_case "basic" `Quick test_build_request_basic
        ; Alcotest.test_case
            "with system_prompt"
            `Quick
            test_build_request_with_system_prompt
        ; Alcotest.test_case "with thinking" `Quick test_build_request_with_thinking
        ; Alcotest.test_case
            "thinking default budget"
            `Quick
            test_build_request_with_thinking_default_budget
        ; Alcotest.test_case "json mode" `Quick test_build_request_json_mode
        ; Alcotest.test_case "with tools" `Quick test_build_request_with_tools
        ; Alcotest.test_case "tool_choice auto" `Quick test_build_request_tool_choice_auto
        ; Alcotest.test_case "tool_choice any" `Quick test_build_request_tool_choice_any
        ; Alcotest.test_case "tool_choice none" `Quick test_build_request_tool_choice_none
        ; Alcotest.test_case
            "tool_choice specific"
            `Quick
            test_build_request_tool_choice_specific
        ; Alcotest.test_case "top_p top_k" `Quick test_build_request_top_p_top_k
        ] )
    ; ( "constants"
      , [ Alcotest.test_case "http code sets" `Quick test_constants_http_code_sets
        ; Alcotest.test_case "inference profiles" `Quick test_constants_inference_profiles
        ; Alcotest.test_case
            "retry cache sampling endpoints"
            `Quick
            test_constants_retry_cache_sampling_and_endpoints
        ; Alcotest.test_case "env helpers" `Quick test_constants_env_helpers
        ] )
    ; "slot_cache", [ Alcotest.test_case "file helpers" `Quick test_slot_cache_helpers ]
    ; ( "llm_transport"
      , [ Alcotest.test_case
            "runtime mcp policy json"
            `Quick
            test_llm_transport_runtime_mcp_policy_json
        ] )
    ; ( "backend_gemini.parse_response"
      , [ Alcotest.test_case "basic" `Quick test_parse_response_basic
        ; Alcotest.test_case "with thinking" `Quick test_parse_response_with_thinking
        ; Alcotest.test_case "function call" `Quick test_parse_response_function_call
        ; Alcotest.test_case "max_tokens" `Quick test_parse_response_max_tokens
        ; Alcotest.test_case "safety" `Quick test_parse_response_safety
        ; Alcotest.test_case "recitation" `Quick test_parse_response_recitation
        ; Alcotest.test_case "unknown reason" `Quick test_parse_response_unknown_reason
        ; Alcotest.test_case
            "no finish reason"
            `Quick
            test_parse_response_no_finish_reason
        ; Alcotest.test_case "no usage" `Quick test_parse_response_no_usage
        ; Alcotest.test_case "error" `Quick test_parse_response_error
        ; Alcotest.test_case
            "error no message"
            `Quick
            test_parse_response_error_no_message
        ; Alcotest.test_case "no candidates" `Quick test_parse_response_no_candidates
        ; Alcotest.test_case
            "usage with cache"
            `Quick
            test_parse_response_usage_with_cache
        ] )
    ; ( "capability_filter.predicates"
      , [ Alcotest.test_case "requires_tools" `Quick test_requires_tools
        ; Alcotest.test_case "requires_streaming" `Quick test_requires_streaming
        ; Alcotest.test_case "requires_reasoning" `Quick test_requires_reasoning
        ; Alcotest.test_case "requires_multimodal" `Quick test_requires_multimodal
        ; Alcotest.test_case "requires_json_format" `Quick test_requires_json_format
        ; Alcotest.test_case "requires_parallel_tools" `Quick test_requires_parallel_tools
        ; Alcotest.test_case "requires_thinking" `Quick test_requires_thinking
        ; Alcotest.test_case
            "requires_structured_output"
            `Quick
            test_requires_structured_output
        ; Alcotest.test_case "requires_caching" `Quick test_requires_caching
        ; Alcotest.test_case "requires_vision" `Quick test_requires_vision
        ; Alcotest.test_case "requires_computer_use" `Quick test_requires_computer_use
        ; Alcotest.test_case "requires_system_prompt" `Quick test_requires_system_prompt
        ] )
    ; ( "capability_filter.limits"
      , [ Alcotest.test_case "fits_context none" `Quick test_fits_context_none
        ; Alcotest.test_case "fits_context within" `Quick test_fits_context_within
        ; Alcotest.test_case "fits_context over" `Quick test_fits_context_over
        ; Alcotest.test_case "fits_output none" `Quick test_fits_output_none
        ; Alcotest.test_case "fits_output within" `Quick test_fits_output_within
        ; Alcotest.test_case "fits_output over" `Quick test_fits_output_over
        ; Alcotest.test_case "check_context unknown" `Quick test_check_context_unknown
        ; Alcotest.test_case "check_context fits" `Quick test_check_context_fits
        ; Alcotest.test_case "check_context over" `Quick test_check_context_over
        ; Alcotest.test_case "check_output unknown" `Quick test_check_output_unknown
        ] )
    ; ( "capability_filter.combinators"
      , [ Alcotest.test_case "requires_all" `Quick test_requires_all
        ; Alcotest.test_case "requires_any" `Quick test_requires_any
        ] )
    ; ( "capabilities.presets"
      , [ Alcotest.test_case "default" `Quick test_default_capabilities
        ; Alcotest.test_case "anthropic" `Quick test_anthropic_capabilities
        ; Alcotest.test_case "openai_chat" `Quick test_openai_compat_chat_capabilities
        ; Alcotest.test_case
            "openai_chat_extended"
            `Quick
            test_openai_compat_chat_extended_capabilities
        ; Alcotest.test_case "gemini" `Quick test_gemini_capabilities
        ; Alcotest.test_case "glm" `Quick test_glm_capabilities
        ] )
    ; ( "capabilities.for_model_id"
      , [ Alcotest.test_case "claude-opus-4" `Quick test_for_model_id_claude_opus_4
        ; Alcotest.test_case "claude-sonnet-4" `Quick test_for_model_id_claude_sonnet_4
        ; Alcotest.test_case "claude-haiku-4" `Quick test_for_model_id_claude_haiku_4
        ; Alcotest.test_case "gpt-5" `Quick test_for_model_id_gpt5
        ; Alcotest.test_case "gpt-4.1" `Quick test_for_model_id_gpt41
        ; Alcotest.test_case "gpt" `Quick test_for_model_id_gpt4o
        ; Alcotest.test_case "gemini legacy" `Quick test_for_model_id_gemini25
        ; Alcotest.test_case "gemini-3" `Quick test_for_model_id_gemini3
        ; Alcotest.test_case "dashscope-3" `Quick test_for_model_id_qwen3
        ; Alcotest.test_case "llama-4" `Quick test_for_model_id_llama4
        ; Alcotest.test_case "llama4" `Quick test_for_model_id_llama4_alt
        ; Alcotest.test_case
            "deepseek-v4-flash"
            `Quick
            test_for_model_id_deepseek_v4_flash
        ; Alcotest.test_case "deepseek-v4-pro" `Quick test_for_model_id_deepseek_v4_pro
        ; Alcotest.test_case "mistral-large" `Quick test_for_model_id_mistral_large
        ; Alcotest.test_case "mistral-small" `Quick test_for_model_id_mistral_small
        ; Alcotest.test_case "command" `Quick test_for_model_id_command
        ; Alcotest.test_case "grok" `Quick test_for_model_id_grok
        ; Alcotest.test_case "glm" `Quick test_for_model_id_glm
        ; Alcotest.test_case "unknown" `Quick test_for_model_id_unknown
        ; Alcotest.test_case "case insensitive" `Quick test_for_model_id_case_insensitive
        ] )
    ; ( "capabilities.with_context_size"
      , [ Alcotest.test_case "set" `Quick test_with_context_size
        ; Alcotest.test_case "overrides" `Quick test_with_context_size_overrides
        ] )
    ; ( "complete_common.thinking_control"
      , [ Alcotest.test_case
            "reasoning+no-control disable unsatisfiable"
            `Quick
            test_thinking_disable_unsatisfiable_when_reasoning_no_control
        ; Alcotest.test_case
            "enable is satisfiable"
            `Quick
            test_thinking_enable_is_satisfiable
        ; Alcotest.test_case
            "unset is satisfiable"
            `Quick
            test_thinking_unset_is_satisfiable
        ; Alcotest.test_case
            "non-reasoning no-op"
            `Quick
            test_thinking_disable_noop_for_non_reasoning
        ; Alcotest.test_case
            "with control format satisfiable"
            `Quick
            test_thinking_disable_satisfiable_with_control_format
        ] )
    ]
;;
