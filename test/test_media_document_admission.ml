(** Document admission and per-wire media serialization (oas#2744).

    The OpenAI-compatible Chat Completions serializer used to emit a [Document]
    block as an [image_url] part: the same typed block reached the model as a
    picture, and no layer reported the substitution. These cases pin the three
    properties that replaced it —

    - a document bound for a wire/row that cannot carry one is a typed, visible
      outcome, never an image part;
    - wires that do carry documents still emit their own native form;
    - image and audio payloads are byte-identical to the pre-fix serializer on
      every wire.

    The image/audio cases are the permanent form of the temporary
    baseline-vs-change probe used while developing the fix: the expected JSON
    below was copied from the pre-change binary's output. *)

open Alcotest
open Llm_provider

let json = testable (Fmt.of_to_string Yojson.Safe.to_string) ( = )

let admission_error =
  testable (Fmt.of_to_string Api_common.document_admission_error_to_string) ( = )
;;

(* Substring search kept local so the suite needs no extra dependency. *)
let contains ~needle haystack =
  let nl = String.length needle
  and hl = String.length haystack in
  let rec at i = i + nl <= hl && (String.sub haystack i nl = needle || at (i + 1)) in
  nl = 0 || at 0
;;

let image_block =
  Types.Image { media_type = "image/png"; data = "SU1H"; source_type = Types.Base64 }
;;

let audio_block =
  Types.Audio { media_type = "wav"; data = "QVVE"; source_type = Types.Base64 }
;;

let document_block =
  Types.Document
    { media_type = "application/pdf"; data = "UERG"; source_type = Types.Base64 }
;;

let user_message content =
  { Types.role = Types.User; content; name = None; tool_call_id = None; metadata = [] }
;;

let openai_config model_id =
  Provider_config.make
    ~kind:Provider_config.OpenAI_compat
    ~model_id
    ~base_url:"http://127.0.0.1:0"
    ()
;;

(* ── (a) inadmissible document is a typed outcome, not an image ───────── *)

let test_admission_rejects_undeclared_row () =
  let error =
    match
      Api_common.admit_document_blocks
        ~wire_form:Api_common.Document_chat_file_part
        ~model_id:"undeclared-model"
        ~supports_document_input:false
        [ Types.Text "prefix"; document_block ]
    with
    | Ok () -> failwith "expected an admission error"
    | Error error -> error
  in
  match error with
  | Api_common.Document_input_not_declared { model_id; media_type } ->
    check string "model named" "undeclared-model" model_id;
    check string "media type named" "application/pdf" media_type
  | Api_common.Document_wire_has_no_representation _ ->
    failwith "expected Document_input_not_declared"
;;

let test_admission_rejects_wire_without_document_part () =
  let error =
    match
      Api_common.admit_document_blocks
        ~wire_form:Api_common.Document_unrepresentable
          (* A row that declares document support still cannot use a wire that
             has no document part; the wire form alone decides here. *)
        ~model_id:"declared-model"
        ~supports_document_input:true
        [ document_block ]
    with
    | Ok () -> failwith "expected an admission error"
    | Error error -> error
  in
  match error with
  | Api_common.Document_wire_has_no_representation { wire_form; media_type } ->
    check
      string
      "wire form named"
      "no document part"
      (Api_common.document_wire_form_to_string wire_form);
    check string "media type named" "application/pdf" media_type
  | Api_common.Document_input_not_declared _ ->
    failwith "expected Document_wire_has_no_representation"
;;

let test_admission_ignores_image_and_audio () =
  (* Image/audio admission is deliberately untouched: a row that declares no
     document support must not start rejecting the media it carries today. *)
  check
    (result unit admission_error)
    "image and audio admitted"
    (Ok ())
    (Api_common.admit_document_blocks
       ~wire_form:Api_common.Document_unrepresentable
       ~model_id:"undeclared-model"
       ~supports_document_input:false
       [ Types.Text "prefix"; image_block; audio_block ])
;;

let test_openai_chat_request_rejects_undeclared_document () =
  (* End-to-end through the request builder: the pre-fix binary produced
     {"type":"image_url",...} here for every one of these rows. *)
  List.iter
    (fun model_id ->
       let config = openai_config model_id in
       let messages = [ user_message [ Types.Text "prefix"; document_block ] ] in
       match Backend_openai_request.build_request_assoc ~config ~messages () with
       | exception Invalid_argument message ->
         check
           bool
           (Printf.sprintf "%s: rejection names the media type" model_id)
           true
           (contains ~needle:"application/pdf" message);
         check
           bool
           (Printf.sprintf "%s: rejection names the capability" model_id)
           true
           (contains ~needle:"supports_document_input" message)
       | body ->
         failf
           "%s: expected a typed rejection, serialized %s"
           model_id
           (Yojson.Safe.to_string body))
    [ "gpt-5.2"; "glm-4.6"; "glm-4-flash"; "qwen3:8b" ]
;;

let test_ollama_native_rejects_document () =
  match
    Backend_openai_serialize.ollama_messages_of_history
      ~model_id:"qwen3:8b"
      [ user_message [ Types.Text "prefix"; document_block ] ]
  with
  | Ok wire ->
    failf "expected a rejection, serialized %s" (Yojson.Safe.to_string (`List wire))
  | Error message ->
    check bool "names the media type" true (contains ~needle:"application/pdf" message);
    check
      bool
      "names the missing wire representation"
      true
      (contains ~needle:"no document part" message)
;;

(* ── (b) wires that carry documents emit their own native form ────────── *)

let test_openai_chat_document_is_a_file_part () =
  check
    json
    "document serializes as a file part, not image_url"
    (`List
        [ `Assoc [ "type", `String "text"; "text", `String "prefix" ]
        ; `Assoc
            [ "type", `String "file"
            ; "file", `Assoc [ "file_data", `String "data:application/pdf;base64,UERG" ]
            ]
        ])
    (`List
        (Backend_openai.openai_content_parts_of_blocks
           [ Types.Text "prefix"; document_block ]))
;;

let test_anthropic_document_is_a_source_block () =
  check
    json
    "anthropic document source block"
    (`Assoc
        [ "type", `String "document"
        ; ( "source"
          , `Assoc
              [ "type", `String "base64"
              ; "media_type", `String "application/pdf"
              ; "data", `String "UERG"
              ] )
        ])
    (Api_common.content_block_to_json document_block)
;;

let test_gemini_document_is_inline_data () =
  let contents, _ =
    Backend_gemini.contents_of_messages
      [ user_message [ Types.Text "prefix"; document_block ] ]
  in
  check
    json
    "gemini inlineData keeps the document mime type"
    (`List
        [ `Assoc
            [ "role", `String "user"
            ; ( "parts"
              , `List
                  [ `Assoc [ "text", `String "prefix" ]
                  ; `Assoc
                      [ ( "inlineData"
                        , `Assoc
                            [ "mimeType", `String "application/pdf"
                            ; "data", `String "UERG"
                            ] )
                      ]
                  ] )
            ]
        ])
    (`List contents)
;;

let test_anthropic_and_gemini_declare_document_input () =
  check
    bool
    "anthropic base declares document input"
    true
    Capabilities.anthropic_capabilities.supports_document_input;
  check
    bool
    "gemini base declares document input"
    true
    Capabilities.gemini_capabilities.supports_document_input;
  check
    bool
    "default declares none"
    false
    Capabilities.default_capabilities.supports_document_input;
  check
    bool
    "openai-compat chat base declares none"
    false
    Capabilities.openai_compat_chat_capabilities.supports_document_input;
  (* The declaration must survive the embedded catalog: a row inherits it from
     its [base] only when it actually has one. *)
  List.iter
    (fun (model_id, expected) ->
       check
         (option bool)
         (Printf.sprintf "%s resolves document input" model_id)
         (Some expected)
         (Capabilities.for_model_id model_id
          |> Option.map (fun (c : Capabilities.capabilities) -> c.supports_document_input)
         ))
    [ "claude-opus-4-8", true; "gemini-3.5-flash", true; "gpt-5.2", false ]
;;

let test_catalog_row_can_declare_document_input () =
  (* The capability must be sourceable the same way its image/audio/video
     siblings are: a models.toml row declares it and it survives to
     [Capabilities.for_model_id]. A row with no [base] resolves against
     [default_capabilities], so it declares nothing unless it says so — the
     failure mode the sibling refactor hit. *)
  let toml =
    "[[models]]\n\
     id_prefix = \"doc-declared\"\n\
     max_context_tokens = 8192\n\
     supports_document_input = true\n\n\
     [[models]]\n\
     id_prefix = \"doc-silent\"\n\
     max_context_tokens = 8192\n"
  in
  let catalog =
    match Model_catalog.of_toml_string ~source:"document admission inline" toml with
    | Ok catalog -> catalog
    | Error message -> failf "inline catalog should parse: %s" message
  in
  Model_catalog.clear_global ();
  Fun.protect ~finally:Model_catalog.clear_global (fun () ->
    Model_catalog.set_global catalog;
    let declares model_id =
      Capabilities.for_model_id model_id
      |> Option.map (fun (c : Capabilities.capabilities) -> c.supports_document_input)
    in
    check
      (option bool)
      "declared row carries the fact"
      (Some true)
      (declares "doc-declared");
    check
      (option bool)
      "base-less silent row declares nothing"
      (Some false)
      (declares "doc-silent"))
;;

(* ── (c) image and audio payloads are unchanged ───────────────────────── *)

let test_openai_chat_image_and_audio_unchanged () =
  check
    json
    "image_url part unchanged"
    (`List
        [ `Assoc
            [ "type", `String "image_url"
            ; "image_url", `Assoc [ "url", `String "data:image/png;base64,SU1H" ]
            ]
        ])
    (`List (Backend_openai.openai_content_parts_of_blocks [ image_block ]));
  check
    json
    "input_audio part unchanged"
    (`List
        [ `Assoc
            [ "type", `String "input_audio"
            ; "input_audio", `Assoc [ "data", `String "QVVE"; "format", `String "wav" ]
            ]
        ])
    (`List (Backend_openai.openai_content_parts_of_blocks [ audio_block ]))
;;

let test_openai_chat_request_image_and_audio_unchanged () =
  List.iter
    (fun (block, expected_part) ->
       let config = openai_config "gpt-5.2" in
       let messages = [ user_message [ Types.Text "prefix"; block ] ] in
       check
         json
         "request body unchanged"
         (`Assoc
             [ "model", `String "gpt-5.2"
             ; ( "messages"
               , `List
                   [ `Assoc
                       [ "role", `String "user"
                       ; ( "content"
                         , `List
                             [ `Assoc [ "type", `String "text"; "text", `String "prefix" ]
                             ; expected_part
                             ] )
                       ]
                   ] )
             ])
         (Backend_openai_request.build_request_assoc ~config ~messages ()))
    [ ( image_block
      , `Assoc
          [ "type", `String "image_url"
          ; "image_url", `Assoc [ "url", `String "data:image/png;base64,SU1H" ]
          ] )
    ; ( audio_block
      , `Assoc
          [ "type", `String "input_audio"
          ; "input_audio", `Assoc [ "data", `String "QVVE"; "format", `String "wav" ]
          ] )
    ]
;;

let test_ollama_native_image_unchanged () =
  match
    Backend_openai_serialize.ollama_messages_of_history
      ~model_id:"qwen3:8b"
      [ user_message [ Types.Text "prefix"; image_block ] ]
  with
  | Error message -> failf "expected serialization, got %s" message
  | Ok wire ->
    check
      json
      "ollama native images array unchanged"
      (`List
          [ `Assoc
              [ "role", `String "user"
              ; "content", `String "prefix"
              ; "images", `List [ `String "SU1H" ]
              ]
          ])
      (`List wire)
;;

let test_gemini_image_and_audio_unchanged () =
  List.iter
    (fun (block, mime, data) ->
       let contents, _ = Backend_gemini.contents_of_messages [ user_message [ block ] ] in
       check
         json
         (Printf.sprintf "gemini inlineData unchanged for %s" mime)
         (`List
             [ `Assoc
                 [ "role", `String "user"
                 ; ( "parts"
                   , `List
                       [ `Assoc
                           [ ( "inlineData"
                             , `Assoc [ "mimeType", `String mime; "data", `String data ] )
                           ]
                       ] )
                 ]
             ])
         (`List contents))
    [ image_block, "image/png", "SU1H"; audio_block, "wav", "QVVE" ]
;;

let () =
  run
    "media_document_admission"
    [ ( "inadmissible documents are typed outcomes"
      , [ test_case
            "undeclared row is rejected by name"
            `Quick
            test_admission_rejects_undeclared_row
        ; test_case
            "wire without a document part is rejected by name"
            `Quick
            test_admission_rejects_wire_without_document_part
        ; test_case
            "image and audio are not gated"
            `Quick
            test_admission_ignores_image_and_audio
        ; test_case
            "openai chat request rejects instead of emitting image_url"
            `Quick
            test_openai_chat_request_rejects_undeclared_document
        ; test_case
            "ollama native rejects instead of appending to images"
            `Quick
            test_ollama_native_rejects_document
        ] )
    ; ( "wires that carry documents keep their native form"
      , [ test_case
            "openai chat file part"
            `Quick
            test_openai_chat_document_is_a_file_part
        ; test_case
            "anthropic document source block"
            `Quick
            test_anthropic_document_is_a_source_block
        ; test_case "gemini inlineData" `Quick test_gemini_document_is_inline_data
        ; test_case
            "capability bases declare document input"
            `Quick
            test_anthropic_and_gemini_declare_document_input
        ; test_case
            "catalog row can declare document input"
            `Quick
            test_catalog_row_can_declare_document_input
        ] )
    ; ( "image and audio payloads are unchanged"
      , [ test_case
            "openai chat content parts"
            `Quick
            test_openai_chat_image_and_audio_unchanged
        ; test_case
            "openai chat request body"
            `Quick
            test_openai_chat_request_image_and_audio_unchanged
        ; test_case "ollama native images" `Quick test_ollama_native_image_unchanged
        ; test_case "gemini inlineData" `Quick test_gemini_image_and_audio_unchanged
        ] )
    ]
;;
