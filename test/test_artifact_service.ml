(** Unit tests for Artifact_service — pure helpers exposed for kind/mime
    coercion. The file-IO surface (`save_text_internal`, `list`, `get_text`)
    requires Eio and a Runtime_store; those are exercised by integration
    tests, not here. *)

open Agent_sdk
open Alcotest

(* ── extension_of_kind ────────────────────────────────── *)

let test_ext_markdown () =
  check string "markdown" "md" (Artifact_service.extension_of_kind "markdown")
;;

let test_ext_md_alias () =
  check string "md alias" "md" (Artifact_service.extension_of_kind "md")
;;

let test_ext_json () =
  check string "json" "json" (Artifact_service.extension_of_kind "json")
;;

let test_ext_html () =
  check string "html" "html" (Artifact_service.extension_of_kind "html")
;;

let test_ext_csv () = check string "csv" "csv" (Artifact_service.extension_of_kind "csv")

let test_ext_text_alias () =
  check string "text → txt" "txt" (Artifact_service.extension_of_kind "text")
;;

let test_ext_txt () = check string "txt" "txt" (Artifact_service.extension_of_kind "txt")

let test_ext_empty_defaults_txt () =
  check string "empty → txt" "txt" (Artifact_service.extension_of_kind "")
;;

let test_ext_unknown_passthrough () =
  (* implementation: unknown kinds pass through after lowercasing+trimming *)
  check
    string
    "unknown kind passes through"
    "yaml"
    (Artifact_service.extension_of_kind "yaml")
;;

let test_ext_uppercase () =
  check string "uppercase normalised" "json" (Artifact_service.extension_of_kind "JSON")
;;

let test_ext_whitespace () =
  check string "trimmed" "md" (Artifact_service.extension_of_kind "  md  ")
;;

(* ── mime_type_of_kind ────────────────────────────────── *)

let test_mime_markdown () =
  check string "md" "text/markdown" (Artifact_service.mime_type_of_kind "markdown")
;;

let test_mime_md_alias () =
  check string "md alias" "text/markdown" (Artifact_service.mime_type_of_kind "md")
;;

let test_mime_json () =
  check string "json" "application/json" (Artifact_service.mime_type_of_kind "json")
;;

let test_mime_html () =
  check string "html" "text/html" (Artifact_service.mime_type_of_kind "html")
;;

let test_mime_csv () =
  check string "csv" "text/csv" (Artifact_service.mime_type_of_kind "csv")
;;

let test_mime_text_alias () =
  check string "text" "text/plain" (Artifact_service.mime_type_of_kind "text")
;;

let test_mime_txt () =
  check string "txt" "text/plain" (Artifact_service.mime_type_of_kind "txt")
;;

let test_mime_empty_defaults () =
  check string "empty → text/plain" "text/plain" (Artifact_service.mime_type_of_kind "")
;;

let test_mime_unknown_falls_back () =
  check
    string
    "unknown → octet-stream"
    "application/octet-stream"
    (Artifact_service.mime_type_of_kind "yaml")
;;

let test_mime_uppercase () =
  check
    string
    "JSON normalised"
    "application/json"
    (Artifact_service.mime_type_of_kind "JSON")
;;

let test_mime_whitespace () =
  check
    string
    "trimmed"
    "text/markdown"
    (Artifact_service.mime_type_of_kind "  markdown  ")
;;

(* ── consistency: kind→ext and kind→mime cohere ───────── *)

let test_consistency_table () =
  List.iter
    (fun (kind, ext, mime) ->
       check string ("ext for " ^ kind) ext (Artifact_service.extension_of_kind kind);
       check string ("mime for " ^ kind) mime (Artifact_service.mime_type_of_kind kind))
    [ "markdown", "md", "text/markdown"
    ; "json", "json", "application/json"
    ; "html", "html", "text/html"
    ; "csv", "csv", "text/csv"
    ; "text", "txt", "text/plain"
    ]
;;

(* ── persisted artifact I/O ───────────────────────────── *)

let require_ok label = function
  | Ok value -> value
  | Error err -> failf "%s: %s" label (Error.to_string err)
;;

let temp_root label =
  Filename.concat
    (Filename.get_temp_dir_name ())
    (Printf.sprintf
       "oas-artifact-service-%s-%d-%d"
       label
       (Unix.getpid ())
       (int_of_float (Unix.gettimeofday () *. 1_000_000.0)))
;;

let make_session ?(artifacts = []) session_id : Runtime.session =
  { session_id
  ; goal = "test"
  ; title = Some "Artifact test"
  ; tag = None
  ; permission_mode = None
  ; phase = Runtime.Completed
  ; created_at = 1.0
  ; updated_at = 2.0
  ; provider = None
  ; model = None
  ; system_prompt = None
  ; max_turns = 1
  ; workdir = None
  ; planned_participants = []
  ; participants = []
  ; artifacts
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = Some "ok"
  }
;;

let make_descriptor ?path ?inline_content artifact_id : Runtime.artifact =
  { artifact_id
  ; name = "inline"
  ; kind = "text"
  ; mime_type = "text/plain"
  ; path
  ; inline_content
  ; size_bytes = Option.fold ~none:0 ~some:String.length inline_content
  ; created_at = 1.0
  }
;;

let test_save_list_get_and_overwrite_text () =
  let root = temp_root "persisted" in
  let session_id = "sess-artifact-persisted" in
  let store = require_ok "create store" (Runtime_store.create ~root ()) in
  let artifact =
    require_ok
      "save artifact"
      (Artifact_service.save_text_internal
         store
         ~session_id
         ~name:" Report / One "
         ~kind:" Markdown "
         ~content:"initial")
  in
  check string "mime" "text/markdown" artifact.mime_type;
  check int "size" 7 artifact.size_bytes;
  let path = require_ok "persisted path" (Artifact_service.persisted_path artifact) in
  check string "extension" ".md" (Filename.extension path);
  check
    string
    "file content"
    "initial"
    (require_ok "load text" (Runtime_store.load_text path));
  require_ok
    "save session"
    (Runtime_store.save_session store (make_session ~artifacts:[ artifact ] session_id));
  let artifacts =
    require_ok "list artifacts" (Artifact_service.list ~session_root:root ~session_id ())
  in
  check int "one artifact" 1 (List.length artifacts);
  check
    string
    "get persisted text"
    "initial"
    (require_ok
       "get text"
       (Artifact_service.get_text
          ~session_root:root
          ~session_id
          ~artifact_id:artifact.artifact_id
          ()));
  require_ok
    "overwrite text"
    (Artifact_service.overwrite_text_internal artifact ~content:"updated");
  check
    string
    "get overwritten text"
    "updated"
    (require_ok
       "get overwritten"
       (Artifact_service.get_text
          ~session_root:root
          ~session_id
          ~artifact_id:artifact.artifact_id
          ()))
;;

let test_get_text_prefers_inline_content () =
  let root = temp_root "inline" in
  let session_id = "sess-artifact-inline" in
  let store = require_ok "create store" (Runtime_store.create ~root ()) in
  let artifact =
    make_descriptor
      ~path:"/path/that/should/not/be/read"
      ~inline_content:"inline body"
      "inline-1"
  in
  require_ok
    "save session"
    (Runtime_store.save_session store (make_session ~artifacts:[ artifact ] session_id));
  check
    string
    "inline body"
    "inline body"
    (require_ok
       "get inline"
       (Artifact_service.get_text
          ~session_root:root
          ~session_id
          ~artifact_id:"inline-1"
          ()))
;;

let test_artifact_error_paths () =
  let root = temp_root "errors" in
  let session_id = "sess-artifact-errors" in
  let store = require_ok "create store" (Runtime_store.create ~root ()) in
  let dangling = make_descriptor "dangling" in
  (match Artifact_service.persisted_path dangling with
   | Ok path -> fail ("expected missing path error, got " ^ path)
   | Error err ->
     check
       bool
       "mentions no persisted file path"
       true
       (String.contains (Error.to_string err) 'n'));
  require_ok
    "save session"
    (Runtime_store.save_session store (make_session ~artifacts:[ dangling ] session_id));
  (match
     Artifact_service.get_text ~session_root:root ~session_id ~artifact_id:"missing" ()
   with
   | Ok body -> fail ("expected missing artifact, got " ^ body)
   | Error err ->
     check bool "missing artifact error" true (String.length (Error.to_string err) > 0));
  match
    Artifact_service.get_text ~session_root:root ~session_id ~artifact_id:"dangling" ()
  with
  | Ok body -> fail ("expected dangling artifact error, got " ^ body)
  | Error err ->
    check bool "dangling artifact error" true (String.length (Error.to_string err) > 0)
;;

let () =
  run
    "Artifact_service"
    [ ( "extension_of_kind"
      , [ test_case "markdown" `Quick test_ext_markdown
        ; test_case "md alias" `Quick test_ext_md_alias
        ; test_case "json" `Quick test_ext_json
        ; test_case "html" `Quick test_ext_html
        ; test_case "csv" `Quick test_ext_csv
        ; test_case "text alias" `Quick test_ext_text_alias
        ; test_case "txt" `Quick test_ext_txt
        ; test_case "empty → txt" `Quick test_ext_empty_defaults_txt
        ; test_case "unknown passes" `Quick test_ext_unknown_passthrough
        ; test_case "uppercase" `Quick test_ext_uppercase
        ; test_case "whitespace trimmed" `Quick test_ext_whitespace
        ] )
    ; ( "mime_type_of_kind"
      , [ test_case "markdown" `Quick test_mime_markdown
        ; test_case "md alias" `Quick test_mime_md_alias
        ; test_case "json" `Quick test_mime_json
        ; test_case "html" `Quick test_mime_html
        ; test_case "csv" `Quick test_mime_csv
        ; test_case "text alias" `Quick test_mime_text_alias
        ; test_case "txt" `Quick test_mime_txt
        ; test_case "empty → text/plain" `Quick test_mime_empty_defaults
        ; test_case "unknown → octet-stream" `Quick test_mime_unknown_falls_back
        ; test_case "uppercase" `Quick test_mime_uppercase
        ; test_case "whitespace trimmed" `Quick test_mime_whitespace
        ] )
    ; "consistency", [ test_case "kind table" `Quick test_consistency_table ]
    ; ( "io"
      , [ test_case "save/list/get/overwrite" `Quick test_save_list_get_and_overwrite_text
        ; test_case "inline content" `Quick test_get_text_prefers_inline_content
        ; test_case "error paths" `Quick test_artifact_error_paths
        ] )
    ]
;;
