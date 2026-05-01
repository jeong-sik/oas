open Base
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
    ]
;;
