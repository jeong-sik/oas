(** Coverage for [Model_registry] (oas#1175 step 2 — vendor model alias
    SSOT).  The module is the single place where short→full Anthropic
    model IDs are resolved; new models are added here only.

    [lib/types.ml:76] documents [Model_registry.resolve_model_id] in
    the public docstring of the [model] type, but the parent
    [Agent_sdk] surface was missing the corresponding re-export.  This
    PR adds [module Model_registry = Model_registry] (parallel to the
    [Memory_tools_parse] precedent in #1240) so the documented API is
    actually reachable, then pins each alias arm + the pass-through
    contract.

    Two public values pinned (per [lib/model_registry.mli] surface):
    1. [default_model_id] — env-resolved constant.
    2. [resolve_model_id alias] — pass-through default for unknown
       names; intentional (custom-model support) so the test pins it
       to prevent an accidental fail-closed refactor. *)

open Alcotest
open Agent_sdk

(* ── default_model_id ───────────────────────────────── *)

let test_default_model_id_non_empty () =
  check
    bool
    "default_model_id is non-empty"
    true
    (String.length Model_registry.default_model_id > 0)
;;

(* ── resolve_model_id — explicit aliases ────────────── *)

let test_resolve_opus_4_6_alias () =
  check
    string
    "claude-opus-4-6 → full ID"
    "claude-opus-4-6-20250514"
    (Model_registry.resolve_model_id "claude-opus-4-6");
  check
    string
    "opus shorthand → full ID"
    "claude-opus-4-6-20250514"
    (Model_registry.resolve_model_id "opus")
;;

let test_resolve_sonnet_4_6_alias () =
  check
    string
    "claude-sonnet-4-6 → full ID"
    "claude-sonnet-4-6-20250514"
    (Model_registry.resolve_model_id "claude-sonnet-4-6");
  check
    string
    "sonnet shorthand → full ID"
    "claude-sonnet-4-6-20250514"
    (Model_registry.resolve_model_id "sonnet")
;;

let test_resolve_opus_4_5_alias () =
  check
    string
    "claude-opus-4-5 → full ID"
    "claude-opus-4-5-20251101"
    (Model_registry.resolve_model_id "claude-opus-4-5")
;;

let test_resolve_sonnet_4_alias () =
  check
    string
    "claude-sonnet-4 → full ID"
    "claude-sonnet-4-20250514"
    (Model_registry.resolve_model_id "claude-sonnet-4")
;;

let test_resolve_haiku_4_5_alias () =
  check
    string
    "claude-haiku-4-5 → full ID"
    "claude-haiku-4-5-20251001"
    (Model_registry.resolve_model_id "claude-haiku-4-5");
  check
    string
    "haiku shorthand → full ID"
    "claude-haiku-4-5-20251001"
    (Model_registry.resolve_model_id "haiku")
;;

let test_resolve_3_7_alias () =
  check
    string
    "claude-3-7-sonnet → full ID"
    "claude-3-7-sonnet-20250219"
    (Model_registry.resolve_model_id "claude-3-7-sonnet")
;;

(* ── resolve_model_id — pass-through ────────────────── *)

let test_resolve_unknown_passes_through () =
  (* Pass-through is intentional: callers can request a custom model
     by full ID without registering it here.  Pin the contract so a
     future "fail-closed" refactor is a deliberate decision rather
     than an accidental compile result. *)
  check
    string
    "custom model passes through"
    "anthropic.claude-vendor-tagged"
    (Model_registry.resolve_model_id "anthropic.claude-vendor-tagged")
;;

let test_resolve_full_id_passes_through () =
  check
    string
    "full ID round-trips"
    "claude-opus-4-6-20250514"
    (Model_registry.resolve_model_id "claude-opus-4-6-20250514")
;;

let test_resolve_empty_passes_through () =
  check
    string
    "empty string passes through (caller responsibility)"
    ""
    (Model_registry.resolve_model_id "")
;;

(* ── Idempotency: resolve(resolve x) = resolve x ────── *)

let test_resolve_idempotent () =
  let cases =
    [ "opus"
    ; "sonnet"
    ; "haiku"
    ; "claude-opus-4-6"
    ; "claude-3-7-sonnet"
    ; "custom-model"
    ; ""
    ]
  in
  List.iter
    (fun input ->
       let once = Model_registry.resolve_model_id input in
       let twice = Model_registry.resolve_model_id once in
       check string (Printf.sprintf "idempotent for %S" input) once twice)
    cases
;;

let () =
  run
    "model_registry"
    [ "default_model_id", [ test_case "non-empty" `Quick test_default_model_id_non_empty ]
    ; ( "resolve_model_id (aliases)"
      , [ test_case "opus 4-6 + opus shorthand" `Quick test_resolve_opus_4_6_alias
        ; test_case "sonnet 4-6 + sonnet shorthand" `Quick test_resolve_sonnet_4_6_alias
        ; test_case "opus 4-5" `Quick test_resolve_opus_4_5_alias
        ; test_case "sonnet 4" `Quick test_resolve_sonnet_4_alias
        ; test_case "haiku 4-5 + haiku shorthand" `Quick test_resolve_haiku_4_5_alias
        ; test_case "3-7 sonnet" `Quick test_resolve_3_7_alias
        ] )
    ; ( "resolve_model_id (pass-through)"
      , [ test_case "custom model" `Quick test_resolve_unknown_passes_through
        ; test_case "full ID round-trip" `Quick test_resolve_full_id_passes_through
        ; test_case "empty string" `Quick test_resolve_empty_passes_through
        ] )
    ; ( "resolve_model_id (idempotency)"
      , [ test_case "resolve∘resolve = resolve" `Quick test_resolve_idempotent ] )
    ]
;;
