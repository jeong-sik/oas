(** Tests for {!Llm_provider.Canonical_tool} — RFC-OAS-024 WP8 Increment 1.

    Covers the result projection (round-trip fidelity, is_error, totality). The
    call projection and reasoning types are deferred to Increment 2 (no live
    consumer yet), so they are not implemented or tested here. The {e wiring} of
    [tool_result_of_block] into the turn pipeline is covered by the inline tests
    on [Pipeline_stage_prepare.last_tool_results_from]. *)

module Ct = Llm_provider.Canonical_tool
module Types = Llm_provider.Types

let json_eq = Alcotest.testable Yojson.Safe.pp Yojson.Safe.equal

let test_result_roundtrip_preserves_fields () =
  let json = `Assoc [ "rows", `Int 3 ] in
  let blocks = [ Types.Text "ignored" ] in
  let block =
    Types.ToolResult
      { tool_use_id = "call_abc"
      ; content = "3 rows"
      ; is_error = false
      ; json = Some json
      ; content_blocks = Some blocks
      }
  in
  match Ct.tool_result_of_block block with
  | None -> Alcotest.fail "expected Some projection for a ToolResult block"
  | Some proj ->
    Alcotest.(check string) "call_id" "call_abc" proj.call_id;
    Alcotest.(check string) "content" "3 rows" proj.content;
    Alcotest.(check bool) "is_error" false proj.is_error;
    (match proj.structured_content with
     | Some j -> Alcotest.check json_eq "structured_content == json" json j
     | None -> Alcotest.fail "structured_content dropped");
    (match proj.content_blocks with
     | Some bs -> Alcotest.(check int) "content_blocks length" 1 (List.length bs)
     | None -> Alcotest.fail "content_blocks dropped")
;;

let test_result_preserves_is_error () =
  let block =
    Types.ToolResult
      { tool_use_id = "call_err"
      ; content = "boom"
      ; is_error = true
      ; json = None
      ; content_blocks = None
      }
  in
  match Ct.tool_result_of_block block with
  | Some proj ->
    Alcotest.(check bool) "is_error preserved" true proj.is_error;
    Alcotest.(check (option json_eq))
      "structured_content None when json None"
      None
      proj.structured_content
  | None -> Alcotest.fail "expected Some projection"
;;

(* All six non-ToolResult content_block constructors project to None. *)
let test_result_none_for_non_toolresult () =
  let cases =
    [ Types.Text "hi"
    ; Types.Thinking { thinking_type = "thinking"; content = "..." }
    ; Types.RedactedThinking "redacted"
    ; Types.ToolUse { id = "call_x"; name = "t"; input = `Null }
    ; Types.Image { media_type = "image/png"; data = "AAAA"; source_type = Types.Base64 }
    ; Types.Document
        { media_type = "application/pdf"; data = "JVBE"; source_type = Types.Base64 }
    ; Types.Audio { media_type = "audio/wav"; data = "UklG"; source_type = Types.Base64 }
    ]
  in
  List.iter
    (fun block ->
       Alcotest.(check bool)
         "non-ToolResult projects to None"
         true
         (Option.is_none (Ct.tool_result_of_block block)))
    cases
;;

let () =
  Alcotest.run
    "canonical_tool"
    [ ( "tool_result_of_block"
      , [ Alcotest.test_case
            "roundtrip preserves fields"
            `Quick
            test_result_roundtrip_preserves_fields
        ; Alcotest.test_case
            "preserves is_error / None json"
            `Quick
            test_result_preserves_is_error
        ; Alcotest.test_case
            "None for non-ToolResult"
            `Quick
            test_result_none_for_non_toolresult
        ] )
    ]
;;
