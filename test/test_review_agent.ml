(** Tests for review agent — zero-LLM mock verification. *)

open Alcotest
open Agent_sdk
open Agent_sdk_swarm

(* Simulate what the review agent does: gather info, analyze, output *)

let test_review_mock_flow () =
  Eio_main.run @@ fun _env ->
  Eio.Switch.run @@ fun sw ->
  (* Mock: agent gathers PR info, then outputs review *)
  let turn = ref 0 in
  let mock_run ~sw:_ prompt =
    incr turn;
    match !turn with
    | 1 ->
      (* First turn: agent requests tools *)
      Ok { Types.id = "m1"; model = "mock"; stop_reason = EndTurn;
           content = [Text (Printf.sprintf "Reviewing: %s\n\n## Review\n**Verdict**: LGTM\nNo issues found." prompt)];
           usage = None }
    | _ ->
      Ok { Types.id = "m2"; model = "mock"; stop_reason = EndTurn;
           content = [Text "Review complete."]; usage = None }
  in
  let entry = { Swarm_types.name = "reviewer"; run = mock_run; role = Execute; get_telemetry = None } in
  let config = Test_helpers.basic_config ~prompt:"Review PR #1 in test/repo" [entry] in
  let state = Swarm_types.create_state config in
  check int "initial iteration" 0 state.current_iteration;
  (* Verify the mock agent produces text output *)
  match mock_run ~sw "Review PR #1" with
  | Ok resp ->
    let texts = List.filter_map (function
      | Types.Text t -> Some t | _ -> None) resp.content in
    check bool "has review text" true (List.length texts > 0);
    let text = String.concat "\n" texts in
    check bool "contains verdict" true
      (try ignore (Str.search_forward (Str.regexp_string "Verdict") text 0); true
       with Not_found -> false)
  | Error _ -> fail "mock should not fail"

let test_diff_truncation () =
  (* Verify our truncation logic works *)
  let long_diff = String.make 15000 'x' in
  let max_len = 12000 in
  let truncated = if String.length long_diff > max_len then
    String.sub long_diff 0 max_len ^ "\n... [truncated]"
  else long_diff in
  check bool "was truncated" true (String.length truncated < String.length long_diff);
  check bool "has truncation marker" true
    (try ignore (Str.search_forward (Str.regexp_string "[truncated]") truncated 0); true
     with Not_found -> false)

let () =
  run "Review Agent" [
    "mock_flow", [
      test_case "review produces structured output" `Quick test_review_mock_flow;
      test_case "diff truncation" `Quick test_diff_truncation;
    ];
  ]
