(** Tests for Guardrail_llm — LLM-powered content guardrails. *)

open Agent_sdk

(* ── Mock judges ─────────────────────────────────────────────── *)

let always_pass : Guardrail_llm.judge = fun _prompt -> Ok (true, "pass")
let always_fail : Guardrail_llm.judge = fun _prompt -> Ok (false, "policy violation")
let error_judge : Guardrail_llm.judge = fun _prompt -> Error "LLM unavailable"

let keyword_judge keyword : Guardrail_llm.judge = fun prompt ->
  if String.length prompt > 0
     && (try ignore (Str.search_forward (Str.regexp_string keyword) prompt 0); true
         with Not_found -> false)
  then Ok (false, Printf.sprintf "contains forbidden keyword: %s" keyword)
  else Ok (true, "clean")

(* ── Helpers ─────────────────────────────────────────────────── *)

let make_messages texts =
  List.map (fun text ->
    Types.{ role = User; content = [Text text]; name = None; tool_call_id = None }
  ) texts

let make_response text =
  Types.{ id = "r1"; model = "m"; stop_reason = EndTurn;
    content = [Text text]; usage = None }

(* ── Parse judge response ────────────────────────────────────── *)

let test_parse_pass () =
  match Guardrail_llm.parse_judge_response "PASS" with
  | Ok (true, _) -> ()
  | _ -> Alcotest.fail "expected PASS"

let test_parse_pass_lowercase () =
  match Guardrail_llm.parse_judge_response "pass" with
  | Ok (true, _) -> ()
  | _ -> Alcotest.fail "expected pass"

let test_parse_fail_with_reason () =
  match Guardrail_llm.parse_judge_response "FAIL: contains PII data" with
  | Ok (false, reason) ->
    Alcotest.(check bool) "has reason" true (String.length reason > 0)
  | _ -> Alcotest.fail "expected FAIL"

let test_parse_fail_no_reason () =
  match Guardrail_llm.parse_judge_response "FAIL" with
  | Ok (false, _) -> ()
  | _ -> Alcotest.fail "expected FAIL"

let test_parse_unexpected () =
  match Guardrail_llm.parse_judge_response "MAYBE" with
  | Error _ -> ()
  | _ -> Alcotest.fail "expected error"

(* ── Input validator ─────────────────────────────────────────── *)

let test_input_pass () =
  let v = Guardrail_llm.make_input_validator
    ~name:"safe_check" ~policy_prompt:"Check safety" ~judge:always_pass in
  let msgs = make_messages ["hello world"] in
  match v.validate msgs with
  | Ok () -> ()
  | Error reason -> Alcotest.failf "should pass: %s" reason

let test_input_fail () =
  let v = Guardrail_llm.make_input_validator
    ~name:"safe_check" ~policy_prompt:"Check safety" ~judge:always_fail in
  let msgs = make_messages ["hello world"] in
  match v.validate msgs with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "should fail"

let test_input_judge_error () =
  let v = Guardrail_llm.make_input_validator
    ~name:"err_check" ~policy_prompt:"Check" ~judge:error_judge in
  let msgs = make_messages ["test"] in
  match v.validate msgs with
  | Error reason ->
    Alcotest.(check bool) "contains 'judge error'" true
      (try ignore (Str.search_forward (Str.regexp_string "judge error") reason 0); true
       with Not_found -> false)
  | Ok () -> Alcotest.fail "should fail on judge error"

let test_input_keyword_detection () =
  let v = Guardrail_llm.make_input_validator
    ~name:"pii_check" ~policy_prompt:"Reject PII"
    ~judge:(keyword_judge "SSN") in
  let clean = make_messages ["tell me about OCaml"] in
  let dirty = make_messages ["my SSN is 123-45-6789"] in
  (match v.validate clean with Ok () -> () | Error r -> Alcotest.failf "clean: %s" r);
  (match v.validate dirty with Error _ -> () | Ok () -> Alcotest.fail "dirty should fail")

(* ── Output validator ────────────────────────────────────────── *)

let test_output_pass () =
  let v = Guardrail_llm.make_output_validator
    ~name:"out_check" ~policy_prompt:"Check output" ~judge:always_pass in
  let resp = make_response "Here is the answer." in
  match v.validate resp with
  | Ok () -> ()
  | Error reason -> Alcotest.failf "should pass: %s" reason

let test_output_fail () =
  let v = Guardrail_llm.make_output_validator
    ~name:"out_check" ~policy_prompt:"Check output" ~judge:always_fail in
  let resp = make_response "Some text" in
  match v.validate resp with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "should fail"

(* ── Integration with Guardrails_async ───────────────────────── *)

let test_async_integration () =
  Eio_main.run @@ fun _env ->
  let iv = Guardrail_llm.make_input_validator
    ~name:"input_guard" ~policy_prompt:"Reject profanity"
    ~judge:(keyword_judge "bad_word") in
  let ov = Guardrail_llm.make_output_validator
    ~name:"output_guard" ~policy_prompt:"Check output"
    ~judge:always_pass in
  let config : Guardrails_async.t = {
    input_validators = [iv];
    output_validators = [ov];
  } in
  let msgs = make_messages ["clean input"] in
  match Guardrails_async.run_input config.input_validators msgs with
  | Guardrails_async.Pass -> ()
  | Guardrails_async.Fail { validator_name; reason } ->
    Alcotest.failf "input should pass: [%s] %s" validator_name reason

(* ── Suite ───────────────────────────────────────────────────── *)

let () =
  Alcotest.run "guardrail_llm" [
    ("parse", [
      Alcotest.test_case "pass" `Quick test_parse_pass;
      Alcotest.test_case "pass_lower" `Quick test_parse_pass_lowercase;
      Alcotest.test_case "fail_reason" `Quick test_parse_fail_with_reason;
      Alcotest.test_case "fail_no_reason" `Quick test_parse_fail_no_reason;
      Alcotest.test_case "unexpected" `Quick test_parse_unexpected;
    ]);
    ("input_validator", [
      Alcotest.test_case "pass" `Quick test_input_pass;
      Alcotest.test_case "fail" `Quick test_input_fail;
      Alcotest.test_case "judge_error" `Quick test_input_judge_error;
      Alcotest.test_case "keyword" `Quick test_input_keyword_detection;
    ]);
    ("output_validator", [
      Alcotest.test_case "pass" `Quick test_output_pass;
      Alcotest.test_case "fail" `Quick test_output_fail;
    ]);
    ("integration", [
      Alcotest.test_case "async" `Quick test_async_integration;
    ]);
  ]
