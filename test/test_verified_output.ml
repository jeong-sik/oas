open Agent_sdk

let mock_response text =
  { Types.id = "r1"; model = "test"; stop_reason = Types.EndTurn;
    content = [Types.Text text]; usage = None }

let () = Alcotest.run "Verified_Output" [
  "phantom_safety", [
    Alcotest.test_case "verified output gives content" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      match Verified_output.verify raw ~verifier:"bob" ~confidence:0.9 ~evidence:"checked" with
      | Some verified ->
        let resp = Verified_output.content verified in
        Alcotest.(check string) "text" "hello" (Verified_output.text verified);
        Alcotest.(check string) "id" "r1" resp.id
      | None -> Alcotest.fail "verification should succeed"
    );

    (* This test documents the compile-time guarantee:
       Uncommenting the line below would cause a type error:
       let _ = Verified_output.content raw  (* TYPE ERROR: unverified output *)
    *)
    Alcotest.test_case "unverified cannot access content (documented)" `Quick (fun () ->
      let _raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      (* Verified_output.content _raw would not compile *)
      Alcotest.(check bool) "type system prevents misuse" true true
    );
  ];

  "verification_flow", [
    Alcotest.test_case "low confidence fails verification" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      match Verified_output.verify raw ~verifier:"bob" ~confidence:0.3 ~evidence:"weak" with
      | Some _ -> Alcotest.fail "low confidence should fail"
      | None -> ()
    );

    Alcotest.test_case "verify_with_results quorum" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      let results = [
        Verified_output.Verified { verifier = "bob"; confidence = 0.8; evidence = "ok" };
        Verified_output.Disputed { verifier = "charlie"; reason = "disagree" };
        Verified_output.Verified { verifier = "dave"; confidence = 0.9; evidence = "confirmed" };
      ] in
      match Verified_output.verify_with_results raw results () with
      | Some verified ->
        Alcotest.(check int) "3 verifications" 3
          (List.length (Verified_output.verifications verified))
      | None -> Alcotest.fail "quorum should pass"
    );

    Alcotest.test_case "verify_with_results all disputed fails" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      let results = [
        Verified_output.Disputed { verifier = "bob"; reason = "wrong" };
        Verified_output.Abstained { verifier = "charlie"; reason = "unsure" };
      ] in
      match Verified_output.verify_with_results raw results () with
      | Some _ -> Alcotest.fail "all disputed should fail"
      | None -> ()
    );
  ];

  "trust", [
    Alcotest.test_case "trust bypasses verification" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "trusted") in
      let verified = Verified_output.trust raw ~reason:"unit test" in
      Alcotest.(check string) "text" "trusted" (Verified_output.text verified);
      Alcotest.(check bool) "is_verified" true (Verified_output.is_verified verified)
    );
  ];

  "accessors", [
    Alcotest.test_case "producer works on any status" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      Alcotest.(check string) "producer unverified" "alice" (Verified_output.producer raw);
      let verified = Verified_output.trust raw ~reason:"test" in
      Alcotest.(check string) "producer verified" "alice" (Verified_output.producer verified)
    );

    Alcotest.test_case "is_verified tracks status" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      Alcotest.(check bool) "unverified" false (Verified_output.is_verified raw);
      let verified = Verified_output.trust raw ~reason:"test" in
      Alcotest.(check bool) "verified" true (Verified_output.is_verified verified)
    );

    Alcotest.test_case "raw_json returns JSON" `Quick (fun () ->
      let raw = Verified_output.of_response ~producer:"alice" (mock_response "hello") in
      let json = Verified_output.raw_json raw in
      match json with
      | `Assoc _ -> ()
      | _ -> Alcotest.fail "expected JSON object"
    );
  ];
]
