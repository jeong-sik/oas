(** Tests for Uncertain module — typed wrapper for non-deterministic values. *)

open Agent_sdk

let () =
  Alcotest.run
    "Uncertain"
    [ ( "constructors"
      , [ Alcotest.test_case "from_llm defaults" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"qwen3.5-9b" "hello" in
            Alcotest.(check string) "value" "hello" (Uncertain.value u);
            Alcotest.(check (float 0.01))
              "default confidence"
              0.5
              (Uncertain.confidence u);
            Alcotest.(check bool) "not deterministic" false (Uncertain.is_deterministic u))
        ; Alcotest.test_case "from_llm with params" `Quick (fun () ->
            let u =
              Uncertain.from_llm ~model:"claude" ~temperature:0.7 ~confidence:0.9 42
            in
            Alcotest.(check int) "value" 42 (Uncertain.value u);
            Alcotest.(check (float 0.01)) "confidence" 0.9 (Uncertain.confidence u);
            match Uncertain.provenance_of u with
            | LLM { model; temperature } ->
              Alcotest.(check string) "model" "claude" model;
              Alcotest.(check (option (float 0.01))) "temp" (Some 0.7) temperature
            | _ -> Alcotest.fail "expected LLM provenance")
        ; Alcotest.test_case "from_heuristic" `Quick (fun () ->
            let u = Uncertain.from_heuristic ~name:"keyword_match" "result" in
            Alcotest.(check (float 0.01))
              "default confidence"
              0.3
              (Uncertain.confidence u);
            match Uncertain.provenance_of u with
            | Heuristic { name } -> Alcotest.(check string) "name" "keyword_match" name
            | _ -> Alcotest.fail "expected Heuristic provenance")
        ; Alcotest.test_case "deterministic" `Quick (fun () ->
            let u = Uncertain.deterministic 100 in
            Alcotest.(check int) "value" 100 (Uncertain.value u);
            Alcotest.(check (float 0.01)) "confidence" 1.0 (Uncertain.confidence u);
            Alcotest.(check bool) "is deterministic" true (Uncertain.is_deterministic u))
        ; Alcotest.test_case "from_user" `Quick (fun () ->
            let u = Uncertain.from_user "input" in
            Alcotest.(check (float 0.01)) "confidence" 1.0 (Uncertain.confidence u);
            match Uncertain.provenance_of u with
            | User -> ()
            | _ -> Alcotest.fail "expected User provenance")
        ] )
    ; ( "confidence_clamping"
      , [ Alcotest.test_case "clamp above 1.0" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:1.5 "x" in
            Alcotest.(check (float 0.01)) "clamped" 1.0 (Uncertain.confidence u))
        ; Alcotest.test_case "clamp below 0.0" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:(-0.3) "x" in
            Alcotest.(check (float 0.01)) "clamped" 0.0 (Uncertain.confidence u))
        ] )
    ; ( "transformations"
      , [ Alcotest.test_case "map preserves metadata" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.8 "hello" in
            let u' = Uncertain.map String.uppercase_ascii u in
            Alcotest.(check string) "mapped" "HELLO" (Uncertain.value u');
            Alcotest.(check (float 0.01))
              "confidence preserved"
              0.8
              (Uncertain.confidence u'))
        ; Alcotest.test_case "bind takes min confidence" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.8 "hello" in
            let u' =
              Uncertain.bind
                (fun s ->
                   Uncertain.from_heuristic ~name:"len" ~confidence:0.4 (String.length s))
                u
            in
            Alcotest.(check int) "value" 5 (Uncertain.value u');
            Alcotest.(check (float 0.01)) "min confidence" 0.4 (Uncertain.confidence u'))
        ; Alcotest.test_case "with_confidence" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" "x" in
            let u' = Uncertain.with_confidence 0.95 u in
            Alcotest.(check (float 0.01)) "overridden" 0.95 (Uncertain.confidence u'))
        ] )
    ; ( "unwrapping"
      , [ Alcotest.test_case "unwrap extracts value" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" "data" in
            Alcotest.(check string) "unwrapped" "data" (Uncertain.unwrap u))
        ; Alcotest.test_case "to_result ok above threshold" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.8 "ok" in
            match Uncertain.to_result ~min_confidence:0.7 u with
            | Ok v -> Alcotest.(check string) "value" "ok" v
            | Error _ -> Alcotest.fail "expected Ok")
        ; Alcotest.test_case "to_result error below threshold" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.3 "low" in
            match Uncertain.to_result ~min_confidence:0.7 u with
            | Ok _ -> Alcotest.fail "expected Error"
            | Error msg ->
              Alcotest.(check bool) "has confidence info" true (String.length msg > 0))
        ] )
    ; ( "predicates"
      , [ Alcotest.test_case "is_confident" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.8 "x" in
            Alcotest.(check bool) "above" true (Uncertain.is_confident ~threshold:0.7 u);
            Alcotest.(check bool) "below" false (Uncertain.is_confident ~threshold:0.9 u))
        ; Alcotest.test_case "is_confident at exact threshold" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.7 "x" in
            Alcotest.(check bool) "equal" true (Uncertain.is_confident ~threshold:0.7 u))
        ] )
    ; ( "stress"
      , [ Alcotest.test_case "default stress is zero" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" "x" in
            let s = Uncertain.stress_of u in
            Alcotest.(check (option (float 0.01))) "no pressure" None s.context_pressure;
            Alcotest.(check bool) "no time pressure" false s.time_pressure;
            Alcotest.(check int) "zero retries" 0 s.retry_count)
        ; Alcotest.test_case "custom stress" `Quick (fun () ->
            let stress =
              Uncertain.
                { context_pressure = Some 0.85; time_pressure = true; retry_count = 2 }
            in
            let u = Uncertain.from_llm ~model:"m" ~stress "x" in
            let s = Uncertain.stress_of u in
            Alcotest.(check (option (float 0.01)))
              "pressure"
              (Some 0.85)
              s.context_pressure;
            Alcotest.(check bool) "time pressure" true s.time_pressure;
            Alcotest.(check int) "retries" 2 s.retry_count)
        ] )
    ; ( "json_roundtrip"
      , [ Alcotest.test_case "LLM provenance roundtrip" `Quick (fun () ->
            let p = Uncertain.LLM { model = "qwen"; temperature = Some 0.3 } in
            let json = Uncertain.provenance_to_yojson p in
            match Uncertain.provenance_of_yojson json with
            | Ok p' ->
              (match p' with
               | LLM { model; temperature } ->
                 Alcotest.(check string) "model" "qwen" model;
                 Alcotest.(check (option (float 0.01))) "temp" (Some 0.3) temperature
               | _ -> Alcotest.fail "wrong provenance variant")
            | Error e -> Alcotest.fail e)
        ; Alcotest.test_case "full value roundtrip" `Quick (fun () ->
            let u = Uncertain.from_llm ~model:"m" ~confidence:0.7 "hello" in
            let json = Uncertain.to_yojson (fun s -> `String s) u in
            let value_of j =
              match j with
              | `String s -> Ok s
              | _ -> Error "expected string"
            in
            match Uncertain.of_yojson value_of json with
            | Ok u' ->
              Alcotest.(check string) "value" "hello" (Uncertain.value u');
              Alcotest.(check (float 0.01)) "confidence" 0.7 (Uncertain.confidence u')
            | Error e -> Alcotest.fail e)
        ; Alcotest.test_case "stress roundtrip" `Quick (fun () ->
            let s =
              Uncertain.
                { context_pressure = Some 0.9; time_pressure = true; retry_count = 3 }
            in
            let json = Uncertain.stress_to_yojson s in
            match Uncertain.stress_of_yojson json with
            | Ok s' ->
              Alcotest.(check (option (float 0.01)))
                "pressure"
                (Some 0.9)
                s'.context_pressure;
              Alcotest.(check int) "retries" 3 s'.retry_count
            | Error e -> Alcotest.fail e)
        ] )
    ]
;;
