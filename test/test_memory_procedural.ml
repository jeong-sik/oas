open Agent_sdk

let () =
  Alcotest.run
    "Memory_Procedural"
    [ ( "store_recall"
      , [ Alcotest.test_case "store and recall procedure" `Quick (fun () ->
            let mem = Memory.create () in
            let proc : Memory.procedure =
              { id = "pr1"
              ; pattern = "deploy error"
              ; action = "rollback then retry"
              ; success_count = 5
              ; failure_count = 1
              ; confidence = 5.0 /. 6.0
              ; last_used = 1000.0
              ; metadata = []
              }
            in
            Memory.store_procedure mem proc;
            match Memory.best_procedure mem ~pattern:"deploy" with
            | None -> Alcotest.fail "procedure not found"
            | Some found ->
              Alcotest.(check string) "id" "pr1" found.id;
              Alcotest.(check string) "action" "rollback then retry" found.action;
              Alcotest.(check int) "success" 5 found.success_count)
        ] )
    ; ( "matching"
      , [ Alcotest.test_case "pattern substring match" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "deploy error"
              ; action = "rollback"
              ; success_count = 10
              ; failure_count = 0
              ; confidence = 1.0
              ; last_used = 100.0
              ; metadata = []
              };
            Memory.store_procedure
              mem
              { id = "pr2"
              ; pattern = "test failure"
              ; action = "rerun"
              ; success_count = 3
              ; failure_count = 7
              ; confidence = 0.3
              ; last_used = 100.0
              ; metadata = []
              };
            Memory.store_procedure
              mem
              { id = "pr3"
              ; pattern = "deploy timeout"
              ; action = "increase timeout"
              ; success_count = 2
              ; failure_count = 1
              ; confidence = 0.667
              ; last_used = 100.0
              ; metadata = []
              };
            let matches = Memory.matching_procedures mem ~pattern:"deploy" () in
            Alcotest.(check int) "2 deploy matches" 2 (List.length matches);
            let best = List.hd matches in
            Alcotest.(check string) "highest confidence first" "pr1" best.id)
        ; Alcotest.test_case "min_confidence filter" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "task"
              ; action = "do it"
              ; success_count = 1
              ; failure_count = 9
              ; confidence = 0.1
              ; last_used = 100.0
              ; metadata = []
              };
            let matches =
              Memory.matching_procedures mem ~pattern:"task" ~min_confidence:0.5 ()
            in
            Alcotest.(check int) "filtered" 0 (List.length matches))
        ; Alcotest.test_case "no match returns None" `Quick (fun () ->
            let mem = Memory.create () in
            Alcotest.(check bool)
              "none"
              true
              (Memory.best_procedure mem ~pattern:"nonexistent" = None))
        ; Alcotest.test_case "filter narrows matches" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "deploy error"
              ; action = "rollback"
              ; success_count = 10
              ; failure_count = 0
              ; confidence = 1.0
              ; last_used = 100.0
              ; metadata = [ "team", `String "release" ]
              };
            Memory.store_procedure
              mem
              { id = "pr2"
              ; pattern = "deploy timeout"
              ; action = "retry"
              ; success_count = 8
              ; failure_count = 1
              ; confidence = 0.889
              ; last_used = 100.0
              ; metadata = [ "team", `String "ci" ]
              };
            let matches =
              Memory.matching_procedures
                mem
                ~pattern:"deploy"
                ~filter:(fun proc -> proc.action = "retry")
                ()
            in
            match matches with
            | [ proc ] -> Alcotest.(check string) "retry only" "pr2" proc.id
            | _ -> Alcotest.fail "expected one filtered procedure")
        ; Alcotest.test_case
            "find_procedure applies filter and min_confidence"
            `Quick
            (fun () ->
               let mem = Memory.create () in
               Memory.store_procedure
                 mem
                 { id = "pr1"
                 ; pattern = "deploy error"
                 ; action = "rollback"
                 ; success_count = 10
                 ; failure_count = 0
                 ; confidence = 1.0
                 ; last_used = 100.0
                 ; metadata = [ "team", `String "release" ]
                 };
               Memory.store_procedure
                 mem
                 { id = "pr2"
                 ; pattern = "deploy timeout"
                 ; action = "retry"
                 ; success_count = 1
                 ; failure_count = 4
                 ; confidence = 0.2
                 ; last_used = 100.0
                 ; metadata = [ "team", `String "ci" ]
                 };
               match
                 Memory.find_procedure
                   mem
                   ~pattern:"deploy"
                   ~min_confidence:0.5
                   ~filter:(fun proc -> proc.action = "rollback")
                   ()
               with
               | Some proc -> Alcotest.(check string) "rollback only" "pr1" proc.id
               | None -> Alcotest.fail "expected filtered procedure")
        ] )
    ; ( "record_outcome"
      , [ Alcotest.test_case "record_success updates confidence" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "task"
              ; action = "do it"
              ; success_count = 1
              ; failure_count = 1
              ; confidence = 0.5
              ; last_used = 100.0
              ; metadata = []
              };
            Memory.record_success mem "pr1";
            match Memory.best_procedure mem ~pattern:"task" with
            | Some proc ->
              Alcotest.(check int) "success_count" 2 proc.success_count;
              Alcotest.(check (float 0.01)) "confidence" 0.667 proc.confidence
            | None -> Alcotest.fail "not found")
        ; Alcotest.test_case "record_failure updates confidence" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "task"
              ; action = "do it"
              ; success_count = 1
              ; failure_count = 1
              ; confidence = 0.5
              ; last_used = 100.0
              ; metadata = []
              };
            Memory.record_failure mem "pr1";
            match Memory.best_procedure mem ~pattern:"task" with
            | Some proc ->
              Alcotest.(check int) "failure_count" 2 proc.failure_count;
              Alcotest.(check (float 0.01)) "confidence" 0.333 proc.confidence
            | None -> Alcotest.fail "not found")
        ; Alcotest.test_case "best_procedure touch updates last_used" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "deploy"
              ; action = "rollback"
              ; success_count = 3
              ; failure_count = 1
              ; confidence = 0.75
              ; last_used = 100.0
              ; metadata = []
              };
            match Memory.find_procedure mem ~pattern:"deploy" ~touch:true () with
            | Some proc ->
              Alcotest.(check bool) "touched" true (proc.last_used > 100.0);
              let persisted = Memory.best_procedure mem ~pattern:"deploy" in
              (match persisted with
               | Some persisted ->
                 Alcotest.(check (float 0.001))
                   "persisted last_used"
                   proc.last_used
                   persisted.last_used
               | None -> Alcotest.fail "persisted procedure missing")
            | None -> Alcotest.fail "not found")
        ] )
    ; ( "forget_count"
      , [ Alcotest.test_case "forget and count" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "x"
              ; action = "y"
              ; success_count = 0
              ; failure_count = 0
              ; confidence = 0.0
              ; last_used = 0.0
              ; metadata = []
              };
            Alcotest.(check int) "count 1" 1 (Memory.procedure_count mem);
            Memory.forget_procedure mem "pr1";
            Alcotest.(check int) "count 0" 0 (Memory.procedure_count mem))
        ] )
    ; ( "stats"
      , [ Alcotest.test_case "procedural count in stats" `Quick (fun () ->
            let mem = Memory.create () in
            Memory.store_procedure
              mem
              { id = "pr1"
              ; pattern = "x"
              ; action = "y"
              ; success_count = 0
              ; failure_count = 0
              ; confidence = 0.0
              ; last_used = 0.0
              ; metadata = []
              };
            let _, _, _, pr, _ = Memory.stats mem in
            Alcotest.(check int) "procedural count" 1 pr)
        ] )
    ]
;;
