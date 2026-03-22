open Agent_sdk

let () = Alcotest.run "Memory_Access" [
  "deny_by_default", [
    Alcotest.test_case "store denied without grant" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      match Memory_access.store acl ~agent:"alice" ~tier:Working "key" (`String "v") with
      | Ok () -> Alcotest.fail "should be denied"
      | Error (Denied _) -> ()
    );

    Alcotest.test_case "recall denied without grant" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      match Memory_access.recall acl ~agent:"alice" ~tier:Working "key" with
      | Ok _ -> Alcotest.fail "should be denied"
      | Error (Denied _) -> ()
    );
  ];

  "grant_revoke", [
    Alcotest.test_case "grant allows access" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "alice"; tier = Working;
        key_pattern = "*"; permission = ReadWrite;
      };
      (match Memory_access.store acl ~agent:"alice" ~tier:Working "key" (`String "v") with
       | Ok () -> ()
       | Error _ -> Alcotest.fail "should be allowed");
      match Memory_access.recall acl ~agent:"alice" ~tier:Working "key" with
      | Ok (Some (`String "v")) -> ()
      | _ -> Alcotest.fail "should recall value"
    );

    Alcotest.test_case "revoke removes access" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "alice"; tier = Working;
        key_pattern = "*"; permission = ReadWrite;
      };
      Memory_access.revoke acl ~agent_name:"alice" ~tier:Working;
      match Memory_access.store acl ~agent:"alice" ~tier:Working "key" (`String "v") with
      | Ok () -> Alcotest.fail "should be denied after revoke"
      | Error (Denied _) -> ()
    );

    Alcotest.test_case "revoke_all clears everything" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "alice"; tier = Working;
        key_pattern = "*"; permission = ReadWrite;
      };
      Memory_access.grant acl {
        agent_name = "alice"; tier = Scratchpad;
        key_pattern = "*"; permission = Read;
      };
      Memory_access.revoke_all acl ~agent_name:"alice";
      Alcotest.(check int) "no policies" 0
        (List.length (Memory_access.policies_for acl "alice"))
    );
  ];

  "permission_levels", [
    Alcotest.test_case "read-only cannot write" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "bob"; tier = Working;
        key_pattern = "*"; permission = Read;
      };
      (* First store directly to memory so there's something to read *)
      ignore (Memory.store mem ~tier:Working "key" (`String "v"));
      (match Memory_access.recall acl ~agent:"bob" ~tier:Working "key" with
       | Ok (Some _) -> ()
       | _ -> Alcotest.fail "should read");
      match Memory_access.store acl ~agent:"bob" ~tier:Working "key" (`String "v2") with
      | Ok () -> Alcotest.fail "read-only should not write"
      | Error (Denied _) -> ()
    );

    Alcotest.test_case "write-only cannot read" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "charlie"; tier = Working;
        key_pattern = "*"; permission = Write;
      };
      (match Memory_access.store acl ~agent:"charlie" ~tier:Working "key" (`String "v") with
       | Ok () -> ()
       | Error _ -> Alcotest.fail "should write");
      match Memory_access.recall acl ~agent:"charlie" ~tier:Working "key" with
      | Ok _ -> Alcotest.fail "write-only should not read"
      | Error (Denied _) -> ()
    );
  ];

  "key_patterns", [
    Alcotest.test_case "prefix pattern match" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "alice"; tier = Working;
        key_pattern = "shared_"; permission = ReadWrite;
      };
      (match Memory_access.store acl ~agent:"alice" ~tier:Working "shared_goal" (`String "v") with
       | Ok () -> ()
       | Error _ -> Alcotest.fail "prefix should match");
      match Memory_access.store acl ~agent:"alice" ~tier:Working "private_key" (`String "v") with
      | Ok () -> Alcotest.fail "non-matching prefix should be denied"
      | Error (Denied _) -> ()
    );
  ];

  "episodic_procedural", [
    Alcotest.test_case "episodic access control" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "alice"; tier = Episodic;
        key_pattern = "*"; permission = ReadWrite;
      };
      let ep : Memory.episode = {
        id = "ep1"; timestamp = 100.0; participants = ["alice"];
        action = "test"; outcome = Neutral; salience = 0.9; metadata = [];
      } in
      (match Memory_access.store_episode acl ~agent:"alice" ep with
       | Ok () -> ()
       | Error _ -> Alcotest.fail "should store episode");
      (* bob has no access *)
      match Memory_access.recall_episodes acl ~agent:"bob" ~now:200.0 () with
      | Ok _ -> Alcotest.fail "bob should be denied"
      | Error (Denied _) -> ()
    );

    Alcotest.test_case "procedural access control" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Memory_access.grant acl {
        agent_name = "alice"; tier = Procedural;
        key_pattern = "*"; permission = ReadWrite;
      };
      let proc : Memory.procedure = {
        id = "pr1"; pattern = "deploy"; action = "rollback";
        success_count = 5; failure_count = 1;
        confidence = 0.833; last_used = 100.0; metadata = [];
      } in
      (match Memory_access.store_procedure acl ~agent:"alice" proc with
       | Ok () -> ()
       | Error _ -> Alcotest.fail "should store procedure");
      (match Memory_access.find_procedure acl ~agent:"alice" ~pattern:"deploy"
                ~min_confidence:0.5 ~touch:true () with
       | Ok (Some found) ->
         Alcotest.(check string) "allowed find" "pr1" found.id;
         Alcotest.(check bool) "touch updates last_used" true
           (found.last_used > 100.0)
       | Ok None -> Alcotest.fail "alice should find procedure"
       | Error _ -> Alcotest.fail "alice should be allowed");
      match Memory_access.best_procedure acl ~agent:"bob" ~pattern:"deploy" with
      | Ok _ -> Alcotest.fail "bob should be denied"
      | Error (Denied _) -> ()
    );

    Alcotest.test_case "find_procedure denied without read access" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      match Memory_access.find_procedure acl ~agent:"bob" ~pattern:"deploy" () with
      | Ok _ -> Alcotest.fail "bob should be denied"
      | Error (Denied _) -> ()
    );
  ];

  "check_utility", [
    Alcotest.test_case "check returns bool" `Quick (fun () ->
      let mem = Memory.create () in
      let acl = Memory_access.create mem in
      Alcotest.(check bool) "denied" false
        (Memory_access.check acl ~agent:"alice" ~tier:Working ~key:"k" Read);
      Memory_access.grant acl {
        agent_name = "alice"; tier = Working;
        key_pattern = "*"; permission = ReadWrite;
      };
      Alcotest.(check bool) "granted" true
        (Memory_access.check acl ~agent:"alice" ~tier:Working ~key:"k" Read)
    );
  ];

  "error_format", [
    Alcotest.test_case "error string" `Quick (fun () ->
      let err = Memory_access.Denied {
        agent_name = "bob"; tier = Working; key = "secret"; needed = Write;
      } in
      let s = Memory_access.access_error_to_string err in
      Alcotest.(check bool) "contains agent" true (String.length s > 0);
      Alcotest.(check bool) "contains bob" true
        (Util.string_contains ~needle:"bob" s)
    );
  ];
]
