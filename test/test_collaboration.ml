open Agent_sdk

let () =
  let open Alcotest in
  run "Collaboration" [

    "create", [
      test_case "default id prefix" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        check bool "starts with collab-" true
          (String.length c.id > 7
           && String.sub c.id 0 7 = "collab-"));

      test_case "custom id" `Quick (fun () ->
        let c = Collaboration.create ~id:"my-collab" ~goal:"test" () in
        check string "id" "my-collab" c.id);

      test_case "goal is set" `Quick (fun () ->
        let c = Collaboration.create ~goal:"build widget" () in
        check string "goal" "build widget" c.goal);

      test_case "default phase is Bootstrapping" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        check bool "phase" true
          (c.phase = Collaboration.Bootstrapping));

      test_case "empty participants" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        check int "participants" 0 (List.length c.participants));

      test_case "custom shared_context" `Quick (fun () ->
        let ctx = Context.create () in
        Context.set ctx "key" (`String "val");
        let c = Collaboration.create ~shared_context:ctx ~goal:"test" () in
        check (option string) "context key"
          (Some "val")
          (match Context.get c.shared_context "key" with
           | Some (`String v) -> Some v
           | _ -> None));

      test_case "outcome is None" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        check (option string) "outcome" None c.outcome);
    ];

    "participant_crud", [
      test_case "add participant" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let p = Collaboration.{
          name = "alice"; role = Some "lead";
          state = Planned; joined_at = None;
          finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c p in
        check int "count" 1 (List.length c.participants));

      test_case "find participant" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let p = Collaboration.{
          name = "bob"; role = None; state = Joined;
          joined_at = Some 1.0; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c p in
        let found = Collaboration.find_participant c "bob" in
        check bool "found" true (Option.is_some found);
        check string "name" "bob" (Option.get found).name);

      test_case "find missing participant" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let found = Collaboration.find_participant c "ghost" in
        check bool "not found" true (Option.is_none found));

      test_case "update participant state" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let p = Collaboration.{
          name = "carol"; role = None; state = Planned;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c p in
        let c = Collaboration.update_participant c "carol"
          (fun p -> Collaboration.{ p with state = Working }) in
        let found = Collaboration.find_participant c "carol" in
        check bool "is Working" true
          ((Option.get found).state = Collaboration.Working));

      test_case "update missing participant is no-op" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let p = Collaboration.{
          name = "dave"; role = None; state = Planned;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c p in
        let c = Collaboration.update_participant c "nobody"
          (fun p -> Collaboration.{ p with state = Working }) in
        check int "still 1" 1 (List.length c.participants);
        let found = Collaboration.find_participant c "dave" in
        check bool "dave still Planned" true
          ((Option.get found).state = Collaboration.Planned));

      test_case "remove participant" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let mk name = Collaboration.{
          name; role = None; state = Joined;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c (mk "a") in
        let c = Collaboration.add_participant c (mk "b") in
        let c = Collaboration.remove_participant c "a" in
        check int "count" 1 (List.length c.participants);
        check bool "b remains" true
          (Option.is_some (Collaboration.find_participant c "b")));

      test_case "active participants" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let mk name state = Collaboration.{
          name; role = None; state;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c (mk "a" Joined) in
        let c = Collaboration.add_participant c (mk "b" Working) in
        let c = Collaboration.add_participant c (mk "c" Done) in
        let c = Collaboration.add_participant c (mk "d" Planned) in
        let active = Collaboration.active_participants c in
        check int "active count" 2 (List.length active));
    ];

    "artifacts_contributions", [
      test_case "add artifact" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let a = Collaboration.{
          id = "art-1"; name = "report"; kind = "document";
          producer = "alice"; created_at = 1.0;
        } in
        let c = Collaboration.add_artifact c a in
        check int "count" 1 (List.length c.artifacts);
        check string "name" "report" (List.hd c.artifacts).name);

      test_case "add contribution" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let v = Collaboration.{
          agent = "bob"; kind = "vote";
          content = "merge?: yes"; created_at = 2.0;
        } in
        let c = Collaboration.add_contribution c v in
        check int "count" 1 (List.length c.contributions);
        check string "content" "merge?: yes" (List.hd c.contributions).content);
    ];

    "phase_lifecycle", [
      test_case "set phase" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Active in
        check bool "is Active" true (c.phase = Collaboration.Active));

      test_case "is_terminal completed" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Completed in
        check bool "terminal" true (Collaboration.is_terminal c));

      test_case "is_terminal failed" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Failed in
        check bool "terminal" true (Collaboration.is_terminal c));

      test_case "is_terminal cancelled" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Cancelled in
        check bool "terminal" true (Collaboration.is_terminal c));

      test_case "not terminal bootstrapping" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "not terminal active" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Active in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "not terminal waiting" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Waiting_on_participants in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "not terminal finalizing" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_phase c Finalizing in
        check bool "not terminal" false (Collaboration.is_terminal c));
    ];

    "outcome", [
      test_case "set outcome" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_outcome c "merged" in
        check (option string) "outcome" (Some "merged") c.outcome);

      test_case "overwrite outcome" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.set_outcome c "draft" in
        let c = Collaboration.set_outcome c "final" in
        check (option string) "outcome" (Some "final") c.outcome);
    ];

    "json_roundtrip", [
      test_case "minimal roundtrip" `Quick (fun () ->
        let c = Collaboration.create ~id:"rt-1" ~goal:"round" () in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check string "id" c.id c2.id;
        check string "goal" c.goal c2.goal;
        check bool "phase" true (c2.phase = Collaboration.Bootstrapping));

      test_case "roundtrip with participants" `Quick (fun () ->
        let c = Collaboration.create ~id:"rt-2" ~goal:"collab" () in
        let p = Collaboration.{
          name = "eve"; role = Some "reviewer"; state = Working;
          joined_at = Some 100.0; finished_at = None;
          summary = Some "reviewing";
        } in
        let c = Collaboration.add_participant c p in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check int "participants" 1 (List.length c2.participants);
        let p2 = List.hd c2.participants in
        check string "name" "eve" p2.name;
        check (option string) "role" (Some "reviewer") p2.role;
        check bool "state" true (p2.state = Collaboration.Working));

      test_case "roundtrip with artifacts and contributions" `Quick (fun () ->
        let c = Collaboration.create ~id:"rt-3" ~goal:"contrib" () in
        let a = Collaboration.{
          id = "a1"; name = "patch"; kind = "code";
          producer = "frank"; created_at = 50.0;
        } in
        let v = Collaboration.{
          agent = "grace"; kind = "review";
          content = "lgtm"; created_at = 60.0;
        } in
        let c = Collaboration.add_artifact c a in
        let c = Collaboration.add_contribution c v in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check int "artifacts" 1 (List.length c2.artifacts);
        check int "contributions" 1 (List.length c2.contributions);
        check string "artifact name" "patch" (List.hd c2.artifacts).name;
        check string "contribution content" "lgtm" (List.hd c2.contributions).content);

      test_case "roundtrip with metadata" `Quick (fun () ->
        let c = Collaboration.create ~id:"rt-4" ~goal:"meta" () in
        let c = { c with metadata = [("key", `String "val")] } in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check int "metadata len" 1 (List.length c2.metadata);
        check string "metadata key" "key" (fst (List.hd c2.metadata));
        check bool "metadata val" true
          (snd (List.hd c2.metadata) = `String "val"));

      test_case "roundtrip with outcome and max_participants" `Quick (fun () ->
        let c = Collaboration.create ~id:"rt-5" ~goal:"full" () in
        let c = Collaboration.set_outcome c "done" in
        let c = { c with max_participants = Some 5 } in
        let c = Collaboration.set_phase c Completed in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check (option string) "outcome" (Some "done") c2.outcome;
        check (option int) "max_participants" (Some 5) c2.max_participants;
        check bool "phase" true (c2.phase = Collaboration.Completed));

      test_case "roundtrip preserves shared_context" `Quick (fun () ->
        let ctx = Context.create () in
        Context.set ctx "lang" (`String "ocaml");
        let c = Collaboration.create ~id:"rt-6" ~shared_context:ctx
          ~goal:"ctx" () in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check (option string) "context val"
          (Some "ocaml")
          (match Context.get c2.shared_context "lang" with
           | Some (`String v) -> Some v
           | _ -> None));

      test_case "malformed json returns Error" `Quick (fun () ->
        let bad = `Assoc [("not_id", `Int 42)] in
        check bool "is error" true
          (Result.is_error (Collaboration.of_json bad)));

      test_case "completely invalid json" `Quick (fun () ->
        let bad = `String "nope" in
        check bool "is error" true
          (Result.is_error (Collaboration.of_json bad)));
    ];

    "edge_cases", [
      test_case "duplicate participant name — find returns first" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let mk state = Collaboration.{
          name = "dup"; role = None; state;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c (mk Planned) in
        let c = Collaboration.add_participant c (mk Joined) in
        check int "both kept" 2 (List.length c.participants);
        let found = Collaboration.find_participant c "dup" in
        check bool "finds first" true
          ((Option.get found).state = Collaboration.Planned));

      test_case "duplicate participant name — update affects all" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let mk state = Collaboration.{
          name = "dup"; role = None; state;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c (mk Planned) in
        let c = Collaboration.add_participant c (mk Joined) in
        let c = Collaboration.update_participant c "dup"
          (fun p -> Collaboration.{ p with state = Done }) in
        let all_done = List.for_all
          (fun (p : Collaboration.participant) -> p.state = Done)
          c.participants in
        check bool "both updated to Done" true all_done);

      test_case "duplicate participant name — remove deletes all" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let mk state = Collaboration.{
          name = "dup"; role = None; state;
          joined_at = None; finished_at = None; summary = None;
        } in
        let c = Collaboration.add_participant c (mk Planned) in
        let c = Collaboration.add_participant c (mk Joined) in
        let c = Collaboration.remove_participant c "dup" in
        check int "all removed" 0 (List.length c.participants));

      test_case "remove nonexistent participant" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let c = Collaboration.remove_participant c "ghost" in
        check int "still empty" 0 (List.length c.participants));

      test_case "touch updates timestamp" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let before = c.updated_at in
        Unix.sleepf 0.01;
        let c = Collaboration.touch c in
        check bool "updated" true (c.updated_at > before));

      test_case "operations update timestamp" `Quick (fun () ->
        let c = Collaboration.create ~goal:"test" () in
        let before = c.updated_at in
        Unix.sleepf 0.01;
        let c = Collaboration.set_phase c Active in
        check bool "phase update touched" true (c.updated_at > before));
    ];

  ]
