(** Deep coverage tests for Collaboration module.
    Targets uncovered branches in of_json (legacy votes, error paths),
    active_participants edge states, and full roundtrip with all fields. *)

open Agent_sdk

let mk_participant ?(role = None) ?(joined_at = None) ?(finished_at = None)
    ?(summary = None) name state : Collaboration.participant =
  { name; role; state; joined_at; finished_at; summary }

let mk_artifact id name kind producer created_at : Collaboration.artifact =
  { id; name; kind; producer; created_at }

let mk_contribution agent kind content created_at : Collaboration.contribution =
  { agent; kind; content; created_at }

let () =
  let open Alcotest in
  run "Collaboration_deep" [

    (* ── create defaults ─────────────────────────────── *)
    "create_defaults", [
      test_case "empty lists on create" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        check int "artifacts" 0 (List.length c.artifacts);
        check int "contributions" 0 (List.length c.contributions);
        check (option int) "max_participants" None c.max_participants;
        check int "metadata" 0 (List.length c.metadata));

      test_case "created_at equals updated_at" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        check (float 0.001) "same timestamps" c.created_at c.updated_at);

      test_case "shared_context default is empty" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        check int "empty context" 0 (List.length (Context.keys c.shared_context)));
    ];

    (* ── touch ───────────────────────────────────────── *)
    "touch", [
      test_case "touch preserves id and goal" `Quick (fun () ->
        let c = Collaboration.create ~id:"t1" ~goal:"g" () in
        Unix.sleepf 0.01;
        let c2 = Collaboration.touch c in
        check string "id preserved" "t1" c2.id;
        check string "goal preserved" "g" c2.goal;
        check bool "updated_at changed" true (c2.updated_at > c.updated_at));

      test_case "touch preserves created_at" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        Unix.sleepf 0.01;
        let c2 = Collaboration.touch c in
        check (float 0.001) "created_at same" c.created_at c2.created_at);
    ];

    (* ── active_participants with all states ──────────── *)
    "active_participants_states", [
      test_case "Left and Failed_participant not active" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.add_participant c
          (mk_participant "a" Left) in
        let c = Collaboration.add_participant c
          (mk_participant "b" Failed_participant) in
        check int "none active" 0
          (List.length (Collaboration.active_participants c)));

      test_case "mix of all 6 states" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let states = [
          ("p", Collaboration.Planned);
          ("j", Joined);
          ("w", Working);
          ("d", Done);
          ("f", Failed_participant);
          ("l", Left);
        ] in
        let c = List.fold_left (fun c (n, s) ->
          Collaboration.add_participant c (mk_participant n s)
        ) c states in
        let active = Collaboration.active_participants c in
        check int "2 active (Joined + Working)" 2 (List.length active);
        let names = List.map (fun (p : Collaboration.participant) -> p.name) active in
        check bool "j active" true (List.mem "j" names);
        check bool "w active" true (List.mem "w" names));
    ];

    (* ── is_terminal exhaustive ──────────────────────── *)
    "is_terminal_all_phases", [
      test_case "Bootstrapping not terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "Active not terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.set_phase c Active in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "Waiting_on_participants not terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.set_phase c Waiting_on_participants in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "Finalizing not terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.set_phase c Finalizing in
        check bool "not terminal" false (Collaboration.is_terminal c));

      test_case "Completed is terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.set_phase c Completed in
        check bool "terminal" true (Collaboration.is_terminal c));

      test_case "Failed is terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.set_phase c Failed in
        check bool "terminal" true (Collaboration.is_terminal c));

      test_case "Cancelled is terminal" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let c = Collaboration.set_phase c Cancelled in
        check bool "terminal" true (Collaboration.is_terminal c));
    ];

    (* ── full roundtrip to_json/of_json ──────────────── *)
    "roundtrip_full", [
      test_case "fully populated collaboration roundtrip" `Quick (fun () ->
        let ctx = Context.create () in
        Context.set ctx "env" (`String "test");
        let c = Collaboration.create ~id:"full-1" ~shared_context:ctx ~goal:"deploy" () in
        let c = Collaboration.add_participant c
          (mk_participant ~role:(Some "lead") ~joined_at:(Some 100.0)
             ~finished_at:(Some 200.0) ~summary:(Some "done reviewing") "alice" Working) in
        let c = Collaboration.add_participant c
          (mk_participant "bob" Joined) in
        let c = Collaboration.add_artifact c
          (mk_artifact "a1" "binary" "executable" "alice" 150.0) in
        let c = Collaboration.add_contribution c
          (mk_contribution "bob" "review" "lgtm" 160.0) in
        let c = Collaboration.set_phase c Finalizing in
        let c = Collaboration.set_outcome c "shipped" in
        let c = { c with max_participants = Some 10;
                         metadata = [("priority", `Int 1); ("tag", `String "deploy")] } in
        let json = Collaboration.to_json c in
        let c2 = Result.get_ok (Collaboration.of_json json) in
        check string "id" c.id c2.id;
        check string "goal" c.goal c2.goal;
        check bool "phase" true (c2.phase = Collaboration.Finalizing);
        check int "participants" 2 (List.length c2.participants);
        check int "artifacts" 1 (List.length c2.artifacts);
        check int "contributions" 1 (List.length c2.contributions);
        check (option string) "outcome" (Some "shipped") c2.outcome;
        check (option int) "max_participants" (Some 10) c2.max_participants;
        check int "metadata" 2 (List.length c2.metadata);
        (* Verify participant details survived roundtrip *)
        let alice = List.find (fun (p : Collaboration.participant) -> p.name = "alice") c2.participants in
        check (option string) "alice role" (Some "lead") alice.role;
        check (option (float 0.1)) "alice joined_at" (Some 100.0) alice.joined_at;
        check (option (float 0.1)) "alice finished_at" (Some 200.0) alice.finished_at;
        check (option string) "alice summary" (Some "done reviewing") alice.summary;
        (* Verify shared_context survived *)
        check bool "context key"
          (Context.get c2.shared_context "env" = Some (`String "test"))
          true);
    ];

    (* ── of_json legacy votes backward compat ────────── *)
    "of_json_legacy_votes", [
      test_case "votes key parsed as contributions" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "legacy-1");
          ("goal", `String "vote test");
          ("phase", Collaboration.phase_to_yojson Active);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `Null);
          ("votes", `List [
            `Assoc [
              ("topic", `String "merge");
              ("choice", `String "yes");
              ("voter", `String "alice");
              ("cast_at", `Float 99.0);
            ];
            `Assoc [
              ("topic", `String "release");
              ("choice", `String "no");
              ("voter", `String "bob");
              ("cast_at", `Float 100.0);
            ];
          ]);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Null);
        ] in
        let c = Result.get_ok (Collaboration.of_json json) in
        check int "2 contributions from votes" 2 (List.length c.contributions);
        let first = List.hd c.contributions in
        check string "agent from voter" "alice" first.agent;
        check string "kind is vote" "vote" first.kind;
        check string "content is topic: choice" "merge: yes" first.content;
        check (float 0.1) "cast_at preserved" 99.0 first.created_at);

      test_case "votes with missing fields fallback" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "legacy-2");
          ("goal", `String "vote fallback");
          ("phase", Collaboration.phase_to_yojson Active);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `Null);
          ("votes", `List [
            `Assoc [
              (* missing topic, choice, voter, cast_at *)
              ("other", `String "junk");
            ];
          ]);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Null);
        ] in
        let c = Result.get_ok (Collaboration.of_json json) in
        check int "1 contribution" 1 (List.length c.contributions);
        let v = List.hd c.contributions in
        check string "agent fallback" "anonymous" v.agent;
        check string "content fallback" ": " v.content;
        check (float 0.1) "cast_at fallback" 0.0 v.created_at);

      test_case "contributions Null and votes Null yields empty" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "legacy-3");
          ("goal", `String "no votes");
          ("phase", Collaboration.phase_to_yojson Bootstrapping);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `Null);
          ("votes", `Null);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Null);
        ] in
        let c = Result.get_ok (Collaboration.of_json json) in
        check int "empty contributions" 0 (List.length c.contributions));

      test_case "contributions Null and votes non-list yields empty" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "legacy-4");
          ("goal", `String "bad votes");
          ("phase", Collaboration.phase_to_yojson Bootstrapping);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `Null);
          ("votes", `String "not a list");
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Null);
        ] in
        let c = Result.get_ok (Collaboration.of_json json) in
        check int "empty contributions" 0 (List.length c.contributions));
    ];

    (* ── of_json error paths ─────────────────────────── *)
    "of_json_errors", [
      test_case "Null input returns Error" `Quick (fun () ->
        let result = Collaboration.of_json `Null in
        check bool "is error" true (Result.is_error result));

      test_case "String input returns Error" `Quick (fun () ->
        let result = Collaboration.of_json (`String "bad") in
        check bool "is error" true (Result.is_error result));

      test_case "Int input returns Error" `Quick (fun () ->
        let result = Collaboration.of_json (`Int 42) in
        check bool "is error" true (Result.is_error result));

      test_case "missing id field returns Error" `Quick (fun () ->
        let json = `Assoc [("goal", `String "test")] in
        let result = Collaboration.of_json json in
        check bool "is error" true (Result.is_error result));

      test_case "wrong type for id returns Error" `Quick (fun () ->
        let json = `Assoc [
          ("id", `Int 1);
          ("goal", `String "test");
          ("phase", Collaboration.phase_to_yojson Active);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `List []);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Null);
        ] in
        let result = Collaboration.of_json json in
        check bool "is error" true (Result.is_error result));

      test_case "invalid phase string returns Error" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "e1");
          ("goal", `String "test");
          ("phase", `String "InvalidPhase");
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `List []);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Null);
        ] in
        let result = Collaboration.of_json json in
        check bool "is error" true (Result.is_error result));
    ];

    (* ── of_json metadata branches ───────────────────── *)
    "of_json_metadata", [
      test_case "metadata as Assoc" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "m1");
          ("goal", `String "meta test");
          ("phase", Collaboration.phase_to_yojson Bootstrapping);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `List []);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `Assoc [("k", `Bool true)]);
        ] in
        let c = Result.get_ok (Collaboration.of_json json) in
        check int "metadata len" 1 (List.length c.metadata));

      test_case "metadata as non-Assoc non-Null falls back to empty" `Quick (fun () ->
        let json = `Assoc [
          ("id", `String "m2");
          ("goal", `String "meta test2");
          ("phase", Collaboration.phase_to_yojson Bootstrapping);
          ("participants", `List []);
          ("artifacts", `List []);
          ("contributions", `List []);
          ("shared_context", `Null);
          ("created_at", `Float 1.0);
          ("updated_at", `Float 2.0);
          ("outcome", `Null);
          ("max_participants", `Null);
          ("metadata", `List [`Int 1]);
        ] in
        let c = Result.get_ok (Collaboration.of_json json) in
        check int "metadata empty" 0 (List.length c.metadata));
    ];

    (* ── to_json field presence ──────────────────────── *)
    "to_json_fields", [
      test_case "all fields present in JSON" `Quick (fun () ->
        let c = Collaboration.create ~id:"f1" ~goal:"field test" () in
        let json = Collaboration.to_json c in
        let open Yojson.Safe.Util in
        let _ = json |> member "id" |> to_string in
        let _ = json |> member "goal" |> to_string in
        let _ = json |> member "phase" in
        let _ = json |> member "participants" in
        let _ = json |> member "artifacts" in
        let _ = json |> member "contributions" in
        let _ = json |> member "shared_context" in
        let _ = json |> member "created_at" |> to_float in
        let _ = json |> member "updated_at" |> to_float in
        let _ = json |> member "outcome" in
        let _ = json |> member "max_participants" in
        let _ = json |> member "metadata" in
        ());

      test_case "outcome None serialized as Null" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let json = Collaboration.to_json c in
        let open Yojson.Safe.Util in
        check bool "outcome is null" true
          (json |> member "outcome" = `Null));

      test_case "max_participants None serialized as Null" `Quick (fun () ->
        let c = Collaboration.create ~goal:"g" () in
        let json = Collaboration.to_json c in
        let open Yojson.Safe.Util in
        check bool "max_participants is null" true
          (json |> member "max_participants" = `Null));
    ];
  ]
