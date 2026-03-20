open Agent_sdk

let () = Alcotest.run "Memory_Episodic" [
  "store_recall", [
    Alcotest.test_case "store and recall episode" `Quick (fun () ->
      let mem = Memory.create () in
      let ep : Memory.episode = {
        id = "ep1"; timestamp = 1000.0; participants = ["alice"; "bob"];
        action = "deployed v2"; outcome = Success "all green";
        salience = 0.8; metadata = [];
      } in
      Memory.store_episode mem ep;
      match Memory.recall_episode mem "ep1" with
      | None -> Alcotest.fail "episode not found"
      | Some found ->
        Alcotest.(check string) "id" "ep1" found.id;
        Alcotest.(check string) "action" "deployed v2" found.action;
        Alcotest.(check (float 0.01)) "salience" 0.8 found.salience
    );

    Alcotest.test_case "recall nonexistent episode" `Quick (fun () ->
      let mem = Memory.create () in
      Alcotest.(check bool) "none" true (Memory.recall_episode mem "nope" = None)
    );
  ];

  "salience_decay", [
    Alcotest.test_case "recent episodes have higher effective salience" `Quick (fun () ->
      let mem = Memory.create () in
      let now = 200.0 in
      Memory.store_episode mem {
        id = "old"; timestamp = 100.0; participants = [];
        action = "old task"; outcome = Neutral; salience = 0.9; metadata = [];
      };
      Memory.store_episode mem {
        id = "new"; timestamp = 199.0; participants = [];
        action = "new task"; outcome = Neutral; salience = 0.9; metadata = [];
      };
      (* decay_rate 0.001: age 100s → 0.9*exp(-0.1)≈0.81, age 1s → 0.9*exp(-0.001)≈0.899 *)
      let episodes = Memory.recall_episodes mem ~now ~decay_rate:0.001 ~min_salience:0.01 () in
      match episodes with
      | first :: second :: _ ->
        Alcotest.(check string) "newest first" "new" first.id;
        Alcotest.(check string) "oldest second" "old" second.id;
        Alcotest.(check bool) "new has higher salience" true
          (first.salience > second.salience)
      | _ -> Alcotest.fail "expected 2 episodes"
    );

    Alcotest.test_case "min_salience filters low episodes" `Quick (fun () ->
      let mem = Memory.create () in
      let now = 10000.0 in
      Memory.store_episode mem {
        id = "ancient"; timestamp = 0.0; participants = [];
        action = "very old"; outcome = Neutral; salience = 0.5; metadata = [];
      };
      let episodes = Memory.recall_episodes mem ~now ~decay_rate:0.01
        ~min_salience:0.1 () in
      Alcotest.(check int) "filtered out" 0 (List.length episodes)
    );

    Alcotest.test_case "limit caps results" `Quick (fun () ->
      let mem = Memory.create () in
      let now = 100.0 in
      for i = 0 to 9 do
        Memory.store_episode mem {
          id = Printf.sprintf "ep%d" i; timestamp = Float.of_int (i * 10);
          participants = []; action = "task"; outcome = Neutral;
          salience = 0.9; metadata = [];
        }
      done;
      let episodes = Memory.recall_episodes mem ~now ~limit:3 () in
      Alcotest.(check int) "limited to 3" 3 (List.length episodes)
    );
  ];

  "filter", [
    Alcotest.test_case "participant filter" `Quick (fun () ->
      let mem = Memory.create () in
      let now = 1000.0 in
      Memory.store_episode mem {
        id = "alice"; timestamp = now; participants = ["alice"];
        action = "deploy"; outcome = Success "ok"; salience = 0.9; metadata = [];
      };
      Memory.store_episode mem {
        id = "bob"; timestamp = now; participants = ["bob"];
        action = "deploy"; outcome = Success "ok"; salience = 0.9; metadata = [];
      };
      let episodes =
        Memory.recall_episodes mem ~now
          ~filter:(fun ep -> List.mem "alice" ep.participants) ()
      in
      Alcotest.(check int) "one match" 1 (List.length episodes);
      match episodes with
      | [ep] -> Alcotest.(check string) "alice only" "alice" ep.id
      | _ -> Alcotest.fail "expected one alice episode"
    );

    Alcotest.test_case "outcome filter sees decayed salience" `Quick (fun () ->
      let mem = Memory.create () in
      let now = 500.0 in
      Memory.store_episode mem {
        id = "fail"; timestamp = now -. 1.0; participants = [];
        action = "deploy"; outcome = Failure "smoke failed";
        salience = 0.9; metadata = [];
      };
      Memory.store_episode mem {
        id = "success"; timestamp = now -. 1.0; participants = [];
        action = "deploy"; outcome = Success "ok";
        salience = 0.9; metadata = [];
      };
      let failures =
        Memory.recall_episodes mem ~now
          ~filter:(fun ep ->
            ep.salience > 0.5 &&
            match ep.outcome with
            | Failure _ -> true
            | Success _ | Neutral -> false) ()
      in
      match failures with
      | [ep] -> Alcotest.(check string) "failure only" "fail" ep.id
      | _ -> Alcotest.fail "expected one failed episode"
    );
  ];

  "boost", [
    Alcotest.test_case "boost increases salience" `Quick (fun () ->
      let mem = Memory.create () in
      Memory.store_episode mem {
        id = "ep1"; timestamp = 100.0; participants = [];
        action = "task"; outcome = Neutral; salience = 0.5; metadata = [];
      };
      Memory.boost_salience mem "ep1" 0.3;
      match Memory.recall_episode mem "ep1" with
      | Some ep -> Alcotest.(check (float 0.01)) "boosted" 0.8 ep.salience
      | None -> Alcotest.fail "not found"
    );

    Alcotest.test_case "boost caps at 1.0" `Quick (fun () ->
      let mem = Memory.create () in
      Memory.store_episode mem {
        id = "ep1"; timestamp = 100.0; participants = [];
        action = "task"; outcome = Neutral; salience = 0.9; metadata = [];
      };
      Memory.boost_salience mem "ep1" 0.5;
      match Memory.recall_episode mem "ep1" with
      | Some ep -> Alcotest.(check (float 0.01)) "capped" 1.0 ep.salience
      | None -> Alcotest.fail "not found"
    );
  ];

  "forget_count", [
    Alcotest.test_case "forget removes episode" `Quick (fun () ->
      let mem = Memory.create () in
      Memory.store_episode mem {
        id = "ep1"; timestamp = 100.0; participants = [];
        action = "task"; outcome = Neutral; salience = 0.5; metadata = [];
      };
      Alcotest.(check int) "count 1" 1 (Memory.episode_count mem);
      Memory.forget_episode mem "ep1";
      Alcotest.(check int) "count 0" 0 (Memory.episode_count mem)
    );
  ];

  "stats", [
    Alcotest.test_case "episodic count in stats" `Quick (fun () ->
      let mem = Memory.create () in
      Memory.store_episode mem {
        id = "ep1"; timestamp = 100.0; participants = [];
        action = "task"; outcome = Neutral; salience = 0.5; metadata = [];
      };
      let (_, _, ep, _, _) = Memory.stats mem in
      Alcotest.(check int) "episodic count" 1 ep
    );
  ];
]
