(** Collaboration example: multi-agent shared context lifecycle.

    Demonstrates:
    - Collaboration.create with a goal
    - Adding participants with roles and states
    - Phase transitions (Bootstrapping -> Active -> Completed)
    - Adding artifacts and contributions
    - JSON serialization round-trip
    - Direct record field access (Collaboration.t is not abstract)

    This example runs standalone (no LLM needed).

    Usage:
      dune exec examples/collaboration.exe *)

open Agent_sdk

(* ── Main ────────────────────────────────────────────── *)

let () =
  Printf.printf "=== Collaboration Lifecycle Demo ===\n\n";

  (* 1. Create collaboration with a goal *)
  let collab = Collaboration.create ~goal:"Review and merge PR #42" () in
  Printf.printf "1. Created collaboration (phase: %s)\n"
    (Collaboration.show_phase collab.phase);

  (* 2. Add participants *)
  let now = Unix.gettimeofday () in
  let collab = Collaboration.add_participant collab
      { name = "reader"; role = Some "gather"; state = Working;
        joined_at = Some now; finished_at = None; summary = None } in
  let collab = Collaboration.add_participant collab
      { name = "reviewer"; role = Some "analyze"; state = Joined;
        joined_at = Some now; finished_at = None; summary = None } in
  let collab = Collaboration.add_participant collab
      { name = "supervisor"; role = Some "synthesize"; state = Joined;
        joined_at = Some now; finished_at = None; summary = None } in
  Printf.printf "2. Added %d participants\n"
    (List.length collab.participants);

  (* 3. Transition to Active *)
  let collab = Collaboration.set_phase collab Active in
  Printf.printf "3. Phase -> %s\n"
    (Collaboration.show_phase collab.phase);

  (* 4. Add an artifact *)
  let collab = Collaboration.add_artifact collab
      { id = "art-1"; name = "PR diff summary"; kind = "review";
        producer = "reader"; created_at = now } in
  Printf.printf "4. Added artifact (total: %d)\n"
    (List.length collab.artifacts);

  (* 5. Update participant state *)
  let collab = Collaboration.update_participant collab "reader"
      (fun p -> { p with state = Done;
                         finished_at = Some (Unix.gettimeofday ());
                         summary = Some "Gathered diff and file list" }) in
  let collab = Collaboration.update_participant collab "reviewer"
      (fun p -> { p with state = Working }) in
  Printf.printf "5. Reader done, reviewer working\n";

  (* 6. Add contribution *)
  let collab = Collaboration.add_contribution collab
      { agent = "reviewer"; kind = "review";
        content = "Found 1 bug: unchecked None";
        created_at = Unix.gettimeofday () } in
  Printf.printf "6. Added contribution from reviewer\n";

  (* 7. Set outcome and complete *)
  let collab = Collaboration.set_outcome collab
      "PR approved with 1 minor fix required" in
  let collab = Collaboration.set_phase collab Completed in
  Printf.printf "7. Phase -> %s, outcome set\n"
    (Collaboration.show_phase collab.phase);

  (* 8. JSON round-trip *)
  let json = Collaboration.to_json collab in
  let collab2 = Collaboration.of_json json in
  (match collab2 with
   | Ok c ->
     Printf.printf "8. JSON round-trip OK (goal: %s)\n" c.goal
   | Error e ->
     Printf.printf "8. JSON round-trip FAILED: %s\n" (Error.to_string e));

  (* 9. Summary *)
  Printf.printf "\n--- Final State ---\n";
  Printf.printf "  Goal: %s\n" collab.goal;
  Printf.printf "  Phase: %s\n" (Collaboration.show_phase collab.phase);
  Printf.printf "  Participants: %d\n" (List.length collab.participants);
  Printf.printf "  Artifacts: %d\n" (List.length collab.artifacts);
  Printf.printf "  Contributions: %d\n" (List.length collab.contributions);
  Printf.printf "  Outcome: %s\n"
    (match collab.outcome with Some o -> o | None -> "none");

  Printf.printf "\nDone.\n"
