open Agent_sdk

let find_tool name tools =
  List.find (fun (tool : Tool.t) -> tool.schema.name = name) tools

let run_tool label tool input =
  match Tool.execute tool input with
  | Ok { content } -> Printf.printf "[%s] %s\n" label content
  | Error { message; _ } -> Printf.printf "[%s] ERROR %s\n" label message

let () =
  let mem = Memory.create () in
  let plain_tools = Memory_tools.all mem in
  let remember = find_tool "memory_remember" plain_tools in
  let recall = find_tool "memory_recall" plain_tools in
  let remember_episode = find_tool "memory_remember_episode" plain_tools in
  let find_procedure = find_tool "memory_find_procedure" plain_tools in

  Memory.store_procedure mem
    {
      id = "pr_deploy";
      pattern = "deploy failed";
      action = "rollback, inspect logs, retry";
      success_count = 6;
      failure_count = 1;
      confidence = 6.0 /. 7.0;
      last_used = Unix.gettimeofday () -. 60.0;
      metadata = [ ("team", `String "release") ];
    };

  Printf.printf "=== Plain memory tools ===\n";
  run_tool "remember" remember
    (`Assoc
      [
        ("key", `String "shared_goal");
        ("value_json", `String {|{"status":"draft","attempt":1}|});
      ]);
  run_tool "recall" recall
    (`Assoc [ ("key", `String "shared_goal") ]);
  run_tool "remember_episode" remember_episode
    (`Assoc
      [
        ("action", `String "deploy v0.76.0");
        ("participants", `List [ `String "alice"; `String "reviewer" ]);
        ("outcome", `String "failure");
        ("detail", `String "smoke test failed");
      ]);
  run_tool "find_procedure" find_procedure
    (`Assoc
      [
        ("pattern", `String "deploy");
        ("min_confidence", `Float 0.7);
        ("touch", `Bool true);
      ]);

  let acl = Memory_access.create mem in
  Memory_access.grant acl
    {
      agent_name = "alice";
      tier = Working;
      key_pattern = "*";
      permission = ReadWrite;
    };
  Memory_access.grant acl
    {
      agent_name = "bob";
      tier = Working;
      key_pattern = "*";
      permission = Read;
    };
  let alice_tools = Memory_tools.all_acl acl ~agent_name:"alice" in
  let bob_tools = Memory_tools.all_acl acl ~agent_name:"bob" in
  let alice_remember = find_tool "memory_remember" alice_tools in
  let bob_remember = find_tool "memory_remember" bob_tools in
  let bob_recall = find_tool "memory_recall" bob_tools in

  Printf.printf "\n=== ACL-aware memory tools ===\n";
  run_tool "alice remember" alice_remember
    (`Assoc
      [
        ("key", `String "shared_note");
        ("value_json", `String {|{"owner":"alice"}|});
      ]);
  run_tool "bob remember" bob_remember
    (`Assoc
      [
        ("key", `String "shared_note");
        ("value_json", `String {|{"owner":"bob"}|});
      ]);
  run_tool "bob recall" bob_recall
    (`Assoc [ ("key", `String "shared_note") ])
