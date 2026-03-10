open Agent_sdk

(* Helper: build a minimal checkpoint for testing *)
let make_checkpoint
    ?(session_id="test-session")
    ?(agent_name="test-agent")
    ?(model=Types.Claude_sonnet_4_6)
    ?(system_prompt=Some "You are helpful.")
    ?(messages=[])
    ?(usage=Types.empty_usage)
    ?(turn_count=0)
    ?(tools=[])
    ?(tool_choice=None)
    () : Checkpoint.t =
  {
    version = Checkpoint.checkpoint_version;
    session_id;
    agent_name;
    model;
    system_prompt;
    messages;
    usage;
    turn_count;
    created_at = 1000.0;
    tools;
    tool_choice;
  }

(* Helper: a sample tool_schema *)
let sample_tool_schema : Types.tool_schema = {
  name = "get_weather";
  description = "Get weather for a city";
  parameters = [
    { name = "city"; description = "City name";
      param_type = Types.String; required = true };
    { name = "units"; description = "Temperature units";
      param_type = Types.String; required = false };
  ];
}

let () =
  let open Alcotest in
  run "Checkpoint" [
    "version", [
      test_case "checkpoint_version is 1" `Quick (fun () ->
        check int "version" 1 Checkpoint.checkpoint_version);

      test_case "version field in to_json" `Quick (fun () ->
        let cp = make_checkpoint () in
        let json = Checkpoint.to_json cp in
        let v = Yojson.Safe.Util.(json |> member "version" |> to_int) in
        check int "version" 1 v);

      test_case "wrong version returns Error" `Quick (fun () ->
        let cp = make_checkpoint () in
        let json = Checkpoint.to_json cp in
        let bad = match json with
          | `Assoc pairs ->
            `Assoc (List.map (fun (k, v) ->
              if k = "version" then (k, `Int 999) else (k, v)
            ) pairs)
          | other -> other
        in
        check bool "is error" true (Result.is_error (Checkpoint.of_json bad)));
    ];

    "roundtrip_basic", [
      test_case "empty checkpoint roundtrip" `Quick (fun () ->
        let cp = make_checkpoint () in
        let json = Checkpoint.to_json cp in
        let cp2 = Result.get_ok (Checkpoint.of_json json) in
        check string "session_id" cp.session_id cp2.session_id;
        check string "agent_name" cp.agent_name cp2.agent_name;
        check int "turn_count" cp.turn_count cp2.turn_count);

      test_case "to_string / of_string roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~session_id:"s1" ~turn_count:5 () in
        let s = Checkpoint.to_string cp in
        let cp2 = Result.get_ok (Checkpoint.of_string s) in
        check string "session_id" "s1" cp2.session_id;
        check int "turn_count" 5 cp2.turn_count);

      test_case "system_prompt None roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~system_prompt:None () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check (option string) "system_prompt" None cp2.system_prompt);

      test_case "system_prompt Some roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~system_prompt:(Some "Be concise.") () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check (option string) "system_prompt" (Some "Be concise.") cp2.system_prompt);
    ];

    "messages", [
      test_case "Text message roundtrip" `Quick (fun () ->
        let msgs = [
          { Types.role = Types.User; content = [Types.Text "Hello"] };
          { Types.role = Types.Assistant; content = [Types.Text "Hi there"] };
        ] in
        let cp = make_checkpoint ~messages:msgs () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "message count" 2 (List.length cp2.messages);
        let first = List.hd cp2.messages in
        check bool "user role" true (first.role = Types.User);
        match List.hd first.content with
        | Types.Text t -> check string "text" "Hello" t
        | _ -> fail "expected Text");

      test_case "ToolUse message roundtrip" `Quick (fun () ->
        let msgs = [
          { Types.role = Types.Assistant;
            content = [Types.ToolUse ("id1", "get_weather",
              `Assoc [("city", `String "Seoul")])] };
        ] in
        let cp = make_checkpoint ~messages:msgs () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        match (List.hd cp2.messages).content with
        | [Types.ToolUse (id, name, _)] ->
          check string "id" "id1" id;
          check string "name" "get_weather" name
        | _ -> fail "expected ToolUse");

      test_case "ToolResult message roundtrip" `Quick (fun () ->
        let msgs = [
          { Types.role = Types.User;
            content = [Types.ToolResult ("id1", "Sunny 22C", false)] };
        ] in
        let cp = make_checkpoint ~messages:msgs () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        match (List.hd cp2.messages).content with
        | [Types.ToolResult (id, content, is_err)] ->
          check string "id" "id1" id;
          check string "content" "Sunny 22C" content;
          check bool "is_error" false is_err
        | _ -> fail "expected ToolResult");

      test_case "empty messages roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~messages:[] () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "no messages" 0 (List.length cp2.messages));

      test_case "mixed content blocks" `Quick (fun () ->
        let msgs = [
          { Types.role = Types.Assistant;
            content = [
              Types.Text "Let me check.";
              Types.ToolUse ("t1", "search", `Assoc [("q", `String "test")]);
            ] };
          { Types.role = Types.User;
            content = [Types.ToolResult ("t1", "found it", false)] };
        ] in
        let cp = make_checkpoint ~messages:msgs () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "2 messages" 2 (List.length cp2.messages);
        let first = List.hd cp2.messages in
        check int "2 blocks in first" 2 (List.length first.content));
    ];

    "usage", [
      test_case "usage roundtrip" `Quick (fun () ->
        let u : Types.usage_stats = {
          total_input_tokens = 1000;
          total_output_tokens = 500;
          total_cache_creation_input_tokens = 200;
          total_cache_read_input_tokens = 100;
          api_calls = 3;
        } in
        let cp = make_checkpoint ~usage:u () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "input" 1000 cp2.usage.total_input_tokens;
        check int "output" 500 cp2.usage.total_output_tokens;
        check int "cache_create" 200 cp2.usage.total_cache_creation_input_tokens;
        check int "cache_read" 100 cp2.usage.total_cache_read_input_tokens;
        check int "api_calls" 3 cp2.usage.api_calls);

      test_case "empty usage roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~usage:Types.empty_usage () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "input" 0 cp2.usage.total_input_tokens;
        check int "api_calls" 0 cp2.usage.api_calls);
    ];

    "tools", [
      test_case "tool_schema roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~tools:[sample_tool_schema] () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "1 tool" 1 (List.length cp2.tools);
        let t = List.hd cp2.tools in
        check string "name" "get_weather" t.name;
        check string "desc" "Get weather for a city" t.description;
        check int "2 params" 2 (List.length t.parameters);
        let p1 = List.hd t.parameters in
        check string "param name" "city" p1.name;
        check bool "required" true p1.required);

      test_case "empty tools roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~tools:[] () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check int "no tools" 0 (List.length cp2.tools));

      test_case "param_type variants" `Quick (fun () ->
        let params = List.map (fun (n, pt) ->
          { Types.name = n; description = n;
            param_type = pt; required = true }
        ) [
          ("s", Types.String); ("i", Types.Integer);
          ("n", Types.Number); ("b", Types.Boolean);
          ("a", Types.Array); ("o", Types.Object);
        ] in
        let tool : Types.tool_schema = {
          name = "multi"; description = "test"; parameters = params } in
        let cp = make_checkpoint ~tools:[tool] () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        let t = List.hd cp2.tools in
        check int "6 params" 6 (List.length t.parameters));
    ];

    "tool_choice", [
      test_case "Auto roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~tool_choice:(Some Types.Auto) () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check bool "auto" true (cp2.tool_choice = Some Types.Auto));

      test_case "Any roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~tool_choice:(Some Types.Any) () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check bool "any" true (cp2.tool_choice = Some Types.Any));

      test_case "Tool name roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~tool_choice:(Some (Types.Tool "search")) () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check bool "tool" true (cp2.tool_choice = Some (Types.Tool "search")));

      test_case "None roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~tool_choice:None () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check bool "none" true (cp2.tool_choice = None));
    ];

    "model", [
      test_case "Opus model roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~model:Types.Claude_opus_4_6 () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check bool "model" true (cp2.model = Types.Claude_opus_4_6));

      test_case "Custom model roundtrip" `Quick (fun () ->
        let cp = make_checkpoint ~model:(Types.Custom "my-model-v1") () in
        let cp2 = Result.get_ok (Checkpoint.of_json (Checkpoint.to_json cp)) in
        check bool "custom" true (cp2.model = Types.Custom "my-model-v1"));
    ];

    "helpers", [
      test_case "message_count" `Quick (fun () ->
        let msgs = [
          { Types.role = Types.User; content = [Types.Text "a"] };
          { Types.role = Types.Assistant; content = [Types.Text "b"] };
          { Types.role = Types.User; content = [Types.Text "c"] };
        ] in
        let cp = make_checkpoint ~messages:msgs () in
        check int "count" 3 (Checkpoint.message_count cp));

      test_case "token_usage returns usage" `Quick (fun () ->
        let u : Types.usage_stats = {
          total_input_tokens = 42;
          total_output_tokens = 10;
          total_cache_creation_input_tokens = 0;
          total_cache_read_input_tokens = 0;
          api_calls = 1;
        } in
        let cp = make_checkpoint ~usage:u () in
        let result = Checkpoint.token_usage cp in
        check int "input" 42 result.total_input_tokens);
    ];

    "error_cases", [
      test_case "malformed JSON string" `Quick (fun () ->
        check bool "error" true
          (Result.is_error (Checkpoint.of_string "not json at all")));

      test_case "missing required field" `Quick (fun () ->
        let bad = `Assoc [("version", `Int 1)] in
        check bool "error" true (Result.is_error (Checkpoint.of_json bad)));

      test_case "wrong type for field" `Quick (fun () ->
        let cp = make_checkpoint () in
        let json = Checkpoint.to_json cp in
        let bad = match json with
          | `Assoc pairs ->
            `Assoc (List.map (fun (k, v) ->
              if k = "turn_count" then (k, `String "not_int") else (k, v)
            ) pairs)
          | other -> other
        in
        check bool "error" true (Result.is_error (Checkpoint.of_json bad)));

      test_case "empty JSON object" `Quick (fun () ->
        check bool "error" true
          (Result.is_error (Checkpoint.of_json (`Assoc []))));
    ];
  ]
