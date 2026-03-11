open Agent_sdk

let sample_tool_schema : Types.tool_schema = {
  name = "get_weather";
  description = "Get weather for a city";
  parameters = [
    { name = "city"; description = "City name";
      param_type = Types.String; required = true };
  ];
}

let multi_param_tool : Types.tool_schema = {
  name = "search";
  description = "Search documents";
  parameters = [
    { name = "query"; description = "Search query";
      param_type = Types.String; required = true };
    { name = "limit"; description = "Max results";
      param_type = Types.Integer; required = false };
    { name = "filters"; description = "Filter object";
      param_type = Types.Object; required = false };
  ];
}

let make_info
    ?(server_name="test-server")
    ?(command="/usr/bin/mcp-server")
    ?(args=[])
    ?(env=[])
    ?(tool_schemas=[])
    () : Mcp_session.info =
  { server_name; command; args; env; tool_schemas }

let () =
  let open Alcotest in
  run "Mcp_session" [
    "json_roundtrip", [
      test_case "basic info roundtrip" `Quick (fun () ->
        let info = make_info ~server_name:"basic" ~command:"echo" () in
        let json = Mcp_session.info_to_json info in
        let info2 = Result.get_ok (Mcp_session.info_of_json json) in
        check string "server_name" "basic" info2.server_name;
        check string "command" "echo" info2.command;
        check int "args" 0 (List.length info2.args);
        check int "env" 0 (List.length info2.env);
        check int "tools" 0 (List.length info2.tool_schemas));

      test_case "empty list roundtrip" `Quick (fun () ->
        let json = Mcp_session.info_list_to_json [] in
        let infos = Result.get_ok (Mcp_session.info_list_of_json json) in
        check int "empty" 0 (List.length infos));

      test_case "info with tools roundtrip" `Quick (fun () ->
        let info = make_info
          ~tool_schemas:[sample_tool_schema; multi_param_tool] () in
        let json = Mcp_session.info_to_json info in
        let info2 = Result.get_ok (Mcp_session.info_of_json json) in
        check int "2 tools" 2 (List.length info2.tool_schemas);
        let t1 = List.hd info2.tool_schemas in
        check string "tool1 name" "get_weather" t1.name;
        check int "tool1 params" 1 (List.length t1.parameters);
        let t2 = List.nth info2.tool_schemas 1 in
        check string "tool2 name" "search" t2.name;
        check int "tool2 params" 3 (List.length t2.parameters);
        let p = List.nth t2.parameters 1 in
        check string "param name" "limit" p.name;
        check bool "param required" false p.required);

      test_case "info with env roundtrip" `Quick (fun () ->
        let info = make_info
          ~env:[("API_KEY", "sk-123"); ("DEBUG", "true"); ("EMPTY", "")] () in
        let json = Mcp_session.info_to_json info in
        let info2 = Result.get_ok (Mcp_session.info_of_json json) in
        check int "3 env pairs" 3 (List.length info2.env);
        let (k1, v1) = List.hd info2.env in
        check string "key1" "API_KEY" k1;
        check string "val1" "sk-123" v1;
        let (k3, v3) = List.nth info2.env 2 in
        check string "key3" "EMPTY" k3;
        check string "val3 empty" "" v3);

      test_case "multiple infos list roundtrip" `Quick (fun () ->
        let infos = [
          make_info ~server_name:"alpha" ~command:"cmd-a"
            ~args:["--port"; "3000"] ();
          make_info ~server_name:"beta" ~command:"cmd-b"
            ~env:[("X", "1")] ~tool_schemas:[sample_tool_schema] ();
          make_info ~server_name:"gamma" ~command:"cmd-c" ();
        ] in
        let json = Mcp_session.info_list_to_json infos in
        let infos2 = Result.get_ok (Mcp_session.info_list_of_json json) in
        check int "3 infos" 3 (List.length infos2);
        check string "first" "alpha" (List.hd infos2).server_name;
        check string "second" "beta" (List.nth infos2 1).server_name;
        check string "third" "gamma" (List.nth infos2 2).server_name;
        check int "beta tools" 1 (List.length (List.nth infos2 1).tool_schemas));
    ];

    "to_server_spec", [
      test_case "converts info to server_spec" `Quick (fun () ->
        let info = make_info
          ~server_name:"my-server"
          ~command:"/usr/local/bin/mcp"
          ~args:["--mode"; "production"]
          ~env:[("TOKEN", "abc")] () in
        let spec = Mcp_session.to_server_spec info in
        check string "name" "my-server" spec.name;
        check string "command" "/usr/local/bin/mcp" spec.command;
        check int "args" 2 (List.length spec.args);
        check string "arg1" "--mode" (List.hd spec.args);
        check string "arg2" "production" (List.nth spec.args 1);
        check int "env" 1 (List.length spec.env);
        let (k, v) = List.hd spec.env in
        check string "env key" "TOKEN" k;
        check string "env val" "abc" v);
    ];

    "json_fields", [
      test_case "info_to_json has expected keys" `Quick (fun () ->
        let info = make_info ~server_name:"s" ~command:"c"
          ~args:["a"] ~env:[("k","v")] ~tool_schemas:[sample_tool_schema] () in
        let json = Mcp_session.info_to_json info in
        let open Yojson.Safe.Util in
        check string "server_name" "s" (json |> member "server_name" |> to_string);
        check string "command" "c" (json |> member "command" |> to_string);
        check int "args len" 1 (json |> member "args" |> to_list |> List.length);
        check int "env len" 1 (json |> member "env" |> to_list |> List.length);
        check int "schemas len" 1 (json |> member "tool_schemas" |> to_list |> List.length));
    ];

    "error_cases", [
      test_case "missing required field" `Quick (fun () ->
        let bad = `Assoc [
          ("server_name", `String "s");
          (* missing command, args, env, tool_schemas *)
        ] in
        check bool "error" true (Result.is_error (Mcp_session.info_of_json bad)));

      test_case "bad tool_schema in info" `Quick (fun () ->
        let bad = `Assoc [
          ("server_name", `String "s");
          ("command", `String "c");
          ("args", `List []);
          ("env", `List []);
          ("tool_schemas", `List [
            `Assoc [
              ("name", `String "t");
              ("description", `String "d");
              ("parameters", `List [
                `Assoc [
                  ("name", `String "p");
                  ("description", `String "d");
                  ("param_type", `String "invalid_type");
                  ("required", `Bool true);
                ]
              ]);
            ]
          ]);
        ] in
        check bool "error" true (Result.is_error (Mcp_session.info_of_json bad)));
    ];
  ]
