(** Tests for Mcp_typed_client using in-memory transport.

    Verifies the MCP protocol SDK v0.15.0 integration including:
    - Initialize handshake + typed capabilities
    - Tool listing and calling via Generic_client functor
    - Resource listing and reading
    - Resource subscribe/unsubscribe
    - Resource template listing
    - Prompt listing and fetching
    - Ping
    - Tool_arg extraction (from SDK re-export)
    - New Mcp module operations (subscribe, unsubscribe, templates) *)

open Mcp_protocol
module Mt = Mcp_protocol_eio.Memory_transport
module Server = Mcp_protocol_eio.Generic_server.Make(Mt)
module Client = Mcp_protocol_eio.Generic_client.Make(Mt)

let () = Eio_main.run @@ fun env ->
let clock = Eio.Stdenv.clock env in

(* ── helper: create connected client+server pair ── *)
let make_server_and_client ~sw ~server =
  let client_t, server_t = Mt.create_pair () in
  Eio.Fiber.fork ~sw (fun () ->
    Server.run server ~transport:server_t ~clock ());
  let client = Client.create ~transport:client_t ~clock () in
  (client, client_t)
in

(* ── test: typed capabilities from initialize ── *)
let test_typed_capabilities () =
  Eio.Switch.run @@ fun sw ->
  let echo_tool = Mcp_types.make_tool
    ~name:"echo" ~description:"Echo input"
    ~input_schema:(`Assoc [("type", `String "object");
      ("properties", `Assoc [
        ("text", `Assoc [("type", `String "string")])])])
    () in
  let server =
    Server.create ~name:"test-server" ~version:"1.0.0" ()
    |> Server.add_tool echo_tool (fun _ctx _name args ->
      let open Tool_arg in
      let text = optional args "text" string ~default:"default" in
      Ok (Mcp_types.tool_result_of_text ("echo: " ^ text)))
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok result ->
     (* v0.14.0+: typed capabilities. tools should be Some, not raw JSON. *)
     Alcotest.(check bool) "has tools capability" true
       (Option.is_some result.capabilities.tools);
     (* Server info *)
     Alcotest.(check string) "server name" "test-server" result.server_info.name;
     Alcotest.(check string) "server version" "1.0.0" result.server_info.version
   | Error e -> Alcotest.fail ("init failed: " ^ e));
  Mt.close client_t
in

(* ── test: tool listing and calling with Tool_arg ── *)
let test_tool_list_and_call () =
  Eio.Switch.run @@ fun sw ->
  let add_tool = Mcp_types.make_tool
    ~name:"add" ~description:"Add two numbers"
    ~input_schema:(`Assoc [("type", `String "object");
      ("properties", `Assoc [
        ("a", `Assoc [("type", `String "integer")]);
        ("b", `Assoc [("type", `String "integer")])])])
    () in
  let server =
    Server.create ~name:"calc-server" ~version:"1.0.0" ()
    |> Server.add_tool add_tool (fun _ctx _name args ->
      let open Tool_arg in
      let ( let* ) = Result.bind in
      let* a = required args "a" int in
      let* b = required args "b" int in
      Ok (Mcp_types.tool_result_of_text (string_of_int (a + b))))
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* List tools *)
  (match Client.list_tools client with
   | Ok tools ->
     Alcotest.(check int) "one tool" 1 (List.length tools);
     let t = List.hd tools in
     Alcotest.(check string) "tool name" "add" t.name;
     Alcotest.(check (option string)) "tool description"
       (Some "Add two numbers") t.description;
     (* v0.14.0+: output_schema and execution fields exist *)
     Alcotest.(check bool) "output_schema is None" true
       (Option.is_none t.output_schema);
     Alcotest.(check bool) "execution is None" true
       (Option.is_none t.execution)
   | Error e -> Alcotest.fail ("list_tools failed: " ^ e));

  (* Call tool *)
  (match Client.call_tool client ~name:"add"
           ~arguments:(`Assoc [("a", `Int 3); ("b", `Int 7)]) () with
   | Ok result ->
     (match result.content with
      | [Mcp_types.TextContent { text; _ }] ->
        Alcotest.(check string) "3 + 7 = 10" "10" text
      | _ -> Alcotest.fail "expected single TextContent")
   | Error e -> Alcotest.fail ("call add failed: " ^ e));

  (* Call with missing required arg *)
  (match Client.call_tool client ~name:"add"
           ~arguments:(`Assoc [("a", `Int 1)]) () with
   | Ok result ->
     (* Tool handler returns error for missing arg *)
     (match result.is_error with
      | Some true -> ()
      | _ -> Alcotest.fail "expected tool error for missing arg")
   | Error _e -> ());

  Mt.close client_t
in

(* ── test: resource listing and reading ── *)
let test_resources () =
  Eio.Switch.run @@ fun sw ->
  let res = Mcp_types.make_resource
    ~uri:"file:///test.txt" ~name:"test"
    ~description:"A test resource" ~mime_type:"text/plain" () in
  let server =
    Server.create ~name:"resource-server" ~version:"1.0.0" ()
    |> Server.add_resource res (fun _ctx _uri ->
      Ok [{ Mcp_types.uri = "file:///test.txt"; mime_type = Some "text/plain";
            text = Some "Hello from resource"; blob = None }])
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok result ->
     (* resources capability should be present *)
     Alcotest.(check bool) "has resources capability" true
       (Option.is_some result.capabilities.resources)
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* List resources *)
  (match Client.list_resources client with
   | Ok resources ->
     Alcotest.(check int) "one resource" 1 (List.length resources);
     let r = List.hd resources in
     Alcotest.(check string) "resource uri" "file:///test.txt" r.uri;
     Alcotest.(check string) "resource name" "test" r.name
   | Error e -> Alcotest.fail ("list_resources failed: " ^ e));

  (* Read resource *)
  (match Client.read_resource client ~uri:"file:///test.txt" with
   | Ok contents ->
     Alcotest.(check int) "one content" 1 (List.length contents);
     let c = List.hd contents in
     Alcotest.(check (option string)) "text content"
       (Some "Hello from resource") c.text
   | Error e -> Alcotest.fail ("read_resource failed: " ^ e));

  Mt.close client_t
in

(* ── test: resource templates (v0.15.0) ── *)
let test_resource_templates () =
  Eio.Switch.run @@ fun sw ->
  let tmpl : Mcp_types.resource_template = {
    uri_template = "file:///{path}";
    name = "files";
    description = Some "File access template";
    mime_type = Some "text/plain";
    icon = None;
  } in
  let server =
    Server.create ~name:"template-server" ~version:"1.0.0" ()
    |> Server.add_resource_template tmpl (fun _ctx uri ->
      Ok [{ Mcp_types.uri; mime_type = Some "text/plain";
            text = Some ("content of " ^ uri); blob = None }])
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* v0.15.0: list resource templates *)
  (match Client.list_resource_templates client with
   | Ok templates ->
     Alcotest.(check int) "one template" 1 (List.length templates);
     let t = List.hd templates in
     Alcotest.(check string) "template uri" "file:///{path}" t.uri_template;
     Alcotest.(check string) "template name" "files" t.name
   | Error e -> Alcotest.fail ("list_resource_templates failed: " ^ e));

  (* Read via template match *)
  (match Client.read_resource client ~uri:"file:///hello.txt" with
   | Ok contents ->
     Alcotest.(check int) "one content" 1 (List.length contents);
     let c = List.hd contents in
     Alcotest.(check (option string)) "template content"
       (Some "content of file:///hello.txt") c.text
   | Error e -> Alcotest.fail ("read template resource failed: " ^ e));

  Mt.close client_t
in

(* ── test: resource subscribe/unsubscribe (v0.15.0) ── *)
let test_resource_subscribe () =
  Eio.Switch.run @@ fun sw ->
  let res = Mcp_types.make_resource
    ~uri:"file:///watched.txt" ~name:"watched" () in
  let server =
    Server.create ~name:"sub-server" ~version:"1.0.0" ()
    |> Server.add_resource res (fun _ctx _uri ->
      Ok [{ Mcp_types.uri = "file:///watched.txt"; mime_type = None;
            text = Some "data"; blob = None }])
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* Subscribe *)
  (match Client.subscribe_resource client ~uri:"file:///watched.txt" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("subscribe failed: " ^ e));

  (* Unsubscribe *)
  (match Client.unsubscribe_resource client ~uri:"file:///watched.txt" with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("unsubscribe failed: " ^ e));

  Mt.close client_t
in

(* ── test: prompts ── *)
let test_prompts () =
  Eio.Switch.run @@ fun sw ->
  let greet_prompt = Mcp_types.make_prompt
    ~name:"greet" ~description:"Greeting prompt"
    ~arguments:[{ name = "name"; description = Some "Who"; required = Some true }]
    () in
  let server =
    Server.create ~name:"prompt-server" ~version:"1.0.0" ()
    |> Server.add_prompt greet_prompt (fun _ctx _name args ->
      let name = match List.assoc_opt "name" args with
        | Some n -> n | None -> "world"
      in
      Ok { Mcp_types.description = Some "A greeting";
           messages = [{ role = User;
             content = PromptText { type_ = "text";
               text = "Hello, " ^ name } }] })
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* List prompts *)
  (match Client.list_prompts client with
   | Ok prompts ->
     Alcotest.(check int) "one prompt" 1 (List.length prompts);
     Alcotest.(check string) "prompt name" "greet" (List.hd prompts).name
   | Error e -> Alcotest.fail ("list_prompts failed: " ^ e));

  (* Get prompt *)
  (match Client.get_prompt client ~name:"greet"
           ~arguments:[("name", "Alice")] () with
   | Ok result ->
     Alcotest.(check (option string)) "description" (Some "A greeting")
       result.description;
     (match result.messages with
      | [msg] ->
        (match msg.content with
         | PromptText { text; _ } ->
           Alcotest.(check string) "greeting text" "Hello, Alice" text
         | _ -> Alcotest.fail "expected PromptText")
      | _ -> Alcotest.fail "expected one message")
   | Error e -> Alcotest.fail ("get_prompt failed: " ^ e));

  Mt.close client_t
in

(* ── test: ping ── *)
let test_ping () =
  Eio.Switch.run @@ fun sw ->
  let server = Server.create ~name:"ping-server" ~version:"1.0.0" () in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (match Client.ping client with
   | Ok () -> ()
   | Error e -> Alcotest.fail ("ping failed: " ^ e));

  Mt.close client_t
in

(* ── test: Tool_arg extraction ── *)
let test_tool_arg () =
  let open Tool_arg in
  let args = Some (`Assoc [("name", `String "test"); ("count", `Int 42)]) in
  (match required args "name" string with
   | Ok "test" -> ()
   | Ok s -> Alcotest.fail ("wrong name: " ^ s)
   | Error e -> Alcotest.fail ("required name failed: " ^ e));
  (match required args "count" int with
   | Ok 42 -> ()
   | Ok n -> Alcotest.fail ("wrong count: " ^ string_of_int n)
   | Error e -> Alcotest.fail ("required count failed: " ^ e));
  let missing = optional args "missing" string ~default:"fallback" in
  Alcotest.(check string) "optional fallback" "fallback" missing;
  let opt = optional_opt args "name" string in
  Alcotest.(check (option string)) "optional_opt present" (Some "test") opt;
  let opt_missing = optional_opt args "nope" string in
  Alcotest.(check (option string)) "optional_opt absent" None opt_missing;
  (* list_of extractor *)
  let list_args = Some (`Assoc [("items", `List [`String "a"; `String "b"])]) in
  (match required list_args "items" (list_of string) with
   | Ok ["a"; "b"] -> ()
   | Ok _ -> Alcotest.fail "wrong list result"
   | Error e -> Alcotest.fail ("list_of failed: " ^ e));
  (* bool extractor *)
  let bool_args = Some (`Assoc [("flag", `Bool true)]) in
  (match required bool_args "flag" bool with
   | Ok true -> ()
   | Ok _ -> Alcotest.fail "wrong bool"
   | Error e -> Alcotest.fail ("bool failed: " ^ e));
  (* float extractor with int coercion *)
  let float_args = Some (`Assoc [("val", `Int 5)]) in
  (match required float_args "val" float with
   | Ok 5.0 -> ()
   | Ok _ -> Alcotest.fail "wrong float"
   | Error e -> Alcotest.fail ("float from int failed: " ^ e))
in

(* ── test: OAS Mcp module new operations ── *)
let test_mcp_schema_bridge () =
  (* Test mcp_tool_of_sdk_tool with new v0.14.0+ fields *)
  let sdk_tool : Mcp_types.tool = {
    name = "test_tool";
    description = Some "A test tool";
    input_schema = `Assoc [("type", `String "object")];
    title = None; annotations = None; icon = None;
    output_schema = Some (`Assoc [("type", `String "string")]);
    execution = Some { task_support = Task_optional };
  } in
  let open Agent_sdk in
  let mcp_tool = Mcp.mcp_tool_of_sdk_tool sdk_tool in
  Alcotest.(check string) "bridge name" "test_tool" mcp_tool.name;
  Alcotest.(check string) "bridge desc" "A test tool" mcp_tool.description;
  (* Verify the SDK tool round-trips through the bridge *)
  let oas_tool = Mcp.mcp_tool_to_sdk_tool mcp_tool
    ~call_fn:(fun _input -> Ok { Types.content = "ok" }) in
  Alcotest.(check string) "oas tool name" "test_tool" oas_tool.schema.name
in

(* ── test: tasks API (v0.15.0 Generic_client) ── *)
let test_tasks () =
  Eio.Switch.run @@ fun sw ->
  (* Server with task handlers *)
  let now = "2026-03-23T00:00:00Z" in
  let server =
    Server.create ~name:"task-server" ~version:"1.0.0" ()
    |> Server.add_task_handlers {
      get = (fun _ctx task_id ->
        Ok (Mcp_types.make_task ~task_id ~created_at:now ()));
      result = (fun _ctx _task_id ->
        Ok (`Assoc [("result", `String "done")]));
      list = (fun _ctx _cursor ->
        Ok ([Mcp_types.make_task ~task_id:"t1" ~created_at:now ()], None));
      cancel = (fun _ctx task_id ->
        Ok (Mcp_types.make_task ~task_id ~created_at:now ()));
    }
  in
  let client, client_t = make_server_and_client ~sw ~server in
  (match Client.initialize client ~client_name:"oas-test" ~client_version:"0.87.0" with
   | Ok _ -> ()
   | Error e -> Alcotest.fail ("init failed: " ^ e));

  (* Get task *)
  (match Client.get_task client ~task_id:"task-123" with
   | Ok task ->
     Alcotest.(check string) "task id" "task-123" task.task_id
   | Error e -> Alcotest.fail ("get_task failed: " ^ e));

  (* List tasks *)
  (match Client.list_tasks client with
   | Ok tasks ->
     Alcotest.(check int) "one task" 1 (List.length tasks);
     Alcotest.(check string) "task id" "t1" (List.hd tasks).task_id
   | Error e -> Alcotest.fail ("list_tasks failed: " ^ e));

  (* Cancel task *)
  (match Client.cancel_task client ~task_id:"task-456" with
   | Ok task ->
     Alcotest.(check string) "cancelled id" "task-456" task.task_id
   | Error e -> Alcotest.fail ("cancel_task failed: " ^ e));

  Mt.close client_t
in

(* ── run all tests ── *)
Alcotest.run "mcp_typed_client" [
  "initialize", [
    Alcotest.test_case "typed capabilities" `Quick test_typed_capabilities;
  ];
  "tools", [
    Alcotest.test_case "list and call with Tool_arg" `Quick test_tool_list_and_call;
  ];
  "resources", [
    Alcotest.test_case "list and read" `Quick test_resources;
    Alcotest.test_case "templates (v0.15.0)" `Quick test_resource_templates;
    Alcotest.test_case "subscribe/unsubscribe (v0.15.0)" `Quick test_resource_subscribe;
  ];
  "prompts", [
    Alcotest.test_case "list and get" `Quick test_prompts;
  ];
  "tasks", [
    Alcotest.test_case "get, list, cancel (v0.15.0)" `Quick test_tasks;
  ];
  "utility", [
    Alcotest.test_case "ping" `Quick test_ping;
    Alcotest.test_case "Tool_arg extraction" `Quick test_tool_arg;
    Alcotest.test_case "OAS bridge with v0.14.0+ fields" `Quick test_mcp_schema_bridge;
  ];
]
