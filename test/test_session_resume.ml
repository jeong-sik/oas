open Agent_sdk

(* ── Test helpers ────────────────────────────────────────────── *)

let make_checkpoint
    ?(session_id = "sess-original")
    ?(agent_name = "test-agent")
    ?(model = Types.Claude_sonnet_4_6)
    ?(system_prompt = Some "You are helpful.")
    ?(messages = [])
    ?(usage = Types.empty_usage)
    ?(turn_count = 0)
    ?(created_at = 1000.0)
    ?(tools = [])
    ?(tool_choice = None)
    ?(context = Context.create ())
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
    created_at;
    tools;
    tool_choice;
    temperature = None;
    top_p = None;
    top_k = None;
    min_p = None;
    enable_thinking = None;
    response_format_json = false;
    thinking_budget = None;
    cache_system_prompt = false;
    max_input_tokens = None;
    max_total_tokens = None;
    context;
    mcp_sessions = [];
  }

let sample_messages = [
  { Types.role = Types.User; content = [Types.Text "Hello"] };
  { Types.role = Types.Assistant; content = [Types.Text "Hi there."] };
  { Types.role = Types.User; content = [Types.Text "What is 2+2?"] };
  { Types.role = Types.Assistant; content = [Types.Text "4"] };
]

let sample_usage : Types.usage_stats = {
  total_input_tokens = 150;
  total_output_tokens = 80;
  total_cache_creation_input_tokens = 20;
  total_cache_read_input_tokens = 10;
  api_calls = 3;
}

let sample_tool_schema : Types.tool_schema = {
  name = "get_weather";
  description = "Get weather for a location";
  parameters = [
    { Types.name = "location"; description = "City name";
      param_type = Types.String; required = true };
  ];
}

(* ── Session.resume_from tests ───────────────────────────────── *)

let test_resume_from_generates_new_id () =
  let cp = make_checkpoint ~session_id:"orig-123" () in
  let s = Session.resume_from cp in
  Alcotest.(check bool) "new id is not original"
    true (s.id <> "orig-123");
  Alcotest.(check bool) "starts with session-"
    true (String.length s.id > 8 && String.sub s.id 0 8 = "session-")

let test_resume_from_links_session_id () =
  let cp = make_checkpoint ~session_id:"orig-456" () in
  let s = Session.resume_from cp in
  Alcotest.(check (option string)) "resumed_from"
    (Some "orig-456") s.resumed_from

let test_resume_from_carries_turn_count () =
  let cp = make_checkpoint ~turn_count:7 () in
  let s = Session.resume_from cp in
  Alcotest.(check int) "turn_count" 7 s.turn_count

let test_resume_from_fresh_timestamps () =
  let cp = make_checkpoint ~created_at:1000.0 () in
  let s = Session.resume_from cp in
  Alcotest.(check bool) "started_at is recent"
    true (s.started_at > 1000.0);
  Alcotest.(check bool) "last_active_at is recent"
    true (s.last_active_at > 1000.0)

let test_resume_from_cwd_is_none () =
  let cp = make_checkpoint () in
  let s = Session.resume_from cp in
  Alcotest.(check (option string)) "cwd" None s.cwd

let test_resume_from_fresh_metadata () =
  let cp = make_checkpoint () in
  let s = Session.resume_from cp in
  Alcotest.(check (option string)) "no keys"
    None
    (match Context.get s.metadata "anything" with
     | Some (`String v) -> Some v
     | _ -> None)

let test_resume_from_copies_checkpoint_context () =
  let ctx = Context.create () in
  Context.set_scoped ctx Context.Session "trace_id" (`String "abc");
  let cp = make_checkpoint ~context:ctx () in
  let s = Session.resume_from cp in
  Alcotest.(check bool) "metadata copied" true
    (Context.get_scoped s.metadata Context.Session "trace_id"
     = Some (`String "abc"))

let test_resume_from_zero_turn_checkpoint () =
  let cp = make_checkpoint ~turn_count:0 ~messages:[] () in
  let s = Session.resume_from cp in
  Alcotest.(check int) "turn_count" 0 s.turn_count;
  Alcotest.(check (option string)) "resumed_from"
    (Some "sess-original") s.resumed_from

let test_resume_from_unique_ids () =
  let cp = make_checkpoint () in
  let s1 = Session.resume_from cp in
  let s2 = Session.resume_from cp in
  Alcotest.(check bool) "different ids" true (s1.id <> s2.id)

let test_resume_from_empty_session_id () =
  let cp = make_checkpoint ~session_id:"" () in
  let s = Session.resume_from cp in
  Alcotest.(check (option string)) "empty session_id yields None"
    None s.resumed_from

(* ── Agent.resume tests ──────────────────────────────────────── *)

let with_net f =
  Eio_main.run @@ fun env ->
  let net = Eio.Stdenv.net env in
  f net

let test_resume_restores_messages () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~messages:sample_messages () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check int) "message count" 4
    (List.length (Agent.state agent).messages)

let test_resume_restores_context () =
  with_net @@ fun net ->
  let ctx = Context.create () in
  Context.set_scoped ctx Context.User "theme" (`String "dark");
  let cp = make_checkpoint ~context:ctx () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check bool) "context restored" true
    (Context.get_scoped (Agent.context agent) Context.User "theme"
     = Some (`String "dark"))

let test_resume_restores_usage () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~usage:sample_usage () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check int) "input_tokens" 150
    (Agent.state agent).usage.total_input_tokens;
  Alcotest.(check int) "output_tokens" 80
    (Agent.state agent).usage.total_output_tokens;
  Alcotest.(check int) "api_calls" 3
    (Agent.state agent).usage.api_calls

let test_resume_restores_turn_count () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~turn_count:5 () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check int) "turn_count" 5 (Agent.state agent).turn_count

let test_resume_restores_model () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~model:Types.Claude_opus_4_6 () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check string) "model"
    (Types.model_to_string Types.Claude_opus_4_6)
    (Types.model_to_string (Agent.state agent).config.model)

let test_resume_restores_agent_name () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~agent_name:"my-agent" () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check string) "name" "my-agent" (Agent.state agent).config.name

let test_resume_restores_system_prompt () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~system_prompt:(Some "Be concise.") () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check (option string)) "system_prompt"
    (Some "Be concise.") (Agent.state agent).config.system_prompt

let test_resume_restores_tool_choice () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~tool_choice:(Some Types.Any) () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  let tc_str = match (Agent.state agent).config.tool_choice with
    | Some Types.Any -> "any"
    | Some Types.Auto -> "auto"
    | Some (Types.Tool n) -> "tool:" ^ n
    | None -> "none"
  in
  Alcotest.(check string) "tool_choice" "any" tc_str

let test_resume_none_system_prompt () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~system_prompt:None () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check (option string)) "system_prompt"
    None (Agent.state agent).config.system_prompt

let test_resume_with_tools () =
  with_net @@ fun net ->
  let tool = Tool.create
    ~name:"echo"
    ~description:"Echo input"
    ~parameters:[]
    (fun input -> Ok { Types.content = Yojson.Safe.to_string input })
  in
  let cp = make_checkpoint () in
  let agent = Agent.resume ~net ~checkpoint:cp ~tools:[tool] () in
  Alcotest.(check int) "tool count" 1 (List.length (Agent.tools agent))

let test_resume_default_options () =
  with_net @@ fun net ->
  let cp = make_checkpoint () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check string) "base_url"
    Api.default_base_url (Agent.options agent).base_url

let test_resume_custom_options () =
  with_net @@ fun net ->
  let cp = make_checkpoint () in
  let opts = { Agent.default_options with
    base_url = "http://localhost:8080" } in
  let agent = Agent.resume ~net ~checkpoint:cp ~options:opts () in
  Alcotest.(check string) "custom base_url"
    "http://localhost:8080" (Agent.options agent).base_url

let test_resume_with_config_override () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~agent_name:"cp-name" () in
  let cfg = { Types.default_config with max_turns = 50; max_tokens = 8192 } in
  let agent = Agent.resume ~net ~checkpoint:cp ~config:cfg () in
  (* checkpoint fields override config *)
  Alcotest.(check string) "name from checkpoint"
    "cp-name" (Agent.state agent).config.name;
  (* config fields not in checkpoint are preserved *)
  Alcotest.(check int) "max_turns from config" 50
    (Agent.state agent).config.max_turns;
  Alcotest.(check int) "max_tokens from config" 8192
    (Agent.state agent).config.max_tokens

let test_resume_empty_checkpoint () =
  with_net @@ fun net ->
  let cp = make_checkpoint
    ~messages:[] ~usage:Types.empty_usage ~turn_count:0 () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check int) "messages" 0
    (List.length (Agent.state agent).messages);
  Alcotest.(check int) "turn_count" 0 (Agent.state agent).turn_count;
  Alcotest.(check int) "api_calls" 0 (Agent.state agent).usage.api_calls

let test_resume_preserves_message_content () =
  with_net @@ fun net ->
  let msgs = [
    { Types.role = Types.User;
      content = [Types.Text "Hello"; Types.Text "World"] };
  ] in
  let cp = make_checkpoint ~messages:msgs () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  let first_msg = List.hd (Agent.state agent).messages in
  Alcotest.(check int) "content blocks" 2
    (List.length first_msg.content);
  let first_text = match List.hd first_msg.content with
    | Types.Text s -> s | _ -> "not text"
  in
  Alcotest.(check string) "first text" "Hello" first_text

(* ── Checkpoint → resume roundtrip ───────────────────────────── *)

let test_checkpoint_resume_roundtrip () =
  with_net @@ fun net ->
  let cp = make_checkpoint
    ~session_id:"rt-sess"
    ~agent_name:"rt-agent"
    ~model:Types.Claude_haiku_4_5
    ~system_prompt:(Some "Be brief.")
    ~messages:sample_messages
    ~usage:sample_usage
    ~turn_count:4
    ~tools:[sample_tool_schema]
    ~tool_choice:(Some (Types.Tool "get_weather"))
    ()
  in
  let json_str = Checkpoint.to_string cp in
  let cp2 = Result.get_ok (Checkpoint.of_string json_str) in
  let agent = Agent.resume ~net ~checkpoint:cp2 () in
  Alcotest.(check string) "agent_name" "rt-agent"
    (Agent.state agent).config.name;
  Alcotest.(check int) "messages" 4
    (List.length (Agent.state agent).messages);
  Alcotest.(check int) "turn_count" 4 (Agent.state agent).turn_count;
  Alcotest.(check int) "input_tokens" 150
    (Agent.state agent).usage.total_input_tokens

let test_session_resume_from_then_record () =
  let cp = make_checkpoint ~turn_count:3 () in
  let s = Session.resume_from cp in
  let s = Session.record_turn s in
  let s = Session.record_turn s in
  Alcotest.(check int) "turn_count after 2 more" 5 s.turn_count

let test_resume_cache_tokens () =
  with_net @@ fun net ->
  let usage : Types.usage_stats = {
    total_input_tokens = 100;
    total_output_tokens = 50;
    total_cache_creation_input_tokens = 30;
    total_cache_read_input_tokens = 25;
    api_calls = 2;
  } in
  let cp = make_checkpoint ~usage () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check int) "cache_creation" 30
    (Agent.state agent).usage.total_cache_creation_input_tokens;
  Alcotest.(check int) "cache_read" 25
    (Agent.state agent).usage.total_cache_read_input_tokens

let test_resume_custom_model () =
  with_net @@ fun net ->
  let cp = make_checkpoint ~model:(Types.Custom "my-local-model") () in
  let agent = Agent.resume ~net ~checkpoint:cp () in
  Alcotest.(check string) "custom model"
    "my-local-model"
    (Types.model_to_string (Agent.state agent).config.model)

(* ── Test runner ─────────────────────────────────────────────── *)

let () =
  let open Alcotest in
  run "Session Resume" [
    "Session.resume_from", [
      test_case "generates new id" `Quick test_resume_from_generates_new_id;
      test_case "links session_id" `Quick test_resume_from_links_session_id;
      test_case "carries turn_count" `Quick test_resume_from_carries_turn_count;
      test_case "fresh timestamps" `Quick test_resume_from_fresh_timestamps;
      test_case "cwd is None" `Quick test_resume_from_cwd_is_none;
      test_case "fresh metadata" `Quick test_resume_from_fresh_metadata;
      test_case "copies checkpoint context" `Quick
        test_resume_from_copies_checkpoint_context;
      test_case "zero turn checkpoint" `Quick test_resume_from_zero_turn_checkpoint;
      test_case "unique ids" `Quick test_resume_from_unique_ids;
      test_case "empty session_id" `Quick test_resume_from_empty_session_id;
    ];

    "Agent.resume", [
      test_case "restores messages" `Quick test_resume_restores_messages;
      test_case "restores context" `Quick test_resume_restores_context;
      test_case "restores usage" `Quick test_resume_restores_usage;
      test_case "restores turn_count" `Quick test_resume_restores_turn_count;
      test_case "restores model" `Quick test_resume_restores_model;
      test_case "restores agent_name" `Quick test_resume_restores_agent_name;
      test_case "restores system_prompt" `Quick test_resume_restores_system_prompt;
      test_case "restores tool_choice" `Quick test_resume_restores_tool_choice;
      test_case "none system_prompt" `Quick test_resume_none_system_prompt;
      test_case "with tools" `Quick test_resume_with_tools;
      test_case "default options" `Quick test_resume_default_options;
      test_case "custom options" `Quick test_resume_custom_options;
      test_case "config override" `Quick test_resume_with_config_override;
      test_case "empty checkpoint" `Quick test_resume_empty_checkpoint;
      test_case "preserves message content" `Quick test_resume_preserves_message_content;
      test_case "cache tokens" `Quick test_resume_cache_tokens;
      test_case "custom model" `Quick test_resume_custom_model;
    ];

    "roundtrip", [
      test_case "checkpoint → serialize → resume" `Quick test_checkpoint_resume_roundtrip;
      test_case "resume_from then record_turn" `Quick test_session_resume_from_then_record;
    ];
  ]
