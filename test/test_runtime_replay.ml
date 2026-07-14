open Agent_sdk
open Alcotest

let expect_ok label = function
  | Ok value -> value
  | Error err -> fail (Printf.sprintf "%s: %s" label (Error.to_string err))
;;

let with_temp_dir f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      (Printf.sprintf "oas-runtime-replay-%d-%06x" (Unix.getpid ()) (Random.int 0xFFFFFF))
  in
  Unix.mkdir dir 0o755;
  Fun.protect
    ~finally:(fun () -> ignore (Sys.command (Printf.sprintf "rm -rf %s" dir)))
    (fun () -> f dir)
;;

let mk_session ?(artifacts = []) ?(updated_at = 1.0) session_id : Runtime.session =
  { session_id
  ; goal = "runtime replay"
  ; title = None
  ; tag = None
  ; phase = Runtime.Running
  ; created_at = updated_at -. 0.5
  ; updated_at
  ; provider = Some "test-provider"
  ; model = Some "model"
  ; system_prompt = None
  ; workdir = None
  ; planned_participants = []
  ; participants = []
  ; artifacts
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let mk_event seq message : Runtime.event =
  { seq
  ; ts = float_of_int seq
  ; kind = Runtime.Turn_recorded { actor = Some "user"; message }
  }
;;

let mk_checkpoint_event seq ?label path : Runtime.event =
  { seq; ts = float_of_int seq; kind = Runtime.Checkpoint_saved { label; path } }
;;

let mk_message text : Types.message =
  { role = Types.User
  ; content = [ Types.Text text ]
  ; name = None
  ; tool_call_id = None
  ; metadata = []
  }
;;

let mk_checkpoint ?(messages = []) ?(created_at = 1.0) ?(turn_count = 0) session_id
  : Checkpoint.t
  =
  { version = Checkpoint.checkpoint_version
  ; session_id
  ; agent_name = "runtime-replay-agent"
  ; model = "claude-sonnet-4-6"
  ; system_prompt = Some "replay"
  ; messages
  ; usage = Types.empty_usage
  ; turn_count
  ; created_at
  ; tools = []
  ; tool_choice = None
  ; disable_parallel_tool_use = false
  ; temperature = None
  ; top_p = None
  ; top_k = None
  ; min_p = None
  ; enable_thinking = None
  ; preserve_thinking = None
  ; response_format = Types.Off
  ; thinking_budget = None
  ; reasoning_effort = None
  ; cache_system_prompt = false
  ; context = Context.create_sync ()
  ; mcp_sessions = []
  ; working_context = None
  }
;;

let save_checkpoint_file root name checkpoint =
  let path = Filename.concat root name in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc (Checkpoint.to_string checkpoint));
  path
;;

(* Literal output from the released checkpoint-v6 persistence schema. Keep this
   independent of [Checkpoint.to_json] so the migration test cannot drift with
   the current v8 serializer. *)
let released_v6_checkpoint_json =
  {|
{
  "version": 6,
  "session_id": "v6-runtime-replay",
  "agent_name": "v6-agent",
  "model": "qwen3.5",
  "system_prompt": "resume the released checkpoint",
  "messages": [
    {
      "role": "tool",
      "content": [
        {
          "type": "tool_result",
          "tool_use_id": "legacy-call",
          "content": "legacy unattributed failure",
          "is_error": true
        }
      ],
      "tool_call_id": "legacy-call"
    }
  ],
  "usage": {
    "total_input_tokens": 11,
    "total_output_tokens": 7,
    "total_cache_creation_input_tokens": 3,
    "total_cache_read_input_tokens": 2,
    "api_calls": 1,
    "estimated_cost_usd": 0.25,
    "unpriced_model": "legacy-unpriced-model"
  },
  "turn_count": 4,
  "created_at": 1700000000.0,
  "tools": [],
  "tool_choice": null,
  "temperature": null,
  "top_p": null,
  "top_k": null,
  "min_p": null,
  "enable_thinking": null,
  "preserve_thinking": null,
  "response_format": { "type": "off" },
  "thinking_budget": null,
  "disable_parallel_tool_use": false,
  "cache_system_prompt": false,
  "context": { "released": "v6" },
  "mcp_sessions": [
    {
      "server_name": "legacy-filesystem",
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/tmp"],
      "env": [{ "key": "LEGACY_TOKEN", "value": "redacted" }],
      "env_policy": "inherit",
      "http_base_url": null,
      "http_headers": [],
      "tool_schemas": [],
      "transport_kind": "stdio"
    }
  ],
  "working_context": null
}
|}
;;

let released_v6_checkpoint () = Yojson.Safe.from_string released_v6_checkpoint_json

(* Sanitized external-consumer-shaped output from the released checkpoint-v5
   serializer. It covers the nested shapes seen in persisted agent snapshots
   while remaining independent of the current v8 serializer. *)
let released_v5_checkpoint_json =
  {|
{
  "version": 5,
  "session_id": "v5-corpus-session",
  "agent_name": "external-agent-corpus",
  "model": "qwen3.5",
  "system_prompt": "continue the agent timeline",
  "messages": [
    {
      "role": "user",
      "content": [
        { "type": "text", "text": "continue" }
      ],
      "metadata": {
        "channel": "workspace-channel",
        "space": "external-agent-corpus"
      }
    },
    {
      "role": "assistant",
      "content": [
        {
          "type": "thinking",
          "thinking": "inspect the current task"
        },
        {
          "type": "tool_use",
          "id": "v5-tool-call",
          "name": "inspect",
          "input": { "path": "README.md" }
        }
      ]
    },
    {
      "role": "tool",
      "content": [
        {
          "type": "tool_result",
          "tool_use_id": "v5-tool-call",
          "content": [
            { "type": "text", "text": "inspection failed" }
          ],
          "is_error": true
        }
      ],
      "tool_call_id": "v5-tool-call"
    }
  ],
  "usage": {
    "total_input_tokens": 120,
    "total_output_tokens": 35,
    "total_cache_creation_input_tokens": 0,
    "total_cache_read_input_tokens": 64,
    "api_calls": 2,
    "estimated_cost_usd": 0.0,
    "unpriced_model": null
  },
  "turn_count": 3,
  "created_at": 1752300000.0,
  "tools": [
    {
      "name": "inspect",
      "description": "Inspect a path",
      "parameters": [
        {
          "name": "path",
          "description": "Path to inspect",
          "param_type": "string",
          "required": true
        }
      ],
      "strict": true
    }
  ],
  "tool_choice": { "type": "auto" },
  "temperature": 0.2,
  "top_p": null,
  "top_k": null,
  "min_p": null,
  "enable_thinking": true,
  "preserve_thinking": true,
  "response_format": { "type": "json_mode" },
  "thinking_budget": 2048,
  "disable_parallel_tool_use": false,
  "cache_system_prompt": true,
  "context": {
    "consumer": "external-agent-corpus",
    "workspace": "shared"
  },
  "mcp_sessions": [],
  "working_context": {
    "goal": "resume safely",
    "task": "inspect"
  }
}
|}
;;

let released_v5_checkpoint () = Yojson.Safe.from_string released_v5_checkpoint_json

(* Literal output from the first released checkpoint-v5 schema. This predates
   preserve_thinking, usage.unpriced_model, and persisted HTTP MCP reconnect
   fields, while still carrying the two retired token-cap fields. *)
let released_v5_pre_preserve_capped_json =
  {|
{
  "version": 5,
  "session_id": "v5-pre-preserve-capped",
  "agent_name": "early-v5-agent",
  "model": "early-v5-model",
  "system_prompt": null,
  "messages": [],
  "usage": {
    "total_input_tokens": 5,
    "total_output_tokens": 3,
    "total_cache_creation_input_tokens": 0,
    "total_cache_read_input_tokens": 0,
    "api_calls": 1,
    "estimated_cost_usd": 0.0
  },
  "turn_count": 1,
  "created_at": 1710000000.0,
  "tools": [],
  "tool_choice": null,
  "temperature": null,
  "top_p": null,
  "top_k": null,
  "min_p": null,
  "enable_thinking": null,
  "response_format": { "type": "off" },
  "thinking_budget": null,
  "disable_parallel_tool_use": false,
  "cache_system_prompt": false,
  "max_input_tokens": 4096,
  "max_total_tokens": 16384,
  "context": {},
  "mcp_sessions": [
    {
      "server_name": "early-stdio",
      "command": "early-mcp",
      "args": ["--stdio"],
      "env": [{ "key": "EARLY_TOKEN", "value": "redacted" }],
      "tool_schemas": [],
      "transport_kind": "stdio"
    }
  ],
  "working_context": null
}
|}
;;

let released_v5_pre_preserve_capped () =
  Yojson.Safe.from_string released_v5_pre_preserve_capped_json
;;

(* Literal output from the later capped checkpoint-v5 schema. It has
   preserve_thinking, usage.unpriced_model, and HTTP MCP reconnect fields, but
   predates env_policy and the removal of the two token-cap fields. *)
let released_v5_preserve_capped_json =
  {|
{
  "version": 5,
  "session_id": "v5-preserve-capped",
  "agent_name": "later-capped-v5-agent",
  "model": "later-capped-v5-model",
  "system_prompt": "continue",
  "messages": [],
  "usage": {
    "total_input_tokens": 8,
    "total_output_tokens": 4,
    "total_cache_creation_input_tokens": 0,
    "total_cache_read_input_tokens": 2,
    "api_calls": 1,
    "estimated_cost_usd": 0.0,
    "unpriced_model": "later-capped-v5-model"
  },
  "turn_count": 2,
  "created_at": 1740000000.0,
  "tools": [],
  "tool_choice": null,
  "temperature": null,
  "top_p": null,
  "top_k": null,
  "min_p": null,
  "enable_thinking": true,
  "preserve_thinking": false,
  "response_format": { "type": "off" },
  "thinking_budget": null,
  "disable_parallel_tool_use": false,
  "cache_system_prompt": false,
  "max_input_tokens": null,
  "max_total_tokens": 32768,
  "context": {},
  "mcp_sessions": [
    {
      "server_name": "later-http",
      "command": "http",
      "args": [],
      "env": [],
      "http_base_url": "https://mcp.example.test",
      "http_headers": [
        { "key": "Authorization", "value": "redacted" }
      ],
      "tool_schemas": [],
      "transport_kind": "http"
    }
  ],
  "working_context": null
}
|}
;;

let released_v5_preserve_capped () =
  Yojson.Safe.from_string released_v5_preserve_capped_json
;;

let replace_json_field name value = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (field_name, field_value) ->
            if String.equal name field_name
            then field_name, value
            else field_name, field_value)
         fields)
  | json -> json
;;

let update_json_field name update = function
  | `Assoc fields ->
    `Assoc
      (List.map
         (fun (field_name, field_value) ->
            if String.equal name field_name
            then field_name, update field_value
            else field_name, field_value)
         fields)
  | json -> json
;;

let update_first_json update = function
  | `List (first :: rest) -> `List (update first :: rest)
  | json -> json
;;

let update_last_json update = function
  | `List values ->
    (match List.rev values with
     | last :: rest -> `List (List.rev (update last :: rest))
     | [] -> `List [])
  | json -> json
;;

let append_json_field name value = function
  | `Assoc fields -> `Assoc (fields @ [ name, value ])
  | json -> json
;;

let remove_json_field name = function
  | `Assoc fields ->
    `Assoc
      (List.filter (fun (field_name, _) -> not (String.equal name field_name)) fields)
  | json -> json
;;

let assert_checkpoint_rejected label json =
  match Checkpoint.of_json json with
  | Error (Error.Serialization (Error.JsonParseError _)) -> ()
  | Error error -> Alcotest.failf "%s: unexpected error: %s" label (Error.to_string error)
  | Ok _ -> Alcotest.failf "%s: checkpoint must be rejected" label
;;

let save_released_v6_checkpoint_file root =
  let path = Filename.concat root "released-v6-checkpoint.json" in
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr oc)
    (fun () -> output_string oc released_v6_checkpoint_json);
  path
;;

let save_artifact store session_id ~artifact_id ~name content =
  let path =
    Runtime_store.save_artifact_text store session_id ~name ~kind:"json" ~content
    |> expect_ok "save artifact"
  in
  ({ Runtime.artifact_id
   ; name
   ; kind = "json"
   ; mime_type = "application/json"
   ; path = Some path
   ; inline_content = None
   ; size_bytes = String.length content
   ; created_at = 1.0
   }
   : Runtime.artifact)
;;

let save_run ?(artifacts = []) store session_id ~updated_at events =
  Runtime_store.save_session store (mk_session ~artifacts ~updated_at session_id)
  |> expect_ok "save session";
  List.iter
    (fun event ->
       Runtime_store.append_event store session_id event |> expect_ok "append event")
    events
;;

let test_sync_windows_from_selected_runs () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let artifact =
      save_artifact store "run-a" ~artifact_id:"art-run-a" ~name:"report" {|{"ok":true}|}
    in
    save_run
      ~artifacts:[ artifact ]
      store
      "run-a"
      ~updated_at:10.0
      [ mk_event 1 "old-a"; mk_event 2 "new-a" ];
    save_run store "run-b" ~updated_at:20.0 [ mk_event 1 "old-b"; mk_event 3 "new-b" ];
    let set =
      Runtime_replay.sync_windows_from_store
        ~after_seq:1
        store
        [ Runtime_store.Last_n_runs 2 ]
      |> expect_ok "sync windows"
    in
    check int "windows" 2 (List.length set.windows);
    check
      (list string)
      "stream order"
      [ "run-a"; "run-b" ]
      (List.map (fun (window : Runtime_sync.window) -> window.stream_id) set.windows);
    check
      (list int)
      "event counts"
      [ 1; 1 ]
      (List.map
         (fun (window : Runtime_sync.window) -> List.length window.events)
         set.windows);
    let first = List.hd set.windows in
    check int "cursor" 1 first.cursor.after_seq;
    check int "next cursor" 2 first.next_cursor.after_seq;
    check (list string) "artifact refs" [ "art-run-a" ] first.artifact_refs;
    List.iter
      (fun window ->
         match Runtime_sync.validate_window window with
         | Ok () -> ()
         | Error detail -> fail detail)
      set.windows)
;;

let test_sync_windows_json_reports_selector_failures_and_dedupes_runs () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    save_run store "run-a" ~updated_at:10.0 [ mk_event 1 "old-a"; mk_event 2 "new-a" ];
    save_run store "run-b" ~updated_at:20.0 [ mk_event 1 "old-b"; mk_event 2 "new-b" ];
    let json =
      Runtime_replay.sync_windows_json_from_store
        store
        [ Runtime_store.Last_n_runs 1
        ; Runtime_store.Session "run-b"
        ; Runtime_store.Session "missing"
        ]
      |> expect_ok "sync window json"
    in
    let open Yojson.Safe.Util in
    check int "one deduped window" 1 (json |> member "windows" |> to_list |> List.length);
    check int "one failure" 1 (json |> member "failures" |> to_list |> List.length);
    check
      string
      "missing failure"
      "missing"
      (json |> member "failures" |> to_list |> List.hd |> member "session_id" |> to_string))
;;

let test_checkpoint_delta_projection_from_selected_runs () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let base =
      mk_checkpoint
        ~created_at:10.0
        ~turn_count:1
        ~messages:[ mk_message "base" ]
        "checkpoint-run"
    in
    let target =
      mk_checkpoint
        ~created_at:20.0
        ~turn_count:2
        ~messages:[ mk_message "base"; mk_message "target" ]
        "checkpoint-run"
    in
    let base_path = save_checkpoint_file root "base-checkpoint.json" base in
    let target_path = save_checkpoint_file root "target-checkpoint.json" target in
    save_run
      store
      "run-a"
      ~updated_at:10.0
      [ mk_checkpoint_event 1 ~label:"base" base_path ];
    save_run
      store
      "run-b"
      ~updated_at:20.0
      [ mk_checkpoint_event 1 ~label:"target" target_path ];
    let projection =
      Runtime_replay.checkpoint_delta_projection_from_store
        store
        [ Runtime_store.Last_n_runs 2 ]
      |> expect_ok "checkpoint projection"
    in
    check int "entries" 2 (List.length projection.entries);
    check int "failures" 0 (List.length projection.failures);
    match projection.entries with
    | [ Runtime_replay.Full_checkpoint { checkpoint; checkpoint_ref = base_ref }
      ; Runtime_replay.Delta_checkpoint
          { base = delta_base; target = delta_target; delta }
      ] ->
      check string "base path" base_path base_ref.path;
      check string "delta base path" base_path delta_base.path;
      check string "delta target path" target_path delta_target.path;
      let rebuilt = Checkpoint.apply_delta checkpoint delta |> expect_ok "apply delta" in
      check
        string
        "rebuilt target"
        (Yojson.Safe.to_string (Checkpoint.to_json target))
        (Yojson.Safe.to_string (Checkpoint.to_json rebuilt))
    | _ -> fail "expected full checkpoint followed by delta checkpoint")
;;

let test_released_v5_checkpoint_migrates_and_roundtrips () =
  let checkpoint =
    released_v5_checkpoint () |> Checkpoint.of_json |> expect_ok "migrate released v5"
  in
  check int "migrated to v8" 8 checkpoint.version;
  check int "corpus messages" 3 (List.length checkpoint.messages);
  check int "corpus tools" 1 (List.length checkpoint.tools);
  check (option bool) "thinking enabled" (Some true) checkpoint.enable_thinking;
  check
    (option string)
    "reasoning effort added"
    None
    (Option.map Llm_provider.Reasoning_effort.to_string checkpoint.reasoning_effort);
  (match (List.nth checkpoint.messages 2).content with
   | [ Types.ToolResult
         { outcome =
             Types.Tool_failed
               { failure_kind = Types.Unattributed_tool_error; error_class = None }
         ; content_blocks = Some [ Types.Text "inspection failed" ]
         ; _
         }
     ] -> ()
   | _ -> fail "v5 failed ToolResult did not migrate without invented attribution");
  let persisted = Checkpoint.to_json checkpoint in
  let reloaded = Checkpoint.of_json persisted |> expect_ok "reload migrated v5 as v8" in
  check
    string
    "stable v8 persistence"
    (Yojson.Safe.to_string persisted)
    (Yojson.Safe.to_string (Checkpoint.to_json reloaded))
;;

let assert_retired_caps_absent label checkpoint =
  match Checkpoint.to_json checkpoint with
  | `Assoc fields ->
    check
      bool
      (label ^ " max_input_tokens")
      false
      (List.mem_assoc "max_input_tokens" fields);
    check
      bool
      (label ^ " max_total_tokens")
      false
      (List.mem_assoc "max_total_tokens" fields)
  | _ -> fail "checkpoint serializer must produce an object"
;;

let test_released_v5_pre_preserve_capped_literal_migrates () =
  let checkpoint =
    Checkpoint.of_string released_v5_pre_preserve_capped_json
    |> expect_ok "migrate pre-preserve capped v5"
  in
  check int "migrated to v8" 8 checkpoint.version;
  check
    (option bool)
    "missing preserve remains unspecified"
    None
    checkpoint.preserve_thinking;
  check
    bool
    "missing pricing observation remains absent"
    true
    (checkpoint.usage.pricing_gap = None);
  assert_retired_caps_absent "pre-preserve capped" checkpoint;
  match checkpoint.mcp_sessions with
  | [ session ] ->
    (match session.transport_kind with
     | Mcp_session.Stdio -> ()
     | Mcp_session.Http -> fail "pre-HTTP stdio session changed transport");
    check (option string) "no invented HTTP URL" None session.http_base_url;
    check (list (pair string string)) "no invented HTTP headers" [] session.http_headers;
    check
      (list (pair string string))
      "stdio environment preserved"
      [ "EARLY_TOKEN", "redacted" ]
      session.env
  | _ -> fail "expected one migrated pre-HTTP MCP session"
;;

let test_released_v5_preserve_capped_literal_migrates () =
  let checkpoint =
    Checkpoint.of_string released_v5_preserve_capped_json
    |> expect_ok "migrate preserve capped v5"
  in
  check int "migrated to v8" 8 checkpoint.version;
  check (option bool) "preserve retained" (Some false) checkpoint.preserve_thinking;
  (match checkpoint.usage.pricing_gap with
   | Some (Types.Pricing_unavailable "later-capped-v5-model") -> ()
   | _ -> fail "released unpriced-model observation changed");
  assert_retired_caps_absent "preserve capped" checkpoint;
  match checkpoint.mcp_sessions with
  | [ session ] ->
    (match session.transport_kind with
     | Mcp_session.Http -> ()
     | Mcp_session.Stdio -> fail "released HTTP session changed transport");
    check
      (option string)
      "HTTP URL preserved"
      (Some "https://mcp.example.test")
      session.http_base_url;
    check
      (list (pair string string))
      "HTTP headers preserved"
      [ "Authorization", "redacted" ]
      session.http_headers
  | _ -> fail "expected one migrated HTTP MCP session"
;;

let test_released_v5_closed_shapes_reject_partial_combinations () =
  released_v5_pre_preserve_capped ()
  |> remove_json_field "max_total_tokens"
  |> assert_checkpoint_rejected "partial cap pair";
  released_v5_checkpoint ()
  |> remove_json_field "preserve_thinking"
  |> assert_checkpoint_rejected "unbounded v5 without preserve_thinking";
  released_v5_preserve_capped ()
  |> update_json_field "usage" (remove_json_field "unpriced_model")
  |> assert_checkpoint_rejected "later capped v5 without unpriced_model";
  released_v5_pre_preserve_capped ()
  |> replace_json_field "max_input_tokens" (`String "4096")
  |> assert_checkpoint_rejected "non-integer retired cap";
  released_v5_pre_preserve_capped ()
  |> update_json_field
       "mcp_sessions"
       (update_first_json (replace_json_field "transport_kind" (`String "http")))
  |> assert_checkpoint_rejected "pre-HTTP fields cannot reconstruct HTTP session";
  released_v5_pre_preserve_capped ()
  |> update_json_field "usage" (append_json_field "unpriced_model" `Null)
  |> assert_checkpoint_rejected "cross-era usage and MCP shape";
  released_v5_pre_preserve_capped ()
  |> update_json_field "mcp_sessions" (function
    | `List [ session ] ->
      let later_session =
        session
        |> append_json_field "http_base_url" `Null
        |> append_json_field "http_headers" (`List [])
      in
      `List [ session; later_session ]
    | json -> json)
  |> assert_checkpoint_rejected "mixed MCP release shapes in one checkpoint"
;;

let test_released_v5_pre_preserve_http_usage_shape_migrates () =
  let json = released_v5_preserve_capped () |> remove_json_field "preserve_thinking" in
  let checkpoint =
    Checkpoint.of_json json |> expect_ok "migrate later pre-preserve HTTP-aware v5"
  in
  check (option bool) "pre-preserve remains unspecified" None checkpoint.preserve_thinking;
  assert_retired_caps_absent "later pre-preserve" checkpoint
;;

let test_released_v5_rejects_v6_only_provenance () =
  let json =
    released_v5_checkpoint ()
    |> update_json_field
         "messages"
         (update_last_json (fun message ->
            update_json_field
              "content"
              (update_first_json
                 (append_json_field
                    "failure_kind"
                    (Types.tool_failure_kind_to_yojson Types.Validation_error)))
              message))
  in
  assert_checkpoint_rejected "v5 cannot contain v6 provenance" json
;;

let test_released_v6_http_minimal_policy_migrates () =
  let migrate_to_http session =
    session
    |> replace_json_field "command" (`String "http")
    |> replace_json_field "args" (`List [])
    |> replace_json_field "env" (`List [])
    |> replace_json_field "env_policy" (`String "minimal")
    |> replace_json_field "http_base_url" (`String "https://mcp.example.test")
    |> replace_json_field
         "http_headers"
         (`List [ `Assoc [ "key", `String "Authorization"; "value", `String "redacted" ] ])
    |> replace_json_field "transport_kind" (`String "http")
  in
  let json =
    released_v6_checkpoint ()
    |> update_json_field "mcp_sessions" (update_first_json migrate_to_http)
  in
  let checkpoint = Checkpoint.of_json json |> expect_ok "migrate released HTTP v6" in
  (match checkpoint.mcp_sessions with
   | [ session ] ->
     (match session.transport_kind with
      | Mcp_session.Http -> ()
      | Mcp_session.Stdio -> fail "HTTP transport changed during migration");
     check
       (option string)
       "HTTP base URL"
       (Some "https://mcp.example.test")
       session.http_base_url;
     check
       (list (pair string string))
       "HTTP headers"
       [ "Authorization", "redacted" ]
       session.http_headers
   | _ -> fail "expected one migrated HTTP MCP session");
  let open Yojson.Safe.Util in
  let persisted_session =
    checkpoint |> Checkpoint.to_json |> member "mcp_sessions" |> index 0
  in
  check
    bool
    "legacy policy removed"
    true
    (persisted_session |> member "env_policy" = `Null)
;;

let test_released_v6_checkpoint_replays_and_resumes () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let checkpoint_path = save_released_v6_checkpoint_file root in
    save_run
      store
      "run-v6"
      ~updated_at:20.0
      [ mk_checkpoint_event 1 ~label:"released-v6" checkpoint_path ];
    let projection =
      Runtime_replay.checkpoint_delta_projection_from_store
        store
        [ Runtime_store.Session "run-v6" ]
      |> expect_ok "v6 checkpoint projection"
    in
    check int "no replay failures" 0 (List.length projection.failures);
    let checkpoint =
      match projection.entries with
      | [ Runtime_replay.Full_checkpoint { checkpoint; _ } ] -> checkpoint
      | _ -> fail "expected one migrated full checkpoint"
    in
    check int "migrated to v8" 8 checkpoint.version;
    check int "usage input" 11 checkpoint.usage.total_input_tokens;
    check (float 0.001) "usage cost" 0.25 checkpoint.usage.estimated_cost_usd;
    (match checkpoint.usage.pricing_gap with
     | Some (Types.Pricing_unavailable "legacy-unpriced-model") -> ()
     | _ -> fail "expected migrated Pricing_unavailable gap");
    (match (List.hd checkpoint.messages).content with
     | [ Types.ToolResult
           { outcome =
               Types.Tool_failed
                 { failure_kind = Types.Unattributed_tool_error; error_class = None }
           ; _
           }
       ] -> ()
     | _ -> fail "expected migrated unattributed tool failure without error_class");
    (match checkpoint.mcp_sessions with
     | [ session ] ->
       check string "MCP server" "legacy-filesystem" session.server_name;
       check
         (list (pair string string))
         "MCP env"
         [ "LEGACY_TOKEN", "redacted" ]
         session.env;
       (match session.transport_kind with
        | Mcp_session.Stdio -> ()
        | Mcp_session.Http -> fail "expected stdio MCP session")
     | _ -> fail "expected one migrated MCP session");
    let migrated_json = Checkpoint.to_json checkpoint in
    let open Yojson.Safe.Util in
    let migrated_mcp = migrated_json |> member "mcp_sessions" |> index 0 in
    check bool "removed MCP env_policy" true (migrated_mcp |> member "env_policy" = `Null);
    Eio_main.run
    @@ fun env ->
    let agent = Agent.resume ~net:(Eio.Stdenv.net env) ~checkpoint () in
    let state = Agent.state agent in
    check int "resumed turn count" 4 state.turn_count;
    check int "resumed messages" 1 (List.length state.messages);
    check
      bool
      "resumed context"
      true
      (Context.get (Agent.context agent) "released" = Some (`String "v6")))
;;

let test_released_v6_unknown_model_identity_is_not_invented () =
  let json =
    released_v6_checkpoint ()
    |> update_json_field
         "usage"
         (replace_json_field "unpriced_model" (`String "<unknown>"))
  in
  let checkpoint = Checkpoint.of_json json |> expect_ok "migrate unknown v6 model" in
  match checkpoint.usage.pricing_gap with
  | Some Types.Model_identity_unavailable -> ()
  | Some (Types.Pricing_unavailable model_id) ->
    Alcotest.failf "legacy unknown sentinel became invented model ID %S" model_id
  | None -> fail "legacy unknown sentinel must remain observable"
;;

let test_released_v6_existing_failure_provenance_is_preserved () =
  let add_provenance = function
    | `Assoc fields ->
      `Assoc
        (fields
         @ [ "failure_kind", Types.tool_failure_kind_to_yojson Types.Validation_error
           ; "error_class", Types.tool_error_class_to_yojson Types.Deterministic
           ])
    | json -> json
  in
  let json =
    released_v6_checkpoint ()
    |> update_json_field
         "messages"
         (update_first_json (fun message ->
            update_json_field "content" (update_first_json add_provenance) message))
  in
  let checkpoint = Checkpoint.of_json json |> expect_ok "migrate attributed v6 failure" in
  match (List.hd checkpoint.messages).content with
  | [ Types.ToolResult
        { outcome =
            Types.Tool_failed
              { failure_kind = Types.Validation_error
              ; error_class = Some Types.Deterministic
              }
        ; _
        }
    ] -> ()
  | _ -> fail "existing v6 failure provenance changed during migration"
;;

let test_released_v6_rejects_current_only_failure_provenance () =
  let add_current_provenance =
    append_json_field
      "failure_kind"
      (Types.tool_failure_kind_to_yojson Types.Reported_tool_error)
  in
  let json =
    released_v6_checkpoint ()
    |> update_json_field
         "messages"
         (update_first_json (fun message ->
            update_json_field "content" (update_first_json add_current_provenance) message))
  in
  assert_checkpoint_rejected "current-only failure provenance in v6" json
;;

let test_released_v6_nested_failure_remains_unattributed () =
  let nested_failure =
    `Assoc
      [ "type", `String "tool_result"
      ; "tool_use_id", `String "nested-legacy-call"
      ; "content", `String "nested legacy failure"
      ; "is_error", `Bool true
      ]
  in
  let json =
    released_v6_checkpoint ()
    |> update_json_field
         "messages"
         (update_first_json (fun message ->
            update_json_field
              "content"
              (update_first_json
                 (replace_json_field "content" (`List [ nested_failure ])))
              message))
  in
  let checkpoint = Checkpoint.of_json json |> expect_ok "migrate nested v6 failure" in
  match (List.hd checkpoint.messages).content with
  | [ Types.ToolResult
        { content_blocks =
            Some
              [ Types.ToolResult
                  { outcome =
                      Types.Tool_failed
                        { failure_kind = Types.Unattributed_tool_error
                        ; error_class = None
                        }
                  ; _
                  }
              ]
        ; _
        }
    ] -> ()
  | _ -> fail "nested v6 failure provenance was invented or discarded"
;;

let test_released_v6_malformed_nested_values_are_rejected () =
  let malformed_tool_content =
    released_v6_checkpoint ()
    |> update_json_field
         "messages"
         (update_first_json (fun message ->
            update_json_field
              "content"
              (update_first_json (replace_json_field "content" (`Int 7)))
              message))
  in
  assert_checkpoint_rejected "numeric tool result content" malformed_tool_content;
  let malformed_headers =
    released_v6_checkpoint ()
    |> update_json_field
         "mcp_sessions"
         (update_first_json (fun session ->
            replace_json_field "http_headers" (`Assoc []) session))
  in
  assert_checkpoint_rejected "object MCP headers" malformed_headers
;;

let test_released_v6_environment_widening_is_rejected () =
  List.iter
    (fun env_policy ->
       let json =
         released_v6_checkpoint ()
         |> update_json_field
              "mcp_sessions"
              (update_first_json (fun session ->
                 replace_json_field "env_policy" (`String env_policy) session))
       in
       assert_checkpoint_rejected ("MCP env_policy " ^ env_policy) json)
    [ "minimal"; "explicit" ]
;;

let test_checkpoint_delta_projection_reports_corrupt_checkpoint () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let checkpoint = mk_checkpoint ~messages:[ mk_message "valid" ] "checkpoint-run" in
    let valid_path = save_checkpoint_file root "valid-checkpoint.json" checkpoint in
    let corrupt_path = Filename.concat root "corrupt-checkpoint.json" in
    let oc = open_out corrupt_path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () -> output_string oc "not checkpoint json");
    save_run
      store
      "run-a"
      ~updated_at:10.0
      [ mk_checkpoint_event 1 valid_path; mk_checkpoint_event 2 corrupt_path ];
    let projection =
      Runtime_replay.checkpoint_delta_projection_from_store
        store
        [ Runtime_store.Session "run-a" ]
      |> expect_ok "checkpoint projection"
    in
    check int "valid entry" 1 (List.length projection.entries);
    check int "one failure" 1 (List.length projection.failures);
    match projection.failures with
    | [ failure ] -> check string "corrupt path" corrupt_path failure.path
    | _ -> fail "expected one corrupt checkpoint failure")
;;

let test_checkpoint_delta_projection_dedupes_overlapping_checkpoint_paths () =
  with_temp_dir (fun root ->
    let store = Runtime_store.create ~root () |> expect_ok "create store" in
    let checkpoint = mk_checkpoint ~messages:[ mk_message "same" ] "checkpoint-run" in
    let path = save_checkpoint_file root "same-checkpoint.json" checkpoint in
    save_run
      store
      "run-a"
      ~updated_at:10.0
      [ mk_checkpoint_event 1 path; mk_checkpoint_event 2 path ];
    let json =
      Runtime_replay.checkpoint_delta_projection_json_from_store
        store
        [ Runtime_store.Last_n_runs 1; Runtime_store.Session "run-a" ]
      |> expect_ok "checkpoint projection json"
    in
    let open Yojson.Safe.Util in
    check
      int
      "one projected checkpoint"
      1
      (json |> member "entries" |> to_list |> List.length);
    check
      string
      "projection kind"
      "checkpoint_delta_v1"
      (json |> member "projection" |> to_string))
;;

let () =
  Alcotest.run
    "runtime_replay"
    [ ( "sync_windows"
      , [ test_case
            "selected runs to sync windows"
            `Quick
            test_sync_windows_from_selected_runs
        ; test_case
            "json reports failures and dedupes runs"
            `Quick
            test_sync_windows_json_reports_selector_failures_and_dedupes_runs
        ] )
    ; ( "checkpoint_delta_projection"
      , [ test_case
            "selected checkpoints project full plus delta"
            `Quick
            test_checkpoint_delta_projection_from_selected_runs
        ; test_case
            "corrupt checkpoint is a partial failure"
            `Quick
            test_checkpoint_delta_projection_reports_corrupt_checkpoint
        ; test_case
            "released v5 checkpoint migrates and roundtrips through v8"
            `Quick
            test_released_v5_checkpoint_migrates_and_roundtrips
        ; test_case
            "released v5 pre-preserve capped literal migrates"
            `Quick
            test_released_v5_pre_preserve_capped_literal_migrates
        ; test_case
            "released v5 preserve capped literal migrates"
            `Quick
            test_released_v5_preserve_capped_literal_migrates
        ; test_case
            "released v5 rejects partial release-shape combinations"
            `Quick
            test_released_v5_closed_shapes_reject_partial_combinations
        ; test_case
            "released v5 pre-preserve HTTP-aware usage shape migrates"
            `Quick
            test_released_v5_pre_preserve_http_usage_shape_migrates
        ; test_case
            "released v5 rejects v6-only provenance"
            `Quick
            test_released_v5_rejects_v6_only_provenance
        ; test_case
            "released v6 checkpoint replays and resumes through v8"
            `Quick
            test_released_v6_checkpoint_replays_and_resumes
        ; test_case
            "released v6 HTTP minimal policy migrates"
            `Quick
            test_released_v6_http_minimal_policy_migrates
        ; test_case
            "released v6 unknown model remains unidentified"
            `Quick
            test_released_v6_unknown_model_identity_is_not_invented
        ; test_case
            "released v6 provenance is preserved"
            `Quick
            test_released_v6_existing_failure_provenance_is_preserved
        ; test_case
            "released v6 rejects current-only provenance"
            `Quick
            test_released_v6_rejects_current_only_failure_provenance
        ; test_case
            "released v6 nested failure remains unattributed"
            `Quick
            test_released_v6_nested_failure_remains_unattributed
        ; test_case
            "released v6 malformed nested values are rejected"
            `Quick
            test_released_v6_malformed_nested_values_are_rejected
        ; test_case
            "released v6 environment widening is rejected"
            `Quick
            test_released_v6_environment_widening_is_rejected
        ; test_case
            "overlapping checkpoint paths are deduped"
            `Quick
            test_checkpoint_delta_projection_dedupes_overlapping_checkpoint_paths
        ] )
    ]
;;
