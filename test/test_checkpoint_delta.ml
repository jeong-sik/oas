open Agent_sdk
open Types

let small_string_gen =
  let open QCheck.Gen in
  map
    (fun chars -> chars |> List.to_seq |> String.of_seq)
    (list_size (int_range 0 12) printable)

let yojson_simple_gen =
  let open QCheck.Gen in
  oneof
    [
      map (fun s -> `String s) small_string_gen;
      map (fun n -> `Int n) (int_range 0 50);
      map (fun b -> `Bool b) bool;
    ]

let message_gen =
  let open QCheck.Gen in
  map2
    (fun role text -> { role; content = [ Text text ]; name = None; tool_call_id = None })
    (oneof [ return User; return Assistant ])
    small_string_gen

let usage_stats_gen =
  let open QCheck.Gen in
  let* input_tokens = int_range 0 500 in
  let* output_tokens = int_range 0 500 in
  let* cache_creation = int_range 0 50 in
  let* cache_read = int_range 0 50 in
  let* api_calls = int_range 0 10 in
  let* estimated_cost_cents = int_range 0 100 in
  return
    {
      total_input_tokens = input_tokens;
      total_output_tokens = output_tokens;
      total_cache_creation_input_tokens = cache_creation;
      total_cache_read_input_tokens = cache_read;
      api_calls;
      estimated_cost_usd = float_of_int estimated_cost_cents /. 100.0;
    }

let tool_param_gen =
  let open QCheck.Gen in
  map4
    (fun name description param_type required ->
      { name; description; param_type; required })
    small_string_gen
    small_string_gen
    (oneof
       [
         return String;
         return Integer;
         return Number;
         return Boolean;
         return Array;
         return Object;
       ])
    bool

let tool_schema_gen =
  let open QCheck.Gen in
  map3
    (fun name description parameters -> { name; description; parameters })
    small_string_gen
    small_string_gen
    (list_size (int_range 0 2) tool_param_gen)

let context_gen =
  let open QCheck.Gen in
  map
    (fun pairs ->
      let ctx = Context.create () in
      List.iter (fun (key, value) -> Context.set ctx key value) pairs;
      ctx)
    (list_size (int_range 0 3) (pair small_string_gen yojson_simple_gen))

let tool_choice_gen =
  let open QCheck.Gen in
  oneof
    [
      return None;
      return (Some Auto);
      return (Some Any);
      map (fun s -> Some (Tool s)) small_string_gen;
      return (Some None_);
    ]

let mcp_info_gen =
  let open QCheck.Gen in
  map4
    (fun server_name command args tool_schemas ->
      {
        Mcp_session.server_name;
        command;
        args;
        env = [];
        tool_schemas;
        transport_kind = Mcp_session.Stdio;
      })
    small_string_gen
    small_string_gen
    (list_size (int_range 0 2) small_string_gen)
    (list_size (int_range 0 1) tool_schema_gen)

let checkpoint_gen =
  let open QCheck.Gen in
  let* session_id = small_string_gen in
  let* agent_name = small_string_gen in
  let* model =
    oneof
      [
        return "claude-sonnet-4-6";
        return "claude-opus-4-6";
        small_string_gen;
      ]
  in
  let* system_prompt = option small_string_gen in
  let* messages = list_size (int_range 0 5) message_gen in
  let* usage = usage_stats_gen in
  let* turn_count = int_range 0 10 in
  let* created_at = int_range 0 10_000 in
  let* tools = list_size (int_range 0 2) tool_schema_gen in
  let* tool_choice = tool_choice_gen in
  let* disable_parallel_tool_use = bool in
  let* temperature = option (map float_of_int (int_range 0 2)) in
  let* top_p = option (map (fun n -> float_of_int n /. 10.0) (int_range 0 10)) in
  let* top_k = option (int_range 1 40) in
  let* min_p = option (map (fun n -> float_of_int n /. 20.0) (int_range 0 20)) in
  let* enable_thinking = option bool in
  let* response_format_json = bool in
  let* thinking_budget = option (int_range 0 2048) in
  let* cache_system_prompt = bool in
  let* max_input_tokens = option (int_range 0 4096) in
  let* max_total_tokens = option (int_range 0 4096) in
  let* context = context_gen in
  let* mcp_sessions = list_size (int_range 0 1) mcp_info_gen in
  let* working_context = option yojson_simple_gen in
  return
    {
      Checkpoint.version = Checkpoint.checkpoint_version;
      session_id;
      agent_name;
      model;
      system_prompt;
      messages;
      usage;
      turn_count;
      created_at = float_of_int created_at;
      tools;
      tool_choice;
      disable_parallel_tool_use;
      temperature;
      top_p;
      top_k;
      min_p;
      enable_thinking;
      response_format_json;
      thinking_budget;
      cache_system_prompt;
      max_input_tokens;
      max_total_tokens;
      context;
      mcp_sessions;
      working_context;
    }

let arb_checkpoint =
  QCheck.make checkpoint_gen ~print:(fun checkpoint ->
      Yojson.Safe.to_string (Checkpoint.to_json checkpoint))

let checkpoint_equal left right =
  Checkpoint.to_json left = Checkpoint.to_json right

let with_eio f () = Eio_main.run (fun _env -> f ())

let with_env key value f =
  let original = Sys.getenv_opt key in
  let restore () =
    match original with
    | Some previous -> Unix.putenv key previous
    | None -> Unix.putenv key ""
  in
  (* OCaml's Unix module on our supported switches does not provide unsetenv.
     For this feature flag helper, the empty string is equivalent to "off". *)
  (match value with
   | Some next -> Unix.putenv key next
   | None -> Unix.putenv key "");
  Fun.protect f ~finally:restore

let make_unit_checkpoint ?(messages = []) ?(session_id = "sess-a")
    ?(agent_name = "agent-a") ?(turn_count = 0) ?(context = Context.create ())
    ?(tool_choice = None) ?(working_context = None) () =
  {
    Checkpoint.version = Checkpoint.checkpoint_version;
    session_id;
    agent_name;
    model = "claude-sonnet-4-6";
    system_prompt = Some "Be careful.";
    messages;
    usage = Types.empty_usage;
    turn_count;
    created_at = 1000.0;
    tools = [];
    tool_choice;
    disable_parallel_tool_use = false;
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
    working_context;
  }

let test_delta_roundtrip_property =
  QCheck.Test.make ~count:100 ~name:"checkpoint delta round-trip"
    QCheck.(pair arb_checkpoint arb_checkpoint)
    (fun (base, target) ->
      match Checkpoint.apply_delta base (Checkpoint.compute_delta base target) with
      | Ok rebuilt -> checkpoint_equal rebuilt target
      | Error _ -> false)

let test_delta_json_roundtrip () =
  let base =
    make_unit_checkpoint
      ~messages:
        [
          { role = User; content = [ Text "hello" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let ctx = Context.create () in
  Context.set ctx "trace_id" (`String "abc");
  let target =
    make_unit_checkpoint
      ~session_id:"sess-b"
      ~agent_name:"agent-b"
      ~turn_count:2
      ~context:ctx
      ~tool_choice:(Some Auto)
      ~working_context:(Some (`Assoc [ ("kind", `String "test_context_v1") ]))
      ~messages:
        [
          { role = User; content = [ Text "hello" ]; name = None; tool_call_id = None };
          { role = Assistant; content = [ Text "world" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let delta = Checkpoint.compute_delta base target in
  let decoded = delta |> Checkpoint.delta_to_json |> Checkpoint.delta_of_json |> Result.get_ok in
  Alcotest.(check bool) "delta apply works after JSON roundtrip" true
    (match Checkpoint.apply_delta base decoded with
     | Ok rebuilt -> checkpoint_equal rebuilt target
     | Error _ -> false)

let test_delta_json_rejects_malformed_context_removed () =
  let base_context = Context.create () in
  let target_context = Context.create () in
  Context.set target_context "trace_id" (`String "abc");
  let base = make_unit_checkpoint ~context:base_context () in
  let target =
    make_unit_checkpoint
      ~context:target_context
      ~messages:
        [
          { role = User; content = [ Text "hello" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let malformed_json =
    Checkpoint.compute_delta base target
    |> Checkpoint.delta_to_json
    |> function
    | `Assoc fields ->
      let operations =
        match List.assoc_opt "operations" fields with
        | Some (`List ops) ->
          `List
            (List.map
               (function
                 | `Assoc op_fields as op_json ->
                   let is_patch_context =
                     List.assoc_opt "kind" op_fields = Some (`String "patch_context")
                   in
                   if not is_patch_context then op_json
                   else
                     `Assoc
                       (List.map
                          (fun (key, value) ->
                            if key = "diff" then
                              match value with
                              | `Assoc diff_fields ->
                                let patched_diff =
                                  List.map
                                    (fun (diff_key, diff_value) ->
                                      if diff_key = "removed" then
                                        (diff_key, `String "bad")
                                      else (diff_key, diff_value))
                                    diff_fields
                                in
                                (key, `Assoc patched_diff)
                              | _ -> (key, value)
                            else (key, value))
                          op_fields)
                 | op_json -> op_json)
               ops)
        | _ -> List.assoc "operations" fields
      in
      `Assoc
        (List.map
           (fun (key, value) ->
             if key = "operations" then (key, operations) else (key, value))
           fields)
    | json -> json
  in
  Alcotest.(check bool) "malformed removed field rejected" true
    (Result.is_error (Checkpoint.delta_of_json malformed_json))

let test_empty_delta_roundtrip () =
  let checkpoint =
    make_unit_checkpoint
      ~session_id:"sess-a"
      ~agent_name:"agent-a"
      ~turn_count:3
      ~messages:
        [
          { role = User; content = [ Text "steady" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let delta = Checkpoint.compute_delta checkpoint checkpoint in
  Alcotest.(check int) "no operations" 0 (List.length delta.operations);
  Alcotest.(check bool) "noop delta applies cleanly" true
    (match Checkpoint.apply_delta checkpoint delta with
     | Ok rebuilt -> checkpoint_equal rebuilt checkpoint
     | Error _ -> false)

let test_apply_delta_rejects_version_and_hash_mismatch () =
  let base = make_unit_checkpoint () in
  let target = make_unit_checkpoint ~session_id:"sess-b" () in
  let delta = Checkpoint.compute_delta base target in
  let bad_version = { delta with delta_version = delta.delta_version + 1 } in
  let bad_checkpoint_version =
    {
      delta with
      base_checkpoint_version = delta.base_checkpoint_version + 1;
    }
  in
  let bad_base_hash = { delta with base_checkpoint_hash = "bad-hash" } in
  let bad_result_hash = { delta with result_checkpoint_hash = "bad-result-hash" } in
  Alcotest.(check bool) "delta version mismatch" true
    (Result.is_error (Checkpoint.apply_delta base bad_version));
  Alcotest.(check bool) "checkpoint version mismatch" true
    (Result.is_error (Checkpoint.apply_delta base bad_checkpoint_version));
  Alcotest.(check bool) "base hash mismatch" true
    (Result.is_error (Checkpoint.apply_delta base bad_base_hash));
  Alcotest.(check bool) "result hash mismatch" true
    (Result.is_error (Checkpoint.apply_delta base bad_result_hash))

let test_apply_delta_rejects_invalid_splice () =
  let base =
    make_unit_checkpoint
      ~messages:
        [
          { role = User; content = [ Text "a" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let target =
    make_unit_checkpoint
      ~messages:
        [
          { role = User; content = [ Text "a" ]; name = None; tool_call_id = None };
          { role = Assistant; content = [ Text "b" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let delta = Checkpoint.compute_delta base target in
  let invalid_delta =
    {
      delta with
      operations =
        [
          Checkpoint.Splice_messages
            { start_index = 3; delete_count = 1; insert = [] };
        ];
    }
  in
  Alcotest.(check bool) "invalid splice rejected" true
    (Result.is_error (Checkpoint.apply_delta base invalid_delta))

let test_restore_with_delta_fallback_disabled () =
  let base = make_unit_checkpoint () in
  let target =
    make_unit_checkpoint
      ~session_id:"sess-b"
      ~messages:
        [
          { role = User; content = [ Text "delta" ]; name = None; tool_call_id = None };
        ]
      ()
  in
  let delta = Checkpoint.compute_delta base target in
  let result =
    with_env "OAS_DELTA_CHECKPOINT" None (fun () ->
        Checkpoint.restore_with_delta_fallback ~base ~delta ~full_checkpoint:target ()
        |> Result.get_ok)
  in
  Alcotest.(check bool) "full checkpoint used" true
    (result.mode = Checkpoint.Full_restore && checkpoint_equal result.checkpoint target)

let test_restore_with_delta_fallback_records_failure_metrics () =
  let metrics = Metrics.create () in
  let base = make_unit_checkpoint () in
  let target = make_unit_checkpoint ~session_id:"sess-b" () in
  let delta =
    { (Checkpoint.compute_delta base target) with base_checkpoint_hash = "bad-hash" }
  in
  let result =
    with_env "OAS_DELTA_CHECKPOINT" (Some "1") (fun () ->
        Checkpoint.restore_with_delta_fallback ~metrics ~base ~delta
          ~full_checkpoint:target ()
        |> Result.get_ok)
  in
  let apply_total =
    Metrics.counter metrics ~name:"oas.checkpoint.delta_apply_total" ~unit_:"1"
  in
  let apply_failures =
    Metrics.counter metrics ~name:"oas.checkpoint.delta_apply_failures_total" ~unit_:"1"
  in
  let fallback_total =
    Metrics.counter metrics ~name:"oas.checkpoint.full_restore_fallback_total" ~unit_:"1"
  in
  let size_histogram =
    Metrics.histogram metrics ~name:"oas.checkpoint.delta_size_bytes"
      ~buckets:[ 128.; 512.; 1024.; 4096.; 16384.; 65536. ]
  in
  Alcotest.(check bool) "fallback used" true
    (result.mode = Checkpoint.Full_restore && checkpoint_equal result.checkpoint target);
  Alcotest.(check int) "apply total" 1 (Metrics.counter_value apply_total ());
  Alcotest.(check int) "apply failures" 1 (Metrics.counter_value apply_failures ());
  Alcotest.(check int) "fallback total" 1 (Metrics.counter_value fallback_total ());
  Alcotest.(check int) "delta size observed" 1 (Metrics.histogram_count size_histogram)

let test_restore_with_delta_fallback_gate_skips_after_failure () =
  let metrics = Metrics.create () in
  let base = make_unit_checkpoint () in
  let target = make_unit_checkpoint ~session_id:"sess-b" () in
  let bad_delta =
    { (Checkpoint.compute_delta base target) with base_checkpoint_hash = "bad-hash" }
  in
  let good_delta = Checkpoint.compute_delta base target in
  let first =
    with_env "OAS_DELTA_CHECKPOINT" (Some "1") (fun () ->
        Checkpoint.restore_with_delta_fallback ~metrics ~base ~delta:bad_delta
          ~full_checkpoint:target ()
        |> Result.get_ok)
  in
  let second =
    with_env "OAS_DELTA_CHECKPOINT" (Some "1") (fun () ->
        Checkpoint.restore_with_delta_fallback ~metrics ~base ~delta:good_delta
          ~full_checkpoint:target ()
        |> Result.get_ok)
  in
  let apply_total =
    Metrics.counter metrics ~name:"oas.checkpoint.delta_apply_total" ~unit_:"1"
  in
  let fallback_total =
    Metrics.counter metrics ~name:"oas.checkpoint.full_restore_fallback_total" ~unit_:"1"
  in
  Alcotest.(check bool) "first fallback" true (first.mode = Checkpoint.Full_restore);
  Alcotest.(check bool) "gate fallback" true (second.mode = Checkpoint.Full_restore);
  Alcotest.(check int) "apply total stays at first failure" 1
    (Metrics.counter_value apply_total ());
  Alcotest.(check int) "fallback total counts both" 2
    (Metrics.counter_value fallback_total ())

let test_delta_enabled_env_var () =
  (* OAS_DELTA_CHECKPOINT enables *)
  with_env "OAS_DELTA_CHECKPOINT" (Some "1") (fun () ->
      Alcotest.(check bool) "OAS var enables" true (Checkpoint.delta_enabled ()));
  (* Not set: disabled *)
  with_env "OAS_DELTA_CHECKPOINT" None (fun () ->
      Alcotest.(check bool) "not set" false (Checkpoint.delta_enabled ()))

let () =
  Alcotest.run "Checkpoint_delta"
    [
      ( "properties",
        List.map QCheck_alcotest.to_alcotest [ test_delta_roundtrip_property ] );
      ( "unit",
        [
          Alcotest.test_case "delta JSON roundtrip" `Quick test_delta_json_roundtrip;
          Alcotest.test_case "delta JSON rejects malformed context removed" `Quick
            test_delta_json_rejects_malformed_context_removed;
          Alcotest.test_case "empty delta roundtrip" `Quick test_empty_delta_roundtrip;
          Alcotest.test_case "apply_delta rejects version/hash mismatch" `Quick
            test_apply_delta_rejects_version_and_hash_mismatch;
          Alcotest.test_case "apply_delta rejects invalid splice" `Quick
            test_apply_delta_rejects_invalid_splice;
          Alcotest.test_case "feature flag disabled falls back" `Quick
            test_restore_with_delta_fallback_disabled;
          Alcotest.test_case "delta env var" `Quick
            test_delta_enabled_env_var;
          Alcotest.test_case "bad delta records failure metrics" `Quick
            (with_eio test_restore_with_delta_fallback_records_failure_metrics);
          Alcotest.test_case "failure gate skips later delta path" `Quick
            (with_eio test_restore_with_delta_fallback_gate_skips_after_failure);
        ] );
    ]
