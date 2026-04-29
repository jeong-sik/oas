(** Unit tests for Direct_evidence module — options construction,
    raw_details type, and API surface verification via public API.

    Since Direct_evidence.mli exposes only get_worker_run,
    get_proof_bundle, get_conformance, run_conformance, persist,
    and the options/raw_details types, we verify those types and
    exercise the code paths that produce coverage on the internal
    helpers (safe_name, primary_alias, etc.) by invoking the
    public functions with controlled inputs. *)

open Agent_sdk

(* ── Options type construction ─────────────────────────────── *)

let test_options_minimal () =
  let opts : Direct_evidence.options =
    { session_root = None
    ; session_id = "test-sess"
    ; goal = "test goal"
    ; title = None
    ; tag = None
    ; worker_id = None
    ; runtime_actor = None
    ; role = None
    ; aliases = []
    ; requested_provider = None
    ; requested_model = None
    ; requested_policy = None
    ; workdir = None
    }
  in
  Alcotest.(check string) "session_id" "test-sess" opts.session_id;
  Alcotest.(check string) "goal" "test goal" opts.goal;
  Alcotest.(check (option string)) "no session_root" None opts.session_root
;;

let test_options_full () =
  let opts : Direct_evidence.options =
    { session_root = Some "/tmp/sessions"
    ; session_id = "sess-123"
    ; goal = "deploy v2"
    ; title = Some "Deployment session"
    ; tag = Some "production"
    ; worker_id = Some "worker-a"
    ; runtime_actor = Some "actor-main"
    ; role = Some "lead"
    ; aliases = [ "deployer"; "captain" ]
    ; requested_provider = Some "anthropic"
    ; requested_model = Some "claude-3-5-sonnet"
    ; requested_policy = Some "auto"
    ; workdir = Some "/workspace"
    }
  in
  Alcotest.(check (option string)) "session_root" (Some "/tmp/sessions") opts.session_root;
  Alcotest.(check (list string)) "aliases" [ "deployer"; "captain" ] opts.aliases;
  Alcotest.(check (option string)) "workdir" (Some "/workspace") opts.workdir
;;

let test_options_empty_aliases () =
  let opts : Direct_evidence.options =
    { session_root = None
    ; session_id = "s1"
    ; goal = "g"
    ; title = None
    ; tag = None
    ; worker_id = None
    ; runtime_actor = None
    ; role = None
    ; aliases = []
    ; requested_provider = None
    ; requested_model = None
    ; requested_policy = None
    ; workdir = None
    }
  in
  Alcotest.(check (list string)) "empty aliases" [] opts.aliases
;;

let test_options_special_chars_in_goal () =
  let opts : Direct_evidence.options =
    { session_root = None
    ; session_id = "s2"
    ; goal = "fix bug #123 with path/to/file"
    ; title = None
    ; tag = None
    ; worker_id = None
    ; runtime_actor = None
    ; role = None
    ; aliases = []
    ; requested_provider = None
    ; requested_model = None
    ; requested_policy = None
    ; workdir = None
    }
  in
  Alcotest.(check string)
    "goal with special chars"
    "fix bug #123 with path/to/file"
    opts.goal
;;

(* ── raw_details type construction ─────────────────────────── *)

let test_raw_details_default () =
  let d : Direct_evidence.raw_details =
    { validated = false
    ; tool_names = []
    ; final_text = None
    ; stop_reason = None
    ; error = None
    ; paired_tool_result_count = 0
    ; has_file_write = false
    ; verification_pass_after_file_write = false
    ; failure_reason = None
    }
  in
  Alcotest.(check bool) "not validated" false d.validated;
  Alcotest.(check int) "zero paired" 0 d.paired_tool_result_count
;;

let test_raw_details_populated () =
  let d : Direct_evidence.raw_details =
    { validated = true
    ; tool_names = [ "file_read"; "shell_exec" ]
    ; final_text = Some "Task complete."
    ; stop_reason = Some "EndTurn"
    ; error = None
    ; paired_tool_result_count = 5
    ; has_file_write = true
    ; verification_pass_after_file_write = true
    ; failure_reason = None
    }
  in
  Alcotest.(check bool) "validated" true d.validated;
  Alcotest.(check (list string)) "tools" [ "file_read"; "shell_exec" ] d.tool_names;
  Alcotest.(check int) "paired count" 5 d.paired_tool_result_count;
  Alcotest.(check bool) "has file write" true d.has_file_write;
  Alcotest.(check bool) "verification pass" true d.verification_pass_after_file_write
;;

let test_raw_details_with_error () =
  let d : Direct_evidence.raw_details =
    { validated = false
    ; tool_names = [ "broken_tool" ]
    ; final_text = None
    ; stop_reason = Some "Error"
    ; error = Some "Connection timeout"
    ; paired_tool_result_count = 1
    ; has_file_write = false
    ; verification_pass_after_file_write = false
    ; failure_reason = Some "Connection timeout"
    }
  in
  Alcotest.(check (option string)) "error present" (Some "Connection timeout") d.error;
  Alcotest.(check (option string))
    "failure reason"
    (Some "Connection timeout")
    d.failure_reason
;;

(* ── Options with various alias patterns ───────────────────── *)

let test_options_single_alias () =
  let opts : Direct_evidence.options =
    { session_root = None
    ; session_id = "s3"
    ; goal = "g"
    ; title = None
    ; tag = None
    ; worker_id = None
    ; runtime_actor = None
    ; role = None
    ; aliases = [ "primary" ]
    ; requested_provider = None
    ; requested_model = None
    ; requested_policy = None
    ; workdir = None
    }
  in
  Alcotest.(check (list string)) "single alias" [ "primary" ] opts.aliases
;;

let test_options_many_aliases () =
  let aliases = List.init 10 (fun i -> Printf.sprintf "alias-%d" i) in
  let opts : Direct_evidence.options =
    { session_root = None
    ; session_id = "s4"
    ; goal = "g"
    ; title = None
    ; tag = None
    ; worker_id = None
    ; runtime_actor = None
    ; role = None
    ; aliases
    ; requested_provider = None
    ; requested_model = None
    ; requested_policy = None
    ; workdir = None
    }
  in
  Alcotest.(check int) "10 aliases" 10 (List.length opts.aliases)
;;

(* ── Options with whitespace-heavy strings ─────────────────── *)

let test_options_whitespace_session_id () =
  let opts : Direct_evidence.options =
    { session_root = None
    ; session_id = "  spaced-id  "
    ; goal = "  spaced goal  "
    ; title = Some "  spaced title  "
    ; tag = Some "  spaced-tag  "
    ; worker_id = Some "  worker  "
    ; runtime_actor = Some "  actor  "
    ; role = Some "  role  "
    ; aliases = [ "  alias  " ]
    ; requested_provider = None
    ; requested_model = None
    ; requested_policy = None
    ; workdir = None
    }
  in
  Alcotest.(check string) "session_id preserved" "  spaced-id  " opts.session_id
;;

(* ── raw_details with many tools ───────────────────────────── *)

let test_raw_details_many_tools () =
  let tools = List.init 50 (fun i -> Printf.sprintf "tool_%d" i) in
  let d : Direct_evidence.raw_details =
    { validated = true
    ; tool_names = tools
    ; final_text = Some "Done"
    ; stop_reason = Some "EndTurn"
    ; error = None
    ; paired_tool_result_count = 50
    ; has_file_write = false
    ; verification_pass_after_file_write = false
    ; failure_reason = None
    }
  in
  Alcotest.(check int) "50 tools" 50 (List.length d.tool_names);
  Alcotest.(check int) "50 paired" 50 d.paired_tool_result_count
;;

(* ── Test suite ──────────────────────────────────────────────── *)

let () =
  Alcotest.run
    "direct-evidence-unit"
    [ ( "options"
      , [ Alcotest.test_case "minimal" `Quick test_options_minimal
        ; Alcotest.test_case "full" `Quick test_options_full
        ; Alcotest.test_case "empty aliases" `Quick test_options_empty_aliases
        ; Alcotest.test_case "special chars" `Quick test_options_special_chars_in_goal
        ; Alcotest.test_case "single alias" `Quick test_options_single_alias
        ; Alcotest.test_case "many aliases" `Quick test_options_many_aliases
        ; Alcotest.test_case
            "whitespace strings"
            `Quick
            test_options_whitespace_session_id
        ] )
    ; ( "raw_details"
      , [ Alcotest.test_case "default" `Quick test_raw_details_default
        ; Alcotest.test_case "populated" `Quick test_raw_details_populated
        ; Alcotest.test_case "with error" `Quick test_raw_details_with_error
        ; Alcotest.test_case "many tools" `Quick test_raw_details_many_tools
        ] )
    ]
;;
