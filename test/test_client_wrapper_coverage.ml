open Agent_sdk
module C = Client

let check_bool = Alcotest.(check bool)
let check_opt_string = Alcotest.(check (option string))
let check_show label rendered = check_bool label true (String.length rendered > 0)

let minimal_session : Runtime.session =
  { session_id = "session-client-wrapper"
  ; goal = "cover client wrapper"
  ; title = Some "Client wrapper"
  ; tag = Some "coverage"
  ; permission_mode = Some "default"
  ; phase = Runtime.Running
  ; created_at = 1.0
  ; updated_at = 2.0
  ; provider = Some "mock"
  ; model = Some "mock-model"
  ; system_prompt = None
  ; max_turns = 2
  ; workdir = None
  ; planned_participants = [ "planner" ]
  ; participants = []
  ; artifacts = []
  ; pending_input = None
  ; turn_count = 0
  ; last_seq = 0
  ; outcome = None
  }
;;

let test_client_show_reexported_types () =
  check_show "default" (C.show_permission_mode C.Default);
  check_show "accept" (C.show_permission_mode C.Accept_edits);
  check_show "plan" (C.show_permission_mode C.Plan);
  check_show "bypass" (C.show_permission_mode C.Bypass_permissions);
  check_show "user" (C.show_setting_source C.User);
  check_show "project" (C.show_setting_source C.Project);
  check_show "local" (C.show_setting_source C.Local);
  let agent : C.agent_definition =
    { description = "planner"; prompt = "plan"; tools = Some [ "read" ]; model = None }
  in
  check_bool
    "agent definition show"
    true
    (String.length (C.show_agent_definition agent) > 0);
  check_bool "options show" true (String.length (C.show_options C.default_options) > 0)
;;

let test_client_show_message_variants () =
  let messages : C.message list =
    [ C.System_message "system"
    ; C.Partial_message { participant_name = "planner"; delta = "partial" }
    ; C.Session_status minimal_session
    ; C.Session_events []
    ; C.Session_report
        { session_id = minimal_session.session_id
        ; summary = [ "done" ]
        ; markdown = "# done"
        ; generated_at = 3.0
        }
    ; C.Session_proof
        { session_id = minimal_session.session_id
        ; ok = true
        ; checks = [ { name = "covered"; passed = true } ]
        ; evidence = [ "test" ]
        ; generated_at = 4.0
        }
    ]
  in
  List.iter
    (fun msg ->
       check_bool "message show non-empty" true (String.length (C.show_message msg) > 0))
    messages
;;

let test_client_default_options_alias () =
  check_opt_string "default provider" (Some "local") C.default_options.provider;
  check_bool "default max turns" true (C.default_options.max_turns = Some 8);
  check_bool "default setting sources" true (C.default_options.setting_sources = []);
  check_bool "default no resume" true (Option.is_none C.default_options.resume_session)
;;

let () =
  Alcotest.run
    "Client_wrapper_coverage"
    [ ( "show"
      , [ Alcotest.test_case "reexported types" `Quick test_client_show_reexported_types
        ; Alcotest.test_case "message variants" `Quick test_client_show_message_variants
        ] )
    ; ( "defaults"
      , [ Alcotest.test_case
            "default options alias"
            `Quick
            test_client_default_options_alias
        ] )
    ]
;;
