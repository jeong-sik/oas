(** Tests for Agent.save_journal and Builder.with_auto_dump_journal. *)

open Alcotest
open Agent_sdk

let ts = 1711234567.0

let test_save_journal_writes_jsonl () =
  Eio_main.run
  @@ fun env ->
  let journal = Durable_event.create () in
  Durable_event.append journal (Turn_started { turn = 1; timestamp = ts });
  Durable_event.append
    journal
    (Llm_request { turn = 1; model = "m"; input_tokens = 10; timestamp = ts });
  let path = Filename.temp_file "auto_dump" ".jsonl" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let net = Eio.Stdenv.net env in
       let agent =
         Builder.create ~net ~model:"test"
         |> Builder.with_journal journal
         |> Builder.build
       in
       match Agent.save_journal agent path with
       | Error e -> fail (Printf.sprintf "save_journal failed: %s" e)
       | Ok () ->
         (match Durable_event.load_from_file path with
          | Error e -> fail (Printf.sprintf "load failed: %s" e)
          | Ok j' -> check int "length" 2 (Durable_event.length j')))
;;

let test_save_journal_no_journal_returns_error () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  let agent = Builder.create ~net ~model:"test" |> Builder.build in
  match Agent.save_journal agent "/tmp/should-never-exist" with
  | Ok () -> fail "expected Error when agent has no journal"
  | Error msg -> check string "error message" "no journal" msg
;;

let test_auto_dump_installs_callback () =
  Eio_main.run
  @@ fun env ->
  let path = Filename.temp_file "auto_dump_cb" ".jsonl" in
  Fun.protect
    ~finally:(fun () ->
      try Sys.remove path with
      | _ -> ())
    (fun () ->
       let net = Eio.Stdenv.net env in
       let agent =
         Builder.create ~net ~model:"test"
         |> Builder.with_auto_dump_journal ~path
         |> Builder.build
       in
       let opts = Agent.options agent in
       check bool "journal attached" true (Option.is_some opts.journal);
       check bool "on_run_complete set" true (Option.is_some opts.on_run_complete);
       (* Simulate run completion — append an event then invoke callback. *)
       (match opts.journal with
        | Some j -> Durable_event.append j (Turn_started { turn = 1; timestamp = ts })
        | None -> fail "journal missing");
       (match opts.on_run_complete with
        | Some cb -> cb true
        | None -> fail "callback missing");
       (* File should now exist with the event. *)
       match Durable_event.load_from_file path with
       | Error e -> fail (Printf.sprintf "load failed: %s" e)
       | Ok j' -> check int "length" 1 (Durable_event.length j'))
;;

let () =
  run
    "Agent auto-dump"
    [ ( "save_journal"
      , [ test_case "writes journal to file" `Quick test_save_journal_writes_jsonl
        ; test_case
            "errors without journal"
            `Quick
            test_save_journal_no_journal_returns_error
        ] )
    ; ( "with_auto_dump_journal"
      , [ test_case
            "installs on_run_complete callback"
            `Quick
            test_auto_dump_installs_callback
        ] )
    ]
;;
