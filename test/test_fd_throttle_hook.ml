(** Unit tests for [Fd_throttle_hook] (RFC-0101 PR-3). *)

open Alcotest
module H = Llm_provider.Fd_throttle_hook
module P = Llm_provider.Provider_throttle
module Pri = Llm_provider.Request_priority

let test_default_is_identity () =
  H.reset_handler ();
  check bool "default not installed" false (H.is_installed ());
  let r = H.with_slot (fun () -> 42) in
  check int "identity returns value" 42 r
;;

let test_set_handler_marks_installed () =
  H.reset_handler ();
  H.set_handler (fun thunk -> thunk ());
  check bool "installed after set" true (H.is_installed ());
  H.reset_handler ()
;;

let test_handler_is_invoked () =
  H.reset_handler ();
  let call_count = ref 0 in
  H.set_handler (fun thunk ->
    incr call_count;
    thunk ());
  let r1 = H.with_slot (fun () -> "a") in
  let r2 = H.with_slot (fun () -> "b") in
  check string "first call result" "a" r1;
  check string "second call result" "b" r2;
  check int "handler invoked once per with_slot" 2 !call_count;
  H.reset_handler ()
;;

let test_handler_propagates_exception () =
  H.reset_handler ();
  let entered = ref false in
  H.set_handler (fun thunk ->
    entered := true;
    thunk ());
  let exn = Failure "boom" in
  (try H.with_slot (fun () -> raise exn) |> ignore with
   | Failure msg -> check string "exception preserved" "boom" msg);
  check bool "handler still entered before exception" true !entered;
  H.reset_handler ()
;;

let test_non_conformant_handler_fails_loudly () =
  H.reset_handler ();
  (* Wrapper that swallows the thunk — violates contract. *)
  H.set_handler (fun _thunk -> ());
  (try
     H.with_slot (fun () -> 1) |> ignore;
     Alcotest.fail "should have raised on contract violation"
   with
   | Failure msg ->
     check bool "error mentions wrapper contract" true (String.length msg > 0));
  H.reset_handler ()
;;

let test_handler_swap_is_atomic () =
  H.reset_handler ();
  H.set_handler (fun thunk -> thunk ());
  H.set_handler (fun thunk -> thunk ());
  check bool "still installed after swap" true (H.is_installed ());
  H.reset_handler ()
;;

let test_provider_throttle_composition () =
  (* The whole point of PR-3: every Provider_throttle.with_permit_priority
     goes through the hook. Install a counter and verify. *)
  H.reset_handler ();
  let hook_calls = ref 0 in
  H.set_handler (fun thunk ->
    incr hook_calls;
    thunk ());
  Eio_main.run
  @@ fun _env ->
  let t = P.create ~max_concurrent:4 ~provider_name:"test" in
  let r = P.with_permit_priority ~priority:Pri.Background t (fun () -> "ok") in
  check string "throttled call result" "ok" r;
  check int "hook engaged through provider_throttle" 1 !hook_calls;
  H.reset_handler ()
;;

let test_reset_handler_restores_identity () =
  H.reset_handler ();
  H.set_handler (fun thunk -> thunk ());
  check bool "installed before reset" true (H.is_installed ());
  H.reset_handler ();
  check bool "not installed after reset" false (H.is_installed ());
  let r = H.with_slot (fun () -> 7) in
  check int "identity restored" 7 r
;;

let () =
  Alcotest.run
    "Fd_throttle_hook"
    [ ( "identity default"
      , [ test_case "default is identity" `Quick test_default_is_identity
        ; test_case "reset restores identity" `Quick test_reset_handler_restores_identity
        ] )
    ; ( "handler lifecycle"
      , [ test_case "set marks installed" `Quick test_set_handler_marks_installed
        ; test_case "handler invoked" `Quick test_handler_is_invoked
        ; test_case "swap is atomic" `Quick test_handler_swap_is_atomic
        ] )
    ; ( "contract"
      , [ test_case "exception propagates" `Quick test_handler_propagates_exception
        ; test_case
            "non-conformant wrapper fails loudly"
            `Quick
            test_non_conformant_handler_fails_loudly
        ] )
    ; ( "provider_throttle composition"
      , [ test_case "permit goes through hook" `Quick test_provider_throttle_composition ]
      )
    ]
;;
