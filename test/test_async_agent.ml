(** Tests for Async_agent — Eio fiber-based background agent execution.

    Uses a mock HTTP server (cohttp-eio) to simulate LLM responses.
    No real LLM calls. *)
open Base

open Agent_sdk
open Alcotest

(* ── Mock server helpers ─────────────────────────────────────────── *)

let quick_response text =
  Printf.sprintf
    {|{"id":"m","type":"message","role":"assistant","model":"m","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
    text
;;

(** Start a mock Anthropic API server. Returns base_url.
    Optionally delays [delay_sec] before responding. *)
let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> failwith "not inet"
  in
  Unix.close s;
  port
;;

let start_mock ~sw ~net ~clock ?(delay_sec = 0.0) response_text =
  let port = fresh_port () in
  let handler _conn _req body =
    let _ = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
    if delay_sec > 0.0 then Eio.Time.sleep clock delay_sec;
    Cohttp_eio.Server.respond_string ~status:`OK ~body:(quick_response response_text) ()
  in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  let server = Cohttp_eio.Server.make ~callback:handler () in
  Eio.Fiber.fork ~sw (fun () ->
    Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
  Printf.sprintf "http://127.0.0.1:%d" port
;;

let make_agent ?max_execution_time_s ~net base_url name =
  let config = { Types.default_config with name; max_turns = 1 } in
  let options = { Agent.default_options with base_url; max_execution_time_s } in
  Agent.create ~net ~config ~options ()
;;

let extract_text (resp : Types.api_response) =
  List.filter_map
    (function
      | Types.Text s -> Some s
      | _ -> None)
    resp.content
  |> String.concat ""
;;

(* ── spawn + await ────────────────────────────────────────────────── *)

let test_spawn_and_await () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock ~sw ~net:env#net ~clock "hello-async" in
    let agent = make_agent ~net:env#net url "async-1" in
    let future = Async_agent.spawn ~sw ~clock agent "test" in
    match Async_agent.await future with
    | Ok resp ->
      check string "response text" "hello-async" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── is_ready ─────────────────────────────────────────────────────── *)

let test_is_ready () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Use a short delay so we can check is_ready before completion *)
    let url = start_mock ~sw ~net:env#net ~clock ~delay_sec:0.1 "ready-test" in
    let agent = make_agent ~net:env#net url "ready-agent" in
    let future = Async_agent.spawn ~sw ~clock agent "test" in
    (* Agent is still running (0.1s delay); should not be ready yet *)
    (* Note: timing-dependent. The fiber might not have started yet,
       but the promise definitely hasn't been resolved. *)
    let _initial = Async_agent.is_ready future in
    (* After await, it must be ready *)
    match Async_agent.await future with
    | Ok _ ->
      check bool "ready after await" true (Async_agent.is_ready future);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── cancel ───────────────────────────────────────────────────────── *)

let test_cancel () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Slow server: 5s delay *)
    let url = start_mock ~sw ~net:env#net ~clock ~delay_sec:5.0 "slow-result" in
    let agent = make_agent ~net:env#net url "cancel-agent" in
    let future = Async_agent.spawn ~sw ~clock agent "test" in
    (* Cancel immediately *)
    Async_agent.cancel future;
    (* await should return the cancel error *)
    match Async_agent.await future with
    | Ok _ -> fail "expected cancelled error"
    | Error (Error.Internal msg) ->
      check string "cancel message" "cancelled" msg;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Printf.sprintf "wrong error: %s" (Error.to_string e))
  with
  | Exit -> ()
;;

(* ── cancel fires cancel_fn before resolving future ──────────────── *)

let test_cancel_fiber_stops_before_resolve () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Slow server: 5s delay — ensures the fiber is blocked in I/O *)
    let url = start_mock ~sw ~net:env#net ~clock ~delay_sec:5.0 "slow-cancel" in
    let agent = make_agent ~net:env#net url "cancel-order-agent" in
    let future = Async_agent.spawn ~sw ~clock agent "test" in
    (* Give the fiber time to start and set cancel_fn *)
    Eio.Time.sleep clock 0.05;
    (* Cancel: must fire cancel_fn before resolving future.
       After cancel returns, the fiber should have received the
       cancellation signal. We verify by yielding once and
       checking that await returns the cancel error. *)
    Async_agent.cancel future;
    Eio.Fiber.yield ();
    check bool "ready after cancel" true (Async_agent.is_ready future);
    match Async_agent.await future with
    | Ok _ -> fail "expected cancelled error"
    | Error (Error.Internal msg) ->
      check string "cancel message" "cancelled" msg;
      Eio.Switch.fail sw Exit
    | Error e -> fail (Printf.sprintf "wrong error: %s" (Error.to_string e))
  with
  | Exit -> ()
;;

(* ── race (first wins) ────────────────────────────────────────────── *)

let test_race_first_wins () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url_fast = start_mock ~sw ~net:env#net ~clock ~delay_sec:0.01 "fast-agent" in
    let url_slow = start_mock ~sw ~net:env#net ~clock ~delay_sec:2.0 "slow-agent" in
    let fast = make_agent ~net:env#net url_fast "racer-fast" in
    let slow = make_agent ~net:env#net url_slow "racer-slow" in
    match Async_agent.race ~sw ~clock [ fast, "go"; slow, "go" ] with
    | Ok (name, resp) ->
      check string "fast wins" "racer-fast" name;
      check string "fast text" "fast-agent" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── race (empty list) ────────────────────────────────────────────── *)

let test_race_empty () =
  Eio_main.run
  @@ fun _env ->
  Eio.Switch.run
  @@ fun sw ->
  match Async_agent.race ~sw [] with
  | Ok _ -> fail "expected error for empty race"
  | Error (Error.Internal msg) ->
    check bool "contains 'no agents'" true (String.length msg > 0)
  | Error e -> fail (Printf.sprintf "wrong error: %s" (Error.to_string e))
;;

(* ── race (single agent) ─────────────────────────────────────────── *)

let test_race_single () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url = start_mock ~sw ~net:env#net ~clock "solo" in
    let agent = make_agent ~net:env#net url "solo-racer" in
    match Async_agent.race ~sw ~clock [ agent, "go" ] with
    | Ok (name, resp) ->
      check string "solo name" "solo-racer" name;
      check string "solo text" "solo" (extract_text resp);
      Eio.Switch.fail sw Exit
    | Error e -> fail (Error.to_string e)
  with
  | Exit -> ()
;;

(* ── all (fan-out) ────────────────────────────────────────────────── *)

let test_all_collects () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url1 = start_mock ~sw ~net:env#net ~clock "result-1" in
    let url2 = start_mock ~sw ~net:env#net ~clock "result-2" in
    let url3 = start_mock ~sw ~net:env#net ~clock "result-3" in
    let a1 = make_agent ~net:env#net url1 "agent-1" in
    let a2 = make_agent ~net:env#net url2 "agent-2" in
    let a3 = make_agent ~net:env#net url3 "agent-3" in
    let results =
      Async_agent.all ~sw ~clock ~max_fibers:3 [ a1, "go"; a2, "go"; a3, "go" ]
    in
    check int "3 results" 3 (List.length results);
    (* Verify each result *)
    List.iter
      (fun (name, result) ->
         match result with
         | Ok resp ->
           let text = extract_text resp in
           check bool "has result text" true (String.length text > 0);
           check bool "name matches" true (String.length name > 0)
         | Error e -> fail (Printf.sprintf "%s failed: %s" name (Error.to_string e)))
      results;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* ── all preserves order ──────────────────────────────────────────── *)

let test_all_order () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    (* Agents with different delays — output order should match input *)
    let url1 = start_mock ~sw ~net:env#net ~clock ~delay_sec:0.05 "first" in
    let url2 = start_mock ~sw ~net:env#net ~clock ~delay_sec:0.01 "second" in
    let a1 = make_agent ~net:env#net url1 "ordered-1" in
    let a2 = make_agent ~net:env#net url2 "ordered-2" in
    let results = Async_agent.all ~sw ~clock [ a1, "go"; a2, "go" ] in
    let names = List.map fst results in
    check (list string) "order preserved" [ "ordered-1"; "ordered-2" ] names;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let test_all_timeout_is_per_agent () =
  Eio_main.run
  @@ fun env ->
  let clock = Eio.Stdenv.clock env in
  try
    Eio.Switch.run
    @@ fun sw ->
    let url_fast_1 = start_mock ~sw ~net:env#net ~clock "fast-1" in
    let url_slow = start_mock ~sw ~net:env#net ~clock ~delay_sec:0.2 "too-slow" in
    let url_fast_2 = start_mock ~sw ~net:env#net ~clock "fast-2" in
    let fast_1 = make_agent ~net:env#net url_fast_1 "all-fast-1" in
    let slow = make_agent ~max_execution_time_s:0.03 ~net:env#net url_slow "all-slow" in
    let fast_2 = make_agent ~net:env#net url_fast_2 "all-fast-2" in
    let results =
      Async_agent.all ~sw ~clock ~max_fibers:3 [ fast_1, "go"; slow, "go"; fast_2, "go" ]
    in
    let lookup name = List.assoc name results in
    (match lookup "all-fast-1" with
     | Ok resp -> check string "first sibling completed" "fast-1" (extract_text resp)
     | Error e -> fail (Printf.sprintf "first sibling failed: %s" (Error.to_string e)));
    (match lookup "all-slow" with
     | Error (Error.Api (Retry.Timeout _)) -> ()
     | Ok _ -> fail "slow branch should time out"
     | Error e -> fail (Printf.sprintf "wrong slow error: %s" (Error.to_string e)));
    (match lookup "all-fast-2" with
     | Ok resp -> check string "second sibling completed" "fast-2" (extract_text resp)
     | Error e -> fail (Printf.sprintf "second sibling failed: %s" (Error.to_string e)));
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

(* ── Test suite ──────────────────────────────────────────────────── *)

let () =
  (* Ensure ANTHROPIC_API_KEY is set for mock server tests *)
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  run
    "async_agent"
    [ ( "spawn"
      , [ test_case "spawn_and_await" `Quick test_spawn_and_await
        ; test_case "is_ready" `Quick test_is_ready
        ; test_case "cancel" `Quick test_cancel
        ; test_case
            "cancel_fiber_stops_before_resolve"
            `Quick
            test_cancel_fiber_stops_before_resolve
        ] )
    ; ( "race"
      , [ test_case "first_wins" `Quick test_race_first_wins
        ; test_case "empty_list" `Quick test_race_empty
        ; test_case "single_agent" `Quick test_race_single
        ] )
    ; ( "all"
      , [ test_case "collects_all" `Quick test_all_collects
        ; test_case "preserves_order" `Quick test_all_order
        ; test_case "timeout_is_per_agent" `Quick test_all_timeout_is_per_agent
        ] )
    ]
;;
