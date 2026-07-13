(** Tests for Agent_registry — capability-based multi-agent discovery. *)

open Alcotest
open Agent_sdk

let make_agent () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Agent.create ~config:(Types.default_config ~model:"test-model") ~net ()
;;

let fresh_port () =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt s Unix.SO_REUSEADDR true;
  Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname s with
    | Unix.ADDR_INET (_, p) -> p
    | _ -> fail "expected TCP socket"
  in
  Unix.close s;
  port
;;

let request_path request_line =
  match String.split_on_char ' ' request_line with
  | _method_ :: path :: _ -> path
  | _ -> request_line
;;

let header_value line =
  match String.index_opt line ':' with
  | None -> None
  | Some idx ->
    let value_start = idx + 1 in
    let value_len = String.length line - value_start in
    Some (String.trim (String.sub line value_start value_len))
;;

let start_card_server ~sw ~net card =
  let port = fresh_port () in
  let seen_path = ref None in
  let seen_connection = ref None in
  let socket =
    Eio.Net.listen
      net
      ~sw
      ~backlog:128
      ~reuse_addr:true
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Fiber.fork ~sw (fun () ->
    Eio.Net.accept_fork
      ~sw
      socket
      ~on_error:(fun _ -> ())
      (fun flow _addr ->
         let reader = Eio.Buf_read.of_flow flow ~max_size:8192 in
         let request_line = Eio.Buf_read.line reader in
         seen_path := Some (request_path request_line);
         let rec read_headers () =
           match Eio.Buf_read.line reader with
           | "" -> ()
           | line ->
             if String.starts_with ~prefix:"connection:" (String.lowercase_ascii line)
             then seen_connection := Option.map String.lowercase_ascii (header_value line);
             read_headers ()
           | exception End_of_file -> ()
         in
         read_headers ();
         let body = Agent_card.to_json card |> Yojson.Safe.to_string in
         let response =
           Printf.sprintf
             "HTTP/1.1 200 OK\r\n\
              Content-Length: %d\r\n\
              Content-Type: application/json\r\n\
              Connection: close\r\n\
              \r\n\
              %s"
             (String.length body)
             body
         in
         Eio.Flow.copy_string response flow));
  Printf.sprintf "http://127.0.0.1:%d" port, seen_path, seen_connection
;;

(* ── Registration and lookup ────────────────────────────── *)

let test_register_local () =
  let reg = Agent_registry.create_sync () in
  let agent = make_agent () in
  Agent_registry.register_local reg ~name:"test-agent" agent;
  match Agent_registry.lookup reg "test-agent" with
  | Some (Local { agent = _; card }) ->
    check string "card name" "agent" card.name (* default agent name *)
  | Some (Remote _) -> fail "expected local"
  | None -> fail "not found"
;;

let test_register_remote () =
  let reg = Agent_registry.create_sync () in
  let card : Agent_card.agent_card =
    { name = "remote-agent"
    ; description = Some "A remote agent"
    ; protocol_version = "1.0"
    ; version = "1.0.0"
    ; url = Some "http://example.com"
    ; authentication = None
    ; supported_interfaces =
        [ { url = "http://example.com"
          ; protocol_binding = "JSONRPC"
          ; protocol_version = "1.0"
          ; tenant = None
          }
        ]
    ; capabilities = [ Tools; Streaming ]
    ; tools = []
    ; skills = []
    ; supported_providers = [ "anthropic" ]
    ; metadata = []
    }
  in
  Agent_registry.register_remote reg ~name:"remote" ~url:"http://example.com" card;
  match Agent_registry.lookup reg "remote" with
  | Some (Remote { url; card = c }) ->
    check string "url" "http://example.com" url;
    check string "card name" "remote-agent" c.name
  | Some (Local _) -> fail "expected remote"
  | None -> fail "not found"
;;

let test_lookup_missing () =
  let reg = Agent_registry.create_sync () in
  check bool "none" true (Option.is_none (Agent_registry.lookup reg "nope"))
;;

(* ── List operations ────────────────────────────────────── *)

let test_list_all () =
  let reg = Agent_registry.create_sync () in
  let agent = make_agent () in
  Agent_registry.register_local reg ~name:"a" agent;
  Agent_registry.register_local reg ~name:"b" agent;
  let all = Agent_registry.list_all reg in
  check int "count" 2 (List.length all)
;;

let test_count () =
  let reg = Agent_registry.create_sync () in
  check int "empty" 0 (Agent_registry.count reg);
  let agent = make_agent () in
  Agent_registry.register_local reg ~name:"x" agent;
  check int "one" 1 (Agent_registry.count reg)
;;

(* ── Capability filtering ───────────────────────────────── *)

let test_list_by_capability () =
  let reg = Agent_registry.create_sync () in
  let card_tools : Agent_card.agent_card =
    { name = "tools-agent"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = [ Tools ]
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let card_thinking : Agent_card.agent_card =
    { name = "think-agent"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = [ Thinking ]
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  Agent_registry.register_remote reg ~name:"tools" ~url:"http://t.com" card_tools;
  Agent_registry.register_remote reg ~name:"think" ~url:"http://k.com" card_thinking;
  let tools_agents = Agent_registry.list_by_capability reg Tools in
  check int "tools agents" 1 (List.length tools_agents);
  check string "name" "tools" (fst (List.hd tools_agents))
;;

(* ── Tool filtering ─────────────────────────────────────── *)

let test_list_by_tool () =
  let reg = Agent_registry.create_sync () in
  let card : Agent_card.agent_card =
    { name = "tool-owner"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = [ Tools ]
    ; tools =
        [ { name = "get_weather"
          ; description = "Weather"
          ; parameters = []
          ; strict = None
          }
        ]
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  Agent_registry.register_remote reg ~name:"weather" ~url:"http://w.com" card;
  let results = Agent_registry.list_by_tool reg "get_weather" in
  check int "found" 1 (List.length results);
  let not_found = Agent_registry.list_by_tool reg "nonexistent" in
  check int "not found" 0 (List.length not_found)
;;

(* ── Unregister ─────────────────────────────────────────── *)

let test_unregister () =
  let reg = Agent_registry.create_sync () in
  let agent = make_agent () in
  Agent_registry.register_local reg ~name:"temp" agent;
  check int "before" 1 (Agent_registry.count reg);
  Agent_registry.unregister reg "temp";
  check int "after" 0 (Agent_registry.count reg)
;;

(* ── card_of_entry ──────────────────────────────────────── *)

let test_card_of_entry () =
  let card : Agent_card.agent_card =
    { name = "test"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = []
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let entry = Agent_registry.Remote { url = "http://x.com"; card } in
  let extracted = Agent_registry.card_of_entry entry in
  check string "name" "test" extracted.name
;;

(* ── Discovery (unreachable) ────────────────────────────── *)

let test_discover_unreachable () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let reg = Agent_registry.create_sync () in
  match
    Agent_registry.discover_and_register
      ~sw
      ~net
      reg
      ~name:"remote"
      ~url:"http://127.0.0.1:19999"
  with
  | Error _ -> () (* expected *)
  | Ok () -> fail "should fail for unreachable"
;;

let test_fetch_card_unreachable () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  match Agent_registry.fetch_remote_card ~sw ~net "http://127.0.0.1:19999" with
  | Error _ -> () (* expected *)
  | Ok _ -> fail "should fail for unreachable"
;;

let test_fetch_card_http_client () =
  Eio_main.run
  @@ fun env ->
  let net = Eio.Stdenv.net env in
  Eio.Switch.run
  @@ fun sw ->
  let card : Agent_card.agent_card =
    { name = "remote-agent"
    ; description = Some "A remote agent"
    ; protocol_version = "1.0"
    ; version = "1.0.0"
    ; url = Some "http://example.com"
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = [ Tools ]
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let base_url, seen_path, seen_connection = start_card_server ~sw ~net card in
  match Agent_registry.fetch_remote_card ~sw ~net base_url with
  | Error err -> failf "fetch failed: %s" (Error.to_string err)
  | Ok fetched ->
    check string "card name" "remote-agent" fetched.name;
    check (option string) "path" (Some "/.well-known/agent.json") !seen_path;
    check (option string) "connection close" (Some "close") !seen_connection
;;

(* ── Overwrite registration ─────────────────────────────── *)

let test_overwrite () =
  let reg = Agent_registry.create_sync () in
  let card1 : Agent_card.agent_card =
    { name = "v1"
    ; description = None
    ; protocol_version = "1.0"
    ; version = "1.0"
    ; url = None
    ; authentication = None
    ; supported_interfaces = []
    ; capabilities = []
    ; tools = []
    ; skills = []
    ; supported_providers = []
    ; metadata = []
    }
  in
  let card2 = { card1 with name = "v2" } in
  Agent_registry.register_remote reg ~name:"svc" ~url:"http://a.com" card1;
  Agent_registry.register_remote reg ~name:"svc" ~url:"http://b.com" card2;
  check int "still 1" 1 (Agent_registry.count reg);
  match Agent_registry.lookup reg "svc" with
  | Some entry -> check string "updated" "v2" (Agent_registry.card_of_entry entry).name
  | None -> fail "not found"
;;

(* ── Suite ──────────────────────────────────────────────── *)

let () =
  run
    "Agent_registry"
    [ ( "registration"
      , [ test_case "register local" `Quick test_register_local
        ; test_case "register remote" `Quick test_register_remote
        ; test_case "lookup missing" `Quick test_lookup_missing
        ; test_case "overwrite" `Quick test_overwrite
        ] )
    ; ( "listing"
      , [ test_case "list all" `Quick test_list_all
        ; test_case "count" `Quick test_count
        ; test_case "by capability" `Quick test_list_by_capability
        ; test_case "by tool" `Quick test_list_by_tool
        ] )
    ; ( "lifecycle"
      , [ test_case "unregister" `Quick test_unregister
        ; test_case "card_of_entry" `Quick test_card_of_entry
        ] )
    ; ( "discovery"
      , [ test_case "unreachable" `Quick test_discover_unreachable
        ; test_case "fetch card unreachable" `Quick test_fetch_card_unreachable
        ; test_case "fetch card via http client" `Quick test_fetch_card_http_client
        ] )
    ]
;;
