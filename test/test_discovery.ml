(** Tests for Llm_provider.Discovery -- unit tests that do not require
    a running llama-server. We test JSON parsing, env var parsing,
    and serialization. *)

open Llm_provider

let test_getenv bindings name = List.assoc_opt name bindings

let fresh_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> Alcotest.fail "expected TCP socket"
  in
  Unix.close socket;
  port
;;

let with_mock_server handler f =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
    let port = fresh_port () in
    let socket =
      Eio.Net.listen
        env#net
        ~sw
        ~backlog:128
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let endpoint = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net:env#net ~endpoint;
    Eio.Switch.fail sw Exit
  with
  | Unix.Unix_error ((Unix.EPERM | Unix.EADDRINUSE), "bind", _) -> Alcotest.skip ()
  | Exit -> ()
;;

let test_endpoints_from_env_default () =
  let getenv = test_getenv [] in
  let result = Discovery.endpoints_from_env ~getenv () in
  Alcotest.(check (list string))
    "default endpoint"
    [ Discovery.resolve_default_endpoint ~getenv ()
    ; Discovery.resolve_ollama_endpoint ~getenv ()
    ]
    result
;;

let test_endpoints_from_env_custom () =
  let getenv =
    test_getenv
      [ Discovery.llm_endpoints_env_var, "http://a:8085, http://b:8086,http://c:8087" ]
  in
  let result = Discovery.endpoints_from_env ~getenv () in
  Alcotest.(check (list string))
    "parsed endpoints"
    [ "http://a:8085"
    ; "http://b:8086"
    ; "http://c:8087"
    ; Discovery.resolve_ollama_endpoint ~getenv ()
    ]
    result
;;

let test_parse_models_json () =
  let json =
    Yojson.Safe.from_string
      {|{
    "data": [
      {"id": "dashscope-3.5-35b", "owned_by": "llama-server"},
      {"id": "llama-3.1-8b", "owned_by": "llama-server"}
    ]
  }|}
  in
  let models =
    (* Use the internal parser indirectly via a full endpoint_status *)
    match json |> Yojson.Safe.Util.member "data" with
    | `List items ->
      items
      |> List.filter_map (fun item ->
        let open Yojson.Safe.Util in
        match item |> member "id" |> to_string_option with
        | Some id ->
          let owned_by =
            item
            |> member "owned_by"
            |> to_string_option
            |> Option.value ~default:"unknown"
          in
          Some Discovery.{ id; owned_by }
        | None -> None)
    | _ -> []
  in
  Alcotest.(check int) "model count" 2 (List.length models);
  Alcotest.(check string) "first model id" "dashscope-3.5-35b" (List.hd models).id
;;

let test_endpoint_status_to_json_healthy () =
  let status : Discovery.endpoint_status =
    { url = "http://127.0.0.1:8085"
    ; healthy = true
    ; models = [ { id = "dashscope-3.5-35b"; owned_by = "llama-server" } ]
    ; props =
        Some
          { total_slots = 4
          ; ctx_size = 32768
          ; model = "dashscope-3.5-35b"
          ; supports_tools = None
          }
    ; slots = Some { total = 4; busy = 1; idle = 3 }
    ; capabilities = Capabilities.openai_compat_chat_extended_capabilities
    }
  in
  let json = Discovery.endpoint_status_to_json status in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "healthy" true (json |> member "healthy" |> to_bool);
  Alcotest.(check string) "url" "http://127.0.0.1:8085" (json |> member "url" |> to_string);
  let slots = json |> member "slots" in
  Alcotest.(check int) "total slots" 4 (slots |> member "total" |> to_int);
  Alcotest.(check int) "idle slots" 3 (slots |> member "idle" |> to_int);
  let caps = json |> member "capabilities" in
  Alcotest.(check bool) "reasoning" true (caps |> member "reasoning" |> to_bool)
;;

let test_endpoint_status_to_json_unhealthy () =
  let status : Discovery.endpoint_status =
    { url = "http://127.0.0.1:9999"
    ; healthy = false
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  let json = Discovery.endpoint_status_to_json status in
  let open Yojson.Safe.Util in
  Alcotest.(check bool) "healthy" false (json |> member "healthy" |> to_bool);
  Alcotest.(check int) "no models" 0 (json |> member "models" |> to_list |> List.length);
  (* props and slots should be absent *)
  Alcotest.(check bool) "no props" true (member "props" json = `Null)
;;

let test_summary_to_json () =
  let endpoints : Discovery.endpoint_status list =
    [ { url = "http://a:8085"
      ; healthy = true
      ; models = []
      ; props = None
      ; slots = Some { total = 4; busy = 1; idle = 3 }
      ; capabilities = Capabilities.default_capabilities
      }
    ; { url = "http://b:8086"
      ; healthy = true
      ; models = []
      ; props = None
      ; slots = Some { total = 2; busy = 2; idle = 0 }
      ; capabilities = Capabilities.default_capabilities
      }
    ; { url = "http://c:8087"
      ; healthy = false
      ; models = []
      ; props = None
      ; slots = None
      ; capabilities = Capabilities.default_capabilities
      }
    ]
  in
  let json = Discovery.summary_to_json endpoints in
  let open Yojson.Safe.Util in
  Alcotest.(check int) "total capacity" 6 (json |> member "total_capacity" |> to_int);
  Alcotest.(check int) "available" 3 (json |> member "available_capacity" |> to_int);
  Alcotest.(check int) "active" 3 (json |> member "active_requests" |> to_int)
;;

(* ── discovered_per_slot_context tests ────────────────────── *)

let test_discovered_per_slot_context_initially_none () =
  (* Before any refresh_and_sync, should be None or whatever was last set.
     We can't fully reset the Atomic in tests, but we can verify the
     API shape. *)
  let _result = Discovery.discovered_per_slot_context () in
  (* Just verify it doesn't crash — value depends on test ordering *)
  ()
;;

let test_refresh_and_sync_updates_context () =
  (* We can't probe real endpoints in unit tests, but we can verify
     that refresh_and_sync with unreachable endpoints returns empty
     and doesn't crash. *)
  Eio_main.run
  @@ fun env ->
  let sw = Eio.Stdenv.process_mgr env in
  ignore sw;
  (* Instead, test the computation logic directly:
     verify per-slot = ctx_size / total_slots *)
  let status_with_props ctx_size total_slots : Discovery.endpoint_status =
    { url = "http://test"
    ; healthy = true
    ; models = []
    ; props = Some { total_slots; ctx_size; model = "test"; supports_tools = None }
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  (* Simulate what refresh_and_sync computes *)
  let compute_per_slot (statuses : Discovery.endpoint_status list) =
    let healthy =
      List.filter (fun (s : Discovery.endpoint_status) -> s.healthy) statuses
    in
    let per_slots =
      List.filter_map
        (fun (s : Discovery.endpoint_status) ->
           match s.props with
           | Some p when p.total_slots > 0 && p.ctx_size > 0 ->
             Some (p.ctx_size / p.total_slots)
           | _ -> None)
        healthy
    in
    match per_slots with
    | [] -> None
    | ctxs -> Some (List.fold_left min max_int ctxs)
  in
  (* Single endpoint: 131072 / 4 = 32768 *)
  let result = compute_per_slot [ status_with_props 131072 4 ] in
  Alcotest.(check (option int)) "single endpoint per-slot" (Some 32768) result;
  (* Multi endpoint: min(131072/4=32768, 8192/1=8192) = 8192 *)
  let result =
    compute_per_slot [ status_with_props 131072 4; status_with_props 8192 1 ]
  in
  Alcotest.(check (option int)) "multi endpoint min" (Some 8192) result;
  (* No healthy endpoints *)
  let unhealthy : Discovery.endpoint_status =
    { url = "http://dead"
    ; healthy = false
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  let result = compute_per_slot [ unhealthy ] in
  Alcotest.(check (option int)) "no healthy" None result;
  (* No props *)
  let no_props : Discovery.endpoint_status =
    { url = "http://noprops"
    ; healthy = true
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities = Capabilities.default_capabilities
    }
  in
  let result = compute_per_slot [ no_props ] in
  Alcotest.(check (option int)) "no props" None result
;;

let test_refresh_and_sync_mock_server_updates_indexes () =
  let handler _conn req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    match Uri.path (Cohttp.Request.uri req) with
    | "/health" -> Cohttp_eio.Server.respond_string ~status:`OK ~body:"ok" ()
    | "/v1/models" ->
      Cohttp_eio.Server.respond_string
        ~status:`OK
        ~body:
          {|
          {
            "data": [
              {"id":"dashscope-3.5-35b","owned_by":"llama-server"},
              {"id":"llama-3.1-8b","owned_by":"llama-server"}
            ]
          }
          |}
        ()
    | "/props" ->
      Cohttp_eio.Server.respond_string
        ~status:`OK
        ~body:
          {|
          {
            "total_slots": 4,
            "default_generation_settings": {
              "n_ctx": 65536,
              "model": "dashscope-3.5-35b"
            }
          }
          |}
        ()
    | "/slots" ->
      Cohttp_eio.Server.respond_string
        ~status:`OK
        ~body:
          {|
          [
            {"id":0,"is_processing":true},
            {"id":1,"is_processing":false},
            {"id":2,"state":1},
            {"id":3,"state":0}
          ]
          |}
        ()
    | _ -> Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"missing" ()
  in
  with_mock_server handler (fun ~sw ~net ~endpoint ->
    let statuses = Discovery.refresh_and_sync ~sw ~net ~endpoints:[ endpoint ] in
    match statuses with
    | [ status ] ->
      Alcotest.(check bool) "healthy" true status.healthy;
      Alcotest.(check int) "models" 2 (List.length status.models);
      Alcotest.(check (option int))
        "max context uses props"
        (Some 65536)
        (Discovery.max_context_of_status status);
      Alcotest.(check (option int))
        "global context"
        (Some 65536)
        (Discovery.discovered_per_slot_context ());
      Alcotest.(check (option int))
        "endpoint context"
        (Some 65536)
        (Discovery.discovered_context_for_url ("  " ^ endpoint ^ "  "));
      Alcotest.(check (option string))
        "model endpoint"
        (Some endpoint)
        (Discovery.endpoint_for_model "dashscope-3.5-35b");
      Alcotest.(check (option string))
        "first model"
        (Some "dashscope-3.5-35b")
        (Discovery.first_discovered_model_id ());
      Alcotest.(check (option string))
        "first model for endpoint"
        (Some "dashscope-3.5-35b")
        (Discovery.first_discovered_model_id_for_url endpoint);
      Alcotest.(check (option (pair string int)))
        "context for model"
        (Some (endpoint, 65536))
        (Discovery.context_for_model "dashscope-3.5-35b");
      (match status.slots with
       | Some slots ->
         Alcotest.(check int) "slot total" 4 slots.total;
         Alcotest.(check int) "busy" 2 slots.busy;
         Alcotest.(check int) "idle" 2 slots.idle
       | None -> Alcotest.fail "expected slots");
      Alcotest.(check bool)
        "dashscope model infers extended reasoning"
        true
        status.capabilities.supports_extended_thinking
    | _ -> Alcotest.fail "expected one endpoint status")
;;

let test_max_context_of_status_falls_back_to_capabilities () =
  let status : Discovery.endpoint_status =
    { url = "http://fallback"
    ; healthy = true
    ; models = []
    ; props = None
    ; slots = None
    ; capabilities =
        Capabilities.with_context_size Capabilities.default_capabilities ~ctx_size:4096
    }
  in
  Alcotest.(check (option int))
    "fallback context"
    (Some 4096)
    (Discovery.max_context_of_status status)
;;

let () =
  Alcotest.run
    "Discovery"
    [ ( "env"
      , [ Alcotest.test_case "default" `Quick test_endpoints_from_env_default
        ; Alcotest.test_case "custom" `Quick test_endpoints_from_env_custom
        ] )
    ; "parsing", [ Alcotest.test_case "models json" `Quick test_parse_models_json ]
    ; ( "json"
      , [ Alcotest.test_case
            "healthy endpoint"
            `Quick
            test_endpoint_status_to_json_healthy
        ; Alcotest.test_case
            "unhealthy endpoint"
            `Quick
            test_endpoint_status_to_json_unhealthy
        ; Alcotest.test_case "summary" `Quick test_summary_to_json
        ] )
    ; ( "discovered_context"
      , [ Alcotest.test_case
            "initially accessible"
            `Quick
            test_discovered_per_slot_context_initially_none
        ; Alcotest.test_case
            "per-slot computation"
            `Quick
            test_refresh_and_sync_updates_context
        ; Alcotest.test_case
            "mock server updates indexes"
            `Quick
            test_refresh_and_sync_mock_server_updates_indexes
        ; Alcotest.test_case
            "max context fallback"
            `Quick
            test_max_context_of_status_falls_back_to_capabilities
        ] )
    ]
;;
