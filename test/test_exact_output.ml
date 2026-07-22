open Alcotest
open Llm_provider
open Types

let msg role content : message =
  { role; content; name = None; tool_call_id = None; metadata = [] }
;;

let tool =
  `Assoc
    [ "name", `String "inspect"
    ; "description", `String "Inspect one artifact"
    ; ( "input_schema"
      , `Assoc
          [ "type", `String "object"
          ; "properties", `Assoc [ "path", `Assoc [ "type", `String "string" ] ]
          ] )
    ]
;;

let config
      ?(kind = Provider_config.Anthropic)
      ?(request_path = "/proxy/messages")
      ?max_context
      ?max_concurrent_requests
      base_url
  =
  Provider_config.make
    ~kind
    ~model_id:"input-count-fixture"
    ~base_url
    ~api_key:"test-key"
    ~headers:[ "Content-Type", "application/json"; "anthropic-version", "2023-06-01" ]
    ~request_path
    ~max_tokens:64
    ?max_context
    ~temperature:0.2
    ~top_p:0.8
    ~top_k:40
    ~system_prompt:"Count the exact projected input."
    ~cache_system_prompt:true
    ~tool_choice:Any
    ~disable_parallel_tool_use:true
    ~supports_tool_choice_override:true
    ~output_schema:(`Assoc [ "type", `String "object" ])
    ?max_concurrent_requests
    ()
;;

let fresh_port () =
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt socket Unix.SO_REUSEADDR true;
  Unix.bind socket (Unix.ADDR_INET (Unix.inet_addr_loopback, 0));
  let port =
    match Unix.getsockname socket with
    | Unix.ADDR_INET (_, port) -> port
    | _ -> fail "loopback socket did not expose a TCP port"
  in
  Unix.close socket;
  port
;;

type exact_request_capture =
  { meth : Cohttp.Code.meth
  ; path : string
  ; headers : Cohttp.Header.t
  ; body : string
  }

let with_exact_output_server
      ?(status = `OK)
      ?response_headers
      ?raw_response
      ?completion_response
      ?completion_delay_s
      ?(stop_reason = "end_turn")
      ?content
      ?(abort_completion = false)
      f
  =
  let completion_posts = Atomic.make 0 in
  let captures = Atomic.make [] in
  let result =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port = fresh_port () in
    let content =
      Option.value content ~default:{|[{"type":"text","text":"{\"accepted\":true}"}]|}
    in
    let response_headers =
      Option.value response_headers ~default:(Cohttp.Header.init ())
    in
    let handler _conn request body =
      let request_body = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let path = Cohttp.Request.uri request |> Uri.path in
      if String.equal (Filename.basename path) "count_tokens"
      then Cohttp_eio.Server.respond_string ~status:`OK ~body:{|{"input_tokens":321}|} ()
      else (
        let completion_number = Atomic.fetch_and_add completion_posts 1 + 1 in
        Atomic.set
          captures
          ({ meth = Cohttp.Request.meth request
           ; path
           ; headers = Cohttp.Request.headers request
           ; body = request_body
           }
           :: Atomic.get captures);
        if abort_completion then raise Exit;
        Option.iter (Eio.Time.sleep clock) completion_delay_s;
        let response =
          match completion_response, raw_response with
          | Some response, _ -> response completion_number
          | None, Some response -> response
          | None, None ->
            Printf.sprintf
              {|{"id":"msg-exact","type":"message","role":"assistant","model":"input-count-fixture","content":%s,"stop_reason":"%s","stop_sequence":null,"usage":{"input_tokens":321,"output_tokens":4}}|}
              content
              stop_reason
        in
        Cohttp_eio.Server.respond_string
          ~status
          ~headers:response_headers
          ~body:response
          ())
    in
    let socket =
      Eio.Net.listen
        net
        ~sw
        ~backlog:4
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let server = Cohttp_eio.Server.make ~callback:handler () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net ~clock ~base_url
  in
  result, Atomic.get completion_posts, List.rev (Atomic.get captures)
;;

let with_stale_exact_output_server
      ?(http_version = "HTTP/1.1")
      ?completion_framing_headers
      f
  =
  let completion_posts = Atomic.make 0 in
  let result =
    Eio_main.run
    @@ fun env ->
    Eio.Switch.run
    @@ fun sw ->
    let net = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    let port = fresh_port () in
    let socket =
      Eio.Net.listen
        net
        ~sw
        ~backlog:8
        ~reuse_addr:true
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
    in
    let read_request flow =
      let reader = Eio.Buf_read.of_flow ~max_size:max_int flow in
      let request_line = Eio.Buf_read.line reader in
      let rec read_headers content_length =
        let line = Eio.Buf_read.line reader |> String.trim in
        if String.equal line ""
        then content_length
        else (
          let content_length =
            match String.index_opt line ':' with
            | None -> content_length
            | Some separator ->
              let name = String.sub line 0 separator |> String.lowercase_ascii in
              if String.equal name "content-length"
              then
                String.sub line (separator + 1) (String.length line - separator - 1)
                |> String.trim
                |> int_of_string
              else content_length
          in
          read_headers content_length)
      in
      let content_length = read_headers 0 in
      ignore (Eio.Buf_read.take content_length reader : string);
      match String.split_on_char ' ' request_line with
      | _meth :: path :: _ -> path
      | _ -> failwith "stale exact fixture received malformed request line"
    in
    let write_response ?framing_headers flow body =
      let framing_headers =
        match framing_headers with
        | Some headers -> headers (String.length body)
        | None -> [ Printf.sprintf "Content-Length: %d" (String.length body) ]
      in
      Eio.Flow.copy_string
        (Printf.sprintf
           "%s 200 OK\r\nContent-Type: application/json\r\n%s\r\n\r\n%s"
           http_version
           (String.concat "\r\n" framing_headers)
           body)
        flow
    in
    let completion_response =
      {|{"id":"msg-stale","type":"message","role":"assistant","model":"input-count-fixture","content":[{"type":"text","text":"{\"accepted\":true}"}],"stop_reason":"end_turn","stop_sequence":null,"usage":{"input_tokens":321,"output_tokens":4}}|}
    in
    let handle flow _addr =
      let path = read_request flow |> Uri.of_string |> Uri.path in
      if String.equal (Filename.basename path) "count_tokens"
      then write_response flow {|{"input_tokens":321}|}
      else (
        Atomic.incr completion_posts;
        write_response
          ?framing_headers:completion_framing_headers
          flow
          completion_response)
    in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork socket ~sw ~on_error:(fun _ -> ()) handle
      done);
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net ~clock ~base_url
  in
  result, Atomic.get completion_posts
;;

let exact_messages = [ msg User [ Text "Return the exact requested output." ] ]

let exact_admitted
      ?(messages = exact_messages)
      ?(tools = [])
      ?body_timeout_s
      ~sw
      ~net
      ~clock
      config
  =
  let prepared = Complete.prepare_request ~config ~messages ~tools ?body_timeout_s () in
  let measured =
    match Complete.measure_request ~sw ~net ~clock prepared with
    | Ok measured -> measured
    | Error _ -> fail "exact request measurement failed"
  in
  match Complete.admit_request ~max_context_tokens:512 measured with
  | Ok admitted -> admitted
  | Error _ -> fail "exact request context admission failed"
;;

let exact_config base_url =
  { (config ~max_context:512 base_url) with
    model_capabilities_override = Some Capabilities.anthropic_capabilities
  ; headers = [ "x-exact-freeze", "admitted" ]
  ; tool_choice = None
  ; tool_stream = false
  ; disable_parallel_tool_use = false
  ; enable_thinking = None
  ; preserve_thinking = None
  ; thinking_budget = None
  ; reasoning_effort = None
  ; clear_thinking = None
  }
;;

let string_of_fingerprint plan =
  Complete.plan_fingerprint plan |> Complete.plan_fingerprint_to_string
;;

let sha256 value = Digestif.SHA256.(to_hex (digest_string value))

let rec canonical_json = function
  | `Assoc fields ->
    `Assoc
      (fields
       |> List.map (fun (name, value) -> name, canonical_json value)
       |> List.sort (fun (left, _) (right, _) -> String.compare left right))
  | `List values -> `List (List.map canonical_json values)
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as scalar -> scalar
;;

let check_receipt ~phase ~dispatch_count ~http_status receipt =
  check bool "receipt phase" true (Complete.receipt_phase receipt = phase);
  check
    int
    "receipt dispatch count"
    dispatch_count
    (Complete.receipt_dispatch_count receipt);
  check
    (option int)
    "receipt HTTP status"
    http_status
    (Complete.receipt_http_status receipt)
;;

let test_exact_output_schema_provenance_and_single_dispatch () =
  let (result, plan_digest, plan_fingerprint, admitted_schema), completion_posts, captures
    =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg = exact_config base_url in
    let admitted_schema =
      match cfg.response_format with
      | JsonSchema schema -> schema
      | Off | JsonMode -> fail "exact schema fixture lost its schema"
    in
    let admitted = exact_admitted ~sw ~net ~clock cfg in
    let plan =
      match Complete.admit_exact_output admitted with
      | Ok plan -> plan
      | Error _ -> fail "exact output admission failed"
    in
    let fingerprint = string_of_fingerprint plan in
    check bool "fingerprint is populated" true (String.length fingerprint = 64);
    let previous_catalog = Model_catalog.global () in
    let result =
      Fun.protect
        ~finally:(fun () ->
          match previous_catalog with
          | Some catalog -> Model_catalog.set_global catalog
          | None -> Model_catalog.clear_global ())
        (fun () ->
           Model_catalog.set_global Model_catalog.empty;
           Complete.execute_once ~net plan)
    in
    ( result
    , Complete.plan_request_body_sha256 plan
    , Complete.plan_fingerprint plan
    , admitted_schema )
  in
  check int "one completion dispatch" 1 completion_posts;
  let capture =
    match captures with
    | [ capture ] -> capture
    | _ -> fail "expected exactly one captured completion"
  in
  check string "method" "POST" (Cohttp.Code.string_of_method capture.meth);
  check string "frozen path" "/proxy/messages" capture.path;
  check
    (option string)
    "frozen custom header"
    (Some "admitted")
    (Cohttp.Header.get capture.headers "x-exact-freeze");
  check
    (option string)
    "frozen content length"
    (Some (string_of_int (String.length capture.body)))
    (Cohttp.Header.get capture.headers "content-length");
  check string "transmitted body digest" plan_digest (sha256 capture.body);
  let request_json = Yojson.Safe.from_string capture.body in
  let output_format =
    Yojson.Safe.Util.(request_json |> member "output_config" |> member "format")
  in
  check
    string
    "provider-native schema request"
    "json_schema"
    Yojson.Safe.Util.(output_format |> member "type" |> to_string);
  let captured_schema = Yojson.Safe.Util.member "schema" output_format in
  check
    bool
    "admitted schema equals captured schema"
    true
    (canonical_json admitted_schema = canonical_json captured_schema);
  match result with
  | Ok
      { Complete.output =
          Complete.Json_output
            { value = `Assoc [ ("accepted", `Bool true) ]
            ; validation = Complete.Provider_schema_requested_client_validation_required
            }
      ; response_format = JsonSchema outcome_schema
      ; receipt
      ; pricing = Complete.Pricing_annotation_omitted
      ; _
      } ->
    check
      bool
      "captured schema equals outcome schema"
      true
      (canonical_json captured_schema = canonical_json outcome_schema);
    check
      string
      "receipt fingerprint binds plan"
      (Complete.plan_fingerprint_to_string plan_fingerprint)
      (Complete.receipt_fingerprint receipt |> Complete.plan_fingerprint_to_string);
    check
      string
      "receipt digest binds captured bytes"
      (sha256 capture.body)
      (Complete.receipt_request_body_sha256 receipt);
    check_receipt
      ~phase:Complete.Terminal
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ -> fail "exact schema outcome lost validation provenance"
  | Error _ -> fail "exact schema execution failed"
;;

let test_exact_output_rejects_contradictory_schema_state () =
  let result, completion_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg =
      { (exact_config base_url) with
        response_format = JsonMode
      ; output_schema = Some (`Assoc [ "type", `String "object" ])
      }
    in
    exact_admitted ~sw ~net ~clock cfg |> Complete.admit_exact_output
  in
  check int "contradiction never completes" 0 completion_posts;
  match result with
  | Error Complete.Contradictory_output_state -> ()
  | Ok _ | Error _ -> fail "expected contradictory output state rejection"
;;

let test_exact_output_rejects_missing_capability_snapshot () =
  let result, completion_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg = { (config ~max_context:512 base_url) with tool_choice = None } in
    exact_admitted ~sw ~net ~clock cfg |> Complete.admit_exact_output
  in
  check int "missing snapshot never completes" 0 completion_posts;
  match result with
  | Error Complete.Explicit_capability_snapshot_required -> ()
  | Ok _ | Error _ -> fail "expected explicit capability snapshot rejection"
;;

let test_exact_output_rejects_partial_json_after_one_dispatch () =
  let result, completion_posts, _ =
    with_exact_output_server ~stop_reason:"max_tokens"
    @@ fun ~sw ~net ~clock ~base_url ->
    let admitted = exact_admitted ~sw ~net ~clock (exact_config base_url) in
    let plan = Complete.admit_exact_output admitted |> Result.get_ok in
    Complete.execute_once ~net plan
  in
  check int "partial response is not retried" 1 completion_posts;
  match result with
  | Error
      { Complete.receipt
      ; cause =
          Complete.Output_normalization_failed
            (Complete.Incomplete_structured_response MaxTokens)
      } ->
    check_receipt
      ~phase:Complete.Response_received
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ | Error _ -> fail "max_tokens JSON must not be exact structured success"
;;

let test_exact_output_rejects_multiple_text_blocks () =
  let content = {|[{"type":"text","text":"{}"},{"type":"text","text":"{}"}]|} in
  let result, completion_posts, _ =
    with_exact_output_server ~content
    @@ fun ~sw ~net ~clock ~base_url ->
    let admitted = exact_admitted ~sw ~net ~clock (exact_config base_url) in
    let plan = Complete.admit_exact_output admitted |> Result.get_ok in
    Complete.execute_once ~net plan
  in
  check int "ambiguous response is not retried" 1 completion_posts;
  match result with
  | Error
      { Complete.receipt
      ; cause =
          Complete.Output_normalization_failed (Complete.Ambiguous_structured_text 2)
      } ->
    check_receipt
      ~phase:Complete.Response_received
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ | Error _ -> fail "multiple text blocks must fail exact normalization"
;;

let test_exact_output_rejects_non_exact_json_text () =
  let cases =
    [ {|[{"type":"text","text":"```json\\n{}\\n```"}]|}
    ; {|[{"type":"text","text":"{} trailing"}]|}
    ; {|[{"type":"text","text":"{} {}"}]|}
    ; {|[{"type":"text","text":"{\"accepted\":"}]|}
    ]
  in
  List.iter
    (fun content ->
       let result, completion_posts, _ =
         with_exact_output_server ~content
         @@ fun ~sw ~net ~clock ~base_url ->
         let admitted = exact_admitted ~sw ~net ~clock (exact_config base_url) in
         let plan = Complete.admit_exact_output admitted |> Result.get_ok in
         Complete.execute_once ~net plan
       in
       check int "invalid JSON is not retried" 1 completion_posts;
       match result with
       | Error
           { Complete.receipt
           ; cause = Complete.Output_normalization_failed (Complete.Invalid_json _)
           } ->
         check_receipt
           ~phase:Complete.Response_received
           ~dispatch_count:1
           ~http_status:(Some 200)
           receipt
       | Ok _ | Error _ -> fail "non-exact JSON text must fail")
    cases
;;

let test_exact_output_rejects_text_plus_tool_use () =
  let content =
    {|[{"type":"text","text":"{}"},{"type":"tool_use","id":"tool-1","name":"lookup","input":{}}]|}
  in
  let result, completion_posts, _ =
    with_exact_output_server ~stop_reason:"stop_sequence" ~content
    @@ fun ~sw ~net ~clock ~base_url ->
    let admitted = exact_admitted ~sw ~net ~clock (exact_config base_url) in
    let plan = Complete.admit_exact_output admitted |> Result.get_ok in
    Complete.execute_once ~net plan
  in
  check int "mixed content is not retried" 1 completion_posts;
  match result with
  | Error
      { Complete.receipt
      ; cause =
          Complete.Output_normalization_failed Complete.Unexpected_structured_content
      } ->
    check_receipt
      ~phase:Complete.Response_received
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ | Error _ -> fail "text plus tool use must fail exact normalization"
;;

let test_exact_output_preserves_reasoning_without_mixing_json_bytes () =
  let content =
    {|[{"type":"thinking","thinking":"{\"must_not_mix\":true}","signature":"sig-exact"},{"type":"text","text":"{\"accepted\":true}"}]|}
  in
  let result, completion_posts, _ =
    with_exact_output_server ~content
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net plan
  in
  check int "reasoning coexistence dispatches once" 1 completion_posts;
  match result with
  | Ok
      { Complete.output =
          Complete.Json_output { value = `Assoc [ ("accepted", `Bool true) ]; _ }
      ; response = { content = [ Thinking { content = reasoning; _ }; Text text ]; _ }
      ; receipt
      ; _
      } ->
    check string "raw reasoning preserved" {|{"must_not_mix":true}|} reasoning;
    check string "raw text preserved" {|{"accepted":true}|} text;
    check_receipt
      ~phase:Complete.Terminal
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ | Error _ -> fail "reasoning must coexist without entering JSON bytes"
;;

let test_exact_output_off_still_requires_terminal_text () =
  let content = {|[{"type":"text","text":"plain"}]|} in
  let result, completion_posts, _ =
    with_exact_output_server ~stop_reason:"max_tokens" ~content
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg =
      { (exact_config base_url) with response_format = Off; output_schema = None }
    in
    let admitted = exact_admitted ~sw ~net ~clock cfg in
    let plan = Complete.admit_exact_output admitted |> Result.get_ok in
    Complete.execute_once ~net plan
  in
  check int "Off partial response is not retried" 1 completion_posts;
  match result with
  | Error
      { Complete.receipt
      ; cause =
          Complete.Output_normalization_failed
            (Complete.Incomplete_structured_response MaxTokens)
      } ->
    check_receipt
      ~phase:Complete.Response_received
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ | Error _ -> fail "Off must retain terminality guard"
;;

let test_exact_output_provider_and_parser_errors_are_single_dispatch () =
  let provider_result, provider_posts, _ =
    with_exact_output_server ~status:`Too_many_requests ~raw_response:"rate limited"
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net plan
  in
  check int "provider error dispatch count" 1 provider_posts;
  (match provider_result with
   | Error
       { Complete.receipt
       ; cause = Complete.Provider_error (Http_client.HttpError { code = 429; _ })
       } ->
     check_receipt
       ~phase:Complete.Response_received
       ~dispatch_count:1
       ~http_status:(Some 429)
       receipt
   | Ok _ | Error _ -> fail "expected typed one-dispatch provider error");
  let parser_result, parser_posts, _ =
    with_exact_output_server ~raw_response:{|{"unexpected":true}|}
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net plan
  in
  check int "parser error dispatch count" 1 parser_posts;
  match parser_result with
  | Error { Complete.receipt; cause = Complete.Provider_error _ } ->
    check_receipt
      ~phase:Complete.Response_received
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Ok _ | Error _ -> fail "expected typed one-dispatch provider parse error"
;;

let test_exact_output_before_and_dispatch_started_receipts () =
  let before_result, before_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~body_timeout_s:1.0 ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net plan
  in
  check int "clock rejection has no dispatch" 0 before_posts;
  (match before_result with
   | Error { Complete.receipt; cause = Complete.Clock_required_for_timeout } ->
     check_receipt
       ~phase:Complete.Before_dispatch
       ~dispatch_count:0
       ~http_status:None
       receipt
   | Ok _ | Error _ -> fail "expected before-dispatch clock rejection");
  let started_result, started_posts, _ =
    with_exact_output_server ~abort_completion:true
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net plan
  in
  check int "aborted provider saw one dispatch" 1 started_posts;
  match started_result with
  | Error { Complete.receipt; cause = Complete.Provider_error _ } ->
    check_receipt
      ~phase:Complete.Dispatch_started
      ~dispatch_count:1
      ~http_status:None
      receipt
  | Ok _ | Error _ -> fail "expected dispatch-started transport receipt"
;;

let test_exact_output_total_deadline_covers_response_headers () =
  let result, completion_posts, _ =
    with_exact_output_server ~completion_delay_s:0.1
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~body_timeout_s:0.01 ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net ~clock plan
  in
  check int "header stall dispatches once" 1 completion_posts;
  match result with
  | Error
      { Complete.receipt
      ; cause =
          Complete.Provider_error
            (Http_client.TimeoutError { phase = Http_client.Wall_clock; _ })
      } ->
    check_receipt
      ~phase:Complete.Dispatch_started
      ~dispatch_count:1
      ~http_status:None
      receipt
  | Ok _ | Error _ -> fail "body_timeout_s must bound response-header stall"
;;

let test_exact_output_caller_timeout_is_not_reclassified () =
  let (timeout_result, next_result, after_timeout, after_next), completion_posts, _ =
    with_exact_output_server ~completion_delay_s:0.1
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let timeout_result =
      Eio.Time.with_timeout clock 0.01 (fun () ->
        Ok (Complete.execute_once ~net ~connection_cache plan))
    in
    let after_timeout = Http_client.cache_stats connection_cache in
    let next_result = Complete.execute_once ~net ~connection_cache plan in
    let after_next = Http_client.cache_stats connection_cache in
    timeout_result, next_result, after_timeout, after_next
  in
  check int "caller timeout and next call dispatch once each" 2 completion_posts;
  check
    int
    "timeout created one connection"
    1
    after_timeout.Http_client.create_count_total;
  check int "timeout connection was not reused" 0 after_timeout.reuse_count_total;
  check int "timeout leaves cache empty" 0 after_timeout.total_idle;
  check int "next call creates a fresh connection" 2 after_next.create_count_total;
  check int "next call did not retry through cache" 0 after_next.reuse_count_total;
  (match timeout_result with
   | Error `Timeout -> ()
   | Ok _ -> fail "caller-owned timeout must escape post_sync_once unchanged");
  match next_result with
  | Ok { Complete.receipt; _ } ->
    check_receipt
      ~phase:Complete.Terminal
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Error _ -> fail "call after caller-owned timeout must use a fresh connection"
;;

let test_exact_output_fingerprint_is_deterministic_and_sensitive () =
  let result, completion_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg = exact_config base_url in
    let plan config ?(messages = exact_messages) ?body_timeout_s () =
      exact_admitted ?body_timeout_s ~messages ~sw ~net ~clock config
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let base = plan cfg () in
    let same = plan cfg () in
    let different_body =
      plan cfg ~messages:[ msg User [ Text "Different exact request." ] ] ()
    in
    let different_body_timeout = plan cfg ~body_timeout_s:2.0 () in
    let different_connect_timeout = plan { cfg with connect_timeout_s = Some 2.0 } () in
    let different_url = plan { cfg with request_path = "/other/messages" } () in
    let different_header =
      plan { cfg with headers = [ "x-exact-freeze", "changed" ] } ()
    in
    let different_provider = plan { cfg with provider_id = Some "slot-b" } () in
    let schema_a =
      `Assoc
        [ "type", `String "object"
        ; "properties", `Assoc [ "accepted", `Assoc [ "type", `String "boolean" ] ]
        ]
    in
    let schema_a_reordered =
      `Assoc
        [ "properties", `Assoc [ "accepted", `Assoc [ "type", `String "boolean" ] ]
        ; "type", `String "object"
        ]
    in
    let schema_b = `Assoc [ "type", `String "array" ] in
    let with_schema schema =
      { cfg with response_format = JsonSchema schema; output_schema = Some schema }
    in
    let canonical_a = plan (with_schema schema_a) () in
    let canonical_a_reordered = plan (with_schema schema_a_reordered) () in
    let different_schema = plan (with_schema schema_b) () in
    let fp = string_of_fingerprint in
    check string "deterministic" (fp base) (fp same);
    check string "canonical schema ordering" (fp canonical_a) (fp canonical_a_reordered);
    check bool "body sensitivity" true (fp base <> fp different_body);
    check bool "body timeout sensitivity" true (fp base <> fp different_body_timeout);
    check bool "connect timeout sensitivity" true (fp base <> fp different_connect_timeout);
    check bool "URL sensitivity" true (fp base <> fp different_url);
    check bool "header sensitivity" true (fp base <> fp different_header);
    check bool "provider sensitivity" true (fp base <> fp different_provider);
    check bool "schema sensitivity" true (fp canonical_a <> fp different_schema);
    check bool "fingerprint hides header secret" false (String.equal (fp base) "admitted");
    Complete.execute_once ~net base
  in
  check int "fingerprint construction does not dispatch" 1 completion_posts;
  match result with
  | Ok { Complete.receipt; _ } ->
    check_receipt
      ~phase:Complete.Terminal
      ~dispatch_count:1
      ~http_status:(Some 200)
      receipt
  | Error _ -> fail "fingerprint baseline execution failed"
;;

let test_exact_output_plan_is_reusable_per_invocation () =
  let (first, second), completion_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    Complete.execute_once ~net plan, Complete.execute_once ~net plan
  in
  check int "one dispatch per invocation" 2 completion_posts;
  List.iter
    (function
      | Ok { Complete.receipt; _ } ->
        check_receipt
          ~phase:Complete.Terminal
          ~dispatch_count:1
          ~http_status:(Some 200)
          receipt
      | Error _ -> fail "reusable exact plan execution failed")
    [ first; second ]
;;

let test_exact_output_plan_is_concurrently_reusable_without_cross_contamination () =
  let completion_response attempt =
    let text = Yojson.Safe.to_string (`Assoc [ "attempt", `Int attempt ]) in
    `Assoc
      [ "id", `String (Printf.sprintf "msg-concurrent-%d" attempt)
      ; "type", `String "message"
      ; "role", `String "assistant"
      ; "model", `String "input-count-fixture"
      ; "content", `List [ `Assoc [ "type", `String "text"; "text", `String text ] ]
      ; "stop_reason", `String "end_turn"
      ; "stop_sequence", `Null
      ; "usage", `Assoc [ "input_tokens", `Int 321; "output_tokens", `Int 4 ]
      ]
    |> Yojson.Safe.to_string
  in
  let (first, second, fingerprint, digest), completion_posts, captures =
    with_exact_output_server ~completion_response
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let first_promise, first_resolver = Eio.Promise.create () in
    let second_promise, second_resolver = Eio.Promise.create () in
    Eio.Fiber.both
      (fun () -> Complete.execute_once ~net plan |> Eio.Promise.resolve first_resolver)
      (fun () -> Complete.execute_once ~net plan |> Eio.Promise.resolve second_resolver);
    let first = Eio.Promise.await first_promise in
    let second = Eio.Promise.await second_promise in
    first, second, Complete.plan_fingerprint plan, Complete.plan_request_body_sha256 plan
  in
  check int "concurrent calls dispatch independently" 2 completion_posts;
  check int "concurrent calls capture two requests" 2 (List.length captures);
  List.iter
    (fun capture -> check string "concurrent body digest" digest (sha256 capture.body))
    captures;
  let attempt_and_receipt = function
    | Ok
        { Complete.output =
            Complete.Json_output { value = `Assoc [ ("attempt", `Int attempt) ]; _ }
        ; receipt
        ; _
        } ->
      check_receipt
        ~phase:Complete.Terminal
        ~dispatch_count:1
        ~http_status:(Some 200)
        receipt;
      check
        string
        "concurrent receipt fingerprint"
        (Complete.plan_fingerprint_to_string fingerprint)
        (Complete.receipt_fingerprint receipt |> Complete.plan_fingerprint_to_string);
      check
        string
        "concurrent receipt digest"
        digest
        (Complete.receipt_request_body_sha256 receipt);
      attempt
    | Ok _ | Error _ -> fail "concurrent exact execution failed"
  in
  let attempts =
    List.sort Int.compare [ attempt_and_receipt first; attempt_and_receipt second ]
  in
  check (list int) "responses do not cross-contaminate" [ 1; 2 ] attempts
;;

let test_exact_output_connection_close_is_not_cached () =
  let (first, second, stats), completion_posts, _ =
    with_exact_output_server
      ~response_headers:(Cohttp.Header.of_list [ "connection", "close" ])
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "closed response dispatches twice" 2 completion_posts;
  check int "closed response creates twice" 2 stats.Http_client.create_count_total;
  check int "closed response is never reused" 0 stats.reuse_count_total;
  check int "closed response is never parked" 0 stats.total_idle;
  List.iter
    (function
      | Ok { Complete.receipt; _ } ->
        check_receipt
          ~phase:Complete.Terminal
          ~dispatch_count:1
          ~http_status:(Some 200)
          receipt
      | Error _ -> fail "connection-close exact execution failed")
    [ first; second ]
;;

let test_exact_output_normal_connection_is_reused () =
  let (first, second, stats), completion_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "normal response dispatches twice" 2 completion_posts;
  check int "normal response creates once" 1 stats.Http_client.create_count_total;
  check int "normal response reuses once" 1 stats.reuse_count_total;
  check int "normal response remains parked" 1 stats.total_idle;
  List.iter
    (function
      | Ok { Complete.receipt; _ } ->
        check_receipt
          ~phase:Complete.Terminal
          ~dispatch_count:1
          ~http_status:(Some 200)
          receipt
      | Error _ -> fail "normal cached exact execution failed")
    [ first; second ]
;;

let test_exact_output_final_chunked_connection_is_reused () =
  let (first, second, stats), completion_posts, _ =
    with_exact_output_server
      ~response_headers:(Cohttp.Header.of_list [ "transfer-encoding", "chunked" ])
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "final chunked dispatches twice" 2 completion_posts;
  check int "final chunked creates once" 1 stats.Http_client.create_count_total;
  check int "final chunked reuses once" 1 stats.reuse_count_total;
  List.iter
    (function
      | Ok { Complete.receipt; _ } ->
        check_receipt
          ~phase:Complete.Terminal
          ~dispatch_count:1
          ~http_status:(Some 200)
          receipt
      | Error _ -> fail "final chunked exact execution failed")
    [ first; second ]
;;

let test_exact_output_ambiguous_response_framing_is_not_cached () =
  let cases =
    [ ( "Content-Length plus Transfer-Encoding"
      , fun length ->
          [ Printf.sprintf "Content-Length: %d" length; "Transfer-Encoding: identity" ] )
    ; ( "duplicate Content-Length"
      , fun length ->
          [ Printf.sprintf "Content-Length: %d" length
          ; Printf.sprintf "Content-Length: %d" length
          ] )
    ; ( "conflicting Content-Length"
      , fun length ->
          [ Printf.sprintf "Content-Length: %d" (length + 1)
          ; Printf.sprintf "Content-Length: %d" length
          ] )
    ; ("invalid Content-Length", fun _ -> [ "Content-Length: invalid" ])
    ; ("non-final chunked", fun _ -> [ "Transfer-Encoding: chunked, gzip" ])
    ; ("malformed chunked", fun _ -> [ "Transfer-Encoding: gzip,,chunked" ])
    ]
  in
  List.iter
    (fun (name, completion_framing_headers) ->
       let (first, second, stats), completion_posts =
         with_stale_exact_output_server ~completion_framing_headers
         @@ fun ~sw ~net ~clock ~base_url ->
         let plan =
           exact_admitted ~sw ~net ~clock (exact_config base_url)
           |> Complete.admit_exact_output
           |> Result.get_ok
         in
         let connection_cache = Http_client.create_cache ~sw () in
         let first = Complete.execute_once ~net ~connection_cache plan in
         let second = Complete.execute_once ~net ~connection_cache plan in
         first, second, Http_client.cache_stats connection_cache
       in
       check int (name ^ " dispatch count") 2 completion_posts;
       check int (name ^ " creates fresh connection") 2 stats.create_count_total;
       check int (name ^ " never reuses") 0 stats.reuse_count_total;
       check int (name ^ " never parks") 0 stats.total_idle;
       List.iter
         (function
           | Ok { Complete.receipt; _ } ->
             check_receipt
               ~phase:Complete.Terminal
               ~dispatch_count:1
               ~http_status:(Some 200)
               receipt
           | Error _ -> fail (name ^ " exact execution failed"))
         [ first; second ])
    cases
;;

let test_exact_output_http_1_0_without_keep_alive_is_not_cached () =
  let (first, second, stats), completion_posts =
    with_stale_exact_output_server ~http_version:"HTTP/1.0"
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "HTTP/1.0 dispatches twice" 2 completion_posts;
  check int "HTTP/1.0 creates twice" 2 stats.Http_client.create_count_total;
  check int "HTTP/1.0 never reuses without keep-alive" 0 stats.reuse_count_total;
  check int "HTTP/1.0 never parks without keep-alive" 0 stats.total_idle;
  List.iter
    (function
      | Ok { Complete.receipt; _ } ->
        check_receipt
          ~phase:Complete.Terminal
          ~dispatch_count:1
          ~http_status:(Some 200)
          receipt
      | Error _ -> fail "HTTP/1.0 exact execution failed")
    [ first; second ]
;;

let test_exact_output_upgrade_responses_are_not_cached () =
  let (first_101, second_101, stats_101), posts_101, _ =
    with_exact_output_server ~status:`Switching_protocols
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "101 dispatches twice" 2 posts_101;
  check int "101 creates twice" 2 stats_101.Http_client.create_count_total;
  check int "101 never reuses" 0 stats_101.reuse_count_total;
  List.iter
    (function
      | Error
          { Complete.receipt
          ; cause = Complete.Provider_error (Http_client.HttpError { code = 101; _ })
          } ->
        check_receipt
          ~phase:Complete.Response_received
          ~dispatch_count:1
          ~http_status:(Some 101)
          receipt
      | Ok _ | Error _ -> fail "101 must remain a non-cached provider response")
    [ first_101; second_101 ];
  let upgrade_headers =
    Cohttp.Header.of_list [ "connection", "upgrade"; "upgrade", "websocket" ]
  in
  let (first_upgrade, second_upgrade, stats_upgrade), posts_upgrade, _ =
    with_exact_output_server ~response_headers:upgrade_headers
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "Upgrade dispatches twice" 2 posts_upgrade;
  check int "Upgrade creates twice" 2 stats_upgrade.Http_client.create_count_total;
  check int "Upgrade never reuses" 0 stats_upgrade.reuse_count_total;
  List.iter
    (function
      | Ok { Complete.receipt; _ } ->
        check_receipt
          ~phase:Complete.Terminal
          ~dispatch_count:1
          ~http_status:(Some 200)
          receipt
      | Error _ -> fail "Upgrade response execution failed")
    [ first_upgrade; second_upgrade ]
;;

let test_exact_output_stale_cached_connection_has_no_hidden_retry () =
  let (first, second, stats), completion_posts =
    with_stale_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let plan =
      exact_admitted ~sw ~net ~clock (exact_config base_url)
      |> Complete.admit_exact_output
      |> Result.get_ok
    in
    let connection_cache = Http_client.create_cache ~sw () in
    let first = Complete.execute_once ~net ~connection_cache plan in
    let second = Complete.execute_once ~net ~connection_cache plan in
    first, second, Http_client.cache_stats connection_cache
  in
  check int "stale server observes no hidden retry" 1 completion_posts;
  check int "stale connection created once" 1 stats.Http_client.create_count_total;
  check int "stale connection attempted once" 1 stats.reuse_count_total;
  (match first with
   | Ok { Complete.receipt; _ } ->
     check_receipt
       ~phase:Complete.Terminal
       ~dispatch_count:1
       ~http_status:(Some 200)
       receipt
   | Error _ -> fail "first stale-cache setup dispatch must succeed");
  match second with
  | Error { Complete.receipt; cause = Complete.Provider_error _ } ->
    check_receipt
      ~phase:Complete.Dispatch_started
      ~dispatch_count:1
      ~http_status:None
      receipt
  | Ok _ | Error _ -> fail "stale cached connection must fail without retry"
;;

let test_exact_output_rejects_tools_and_reasoning_pre_dispatch () =
  let tool_result, tool_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg =
      { (exact_config base_url) with
        response_format = Off
      ; output_schema = None
      ; tool_choice = Some Any
      }
    in
    exact_admitted ~tools:[ tool ] ~sw ~net ~clock cfg |> Complete.admit_exact_output
  in
  check int "tools rejected before completion" 0 tool_posts;
  (match tool_result with
   | Error Complete.Unsupported_exact_cross_feature -> ()
   | Ok _ | Error _ -> fail "exact tools must reject before dispatch");
  let reasoning_result, reasoning_posts, _ =
    with_exact_output_server
    @@ fun ~sw ~net ~clock ~base_url ->
    let cfg =
      { (exact_config base_url) with
        response_format = Off
      ; output_schema = None
      ; clear_thinking = Some false
      }
    in
    exact_admitted ~sw ~net ~clock cfg |> Complete.admit_exact_output
  in
  check int "reasoning rejected before completion" 0 reasoning_posts;
  match reasoning_result with
  | Error Complete.Unsupported_exact_cross_feature -> ()
  | Ok _ | Error _ -> fail "exact reasoning must reject before dispatch"
;;

let test_exact_output_rejects_caller_framing_headers_before_completion () =
  let check_headers headers expected_name =
    let result, completion_posts, _ =
      with_exact_output_server
      @@ fun ~sw ~net ~clock ~base_url ->
      let cfg = { (exact_config base_url) with headers } in
      exact_admitted ~sw ~net ~clock cfg |> Complete.admit_exact_output
    in
    check int "framing header never completes" 0 completion_posts;
    match result with
    | Error (Complete.Caller_supplied_framing_header_not_allowed actual) ->
      check string "typed framing header" expected_name actual
    | Ok _ | Error _ -> fail "caller framing header must reject before completion"
  in
  check_headers [ "Connection", "close" ] "connection";
  check_headers [ "Transfer-Encoding", "identity" ] "transfer-encoding";
  check_headers [ "cOnTeNt-LeNgTh", "1" ] "content-length";
  check_headers [ "Content-Length", "1"; "content-length", "1" ] "content-length"
;;

let () =
  run
    "exact-output"
    [ ( "exact-output"
      , [ test_case
            "schema provenance and one dispatch"
            `Quick
            test_exact_output_schema_provenance_and_single_dispatch
        ; test_case
            "contradictory schema state"
            `Quick
            test_exact_output_rejects_contradictory_schema_state
        ; test_case
            "explicit capability snapshot required"
            `Quick
            test_exact_output_rejects_missing_capability_snapshot
        ; test_case
            "partial JSON is not retried"
            `Quick
            test_exact_output_rejects_partial_json_after_one_dispatch
        ; test_case
            "multiple text blocks are ambiguous"
            `Quick
            test_exact_output_rejects_multiple_text_blocks
        ; test_case
            "fenced trailing multiple and truncated JSON reject"
            `Quick
            test_exact_output_rejects_non_exact_json_text
        ; test_case
            "text plus tool use rejects"
            `Quick
            test_exact_output_rejects_text_plus_tool_use
        ; test_case
            "reasoning is preserved without JSON byte mixing"
            `Quick
            test_exact_output_preserves_reasoning_without_mixing_json_bytes
        ; test_case
            "Off retains terminality guard"
            `Quick
            test_exact_output_off_still_requires_terminal_text
        ; test_case
            "provider and parser errors dispatch once"
            `Quick
            test_exact_output_provider_and_parser_errors_are_single_dispatch
        ; test_case
            "before and dispatch-started receipts"
            `Quick
            test_exact_output_before_and_dispatch_started_receipts
        ; test_case
            "body timeout covers response headers"
            `Quick
            test_exact_output_total_deadline_covers_response_headers
        ; test_case
            "caller timeout is not reclassified"
            `Quick
            test_exact_output_caller_timeout_is_not_reclassified
        ; test_case
            "fingerprint deterministic and sensitive"
            `Quick
            test_exact_output_fingerprint_is_deterministic_and_sensitive
        ; test_case
            "plan reusable per invocation"
            `Quick
            test_exact_output_plan_is_reusable_per_invocation
        ; test_case
            "plan concurrently reusable without contamination"
            `Quick
            test_exact_output_plan_is_concurrently_reusable_without_cross_contamination
        ; test_case
            "Connection close is not cached"
            `Quick
            test_exact_output_connection_close_is_not_cached
        ; test_case
            "normal connection is reused"
            `Quick
            test_exact_output_normal_connection_is_reused
        ; test_case
            "valid final chunked connection is reused"
            `Quick
            test_exact_output_final_chunked_connection_is_reused
        ; test_case
            "ambiguous response framing is not cached"
            `Quick
            test_exact_output_ambiguous_response_framing_is_not_cached
        ; test_case
            "HTTP/1.0 without keep-alive is not cached"
            `Quick
            test_exact_output_http_1_0_without_keep_alive_is_not_cached
        ; test_case
            "101 and Upgrade responses are not cached"
            `Quick
            test_exact_output_upgrade_responses_are_not_cached
        ; test_case
            "stale cached connection has no hidden retry"
            `Quick
            test_exact_output_stale_cached_connection_has_no_hidden_retry
        ; test_case
            "tools and reasoning reject pre-dispatch"
            `Quick
            test_exact_output_rejects_tools_and_reasoning_pre_dispatch
        ; test_case
            "caller framing headers reject before completion"
            `Quick
            test_exact_output_rejects_caller_framing_headers_before_completion
        ] )
    ]
;;
