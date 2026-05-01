(** Integration tests for guardrails — verify tool filtering and limits
    interact correctly with Agent.run.

    Uses mock HTTP server (Cohttp_eio, Anthropic Messages API format).
    Each test gets a unique port. *)

open Agent_sdk
open Types

(* ── Mock HTTP helpers ───────────────────────────────── *)

let text_body text =
  Printf.sprintf
    {|{"id":"g1","type":"message","role":"assistant","model":"mock","content":[{"type":"text","text":"%s"}],"stop_reason":"end_turn","usage":{"input_tokens":10,"output_tokens":5}}|}
    text
;;

let tool_use_body ~tool_name =
  Printf.sprintf
    {|{"id":"g2","type":"message","role":"assistant","model":"mock","content":[{"type":"tool_use","id":"tu_%s","name":"%s","input":{}}],"stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":5}}|}
    tool_name
    tool_name
;;

(** Handler that tracks how many tools were in the request,
    then returns tool_use on first N calls, text on rest. *)
let tracking_handler ~tool_use_count ~tools_seen_count _conn _req body =
  let body_str = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
  let json = Yojson.Safe.from_string body_str in
  (* Count tools in the request *)
  (match Yojson.Safe.Util.(json |> member "tools") with
   | `List l -> tools_seen_count := List.length l
   | `Null -> tools_seen_count := 0
   | _ -> ());
  let n = !tool_use_count in
  decr tool_use_count;
  let response_body =
    if n > 0 then tool_use_body ~tool_name:"echo" else text_body "finished"
  in
  Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
;;

let text_only_handler _conn _req body =
  let _ = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
  Cohttp_eio.Server.respond_string ~status:`OK ~body:(text_body "ok") ()
;;

let with_mock_server ~port handler f =
  Eio_main.run
  @@ fun env ->
  try
    Eio.Switch.run
    @@ fun sw ->
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
    let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net:env#net ~base_url;
    Eio.Switch.fail sw Exit
  with
  | Exit -> ()
;;

let make_tool name =
  Tool.create ~name ~description:("tool " ^ name) ~parameters:[] (fun _input ->
    Ok { Types.content = name ^ " result" })
;;

(* ── tool_filter tests ───────────────────────────────── *)

let test_deny_list_hides_tool () =
  let tools_seen = ref 0 in
  let tool_calls = ref 0 in
  with_mock_server
    ~port:18201
    (tracking_handler ~tool_use_count:tool_calls ~tools_seen_count:tools_seen)
    (fun ~sw ~net ~base_url ->
       let tools = [ make_tool "a"; make_tool "b"; make_tool "c" ] in
       let options =
         { Agent.default_options with
           base_url
         ; guardrails =
             { Guardrails.default with tool_filter = Guardrails.DenyList [ "b" ] }
         }
       in
       let agent = Agent.create ~net ~options ~tools () in
       (match Agent.run ~sw agent "test" with
        | Ok _ | Error _ -> ());
       Alcotest.(check int) "2 tools visible" 2 !tools_seen)
;;

let test_allow_list_filters () =
  let tools_seen = ref 0 in
  let tool_calls = ref 0 in
  with_mock_server
    ~port:18202
    (tracking_handler ~tool_use_count:tool_calls ~tools_seen_count:tools_seen)
    (fun ~sw ~net ~base_url ->
       let tools = [ make_tool "a"; make_tool "b"; make_tool "c" ] in
       let options =
         { Agent.default_options with
           base_url
         ; guardrails =
             { Guardrails.default with tool_filter = Guardrails.AllowList [ "a" ] }
         }
       in
       let agent = Agent.create ~net ~options ~tools () in
       (match Agent.run ~sw agent "test" with
        | Ok _ | Error _ -> ());
       Alcotest.(check int) "1 tool visible" 1 !tools_seen)
;;

let test_custom_prefix_filter () =
  let tools_seen = ref 0 in
  let tool_calls = ref 0 in
  with_mock_server
    ~port:18203
    (tracking_handler ~tool_use_count:tool_calls ~tools_seen_count:tools_seen)
    (fun ~sw ~net ~base_url ->
       let tools =
         [ make_tool "safe_read"; make_tool "safe_write"; make_tool "danger_delete" ]
       in
       let options =
         { Agent.default_options with
           base_url
         ; guardrails =
             { Guardrails.default with
               tool_filter =
                 Guardrails.Custom
                   (fun schema ->
                     String.length schema.name >= 5
                     && String.sub schema.name 0 5 = "safe_")
             }
         }
       in
       let agent = Agent.create ~net ~options ~tools () in
       (match Agent.run ~sw agent "test" with
        | Ok _ | Error _ -> ());
       Alcotest.(check int) "2 safe tools visible" 2 !tools_seen)
;;

(* ── max_tool_calls_per_turn tests ───────────────────── *)

(* max_tool_calls_per_turn limits tool_use blocks in a SINGLE LLM response.
   When exceeded, tools are NOT executed and an error message is injected. *)
let test_limit_blocks_multi_tool_response () =
  let multi_tool_handler _conn _req body =
    let body_str = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
    let json = Yojson.Safe.from_string body_str in
    let messages = Yojson.Safe.Util.(json |> member "messages" |> to_list) in
    let last = List.hd (List.rev messages) in
    let has_limit_msg =
      try
        let c = Yojson.Safe.Util.(last |> member "content") in
        let s = Yojson.Safe.to_string c in
        String.length s > 0
        &&
        try
          ignore (Str.search_forward (Str.regexp_string "limit exceeded") s 0);
          true
        with
        | Not_found -> false
      with
      | _ -> false
    in
    let response_body =
      if has_limit_msg
      then text_body "understood"
      else
        (* Return 3 tool_use blocks in one response *)
        {|{"id":"g3","type":"message","role":"assistant","model":"mock","content":[{"type":"tool_use","id":"tu_a","name":"echo","input":{}},{"type":"tool_use","id":"tu_b","name":"echo","input":{}},{"type":"tool_use","id":"tu_c","name":"echo","input":{}}],"stop_reason":"tool_use","usage":{"input_tokens":10,"output_tokens":5}}|}
    in
    Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  in
  with_mock_server ~port:18204 multi_tool_handler (fun ~sw ~net ~base_url ->
    let tool_calls = ref 0 in
    let tool =
      Tool.create ~name:"echo" ~description:"echo" ~parameters:[] (fun _input ->
        incr tool_calls;
        Ok { content = "ok" })
    in
    let options =
      { Agent.default_options with
        base_url
      ; guardrails = { Guardrails.default with max_tool_calls_per_turn = Some 2 }
      }
    in
    let config = { default_config with max_turns = 3 } in
    let agent = Agent.create ~net ~config ~options ~tools:[ tool ] () in
    (match Agent.run ~sw agent "test" with
     | Ok _ | Error _ -> ());
    (* 3 tool_use blocks exceed limit of 2 — none should execute *)
    Alcotest.(check int) "no tool calls (limit exceeded)" 0 !tool_calls)
;;

let test_no_limit_allows_all () =
  let tool_use_count = ref 3 in
  let tools_seen = ref 0 in
  with_mock_server
    ~port:18205
    (tracking_handler ~tool_use_count ~tools_seen_count:tools_seen)
    (fun ~sw ~net ~base_url ->
       let tool_calls = ref 0 in
       let tool =
         Tool.create ~name:"echo" ~description:"echo" ~parameters:[] (fun _input ->
           incr tool_calls;
           Ok { content = "ok" })
       in
       let options = { Agent.default_options with base_url } in
       let config = { default_config with max_turns = 5 } in
       let agent = Agent.create ~net ~config ~options ~tools:[ tool ] () in
       (match Agent.run ~sw agent "test" with
        | Ok _ | Error _ -> ());
       Alcotest.(check int) "all 3 tool calls executed" 3 !tool_calls)
;;

(* ── edge case tests ─────────────────────────────────── *)

let test_empty_tools_no_crash () =
  with_mock_server ~port:18206 text_only_handler (fun ~sw ~net ~base_url ->
    let options =
      { Agent.default_options with
        base_url
      ; guardrails =
          { Guardrails.default with tool_filter = Guardrails.DenyList [ "nonexistent" ] }
      }
    in
    let agent = Agent.create ~net ~options ~tools:[] () in
    match Agent.run ~sw agent "test" with
    | Ok _ -> ()
    | Error e -> Alcotest.fail (Error.to_string e))
;;

let test_guardrails_default_with_agent () =
  with_mock_server ~port:18207 text_only_handler (fun ~sw ~net ~base_url ->
    let options = { Agent.default_options with base_url } in
    let tools = [ make_tool "a"; make_tool "b" ] in
    let agent = Agent.create ~net ~options ~tools () in
    match Agent.run ~sw agent "test" with
    | Ok resp ->
      let text =
        List.filter_map
          (function
            | Text s -> Some s
            | _ -> None)
          resp.content
        |> String.concat ""
      in
      Alcotest.(check string) "response ok" "ok" text
    | Error e -> Alcotest.fail (Error.to_string e))
;;

(* ── Suite ───────────────────────────────────────────── *)

let () =
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None
  then Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  let open Alcotest in
  run
    "Guardrails_Integration"
    [ ( "tool_filter"
      , [ test_case "deny list hides tool" `Quick test_deny_list_hides_tool
        ; test_case "allow list filters" `Quick test_allow_list_filters
        ; test_case "custom prefix filter" `Quick test_custom_prefix_filter
        ] )
    ; ( "max_tool_calls"
      , [ test_case
            "blocks multi-tool response"
            `Quick
            test_limit_blocks_multi_tool_response
        ; test_case "no limit allows all" `Quick test_no_limit_allows_all
        ] )
    ; ( "edge_cases"
      , [ test_case "empty tools no crash" `Quick test_empty_tools_no_crash
        ; test_case "default guardrails work" `Quick test_guardrails_default_with_agent
        ] )
    ]
;;
