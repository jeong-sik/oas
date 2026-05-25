open Alcotest
open Llm_provider

let contains_substring ~sub text =
  let sub_len = String.length sub in
  let text_len = String.length text in
  let rec loop idx =
    if idx + sub_len > text_len
    then false
    else if String.sub text idx sub_len = sub
    then true
    else loop (idx + 1)
  in
  sub_len = 0 || loop 0
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
    let endpoint = Printf.sprintf "http://127.0.0.1:%d" port in
    f ~sw ~net:env#net ~endpoint;
    Eio.Switch.fail sw Exit
  with
  | Unix.Unix_error (Unix.EPERM, "bind", _) -> Alcotest.skip ()
  | Exit -> ()
;;

let test_save_restore_and_erase_send_expected_requests () =
  let seen = ref [] in
  let handler _conn req body =
    let uri = Cohttp.Request.uri req in
    let path = Uri.path uri in
    let action = Uri.get_query_param uri "action" |> Option.value ~default:"" in
    let body_text = Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) in
    seen := (path, action, body_text) :: !seen;
    Cohttp_eio.Server.respond_string ~status:`OK ~body:"{}" ()
  in
  with_mock_server ~port:18343 handler (fun ~sw ~net ~endpoint ->
    check
      (result unit string)
      "save"
      (Ok ())
      (Slot_cache.save ~sw ~net ~endpoint ~slot_id:7 ~filename:"slot.bin");
    check
      (result unit string)
      "restore"
      (Ok ())
      (Slot_cache.restore ~sw ~net ~endpoint ~slot_id:7 ~filename:"slot.bin");
    check
      (result unit string)
      "erase"
      (Ok ())
      (Slot_cache.erase ~sw ~net ~endpoint ~slot_id:7));
  let entries = List.rev !seen in
  check int "request count" 3 (List.length entries);
  check
    (list (triple string string string))
    "requests"
    [ "/slots/7", "save", {|{"filename":"slot.bin"}|}
    ; "/slots/7", "restore", {|{"filename":"slot.bin"}|}
    ; "/slots/7", "erase", "{}"
    ]
    entries
;;

let test_non_2xx_response_surfaces_action_and_body () =
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`Internal_server_error ~body:"slot failed" ()
  in
  with_mock_server ~port:18344 handler (fun ~sw ~net ~endpoint ->
    match Slot_cache.restore ~sw ~net ~endpoint ~slot_id:3 ~filename:"missing.bin" with
    | Ok () -> fail "expected restore error"
    | Error msg ->
      check
        bool
        "mentions action"
        true
        (contains_substring ~sub:"slot restore HTTP 500" msg);
      check bool "mentions body" true (contains_substring ~sub:"slot failed" msg))
;;

let test_non_2xx_response_truncates_long_body () =
  let long_body = String.make 240 'x' ^ "tail" in
  let handler _conn _req body =
    ignore (Eio.Buf_read.(of_flow ~max_size:(1024 * 1024) body |> take_all) : string);
    Cohttp_eio.Server.respond_string ~status:`Bad_gateway ~body:long_body ()
  in
  with_mock_server ~port:18345 handler (fun ~sw ~net ~endpoint ->
    match Slot_cache.save ~sw ~net ~endpoint ~slot_id:4 ~filename:"slot.bin" with
    | Ok () -> fail "expected save error"
    | Error msg ->
      check bool "mentions status" true (contains_substring ~sub:"slot save HTTP 502" msg);
      check bool "truncated body" true (String.length msg < String.length long_body))
;;

let () =
  run
    "slot-cache-http"
    [ ( "slot api"
      , [ test_case
            "save restore erase requests"
            `Quick
            test_save_restore_and_erase_send_expected_requests
        ; test_case
            "non-2xx response"
            `Quick
            test_non_2xx_response_surfaces_action_and_body
        ; test_case
            "non-2xx long body is truncated"
            `Quick
            test_non_2xx_response_truncates_long_body
        ] )
    ]
;;
