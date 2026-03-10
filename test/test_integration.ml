open Agent_sdk
open Types

(** Mock Server Logic *)
let mock_handler _conn req body =
  let path = Uri.path (Cohttp.Request.uri req) in
  match path with
  | "/v1/messages" ->
      let body_str = Eio.Buf_read.(of_flow ~max_size:max_int body |> take_all) in
      let json = Yojson.Safe.from_string body_str in
      
      let messages = Yojson.Safe.Util.(json |> member "messages" |> to_list) in
      let last_msg = List.hd (List.rev messages) in
      let content_list = Yojson.Safe.Util.(last_msg |> member "content" |> to_list) in
      let text = 
        match content_list with
        | [] -> "No content"
        | item :: _ -> 
            let open Yojson.Safe.Util in
            match item |> member "text" |> to_string_option with
            | Some t -> t
            | None -> item |> member "content" |> to_string_option |> Option.value ~default:"unknown"
      in
      
      let response_body = 
        if text = "ping" then
          {|{"id":"m1","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"pong"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
        else if text = "use_tool" then
          {|{"id":"m2","type":"message","role":"assistant","model":"c","content":[{"type":"tool_use","id":"t1","name":"calculator","input":{"a":1,"b":2}}],"stop_reason":"tool_use","usage":{"input_tokens":1,"output_tokens":1}}|}
        else if text = "3" then
          {|{"id":"m3","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"The result is 3"}],"stop_reason":"end_turn","usage":{"input_tokens":1,"output_tokens":1}}|}
        else
          {|{"id":"me","type":"message","role":"assistant","model":"c","content":[{"type":"text","text":"err"}],"stop_reason":"end_turn","usage":{"input_tokens":0,"output_tokens":0}}|}
      in
      Cohttp_eio.Server.respond_string ~status:`OK ~body:response_body ()
  | _ ->
      Cohttp_eio.Server.respond_string ~status:`Not_found ~body:"Not found" ()

let test_simple_conversation () =
  let port = 8081 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
      let socket = Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
      let server = Cohttp_eio.Server.make ~callback:mock_handler () in
      Eio.Fiber.fork ~sw (fun () -> Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));

      let agent = Agent.create ~net:env#net ~base_url () in
      match Agent.run ~sw agent "ping" with
      | Ok response ->
          let text = List.filter_map (function Text s -> Some s | _ -> None) response.content |> String.concat "" in
          Alcotest.(check string) "response is pong" "pong" text;
          Eio.Switch.fail sw Exit
      | Error e -> Alcotest.fail e
  with Exit -> ()

let test_tool_use () =
  let port = 8082 in
  let base_url = Printf.sprintf "http://127.0.0.1:%d" port in
  
  Eio_main.run @@ fun env ->
  try
    Eio.Switch.run @@ fun sw ->
      let socket = Eio.Net.listen env#net ~sw ~backlog:128 ~reuse_addr:true (`Tcp (Eio.Net.Ipaddr.V4.loopback, port)) in
      let server = Cohttp_eio.Server.make ~callback:mock_handler () in
      Eio.Fiber.fork ~sw (fun () -> Cohttp_eio.Server.run socket server ~on_error:(fun _ -> ()));

      let calc_tool = Tool.create ~name:"calculator" ~description:"add" ~parameters:[] (fun input -> 
         let a = Yojson.Safe.Util.(input |> member "a" |> to_int) in
         let b = Yojson.Safe.Util.(input |> member "b" |> to_int) in
         Ok (string_of_int (a + b))) in

      let agent = Agent.create ~net:env#net ~base_url ~tools:[calc_tool] () in
      match Agent.run ~sw agent "use_tool" with
      | Ok response ->
          let text = List.filter_map (function Text s -> Some s | _ -> None) response.content |> String.concat "" in
          Alcotest.(check string) "tool result" "The result is 3" text;
          Eio.Switch.fail sw Exit
      | Error e -> Alcotest.fail e
  with Exit -> ()

let () =
  (* Mock server tests need a key in env but don't use it *)
  if Sys.getenv_opt "ANTHROPIC_API_KEY" = None then
    Unix.putenv "ANTHROPIC_API_KEY" "test-mock-key";
  let open Alcotest in
  run "Agent Integration" [
    "mock_server", [
      test_case "simple_conversation" `Quick test_simple_conversation;
      test_case "tool_execution" `Quick test_tool_use;
    ];
  ]
