let text_of_block = function
  | Types.Text t -> Some t
  | _ -> None

let string_of_role = function
  | Types.System -> "System"
  | Types.User -> "User"
  | Types.Assistant -> "Assistant"
  | Types.Tool -> "Tool"

let prompt_of_messages (messages : Types.message list) =
  let text_of_msg (m : Types.message) =
    List.filter_map text_of_block m.content |> String.concat "\n"
  in
  match List.rev messages with
  | [] -> ""
  | [m] -> text_of_msg m
  | last :: earlier ->
    let context = List.rev earlier |> List.map (fun (m : Types.message) ->
      Printf.sprintf "%s: %s" (string_of_role m.role) (text_of_msg m)
    ) |> String.concat "\n\n" in
    Printf.sprintf "%s\n\n%s" context (text_of_msg last)

let non_system_messages (messages : Types.message list) =
  List.filter (fun (m : Types.message) -> m.role <> Types.System) messages

let system_prompt_of ~(req_config : Provider_config.t)
    (messages : Types.message list) =
  match req_config.system_prompt with
  | Some sp -> Some sp
  | None ->
    match messages with
    | { Types.role = System; content; _ } :: _ ->
      Some (List.filter_map text_of_block content |> String.concat "\n")
    | _ -> None

[@@@coverage off]

let msg role content : Types.message =
  { role; content; name = None; tool_call_id = None }

let%test "prompt_of_messages single user" =
  prompt_of_messages [msg User [Text "hello"]] = "hello"

let%test "prompt_of_messages empty" =
  prompt_of_messages [] = ""

let%test "prompt_of_messages multi-turn has context" =
  let msgs = [
    msg User [Text "hi"];
    msg Assistant [Text "hello"];
    msg User [Text "how are you?"];
  ] in
  let result = prompt_of_messages msgs in
  String.length result > 0
  && result |> String.split_on_char '\n' |> List.length > 1

let%test "non_system_messages drops system" =
  let filtered = non_system_messages [
    msg System [Text "be helpful"];
    msg User [Text "hi"];
  ] in
  List.length filtered = 1

let%test "system_prompt_of prefers explicit req_config" =
  let req = Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:""
    ~system_prompt:"explicit" () in
  system_prompt_of ~req_config:req [msg System [Text "from msg"]]
    = Some "explicit"

let%test "system_prompt_of falls back to system message" =
  let req = Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" () in
  system_prompt_of ~req_config:req [msg System [Text "be helpful"]]
    = Some "be helpful"

let%test "system_prompt_of returns None when absent" =
  let req = Provider_config.make ~kind:Claude_code ~model_id:"" ~base_url:"" () in
  system_prompt_of ~req_config:req [msg User [Text "hi"]] = None
