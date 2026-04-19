let text_of_block = function
  | Types.Text t -> Some t
  | _ -> None

(** Render a content block when [~include_tool_blocks:true].  Text goes
    through as-is; [ToolUse] and [ToolResult] are flattened into
    bracket-tagged lines so the CLI can reconstruct the tool history in
    its next turn; everything else (thinking, image, document, audio) is
    dropped — Claude never expects to see its own thinking blocks
    looped back. *)
let render_block_with_tools = function
  | Types.Text t -> Some t
  | Types.ToolUse { id; name; input } ->
    let input_str = Yojson.Safe.to_string input in
    Some (Printf.sprintf "[tool_use id=%s name=%s] %s" id name input_str)
  | Types.ToolResult { tool_use_id; content; is_error; _ } ->
    let tag = if is_error then "tool_result (error)" else "tool_result" in
    Some (Printf.sprintf "[%s id=%s] %s" tag tool_use_id content)
  | _ -> None

let string_of_role = function
  | Types.System -> "System"
  | Types.User -> "User"
  | Types.Assistant -> "Assistant"
  | Types.Tool -> "Tool"

let prompt_of_messages ?(include_tool_blocks = false)
    (messages : Types.message list) =
  let render = if include_tool_blocks then render_block_with_tools
               else text_of_block in
  let text_of_msg (m : Types.message) =
    List.filter_map render m.content |> String.concat "\n"
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

let prompt_with_system_prompt ~prompt ~system_prompt =
  match system_prompt |> Option.map String.trim with
  | None | Some "" -> prompt
  | Some sp ->
    let prompt = String.trim prompt in
    if prompt = "" then
      Printf.sprintf "System:\n%s" sp
    else
      Printf.sprintf "System:\n%s\n\n%s" sp prompt

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

let%test "prompt_with_system_prompt prepends section" =
  prompt_with_system_prompt ~prompt:"hello" ~system_prompt:(Some "be helpful")
  = "System:\nbe helpful\n\nhello"

let%test "prompt_with_system_prompt leaves prompt unchanged when absent" =
  prompt_with_system_prompt ~prompt:"hello" ~system_prompt:None = "hello"

let%test "prompt_of_messages default drops tool blocks" =
  let msgs = [
    msg Assistant [ToolUse { id = "tu_1"; name = "calc"; input = `Assoc [("x", `Int 1)] }];
    msg User [ToolResult { tool_use_id = "tu_1"; content = "2"; is_error = false; json = None }];
    msg User [Text "thanks"];
  ] in
  let out = prompt_of_messages msgs in
  (* Default flatten: tool blocks dropped → only "thanks" remains as tail;
     earlier messages become blank lines. *)
  not (String.length out > 0 && String.starts_with ~prefix:"[tool_use" out)

let%test "prompt_of_messages with include_tool_blocks renders tool_use" =
  let msgs = [
    msg Assistant [ToolUse { id = "tu_1"; name = "calc"; input = `Assoc [("x", `Int 1)] }];
    msg User [Text "?"];
  ] in
  let out = prompt_of_messages ~include_tool_blocks:true msgs in
  let contains s sub =
    let n = String.length s and m = String.length sub in
    if m = 0 then true
    else
      let rec loop i = i + m <= n &&
        (String.sub s i m = sub || loop (i + 1)) in
      loop 0
  in
  contains out "tool_use id=tu_1 name=calc"

let%test "prompt_of_messages with include_tool_blocks renders tool_result error" =
  let msgs = [
    msg User [ToolResult { tool_use_id = "tu_1"; content = "boom";
                            is_error = true; json = None }];
    msg User [Text "retry"];
  ] in
  let out = prompt_of_messages ~include_tool_blocks:true msgs in
  let contains s sub =
    let n = String.length s and m = String.length sub in
    if m = 0 then true
    else
      let rec loop i = i + m <= n &&
        (String.sub s i m = sub || loop (i + 1)) in
      loop 0
  in
  contains out "tool_result (error) id=tu_1"

let%test "render_block_with_tools drops thinking" =
  match render_block_with_tools
          (Types.Thinking { thinking_type = "thinking"; content = "internal" }) with
  | None -> true
  | Some _ -> false
