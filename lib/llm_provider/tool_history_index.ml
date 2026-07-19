open Types

type t = (string, string) Hashtbl.t

type error =
  | Conflicting_tool_use_id of
      { tool_use_id : string
      ; first_name : string
      ; conflicting_name : string
      }
  | Missing_tool_use_id of { tool_use_id : string }

let error_to_string = function
  | Conflicting_tool_use_id { tool_use_id; first_name; conflicting_name } ->
    Printf.sprintf
      "conflicting ToolUse identity %S names %S and %S"
      tool_use_id
      first_name
      conflicting_name
  | Missing_tool_use_id { tool_use_id } ->
    Printf.sprintf "ToolResult identity %S has no matching ToolUse" tool_use_id
;;

let of_messages (messages : message list) =
  (* The table is mutated only while constructing this function-local value.
     [t] is abstract and exposes no mutation operation, so successful return
     seals an immutable lookup snapshot. Hashtbl gives one O(n) history pass
     and expected O(1) result correlation instead of a scan per ToolResult. *)
  let index = Hashtbl.create 0 in
  let rec add_blocks = function
    | [] -> Ok ()
    | ToolUse { id; name; _ } :: rest ->
      (match Hashtbl.find_opt index id with
       | None ->
         Hashtbl.add index id name;
         add_blocks rest
       | Some first_name when String.equal first_name name -> add_blocks rest
       | Some first_name ->
         Error
           (Conflicting_tool_use_id
              { tool_use_id = id; first_name; conflicting_name = name }))
    | ( Text _
      | Thinking _
      | ReasoningDetails _
      | RedactedThinking _
      | ToolResult _
      | Image _
      | Document _
      | Audio _ )
      :: rest -> add_blocks rest
  in
  let rec add_messages = function
    | [] -> Ok index
    | (message : message) :: rest ->
      (match add_blocks message.content with
       | Error _ as error -> error
       | Ok () -> add_messages rest)
  in
  add_messages messages
;;

let resolve index ~tool_use_id =
  match Hashtbl.find_opt index tool_use_id with
  | Some name -> Ok name
  | None -> Error (Missing_tool_use_id { tool_use_id })
;;

[@@@coverage off]

let message content : message =
  { role = Assistant; content; name = None; tool_call_id = None; metadata = [] }
;;

let tool_use id name = ToolUse { id; name; input = `Assoc [] }

let%test "same identity and name is idempotent" =
  match
    of_messages [ message [ tool_use "call-1" "lookup"; tool_use "call-1" "lookup" ] ]
  with
  | Ok index ->
    Hashtbl.length index = 1 && resolve index ~tool_use_id:"call-1" = Ok "lookup"
  | Error _ -> false
;;

let%test "conflicting duplicate identity is rejected" =
  match
    of_messages [ message [ tool_use "call-1" "lookup"; tool_use "call-1" "write" ] ]
  with
  | Error
      (Conflicting_tool_use_id
         { tool_use_id = "call-1"; first_name = "lookup"; conflicting_name = "write" }) ->
    true
  | Ok _ | Error _ -> false
;;

let%test "missing identity is explicit" =
  match of_messages [] with
  | Error _ -> false
  | Ok index ->
    resolve index ~tool_use_id:"absent"
    = Error (Missing_tool_use_id { tool_use_id = "absent" })
;;
