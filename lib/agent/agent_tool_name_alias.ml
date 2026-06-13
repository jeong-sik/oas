let has_assoc_key key fields = List.exists (fun (k, _) -> String.equal k key) fields

let assoc_first_string keys fields =
  let rec loop = function
    | [] -> None
    | key :: rest ->
      (match List.assoc_opt key fields with
       | Some (`String value) -> Some value
       | _ -> loop rest)
  in
  loop keys
;;

let select_assoc_keys keys fields =
  List.filter_map
    (fun key ->
       match List.assoc_opt key fields with
       | Some value -> Some (key, value)
       | None -> None)
    keys
;;

let normalize_read_input = function
  | `Assoc fields as input ->
    if has_assoc_key "file_path" fields
    then input
    else (
      match List.assoc_opt "path" fields with
      | Some path -> `Assoc (("file_path", path) :: fields)
      | None -> input)
  | input -> input
;;

let normalize_search_input = function
  | `Assoc fields as input ->
    if has_assoc_key "pattern" fields
    then input
    else (
      match assoc_first_string [ "query"; "regex"; "name"; "needle" ] fields with
      | Some pattern -> `Assoc (("pattern", `String pattern) :: fields)
      | None -> input)
  | input -> input
;;

let simple_argv_unsafe_char = function
  | '\000'
  | '\n'
  | '\r'
  | '\t'
  | '\''
  | '"'
  | '`'
  | '|'
  | '&'
  | ';'
  | '<'
  | '>'
  | '('
  | ')'
  | '{'
  | '}'
  | '['
  | ']'
  | '$'
  | '*'
  | '?'
  | '!'
  | '~'
  | '#' -> true
  | _ -> false
;;

let simple_command_tokens command =
  let command = String.trim command in
  if String.length command = 0 || String.exists simple_argv_unsafe_char command
  then None
  else (
    let tokens =
      command
      |> String.split_on_char ' '
      |> List.filter (fun token -> not (String.equal token ""))
    in
    match tokens with
    | [] -> None
    | _ -> Some tokens)
;;

let normalize_execute_input = function
  | `Assoc fields as input ->
    if has_assoc_key "executable" fields || has_assoc_key "pipeline" fields
    then input
    else (
      match
        match assoc_first_string [ "command"; "cmd" ] fields with
        | Some command -> simple_command_tokens command
        | None -> None
      with
      | Some (executable :: argv) ->
        let passthrough =
          select_assoc_keys
            [ "cwd"; "env"; "timeout_sec"; "stdin"; "stdout"; "stderr" ]
            fields
        in
        `Assoc
          (("executable", `String executable)
           :: ("argv", `List (List.map (fun arg -> `String arg) argv))
           :: passthrough)
      | Some [] | None -> input)
  | input -> input
;;

(** Mutable registry for consumer-registered aliases.
    Maps alias -> canonical tool name. *)
let registry = Hashtbl.create 16
let registry_mu = Eio.Mutex.create ()

let register_alias ~alias ~canonical =
  Eio.Mutex.use_rw ~protect:true registry_mu
  @@ fun () -> Hashtbl.replace registry alias canonical

let resolve_alias alias =
  Eio.Mutex.use_ro registry_mu @@ fun () -> Hashtbl.find_opt registry alias

let resolve ~requested ~input =
  match requested with
  | "Read" -> Some ("ReadFile", normalize_read_input input)
  | "Grep" | "Search" | "Find" -> Some ("SearchFiles", normalize_search_input input)
  | "Bash" | "Shell" | "execute_command" -> Some ("Execute", normalize_execute_input input)
  | _ ->
    (match resolve_alias requested with
     | Some canonical -> Some (canonical, input)
     | None -> None)
;;
