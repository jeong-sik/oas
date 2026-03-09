(** Markdown skill loading with lightweight frontmatter support.

    The parser intentionally accepts only the small subset of metadata the
    runtime needs for tool policy and prompt composition. *)

type scope =
  | Project
  | User
  | Local
  | Custom of string
[@@deriving show]

type t = {
  name : string;
  description : string option;
  body : string;
  path : string option;
  scope : scope option;
  allowed_tools : string list;
  argument_hint : string option;
  model : string option;
  disable_model_invocation : bool;
  supporting_files : string list;
  metadata : (string * string list) list;
}
[@@deriving show]

let trim = String.trim

let strip_quotes value =
  let value = trim value in
  let len = String.length value in
  if len >= 2 then
    let first = value.[0] in
    let last = value.[len - 1] in
    if (first = '"' && last = '"') || (first = '\'' && last = '\'') then
      String.sub value 1 (len - 2)
    else
      value
  else
    value

let split_csv value =
  let value = trim value in
  let inner =
    if String.length value >= 2 && value.[0] = '[' && value.[String.length value - 1] = ']'
    then String.sub value 1 (String.length value - 2)
    else value
  in
  inner
  |> String.split_on_char ','
  |> List.map strip_quotes
  |> List.filter (fun item -> item <> "")

let assoc_replace key values pairs =
  let without_key = List.remove_assoc key pairs in
  (key, values) :: without_key

let parse_frontmatter markdown =
  let rec drop_leading_blank = function
    | [] -> []
    | line :: rest when trim line = "" -> drop_leading_blank rest
    | lines -> lines
  in
  let lines =
    String.split_on_char '\n' markdown
    |> List.map (fun line ->
      if String.ends_with ~suffix:"\r" line then
        String.sub line 0 (String.length line - 1)
      else
        line)
    |> drop_leading_blank
  in
  match lines with
  | "---" :: rest ->
      let rec consume current_key acc frontmatter body =
        match frontmatter with
        | [] -> (List.rev acc, body)
        | "---" :: remaining -> (List.rev acc, remaining)
        | line :: remaining ->
            let line = trim line in
            if line = "" || String.starts_with ~prefix:"# " line then
              consume current_key acc remaining body
            else if String.starts_with ~prefix:"- " line then
              let item = String.sub line 2 (String.length line - 2) |> strip_quotes in
              (match current_key with
               | Some key ->
                   let existing =
                     match List.assoc_opt key acc with
                     | Some values -> values
                     | None -> []
                   in
                   consume current_key (assoc_replace key (existing @ [ item ]) acc)
                     remaining body
               | None ->
                   consume current_key acc remaining body)
            else
              match String.index_opt line ':' with
              | None -> consume current_key acc remaining body
              | Some idx ->
                  let key = String.sub line 0 idx |> trim |> String.lowercase_ascii in
                  let raw =
                    String.sub line (idx + 1) (String.length line - idx - 1) |> trim
                  in
                  let values = if raw = "" then [] else split_csv raw in
                  consume (Some key) (assoc_replace key values acc) remaining body
      in
      let frontmatter, body_lines = consume None [] rest rest in
      (frontmatter, String.concat "\n" body_lines |> trim)
  | _ -> ([], trim markdown)

let frontmatter_value frontmatter key =
  match List.assoc_opt key frontmatter with
  | Some (value :: _) -> Some value
  | _ -> None

let frontmatter_values frontmatter key =
  match List.assoc_opt key frontmatter with
  | Some values -> values
  | None -> []

let scope_of_string = function
  | "project" -> Project
  | "user" -> User
  | "local" -> Local
  | other -> Custom other

let basename_without_ext path =
  path |> Filename.basename |> Filename.remove_extension

let of_markdown ?path ?scope markdown =
  let frontmatter, body = parse_frontmatter markdown in
  let description = frontmatter_value frontmatter "description" in
  let name =
    match frontmatter_value frontmatter "name", path with
    | Some value, _ -> value
    | None, Some value -> basename_without_ext value
    | None, None -> "skill"
  in
  {
    name;
    description;
    body;
    path;
    scope = (
      match frontmatter_value frontmatter "scope", scope with
      | Some value, _ -> Some (scope_of_string value)
      | None, value -> value
    );
    allowed_tools =
      frontmatter_values frontmatter "allowed-tools"
      @ frontmatter_values frontmatter "allowed_tools";
    argument_hint = (
      match frontmatter_value frontmatter "argument-hint" with
      | Some _ as value -> value
      | None -> frontmatter_value frontmatter "argument_hint"
    );
    model = frontmatter_value frontmatter "model";
    disable_model_invocation =
      List.exists
        (fun value -> String.lowercase_ascii value = "true")
        (frontmatter_values frontmatter "disable-model-invocation"
         @ frontmatter_values frontmatter "disable_model_invocation");
    supporting_files =
      frontmatter_values frontmatter "supporting-files"
      @ frontmatter_values frontmatter "supporting_files";
    metadata = frontmatter;
  }

let load ?scope path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () ->
      let content = really_input_string channel (in_channel_length channel) in
      of_markdown ~path ?scope content)

let load_dir ?scope dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.filter (fun name ->
    Filename.check_suffix name ".md" && not (String.starts_with ~prefix:"." name))
  |> List.sort String.compare
  |> List.map (fun name -> load ?scope (Filename.concat dir name))

let render_prompt ?arguments skill =
  let rendered =
    match arguments with
    | None -> skill.body
    | Some value ->
        skill.body
        |> Str.global_replace (Str.regexp_string "$ARGUMENTS") value
        |> Str.global_replace (Str.regexp_string "{{arguments}}") value
  in
  rendered |> trim

let supporting_file_paths skill =
  match skill.path with
  | None -> skill.supporting_files
  | Some path ->
      let base_dir = Filename.dirname path in
      List.map
        (fun item ->
          if Filename.is_relative item then Filename.concat base_dir item else item)
        skill.supporting_files
