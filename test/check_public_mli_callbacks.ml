open Asttypes
open Parsetree

module String_map = Map.Make (String)
module String_set = Set.Make (String)

let parse_interface path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
       let lexbuf = Lexing.from_channel channel in
       Location.init lexbuf path;
       Parse.interface lexbuf)
;;

let aliases signature =
  List.fold_left
    (fun aliases item ->
       match item.psig_desc with
       | Psig_type (_, declarations) ->
         List.fold_left
           (fun aliases declaration ->
              match declaration.ptype_manifest with
              | None -> aliases
              | Some manifest ->
                String_map.add declaration.ptype_name.txt manifest aliases)
           aliases
           declarations
       | _ -> aliases)
    String_map.empty
    signature
;;

let type_facts core_type =
  let contains_arrow = ref false in
  let references = ref String_set.empty in
  let iterator =
    { Ast_iterator.default_iterator with
      typ =
        (fun self current ->
          (match current.ptyp_desc with
           | Ptyp_arrow _ -> contains_arrow := true
           | Ptyp_constr ({ txt = Longident.Lident name; _ }, _) ->
             references := String_set.add name !references
           | _ -> ());
          Ast_iterator.default_iterator.typ self current)
    }
  in
  iterator.typ iterator core_type;
  !contains_arrow, !references
;;

let type_contains_callback aliases core_type =
  let rec inspect visiting current =
    let contains_arrow, references = type_facts current in
    contains_arrow
    || String_set.exists
         (fun name ->
            if String_set.mem name visiting
            then false
            else
              match String_map.find_opt name aliases with
              | None -> false
              | Some manifest -> inspect (String_set.add name visiting) manifest)
         references
  in
  inspect String_set.empty core_type
;;

let rec argument_types reversed = function
  | { ptyp_desc = Ptyp_arrow (_, argument, result); _ } ->
    argument_types (argument :: reversed) result
  | _ -> List.rev reversed
;;

let callback_values path signature =
  let aliases = aliases signature in
  List.filter_map
    (fun item ->
       match item.psig_desc with
       | Psig_value description
         when List.exists
                (type_contains_callback aliases)
                (argument_types [] description.pval_type) ->
         Some (path, description.pval_name.txt)
       | _ -> None)
    signature
;;

let report (path, name) =
  prerr_endline
    (Printf.sprintf
       "%s: public value %s accepts a callback-bearing argument type"
       path
       name)
;;

let () =
  let rec parse allow expect paths = function
    | [] -> allow, expect, List.rev paths
    | "--allow" :: name :: rest ->
      parse (String_set.add name allow) expect paths rest
    | "--expect-callback" :: rest -> parse allow true paths rest
    | path :: rest -> parse allow expect (path :: paths) rest
  in
  let allow, expect_callback, paths =
    match Array.to_list Sys.argv with
    | _ :: arguments -> parse String_set.empty false [] arguments
    | [] -> assert false
  in
  if paths = []
  then (
    prerr_endline "usage: check_public_mli_callbacks [flags] <interface>...";
    exit 2);
  let callbacks =
    List.concat_map
      (fun path ->
         try callback_values path (parse_interface path) with
         | Sys_error detail ->
           prerr_endline detail;
           exit 2
         | Syntaxerr.Error _ ->
           prerr_endline ("syntax error while parsing " ^ path);
           exit 2)
      paths
  in
  let violations =
    List.filter (fun (_, name) -> not (String_set.mem name allow)) callbacks
  in
  if expect_callback
  then (
    if violations = []
    then (
      prerr_endline "negative fixture did not expose a callback-bearing public value";
      exit 1))
  else if violations <> []
  then (
    List.iter report violations;
    exit 1)
;;
