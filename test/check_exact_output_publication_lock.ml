open Asttypes
open Parsetree

let outside_callback = "record_preference_locked is outside with_preference_lock callback"
let namespace_import = "open/include syntax is forbidden in publication lock boundary"
let fail message = Error message

let parse_implementation path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in channel)
    (fun () ->
       let lexbuf = Lexing.from_channel channel in
       Location.init lexbuf path;
       Parse.implementation lexbuf)
;;

let exact_identifier_name expression =
  match expression.pexp_desc with
  | Pexp_ident { txt = Longident.Lident name; _ } -> Some name
  | _ -> None
;;

let simple_identifier expression = exact_identifier_name expression

let binding_name pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> Some name.txt
  | _ -> None
;;

let is_named_application name expression =
  match expression.pexp_desc with
  | Pexp_apply (callee, arguments) ->
    Option.equal String.equal (exact_identifier_name callee) (Some name), arguments
  | _ -> false, []
;;

type collected =
  { mutable recorder_binding_count : int
  ; mutable recorder_top_level_binding_count : int
  ; mutable lock_binding_count : int
  ; mutable lock_top_level_binding_count : int
  ; mutable recorder_calls : (expression * (arg_label * expression) list) list
  ; mutable recorder_first_class_references : expression list
  ; mutable lock_calls : (expression * (arg_label * expression) list) list
  ; mutable namespace_import_seen : bool
  }

let observe_binding collected ~top_level name =
  match name with
  | "record_preference_locked" ->
    if top_level
    then
      collected.recorder_top_level_binding_count
      <- collected.recorder_top_level_binding_count + 1
    else collected.recorder_binding_count <- collected.recorder_binding_count + 1
  | "with_preference_lock" ->
    if top_level
    then
      collected.lock_top_level_binding_count <- collected.lock_top_level_binding_count + 1
    else collected.lock_binding_count <- collected.lock_binding_count + 1
  | _ -> ()
;;

let observe_top_level_bindings collected structure =
  List.iter
    (fun item ->
       match item.pstr_desc with
       | Pstr_value (_, bindings) ->
         List.iter
           (fun binding ->
              match binding_name binding.pvb_pat with
              | Some name -> observe_binding collected ~top_level:true name
              | None -> ())
           bindings
       | _ -> ())
    structure
;;

let collect structure =
  let collected =
    { recorder_binding_count = 0
    ; recorder_top_level_binding_count = 0
    ; lock_binding_count = 0
    ; lock_top_level_binding_count = 0
    ; recorder_calls = []
    ; recorder_first_class_references = []
    ; lock_calls = []
    ; namespace_import_seen = false
    }
  in
  observe_top_level_bindings collected structure;
  let iterator =
    { Ast_iterator.default_iterator with
      pat =
        (fun self pattern ->
          (match pattern.ppat_desc with
           | Ppat_var name -> observe_binding collected ~top_level:false name.txt
           | Ppat_alias (_, name) -> observe_binding collected ~top_level:false name.txt
           | _ -> ());
          Ast_iterator.default_iterator.pat self pattern)
    ; structure_item =
        (fun self item ->
          (match item.pstr_desc with
           | Pstr_open _ | Pstr_include _ -> collected.namespace_import_seen <- true
           | _ -> ());
          Ast_iterator.default_iterator.structure_item self item)
    ; expr =
        (fun self expression ->
          (match expression.pexp_desc with
           | Pexp_open _ -> collected.namespace_import_seen <- true
           | _ -> ());
          let is_recorder, recorder_arguments =
            is_named_application "record_preference_locked" expression
          in
          let is_lock, lock_arguments =
            is_named_application "with_preference_lock" expression
          in
          if is_recorder
          then (
            collected.recorder_calls
            <- (expression, recorder_arguments) :: collected.recorder_calls;
            List.iter (fun (_, argument) -> self.expr self argument) recorder_arguments)
          else (
            (match expression.pexp_desc with
             | Pexp_ident identifier
               when String.equal
                      (Longident.last identifier.txt)
                      "record_preference_locked" ->
               collected.recorder_first_class_references
               <- expression :: collected.recorder_first_class_references
             | _ -> ());
            if is_lock
            then
              collected.lock_calls <- (expression, lock_arguments) :: collected.lock_calls;
            Ast_iterator.default_iterator.expr self expression))
    }
  in
  iterator.structure iterator structure;
  collected
;;

let expression_contains target root =
  let found = ref false in
  let iterator =
    { Ast_iterator.default_iterator with
      expr =
        (fun self expression ->
          if expression == target
          then found := true
          else Ast_iterator.default_iterator.expr self expression)
    }
  in
  iterator.expr iterator root;
  !found
;;

let expression_binds name root =
  let found = ref false in
  let iterator =
    { Ast_iterator.default_iterator with
      pat =
        (fun self pattern ->
          (match pattern.ppat_desc with
           | (Ppat_var bound | Ppat_alias (_, bound)) when String.equal bound.txt name ->
             found := true
           | _ -> ());
          Ast_iterator.default_iterator.pat self pattern)
    }
  in
  iterator.expr iterator root;
  !found
;;

let recorder_store_and_labels arguments =
  match arguments with
  | [ (Nolabel, store)
    ; (Labelled "scope", _)
    ; (Labelled "reservation", _)
    ; (Labelled "candidate", _)
    ; (Labelled "ordinal", _)
    ] ->
    (match simple_identifier store with
     | Some store_name -> Ok store_name
     | None -> fail "record_preference_locked store argument is not a simple identifier")
  | _ -> fail "record_preference_locked is not one saturated direct call"
;;

let lock_store_and_callback arguments =
  match arguments with
  | [ (Nolabel, store); (Nolabel, callback) ] ->
    (match simple_identifier store with
     | Some store_name -> Some (store_name, callback)
     | None -> None)
  | _ -> None
;;

let check structure =
  let collected = collect structure in
  if collected.namespace_import_seen
  then fail namespace_import
  else if
    collected.recorder_binding_count <> 1
    || collected.recorder_top_level_binding_count <> 1
  then fail "record_preference_locked must have exactly one binding"
  else if collected.lock_binding_count <> 1 || collected.lock_top_level_binding_count <> 1
  then fail "with_preference_lock must have exactly one binding"
  else if collected.recorder_first_class_references <> []
  then
    fail
      "record_preference_locked escaped as a first-class, partial, or qualified reference"
  else (
    match collected.recorder_calls with
    | [ (recorder_call, arguments) ] ->
      (match recorder_store_and_labels arguments with
       | Error _ as error -> error
       | Ok recorder_store ->
         let containing_callbacks =
           List.filter_map
             (fun (_, lock_arguments) ->
                match lock_store_and_callback lock_arguments with
                | Some (lock_store, callback)
                  when expression_contains recorder_call callback ->
                  Some (lock_store, callback)
                | Some _ | None -> None)
             collected.lock_calls
         in
         (match containing_callbacks with
          | [] -> fail outside_callback
          | [ (lock_store, callback) ] ->
            if not (String.equal lock_store recorder_store)
            then fail "preference lock and recorder use different store identifiers"
            else if expression_binds recorder_store callback
            then fail "preference store identifier is shadowed inside lock callback"
            else Ok ()
          | _ -> fail "record_preference_locked is nested under multiple lock callbacks"))
    | [] -> fail "record_preference_locked has no saturated direct call"
    | _ -> fail "record_preference_locked has more than one direct call")
;;

let report_error message =
  prerr_endline ("exact-output publication lock violation: " ^ message)
;;

let check_file path =
  try check (parse_implementation path) with
  | Sys_error message -> fail message
  | Syntaxerr.Error _ -> fail ("syntax error while parsing " ^ path)
;;

let expected_error_of_flag = function
  | "--expect-outside-callback" -> Some outside_callback
  | "--expect-namespace-import" -> Some namespace_import
  | "--expect-shadowed-lock-binding" ->
    Some "with_preference_lock must have exactly one binding"
  | "--expect-noncanonical-recorder" ->
    Some
      "record_preference_locked escaped as a first-class, partial, or qualified reference"
  | "--expect-shadowed-store" ->
    Some "preference store identifier is shadowed inside lock callback"
  | _ -> None
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; path ] ->
    (match check_file path with
     | Ok () -> ()
     | Error message ->
       report_error message;
       exit 1)
  | [ _; flag; path ] ->
    (match expected_error_of_flag flag with
     | None ->
       report_error ("unknown negative-fixture mode: " ^ flag);
       exit 2
     | Some expected ->
       (match check_file path with
        | Error message when String.equal message expected -> ()
        | Error message ->
          report_error ("negative fixture failed for the wrong reason: " ^ message);
          exit 2
        | Ok () ->
          report_error "negative fixture unexpectedly passed";
          exit 2))
  | _ ->
    prerr_endline "usage: check_exact_output_publication_lock [--expect-*] SOURCE.ml";
    exit 2
;;
