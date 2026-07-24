open Asttypes
open Parsetree

let outside_callback =
  "record_preference_locked is outside with_preference_lock callback"
;;

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

let identifier_name expression =
  match expression.pexp_desc with
  | Pexp_ident identifier -> Some (Longident.last identifier.txt)
  | _ -> None
;;

let simple_identifier expression =
  match expression.pexp_desc with
  | Pexp_ident identifier ->
    (match Longident.flatten identifier.txt with
     | [ name ] -> Some name
     | _ -> None)
  | _ -> None
;;

let binding_name pattern =
  match pattern.ppat_desc with
  | Ppat_var name -> Some name.txt
  | _ -> None
;;

let is_named_application name expression =
  match expression.pexp_desc with
  | Pexp_apply (callee, arguments) ->
    Option.equal String.equal (identifier_name callee) (Some name), arguments
  | _ -> false, []
;;

type collected =
  { mutable recorder_binding_count : int
  ; mutable recorder_calls : (expression * (arg_label * expression) list) list
  ; mutable recorder_first_class_references : expression list
  ; mutable lock_calls : (expression * (arg_label * expression) list) list
  }

let collect structure =
  let collected =
    { recorder_binding_count = 0
    ; recorder_calls = []
    ; recorder_first_class_references = []
    ; lock_calls = []
    }
  in
  let iterator =
    { Ast_iterator.default_iterator with
      pat =
        (fun self pattern ->
           (match binding_name pattern with
            | Some "record_preference_locked" ->
              collected.recorder_binding_count <- collected.recorder_binding_count + 1
            | Some _ | None -> ());
           Ast_iterator.default_iterator.pat self pattern)
    ; expr =
        (fun self expression ->
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
               collected.lock_calls
               <- (expression, lock_arguments) :: collected.lock_calls;
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
           (match binding_name pattern with
            | Some bound when String.equal bound name -> found := true
            | Some _ | None -> ());
           Ast_iterator.default_iterator.pat self pattern)
    }
  in
  iterator.expr iterator root;
  !found
;;

let recorder_store_and_labels arguments =
  match arguments with
  | [ Nolabel, store
    ; Labelled "scope", _
    ; Labelled "reservation", _
    ; Labelled "candidate", _
    ; Labelled "ordinal", _
    ] ->
    (match simple_identifier store with
     | Some store_name -> Ok store_name
     | None -> fail "record_preference_locked store argument is not a simple identifier")
  | _ -> fail "record_preference_locked is not one saturated direct call"
;;

let lock_store_and_callback arguments =
  match arguments with
  | [ Nolabel, store; Nolabel, callback ] ->
    (match simple_identifier store with
     | Some store_name -> Some (store_name, callback)
     | None -> None)
  | _ -> None
;;

let check structure =
  let collected = collect structure in
  if collected.recorder_binding_count <> 1
  then fail "record_preference_locked must have exactly one binding"
  else if collected.recorder_first_class_references <> []
  then fail "record_preference_locked escaped as a first-class or partial reference"
  else
    match collected.recorder_calls with
    | [ recorder_call, arguments ] ->
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
          | [ lock_store, callback ] ->
            if not (String.equal lock_store recorder_store)
            then fail "preference lock and recorder use different store identifiers"
            else if expression_binds recorder_store callback
            then fail "preference store identifier is shadowed inside lock callback"
            else Ok ()
          | _ -> fail "record_preference_locked is nested under multiple lock callbacks"))
    | [] -> fail "record_preference_locked has no saturated direct call"
    | _ -> fail "record_preference_locked has more than one direct call"
;;

let report_error message =
  prerr_endline ("exact-output publication lock violation: " ^ message)
;;

let check_file path =
  try check (parse_implementation path) with
  | Sys_error message -> fail message
  | Syntaxerr.Error _ -> fail ("syntax error while parsing " ^ path)
;;

let () =
  match Array.to_list Sys.argv with
  | [ _; path ] ->
    (match check_file path with
     | Ok () -> ()
     | Error message ->
       report_error message;
       exit 1)
  | [ _; "--expect-outside-callback"; path ] ->
    (match check_file path with
     | Error message when String.equal message outside_callback -> ()
     | Error message ->
       report_error
         ("negative fixture failed for the wrong reason: " ^ message);
       exit 2
     | Ok () ->
       report_error "negative fixture unexpectedly passed";
       exit 2)
  | _ ->
    prerr_endline
      "usage: check_exact_output_publication_lock \
       [--expect-outside-callback] SOURCE.ml";
    exit 2
;;
