type path = string

type violation =
  | Array_without_items of path
  | Object_without_properties of path
  | Root_not_object of path
  | Object_open_additional_properties of path

let violation_to_string = function
  | Array_without_items p -> Printf.sprintf "%s: array schema has no \"items\"" p
  | Object_without_properties p ->
    Printf.sprintf "%s: object schema has no \"properties\"" p
  | Root_not_object p -> Printf.sprintf "%s: schema root is not an object schema" p
  | Object_open_additional_properties p ->
    Printf.sprintf "%s: object allows extra keys, which strict mode forbids" p
;;

let violations_to_string vs = String.concat ", " (List.map violation_to_string vs)
let root_path = "<root>"
let child path segment = if path = root_path then segment else path ^ "." ^ segment

(* Schema-level [type] as declared. A JSON Schema [type] may be a string or an
   array of strings; anything else (absent, or a non-string member) is treated
   as undeclared, because the projection only rewrites shapes it can name. *)
type declared_type =
  | Type_object
  | Type_array
  | Type_other
  | Type_absent

let declared_type_of fields =
  match List.assoc_opt "type" fields with
  | Some (`String "object") -> Type_object
  | Some (`String "array") -> Type_array
  | Some (`String _) -> Type_other
  | Some (`List members) ->
    if List.exists (fun m -> m = `String "object") members
    then Type_object
    else if List.exists (fun m -> m = `String "array") members
    then Type_array
    else Type_other
  | Some _ | None -> Type_absent
;;

(* Widen a property's declared [type] with "null" so the property can be
   promoted into [required] without forcing the model to invent a value.

   Where the property declares a top-level [type], null is added to it. Where
   it does not — [enum]-only, [const], [$ref], [anyOf]/[oneOf]/[allOf] — there
   is no [type] to widen, so nullability is expressed the only other way the
   subset allows: an [anyOf] carrying the original schema and a [null] branch.
   The earlier version returned such schemas unchanged, which combined with
   the unconditional [required] promotion below to silently convert an
   optional [enum]/[$ref] property into a mandatory one. *)
let make_nullable (schema : Yojson.Safe.t) : Yojson.Safe.t =
  let anyof_null branches =
    if List.exists (fun b -> b = `Assoc [ "type", `String "null" ]) branches
    then None (* already reachable *)
    else Some (branches @ [ `Assoc [ "type", `String "null" ] ])
  in
  match schema with
  | `Assoc fields ->
    (match List.assoc_opt "type" fields with
     | Some (`String "null") -> schema
     | Some (`String t) ->
       `Assoc
         (List.map
            (fun (k, v) ->
               if k = "type" then k, `List [ `String t; `String "null" ] else k, v)
            fields)
     | Some (`List members) when List.exists (fun m -> m = `String "null") members ->
       schema
     | Some (`List members) ->
       `Assoc
         (List.map
            (fun (k, v) ->
               if k = "type" then k, `List (members @ [ `String "null" ]) else k, v)
            fields)
     | Some _ | None ->
       (* No usable top-level [type]. If the schema is already an [anyOf], add
          the null branch to it rather than nesting; otherwise wrap. *)
       (match List.assoc_opt "anyOf" fields with
        | Some (`List branches) ->
          (match anyof_null branches with
           | None -> schema
           | Some widened ->
             `Assoc
               (List.map
                  (fun (k, v) -> if k = "anyOf" then k, `List widened else k, v)
                  fields))
        | Some _ | None ->
          `Assoc [ "anyOf", `List [ schema; `Assoc [ "type", `String "null" ] ] ]))
  | `Bool _ | `Null | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> schema
;;

let set_field fields key value =
  if List.mem_assoc key fields
  then List.map (fun (k, v) -> if k = key then k, value else k, v) fields
  else fields @ [ key, value ]
;;

let required_names fields =
  match List.assoc_opt "required" fields with
  | Some (`List names) ->
    List.filter_map
      (function
        | `String s -> Some s
        | _ -> None)
      names
  | Some _ | None -> []
;;

(* Accumulates violations while rewriting, so one pass reports every problem
   rather than only the first. *)
let rec project_at ~path ~acc (schema : Yojson.Safe.t) : Yojson.Safe.t * violation list =
  match schema with
  | `Assoc fields ->
    (match declared_type_of fields with
     | Type_object -> project_object ~path ~acc fields
     | Type_array -> project_array ~path ~acc fields
     | Type_other | Type_absent -> project_combinators ~path ~acc fields)
  | `Bool _ | `Null | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ -> schema, acc

and project_object ~path ~acc fields =
  match List.assoc_opt "properties" fields with
  | Some (`Assoc props) ->
    let already_required = required_names fields in
    let projected_props, acc =
      List.fold_left
        (fun (out, acc) (name, prop_schema) ->
           let projected, acc = project_at ~path:(child path name) ~acc prop_schema in
           let projected =
             if List.mem name already_required then projected else make_nullable projected
           in
           (name, projected) :: out, acc)
        ([], acc)
        props
    in
    let projected_props = List.rev projected_props in
    let all_names = List.map fst projected_props in
    let fields = set_field fields "properties" (`Assoc projected_props) in
    let fields =
      set_field fields "required" (`List (List.map (fun n -> `String n) all_names))
    in
    (* Only inject [additionalProperties: false] when the caller left it
       unset — that is the lossless case, since the caller declared no extra
       keys. An explicit [false] is kept. An explicit [true], or a schema-valued
       [additionalProperties], is an open-object declaration the strict subset
       cannot express; overwriting it to [false] would silently forbid keys the
       caller permitted, so it is reported as a violation and the request
       degrades to non-strict with the caller's schema intact. *)
    let fields, acc =
      match List.assoc_opt "additionalProperties" fields with
      | None -> set_field fields "additionalProperties" (`Bool false), acc
      | Some (`Bool false) -> fields, acc
      | Some (`Bool true | `Assoc _) ->
        fields, Object_open_additional_properties path :: acc
      | Some _ -> set_field fields "additionalProperties" (`Bool false), acc
    in
    let projected, acc = project_defs ~path ~acc fields in
    projected, acc
  | Some _ | None -> `Assoc fields, Object_without_properties path :: acc

and project_array ~path ~acc fields =
  match List.assoc_opt "items" fields with
  | Some items ->
    let projected_items, acc = project_at ~path:(child path "items") ~acc items in
    let fields = set_field fields "items" projected_items in
    project_defs ~path ~acc fields
  | None -> `Assoc fields, Array_without_items path :: acc

(* [anyOf] / [allOf] / [oneOf] branches and [$defs] entries are schemas in
   their own right and carry the same structural requirements, so the
   projection descends into them wherever they appear. *)
and project_combinators ~path ~acc fields =
  let project_branch_list ~key (fields, acc) =
    match List.assoc_opt key fields with
    | Some (`List branches) ->
      let projected, acc =
        List.fold_left
          (fun (out, acc) branch ->
             let projected, acc = project_at ~path:(child path key) ~acc branch in
             projected :: out, acc)
          ([], acc)
          branches
      in
      set_field fields key (`List (List.rev projected)), acc
    | Some _ | None -> fields, acc
  in
  let fields, acc =
    (fields, acc)
    |> project_branch_list ~key:"anyOf"
    |> project_branch_list ~key:"allOf"
    |> project_branch_list ~key:"oneOf"
  in
  project_defs ~path ~acc fields

and project_defs ~path ~acc fields =
  match List.assoc_opt "$defs" fields with
  | Some (`Assoc defs) ->
    let projected, acc =
      List.fold_left
        (fun (out, acc) (name, def) ->
           let projected, acc =
             project_at ~path:(child path ("$defs." ^ name)) ~acc def
           in
           (name, projected) :: out, acc)
        ([], acc)
        defs
    in
    `Assoc (set_field fields "$defs" (`Assoc (List.rev projected))), acc
  | Some _ | None -> `Assoc fields, acc
;;

let project (schema : Yojson.Safe.t) : (Yojson.Safe.t, violation list) result =
  match schema with
  | `Assoc fields when declared_type_of fields = Type_object ->
    let projected, violations = project_at ~path:root_path ~acc:[] schema in
    if violations = [] then Ok projected else Error (List.rev violations)
  | `Assoc _ | `Bool _ | `Null | `Int _ | `Intlit _ | `Float _ | `String _ | `List _ ->
    Error [ Root_not_object root_path ]
;;

(* ── Inline tests ──────────────────────────────────────────

   The expected shapes below are the ones measured against
   api.openai.com/v1/chat/completions with gpt-5.5 on 2026-07-22: the
   "accepted" cases returned 200 and the "violation" cases returned HTTP 400
   with the quoted provider message. *)

let obj fields = `Assoc fields
let str = `Assoc [ "type", `String "string" ]

let%test
    "scalar object gains additionalProperties:false and promotes every property into \
     required"
  =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "city", str; "note", str ]
      ; "required", `List [ `String "city" ]
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok (`Assoc fields) ->
    let open Yojson.Safe.Util in
    let projected = `Assoc fields in
    projected |> member "additionalProperties" = `Bool false
    && projected |> member "required" = `List [ `String "city"; `String "note" ]
    (* [city] was already required, so its declared type is untouched. *)
    && projected
       |> member "properties"
       |> member "city"
       |> member "type"
       = `String "string"
    (* [note] was optional; the documented emulation is a null union, which
       keeps an absent value expressible while satisfying "all fields must be
       required". *)
    && projected
       |> member "properties"
       |> member "note"
       |> member "type"
       = `List [ `String "string"; `String "null" ]
  | Ok _ -> false
;;

let%test "array property without items is a violation, not a guess" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "tags", `Assoc [ "type", `String "array" ] ]
      ; "required", `List [ `String "tags" ]
      ]
  in
  match project schema with
  | Error [ Array_without_items "tags" ] -> true
  | Error _ | Ok _ -> false
;;

let%test "nested object without properties is a violation" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "meta", `Assoc [ "type", `String "object" ] ]
      ; "required", `List [ `String "meta" ]
      ]
  in
  match project schema with
  | Error [ Object_without_properties "meta" ] -> true
  | Error _ | Ok _ -> false
;;

let%test "non-object root is a violation" =
  match project (`Assoc [ "type", `String "array"; "items", str ]) with
  | Error [ Root_not_object _ ] -> true
  | Error _ | Ok _ -> false
;;

let%test "every violation in one schema is reported, in schema order" =
  let schema =
    obj
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc
            [ "tags", `Assoc [ "type", `String "array" ]
            ; "meta", `Assoc [ "type", `String "object" ]
            ] )
      ]
  in
  match project schema with
  | Error [ Array_without_items "tags"; Object_without_properties "meta" ] -> true
  | Error _ | Ok _ -> false
;;

let%test "projection recurses into array items and nested objects" =
  let schema =
    obj
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc
            [ ( "steps"
              , `Assoc
                  [ "type", `String "array"
                  ; ( "items"
                    , `Assoc
                        [ "type", `String "object"
                        ; "properties", `Assoc [ "explanation", str ]
                        ] )
                  ] )
            ] )
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected ->
    let open Yojson.Safe.Util in
    projected
    |> member "properties"
    |> member "steps"
    |> member "items"
    |> member "additionalProperties"
    = `Bool false
;;

let%test "a property whose type is already nullable is not widened twice" =
  let schema =
    obj
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc [ "note", `Assoc [ "type", `List [ `String "string"; `String "null" ] ] ]
        )
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected ->
    let open Yojson.Safe.Util in
    projected
    |> member "properties"
    |> member "note"
    |> member "type"
    = `List [ `String "string"; `String "null" ]
;;

let%test "already-strict schemas are a fixpoint" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "city", str ]
      ; "required", `List [ `String "city" ]
      ; "additionalProperties", `Bool false
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok once ->
    (match project once with
     | Error _ -> false
     | Ok twice -> once = twice && once = schema)
;;

let%test "$defs and anyOf branches are projected too" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "city", str ]
      ; ( "$defs"
        , `Assoc
            [ ( "address"
              , `Assoc [ "type", `String "object"; "properties", `Assoc [ "line1", str ] ]
              )
            ] )
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected ->
    let open Yojson.Safe.Util in
    projected
    |> member "$defs"
    |> member "address"
    |> member "additionalProperties"
    = `Bool false
;;

let%test "optional enum property is made nullable via anyOf, not silently forced required"
  =
  let schema =
    obj
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc [ "status", `Assoc [ "enum", `List [ `String "a"; `String "b" ] ] ] )
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected ->
    let open Yojson.Safe.Util in
    (* promoted to required (strict subset) ... *)
    projected |> member "required" = `List [ `String "status" ]
    (* ... but nullability preserved through an anyOf null branch, so an absent
       value is still expressible — the enum was not turned into a mandatory choice *)
    &&
    let st = projected |> member "properties" |> member "status" in
    (match st |> member "anyOf" with
     | `List branches ->
       List.exists (fun b -> b = `Assoc [ "type", `String "null" ]) branches
     | _ -> false)
;;

let%test "optional $ref property is made nullable via anyOf" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "addr", `Assoc [ "$ref", `String "#/$defs/a" ] ]
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected ->
    let open Yojson.Safe.Util in
    (match projected |> member "properties" |> member "addr" |> member "anyOf" with
     | `List branches ->
       List.exists (fun b -> b = `Assoc [ "type", `String "null" ]) branches
       && List.exists (fun b -> b = `Assoc [ "$ref", `String "#/$defs/a" ]) branches
     | _ -> false)
;;

let%test "caller additionalProperties:true is a violation, not a silent overwrite" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "a", str ]
      ; "required", `List [ `String "a" ]
      ; "additionalProperties", `Bool true
      ]
  in
  match project schema with
  | Error [ Object_open_additional_properties "<root>" ] -> true
  | Error _ | Ok _ -> false
;;

let%test "caller additionalProperties:false is preserved and stays strict-eligible" =
  let schema =
    obj
      [ "type", `String "object"
      ; "properties", `Assoc [ "a", str ]
      ; "required", `List [ `String "a" ]
      ; "additionalProperties", `Bool false
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected -> Yojson.Safe.Util.member "additionalProperties" projected = `Bool false
;;

let%test "an already-nullable optional (type includes null) is not double-wrapped" =
  let schema =
    obj
      [ "type", `String "object"
      ; ( "properties"
        , `Assoc [ "note", `Assoc [ "type", `List [ `String "string"; `String "null" ] ] ]
        )
      ]
  in
  match project schema with
  | Error _ -> false
  | Ok projected ->
    let open Yojson.Safe.Util in
    projected
    |> member "properties"
    |> member "note"
    |> member "type"
    = `List [ `String "string"; `String "null" ]
;;
