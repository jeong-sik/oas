(** Tool schema generator — combinator-based schema derivation.

    @stability Experimental
    @since 0.120.0 *)

(* ── Field specification ────────────────────────────────── *)

type done_t = Done [@@warning "-37"]

type ('a, _) field_spec =
  { name : string
  ; param_type : Types.param_type
  ; required : bool
  ; description : string
  ; extract : Yojson.Safe.t -> ('a, Tool_input_validation.field_error) result
  }

let make_field name ~typ ~required ~desc ~extract =
  { name; param_type = typ; required; description = desc; extract }
;;

(** Extract a required field without changing the caller's JSON value. *)
let extract_required ~name ~typ ~unwrap json =
  let open Yojson.Safe.Util in
  let raw = member name json in
  match raw with
  | `Null ->
    Error
      { Tool_input_validation.path = name
      ; expected = Types.param_type_to_string typ
      ; actual = Tool_input_validation.Missing
      }
  | v ->
    (match unwrap v with
     | Some a -> Ok a
     | None ->
       Error
         { Tool_input_validation.path = name
         ; expected = Types.param_type_to_string typ
         ; actual =
             Tool_input_validation.Received (Tool_input_validation.describe_json_value v)
         })
;;

let extract_optional ~name ~typ ~unwrap json =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok None
  | value ->
    (match unwrap value with
     | Some parsed -> Ok (Some parsed)
     | None ->
       Error
         { Tool_input_validation.path = name
         ; expected = Types.param_type_to_string typ
         ; actual =
             Tool_input_validation.Received
               (Tool_input_validation.describe_json_value value)
         })
;;

let extract_defaulted ~name ~typ ~default ~unwrap json =
  match extract_optional ~name ~typ ~unwrap json with
  | Ok None -> Ok default
  | Ok (Some value) -> Ok value
  | Error _ as error -> error
;;

let string_value = function
  | `String value -> Some value
  | _ -> None
;;

let int_value = function
  | `Int value -> Some value
  | _ -> None
;;

let float_value = function
  | `Float value -> Some value
  | `Int value -> Some (float_of_int value)
  | _ -> None
;;

let bool_value = function
  | `Bool value -> Some value
  | _ -> None
;;

let string_field name ~desc () =
  make_field
    name
    ~typ:Types.String
    ~required:true
    ~desc
    ~extract:(extract_required ~name ~typ:Types.String ~unwrap:string_value)
;;

let optional_string_field name ~desc () =
  make_field
    name
    ~typ:Types.String
    ~required:false
    ~desc
    ~extract:(extract_optional ~name ~typ:Types.String ~unwrap:string_value)
;;

let defaulted_string_field name ~desc ~default () =
  make_field
    name
    ~typ:Types.String
    ~required:false
    ~desc
    ~extract:(extract_defaulted ~name ~typ:Types.String ~default ~unwrap:string_value)
;;

let int_field name ~desc () =
  make_field
    name
    ~typ:Types.Integer
    ~required:true
    ~desc
    ~extract:(extract_required ~name ~typ:Types.Integer ~unwrap:int_value)
;;

let optional_int_field name ~desc () =
  make_field
    name
    ~typ:Types.Integer
    ~required:false
    ~desc
    ~extract:(extract_optional ~name ~typ:Types.Integer ~unwrap:int_value)
;;

let defaulted_int_field name ~desc ~default () =
  make_field
    name
    ~typ:Types.Integer
    ~required:false
    ~desc
    ~extract:(extract_defaulted ~name ~typ:Types.Integer ~default ~unwrap:int_value)
;;

let float_field name ~desc () =
  make_field
    name
    ~typ:Types.Number
    ~required:true
    ~desc
    ~extract:(extract_required ~name ~typ:Types.Number ~unwrap:float_value)
;;

let optional_float_field name ~desc () =
  make_field
    name
    ~typ:Types.Number
    ~required:false
    ~desc
    ~extract:(extract_optional ~name ~typ:Types.Number ~unwrap:float_value)
;;

let defaulted_float_field name ~desc ~default () =
  make_field
    name
    ~typ:Types.Number
    ~required:false
    ~desc
    ~extract:(extract_defaulted ~name ~typ:Types.Number ~default ~unwrap:float_value)
;;

let bool_field name ~desc () =
  make_field
    name
    ~typ:Types.Boolean
    ~required:true
    ~desc
    ~extract:(extract_required ~name ~typ:Types.Boolean ~unwrap:bool_value)
;;

let optional_bool_field name ~desc () =
  make_field
    name
    ~typ:Types.Boolean
    ~required:false
    ~desc
    ~extract:(extract_optional ~name ~typ:Types.Boolean ~unwrap:bool_value)
;;

let defaulted_bool_field name ~desc ~default () =
  make_field
    name
    ~typ:Types.Boolean
    ~required:false
    ~desc
    ~extract:(extract_defaulted ~name ~typ:Types.Boolean ~default ~unwrap:bool_value)
;;

(* ── Schema type ────────────────────────────────────────── *)

type _ schema =
  | One : ('a, done_t) field_spec -> 'a schema
  | Two : ('a, done_t) field_spec * ('b, done_t) field_spec -> ('a * 'b) schema
  | Three :
      ('a, done_t) field_spec * ('b, done_t) field_spec * ('c, done_t) field_spec
      -> ('a * 'b * 'c) schema
  | Four :
      ('a, done_t) field_spec
      * ('b, done_t) field_spec
      * ('c, done_t) field_spec
      * ('d, done_t) field_spec
      -> ('a * 'b * 'c * 'd) schema

let one a = One a
let two a b = Two (a, b)
let three a b c = Three (a, b, c)
let four a b c d = Four (a, b, c, d)

(* ── Field to param ─────────────────────────────────────── *)

let field_to_param (f : (_, _) field_spec) : Types.tool_param =
  { name = f.name
  ; description = f.description
  ; param_type = f.param_type
  ; required = f.required
  }
;;

(* ── Derivation ─────────────────────────────────────────── *)

let to_params : type a. a schema -> Types.tool_param list = function
  | One a -> [ field_to_param a ]
  | Two (a, b) -> [ field_to_param a; field_to_param b ]
  | Three (a, b, c) -> [ field_to_param a; field_to_param b; field_to_param c ]
  | Four (a, b, c, d) ->
    [ field_to_param a; field_to_param b; field_to_param c; field_to_param d ]
;;

let collect_errors results =
  List.filter_map
    (function
      | Error e -> Some e
      | Ok _ -> None)
    results
;;

let parse
  : type a.
    a schema -> Yojson.Safe.t -> (a, Tool_input_validation.field_error list) result
  =
  fun schema json ->
  match json with
  | `Assoc _ ->
    (match schema with
     | One a -> a.extract json |> Result.map_error (fun e -> [ e ])
     | Two (a, b) ->
       let ra = a.extract json
       and rb = b.extract json in
       (match ra, rb with
        | Ok va, Ok vb -> Ok (va, vb)
        | _ -> Error (collect_errors [ Result.map ignore ra; Result.map ignore rb ]))
     | Three (a, b, c) ->
       let ra = a.extract json
       and rb = b.extract json
       and rc = c.extract json in
       (match ra, rb, rc with
        | Ok va, Ok vb, Ok vc -> Ok (va, vb, vc)
        | _ ->
          Error
            (collect_errors
               [ Result.map ignore ra; Result.map ignore rb; Result.map ignore rc ]))
     | Four (a, b, c, d) ->
       let ra = a.extract json
       and rb = b.extract json
       and rc = c.extract json
       and rd = d.extract json in
       (match ra, rb, rc, rd with
        | Ok va, Ok vb, Ok vc, Ok vd -> Ok (va, vb, vc, vd)
        | _ ->
          Error
            (collect_errors
               [ Result.map ignore ra
               ; Result.map ignore rb
               ; Result.map ignore rc
               ; Result.map ignore rd
               ])))
  | other ->
    Error
      [ { Tool_input_validation.path = "/"
        ; expected = "object"
        ; actual =
            Tool_input_validation.Received
              (Tool_input_validation.describe_json_value other)
        }
      ]
;;

let to_json_schema : type a. a schema -> Yojson.Safe.t =
  fun schema -> Types.params_to_input_schema (to_params schema)
;;
