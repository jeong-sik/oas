(** Tool schema generator — combinator-based schema derivation.

    @stability Experimental
    @since 0.120.0 *)

(* ── Field specification ────────────────────────────────── *)

type done_t = Done [@@warning "-37"]

type ('a, _) field_spec = {
  name : string;
  param_type : Types.param_type;
  required : bool;
  description : string;
  extract : Yojson.Safe.t -> ('a, string) result;
}

let make_field name ~typ ~required ~desc ~extract =
  { name; param_type = typ; required; description = desc; extract }

(** Extract a field value, applying try_coerce for type normalization.
    Returns [default] for missing optional fields, error for missing required. *)
let extract_with_coerce ~name ~typ ~required ~default ~unwrap json =
  let open Yojson.Safe.Util in
  let raw = member name json in
  match raw with
  | `Null when not required -> Ok default
  | `Null -> Error (Printf.sprintf "missing required field: %s" name)
  | v ->
    let coerced = match Tool_input_validation.try_coerce typ v with
      | Some c -> c | None -> v in
    match unwrap coerced with
    | Some a -> Ok a
    | None -> Error (Printf.sprintf "%s: expected %s, got %s" name
                       (Types.param_type_to_string typ) (Yojson.Safe.to_string v))

let string_field name ~required ~desc =
  make_field name ~typ:Types.String ~required ~desc
    ~extract:(extract_with_coerce ~name ~typ:Types.String ~required ~default:""
      ~unwrap:(function `String s -> Some s | _ -> None))

let int_field name ~required ~desc =
  make_field name ~typ:Types.Integer ~required ~desc
    ~extract:(extract_with_coerce ~name ~typ:Types.Integer ~required ~default:0
      ~unwrap:(function `Int i -> Some i | _ -> None))

let float_field name ~required ~desc =
  make_field name ~typ:Types.Number ~required ~desc
    ~extract:(extract_with_coerce ~name ~typ:Types.Number ~required ~default:0.0
      ~unwrap:(function `Float f -> Some f | `Int i -> Some (float_of_int i) | _ -> None))

let bool_field name ~required ~desc =
  make_field name ~typ:Types.Boolean ~required ~desc
    ~extract:(extract_with_coerce ~name ~typ:Types.Boolean ~required ~default:false
      ~unwrap:(function `Bool b -> Some b | _ -> None))

(* ── Schema type ────────────────────────────────────────── *)

type _ schema =
  | One : ('a, done_t) field_spec -> 'a schema
  | Two : ('a, done_t) field_spec * ('b, done_t) field_spec -> ('a * 'b) schema
  | Three : ('a, done_t) field_spec * ('b, done_t) field_spec * ('c, done_t) field_spec -> ('a * 'b * 'c) schema
  | Four : ('a, done_t) field_spec * ('b, done_t) field_spec * ('c, done_t) field_spec * ('d, done_t) field_spec -> ('a * 'b * 'c * 'd) schema

let one a = One a
let two a b = Two (a, b)
let three a b c = Three (a, b, c)
let four a b c d = Four (a, b, c, d)

(* ── Field to param ─────────────────────────────────────── *)

let field_to_param (f : (_, _) field_spec) : Types.tool_param =
  { name = f.name; description = f.description;
    param_type = f.param_type; required = f.required }

(* ── Derivation ─────────────────────────────────────────── *)

let to_params : type a. a schema -> Types.tool_param list = function
  | One a -> [field_to_param a]
  | Two (a, b) -> [field_to_param a; field_to_param b]
  | Three (a, b, c) -> [field_to_param a; field_to_param b; field_to_param c]
  | Four (a, b, c, d) -> [field_to_param a; field_to_param b; field_to_param c; field_to_param d]

let collect_errors results =
  let errors = List.filter_map (function Error e -> Some e | Ok _ -> None) results in
  match errors with
  | [] -> None
  | es -> Some (String.concat "; " es)

let parse : type a. a schema -> Yojson.Safe.t -> (a, string) result =
  fun schema json ->
  match schema with
  | One a -> a.extract json
  | Two (a, b) ->
    let ra = a.extract json and rb = b.extract json in
    (match ra, rb with
     | Ok va, Ok vb -> Ok (va, vb)
     | _ -> Error (Option.get (collect_errors [Result.map ignore ra; Result.map ignore rb])))
  | Three (a, b, c) ->
    let ra = a.extract json and rb = b.extract json and rc = c.extract json in
    (match ra, rb, rc with
     | Ok va, Ok vb, Ok vc -> Ok (va, vb, vc)
     | _ -> Error (Option.get (collect_errors [Result.map ignore ra; Result.map ignore rb; Result.map ignore rc])))
  | Four (a, b, c, d) ->
    let ra = a.extract json and rb = b.extract json and rc = c.extract json and rd = d.extract json in
    (match ra, rb, rc, rd with
     | Ok va, Ok vb, Ok vc, Ok vd -> Ok (va, vb, vc, vd)
     | _ -> Error (Option.get (collect_errors [Result.map ignore ra; Result.map ignore rb; Result.map ignore rc; Result.map ignore rd])))

let to_json_schema : type a. a schema -> Yojson.Safe.t =
  fun schema -> Types.params_to_input_schema (to_params schema)
