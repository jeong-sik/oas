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

let string_field name ~required ~desc =
  make_field name ~typ:Types.String ~required ~desc
    ~extract:(fun json ->
      let open Yojson.Safe.Util in
      match member name json with
      | `Null when not required -> Ok ""
      | `Null -> Error (Printf.sprintf "missing required field: %s" name)
      | `String s -> Ok s
      | v -> Error (Printf.sprintf "%s: expected string, got %s" name
                      (Yojson.Safe.to_string v)))

let int_field name ~required ~desc =
  make_field name ~typ:Types.Integer ~required ~desc
    ~extract:(fun json ->
      let open Yojson.Safe.Util in
      match member name json with
      | `Null when not required -> Ok 0
      | `Null -> Error (Printf.sprintf "missing required field: %s" name)
      | `Int i -> Ok i
      | `String s -> (match int_of_string_opt s with
        | Some i -> Ok i
        | None -> Error (Printf.sprintf "%s: cannot parse integer from %S" name s))
      | v -> Error (Printf.sprintf "%s: expected integer, got %s" name
                      (Yojson.Safe.to_string v)))

let float_field name ~required ~desc =
  make_field name ~typ:Types.Number ~required ~desc
    ~extract:(fun json ->
      let open Yojson.Safe.Util in
      match member name json with
      | `Null when not required -> Ok 0.0
      | `Null -> Error (Printf.sprintf "missing required field: %s" name)
      | `Float f -> Ok f
      | `Int i -> Ok (float_of_int i)
      | `String s -> (match float_of_string_opt s with
        | Some f -> Ok f
        | None -> Error (Printf.sprintf "%s: cannot parse number from %S" name s))
      | v -> Error (Printf.sprintf "%s: expected number, got %s" name
                      (Yojson.Safe.to_string v)))

let bool_field name ~required ~desc =
  make_field name ~typ:Types.Boolean ~required ~desc
    ~extract:(fun json ->
      let open Yojson.Safe.Util in
      match member name json with
      | `Null when not required -> Ok false
      | `Null -> Error (Printf.sprintf "missing required field: %s" name)
      | `Bool b -> Ok b
      | `String "true" -> Ok true
      | `String "false" -> Ok false
      | v -> Error (Printf.sprintf "%s: expected boolean, got %s" name
                      (Yojson.Safe.to_string v)))

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

let parse : type a. a schema -> Yojson.Safe.t -> (a, string) result =
  fun schema json ->
  match schema with
  | One a ->
    (match a.extract json with
     | Ok va -> Ok va
     | Error e -> Error e)
  | Two (a, b) ->
    (match a.extract json, b.extract json with
     | Ok va, Ok vb -> Ok (va, vb)
     | Error e, _ | _, Error e -> Error e)
  | Three (a, b, c) ->
    (match a.extract json, b.extract json, c.extract json with
     | Ok va, Ok vb, Ok vc -> Ok (va, vb, vc)
     | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e)
  | Four (a, b, c, d) ->
    (match a.extract json, b.extract json, c.extract json, d.extract json with
     | Ok va, Ok vb, Ok vc, Ok vd -> Ok (va, vb, vc, vd)
     | Error e, _, _, _ | _, Error e, _, _ | _, _, Error e, _ | _, _, _, Error e -> Error e)

let to_json_schema : type a. a schema -> Yojson.Safe.t =
  fun schema ->
  let params = to_params schema in
  let properties = List.map (fun (p : Types.tool_param) ->
    (p.name, `Assoc [
      ("type", `String (Types.param_type_to_string p.param_type));
      ("description", `String p.description);
    ])
  ) params in
  let required = List.filter_map (fun (p : Types.tool_param) ->
    if p.required then Some (`String p.name) else None
  ) params in
  `Assoc [
    ("type", `String "object");
    ("properties", `Assoc properties);
    ("required", `List required);
  ]
