(** Tool schema generator — combinator-based schema derivation.

    Alternative to PPX: declare fields with combinators, get
    [tool_param list] + [of_json] + [to_json_schema] for free.

    Usage:
    {[
      let schema = Tool_schema_gen.(
        field "message" ~typ:String ~required:true ~desc:"Message content"
        @> field "format" ~typ:String ~required:false ~desc:"Output format"
        @> done_)

      let params = Tool_schema_gen.to_params schema
      let parse json = Tool_schema_gen.parse schema json
      (* parse returns: (string * string option, field_error list) result *)
    ]}

    Each field type maps to a single OCaml type with zero-default for optional:
    - String -> string (optional: [""])
    - Integer -> int (optional: [0])
    - Number -> float (optional: [0.0])
    - Boolean -> bool (optional: [false])

    @stability Experimental
    @since 0.120.0 *)

(** {1 Field specification} *)

(** A single field in the schema with its type and optionality. *)
type ('a, 'rest) field_spec

(** Schema terminator. *)
type done_t

(** {1 Field constructors} *)

val string_field :
  string -> required:bool -> desc:string ->
  ?default:string -> unit -> (string, 'rest) field_spec

val int_field :
  string -> required:bool -> desc:string ->
  ?default:int -> unit -> (int, 'rest) field_spec

val float_field :
  string -> required:bool -> desc:string ->
  ?default:float -> unit -> (float, 'rest) field_spec

val bool_field :
  string -> required:bool -> desc:string ->
  ?default:bool -> unit -> (bool, 'rest) field_spec

(** {1 Schema construction} *)

(** A complete schema with heterogeneous field types. *)
type 'a schema

(** Single-field schema. *)
val one : ('a, done_t) field_spec -> 'a schema

(** Two-field schema. *)
val two :
  ('a, done_t) field_spec ->
  ('b, done_t) field_spec ->
  ('a * 'b) schema

(** Three-field schema. *)
val three :
  ('a, done_t) field_spec ->
  ('b, done_t) field_spec ->
  ('c, done_t) field_spec ->
  ('a * 'b * 'c) schema

(** Four-field schema. *)
val four :
  ('a, done_t) field_spec ->
  ('b, done_t) field_spec ->
  ('c, done_t) field_spec ->
  ('d, done_t) field_spec ->
  ('a * 'b * 'c * 'd) schema

(** {1 Derivation} *)

(** Generate [Types.tool_param list] from schema. *)
val to_params : _ schema -> Types.tool_param list

(** Parse JSON input according to schema.
    Required fields must be present; optional fields default to [None]/zero.
    Returns typed tuple or structured field errors. *)
val parse : 'a schema -> Yojson.Safe.t -> ('a, Tool_input_validation.field_error list) result

(** Generate JSON Schema object for MCP tool registration. *)
val to_json_schema : _ schema -> Yojson.Safe.t
