open Base
(** Memory tool JSON parameter parsing helpers.

    @since 0.92.0 extracted from Memory_tools

    @stability Evolving
    @since 0.93.1 *)

val tool_error : string -> ('a, Types.tool_error) result
val parse_string_field : Yojson.Safe.t -> string -> (string, Types.tool_error) result

val parse_optional_string_field
  :  Yojson.Safe.t
  -> string
  -> (string option, Types.tool_error) result

val parse_bool_field
  :  Yojson.Safe.t
  -> string
  -> default:bool
  -> (bool, Types.tool_error) result

val parse_float_field
  :  Yojson.Safe.t
  -> string
  -> default:float
  -> (float, Types.tool_error) result

val parse_generic_tier
  :  Yojson.Safe.t
  -> default:Memory.tier
  -> (Memory.tier, Types.tool_error) result

val parse_string_list_field
  :  Yojson.Safe.t
  -> string
  -> (string list, Types.tool_error) result

val parse_metadata_field
  :  Yojson.Safe.t
  -> ((string * Yojson.Safe.t) list, Types.tool_error) result

val parse_value_json : Yojson.Safe.t -> (Yojson.Safe.t, Types.tool_error) result
val parse_outcome : Yojson.Safe.t -> (Memory.outcome, Types.tool_error) result
