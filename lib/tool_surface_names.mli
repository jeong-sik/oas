type validation_error =
  | Blank_name
  | Duplicate_name of string

val validate : string list -> (unit, validation_error) result
val to_yojson : string list -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (string list, string) result
