type validation_error =
  | Blank_name
  | Duplicate_name of string

let validate names =
  let seen = Hashtbl.create (List.length names) in
  let rec loop = function
    | [] -> Ok ()
    | name :: _ when String.equal (String.trim name) "" -> Error Blank_name
    | name :: _ when Hashtbl.mem seen name -> Error (Duplicate_name name)
    | name :: rest ->
      Hashtbl.add seen name ();
      loop rest
  in
  loop names
;;

let to_yojson names = `List (List.map (fun name -> `String name) names)

let of_yojson = function
  | `List names ->
    let rec decode decoded_rev = function
      | [] -> Ok (List.rev decoded_rev)
      | `String name :: rest -> decode (name :: decoded_rev) rest
      | _ :: _ -> Error "tool_names must contain only strings"
    in
    decode [] names
  | _ -> Error "tool_names must be an array"
;;
