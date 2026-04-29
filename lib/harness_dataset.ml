(** JSONL-backed harness datasets. *)

type t = Harness_case.t list

let non_empty_lines content =
  content
  |> String.split_on_char '\n'
  |> List.map String.trim
  |> List.filter (fun line -> line <> "" && not (String.starts_with ~prefix:"#" line))
;;

let load ~path =
  match Fs_result.read_file path with
  | Error _ as err -> err
  | Ok content ->
    let parse_line line =
      try
        let json = Yojson.Safe.from_string line in
        Harness_case.of_json json
      with
      | Yojson.Json_error msg -> Error (Util.json_parse_error msg)
    in
    Util.result_traverse ~f:parse_line (non_empty_lines content)
;;

let encode_lines cases =
  cases
  |> List.map (fun case_ -> Yojson.Safe.to_string (Harness_case.to_json case_))
  |> String.concat "\n"
;;

let save ~path (cases : t) =
  let payload =
    match cases with
    | [] -> ""
    | _ -> encode_lines cases ^ "\n"
  in
  Fs_result.write_file path payload
;;

let append_case ~path case_ =
  let line = Yojson.Safe.to_string (Harness_case.to_json case_) ^ "\n" in
  Fs_result.append_file path line
;;
