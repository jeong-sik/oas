(** Tool_op -- algebraic operations on tool name sets. *)

type t =
  | Keep_all
  | Clear_all
  | Add of string list
  | Remove of string list
  | Replace_with of string list
  | Intersect_with of string list
  | Seq of t list

(* ── helpers ──────────────────────────────────────────────── *)

let set_of names =
  let tbl = Hashtbl.create (List.length names) in
  List.iter (fun n -> Hashtbl.replace tbl n ()) names;
  tbl
;;

let dedup names =
  let seen = Hashtbl.create (List.length names) in
  List.filter
    (fun n ->
       if Hashtbl.mem seen n
       then false
       else (
         Hashtbl.replace seen n ();
         true))
    names
;;

(* ── apply ────────────────────────────────────────────────── *)

let rec apply op current =
  let current = dedup current in
  match op with
  | Keep_all -> current
  | Clear_all -> []
  | Add names ->
    let s = set_of current in
    let new_names = List.filter (fun n -> not (Hashtbl.mem s n)) (dedup names) in
    current @ new_names
  | Remove names ->
    let rm = set_of (dedup names) in
    List.filter (fun n -> not (Hashtbl.mem rm n)) current
  | Replace_with names -> dedup names
  | Intersect_with names ->
    let keep = set_of (dedup names) in
    List.filter (fun n -> Hashtbl.mem keep n) current
  | Seq ops -> List.fold_left (fun acc op -> apply op acc) current ops
;;

let apply_to_tool_set op ts =
  let result_names = apply op (Tool_set.names ts) in
  (* Build a lookup table from tool name to tool, based on the original set. *)
  let by_name = Hashtbl.create (List.length result_names) in
  List.iter
    (fun (tool : Tool.t) -> Hashtbl.replace by_name tool.schema.Types.name tool)
    (Tool_set.to_list ts);
  (* Construct the resulting tool list in the order given by [result_names]. *)
  let rec collect acc = function
    | [] -> List.rev acc
    | name :: rest ->
      (match Hashtbl.find_opt by_name name with
       | Some tool -> collect (tool :: acc) rest
       | None -> collect acc rest)
  in
  let ordered_tools = collect [] result_names in
  Tool_set.of_list ordered_tools
;;

(* ── compose ──────────────────────────────────────────────── *)

let compose ops =
  let rec flatten acc = function
    | [] -> acc
    | Seq inner :: rest -> flatten (flatten acc inner) rest
    | Keep_all :: rest -> flatten acc rest
    | Add [] :: rest -> flatten acc rest
    | Remove [] :: rest -> flatten acc rest
    | op :: rest -> flatten (op :: acc) rest
  in
  match List.rev (flatten [] ops) with
  | [] -> Keep_all
  | [ x ] -> x
  | many -> Seq many
;;

(* ── to_tool_filter ───────────────────────────────────────── *)

let to_tool_filter op current = Guardrails.AllowList (apply op current)

(* ── predicates ───────────────────────────────────────────── *)

let rec is_identity = function
  | Keep_all -> true
  | Add names -> dedup names = []
  | Remove names -> dedup names = []
  | Seq ops -> List.for_all is_identity ops
  | Clear_all | Replace_with _ | Intersect_with _ -> false
;;

let rec is_destructive = function
  | Clear_all | Replace_with _ | Intersect_with _ -> true
  | Keep_all | Add _ | Remove _ -> false
  | Seq ops -> List.exists is_destructive ops
;;

(* ── serialization ────────────────────────────────────────── *)

let names_to_json names = `List (List.map (fun n -> `String n) names)

let rec to_yojson = function
  | Keep_all -> `Assoc [ "op", `String "keep_all" ]
  | Clear_all -> `Assoc [ "op", `String "clear_all" ]
  | Add names -> `Assoc [ "op", `String "add"; "names", names_to_json names ]
  | Remove names -> `Assoc [ "op", `String "remove"; "names", names_to_json names ]
  | Replace_with names ->
    `Assoc [ "op", `String "replace_with"; "names", names_to_json names ]
  | Intersect_with names ->
    `Assoc [ "op", `String "intersect_with"; "names", names_to_json names ]
  | Seq ops -> `Assoc [ "op", `String "seq"; "ops", `List (List.map to_yojson ops) ]
;;

let names_of_json = function
  | `List items ->
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | `String s :: rest -> go (s :: acc) rest
      | _ :: _ -> Error "names must be strings"
    in
    go [] items
  | _ -> Error "names must be a JSON array"
;;

let rec of_yojson json =
  match json with
  | `Assoc fields ->
    (match List.assoc_opt "op" fields with
     | Some (`String "keep_all") -> Ok Keep_all
     | Some (`String "clear_all") -> Ok Clear_all
     | Some (`String "add") -> of_yojson_names fields (fun ns -> Add ns)
     | Some (`String "remove") -> of_yojson_names fields (fun ns -> Remove ns)
     | Some (`String "replace_with") -> of_yojson_names fields (fun ns -> Replace_with ns)
     | Some (`String "intersect_with") ->
       of_yojson_names fields (fun ns -> Intersect_with ns)
     | Some (`String "seq") ->
       (match List.assoc_opt "ops" fields with
        | Some (`List items) ->
          let rec go acc = function
            | [] -> Ok (Seq (List.rev acc))
            | j :: rest ->
              (match of_yojson j with
               | Ok op -> go (op :: acc) rest
               | Error e -> Error e)
          in
          go [] items
        | _ -> Error "seq requires 'ops' array")
     | Some (`String other) -> Error (Printf.sprintf "unknown op: %s" other)
     | _ -> Error "missing or invalid 'op' field")
  | _ -> Error "tool_op must be a JSON object"

and of_yojson_names fields wrap =
  match List.assoc_opt "names" fields with
  | Some json ->
    (match names_of_json json with
     | Ok ns -> Ok (wrap ns)
     | Error e -> Error e)
  | None -> Error "missing 'names' field"
;;

(* ── equal ────────────────────────────────────────────────── *)

let sort_dedup names = names |> dedup |> List.sort String.compare

let rec equal a b =
  match a, b with
  | Keep_all, Keep_all -> true
  | Clear_all, Clear_all -> true
  | Add a_names, Add b_names -> sort_dedup a_names = sort_dedup b_names
  | Remove a_names, Remove b_names -> sort_dedup a_names = sort_dedup b_names
  | Replace_with a_names, Replace_with b_names -> sort_dedup a_names = sort_dedup b_names
  | Intersect_with a_names, Intersect_with b_names ->
    sort_dedup a_names = sort_dedup b_names
  | Seq a_ops, Seq b_ops ->
    List.compare_lengths a_ops b_ops = 0 && List.for_all2 equal a_ops b_ops
  | _ -> false
;;
