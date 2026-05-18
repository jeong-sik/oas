(* See discovery_parse.mli for module rationale. *)

type model_info =
  { id : string
  ; owned_by : string
  }

type server_props =
  { total_slots : int
  ; ctx_size : int
  ; model : string
  ; supports_tools : bool option
  }

type slot_status =
  { total : int
  ; busy : int
  ; idle : int
  }

let parse_models json =
  let open Yojson.Safe.Util in
  match member "data" json with
  | `List items ->
    items
    |> List.filter_map (fun item ->
      match item |> member "id" |> to_string_option with
      | Some id ->
        let owned_by =
          item |> member "owned_by" |> to_string_option |> Option.value ~default:"unknown"
        in
        Some { id; owned_by }
      | None -> None)
  | _ -> []
;;

let parse_props json =
  let open Yojson.Safe.Util in
  match member "total_slots" json with
  | `Int total_slots ->
    let dgs = member "default_generation_settings" json in
    let ctx_size =
      match dgs with
      | `Assoc _ ->
        (match member "n_ctx" dgs with
         | `Int n -> n
         | _ -> 0)
      | _ -> 0
    in
    let model =
      match dgs with
      | `Assoc _ ->
        (match member "model" dgs with
         | `String s -> s
         | _ -> "")
      | _ -> ""
    in
    Some { total_slots; ctx_size; model; supports_tools = None }
  | _ -> None
;;

let parse_slots json =
  let open Yojson.Safe.Util in
  let items =
    match json with
    | `List items -> items
    | _ -> []
  in
  if items = []
  then None
  else (
    let total = List.length items in
    let busy =
      items
      |> List.fold_left
           (fun acc slot ->
              let is_busy =
                slot
                |> member "is_processing"
                |> to_bool_option
                |> Option.value ~default:false
                ||
                match slot |> member "state" with
                | `Int n -> n <> 0
                | _ -> false
              in
              if is_busy then acc + 1 else acc)
           0
    in
    Some { total; busy; idle = total - busy })
;;
