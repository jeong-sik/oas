type status =
  | Status_ok
  | Degraded
  | Failed
  | Unknown

type probe_name =
  | Provider
  | Transport
  | Checkpoint
  | Context
  | Event_bus
  | Custom of string

type probe = {
  name: probe_name;
  status: status;
  detail: string option;
  checked_at: float;
  latency_ms: float option;
}

type t = {
  generated_at: float;
  overall: status;
  probes: probe list;
}

let status_to_string = function
  | Status_ok -> "ok"
  | Degraded -> "degraded"
  | Failed -> "failed"
  | Unknown -> "unknown"

let status_of_string = function
  | "ok" -> Ok Status_ok
  | "degraded" -> Ok Degraded
  | "failed" -> Ok Failed
  | "unknown" -> Ok Unknown
  | s -> Error (Printf.sprintf "unknown health status: %s" s)

let probe_name_to_string = function
  | Provider -> "provider"
  | Transport -> "transport"
  | Checkpoint -> "checkpoint"
  | Context -> "context"
  | Event_bus -> "event_bus"
  | Custom name -> name

let probe_name_of_string = function
  | "provider" -> Provider
  | "transport" -> Transport
  | "checkpoint" -> Checkpoint
  | "context" -> Context
  | "event_bus" -> Event_bus
  | name -> Custom name

let make_probe ~name ~status ?detail ?checked_at ?latency_ms () =
  {
    name;
    status;
    detail;
    checked_at = Option.value checked_at ~default:(Unix.gettimeofday ());
    latency_ms;
  }

let status_rank = function
  | Status_ok -> 0
  | Unknown -> 1
  | Degraded -> 2
  | Failed -> 3

let max_status a b =
  if status_rank a >= status_rank b then a else b

let overall_status = function
  | [] -> Unknown
  | probes ->
    List.fold_left
      (fun acc (probe : probe) -> max_status acc probe.status)
      Status_ok probes

let make ?generated_at probes =
  {
    generated_at = Option.value generated_at ~default:(Unix.gettimeofday ());
    overall = overall_status probes;
    probes;
  }

let option_to_json f = function
  | None -> `Null
  | Some v -> f v

let probe_to_json probe =
  `Assoc [
    "name", `String (probe_name_to_string probe.name);
    "status", `String (status_to_string probe.status);
    "detail", option_to_json (fun v -> `String v) probe.detail;
    "checked_at", `Float probe.checked_at;
    "latency_ms", option_to_json (fun v -> `Float v) probe.latency_ms;
  ]

let assoc_field name fields =
  match List.assoc_opt name fields with
  | Some v -> Ok v
  | None -> Error (Printf.sprintf "missing field: %s" name)

let string_field name fields =
  match assoc_field name fields with
  | Ok (`String v) -> Ok v
  | Ok _ -> Error (Printf.sprintf "field %s must be a string" name)
  | Error _ as err -> err

let float_field name fields =
  match assoc_field name fields with
  | Ok (`Float v) -> Ok v
  | Ok (`Int v) -> Ok (float_of_int v)
  | Ok _ -> Error (Printf.sprintf "field %s must be a number" name)
  | Error _ as err -> err

let option_string_field name fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some (`String v) -> Ok (Some v)
  | Some _ -> Error (Printf.sprintf "field %s must be a string or null" name)

let option_float_field name fields =
  match List.assoc_opt name fields with
  | None | Some `Null -> Ok None
  | Some (`Float v) -> Ok (Some v)
  | Some (`Int v) -> Ok (Some (float_of_int v))
  | Some _ -> Error (Printf.sprintf "field %s must be a number or null" name)

let ( let* ) = Result.bind

let probe_of_json = function
  | `Assoc fields ->
    let* raw_name = string_field "name" fields in
    let name = probe_name_of_string raw_name in
    let* raw_status = string_field "status" fields in
    let* status = status_of_string raw_status in
    let* detail = option_string_field "detail" fields in
    let* checked_at = float_field "checked_at" fields in
    let* latency_ms = option_float_field "latency_ms" fields in
    Ok { name; status; detail; checked_at; latency_ms }
  | _ -> Error "health probe must be a JSON object"

let to_json t =
  `Assoc [
    "generated_at", `Float t.generated_at;
    "overall", `String (status_to_string t.overall);
    "probes", `List (List.map probe_to_json t.probes);
  ]

let rec parse_probes acc = function
  | [] -> Ok (List.rev acc)
  | item :: rest ->
    match probe_of_json item with
    | Ok probe -> parse_probes (probe :: acc) rest
    | Error _ as err -> err

let of_json = function
  | `Assoc fields ->
    let* generated_at = float_field "generated_at" fields in
    let* raw_overall = string_field "overall" fields in
    let* overall = status_of_string raw_overall in
    let* probes =
      match assoc_field "probes" fields with
      | Ok (`List items) -> parse_probes [] items
      | Ok _ -> Error "field probes must be a list"
      | Error _ as err -> err
    in
    Ok { generated_at; overall; probes }
  | _ -> Error "runtime health report must be a JSON object"
