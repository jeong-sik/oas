(** SDK metrics collection — counters and histograms.

    Instance-based: each [create ()] returns independent state.
    Thread-safe via Eio.Mutex. *)

(* -- Internal types --------------------------------------------------- *)

type label_key = (string * string) list

let label_key_of labels = List.sort (fun (a, _) (b, _) -> String.compare a b) labels

module LabelMap = Map.Make (struct
    type t = label_key

    let compare = compare
  end)

(* -- Counter ---------------------------------------------------------- *)

type counter_data =
  { c_name : string
  ; c_unit : string
  ; c_values : int LabelMap.t
  }

(* -- Histogram -------------------------------------------------------- *)

type histogram_data =
  { h_name : string
  ; h_buckets : float list
  ; h_observations : float list
  ; h_sum : float
  ; h_count : int
  }

(* -- Metrics instance ------------------------------------------------- *)

type t =
  { mu : Eio.Mutex.t
  ; mutable counters : counter_data list
  ; mutable histograms : histogram_data list
  }

type counter = t * string
type histogram = t * string

let create () = { mu = Eio.Mutex.create (); counters = []; histograms = [] }
let with_lock t f = Eio.Mutex.use_rw ~protect:true t.mu f

let counter t ~name ~unit_ =
  with_lock t (fun () ->
    match List.find_opt (fun c -> c.c_name = name) t.counters with
    | Some _ -> t, name
    | None ->
      let c = { c_name = name; c_unit = unit_; c_values = LabelMap.empty } in
      t.counters <- c :: t.counters;
      t, name)
;;

let histogram t ~name ~buckets =
  with_lock t (fun () ->
    match List.find_opt (fun h -> h.h_name = name) t.histograms with
    | Some _ -> t, name
    | None ->
      let h =
        { h_name = name
        ; h_buckets = buckets
        ; h_observations = []
        ; h_sum = 0.0
        ; h_count = 0
        }
      in
      t.histograms <- h :: t.histograms;
      t, name)
;;

let incr (t, name) ?(labels = []) n =
  let key = label_key_of labels in
  with_lock t (fun () ->
    t.counters
    <- List.map
         (fun c ->
            if c.c_name = name
            then (
              let current =
                match LabelMap.find_opt key c.c_values with
                | Some v -> v
                | None -> 0
              in
              { c with c_values = LabelMap.add key (current + n) c.c_values })
            else c)
         t.counters)
;;

let counter_value (t, name) ?(labels = []) () =
  let key = label_key_of labels in
  with_lock t (fun () ->
    match List.find_opt (fun c -> c.c_name = name) t.counters with
    | Some c ->
      (match LabelMap.find_opt key c.c_values with
       | Some v -> v
       | None -> 0)
    | None -> 0)
;;

let observe (t, name) value =
  with_lock t (fun () ->
    t.histograms
    <- List.map
         (fun h ->
            if h.h_name = name
            then
              { h with
                h_observations = value :: h.h_observations
              ; h_sum = h.h_sum +. value
              ; h_count = h.h_count + 1
              }
            else h)
         t.histograms)
;;

let histogram_count (t, name) =
  with_lock t (fun () ->
    match List.find_opt (fun h -> h.h_name = name) t.histograms with
    | Some h -> h.h_count
    | None -> 0)
;;

let reset t =
  with_lock t (fun () ->
    t.counters <- List.map (fun c -> { c with c_values = LabelMap.empty }) t.counters;
    t.histograms
    <- List.map
         (fun h -> { h with h_observations = []; h_sum = 0.0; h_count = 0 })
         t.histograms)
;;

(* -- OTLP JSON export ------------------------------------------------ *)

let labels_to_json labels : Yojson.Safe.t =
  `List
    (List.map
       (fun (k, v) ->
          `Assoc [ "key", `String k; "value", `Assoc [ "stringValue", `String v ] ])
       labels)
;;

let counter_to_json (c : counter_data) : Yojson.Safe.t =
  let data_points =
    LabelMap.fold
      (fun labels value acc ->
         `Assoc
           [ "attributes", labels_to_json labels; "asInt", `String (string_of_int value) ]
         :: acc)
      c.c_values
      []
  in
  `Assoc
    [ "name", `String c.c_name
    ; "unit", `String c.c_unit
    ; "sum", `Assoc [ "dataPoints", `List data_points; "isMonotonic", `Bool true ]
    ]
;;

let bucket_counts buckets observations =
  let sorted_buckets = List.sort Float.compare buckets in
  let counts =
    List.map
      (fun bound -> List.length (List.filter (fun v -> v <= bound) observations))
      sorted_buckets
  in
  let overflow = List.length observations in
  counts @ [ overflow ]
;;

let histogram_to_json (h : histogram_data) : Yojson.Safe.t =
  let bc = bucket_counts h.h_buckets h.h_observations in
  `Assoc
    [ "name", `String h.h_name
    ; ( "histogram"
      , `Assoc
          [ ( "dataPoints"
            , `List
                [ `Assoc
                    [ "count", `String (string_of_int h.h_count)
                    ; "sum", `Float h.h_sum
                    ; ( "bucketCounts"
                      , `List (List.map (fun n -> `String (string_of_int n)) bc) )
                    ; "explicitBounds", `List (List.map (fun b -> `Float b) h.h_buckets)
                    ]
                ] )
          ] )
    ]
;;

let to_otlp_json t =
  with_lock t (fun () ->
    let scope_metrics =
      `Assoc
        [ "scope", `Assoc [ "name", `String "agent_sdk.metrics" ]
        ; ( "metrics"
          , `List
              (List.map counter_to_json t.counters
               @ List.map histogram_to_json t.histograms) )
        ]
    in
    `Assoc
      [ ( "resourceMetrics"
        , `List
            [ `Assoc
                [ ( "resource"
                  , `Assoc
                      [ ( "attributes"
                        , `List
                            [ `Assoc
                                [ "key", `String "service.name"
                                ; "value", `Assoc [ "stringValue", `String "agent_sdk" ]
                                ]
                            ] )
                      ] )
                ; "scopeMetrics", `List [ scope_metrics ]
                ]
            ] )
      ])
;;

(* -- Prometheus text export ------------------------------------------ *)

let is_identifier_start = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' | ':' -> true
  | _ -> false
;;

let is_identifier_char = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | ':' -> true
  | _ -> false
;;

let prometheus_identifier raw =
  let buf = Buffer.create (String.length raw + 1) in
  String.iteri
    (fun index ch ->
       let valid = if index = 0 then is_identifier_start ch else is_identifier_char ch in
       Buffer.add_char buf (if valid then ch else '_'))
    raw;
  let rendered = Buffer.contents buf in
  if String.equal rendered "" then "_" else rendered
;;

let escape_prometheus_value raw =
  let buf = Buffer.create (String.length raw) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buf "\\\\"
      | '"' -> Buffer.add_string buf "\\\""
      | '\n' -> Buffer.add_string buf "\\n"
      | ch -> Buffer.add_char buf ch)
    raw;
  Buffer.contents buf
;;

let float_to_prometheus value =
  match classify_float value with
  | FP_nan -> "NaN"
  | FP_infinite -> if value > 0.0 then "+Inf" else "-Inf"
  | FP_normal | FP_subnormal | FP_zero -> Printf.sprintf "%.17g" value
;;

let labels_to_prometheus labels =
  match labels with
  | [] -> ""
  | _ ->
    labels
    |> List.map (fun (key, value) ->
      Printf.sprintf
        "%s=\"%s\""
        (prometheus_identifier key)
        (escape_prometheus_value value))
    |> String.concat ","
    |> Printf.sprintf "{%s}"
;;

let add_prometheus_header buf ~name ~kind =
  let prom_name = prometheus_identifier name in
  Buffer.add_string buf (Printf.sprintf "# HELP %s %s\n" prom_name name);
  Buffer.add_string buf (Printf.sprintf "# TYPE %s %s\n" prom_name kind);
  prom_name
;;

let counter_to_prometheus buf (c : counter_data) =
  let prom_name = add_prometheus_header buf ~name:c.c_name ~kind:"counter" in
  LabelMap.iter
    (fun labels value ->
       Buffer.add_string
         buf
         (Printf.sprintf "%s%s %d\n" prom_name (labels_to_prometheus labels) value))
    c.c_values
;;

let histogram_to_prometheus buf (h : histogram_data) =
  let prom_name = add_prometheus_header buf ~name:h.h_name ~kind:"histogram" in
  let sorted_buckets = List.sort Float.compare h.h_buckets in
  List.iter
    (fun bound ->
       let count =
         List.length (List.filter (fun value -> value <= bound) h.h_observations)
       in
       Buffer.add_string
         buf
         (Printf.sprintf
            "%s_bucket%s %d\n"
            prom_name
            (labels_to_prometheus [ "le", float_to_prometheus bound ])
            count))
    sorted_buckets;
  Buffer.add_string
    buf
    (Printf.sprintf
       "%s_bucket%s %d\n"
       prom_name
       (labels_to_prometheus [ "le", "+Inf" ])
       h.h_count);
  Buffer.add_string
    buf
    (Printf.sprintf "%s_sum %s\n" prom_name (float_to_prometheus h.h_sum));
  Buffer.add_string buf (Printf.sprintf "%s_count %d\n" prom_name h.h_count)
;;

let to_prometheus_text t =
  with_lock t (fun () ->
    let buf = Buffer.create 256 in
    let counters = List.sort (fun a b -> String.compare a.c_name b.c_name) t.counters in
    let histograms =
      List.sort (fun a b -> String.compare a.h_name b.h_name) t.histograms
    in
    List.iter (counter_to_prometheus buf) counters;
    List.iter (histogram_to_prometheus buf) histograms;
    Buffer.contents buf)
;;
