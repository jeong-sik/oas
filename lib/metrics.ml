(** SDK metrics collection — counters and histograms.

    Instance-based: each [create ()] returns independent state.
    Thread-safe via Stdlib.Mutex (no Eio dependency needed). *)

(* -- Internal types --------------------------------------------------- *)

type label_key = (string * string) list

let label_key_of labels =
  List.sort (fun (a, _) (b, _) -> String.compare a b) labels

(* -- Counter ---------------------------------------------------------- *)

type counter_data = {
  c_name: string;
  c_unit: string;
  mutable c_values: (label_key * int) list;
}

type counter = counter_data

let counter_find_or_init data labels =
  let key = label_key_of labels in
  match List.assoc_opt key data.c_values with
  | Some _ -> ()
  | None -> data.c_values <- (key, 0) :: data.c_values

let incr (c : counter) ?(labels = []) n =
  let key = label_key_of labels in
  counter_find_or_init c labels;
  c.c_values <- List.map (fun (k, v) ->
    if k = key then (k, v + n) else (k, v)
  ) c.c_values

let counter_value (c : counter) ?(labels = []) () =
  let key = label_key_of labels in
  match List.assoc_opt key c.c_values with
  | Some v -> v
  | None -> 0

(* -- Histogram -------------------------------------------------------- *)

type histogram_data = {
  h_name: string;
  h_buckets: float list;
  mutable h_observations: float list;
  mutable h_sum: float;
  mutable h_count: int;
}

type histogram = histogram_data

let observe (h : histogram) value =
  h.h_observations <- value :: h.h_observations;
  h.h_sum <- h.h_sum +. value;
  h.h_count <- h.h_count + 1

let histogram_count (h : histogram) = h.h_count

(* -- Metrics instance ------------------------------------------------- *)

type t = {
  mu: Mutex.t;
  mutable counters: counter_data list;
  mutable histograms: histogram_data list;
}

let create () = {
  mu = Mutex.create ();
  counters = [];
  histograms = [];
}

let with_lock t f =
  Mutex.lock t.mu;
  Fun.protect f ~finally:(fun () -> Mutex.unlock t.mu)

let counter t ~name ~unit_ =
  with_lock t (fun () ->
    match List.find_opt (fun c -> c.c_name = name) t.counters with
    | Some c -> c
    | None ->
      let c = { c_name = name; c_unit = unit_; c_values = [] } in
      t.counters <- c :: t.counters;
      c)

let histogram t ~name ~buckets =
  with_lock t (fun () ->
    match List.find_opt (fun h -> h.h_name = name) t.histograms with
    | Some h -> h
    | None ->
      let h = { h_name = name; h_buckets = buckets;
                h_observations = []; h_sum = 0.0; h_count = 0 } in
      t.histograms <- h :: t.histograms;
      h)

let reset t =
  with_lock t (fun () ->
    List.iter (fun c -> c.c_values <- []) t.counters;
    List.iter (fun h ->
      h.h_observations <- [];
      h.h_sum <- 0.0;
      h.h_count <- 0
    ) t.histograms)

(* -- OTLP JSON export ------------------------------------------------ *)

let labels_to_json labels : Yojson.Safe.t =
  `List (List.map (fun (k, v) ->
    `Assoc [
      ("key", `String k);
      ("value", `Assoc [("stringValue", `String v)])
    ]
  ) labels)

let counter_to_json (c : counter_data) : Yojson.Safe.t =
  let data_points = List.map (fun (labels, value) ->
    `Assoc [
      ("attributes", labels_to_json labels);
      ("asInt", `String (string_of_int value));
    ]
  ) c.c_values in
  `Assoc [
    ("name", `String c.c_name);
    ("unit", `String c.c_unit);
    ("sum", `Assoc [
      ("dataPoints", `List data_points);
      ("isMonotonic", `Bool true);
    ]);
  ]

let bucket_counts buckets observations =
  let sorted_buckets = List.sort Float.compare buckets in
  let counts = List.map (fun bound ->
    List.length (List.filter (fun v -> v <= bound) observations)
  ) sorted_buckets in
  let overflow = List.length observations in
  counts @ [overflow]

let histogram_to_json (h : histogram_data) : Yojson.Safe.t =
  let bc = bucket_counts h.h_buckets h.h_observations in
  `Assoc [
    ("name", `String h.h_name);
    ("histogram", `Assoc [
      ("dataPoints", `List [
        `Assoc [
          ("count", `String (string_of_int h.h_count));
          ("sum", `Float h.h_sum);
          ("bucketCounts", `List (List.map (fun n -> `String (string_of_int n)) bc));
          ("explicitBounds", `List (List.map (fun b -> `Float b) h.h_buckets));
        ]
      ]);
    ]);
  ]

let to_otlp_json t =
  with_lock t (fun () ->
    let metrics =
      List.rev_map counter_to_json t.counters
      @ List.rev_map histogram_to_json t.histograms
    in
    `Assoc [
      ("resourceMetrics", `List [
        `Assoc [
          ("scopeMetrics", `List [
            `Assoc [
              ("scope", `Assoc [
                ("name", `String "agent_sdk.metrics");
                ("version", `String Sdk_version.version);
              ]);
              ("metrics", `List metrics);
            ]
          ])
        ]
      ])
    ])
