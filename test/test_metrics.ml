(** Tests for metrics.ml — SDK metrics collection. *)

open Alcotest
open Agent_sdk

let test_counter_basic () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"test.counter" ~unit_:"1" in
  Metrics.incr c 5;
  Metrics.incr c 3;
  check int "counter total" 8 (Metrics.counter_value c ())

let test_counter_with_labels () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"gen_ai.client.token.usage" ~unit_:"token" in
  Metrics.incr c ~labels:[("gen_ai.token.type", "input")] 150;
  Metrics.incr c ~labels:[("gen_ai.token.type", "output")] 42;
  Metrics.incr c ~labels:[("gen_ai.token.type", "input")] 50;
  check int "input tokens" 200
    (Metrics.counter_value c ~labels:[("gen_ai.token.type", "input")] ());
  check int "output tokens" 42
    (Metrics.counter_value c ~labels:[("gen_ai.token.type", "output")] ());
  check int "unlabeled" 0 (Metrics.counter_value c ())

let test_histogram_basic () =
  let m = Metrics.create () in
  let h = Metrics.histogram m ~name:"gen_ai.client.operation.duration"
    ~buckets:[0.1; 0.5; 1.0; 5.0] in
  Metrics.observe h 0.05;
  Metrics.observe h 0.3;
  Metrics.observe h 2.5;
  check int "count" 3 (Metrics.histogram_count h)

let test_counter_same_name_returns_same () =
  let m = Metrics.create () in
  let c1 = Metrics.counter m ~name:"x" ~unit_:"1" in
  let c2 = Metrics.counter m ~name:"x" ~unit_:"1" in
  Metrics.incr c1 10;
  check int "same counter" 10 (Metrics.counter_value c2 ())

let test_reset () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"x" ~unit_:"1" in
  let h = Metrics.histogram m ~name:"y" ~buckets:[1.0] in
  Metrics.incr c 5;
  Metrics.observe h 0.5;
  Metrics.reset m;
  check int "counter reset" 0 (Metrics.counter_value c ());
  check int "histogram reset" 0 (Metrics.histogram_count h)

let test_otlp_json_structure () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"test.c" ~unit_:"1" in
  Metrics.incr c 1;
  let h = Metrics.histogram m ~name:"test.h" ~buckets:[1.0; 5.0] in
  Metrics.observe h 2.0;
  let json = Metrics.to_otlp_json m in
  let open Yojson.Safe.Util in
  let rm = json |> member "resourceMetrics" |> to_list in
  check bool "has resourceMetrics" true (List.length rm > 0);
  let sm = List.hd rm |> member "scopeMetrics" |> to_list in
  check bool "has scopeMetrics" true (List.length sm > 0);
  let metrics = List.hd sm |> member "metrics" |> to_list in
  check int "2 metrics" 2 (List.length metrics)

let () =
  run "Metrics" [
    "counter", [
      test_case "basic increment" `Quick test_counter_basic;
      test_case "labeled counters" `Quick test_counter_with_labels;
      test_case "same name returns same" `Quick test_counter_same_name_returns_same;
    ];
    "histogram", [
      test_case "basic observe" `Quick test_histogram_basic;
    ];
    "lifecycle", [
      test_case "reset clears all" `Quick test_reset;
      test_case "otlp json structure" `Quick test_otlp_json_structure;
    ];
  ]
