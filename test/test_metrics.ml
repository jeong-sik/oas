(** Tests for metrics.ml — SDK metrics collection. *)

open Alcotest
open Agent_sdk

let with_eio f () = Eio_main.run (fun _env -> f ())

let test_counter_basic () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"test.counter" ~unit_:"1" in
  Metrics.incr c 5;
  Metrics.incr c 3;
  check int "counter total" 8 (Metrics.counter_value c ())
;;

let test_counter_with_labels () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"gen_ai.client.token.usage" ~unit_:"token" in
  Metrics.incr c ~labels:[ "gen_ai.token.type", "input" ] 150;
  Metrics.incr c ~labels:[ "gen_ai.token.type", "output" ] 42;
  Metrics.incr c ~labels:[ "gen_ai.token.type", "input" ] 50;
  check
    int
    "input tokens"
    200
    (Metrics.counter_value c ~labels:[ "gen_ai.token.type", "input" ] ());
  check
    int
    "output tokens"
    42
    (Metrics.counter_value c ~labels:[ "gen_ai.token.type", "output" ] ());
  check int "unlabeled" 0 (Metrics.counter_value c ())
;;

let test_histogram_basic () =
  let m = Metrics.create () in
  let h =
    Metrics.histogram
      m
      ~name:"gen_ai.client.operation.duration"
      ~buckets:[ 0.1; 0.5; 1.0; 5.0 ]
  in
  Metrics.observe h 0.05;
  Metrics.observe h 0.3;
  Metrics.observe h 2.5;
  check int "count" 3 (Metrics.histogram_count h)
;;

let test_counter_same_name_returns_same () =
  let m = Metrics.create () in
  let c1 = Metrics.counter m ~name:"x" ~unit_:"1" in
  let c2 = Metrics.counter m ~name:"x" ~unit_:"1" in
  Metrics.incr c1 10;
  check int "same counter" 10 (Metrics.counter_value c2 ())
;;

let test_reset () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"x" ~unit_:"1" in
  let h = Metrics.histogram m ~name:"y" ~buckets:[ 1.0 ] in
  Metrics.incr c 5;
  Metrics.observe h 0.5;
  Metrics.reset m;
  check int "counter reset" 0 (Metrics.counter_value c ());
  check int "histogram reset" 0 (Metrics.histogram_count h)
;;

let test_otlp_json_structure () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"test.c" ~unit_:"1" in
  Metrics.incr c 1;
  let h = Metrics.histogram m ~name:"test.h" ~buckets:[ 1.0; 5.0 ] in
  Metrics.observe h 2.0;
  let json = Metrics.to_otlp_json m in
  let open Yojson.Safe.Util in
  let rm = json |> member "resourceMetrics" |> to_list in
  check bool "has resourceMetrics" true (List.length rm > 0);
  let sm = List.hd rm |> member "scopeMetrics" |> to_list in
  check bool "has scopeMetrics" true (List.length sm > 0);
  let metrics = List.hd sm |> member "metrics" |> to_list in
  check int "2 metrics" 2 (List.length metrics)
;;

let prometheus_lines text =
  text |> String.split_on_char '\n' |> List.filter (fun line -> line <> "")
;;

let check_line label expected text =
  check bool label true (List.mem expected (prometheus_lines text))
;;

let test_prometheus_text_counter_normalizes_names_and_labels () =
  let m = Metrics.create () in
  let c = Metrics.counter m ~name:"gen_ai.client.token.usage" ~unit_:"token" in
  Metrics.incr c ~labels:[ "gen_ai.token.type", "input" ] 150;
  Metrics.incr c ~labels:[ "gen_ai.token.type", "output" ] 42;
  let text = Metrics.to_prometheus_text m in
  check_line
    "counter HELP"
    "# HELP gen_ai_client_token_usage gen_ai.client.token.usage"
    text;
  check_line "counter TYPE" "# TYPE gen_ai_client_token_usage counter" text;
  check_line
    "input counter sample"
    "gen_ai_client_token_usage{gen_ai_token_type=\"input\"} 150"
    text;
  check_line
    "output counter sample"
    "gen_ai_client_token_usage{gen_ai_token_type=\"output\"} 42"
    text
;;

let test_prometheus_text_histogram_exports_buckets_sum_and_count () =
  let m = Metrics.create () in
  let h =
    Metrics.histogram m ~name:"gen_ai.client.operation.duration" ~buckets:[ 1.0; 2.0 ]
  in
  Metrics.observe h 1.0;
  Metrics.observe h 3.0;
  let text = Metrics.to_prometheus_text m in
  check_line
    "histogram HELP"
    "# HELP gen_ai_client_operation_duration gen_ai.client.operation.duration"
    text;
  check_line "histogram TYPE" "# TYPE gen_ai_client_operation_duration histogram" text;
  check_line "bucket le 1" "gen_ai_client_operation_duration_bucket{le=\"1\"} 1" text;
  check_line "bucket le 2" "gen_ai_client_operation_duration_bucket{le=\"2\"} 1" text;
  check_line
    "bucket le +Inf"
    "gen_ai_client_operation_duration_bucket{le=\"+Inf\"} 2"
    text;
  check_line "histogram sum" "gen_ai_client_operation_duration_sum 4" text;
  check_line "histogram count" "gen_ai_client_operation_duration_count 2" text
;;

let test_prometheus_text_histogram_deduplicates_bucket_bounds () =
  let m = Metrics.create () in
  let h = Metrics.histogram m ~name:"dup.bounds" ~buckets:[ 1.0; 1.0; 2.0 ] in
  Metrics.observe h 0.5;
  let text = Metrics.to_prometheus_text m in
  let count_substring text needle =
    let len = String.length needle in
    let rec loop start acc =
      match String.index_from_opt text start needle.[0] with
      | None -> acc
      | Some i when i + len <= String.length text && String.sub text i len = needle ->
        loop (i + len) (acc + 1)
      | Some i -> loop (i + 1) acc
    in
    loop 0 0
  in
  check
    int
    "duplicate bound emitted exactly once"
    1
    (count_substring text "dup_bounds_bucket{le=\"1\"}")
;;

let () =
  run
    "Metrics"
    [ ( "counter"
      , [ test_case "basic increment" `Quick (with_eio test_counter_basic)
        ; test_case "labeled counters" `Quick (with_eio test_counter_with_labels)
        ; test_case
            "same name returns same"
            `Quick
            (with_eio test_counter_same_name_returns_same)
        ] )
    ; "histogram", [ test_case "basic observe" `Quick (with_eio test_histogram_basic) ]
    ; ( "lifecycle"
      , [ test_case "reset clears all" `Quick (with_eio test_reset)
        ; test_case "otlp json structure" `Quick (with_eio test_otlp_json_structure)
        ] )
    ; ( "prometheus"
      , [ test_case
            "counter text export normalizes names and labels"
            `Quick
            (with_eio test_prometheus_text_counter_normalizes_names_and_labels)
        ; test_case
            "histogram text export emits buckets sum and count"
            `Quick
            (with_eio test_prometheus_text_histogram_exports_buckets_sum_and_count)
        ; test_case
            "histogram text export deduplicates bucket bounds"
            `Quick
            (with_eio test_prometheus_text_histogram_deduplicates_bucket_bounds)
        ] )
    ]
;;
