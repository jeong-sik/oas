(** Code-snippet tool strategy evaluation gate. *)

let experimental_env_var = "OAS_EXPERIMENTAL_CODE_SNIPPET"

type mode_metrics =
  { turns : int
  ; llm_calls : int
  ; tokens : int
  ; passed : bool
  }

type comparison =
  { task_name : string
  ; json_mode : mode_metrics
  ; snippet_mode : mode_metrics
  ; call_reduction_pct : float
  ; tokens_saved : int
  }

type gate =
  { min_tasks : int
  ; min_avg_call_reduction_pct : float
  ; require_no_pass_regression : bool
  }

type gate_result =
  { passed : bool
  ; task_count : int
  ; avg_call_reduction_pct : float
  ; json_passes : int
  ; snippet_passes : int
  ; failures : string list
  }

type comparison_error =
  | Empty_task_name
  | Negative_metric of
      { task_name : string
      ; mode : string
      ; field : string
      ; value : int
      }
  | Nonpositive_json_llm_calls of string

let default_gate =
  { min_tasks = 10; min_avg_call_reduction_pct = 25.0; require_no_pass_regression = true }
;;

let show_comparison_error = function
  | Empty_task_name -> "task_name must not be empty"
  | Negative_metric { task_name; mode; field; value } ->
    Printf.sprintf "%s %s.%s must be non-negative, got %d" task_name mode field value
  | Nonpositive_json_llm_calls task_name ->
    Printf.sprintf "%s json_mode.llm_calls must be positive" task_name
;;

let validate_mode ~task_name ~mode_name mode =
  let fields =
    [ "turns", mode.turns; "llm_calls", mode.llm_calls; "tokens", mode.tokens ]
  in
  List.find_map
    (fun (field, value) ->
       if value < 0
       then Some (Negative_metric { task_name; mode = mode_name; field; value })
       else None)
    fields
;;

let compare_task ~task_name ~json_mode ~snippet_mode =
  let task_name = String.trim task_name in
  if String.equal task_name ""
  then Error Empty_task_name
  else (
    match validate_mode ~task_name ~mode_name:"json_mode" json_mode with
    | Some err -> Error err
    | None ->
      (match validate_mode ~task_name ~mode_name:"snippet_mode" snippet_mode with
       | Some err -> Error err
       | None ->
         if json_mode.llm_calls <= 0
         then Error (Nonpositive_json_llm_calls task_name)
         else (
           let call_reduction_pct =
             float_of_int (json_mode.llm_calls - snippet_mode.llm_calls)
             /. float_of_int json_mode.llm_calls
             *. 100.0
           in
           Ok
             { task_name
             ; json_mode
             ; snippet_mode
             ; call_reduction_pct
             ; tokens_saved = json_mode.tokens - snippet_mode.tokens
             })))
;;

let count_passes comparisons ~(select : comparison -> mode_metrics) =
  List.fold_left
    (fun acc comparison ->
       let mode = select comparison in
       if mode.passed then acc + 1 else acc)
    0
    comparisons
;;

let average_call_reduction = function
  | [] -> 0.0
  | comparisons ->
    let total =
      List.fold_left
        (fun acc comparison -> acc +. comparison.call_reduction_pct)
        0.0
        comparisons
    in
    total /. float_of_int (List.length comparisons)
;;

let evaluate ?(gate = default_gate) comparisons =
  let task_count = List.length comparisons in
  let avg_call_reduction_pct = average_call_reduction comparisons in
  let json_passes = count_passes comparisons ~select:(fun c -> c.json_mode) in
  let snippet_passes = count_passes comparisons ~select:(fun c -> c.snippet_mode) in
  let failures =
    []
    |> (fun acc ->
    if task_count < gate.min_tasks
    then Printf.sprintf "task_count %d < required %d" task_count gate.min_tasks :: acc
    else acc)
    |> (fun acc ->
    if avg_call_reduction_pct < gate.min_avg_call_reduction_pct
    then
      Printf.sprintf
        "avg_call_reduction_pct %.2f < required %.2f"
        avg_call_reduction_pct
        gate.min_avg_call_reduction_pct
      :: acc
    else acc)
    |> (fun acc ->
    if gate.require_no_pass_regression && snippet_passes < json_passes
    then
      Printf.sprintf "snippet_passes %d < json_passes %d" snippet_passes json_passes
      :: acc
    else acc)
    |> List.rev
  in
  { passed = failures = []
  ; task_count
  ; avg_call_reduction_pct
  ; json_passes
  ; snippet_passes
  ; failures
  }
;;

let metric name value =
  { Eval.name; value; unit_ = None; tags = [ "strategy", "code_snippet" ] }
;;

let metrics_of_gate_result result =
  [ metric "code_snippet_eval_task_count" (Eval.Int_val result.task_count)
  ; metric
      "code_snippet_eval_avg_call_reduction_pct"
      (Eval.Float_val result.avg_call_reduction_pct)
  ; metric "code_snippet_eval_json_passes" (Eval.Int_val result.json_passes)
  ; metric "code_snippet_eval_snippet_passes" (Eval.Int_val result.snippet_passes)
  ]
;;

let verdict_of_gate_result result =
  let score = result.avg_call_reduction_pct /. 100.0 |> Float.max 0.0 |> Float.min 1.0 in
  let evidence =
    [ Printf.sprintf "task_count=%d" result.task_count
    ; Printf.sprintf "avg_call_reduction_pct=%.2f" result.avg_call_reduction_pct
    ; Printf.sprintf "json_passes=%d" result.json_passes
    ; Printf.sprintf "snippet_passes=%d" result.snippet_passes
    ]
    @ List.map (Printf.sprintf "failure=%s") result.failures
  in
  { Harness.passed = result.passed
  ; score = Some score
  ; evidence
  ; detail =
      (if result.passed
       then Some "CodeSnippet adoption gate passed"
       else Some "CodeSnippet adoption gate failed")
  }
;;

let truthy_env value =
  match String.lowercase_ascii (String.trim value) with
  | "1" | "true" | "yes" | "on" -> true
  | _ -> false
;;

let is_experiment_enabled ?(getenv = Llm_provider.Cli_common_env.default_getenv) () =
  match getenv experimental_env_var with
  | Some value -> truthy_env value
  | None -> false
;;

let require_experiment_enabled ?getenv () =
  if is_experiment_enabled ?getenv ()
  then Ok ()
  else
    Error
      (Printf.sprintf
         "%s must be truthy to run CodeSnippet experiments"
         experimental_env_var)
;;
