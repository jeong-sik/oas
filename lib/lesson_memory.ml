open Base
(** Lesson_memory — helpers for failure-driven autonomy feedback loops.

    @since 0.92.1 *)

type failure_record =
  { pattern : string
  ; summary : string
  ; action : string option
  ; stdout : string option
  ; stderr : string option
  ; diff_summary : string option
  ; trace_summary : string option
  ; metric_name : string option
  ; metric_error : string option
  ; participants : string list
  ; metadata : (string * Yojson.Safe.t) list
  }

type lesson =
  { procedure : Memory.procedure
  ; recent_failures : Memory.episode list
  }

let normalize_pattern pattern = pattern |> String.trim |> String.lowercase_ascii

let procedure_id_of_pattern pattern =
  "lesson:" ^ Digest.to_hex (Digest.string (normalize_pattern pattern))
;;

let metadata_assoc key metadata = List.assoc_opt key metadata

let metadata_string key metadata =
  match metadata_assoc key metadata with
  | Some (`String value) -> Some value
  | _ -> None
;;

let upsert_metadata key value metadata = (key, value) :: List.remove_assoc key metadata

let exact_procedure mem pattern =
  let normalized = normalize_pattern pattern in
  Memory.matching_procedures mem ~pattern ()
  |> List.find_opt (fun (proc : Memory.procedure) ->
    normalize_pattern proc.pattern = normalized)
;;

let optional_metadata = function
  | None -> []
  | Some (key, value) -> [ key, `String value ]
;;

let record_failure mem (record : failure_record) =
  let now = Unix.gettimeofday () in
  let proc_id = procedure_id_of_pattern record.pattern in
  let episode_id =
    Printf.sprintf "%s:%Ld" proc_id (Int64.of_float (now *. 1_000_000.0))
  in
  let episode_metadata =
    [ "procedure_id", `String proc_id
    ; "pattern", `String record.pattern
    ; "summary", `String record.summary
    ]
    @ optional_metadata
        (Option.map (fun value -> "stdout", Util.clip value 1000) record.stdout)
    @ optional_metadata
        (Option.map (fun value -> "stderr", Util.clip value 1000) record.stderr)
    @ optional_metadata
        (Option.map (fun value -> "diff_summary", value) record.diff_summary)
    @ optional_metadata
        (Option.map (fun value -> "trace_summary", value) record.trace_summary)
    @ optional_metadata
        (Option.map (fun value -> "metric_name", value) record.metric_name)
    @ optional_metadata
        (Option.map (fun value -> "metric_error", value) record.metric_error)
    @ record.metadata
  in
  let episode : Memory.episode =
    { id = episode_id
    ; timestamp = now
    ; participants =
        (if record.participants = [] then [ "autonomy_loop" ] else record.participants)
    ; action = record.summary
    ; outcome = Memory.Failure record.summary
    ; salience = 0.95
    ; metadata = episode_metadata
    }
  in
  Memory.store_episode mem episode;
  let action_text = Option.value ~default:record.summary record.action in
  let updated_proc =
    match exact_procedure mem record.pattern with
    | Some proc ->
      { proc with
        action = action_text
      ; last_used = now
      ; metadata =
          proc.metadata
          |> upsert_metadata "last_episode_id" (`String episode_id)
          |> upsert_metadata "last_summary" (`String record.summary)
      }
    | None ->
      { id = proc_id
      ; pattern = record.pattern
      ; action = action_text
      ; success_count = 0
      ; failure_count = 0
      ; confidence = 0.0
      ; last_used = now
      ; metadata =
          [ "last_episode_id", `String episode_id
          ; "last_summary", `String record.summary
          ]
      }
  in
  Memory.store_procedure mem updated_proc;
  Memory.record_failure mem updated_proc.id;
  updated_proc.id
;;

let recent_failures_for_procedure mem proc_id =
  Memory.recall_episodes
    mem
    ~limit:2
    ~filter:(fun episode ->
      match metadata_string "procedure_id" episode.metadata with
      | Some id -> id = proc_id
      | None -> false)
    ()
;;

let retrieve_lessons mem ~pattern ?(limit = 3) () =
  Memory.matching_procedures mem ~pattern ()
  |> List.sort (fun (a : Memory.procedure) (b : Memory.procedure) ->
    Float.compare b.last_used a.last_used)
  |> fun (procedures : Memory.procedure list) ->
  let rec take n acc = function
    | [] -> List.rev acc
    | _ when n <= 0 -> List.rev acc
    | proc :: rest ->
      take
        (n - 1)
        ({ procedure = proc; recent_failures = recent_failures_for_procedure mem proc.id }
         :: acc)
        rest
  in
  take limit [] procedures
;;

let render_failure_episode idx (episode : Memory.episode) =
  let detail =
    match episode.outcome with
    | Memory.Failure text -> text
    | Memory.Success text -> text
    | Memory.Neutral -> episode.action
  in
  Printf.sprintf "%d. %s" idx (Util.clip detail 220)
;;

let render_prompt_context lessons =
  match lessons with
  | [] -> None
  | _ ->
    let sections =
      List.mapi
        (fun idx (lesson : lesson) ->
           let failure_lines =
             match lesson.recent_failures with
             | [] -> "  Recent failures: none stored"
             | failures ->
               failures
               |> List.mapi (fun i ep -> "  " ^ render_failure_episode (i + 1) ep)
               |> String.concat "\n"
           in
           Printf.sprintf
             "Lesson %d\nPattern: %s\nRecommended action: %s\n%s"
             (idx + 1)
             lesson.procedure.pattern
             lesson.procedure.action
             failure_lines)
        lessons
    in
    Some ("Relevant prior failure lessons:\n" ^ String.concat "\n\n" sections)
;;

[@@@coverage off]
(* === Inline tests === *)

let base_failure_record =
  { pattern = "improve metric"
  ; summary = "Avoid editing files outside lib/"
  ; action = Some "Stay inside lib/ and rerun the metric harness."
  ; stdout = Some "stdout"
  ; stderr = Some "stderr"
  ; diff_summary = Some "touched lib/foo.ml"
  ; trace_summary = Some "metric parse failed once"
  ; metric_name = Some "score"
  ; metric_error = Some "missing metric tag"
  ; participants = [ "coder"; "reviewer" ]
  ; metadata = [ "source", `String "test" ]
  }
;;

let%test "record_failure stores a procedure and an episode" =
  let mem = Memory.create () in
  let proc_id = record_failure mem base_failure_record in
  Memory.procedure_count mem = 1
  && Memory.episode_count mem = 1
  && Memory.find_procedure mem ~pattern:"improve metric" () <> None
  && Util.contains_substring_ci ~haystack:proc_id ~needle:"lesson:"
;;

let%test "record_failure increments existing procedure failure count" =
  let mem = Memory.create () in
  let proc_id = record_failure mem base_failure_record in
  ignore (record_failure mem { base_failure_record with summary = "Try smaller diff" });
  match Memory.find_procedure mem ~pattern:"improve metric" () with
  | Some proc -> proc.id = proc_id && proc.failure_count = 2
  | None -> false
;;

let%test "retrieve_lessons returns recent failures" =
  let mem = Memory.create () in
  ignore (record_failure mem base_failure_record);
  match retrieve_lessons mem ~pattern:"metric" () with
  | [ { procedure; recent_failures } ] ->
    procedure.pattern = "improve metric" && List.length recent_failures = 1
  | _ -> false
;;

let%test "render_prompt_context empty" = render_prompt_context [] = None

let%test "render_prompt_context contains pattern and action" =
  let mem = Memory.create () in
  ignore (record_failure mem base_failure_record);
  match render_prompt_context (retrieve_lessons mem ~pattern:"metric" ()) with
  | Some text ->
    Util.contains_substring_ci ~haystack:text ~needle:"improve metric"
    && Util.contains_substring_ci ~haystack:text ~needle:"recommended action"
  | None -> false
;;
