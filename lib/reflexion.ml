open Base
(** Reflexion: act-evaluate-reflect-retry loop primitive.

    Based on MAR (Multi-Agent Reflexion) pattern:
    separated Act / Diagnose / Critique / Aggregate concerns.

    @since 0.89.0 *)

(** {1 Verdict} *)

type verdict =
  | Pass
  | Fail of
      { diagnosis : string
      ; critique : string list
      }

(** {1 Evaluator} *)

type evaluator = Types.api_response -> verdict

(** {1 Configuration} *)

type config =
  { max_attempts : int
  ; evaluator : evaluator
  ; memory_prefix : string
  ; include_critique : bool
  }

let default_config ~evaluator =
  { max_attempts = 3; evaluator; memory_prefix = "reflexion"; include_critique = true }
;;

(** {1 Result} *)

type attempt =
  { attempt_number : int
  ; response : Types.api_response
  ; verdict : verdict
  ; reflection_text : string option
  }

type run_result =
  { final_response : Types.api_response
  ; attempts : attempt list
  ; passed : bool
  ; total_attempts : int
  }

(** {1 Reflection formatting} *)

let format_reflection ~attempt_number verdict =
  match verdict with
  | Pass -> ""
  | Fail { diagnosis; critique } ->
    let header = Printf.sprintf "[Reflection from attempt %d]" attempt_number in
    let diag = Printf.sprintf "Diagnosis: %s" diagnosis in
    let critique_section =
      match critique with
      | [] -> ""
      | cs ->
        let items = List.mapi (fun i c -> Printf.sprintf "  %d. %s" (i + 1) c) cs in
        "\nCritique:\n" ^ String.concat "\n" items
    in
    String.concat "\n" [ header; diag; critique_section ]
;;

(** {1 Episodic memory integration} *)

(** Store a reflection as an episodic memory entry. *)
let store_reflection_episode
      (memory : Memory.t)
      ~(prefix : string)
      ~(attempt_number : int)
      ~(reflection : string)
  =
  let id = Printf.sprintf "%s:attempt_%d" prefix attempt_number in
  let episode : Memory.episode =
    { id
    ; timestamp = Unix.gettimeofday ()
    ; participants = [ "reflexion_loop" ]
    ; action = Printf.sprintf "Attempt %d failed — reflection stored" attempt_number
    ; outcome = Memory.Failure reflection
    ; salience = 0.8
    ; (* high salience for recent failures *)
      metadata = [ "attempt", `Int attempt_number; "type", `String "reflexion" ]
    }
  in
  Memory.store_episode memory episode
;;

(** {1 Core loop} *)

let make_run_result acc response passed =
  let attempts = List.rev acc in
  Ok
    { final_response = response; attempts; passed; total_attempts = List.length attempts }
;;

let run ~config ?memory ~run_agent () =
  let max = max 1 config.max_attempts in
  let rec loop attempt_number reflections acc =
    if attempt_number > max
    then (
      (* Exhausted: acc is newest-first, so hd is the last attempt *)
      let last = List.hd acc in
      make_run_result
        acc
        last.response
        (match last.verdict with
         | Pass -> true
         | Fail _ -> false))
    else (
      match run_agent ~reflections with
      | Error e -> Error e
      | Ok response ->
        let verdict = config.evaluator response in
        let reflection_text =
          match verdict with
          | Pass -> None
          | Fail _ ->
            let text = format_reflection ~attempt_number verdict in
            (match memory with
             | Some mem ->
               store_reflection_episode
                 mem
                 ~prefix:config.memory_prefix
                 ~attempt_number
                 ~reflection:text
             | None -> ());
            Some text
        in
        let attempt = { attempt_number; response; verdict; reflection_text } in
        let acc = attempt :: acc in
        (match verdict with
         | Pass -> make_run_result acc response true
         | Fail { diagnosis; _ } ->
           let new_reflections =
             match reflection_text with
             | Some text ->
               if config.include_critique
               then reflections @ [ text ]
               else (
                 let diag_only =
                   Printf.sprintf
                     "[Reflection from attempt %d]\nDiagnosis: %s"
                     attempt_number
                     diagnosis
                 in
                 reflections @ [ diag_only ])
             | None -> reflections
           in
           loop (attempt_number + 1) new_reflections acc))
  in
  loop 1 [] []
;;

(** {1 Hook integration} *)

let on_stop_evaluator ~config response = config.evaluator response
