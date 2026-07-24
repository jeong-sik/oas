type state =
  | Not_started
  | Running
  | Terminal

type t = state Atomic.t

type ('admission, 'attempt) progress_snapshot =
  { candidate_visit_count : int
  ; admissions : 'admission list
  ; attempts : 'attempt list
  }

type ('admission, 'attempt) progress_state =
  { candidate_visit_count : int
  ; admissions_rev : 'admission list
  ; attempts_rev : 'attempt list
  }

type ('admission, 'attempt) progress = ('admission, 'attempt) progress_state Atomic.t

type ('candidate, 'success, 'execution_error, 'advanceable_error, 'callback_error) outcome =
  | Succeeded of
      { candidate : 'candidate
      ; success : 'success
      }
  | Attempt_already_started
  | Before_advance_callback_failed of
      { failed_candidate : 'candidate
      ; failure : 'advanceable_error
      ; next_candidate : 'candidate
      ; cause : 'callback_error
      }
  | Execution_failed of
      { candidate : 'candidate
      ; cause : 'execution_error
      }

let create () = Atomic.make Not_started

let create_progress () =
  Atomic.make { candidate_visit_count = 0; admissions_rev = []; attempts_rev = [] }
;;

let record_admission progress admission =
  let current = Atomic.get progress in
  let candidate_visit_count = current.candidate_visit_count + 1 in
  Atomic.set
    progress
    { current with
      candidate_visit_count
    ; admissions_rev = admission :: current.admissions_rev
    }
;;

let record_attempt progress attempt =
  let current = Atomic.get progress in
  Atomic.set progress { current with attempts_rev = attempt :: current.attempts_rev }
;;

let progress_snapshot progress =
  let current = Atomic.get progress in
  { candidate_visit_count = current.candidate_visit_count
  ; admissions = List.rev current.admissions_rev
  ; attempts = List.rev current.attempts_rev
  }
;;

let duplicate_key ~equal ~key candidates =
  let rec find position seen = function
    | [] -> None
    | candidate :: rest ->
      let value = key candidate in
      (match List.find_opt (fun (seen_value, _) -> equal seen_value value) seen with
       | Some (_, first_position) -> Some (value, first_position, position)
       | None -> find (position + 1) ((value, position) :: seen) rest)
  in
  find 1 [] candidates
;;

let execute_once state ~candidates ~execute ~advanceable ~before_advance =
  if not (Atomic.compare_and_set state Not_started Running)
  then Attempt_already_started
  else
    Fun.protect
      ~finally:(fun () -> Atomic.set state Terminal)
      (fun () ->
         let rec execute_candidates = function
           | [] -> invalid_arg "Exact_output_flow: empty candidate snapshot"
           | candidate :: rest ->
             (match execute candidate with
              | Ok success -> Succeeded { candidate; success }
              | Error failure ->
                (match rest, advanceable failure with
                 | next :: _, Some advanceable_failure ->
                   (match
                      before_advance ~failed:candidate ~failure:advanceable_failure ~next
                    with
                    | Ok () -> execute_candidates rest
                    | Error cause ->
                      Before_advance_callback_failed
                        { failed_candidate = candidate
                        ; failure = advanceable_failure
                        ; next_candidate = next
                        ; cause
                        })
                 | [], _ | _ :: _, None -> Execution_failed { candidate; cause = failure }))
         in
         execute_candidates candidates)
;;
