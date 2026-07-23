type state =
  | Not_started
  | Running
  | Terminal

type t = state Atomic.t

type ('candidate, 'success, 'execution_error, 'callback_error) outcome =
  | Succeeded of
      { candidate : 'candidate
      ; success : 'success
      }
  | Attempt_already_started
  | Before_dispatch_callback_failed of
      { candidate : 'candidate
      ; cause : 'callback_error
      }
  | Before_advance_callback_failed of
      { failed_candidate : 'candidate
      ; failure : 'execution_error
      ; next_candidate : 'candidate
      ; cause : 'callback_error
      }
  | Execution_failed of
      { candidate : 'candidate
      ; cause : 'execution_error
      }

let create () = Atomic.make Not_started

let execute_once state ~candidates ~before_dispatch ~execute ~can_advance ~before_advance =
  if not (Atomic.compare_and_set state Not_started Running)
  then Attempt_already_started
  else
    Fun.protect
      ~finally:(fun () -> Atomic.set state Terminal)
      (fun () ->
         let rec execute_candidates = function
           | [] -> invalid_arg "Exact_output_flow: empty candidate snapshot"
           | candidate :: rest ->
             (match before_dispatch candidate with
              | Error cause -> Before_dispatch_callback_failed { candidate; cause }
              | Ok () ->
                (match execute candidate with
                 | Ok success -> Succeeded { candidate; success }
                 | Error failure ->
                   (match rest with
                    | next :: _ when can_advance failure ->
                      (match before_advance ~failed:candidate ~failure ~next with
                       | Ok () -> execute_candidates rest
                       | Error cause ->
                         Before_advance_callback_failed
                           { failed_candidate = candidate
                           ; failure
                           ; next_candidate = next
                           ; cause
                           })
                    | [] | _ :: _ -> Execution_failed { candidate; cause = failure })))
         in
         execute_candidates candidates)
;;
